(* Generate OpenAPI 3.1 YAML *)

(* ── YAML helpers ──────────────────────────────────────────────────── *)

let yi n s = String.make (n * 2) ' ' ^ s

let rec yaml_schema indent_n = function
  | Ast.Prim Ast.String -> [yi indent_n "type: string"]
  | Ast.Prim Ast.Int -> [yi indent_n "type: integer"]
  | Ast.Prim Ast.Int64 -> [yi indent_n "type: integer"; yi indent_n "format: int64"]
  | Ast.Prim Ast.Float -> [yi indent_n "type: number"]
  | Ast.Prim Ast.Byte -> [yi indent_n "type: integer"; yi indent_n "minimum: 0"; yi indent_n "maximum: 255"]
  | Ast.Prim Ast.Bool -> [yi indent_n "type: boolean"]
  | Ast.Prim Ast.Binary -> [yi indent_n "type: string"; yi indent_n "contentEncoding: base64"]
  | Ast.Prim Ast.Uuid -> [yi indent_n "type: string"; yi indent_n "format: uuid"]
  | Ast.Prim Ast.Uuid_v7 -> [yi indent_n "type: string"; yi indent_n "format: uuid"]
  | Ast.Prim Ast.Void -> [yi indent_n "type: object"]
  | Ast.Named n -> [yi indent_n ("$ref: '#/components/schemas/" ^ n ^ "'")]
  | Ast.Ref n -> [yi indent_n ("$ref: '#/components/schemas/" ^ Emitter.ref_id n ^ "'")]
  | Ast.Option t -> yaml_schema indent_n t
  | Ast.List t ->
    [yi indent_n "type: array"; yi indent_n "items:"] @ yaml_schema (indent_n + 1) t
  | Ast.Set t ->
    [yi indent_n "type: array"; yi indent_n "uniqueItems: true"; yi indent_n "items:"]
    @ yaml_schema (indent_n + 1) t
  | Ast.Tuple _ -> [yi indent_n "type: array"]
  | Ast.Map (_, v) ->
    [yi indent_n "type: object"; yi indent_n "additionalProperties:"]
    @ yaml_schema (indent_n + 1) v

(* ── Route types ───────────────────────────────────────────────────── *)

type route_op = {
  http_method : string;
  operation_id : string;
  summary : string;
  method_decl : Ast.method_decl;
}

(* ── Route resolution ──────────────────────────────────────────────── *)

let resolve_base_path ~name ~annotations =
  match Emitter.find_annotation "rest" annotations with
  | Some a ->
    let from_named = Emitter.annotation_string_arg "base" a in
    let from_single = Emitter.annotation_string_arg "" a in
    (match from_named with
     | Some b -> b
     | None -> match from_single with
       | Some b -> b
       | None -> "/" ^ String.lowercase_ascii name)
  | None -> "/" ^ String.lowercase_ascii name

let method_to_route base (m : Ast.method_decl) =
  match m.name with
  | "create" -> ("post", base)
  | "list" -> ("get", base)
  | "get" -> ("get", base ^ "/{id}")
  | "update" -> ("put", base ^ "/{id}")
  | "delete" -> ("delete", base ^ "/{id}")
  | name -> ("post", base ^ "/" ^ name)

(* ── Path collection ───────────────────────────────────────────────── *)

let collect_paths (file : Ast.file) =
  let paths : (string, route_op list) Hashtbl.t = Hashtbl.create 32 in
  let path_order = ref [] in

  let add_path path op =
    if not (Hashtbl.mem paths path) then
      path_order := path :: !path_order;
    let existing = try Hashtbl.find paths path with Not_found -> [] in
    Hashtbl.replace paths path (existing @ [op])
  in

  List.iter (function
    | Ast.Service s ->
      let base = resolve_base_path ~name:s.name ~annotations:s.annotations in
      List.iter (fun (m : Ast.method_decl) ->
        let (http_method, path) = method_to_route base m in
        add_path path {
          http_method;
          operation_id = s.name ^ "_" ^ m.name;
          summary = s.name ^ "." ^ m.name;
          method_decl = m;
        }
      ) s.methods
    | _ -> ()
  ) file.declarations;

  (List.rev !path_order, paths)

(* ── Operation emitter ─────────────────────────────────────────────── *)

let is_ref_param (p : Ast.param) =
  match p.typ with Ast.Ref _ -> true | _ -> false

let emit_operation line lines (op : route_op) =
  let m = op.method_decl in

  line (Printf.sprintf "    %s:" op.http_method);
  line (Printf.sprintf "      operationId: %s" op.operation_id);
  line (Printf.sprintf "      summary: %s" op.summary);

  if m.params <> [] then begin
    let id_params = List.filter is_ref_param m.params in
    let body_params = List.filter (fun p -> not (is_ref_param p)) m.params in

    (* Path parameters *)
    if id_params <> [] then begin
      line "      parameters:";
      List.iter (fun (p : Ast.param) ->
        line (Printf.sprintf "        - name: %s" p.name);
        line "          in: path";
        line "          required: true";
        line "          schema:";
        lines (yaml_schema 6 p.typ)
      ) id_params
    end;

    (* Request body *)
    if body_params <> [] then begin
      line "      requestBody:";
      line "        required: true";
      line "        content:";
      line "          application/json:";
      line "            schema:";
      line "              type: object";
      line "              properties:";
      List.iter (fun (p : Ast.param) ->
        line (Printf.sprintf "                %s:" p.name);
        lines (yaml_schema 9 p.typ)
      ) body_params
    end
  end;

  (* Response *)
  line "      responses:";
  line "        '200':";
  line "          description: Success";
  (match m.return_type with
   | Ast.Prim Ast.Void -> ()
   | rt ->
     line "          content:";
     line "            application/json:";
     line "              schema:";
     lines (yaml_schema 8 rt));

  if m.raises <> [] then begin
    line "        '400':";
    line (Printf.sprintf "          description: %s" (String.concat " | " m.raises))
  end

(* ── Paths section ─────────────────────────────────────────────────── *)

let emit_paths line lines (ordered_paths, paths) =
  if ordered_paths <> [] then begin
    line "paths:";
    List.iter (fun path ->
      line (Printf.sprintf "  %s:" path);
      let ops = Hashtbl.find paths path in
      List.iter (emit_operation line lines) ops
    ) ordered_paths
  end

(* ── Components section ────────────────────────────────────────────── *)

let emit_components line lines (file : Ast.file) =
  let refs = Emitter.collect_ref_types file in
  let has_schemas = refs <> [] || List.exists (function
    | Ast.Struct _ | Ast.Enum _ | Ast.Union _ -> true | _ -> false
  ) file.declarations in

  if not has_schemas then ()
  else begin
    line "components:";
    line "  schemas:";

    (* Branded IDs *)
    List.iter (fun name ->
      let id = Emitter.ref_id name in
      line (Printf.sprintf "    %s:" id);
      line "      type: string";
      line (Printf.sprintf "      description: Branded ID for %s" name)
    ) refs;

    (* Structs *)
    List.iter (function
      | Ast.Struct s ->
        line (Printf.sprintf "    %s:" s.name);
        line "      type: object";
        line "      properties:";
        List.iter (fun (f : Ast.field) ->
          line (Printf.sprintf "        %s:" f.name);
          lines (yaml_schema 5 f.typ)
        ) s.fields;
        let required = List.filter_map (fun (f : Ast.field) ->
          if not f.optional then Some f.name else None
        ) s.fields in
        if required <> [] then begin
          line "      required:";
          List.iter (fun r -> line (Printf.sprintf "        - %s" r)) required
        end
      | _ -> ()
    ) file.declarations;

    (* Enums *)
    List.iter (function
      | Ast.Enum e ->
        line (Printf.sprintf "    %s:" e.name);
        line "      type: string";
        line "      enum:";
        List.iter (fun (m : Ast.enum_member) ->
          line (Printf.sprintf "        - %s" m.name)
        ) e.members
      | _ -> ()
    ) file.declarations;

    (* Unions *)
    List.iter (function
      | Ast.Union u ->
        line (Printf.sprintf "    %s:" u.name);
        line "      oneOf:";
        List.iter (fun t ->
          line (Printf.sprintf "        - %s" (String.concat "\n" (yaml_schema 0 t)))
        ) u.variants
      | _ -> ()
    ) file.declarations
  end

(* ── Main entry point ──────────────────────────────────────────────── *)

let generate (file : Ast.file) : string =
  let buf = Buffer.create 8192 in
  let line s = Buffer.add_string buf s; Buffer.add_char buf '\n' in
  let lines ls = List.iter line ls in

  line "openapi: '3.1.0'";
  line "info:";
  line "  title: Generated API";
  line "  version: '1.0.0'";

  file |> collect_paths |> emit_paths line lines;
  emit_components line lines file;

  Buffer.contents buf
