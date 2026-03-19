(* Shared utilities for code generators *)

let indent n s =
  let prefix = String.make (n * 2) ' ' in
  prefix ^ s

let collect_ref_types (file : Ast.file) : string list =
  let refs = Hashtbl.create 16 in
  let rec scan_type = function
    | Ast.Ref name -> Hashtbl.replace refs name ()
    | Ast.Option t | Ast.List t | Ast.Set t -> scan_type t
    | Ast.Map (k, v) -> scan_type k; scan_type v
    | Ast.Tuple ts -> List.iter scan_type ts
    | Ast.Prim _ | Ast.Named _ -> ()
  in
  let scan_field (f : Ast.field) = scan_type f.typ in
  let scan_param (p : Ast.param) = scan_type p.typ in
  let scan_method (m : Ast.method_decl) =
    List.iter scan_param m.params;
    scan_type m.return_type
  in
  List.iter (function
    | Ast.Struct s -> List.iter scan_field s.fields
    | Ast.Service s -> List.iter scan_method s.methods
    | Ast.Union u -> List.iter scan_type u.variants
    | Ast.Enum _ | Ast.Const _ -> ()
  ) file.declarations;
  Hashtbl.fold (fun k () acc -> k :: acc) refs []
  |> List.sort String.compare

let ref_id name = name ^ "Id"

let find_annotation name (anns : Ast.annotation list) =
  List.find_opt (fun (a : Ast.annotation) -> a.name = name) anns

let annotation_string_arg key (a : Ast.annotation) =
  match a.args with
  | Ast.Args_single (Ast.Lit_string s) when key = "" -> Some s
  | Ast.Args_named pairs ->
    (match List.assoc_opt key pairs with
     | Some (Ast.Lit_string s) -> Some s
     | _ -> None)
  | _ -> None

let write_file path content =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  mkdir_p dir;
  let oc = open_out path in
  output_string oc content;
  close_out oc

let rec ts_type = function
  | Ast.Prim Ast.String -> "string"
  | Ast.Prim Ast.Int -> "number"
  | Ast.Prim Ast.Int64 -> "bigint"
  | Ast.Prim Ast.Float -> "number"
  | Ast.Prim Ast.Byte -> "number"
  | Ast.Prim Ast.Bool -> "boolean"
  | Ast.Prim Ast.Binary -> "Uint8Array"
  | Ast.Prim Ast.Uuid -> "string"
  | Ast.Prim Ast.Uuid_v7 -> "string"
  | Ast.Prim Ast.Void -> "void"
  | Ast.Named n -> n
  | Ast.Ref n -> ref_id n
  | Ast.Option t -> ts_type t ^ " | undefined"
  | Ast.Tuple ts -> "[" ^ String.concat ", " (List.map ts_type ts) ^ "]"
  | Ast.List t -> ts_type t ^ "[]"
  | Ast.Set t -> "Set<" ^ ts_type t ^ ">"
  | Ast.Map (k, v) -> "Record<" ^ ts_type k ^ ", " ^ ts_type v ^ ">"

let rec zod_type = function
  | Ast.Prim Ast.String -> "z.string()"
  | Ast.Prim Ast.Int -> "z.number().int()"
  | Ast.Prim Ast.Int64 -> "z.bigint()"
  | Ast.Prim Ast.Float -> "z.number()"
  | Ast.Prim Ast.Byte -> "z.number().int().min(0).max(255)"
  | Ast.Prim Ast.Bool -> "z.boolean()"
  | Ast.Prim Ast.Binary -> "z.instanceof(Uint8Array)"
  | Ast.Prim Ast.Uuid -> "z.string().uuid()"
  | Ast.Prim Ast.Uuid_v7 -> "z.string().uuid()"
  | Ast.Prim Ast.Void -> "z.void()"
  | Ast.Named n -> n ^ "Schema"
  | Ast.Ref n -> ref_id n ^ "Schema"
  | Ast.Option t -> zod_type t ^ ".optional()"
  | Ast.Tuple ts -> "z.tuple([" ^ String.concat ", " (List.map zod_type ts) ^ "])"
  | Ast.List t -> "z.array(" ^ zod_type t ^ ")"
  | Ast.Set t -> "z.set(" ^ zod_type t ^ ")"
  | Ast.Map (_, v) -> "z.record(z.string(), " ^ zod_type v ^ ")"

let rec json_schema_type = function
  | Ast.Prim Ast.String -> "{ \"type\": \"string\" }"
  | Ast.Prim Ast.Int -> "{ \"type\": \"integer\" }"
  | Ast.Prim Ast.Int64 -> "{ \"type\": \"integer\", \"format\": \"int64\" }"
  | Ast.Prim Ast.Float -> "{ \"type\": \"number\" }"
  | Ast.Prim Ast.Byte -> "{ \"type\": \"integer\", \"minimum\": 0, \"maximum\": 255 }"
  | Ast.Prim Ast.Bool -> "{ \"type\": \"boolean\" }"
  | Ast.Prim Ast.Binary -> "{ \"type\": \"string\", \"contentEncoding\": \"base64\" }"
  | Ast.Prim Ast.Uuid -> "{ \"type\": \"string\", \"format\": \"uuid\" }"
  | Ast.Prim Ast.Uuid_v7 -> "{ \"type\": \"string\", \"format\": \"uuid\" }"
  | Ast.Prim Ast.Void -> "{ }"
  | Ast.Named n -> "{ \"$ref\": \"#/$defs/" ^ n ^ "\" }"
  | Ast.Ref n -> "{ \"type\": \"string\", \"description\": \"" ^ ref_id n ^ "\" }"
  | Ast.Option t -> json_schema_type t
  | Ast.Tuple ts ->
    "{ \"type\": \"array\", \"prefixItems\": [" ^
    String.concat ", " (List.map json_schema_type ts) ^
    "], \"items\": false }"
  | Ast.List t -> "{ \"type\": \"array\", \"items\": " ^ json_schema_type t ^ " }"
  | Ast.Set t -> "{ \"type\": \"array\", \"items\": " ^ json_schema_type t ^ ", \"uniqueItems\": true }"
  | Ast.Map (_, v) -> "{ \"type\": \"object\", \"additionalProperties\": " ^ json_schema_type v ^ " }"
