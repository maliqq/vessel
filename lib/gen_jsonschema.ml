(* Generate JSON Schema (Draft 2020-12) *)

let json_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | _ -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let rec literal_to_json = function
  | Ast.Lit_string s -> "\"" ^ json_escape s ^ "\""
  | Ast.Lit_int i -> string_of_int i
  | Ast.Lit_float f -> string_of_float f
  | Ast.Lit_bool b -> string_of_bool b
  | Ast.Lit_array xs -> "[" ^ String.concat ", " (List.map literal_to_json xs) ^ "]"

let generate (file : Ast.file) : string =
  let buf = Buffer.create 4096 in
  let line s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  line "{";
  line "  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",";
  line "  \"$defs\": {";

  let defs = ref [] in

  (* Collect branded ID types *)
  let refs = Emitter.collect_ref_types file in
  List.iter (fun name ->
    let id = Emitter.ref_id name in
    let def = Printf.sprintf "    \"%s\": { \"type\": \"string\", \"description\": \"Branded ID for %s\" }" id name in
    defs := def :: !defs
  ) refs;

  (* Structs *)
  List.iter (function
    | Ast.Struct s ->
      let props = List.map (fun (f : Ast.field) ->
        Printf.sprintf "        \"%s\": %s" f.name (Emitter.json_schema_type f.typ)
      ) s.fields in
      let required = List.filter_map (fun (f : Ast.field) ->
        if not f.optional then Some (Printf.sprintf "\"%s\"" f.name) else None
      ) s.fields in
      let req_str = match required with
        | [] -> ""
        | rs -> Printf.sprintf ",\n        \"required\": [%s]" (String.concat ", " rs)
      in
      let def = Printf.sprintf "    \"%s\": {\n      \"type\": \"object\",\n      \"properties\": {\n%s\n      }%s\n    }"
        s.name (String.concat ",\n" props) req_str in
      defs := def :: !defs
    | _ -> ()
  ) file.declarations;

  (* Enums *)
  List.iter (function
    | Ast.Enum e ->
      let members = List.map (fun (m : Ast.enum_member) ->
        "\"" ^ m.name ^ "\""
      ) e.members in
      let def = Printf.sprintf "    \"%s\": { \"type\": \"string\", \"enum\": [%s] }"
        e.name (String.concat ", " members) in
      defs := def :: !defs
    | _ -> ()
  ) file.declarations;

  (* Unions *)
  List.iter (function
    | Ast.Union u ->
      let variants = List.map Emitter.json_schema_type u.variants in
      let def = Printf.sprintf "    \"%s\": { \"oneOf\": [%s] }"
        u.name (String.concat ", " variants) in
      defs := def :: !defs
    | _ -> ()
  ) file.declarations;

  (* Consts *)
  List.iter (function
    | Ast.Const c ->
      let def = Printf.sprintf "    \"%s\": { \"const\": %s }"
        c.name (literal_to_json c.value) in
      defs := def :: !defs
    | _ -> ()
  ) file.declarations;

  let all_defs = List.rev !defs in
  line (String.concat ",\n" all_defs);
  line "  }";
  line "}";

  Buffer.contents buf
