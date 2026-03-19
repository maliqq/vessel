(* Generate JSON Schema (Draft 2020-12) *)

(* ── JSON helpers ──────────────────────────────────────────────────── *)

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

(* ── Definition builders ───────────────────────────────────────────── *)

let def_branded_id name =
  let id = Emitter.ref_id name in
  Printf.sprintf "    \"%s\": { \"type\": \"string\", \"description\": \"Branded ID for %s\" }"
    id name

let def_struct ~name ~fields =
  let props = List.map (fun (f : Ast.field) ->
    Printf.sprintf "        \"%s\": %s" f.name (Emitter.json_schema_type f.typ)
  ) fields in

  let required = List.filter_map (fun (f : Ast.field) ->
    if not f.optional then Some (Printf.sprintf "\"%s\"" f.name) else None
  ) fields in

  let req_str = match required with
    | [] -> ""
    | rs -> Printf.sprintf ",\n        \"required\": [%s]" (String.concat ", " rs)
  in

  Printf.sprintf "    \"%s\": {\n      \"type\": \"object\",\n      \"properties\": {\n%s\n      }%s\n    }"
    name (String.concat ",\n" props) req_str

let def_enum ~name ~members =
  let member_strs = List.map (fun (m : Ast.enum_member) ->
    "\"" ^ m.name ^ "\""
  ) members in
  Printf.sprintf "    \"%s\": { \"type\": \"string\", \"enum\": [%s] }"
    name (String.concat ", " member_strs)

let def_union ~name ~variants =
  let variant_strs = List.map Emitter.json_schema_type variants in
  Printf.sprintf "    \"%s\": { \"oneOf\": [%s] }"
    name (String.concat ", " variant_strs)

let def_const ~name ~value =
  Printf.sprintf "    \"%s\": { \"const\": %s }"
    name (literal_to_json value)

(* ── Collect all definitions ───────────────────────────────────────── *)

let collect_defs (file : Ast.file) : string list =
  let refs = Emitter.collect_ref_types file in
  let id_defs = List.map def_branded_id refs in

  let decl_defs = List.filter_map (function
    | Ast.Struct s -> Some (def_struct ~name:s.name ~fields:s.fields)
    | Ast.Enum e -> Some (def_enum ~name:e.name ~members:e.members)
    | Ast.Union u -> Some (def_union ~name:u.name ~variants:u.variants)
    | Ast.Const c -> Some (def_const ~name:c.name ~value:c.value)
    | Ast.Service _ -> None
  ) file.declarations in

  id_defs @ decl_defs

(* ── Main entry point ──────────────────────────────────────────────── *)

let generate (file : Ast.file) : string =
  let buf = Buffer.create 4096 in
  let line s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  line "{";
  line "  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",";
  line "  \"$defs\": {";

  let all_defs = collect_defs file in
  line (String.concat ",\n" all_defs);

  line "  }";
  line "}";

  Buffer.contents buf
