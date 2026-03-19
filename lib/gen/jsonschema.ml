(* Generate JSON Schema (Draft 2020-12) using Yojson *)

module J = Yojson.Basic

(* ── Type mapping (AST → Yojson tree) ────────────────────────────── *)

let obj pairs : J.t = `Assoc pairs

let rec json_of_type : Ast.typ -> J.t = function
  | Ast.Prim Ast.String  -> obj ["type", `String "string"]
  | Ast.Prim Ast.Int     -> obj ["type", `String "integer"]
  | Ast.Prim Ast.Int64   -> obj ["type", `String "integer"; "format", `String "int64"]
  | Ast.Prim Ast.Float   -> obj ["type", `String "number"]
  | Ast.Prim Ast.Byte    -> obj ["type", `String "integer"; "minimum", `Int 0; "maximum", `Int 255]
  | Ast.Prim Ast.Bool    -> obj ["type", `String "boolean"]
  | Ast.Prim Ast.Binary  -> obj ["type", `String "string"; "contentEncoding", `String "base64"]
  | Ast.Prim Ast.Uuid    -> obj ["type", `String "string"; "format", `String "uuid"]
  | Ast.Prim Ast.Uuid_v7 -> obj ["type", `String "string"; "format", `String "uuid"]
  | Ast.Prim Ast.Void    -> obj []
  | Ast.Named n          -> obj ["$ref", `String ("#/$defs/" ^ n)]
  | Ast.Ref n            -> obj ["type", `String "string"; "description", `String (Emitter.ref_id n)]
  | Ast.Option t         -> json_of_type t
  | Ast.Tuple ts         -> obj ["type", `String "array";
                                  "prefixItems", `List (List.map json_of_type ts);
                                  "items", `Bool false]
  | Ast.List t           -> obj ["type", `String "array"; "items", json_of_type t]
  | Ast.Set t            -> obj ["type", `String "array"; "items", json_of_type t;
                                  "uniqueItems", `Bool true]
  | Ast.Map (_, v)       -> obj ["type", `String "object";
                                  "additionalProperties", json_of_type v]

(* ── Literal → JSON ─────────────────────────────────────────────── *)

let rec literal_to_json : Ast.literal -> J.t = function
  | Ast.Lit_string s -> `String s
  | Ast.Lit_int i    -> `Int i
  | Ast.Lit_float f  -> `Float f
  | Ast.Lit_bool b   -> `Bool b
  | Ast.Lit_array xs -> `List (List.map literal_to_json xs)

(* ── Definition builders ─────────────────────────────────────────── *)

let def_branded_id name : string * J.t =
  let id = Emitter.ref_id name in
  (id, `Assoc [
    "type", `String "string";
    "description", `String ("Branded ID for " ^ name);
  ])

let def_struct ~name ~(fields : Ast.field list) : string * J.t =
  let properties = List.map (fun (f : Ast.field) ->
    (f.name, json_of_type f.typ)
  ) fields in

  let required = List.filter_map (fun (f : Ast.field) ->
    if not f.optional then Some (`String f.name) else None
  ) fields in

  let base = [
    "type", `String "object";
    "properties", `Assoc properties;
  ] in

  let with_required = match required with
    | [] -> base
    | rs -> base @ ["required", `List rs]
  in

  (name, `Assoc with_required)

let def_enum ~name ~(members : Ast.enum_member list) : string * J.t =
  let values = List.map (fun (m : Ast.enum_member) -> `String m.name) members in
  (name, `Assoc [
    "type", `String "string";
    "enum", `List values;
  ])

let def_union ~name ~(variants : Ast.typ list) : string * J.t =
  let schemas = List.map json_of_type variants in
  (name, `Assoc ["oneOf", `List schemas])

let def_const ~name ~value : string * J.t =
  (name, `Assoc ["const", literal_to_json value])

(* ── Collect all definitions ─────────────────────────────────────── *)

let collect_defs (file : Ast.file) : (string * J.t) list =
  let refs = Emitter.collect_ref_types file in
  let id_defs = List.map def_branded_id refs in

  let decl_defs = List.filter_map (function
    | Ast.Struct s  -> Some (def_struct ~name:s.name ~fields:s.fields)
    | Ast.Enum e    -> Some (def_enum ~name:e.name ~members:e.members)
    | Ast.Union u   -> Some (def_union ~name:u.name ~variants:u.variants)
    | Ast.Const c   -> Some (def_const ~name:c.name ~value:c.value)
    | Ast.Service _ -> None
  ) file.declarations in

  id_defs @ decl_defs

(* ── Main entry point ────────────────────────────────────────────── *)

let generate (file : Ast.file) : string =
  let schema : J.t = `Assoc [
    "$schema", `String "https://json-schema.org/draft/2020-12/schema";
    "$defs", `Assoc (collect_defs file);
  ] in
  J.pretty_to_string ~std:true schema ^ "\n"
