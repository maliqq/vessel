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

(* ── JSON Schema type mapping (returns Yojson tree) ─────────────────── *)

let obj pairs : Yojson.Basic.t = `Assoc pairs

let rec json_of_type : Ast.typ -> Yojson.Basic.t = function
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
  | Ast.Ref n            -> obj ["type", `String "string"; "description", `String (ref_id n)]
  | Ast.Option t         -> json_of_type t
  | Ast.Tuple ts         -> obj ["type", `String "array";
                                 "prefixItems", `List (List.map json_of_type ts);
                                 "items", `Bool false]
  | Ast.List t           -> obj ["type", `String "array"; "items", json_of_type t]
  | Ast.Set t            -> obj ["type", `String "array"; "items", json_of_type t;
                                 "uniqueItems", `Bool true]
  | Ast.Map (_, v)       -> obj ["type", `String "object";
                                 "additionalProperties", json_of_type v]

(* ── YAML type mapping (returns Yaml.value tree) ───────────────────── *)

let yobj pairs : Yaml.value = `O pairs
let ystr s : Yaml.value = `String s

let rec yaml_of_type : Ast.typ -> Yaml.value = function
  | Ast.Prim Ast.String  -> yobj ["type", ystr "string"]
  | Ast.Prim Ast.Int     -> yobj ["type", ystr "integer"]
  | Ast.Prim Ast.Int64   -> yobj ["type", ystr "integer"; "format", ystr "int64"]
  | Ast.Prim Ast.Float   -> yobj ["type", ystr "number"]
  | Ast.Prim Ast.Byte    -> yobj ["type", ystr "integer";
                                  "minimum", `Float 0.0; "maximum", `Float 255.0]
  | Ast.Prim Ast.Bool    -> yobj ["type", ystr "boolean"]
  | Ast.Prim Ast.Binary  -> yobj ["type", ystr "string"; "contentEncoding", ystr "base64"]
  | Ast.Prim Ast.Uuid    -> yobj ["type", ystr "string"; "format", ystr "uuid"]
  | Ast.Prim Ast.Uuid_v7 -> yobj ["type", ystr "string"; "format", ystr "uuid"]
  | Ast.Prim Ast.Void    -> yobj ["type", ystr "object"]
  | Ast.Named n          -> yobj ["$ref", ystr ("#/components/schemas/" ^ n)]
  | Ast.Ref n            -> yobj ["$ref", ystr ("#/components/schemas/" ^ ref_id n)]
  | Ast.Option t         -> yaml_of_type t
  | Ast.List t           -> yobj ["type", ystr "array"; "items", yaml_of_type t]
  | Ast.Set t            -> yobj ["type", ystr "array"; "uniqueItems", `Bool true;
                                  "items", yaml_of_type t]
  | Ast.Tuple _          -> yobj ["type", ystr "array"]
  | Ast.Map (_, v)       -> yobj ["type", ystr "object";
                                  "additionalProperties", yaml_of_type v]
