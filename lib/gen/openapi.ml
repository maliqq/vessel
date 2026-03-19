(* Generate OpenAPI 3.1 YAML using the yaml library *)

let ystr s : Yaml.value = `String s
let yobj pairs : Yaml.value = `O pairs
let ylist items : Yaml.value = `A items
let ybool b : Yaml.value = `Bool b

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
  | "list"   -> ("get", base)
  | "get"    -> ("get", base ^ "/{id}")
  | "update" -> ("put", base ^ "/{id}")
  | "delete" -> ("delete", base ^ "/{id}")
  | name     -> ("post", base ^ "/" ^ name)

(* ── Param classification ──────────────────────────────────────────── *)

let is_ref_param (p : Ast.param) =
  match p.typ with Ast.Ref _ -> true | _ -> false

let partition_params params =
  let path_params = List.filter is_ref_param params in
  let body_params = List.filter (fun p -> not (is_ref_param p)) params in
  (path_params, body_params)

(* ── Operation builder ─────────────────────────────────────────────── *)

let build_path_param (p : Ast.param) : Yaml.value =
  yobj [
    "name", ystr p.name;
    "in", ystr "path";
    "required", ybool true;
    "schema", Emitter.yaml_of_type p.typ;
  ]

let build_request_body (params : Ast.param list) : Yaml.value =
  let properties = List.map (fun (p : Ast.param) ->
    (p.name, Emitter.yaml_of_type p.typ)
  ) params in
  yobj [
    "required", ybool true;
    "content", yobj [
      "application/json", yobj [
        "schema", yobj [
          "type", ystr "object";
          "properties", yobj properties;
        ]
      ]
    ]
  ]

let build_response (return_type : Ast.typ) (raises : string list) : Yaml.value =
  let success_response = match return_type with
    | Ast.Prim Ast.Void ->
      yobj ["description", ystr "Success"]
    | rt ->
      yobj [
        "description", ystr "Success";
        "content", yobj [
          "application/json", yobj [
            "schema", Emitter.yaml_of_type rt;
          ]
        ]
      ]
  in
  let base = ["200", success_response] in
  let with_errors = match raises with
    | [] -> base
    | rs -> base @ ["400", yobj ["description", ystr (String.concat " | " rs)]]
  in
  yobj with_errors

let build_operation ~service_name (m : Ast.method_decl) : Yaml.value =
  let (path_params, body_params) = partition_params m.params in

  let base = [
    "operationId", ystr (service_name ^ "_" ^ m.name);
    "summary", ystr (service_name ^ "." ^ m.name);
  ] in

  let with_params = match path_params with
    | [] -> base
    | ps -> base @ ["parameters", ylist (List.map build_path_param ps)]
  in

  let with_body = match body_params with
    | [] -> with_params
    | ps -> with_params @ ["requestBody", build_request_body ps]
  in

  let with_responses =
    with_body @ ["responses", build_response m.return_type m.raises]
  in

  yobj with_responses

(* ── Path collection ───────────────────────────────────────────────── *)

type route = {
  http_method : string;
  operation : Yaml.value;
}

let collect_paths (file : Ast.file) : (string * route list) list =
  let paths : (string, route list) Hashtbl.t = Hashtbl.create 32 in
  let path_order = ref [] in

  let add_path path route =
    if not (Hashtbl.mem paths path) then
      path_order := path :: !path_order;
    let existing = try Hashtbl.find paths path with Not_found -> [] in
    Hashtbl.replace paths path (existing @ [route])
  in

  List.iter (function
    | Ast.Service s ->
      let base = resolve_base_path ~name:s.name ~annotations:s.annotations in
      List.iter (fun (m : Ast.method_decl) ->
        let (http_method, path) = method_to_route base m in
        let operation = build_operation ~service_name:s.name m in
        add_path path { http_method; operation }
      ) s.methods
    | _ -> ()
  ) file.declarations;

  List.rev !path_order
  |> List.map (fun path -> (path, Hashtbl.find paths path))

(* ── Paths section ─────────────────────────────────────────────────── *)

let build_paths (file : Ast.file) : (string * Yaml.value) list =
  let grouped = collect_paths file in
  match grouped with
  | [] -> []
  | _ ->
    let path_entries = List.map (fun (path, routes) ->
      let methods = List.map (fun r -> (r.http_method, r.operation)) routes in
      (path, yobj methods)
    ) grouped in
    ["paths", yobj path_entries]

(* ── Components section ────────────────────────────────────────────── *)

let build_schema_component = function
  | Ast.Struct s ->
    let properties = List.map (fun (f : Ast.field) ->
      (f.name, Emitter.yaml_of_type f.typ)
    ) s.fields in
    let required = List.filter_map (fun (f : Ast.field) ->
      if not f.optional then Some (ystr f.name) else None
    ) s.fields in
    let base = ["type", ystr "object"; "properties", yobj properties] in
    let with_required = match required with
      | [] -> base
      | rs -> base @ ["required", ylist rs]
    in
    Some (s.name, yobj with_required)

  | Ast.Enum e ->
    let members = List.map (fun (m : Ast.enum_member) -> ystr m.name) e.members in
    Some (e.name, yobj ["type", ystr "string"; "enum", ylist members])

  | Ast.Union u ->
    let variants = List.map Emitter.yaml_of_type u.variants in
    Some (u.name, yobj ["oneOf", ylist variants])

  | _ -> None

let build_components (file : Ast.file) : (string * Yaml.value) list =
  let refs = Emitter.collect_ref_types file in
  let id_schemas = List.map (fun name ->
    let id = Emitter.ref_id name in
    (id, yobj [
      "type", ystr "string";
      "description", ystr ("Branded ID for " ^ name);
    ])
  ) refs in

  let decl_schemas = List.filter_map build_schema_component file.declarations in
  let all_schemas = id_schemas @ decl_schemas in

  match all_schemas with
  | [] -> []
  | _ -> ["components", yobj ["schemas", yobj all_schemas]]

(* ── Main entry point ──────────────────────────────────────────────── *)

let generate (file : Ast.file) : string =
  let doc = yobj (
    [
      "openapi", ystr "3.1.0";
      "info", yobj [
        "title", ystr "Generated API";
        "version", ystr "1.0.0";
      ];
    ]
    @ build_paths file
    @ build_components file
  ) in
  Yaml.to_string_exn doc
