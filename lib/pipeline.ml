(* Compilation pipeline — chains AST transforms and code generation.

   source
   |> parse
   |> extend           (AST visitors: @crud, @paginate, etc.)
   |> resolve(target)  (merge universal + target-scoped annotations)
   |> generate         (per-target code generation)
   |> emit             (write to disk)
*)

(* ── Types ─────────────────────────────────────────────────────────── *)

type target = {
  name : string;
  path : string;
  generate : Ast.file -> string;
}

type output = {
  content : string;
  relative_path : string;
}

(* ── Parse ─────────────────────────────────────────────────────────── *)

let parse source =
  source |> Parser.parse_file

(* ── Extend ────────────────────────────────────────────────────────── *)

let extensions = [
  Ext.Crud.apply;
  (* future: Ext.Paginate.apply; *)
]

let extend ast =
  extensions |> List.fold_left (fun acc ext -> ext acc) ast

(* ── Resolve ───────────────────────────────────────────────────────── *)

(* Resolves annotations for a target:
   1. Keep universal + matching target-scoped annotations
   2. Target-scoped overrides universal of the same name (last wins)
   3. Strip target field so generators see a clean list

   Example: @some_fact(true) @some_fact:ts(false)
   ts generator sees: @some_fact(false) — the scoped value overrides. *)

let resolve_annotations target_name (anns : Ast.annotation list) =
  let relevant =
    anns
    |> List.filter (fun (a : Ast.annotation) ->
      match a.target with
      | None -> true
      | Some t -> t = target_name)
  in
  (* Last annotation of the same name wins.
     Walk in order, accumulate into a name -> annotation map. *)
  let seen = Hashtbl.create 8 in
  let order = ref [] in
  List.iter (fun (a : Ast.annotation) ->
    if not (Hashtbl.mem seen a.name) then
      order := a.name :: !order;
    Hashtbl.replace seen a.name { a with Ast.target = None }
  ) relevant;
  List.rev !order
  |> List.map (Hashtbl.find seen)

let resolve_field target_name (f : Ast.field) =
  { f with annotations = resolve_annotations target_name f.annotations }

let resolve_enum_member target_name (m : Ast.enum_member) =
  { m with annotations = resolve_annotations target_name m.annotations }

let resolve_method target_name (m : Ast.method_decl) =
  { m with annotations = resolve_annotations target_name m.annotations }

let resolve_decl target_name = function
  | Ast.Struct s ->
    Ast.Struct {
      s with
      annotations = resolve_annotations target_name s.annotations;
      fields = List.map (resolve_field target_name) s.fields;
    }
  | Ast.Enum e ->
    Ast.Enum {
      e with
      annotations = resolve_annotations target_name e.annotations;
      members = List.map (resolve_enum_member target_name) e.members;
    }
  | Ast.Union u ->
    Ast.Union {
      u with annotations = resolve_annotations target_name u.annotations;
    }
  | Ast.Service s ->
    Ast.Service {
      s with
      annotations = resolve_annotations target_name s.annotations;
      methods = List.map (resolve_method target_name) s.methods;
    }
  | other -> other

let resolve target_name (file : Ast.file) : Ast.file =
  { file with declarations = List.map (resolve_decl target_name) file.declarations }

(* ── Targets ───────────────────────────────────────────────────────── *)

let targets : target list = [
  { name = "ts";         path = "typescript/types.ts";    generate = Gen.Ts_types.generate };
  { name = "ts";         path = "typescript/schemas.ts";  generate = Gen.Ts_zod.generate };
  { name = "jsonschema"; path = "jsonschema/schema.json"; generate = Gen.Jsonschema.generate };
  { name = "openapi";    path = "openapi/schema.yaml";    generate = Gen.Openapi.generate };
]

(* ── Target filtering ──────────────────────────────────────────────── *)

let select_targets = function
  | None -> targets
  | Some name ->
    targets
    |> List.filter (fun t -> t.name = name)

let available_target_names () =
  targets
  |> List.map (fun t -> t.name)
  |> List.sort_uniq String.compare

(* ── Generate ──────────────────────────────────────────────────────── *)

let generate selected_targets extended_ast =
  selected_targets
  |> List.map (fun t ->
    let resolved_ast = extended_ast |> resolve t.name in
    { content = t.generate resolved_ast; relative_path = t.path })

(* ── Emit ──────────────────────────────────────────────────────────── *)

let emit out_dir outputs =
  outputs
  |> List.map (fun output ->
    let full_path = Filename.concat out_dir output.relative_path in
    Emitter.write_file full_path output.content;
    full_path)

(* ── Full pipeline ─────────────────────────────────────────────────── *)

let compile ?target source out_dir =
  let selected = select_targets target in
  source
  |> parse
  |> extend
  |> generate selected
  |> emit out_dir
