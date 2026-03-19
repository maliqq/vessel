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

(* Filters annotation lists: keeps universal (no target)
   plus annotations scoped to the given target name.
   Strips the target field so generators see a clean list. *)

let resolve_annotations target_name (anns : Ast.annotation list) =
  anns
  |> List.filter (fun (a : Ast.annotation) ->
    match a.target with
    | None -> true
    | Some t -> t = target_name)
  |> List.map (fun a -> { a with Ast.target = None })

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

(* ── Generate ──────────────────────────────────────────────────────── *)

let generate extended_ast =
  targets
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

let compile source out_dir =
  source
  |> parse
  |> extend
  |> generate
  |> emit out_dir
