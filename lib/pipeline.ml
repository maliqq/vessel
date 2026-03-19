(* Compilation pipeline — chains AST transforms and code generation.

   source
   |> parse
   |> extend      (AST visitors: @crud, @paginate, etc.)
   |> generate    (all output targets)
   |> emit        (write to disk)
*)

(* ── Types ─────────────────────────────────────────────────────────── *)

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

(* ── Generate ──────────────────────────────────────────────────────── *)

let targets : (string * (Ast.file -> string)) list = [
  ("typescript/types.ts",    Gen.Ts_types.generate);
  ("typescript/schemas.ts",  Gen.Ts_zod.generate);
  ("jsonschema/schema.json", Gen.Jsonschema.generate);
  ("openapi/schema.yaml",   Gen.Openapi.generate);
]

let generate ast =
  targets
  |> List.map (fun (path, gen) -> { content = gen ast; relative_path = path })

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
