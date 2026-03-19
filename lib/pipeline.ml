(* Compilation pipeline — chains AST transforms and code generation.

   The pipeline reads left-to-right:
     source
     |> parse
     |> expand       (@crud, future: @paginate, etc.)
     |> generate     (all targets at once)
     |> emit         (write to disk)
*)

(* ── Types ─────────────────────────────────────────────────────────── *)

type target =
  | Typescript_types
  | Typescript_zod
  | Json_schema
  | Openapi

type output = {
  target : target;
  content : string;
  relative_path : string;
}

(* ── Parse ─────────────────────────────────────────────────────────── *)

let parse source =
  Parser.parse_file source

(* ── AST transforms ────────────────────────────────────────────────── *)

let expand ast =
  ast
  |> Extensions.apply

(* ── Code generation ───────────────────────────────────────────────── *)

let generators : (target * string * (Ast.file -> string)) list = [
  (Typescript_types, "typescript/types.ts",       Gen_ts_types.generate);
  (Typescript_zod,   "typescript/schemas.ts",     Gen_ts_zod.generate);
  (Json_schema,      "jsonschema/schema.json",    Gen_jsonschema.generate);
  (Openapi,          "openapi/schema.yaml",       Gen_openapi.generate);
]

let generate ast =
  generators
  |> List.map (fun (target, path, gen) ->
    { target; content = gen ast; relative_path = path })

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
  |> expand
  |> generate
  |> emit out_dir
