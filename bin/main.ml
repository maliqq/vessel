let usage () =
  Printf.eprintf "usage: idlc <command> [options]\n\n";
  Printf.eprintf "commands:\n";
  Printf.eprintf "  compile <file.idl> [--out <dir>]   Generate code (default: ./build/)\n";
  Printf.eprintf "  print <file.idl>                   Parse and pretty-print AST\n";
  exit 1

let read_file filename =
  let ic = open_in filename in
  let src = In_channel.input_all ic in
  close_in ic;
  src

let compile filename out_dir =
  let src = read_file filename in
  let ast = Idlc.Parser.parse_file src in
  let ast = Idlc.Crud.expand ast in

  let ts_types = Idlc.Gen_ts_types.generate ast in
  let ts_zod = Idlc.Gen_ts_zod.generate ast in
  let json_schema = Idlc.Gen_jsonschema.generate ast in
  let openapi = Idlc.Gen_openapi.generate ast in

  Idlc.Emitter.write_file (Filename.concat out_dir "typescript/types.ts") ts_types;
  Idlc.Emitter.write_file (Filename.concat out_dir "typescript/schemas.ts") ts_zod;
  Idlc.Emitter.write_file (Filename.concat out_dir "jsonschema/schema.json") json_schema;
  Idlc.Emitter.write_file (Filename.concat out_dir "openapi/schema.yaml") openapi;

  Printf.printf "Generated:\n";
  Printf.printf "  %s/typescript/types.ts\n" out_dir;
  Printf.printf "  %s/typescript/schemas.ts\n" out_dir;
  Printf.printf "  %s/jsonschema/schema.json\n" out_dir;
  Printf.printf "  %s/openapi/schema.yaml\n" out_dir

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["print"; filename] ->
    let src = read_file filename in
    let ast = Idlc.Parser.parse_file src in
    print_string (Idlc.Printer.pp_file ast)
  | "compile" :: filename :: rest ->
    let out_dir = match rest with
      | ["--out"; dir] -> dir
      | [] -> "./build"
      | _ -> usage ()
    in
    compile filename out_dir
  | _ -> usage ()
