let usage () =
  let target_names = Idlc.Pipeline.available_target_names () in
  Printf.eprintf "usage: idlc <command> [options]\n\n";
  Printf.eprintf "commands:\n";
  Printf.eprintf "  compile <file.idl> [--out <dir>] [--target <name>]\n";
  Printf.eprintf "  print <file.idl>\n\n";
  Printf.eprintf "targets: %s\n" (String.concat ", " target_names);
  Printf.eprintf "defaults: --out ./build/, --target all\n";
  exit 1

let read_file filename =
  let ic = open_in filename in
  let src = In_channel.input_all ic in
  close_in ic;
  src

(* ── Option parsing ────────────────────────────────────────────────── *)

type compile_opts = {
  out_dir : string;
  target : string option;
}

let rec parse_compile_opts opts = function
  | [] -> opts
  | "--out" :: dir :: rest ->
    parse_compile_opts { opts with out_dir = dir } rest
  | "--target" :: name :: rest ->
    parse_compile_opts { opts with target = Some name } rest
  | _ -> usage ()

(* ── Commands ──────────────────────────────────────────────────────── *)

let cmd_compile filename opts =
  let written_files =
    filename
    |> read_file
    |> Idlc.Pipeline.compile ?target:opts.target
    |> (fun compile -> compile opts.out_dir)
  in
  Printf.printf "Generated:\n";
  written_files |> List.iter (Printf.printf "  %s\n")

let cmd_print filename =
  filename
  |> read_file
  |> Idlc.Pipeline.parse
  |> Idlc.Printer.pp_file
  |> print_string

(* ── Entry point ───────────────────────────────────────────────────── *)

let () =
  match Array.to_list Sys.argv |> List.tl with
  | ["print"; filename] ->
    cmd_print filename

  | "compile" :: filename :: rest ->
    let opts = parse_compile_opts { out_dir = "./build"; target = None } rest in
    cmd_compile filename opts

  | _ -> usage ()
