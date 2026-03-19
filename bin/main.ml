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

let cmd_compile filename out_dir =
  let written_files =
    filename
    |> read_file
    |> Idlc.Pipeline.compile
    |> (fun compile -> compile out_dir)
  in
  Printf.printf "Generated:\n";
  written_files |> List.iter (Printf.printf "  %s\n")

let cmd_print filename =
  filename
  |> read_file
  |> Idlc.Pipeline.parse
  |> Idlc.Printer.pp_file
  |> print_string

let () =
  match Array.to_list Sys.argv |> List.tl with
  | ["print"; filename] ->
    cmd_print filename

  | "compile" :: filename :: rest ->
    let out_dir = match rest with
      | ["--out"; dir] -> dir
      | [] -> "./build"
      | _ -> usage ()
    in
    cmd_compile filename out_dir

  | _ -> usage ()
