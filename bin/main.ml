let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: idlc <file.idl>\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let src = In_channel.input_all ic in
  close_in ic;
  let ast = Idlc.Parser.parse_file src in
  print_string (Idlc.Printer.pp_file ast)
