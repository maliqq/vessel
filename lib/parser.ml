(* Entry point for parsing — delegates to Parse.* submodules.

   Parse.Input     — scanner primitives (peek, advance, whitespace)
   Parse.Literal   — literals, string parsing, argument lists
   Parse.Types     — type expressions (primitives, generics, refs)
   Parse.Annotation — @annotations and #directives
   Parse.Decl      — struct, enum, union, service, const *)

let parse_file src : Ast.file =
  let inp = Parse.Input.make src in
  Parse.Input.skip_whitespace inp;

  let rec parse_directives acc =
    Parse.Input.skip_whitespace inp;
    match Parse.Input.peek inp with
    | Some '#' -> parse_directives (Parse.Annotation.parse_directive inp :: acc)
    | _ -> List.rev acc
  in
  let directives = parse_directives [] in

  let rec parse_decls acc =
    Parse.Input.skip_whitespace inp;
    if Parse.Input.eof inp then List.rev acc
    else parse_decls (Parse.Decl.parse_decl inp :: acc)
  in
  let declarations = parse_decls [] in

  { Ast.directives; declarations }
