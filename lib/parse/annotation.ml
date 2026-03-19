(* Annotation and directive parsing — @name:target(args) and #name(args). *)

(* ── Directives ──────────────────────────────────────────────────── *)

let parse_directive inp : Ast.directive =
  let l = Input.loc inp in
  Input.expect_char inp '#';
  let name = Input.parse_ident inp in
  let args = Literal.parse_args inp in
  { Ast.name; args; loc = l }

(* ── Annotations ─────────────────────────────────────────────────── *)

let parse_annotation inp : Ast.annotation =
  let l = Input.loc inp in
  Input.expect_char inp '@';
  let name = Input.parse_ident inp in
  let target = match Input.peek inp with
    | Some ':' -> Input.advance inp; Input.skip_whitespace inp; Some (Input.parse_ident inp)
    | _ -> None
  in
  let args = Literal.parse_args inp in
  { Ast.name; target; args; loc = l }

let parse_annotations inp =
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some '@' -> go (parse_annotation inp :: acc)
    | _ -> List.rev acc
  in
  go []
