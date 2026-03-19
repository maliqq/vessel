(* Declaration parsing — struct, enum, union, service, const,
   and their sub-elements (fields, members, methods, params). *)

(* ── Fields and members ──────────────────────────────────────────── *)

let parse_field inp anns : Ast.field =
  let l = Input.loc inp in
  let typ = Types.parse_type inp in
  let name = Input.parse_ident inp in
  Input.skip_whitespace inp;
  let optional = match Input.peek inp with
    | Some '?' -> Input.advance inp; Input.skip_whitespace inp; true
    | _ -> false
  in
  Input.expect_char inp ';';
  { Ast.annotations = anns; typ; name; optional; loc = l }

let parse_enum_member inp anns : Ast.enum_member =
  let l = Input.loc inp in
  let name = Input.parse_ident inp in
  Input.expect_char inp ';';
  { Ast.annotations = anns; name; loc = l }

(* ── Service methods ─────────────────────────────────────────────── *)

let parse_param inp : Ast.param =
  let typ = Types.parse_type inp in
  let name = Input.parse_ident inp in
  { Ast.typ; name }

let parse_param_list inp : Ast.param list =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some ')' -> []
  | _ ->
    let first = parse_param inp in
    let rec go acc =
      Input.skip_whitespace inp;
      match Input.peek inp with
      | Some ',' -> Input.advance inp; Input.skip_whitespace inp; go (parse_param inp :: acc)
      | _ -> List.rev acc
    in
    go [first]

let parse_raises inp : string list =
  Input.skip_whitespace inp;
  if Input.match_string inp "raises" then begin
    let first = Input.parse_ident inp in
    let rec go acc =
      Input.skip_whitespace inp;
      match Input.peek inp with
      | Some ',' -> Input.advance inp; Input.skip_whitespace inp; go (Input.parse_ident inp :: acc)
      | _ -> List.rev acc
    in
    go [first]
  end else
    []

let parse_method inp anns : Ast.method_decl =
  let l = Input.loc inp in
  let name = Input.parse_ident inp in
  Input.expect_char inp '(';
  let params = parse_param_list inp in
  Input.expect_char inp ')';
  let return_type = Types.parse_type inp in
  let raises = parse_raises inp in
  Input.expect_char inp ';';
  { Ast.annotations = anns; name; params; return_type; raises; loc = l }

(* ── Top-level declarations ──────────────────────────────────────── *)

let parse_struct inp anns : Ast.decl =
  let l = Input.loc inp in
  ignore (Input.match_string inp "struct");
  let name = Input.parse_ident inp in
  Input.expect_char inp '{';
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some '}' -> Input.advance inp; Input.skip_whitespace inp; List.rev acc
    | _ ->
      let field_anns = Annotation.parse_annotations inp in
      go (parse_field inp field_anns :: acc)
  in
  let fields = go [] in
  Ast.Struct { annotations = anns; name; fields; loc = l }

let parse_enum inp anns : Ast.decl =
  let l = Input.loc inp in
  ignore (Input.match_string inp "enum");
  let name = Input.parse_ident inp in
  Input.expect_char inp '{';
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some '}' -> Input.advance inp; Input.skip_whitespace inp; List.rev acc
    | _ ->
      let member_anns = Annotation.parse_annotations inp in
      go (parse_enum_member inp member_anns :: acc)
  in
  let members = go [] in
  Ast.Enum { annotations = anns; name; members; loc = l }

let parse_union inp anns : Ast.decl =
  let l = Input.loc inp in
  ignore (Input.match_string inp "union");
  let name = Input.parse_ident inp in
  Input.expect_char inp '=';
  let first = Types.parse_type inp in
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some '|' -> Input.advance inp; Input.skip_whitespace inp; go (Types.parse_type inp :: acc)
    | _ -> List.rev acc
  in
  let variants = go [first] in
  Input.expect_char inp ';';
  Ast.Union { annotations = anns; name; variants; loc = l }

let parse_service inp anns : Ast.decl =
  let l = Input.loc inp in
  ignore (Input.match_string inp "service");
  let name = Input.parse_ident inp in
  Input.expect_char inp '{';
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some '}' -> Input.advance inp; Input.skip_whitespace inp; List.rev acc
    | _ ->
      let method_anns = Annotation.parse_annotations inp in
      go (parse_method inp method_anns :: acc)
  in
  let methods = go [] in
  Ast.Service { annotations = anns; name; methods; loc = l }

let parse_const inp : Ast.decl =
  let l = Input.loc inp in
  ignore (Input.match_string inp "const");
  let name = Input.parse_ident inp in
  Input.expect_char inp '=';
  let value = Literal.parse_literal inp in
  Input.expect_char inp ';';
  Ast.Const { name; value; loc = l }

(* ── Declaration dispatch ────────────────────────────────────────── *)

let parse_decl inp : Ast.decl =
  let anns = Annotation.parse_annotations inp in
  Input.skip_whitespace inp;
  let saved = Input.save inp in
  let word =
    let start = inp.pos in
    (match Input.peek inp with
     | Some ('a'..'z' | 'A'..'Z') ->
       let rec go () = match Input.peek inp with
         | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> Input.advance inp; go ()
         | _ -> ()
       in Input.advance inp; go ();
       String.sub inp.src start (inp.pos - start)
     | _ -> "")
  in
  Input.restore inp saved;
  match word with
  | "struct"  -> parse_struct inp anns
  | "enum"    -> parse_enum inp anns
  | "union"   -> parse_union inp anns
  | "service" -> parse_service inp anns
  | "const"   -> parse_const inp
  | w -> failwith (Printf.sprintf "%d:%d: expected declaration, got '%s'" inp.line inp.col w)
