(* Hand-written recursive-descent parser for the IDL grammar.
   Follows the PEG spec in idl.peg. *)

type input = {
  src : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

let make_input src = { src; pos = 0; line = 1; col = 1 }

let loc inp : Ast.loc = { line = inp.line; col = inp.col }

let eof inp = inp.pos >= String.length inp.src

let peek inp = if eof inp then None else Some inp.src.[inp.pos]

let advance inp =
  if not (eof inp) then begin
    if inp.src.[inp.pos] = '\n' then begin
      inp.line <- inp.line + 1;
      inp.col <- 1
    end else
      inp.col <- inp.col + 1;
    inp.pos <- inp.pos + 1
  end

let skip_whitespace inp =
  let rec go () =
    match peek inp with
    | Some (' ' | '\t' | '\r' | '\n') -> advance inp; go ()
    | Some '/' when inp.pos + 1 < String.length inp.src
                 && inp.src.[inp.pos + 1] = '/' ->
      advance inp; advance inp;
      let rec skip_line () =
        match peek inp with
        | None | Some '\n' -> if not (eof inp) then advance inp
        | _ -> advance inp; skip_line ()
      in
      skip_line (); go ()
    | _ -> ()
  in
  go ()

let expect_char inp c =
  skip_whitespace inp;
  match peek inp with
  | Some c' when c' = c -> advance inp; skip_whitespace inp
  | _ -> failwith (Printf.sprintf "%d:%d: expected '%c'" inp.line inp.col c)

let match_string inp s =
  skip_whitespace inp;
  let len = String.length s in
  if inp.pos + len <= String.length inp.src
     && String.sub inp.src inp.pos len = s then begin
    (* Check word boundary for alpha keywords *)
    let after = inp.pos + len in
    if after < String.length inp.src then
      match inp.src.[after] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> false
      | _ ->
        for _ = 1 to len do advance inp done;
        skip_whitespace inp; true
    else begin
      for _ = 1 to len do advance inp done;
      skip_whitespace inp; true
    end
  end else
    false

let parse_ident inp =
  skip_whitespace inp;
  let start = inp.pos in
  (match peek inp with
   | Some ('a'..'z' | 'A'..'Z' | '_') -> advance inp
   | _ -> failwith (Printf.sprintf "%d:%d: expected identifier" inp.line inp.col));
  let rec go () =
    match peek inp with
    | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> advance inp; go ()
    | _ -> ()
  in
  go ();
  let id = String.sub inp.src start (inp.pos - start) in
  skip_whitespace inp;
  id

let parse_string_literal inp =
  skip_whitespace inp;
  expect_char inp '"';
  let buf = Buffer.create 32 in
  let rec go () =
    match peek inp with
    | None -> failwith "unterminated string"
    | Some '"' -> advance inp; skip_whitespace inp; Buffer.contents buf
    | Some c -> advance inp; Buffer.add_char buf c; go ()
  in
  go ()

let parse_primitive inp : Ast.typ option =
  let saved_pos = inp.pos and saved_line = inp.line and saved_col = inp.col in
  let try_kw s prim =
    if match_string inp s then Some (Ast.Prim prim)
    else None
  in
  let prims = [
    ("string", Ast.String); ("int64", Ast.Int64); ("int", Ast.Int);
    ("float", Ast.Float); ("byte", Ast.Byte); ("bool", Ast.Bool);
    ("binary", Ast.Binary); ("uuid_v7", Ast.Uuid_v7); ("uuid", Ast.Uuid);
    ("void", Ast.Void);
  ] in
  let rec try_prims = function
    | [] ->
      inp.pos <- saved_pos; inp.line <- saved_line; inp.col <- saved_col;
      None
    | (s, p) :: rest ->
      match try_kw s p with
      | Some _ as r -> r
      | None -> try_prims rest
  in
  try_prims prims

let rec parse_type inp : Ast.typ =
  skip_whitespace inp;
  match peek inp with
  | Some '*' ->
    advance inp; skip_whitespace inp;
    let name = parse_ident inp in
    Ast.Ref name
  | _ ->
    if match_string inp "Option" then begin
      expect_char inp '<';
      let t = parse_type inp in
      expect_char inp '>';
      Ast.Option t
    end else if match_string inp "List" then begin
      expect_char inp '<';
      let t = parse_type inp in
      expect_char inp '>';
      Ast.List t
    end else if match_string inp "Set" then begin
      expect_char inp '<';
      let t = parse_set_element inp in
      expect_char inp '>';
      Ast.Set t
    end else if match_string inp "Map" then begin
      expect_char inp '<';
      let k = parse_map_key inp in
      expect_char inp ',';
      let v = parse_type inp in
      expect_char inp '>';
      Ast.Map (k, v)
    end else
      parse_primitive_or_named inp

and parse_set_element inp : Ast.typ =
  skip_whitespace inp;
  match peek inp with
  | Some '*' ->
    advance inp; skip_whitespace inp;
    let name = parse_ident inp in
    Ast.Ref name
  | _ ->
    match parse_primitive inp with
    | Some t -> t
    | None ->
      failwith (Printf.sprintf "%d:%d: Set element must be a primitive or *Ref type" inp.line inp.col)

and parse_map_key inp : Ast.typ =
  skip_whitespace inp;
  match peek inp with
  | Some '*' ->
    advance inp; skip_whitespace inp;
    let name = parse_ident inp in
    Ast.Ref name
  | _ ->
    match parse_primitive inp with
    | Some t -> t
    | None ->
      failwith (Printf.sprintf "%d:%d: Map key must be a primitive or *Ref type" inp.line inp.col)

and parse_primitive_or_named inp =
  match parse_primitive inp with
  | Some t -> t
  | None -> Ast.Named (parse_ident inp)

let parse_literal inp : Ast.literal =
  skip_whitespace inp;
  match peek inp with
  | Some '"' ->
    let s = parse_string_literal inp in
    Ast.Lit_string s
  | Some ('0'..'9' | '-') ->
    let start = inp.pos in
    (match peek inp with
     | Some '-' -> advance inp
     | _ -> ());
    let rec digits () = match peek inp with
      | Some ('0'..'9') -> advance inp; digits ()
      | _ -> ()
    in
    digits ();
    let is_float = match peek inp with
      | Some '.' -> advance inp; digits (); true
      | _ -> false
    in
    let s = String.sub inp.src start (inp.pos - start) in
    skip_whitespace inp;
    if is_float then Ast.Lit_float (float_of_string s)
    else Ast.Lit_int (int_of_string s)
  | _ ->
    if match_string inp "true" then Ast.Lit_bool true
    else if match_string inp "false" then Ast.Lit_bool false
    else failwith (Printf.sprintf "%d:%d: expected literal value" inp.line inp.col)

let parse_args inp : Ast.args =
  skip_whitespace inp;
  match peek inp with
  | Some '(' ->
    advance inp; skip_whitespace inp;
    (* peek ahead to distinguish single literal from key=value pairs *)
    let saved_pos = inp.pos and saved_line = inp.line and saved_col = inp.col in
    let is_named =
      match peek inp with
      | Some ('a'..'z' | 'A'..'Z' | '_') ->
        let rec skip_id () = match peek inp with
          | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> advance inp; skip_id ()
          | _ -> ()
        in
        advance inp; skip_id ();
        skip_whitespace inp;
        let result = match peek inp with
          | Some '=' -> true
          | _ -> false
        in
        inp.pos <- saved_pos; inp.line <- saved_line; inp.col <- saved_col;
        result
      | _ -> false
    in
    if is_named then begin
      let rec go acc =
        skip_whitespace inp;
        match peek inp with
        | Some ')' -> advance inp; skip_whitespace inp; Ast.Args_named (List.rev acc)
        | _ ->
          let key = parse_ident inp in
          expect_char inp '=';
          let value = parse_literal inp in
          skip_whitespace inp;
          (match peek inp with
           | Some ',' -> advance inp; skip_whitespace inp
           | _ -> ());
          go ((key, value) :: acc)
      in
      go []
    end else begin
      let lit = parse_literal inp in
      skip_whitespace inp;
      (match peek inp with
       | Some ')' -> advance inp; skip_whitespace inp
       | _ -> failwith (Printf.sprintf "%d:%d: expected ')'" inp.line inp.col));
      Ast.Args_single lit
    end
  | _ -> Ast.Args_none

let parse_directive inp : Ast.directive =
  let l = loc inp in
  expect_char inp '#';
  let name = parse_ident inp in
  let args = parse_args inp in
  { Ast.name; args; loc = l }

let parse_annotation inp : Ast.annotation =
  let l = loc inp in
  expect_char inp '@';
  let name = parse_ident inp in
  let args = parse_args inp in
  { Ast.name; args; loc = l }

let parse_annotations inp =
  let rec go acc =
    skip_whitespace inp;
    match peek inp with
    | Some '@' -> go (parse_annotation inp :: acc)
    | _ -> List.rev acc
  in
  go []

let parse_field inp anns : Ast.field =
  let l = loc inp in
  let typ = parse_type inp in
  let name = parse_ident inp in
  skip_whitespace inp;
  let optional = match peek inp with
    | Some '?' -> advance inp; skip_whitespace inp; true
    | _ -> false
  in
  expect_char inp ';';
  { Ast.annotations = anns; typ; name; optional; loc = l }

let parse_enum_member inp anns : Ast.enum_member =
  let l = loc inp in
  let name = parse_ident inp in
  expect_char inp ';';
  { Ast.annotations = anns; name; loc = l }

let parse_param inp : Ast.param =
  let typ = parse_type inp in
  let name = parse_ident inp in
  { Ast.typ; name }

let parse_param_list inp : Ast.param list =
  skip_whitespace inp;
  match peek inp with
  | Some ')' -> []
  | _ ->
    let first = parse_param inp in
    let rec go acc =
      skip_whitespace inp;
      match peek inp with
      | Some ',' -> advance inp; skip_whitespace inp; go (parse_param inp :: acc)
      | _ -> List.rev acc
    in
    go [first]

let parse_raises inp : string list =
  skip_whitespace inp;
  if match_string inp "raises" then begin
    let first = parse_ident inp in
    let rec go acc =
      skip_whitespace inp;
      match peek inp with
      | Some ',' -> advance inp; skip_whitespace inp; go (parse_ident inp :: acc)
      | _ -> List.rev acc
    in
    go [first]
  end else
    []

let parse_method inp anns : Ast.method_decl =
  let l = loc inp in
  let name = parse_ident inp in
  expect_char inp '(';
  let params = parse_param_list inp in
  expect_char inp ')';
  let return_type = parse_type inp in
  let raises = parse_raises inp in
  expect_char inp ';';
  { Ast.annotations = anns; name; params; return_type; raises; loc = l }

let parse_struct inp anns : Ast.decl =
  let l = loc inp in
  ignore (match_string inp "struct");
  let name = parse_ident inp in
  expect_char inp '{';
  let rec go acc =
    skip_whitespace inp;
    match peek inp with
    | Some '}' -> advance inp; skip_whitespace inp; List.rev acc
    | _ ->
      let field_anns = parse_annotations inp in
      go (parse_field inp field_anns :: acc)
  in
  let fields = go [] in
  Ast.Struct { annotations = anns; name; fields; loc = l }

let parse_enum inp anns : Ast.decl =
  let l = loc inp in
  ignore (match_string inp "enum");
  let name = parse_ident inp in
  expect_char inp '{';
  let rec go acc =
    skip_whitespace inp;
    match peek inp with
    | Some '}' -> advance inp; skip_whitespace inp; List.rev acc
    | _ ->
      let member_anns = parse_annotations inp in
      go (parse_enum_member inp member_anns :: acc)
  in
  let members = go [] in
  Ast.Enum { annotations = anns; name; members; loc = l }

let parse_union inp anns : Ast.decl =
  let l = loc inp in
  ignore (match_string inp "union");
  let name = parse_ident inp in
  expect_char inp '=';
  let first = parse_type inp in
  let rec go acc =
    skip_whitespace inp;
    match peek inp with
    | Some '|' -> advance inp; skip_whitespace inp; go (parse_type inp :: acc)
    | _ -> List.rev acc
  in
  let variants = go [first] in
  expect_char inp ';';
  Ast.Union { annotations = anns; name; variants; loc = l }

let parse_service inp anns : Ast.decl =
  let l = loc inp in
  ignore (match_string inp "service");
  let name = parse_ident inp in
  expect_char inp '{';
  let rec go acc =
    skip_whitespace inp;
    match peek inp with
    | Some '}' -> advance inp; skip_whitespace inp; List.rev acc
    | _ ->
      let method_anns = parse_annotations inp in
      go (parse_method inp method_anns :: acc)
  in
  let methods = go [] in
  Ast.Service { annotations = anns; name; methods; loc = l }

let parse_decl inp : Ast.decl =
  let anns = parse_annotations inp in
  skip_whitespace inp;
  let saved_pos = inp.pos and saved_line = inp.line and saved_col = inp.col in
  let word =
    let start = inp.pos in
    (match peek inp with
     | Some ('a'..'z' | 'A'..'Z') ->
       let rec go () = match peek inp with
         | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> advance inp; go ()
         | _ -> ()
       in advance inp; go ();
       String.sub inp.src start (inp.pos - start)
     | _ -> "")
  in
  (* restore position — match_string in parse_* will re-consume *)
  inp.pos <- saved_pos; inp.line <- saved_line; inp.col <- saved_col;
  match word with
  | "struct"  -> parse_struct inp anns
  | "enum"    -> parse_enum inp anns
  | "union"   -> parse_union inp anns
  | "service" -> parse_service inp anns
  | w -> failwith (Printf.sprintf "%d:%d: expected declaration, got '%s'" inp.line inp.col w)

let parse_file src : Ast.file =
  let inp = make_input src in
  skip_whitespace inp;
  let rec parse_directives acc =
    skip_whitespace inp;
    match peek inp with
    | Some '#' -> parse_directives (parse_directive inp :: acc)
    | _ -> List.rev acc
  in
  let directives = parse_directives [] in
  let rec parse_decls acc =
    skip_whitespace inp;
    if eof inp then List.rev acc
    else parse_decls (parse_decl inp :: acc)
  in
  let declarations = parse_decls [] in
  { Ast.directives; declarations }
