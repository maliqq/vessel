(* Literal and argument parsing — string literals, numbers, booleans,
   arrays, and annotation/directive argument lists. *)

(* ── String literals ─────────────────────────────────────────────── *)

let parse_string_literal inp =
  Input.skip_whitespace inp;
  Input.expect_char inp '"';
  let buf = Buffer.create 32 in
  let rec go () =
    match Input.peek inp with
    | None -> failwith "unterminated string"
    | Some '"' -> Input.advance inp; Input.skip_whitespace inp; Buffer.contents buf
    | Some c -> Input.advance inp; Buffer.add_char buf c; go ()
  in
  go ()

(* ── Literal values ──────────────────────────────────────────────── *)

let rec parse_literal inp : Ast.literal =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some '"' ->
    Ast.Lit_string (parse_string_literal inp)

  | Some '[' ->
    Input.advance inp; Input.skip_whitespace inp;
    let rec go acc =
      Input.skip_whitespace inp;
      match Input.peek inp with
      | Some ']' -> Input.advance inp; Input.skip_whitespace inp; Ast.Lit_array (List.rev acc)
      | _ ->
        let lit = parse_literal inp in
        Input.skip_whitespace inp;
        (match Input.peek inp with
         | Some ',' -> Input.advance inp; Input.skip_whitespace inp
         | _ -> ());
        go (lit :: acc)
    in
    go []

  | Some ('0'..'9' | '-') ->
    let start = inp.pos in
    (match Input.peek inp with
     | Some '-' -> Input.advance inp
     | _ -> ());
    let rec digits () = match Input.peek inp with
      | Some ('0'..'9') -> Input.advance inp; digits ()
      | _ -> ()
    in
    digits ();
    let is_float = match Input.peek inp with
      | Some '.' -> Input.advance inp; digits (); true
      | _ -> false
    in
    let s = String.sub inp.src start (inp.pos - start) in
    Input.skip_whitespace inp;
    if is_float then Ast.Lit_float (float_of_string s)
    else Ast.Lit_int (int_of_string s)

  | _ ->
    if Input.match_string inp "true" then Ast.Lit_bool true
    else if Input.match_string inp "false" then Ast.Lit_bool false
    else failwith (Printf.sprintf "%d:%d: expected literal value" inp.line inp.col)

(* ── Arguments (for annotations and directives) ──────────────────── *)

let parse_args inp : Ast.args =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some '(' ->
    Input.advance inp; Input.skip_whitespace inp;
    let saved = Input.save inp in
    let is_named =
      match Input.peek inp with
      | Some ('a'..'z' | 'A'..'Z' | '_') ->
        let rec skip_id () = match Input.peek inp with
          | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> Input.advance inp; skip_id ()
          | _ -> ()
        in
        Input.advance inp; skip_id ();
        Input.skip_whitespace inp;
        let result = match Input.peek inp with
          | Some '=' -> true
          | _ -> false
        in
        Input.restore inp saved;
        result
      | _ -> false
    in
    if is_named then begin
      let rec go acc =
        Input.skip_whitespace inp;
        match Input.peek inp with
        | Some ')' -> Input.advance inp; Input.skip_whitespace inp; Ast.Args_named (List.rev acc)
        | _ ->
          let key = Input.parse_ident inp in
          Input.expect_char inp '=';
          let value = parse_literal inp in
          Input.skip_whitespace inp;
          (match Input.peek inp with
           | Some ',' -> Input.advance inp; Input.skip_whitespace inp
           | _ -> ());
          go ((key, value) :: acc)
      in
      go []
    end else begin
      let lit = parse_literal inp in
      Input.skip_whitespace inp;
      (match Input.peek inp with
       | Some ')' -> Input.advance inp; Input.skip_whitespace inp
       | _ -> failwith (Printf.sprintf "%d:%d: expected ')'" inp.line inp.col));
      Ast.Args_single lit
    end
  | _ -> Ast.Args_none
