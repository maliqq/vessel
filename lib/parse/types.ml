(* Type parsing — primitives, named types, refs, and generic
   containers (Option, List, Set, Map, Tuple). *)

(* ── Primitive types ─────────────────────────────────────────────── *)

let parse_primitive inp : Ast.typ option =
  let saved = Input.save inp in
  let try_kw s prim =
    if Input.match_string inp s then Some (Ast.Prim prim)
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
      Input.restore inp saved;
      None
    | (s, p) :: rest ->
      match try_kw s p with
      | Some _ as r -> r
      | None -> try_prims rest
  in
  try_prims prims

(* ── Set element (restricted to primitives and *Ref) ─────────────── *)

let parse_set_element inp : Ast.typ =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some '*' ->
    Input.advance inp; Input.skip_whitespace inp;
    Ast.Ref (Input.parse_ident inp)
  | _ ->
    match parse_primitive inp with
    | Some t -> t
    | None ->
      failwith (Printf.sprintf "%d:%d: Set element must be a primitive or *Ref type"
        inp.line inp.col)

(* ── Map key (restricted to primitives and *Ref) ─────────────────── *)

let parse_map_key inp : Ast.typ =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some '*' ->
    Input.advance inp; Input.skip_whitespace inp;
    Ast.Ref (Input.parse_ident inp)
  | _ ->
    match parse_primitive inp with
    | Some t -> t
    | None ->
      failwith (Printf.sprintf "%d:%d: Map key must be a primitive or *Ref type"
        inp.line inp.col)

(* ── Fallback: primitive or named type ───────────────────────────── *)

let parse_primitive_or_named inp =
  match parse_primitive inp with
  | Some t -> t
  | None -> Ast.Named (Input.parse_ident inp)

(* ── Full type parser ────────────────────────────────────────────── *)

let rec parse_type inp : Ast.typ =
  Input.skip_whitespace inp;
  match Input.peek inp with
  | Some '*' ->
    Input.advance inp; Input.skip_whitespace inp;
    Ast.Ref (Input.parse_ident inp)
  | _ ->
    if Input.match_string inp "Tuple" then
      parse_tuple inp
    else if Input.match_string inp "Option" then
      parse_generic1 (fun t -> Ast.Option t) inp
    else if Input.match_string inp "List" then
      parse_generic1 (fun t -> Ast.List t) inp
    else if Input.match_string inp "Set" then
      parse_set inp
    else if Input.match_string inp "Map" then
      parse_map inp
    else
      parse_primitive_or_named inp

and parse_tuple inp =
  Input.expect_char inp '<';
  let first = parse_type inp in
  let rec go acc =
    Input.skip_whitespace inp;
    match Input.peek inp with
    | Some ',' -> Input.advance inp; Input.skip_whitespace inp; go (parse_type inp :: acc)
    | _ -> List.rev acc
  in
  let types = go [first] in
  Input.expect_char inp '>';
  Ast.Tuple types

and parse_generic1 wrap inp =
  Input.expect_char inp '<';
  let t = parse_type inp in
  Input.expect_char inp '>';
  wrap t

and parse_set inp =
  Input.expect_char inp '<';
  let t = parse_set_element inp in
  Input.expect_char inp '>';
  Ast.Set t

and parse_map inp =
  Input.expect_char inp '<';
  let k = parse_map_key inp in
  Input.expect_char inp ',';
  let v = parse_type inp in
  Input.expect_char inp '>';
  Ast.Map (k, v)
