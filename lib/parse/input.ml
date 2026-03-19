(* Scanner primitives — low-level input handling, character-by-character
   navigation, whitespace/comment skipping, and keyword matching. *)

type t = {
  src : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

let make src = { src; pos = 0; line = 1; col = 1 }

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

let save inp = (inp.pos, inp.line, inp.col)

let restore inp (pos, line, col) =
  inp.pos <- pos;
  inp.line <- line;
  inp.col <- col

(* ── Whitespace and comments ─────────────────────────────────────── *)

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

(* ── Character expectations ──────────────────────────────────────── *)

let expect_char inp c =
  skip_whitespace inp;
  match peek inp with
  | Some c' when c' = c -> advance inp; skip_whitespace inp
  | _ -> failwith (Printf.sprintf "%d:%d: expected '%c'" inp.line inp.col c)

(* ── Keyword / string matching ───────────────────────────────────── *)

let match_string inp s =
  skip_whitespace inp;
  let len = String.length s in
  if inp.pos + len <= String.length inp.src
     && String.sub inp.src inp.pos len = s then begin
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

(* ── Identifier parsing ──────────────────────────────────────────── *)

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
