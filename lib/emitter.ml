(* Shared utilities for code generators — file IO, annotation helpers,
   and ref-type scanning. Target-specific type mappings live in their
   respective gen/*.ml modules. *)

(* ── File IO ─────────────────────────────────────────────────────── *)

let indent n s =
  let prefix = String.make (n * 2) ' ' in
  prefix ^ s

let write_file path content =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755
    end
  in
  mkdir_p dir;
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* ── Ref types ───────────────────────────────────────────────────── *)

let ref_id name = name ^ "Id"

let collect_ref_types (file : Ast.file) : string list =
  let refs = Hashtbl.create 16 in
  let rec scan_type = function
    | Ast.Ref name -> Hashtbl.replace refs name ()
    | Ast.Option t | Ast.List t | Ast.Set t -> scan_type t
    | Ast.Map (k, v) -> scan_type k; scan_type v
    | Ast.Tuple ts -> List.iter scan_type ts
    | Ast.Prim _ | Ast.Named _ -> ()
  in
  let scan_field (f : Ast.field) = scan_type f.typ in
  let scan_param (p : Ast.param) = scan_type p.typ in
  let scan_method (m : Ast.method_decl) =
    List.iter scan_param m.params;
    scan_type m.return_type
  in
  List.iter (function
    | Ast.Struct s -> List.iter scan_field s.fields
    | Ast.Service s -> List.iter scan_method s.methods
    | Ast.Union u -> List.iter scan_type u.variants
    | Ast.Enum _ | Ast.Const _ -> ()
  ) file.declarations;
  Hashtbl.fold (fun k () acc -> k :: acc) refs []
  |> List.sort String.compare

(* ── Annotation helpers ──────────────────────────────────────────── *)

let find_annotation name (anns : Ast.annotation list) =
  List.find_opt (fun (a : Ast.annotation) -> a.name = name) anns

let annotation_string_arg key (a : Ast.annotation) =
  match a.args with
  | Ast.Args_single (Ast.Lit_string s) when key = "" -> Some s
  | Ast.Args_named pairs ->
    (match List.assoc_opt key pairs with
     | Some (Ast.Lit_string s) -> Some s
     | _ -> None)
  | _ -> None
