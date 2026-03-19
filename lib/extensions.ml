(* Extensions — visitor-style AST transforms driven by annotations.

   Each extension declares:
   - which annotation it handles
   - a visitor that transforms declarations matching that annotation

   After all extensions run, the AST is fully expanded
   and downstream layers (codegen) see the enriched context.
*)

(* ── Extension type ────────────────────────────────────────────────── *)

type t = {
  name : string;
  visit_decl : Ast.decl -> Ast.decl;
}

(* ── Annotation helpers ────────────────────────────────────────────── *)

let has_annotation name (anns : Ast.annotation list) =
  List.exists (fun (a : Ast.annotation) -> a.name = name) anns

let without_annotation name (anns : Ast.annotation list) =
  List.filter (fun (a : Ast.annotation) -> a.name <> name) anns

let dummy_loc : Ast.loc = { line = 0; col = 0 }

(* ── @crud ─────────────────────────────────────────────────────────── *)

let crud_methods service_name : Ast.method_decl list =
  let ref_type = Ast.Ref service_name in
  let named_type = Ast.Named service_name in
  let method_ name params return_type raises =
    { Ast.annotations = []; name; params; return_type; raises; loc = dummy_loc }
  in
  [
    method_ "create"
      [{ typ = named_type; name = "data" }]
      ref_type [];

    method_ "get"
      [{ typ = ref_type; name = "id" }]
      named_type ["NotFound"];

    method_ "update"
      [{ typ = ref_type; name = "id" }; { typ = named_type; name = "data" }]
      named_type ["NotFound"];

    method_ "delete"
      [{ typ = ref_type; name = "id" }]
      (Ast.Prim Ast.Void) ["NotFound"];

    method_ "list"
      [] (Ast.List ref_type) [];
  ]

let visit_crud : Ast.decl -> Ast.decl = function
  | Ast.Service s when has_annotation "crud" s.annotations ->
    let expanded_methods = crud_methods s.name in
    let cleaned_annotations = without_annotation "crud" s.annotations in
    Ast.Service {
      s with
      annotations = cleaned_annotations;
      methods = expanded_methods @ s.methods;
    }
  | other -> other

let crud : t = {
  name = "crud";
  visit_decl = visit_crud;
}

(* ── Registry ──────────────────────────────────────────────────────── *)

let all : t list = [
  crud;
  (* future: paginate; *)
]

(* ── Apply ─────────────────────────────────────────────────────────── *)

let apply_one (ext : t) (file : Ast.file) : Ast.file =
  { file with declarations = List.map ext.visit_decl file.declarations }

let apply (file : Ast.file) : Ast.file =
  all |> List.fold_left (fun acc ext -> apply_one ext acc) file
