(* @crud — injects create/get/update/delete/list methods into services *)

let dummy_loc : Ast.loc = { line = 0; col = 0 }

(* ── Annotation matching ───────────────────────────────────────────── *)

let has_crud (anns : Ast.annotation list) =
  List.exists (fun (a : Ast.annotation) -> a.name = "crud") anns

let without_crud (anns : Ast.annotation list) =
  List.filter (fun (a : Ast.annotation) -> a.name <> "crud") anns

(* ── Method builder ────────────────────────────────────────────────── *)

let make_method name params return_type raises =
  { Ast.annotations = []; name; params; return_type; raises; loc = dummy_loc }

(* ── CRUD method set ───────────────────────────────────────────────── *)

let crud_methods service_name : Ast.method_decl list =
  let ref_type = Ast.Ref service_name in
  let named_type = Ast.Named service_name in
  [
    make_method "create"
      [{ typ = named_type; name = "data" }]
      ref_type
      [];

    make_method "get"
      [{ typ = ref_type; name = "id" }]
      named_type
      ["NotFound"];

    make_method "update"
      [{ typ = ref_type; name = "id" }; { typ = named_type; name = "data" }]
      named_type
      ["NotFound"];

    make_method "delete"
      [{ typ = ref_type; name = "id" }]
      (Ast.Prim Ast.Void)
      ["NotFound"];

    make_method "list"
      []
      (Ast.List ref_type)
      [];
  ]

(* ── Visitor ───────────────────────────────────────────────────────── *)

let visit_decl : Ast.decl -> Ast.decl = function
  | Ast.Service s when has_crud s.annotations ->
    let expanded = crud_methods s.name in
    let cleaned = without_crud s.annotations in
    Ast.Service { s with annotations = cleaned; methods = expanded @ s.methods }

  | other -> other

let apply (file : Ast.file) : Ast.file =
  { file with declarations = List.map visit_decl file.declarations }
