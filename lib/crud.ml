(* @crud expansion: injects create/get/update/delete/list methods into services *)

let dummy_loc : Ast.loc = { line = 0; col = 0 }

let has_crud anns =
  List.exists (fun (a : Ast.annotation) -> a.name = "crud") anns

let without_crud anns =
  List.filter (fun (a : Ast.annotation) -> a.name <> "crud") anns

let crud_methods name : Ast.method_decl list =
  let ref_type = Ast.Ref name in
  let named_type = Ast.Named name in
  [
    { annotations = [];
      name = "create";
      params = [{ typ = named_type; name = "data" }];
      return_type = ref_type;
      raises = [];
      loc = dummy_loc };
    { annotations = [];
      name = "get";
      params = [{ typ = ref_type; name = "id" }];
      return_type = named_type;
      raises = ["NotFound"];
      loc = dummy_loc };
    { annotations = [];
      name = "update";
      params = [{ typ = ref_type; name = "id" };
                { typ = named_type; name = "data" }];
      return_type = named_type;
      raises = ["NotFound"];
      loc = dummy_loc };
    { annotations = [];
      name = "delete";
      params = [{ typ = ref_type; name = "id" }];
      return_type = Ast.Prim Ast.Void;
      raises = ["NotFound"];
      loc = dummy_loc };
    { annotations = [];
      name = "list";
      params = [];
      return_type = Ast.List ref_type;
      raises = [];
      loc = dummy_loc };
  ]

let expand_decl = function
  | Ast.Service s when has_crud s.annotations ->
    Ast.Service {
      annotations = without_crud s.annotations;
      name = s.name;
      methods = crud_methods s.name @ s.methods;
      loc = s.loc;
    }
  | d -> d

let expand (file : Ast.file) : Ast.file =
  { file with declarations = List.map expand_decl file.declarations }
