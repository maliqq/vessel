(* Debug printer for the AST *)

let rec pp_type = function
  | Ast.Prim Ast.String -> "string"
  | Ast.Prim Ast.Int -> "int"
  | Ast.Prim Ast.Int64 -> "int64"
  | Ast.Prim Ast.Float -> "float"
  | Ast.Prim Ast.Byte -> "byte"
  | Ast.Prim Ast.Bool -> "bool"
  | Ast.Prim Ast.Binary -> "binary"
  | Ast.Prim Ast.Uuid -> "uuid"
  | Ast.Prim Ast.Uuid_v7 -> "uuid_v7"
  | Ast.Prim Ast.Void -> "void"
  | Ast.Named n -> n
  | Ast.Ref n -> "*" ^ n
  | Ast.Option t -> "Option<" ^ pp_type t ^ ">"
  | Ast.Tuple ts -> "Tuple<" ^ String.concat ", " (List.map pp_type ts) ^ ">"
  | Ast.List t -> "List<" ^ pp_type t ^ ">"
  | Ast.Set t -> "Set<" ^ pp_type t ^ ">"
  | Ast.Map (k, v) -> "Map<" ^ pp_type k ^ ", " ^ pp_type v ^ ">"

let rec pp_literal = function
  | Ast.Lit_string s -> "\"" ^ s ^ "\""
  | Ast.Lit_int i -> string_of_int i
  | Ast.Lit_float f -> string_of_float f
  | Ast.Lit_bool b -> string_of_bool b
  | Ast.Lit_array xs -> "[" ^ String.concat ", " (List.map pp_literal xs) ^ "]"

let pp_args = function
  | Ast.Args_none -> ""
  | Ast.Args_single lit -> "(" ^ pp_literal lit ^ ")"
  | Ast.Args_named pairs ->
    "(" ^ String.concat ", " (List.map (fun (k, v) ->
      k ^ "=" ^ pp_literal v
    ) pairs) ^ ")"

let pp_directive (d : Ast.directive) =
  "#" ^ d.name ^ pp_args d.args

let pp_annotation (a : Ast.annotation) =
  "@" ^ a.name ^ pp_args a.args

let pp_decl = function
  | Ast.Struct s ->
    let anns = List.map pp_annotation s.annotations in
    let fields = List.map (fun (f : Ast.field) ->
      let fa = List.map pp_annotation f.annotations in
      "  " ^ String.concat " " fa
      ^ (if fa <> [] then " " else "")
      ^ pp_type f.typ ^ " " ^ f.name
      ^ (if f.optional then "?" else "") ^ ";"
    ) s.fields in
    String.concat "\n" anns
    ^ (if anns <> [] then "\n" else "")
    ^ "struct " ^ s.name ^ " {\n"
    ^ String.concat "\n" fields ^ "\n}"
  | Ast.Enum e ->
    let anns = List.map pp_annotation e.annotations in
    let members = List.map (fun (m : Ast.enum_member) ->
      let ma = List.map pp_annotation m.annotations in
      "  " ^ String.concat " " ma
      ^ (if ma <> [] then " " else "")
      ^ m.name ^ ";"
    ) e.members in
    String.concat "\n" anns
    ^ (if anns <> [] then "\n" else "")
    ^ "enum " ^ e.name ^ " {\n"
    ^ String.concat "\n" members ^ "\n}"
  | Ast.Union u ->
    let anns = List.map pp_annotation u.annotations in
    String.concat "\n" anns
    ^ (if anns <> [] then "\n" else "")
    ^ "union " ^ u.name ^ " = "
    ^ String.concat " | " (List.map pp_type u.variants) ^ ";"
  | Ast.Service s ->
    let anns = List.map pp_annotation s.annotations in
    let methods = List.map (fun (m : Ast.method_decl) ->
      let ma = List.map pp_annotation m.annotations in
      let params = List.map (fun (p : Ast.param) ->
        pp_type p.typ ^ " " ^ p.name
      ) m.params in
      let raises = match m.raises with
        | [] -> ""
        | rs -> " raises " ^ String.concat ", " rs
      in
      "  " ^ String.concat " " ma
      ^ (if ma <> [] then " " else "")
      ^ m.name ^ "(" ^ String.concat ", " params ^ ") "
      ^ pp_type m.return_type ^ raises ^ ";"
    ) s.methods in
    String.concat "\n" anns
    ^ (if anns <> [] then "\n" else "")
    ^ "service " ^ s.name ^ " {\n"
    ^ String.concat "\n" methods ^ "\n}"
  | Ast.Const c ->
    "const " ^ c.name ^ " = " ^ pp_literal c.value ^ ";"

let pp_file file =
  let dirs = match file.Ast.directives with
    | [] -> ""
    | ds -> String.concat "\n" (List.map pp_directive ds) ^ "\n\n"
  in
  dirs ^ String.concat "\n\n" (List.map pp_decl file.Ast.declarations) ^ "\n"
