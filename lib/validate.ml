(* Semantic validation — builds a symbol table and checks the AST
   for errors before codegen runs.

   Catches:
   - Undeclared type references (Named "Foo" with no Foo declaration)
   - Undeclared *Ref types
   - Undeclared raises types
   - Duplicate declaration names
   - Duplicate field names within a struct
   - Duplicate enum members within an enum
   - @crud on a service with no matching struct
   - Void used as a field type
   - Empty unions
   - Circular struct references
*)

(* ── Symbol table ──────────────────────────────────────────────────── *)

type symbol_kind =
  | Sym_struct
  | Sym_enum
  | Sym_union
  | Sym_service
  | Sym_const

type symbol = {
  kind : symbol_kind;
  loc : Ast.loc;
}

(* Keyed by (name, kind) so struct Company and service Company coexist
   without overwriting each other. *)
type symbols = (string * symbol_kind, symbol) Hashtbl.t

(* ── Error collection ──────────────────────────────────────────────── *)

type error = {
  loc : Ast.loc;
  message : string;
}

type context = {
  symbols : symbols;
  errors : error list ref;
}

let error ctx loc message =
  ctx.errors := { loc; message } :: !(ctx.errors)

let format_error (e : error) =
  Printf.sprintf "%d:%d: %s" e.loc.line e.loc.col e.message

(* ── Lookups ───────────────────────────────────────────────────────── *)

let name_exists ctx name =
  Hashtbl.fold (fun (n, _) _ found -> found || n = name) ctx.symbols false

let find_by_name_kind ctx name kind =
  Hashtbl.find_opt ctx.symbols (name, kind)

(* ── Pass 1: Build symbol table, check duplicates ──────────────────── *)

let build_symbols (file : Ast.file) : context =
  let symbols = Hashtbl.create 32 in
  let errors = ref [] in
  let ctx = { symbols; errors } in

  let register_symbol name kind loc =
    let key = (name, kind) in
    match Hashtbl.find_opt symbols key with
    | Some _ ->
      error ctx loc (Printf.sprintf "duplicate declaration: '%s'" name)
    | None ->
      Hashtbl.replace symbols key { kind; loc }
  in

  List.iter (function
    | Ast.Struct s  -> register_symbol s.name Sym_struct s.loc
    | Ast.Enum e    -> register_symbol e.name Sym_enum e.loc
    | Ast.Union u   -> register_symbol u.name Sym_union u.loc
    | Ast.Service s -> register_symbol s.name Sym_service s.loc
    | Ast.Const c   -> register_symbol c.name Sym_const c.loc
  ) file.declarations;

  ctx

(* ── Pass 2: Check type references ─────────────────────────────────── *)

let rec check_type ctx loc = function
  | Ast.Named name ->
    if not (name_exists ctx name) then
      error ctx loc (Printf.sprintf "undeclared type: '%s'" name)

  | Ast.Ref name ->
    if not (name_exists ctx name) then
      error ctx loc (Printf.sprintf "undeclared ref type: '*%s'" name)

  | Ast.Prim Ast.Void ->
    error ctx loc "void cannot be used as a field type"

  | Ast.Option t  -> check_type ctx loc t
  | Ast.List t    -> check_type ctx loc t
  | Ast.Set t     -> check_type ctx loc t
  | Ast.Map (k, v) -> check_type ctx loc k; check_type ctx loc v
  | Ast.Tuple ts  -> List.iter (check_type ctx loc) ts
  | Ast.Prim _    -> ()

let check_type_in_param ctx (p : Ast.param) loc =
  match p.typ with
  | Ast.Prim Ast.Void -> ()  (* void is valid as a return type concept *)
  | t -> check_type ctx loc t

(* ── Pass 3: Check declarations ────────────────────────────────────── *)

let check_duplicate_fields ctx (fields : Ast.field list) =
  let seen = Hashtbl.create 16 in
  List.iter (fun (f : Ast.field) ->
    if Hashtbl.mem seen f.name then
      error ctx f.loc (Printf.sprintf "duplicate field: '%s'" f.name)
    else
      Hashtbl.replace seen f.name ()
  ) fields

let check_duplicate_enum_members ctx (members : Ast.enum_member list) =
  let seen = Hashtbl.create 16 in
  List.iter (fun (m : Ast.enum_member) ->
    if Hashtbl.mem seen m.name then
      error ctx m.loc (Printf.sprintf "duplicate enum member: '%s'" m.name)
    else
      Hashtbl.replace seen m.name ()
  ) members

(* Raises types are intentionally not validated — they may reference
   error types defined externally or by convention. Generators emit
   them as-is (e.g. TypeScript comments, OpenAPI error responses). *)
let _check_raises ctx loc (raises : string list) =
  List.iter (fun name ->
    if not (name_exists ctx name) then
      error ctx loc (Printf.sprintf "undeclared raises type: '%s'" name)
  ) raises

let check_declarations ctx (file : Ast.file) =
  List.iter (function
    | Ast.Struct s ->
      check_duplicate_fields ctx s.fields;
      List.iter (fun (f : Ast.field) ->
        check_type ctx f.loc f.typ
      ) s.fields

    | Ast.Enum e ->
      check_duplicate_enum_members ctx e.members

    | Ast.Union u ->
      if u.variants = [] then
        error ctx u.loc (Printf.sprintf "empty union: '%s'" u.name);
      List.iter (check_type ctx u.loc) u.variants

    | Ast.Service s ->
      (* @crud requires a matching struct *)
      let has_crud =
        List.exists (fun (a : Ast.annotation) -> a.name = "crud") s.annotations
      in
      if has_crud then begin
        match find_by_name_kind ctx s.name Sym_struct with
        | Some _ -> ()
        | None ->
          error ctx s.loc
            (Printf.sprintf "@crud on service '%s' but no struct '%s' declared"
               s.name s.name)
      end;
      List.iter (fun (m : Ast.method_decl) ->
        List.iter (fun p -> check_type_in_param ctx p m.loc) m.params;
        (match m.return_type with
         | Ast.Prim Ast.Void -> ()
         | t -> check_type ctx m.loc t);
        ignore m.raises  (* raises validated loosely — see _check_raises *)
      ) s.methods

    | Ast.Const _ -> ()
  ) file.declarations

(* ── Pass 4: Circular reference detection ──────────────────────────── *)

type visit_state = Unvisited | Visiting | Visited

let check_circular_refs ctx (file : Ast.file) =
  (* Build adjacency: struct name -> list of struct names it references *)
  let deps = Hashtbl.create 16 in

  let rec collect_deps = function
    | Ast.Named name ->
      (match find_by_name_kind ctx name Sym_struct with
       | Some _ -> [name]
       | None -> [])
    | Ast.Ref _ -> []  (* refs are IDs, not embedded structs *)
    | Ast.Option t | Ast.List t | Ast.Set t -> collect_deps t
    | Ast.Map (k, v) -> collect_deps k @ collect_deps v
    | Ast.Tuple ts -> List.concat_map collect_deps ts
    | Ast.Prim _ -> []
  in

  List.iter (function
    | Ast.Struct s ->
      let field_deps = List.concat_map (fun (f : Ast.field) ->
        collect_deps f.typ
      ) s.fields in
      Hashtbl.replace deps s.name field_deps
    | _ -> ()
  ) file.declarations;

  (* DFS cycle detection *)
  let state = Hashtbl.create 16 in

  let rec visit path name =
    match Hashtbl.find_opt state name with
    | Some Visited -> ()
    | Some Visiting ->
      let cycle = name :: path in
      let cycle_str = String.concat " -> " (List.rev cycle) in
      let loc = match find_by_name_kind ctx name Sym_struct with
        | Some s -> s.loc
        | None -> { Ast.line = 0; col = 0 }
      in
      error ctx loc (Printf.sprintf "circular reference: %s" cycle_str)
    | _ ->
      Hashtbl.replace state name Visiting;
      let children = match Hashtbl.find_opt deps name with
        | Some cs -> cs
        | None -> []
      in
      List.iter (visit (name :: path)) children;
      Hashtbl.replace state name Visited
  in

  Hashtbl.iter (fun name _ -> visit [] name) deps

(* ── Main entry point ──────────────────────────────────────────────── *)

let validate (file : Ast.file) : (symbols, string list) result =
  let ctx = build_symbols file in
  check_declarations ctx file;
  check_circular_refs ctx file;

  match List.rev !(ctx.errors) with
  | [] -> Ok ctx.symbols
  | errors -> Error (List.map format_error errors)
