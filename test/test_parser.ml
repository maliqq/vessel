let decls src = (Idlc.Parser.parse_file src).declarations

let test_struct () =
  let src = {|
    struct Profile {
      string name;
      string headline;
    }
  |} in
  match decls src with
  | [Idlc.Ast.Struct s] ->
    assert (s.name = "Profile");
    assert (List.length s.fields = 2);
    Printf.printf "test_struct: OK\n"
  | _ -> failwith "test_struct: unexpected AST"

let test_enum () =
  match decls {|
    enum EmploymentType {
      FULL_TIME;
      CONTRACT;
      TEMPORARY;
    }
  |} with
  | [Idlc.Ast.Enum e] ->
    assert (e.name = "EmploymentType");
    assert (List.length e.members = 3);
    Printf.printf "test_enum: OK\n"
  | _ -> failwith "test_enum: unexpected AST"

let test_union () =
  match decls {|
    union MediaContent = Photo | Video | Audio;
  |} with
  | [Idlc.Ast.Union u] ->
    assert (u.name = "MediaContent");
    assert (List.length u.variants = 3);
    Printf.printf "test_union: OK\n"
  | _ -> failwith "test_union: unexpected AST"

let test_union_with_ref () =
  match decls {|
    union SearchResult = *Profile | *Company | *Job;
  |} with
  | [Idlc.Ast.Union u] ->
    assert (u.name = "SearchResult");
    assert (List.length u.variants = 3);
    (match List.hd u.variants with
     | Idlc.Ast.Ref "Profile" -> ()
     | _ -> failwith "expected *Profile");
    Printf.printf "test_union_with_ref: OK\n"
  | _ -> failwith "test_union_with_ref: unexpected AST"

let test_optional_field_sugar () =
  match decls {|
    struct Profile {
      string name;
      string headline?;
      string phone?;
    }
  |} with
  | [Idlc.Ast.Struct s] ->
    assert (List.length s.fields = 3);
    assert (not (List.nth s.fields 0).optional);
    assert (List.nth s.fields 1).optional;
    assert (List.nth s.fields 2).optional;
    Printf.printf "test_optional_field_sugar: OK\n"
  | _ -> failwith "test_optional_field_sugar: unexpected AST"

let test_option_type () =
  match decls {|
    struct Profile {
      Option<string> phone;
      Option<*Company> employer;
    }
  |} with
  | [Idlc.Ast.Struct s] ->
    assert (List.length s.fields = 2);
    (match (List.nth s.fields 0).typ with
     | Idlc.Ast.Option (Idlc.Ast.Prim Idlc.Ast.String) -> ()
     | _ -> failwith "expected Option<string>");
    (match (List.nth s.fields 1).typ with
     | Idlc.Ast.Option (Idlc.Ast.Ref "Company") -> ()
     | _ -> failwith "expected Option<*Company>");
    Printf.printf "test_option_type: OK\n"
  | _ -> failwith "test_option_type: unexpected AST"

let test_map_keys () =
  match decls {|
    struct Index {
      Map<string, int> scores;
      Map<uuid, Profile> by_id;
      Map<*Profile, Dialog> dialogs;
    }
  |} with
  | [Idlc.Ast.Struct s] ->
    assert (List.length s.fields = 3);
    (match (List.nth s.fields 0).typ with
     | Idlc.Ast.Map (Idlc.Ast.Prim Idlc.Ast.String, Idlc.Ast.Prim Idlc.Ast.Int) -> ()
     | _ -> failwith "expected Map<string, int>");
    (match (List.nth s.fields 1).typ with
     | Idlc.Ast.Map (Idlc.Ast.Prim Idlc.Ast.Uuid, Idlc.Ast.Named "Profile") -> ()
     | _ -> failwith "expected Map<uuid, Profile>");
    (match (List.nth s.fields 2).typ with
     | Idlc.Ast.Map (Idlc.Ast.Ref "Profile", Idlc.Ast.Named "Dialog") -> ()
     | _ -> failwith "expected Map<*Profile, Dialog>");
    Printf.printf "test_map_keys: OK\n"
  | _ -> failwith "test_map_keys: unexpected AST"

let test_map_bare_complex_key_fails () =
  (try
     ignore (decls {|
       struct Bad {
         Map<Profile, string> m;
       }
     |});
     failwith "should have failed"
   with Failure msg ->
     assert (String.length msg > 0);
     Printf.printf "test_map_bare_complex_key_fails: OK (%s)\n" msg)

let test_new_primitives () =
  match decls {|
    struct Record {
      int64 big_count;
      byte flags;
      uuid id;
      uuid_v7 sortable_id;
    }
  |} with
  | [Idlc.Ast.Struct s] ->
    assert (List.length s.fields = 4);
    (match (List.nth s.fields 0).typ with
     | Idlc.Ast.Prim Idlc.Ast.Int64 -> ()
     | _ -> failwith "expected int64");
    (match (List.nth s.fields 1).typ with
     | Idlc.Ast.Prim Idlc.Ast.Byte -> ()
     | _ -> failwith "expected byte");
    (match (List.nth s.fields 2).typ with
     | Idlc.Ast.Prim Idlc.Ast.Uuid -> ()
     | _ -> failwith "expected uuid");
    (match (List.nth s.fields 3).typ with
     | Idlc.Ast.Prim Idlc.Ast.Uuid_v7 -> ()
     | _ -> failwith "expected uuid_v7");
    Printf.printf "test_new_primitives: OK\n"
  | _ -> failwith "test_new_primitives: unexpected AST"

let test_service () =
  match decls {|
    @crud
    @rest(base="/api/v1/companies")
    service Company {

      @context("hiring")
      add(*Company company, Job job) *Job;

      remove(*Company company, *Job job) void;

      @paginate
      listJobs(*Company company) List<*Job>;
    }
  |} with
  | [Idlc.Ast.Service s] ->
    assert (s.name = "Company");
    assert (List.length s.methods = 3);
    assert (List.length s.annotations = 2);
    assert ((List.nth s.annotations 0).name = "crud");
    assert ((List.nth s.annotations 1).name = "rest");
    (match (List.nth s.annotations 1).args with
     | Idlc.Ast.Args_named [("base", Idlc.Ast.Lit_string "/api/v1/companies")] -> ()
     | _ -> failwith "expected Args_named rest base");
    Printf.printf "test_service: OK\n"
  | _ -> failwith "test_service: unexpected AST"

let test_raises () =
  match decls {|
    @crud
    service Company {
      get(*Company id) Company raises NotFound;
      update(*Company id, Company data) void raises NotFound, Unauthorized, ValidationError;
      list() List<Company>;
    }
  |} with
  | [Idlc.Ast.Service s] ->
    assert (List.length s.methods = 3);
    let m0 = List.nth s.methods 0 in
    let m1 = List.nth s.methods 1 in
    let m2 = List.nth s.methods 2 in
    assert (m0.raises = ["NotFound"]);
    assert (m1.raises = ["NotFound"; "Unauthorized"; "ValidationError"]);
    assert (m2.raises = []);
    Printf.printf "test_raises: OK\n"
  | _ -> failwith "test_raises: unexpected AST"

let test_annotation_formats () =
  match decls {|
    struct Profile {
      @searchable
      string name;

      @deprecated("use email instead")
      string phone;

      @validate(min=1, max=100, required=true)
      string headline;
    }
  |} with
  | [Idlc.Ast.Struct s] ->
    let f0 = List.nth s.fields 0 in
    assert ((List.hd f0.annotations).name = "searchable");
    (match (List.hd f0.annotations).args with
     | Idlc.Ast.Args_none -> ()
     | _ -> failwith "expected Args_none");
    let f1 = List.nth s.fields 1 in
    (match (List.hd f1.annotations).args with
     | Idlc.Ast.Args_single (Idlc.Ast.Lit_string "use email instead") -> ()
     | _ -> failwith "expected Args_single string");
    let f2 = List.nth s.fields 2 in
    (match (List.hd f2.annotations).args with
     | Idlc.Ast.Args_named pairs ->
       assert (List.length pairs = 3);
       (match List.assoc "min" pairs with
        | Idlc.Ast.Lit_int 1 -> ()
        | _ -> failwith "expected min=1");
       (match List.assoc "max" pairs with
        | Idlc.Ast.Lit_int 100 -> ()
        | _ -> failwith "expected max=100");
       (match List.assoc "required" pairs with
        | Idlc.Ast.Lit_bool true -> ()
        | _ -> failwith "expected required=true");
     | _ -> failwith "expected Args_named");
    Printf.printf "test_annotation_formats: OK\n"
  | _ -> failwith "test_annotation_formats: unexpected AST"

let test_directives () =
  let ast = Idlc.Parser.parse_file {|
    #version("1.0")
    #target("wasm")
    #import("common.idl")

    struct Company {
      string name;
    }
  |} in
  assert (List.length ast.directives = 3);
  let d0 = List.nth ast.directives 0 in
  let d1 = List.nth ast.directives 1 in
  let d2 = List.nth ast.directives 2 in
  assert (d0.name = "version");
  (match d0.args with
   | Idlc.Ast.Args_single (Idlc.Ast.Lit_string "1.0") -> ()
   | _ -> failwith "expected version 1.0");
  assert (d1.name = "target");
  (match d1.args with
   | Idlc.Ast.Args_single (Idlc.Ast.Lit_string "wasm") -> ()
   | _ -> failwith "expected target wasm");
  assert (d2.name = "import");
  assert (List.length ast.declarations = 1);
  Printf.printf "test_directives: OK\n"

let test_directive_named_args () =
  let ast = Idlc.Parser.parse_file {|
    #config(debug=true, timeout=30)

    struct Foo { string x; }
  |} in
  assert (List.length ast.directives = 1);
  let d = List.hd ast.directives in
  assert (d.name = "config");
  (match d.args with
   | Idlc.Ast.Args_named pairs ->
     assert (List.length pairs = 2);
     (match List.assoc "debug" pairs with
      | Idlc.Ast.Lit_bool true -> ()
      | _ -> failwith "expected debug=true");
     (match List.assoc "timeout" pairs with
      | Idlc.Ast.Lit_int 30 -> ()
      | _ -> failwith "expected timeout=30");
   | _ -> failwith "expected Args_named");
  Printf.printf "test_directive_named_args: OK\n"

let test_roundtrip () =
  let src = {|#version("1.0")
#target("typescript")

struct Company {
  string name;
  string description?;
}

enum EmploymentType {
  FULL_TIME;
  CONTRACT;
}

union MediaContent = Photo | Video | Audio;

@crud
@rest(base="/api/v1/companies")
service Company {
  @context("hiring") add(*Company company, Job job) *Job;
  remove(*Company company, *Job job) void;
  get(*Company id) Company raises NotFound;
  @paginate listJobs(*Company company) List<*Job>;
}
|} in
  let ast = Idlc.Parser.parse_file src in
  let out = Idlc.Printer.pp_file ast in
  Printf.printf "--- roundtrip ---\n%s\n" out

let () =
  test_struct ();
  test_enum ();
  test_union ();
  test_union_with_ref ();
  test_optional_field_sugar ();
  test_option_type ();
  test_map_keys ();
  test_map_bare_complex_key_fails ();
  test_new_primitives ();
  test_service ();
  test_raises ();
  test_annotation_formats ();
  test_directives ();
  test_directive_named_args ();
  test_roundtrip ();
  Printf.printf "All tests passed.\n"
