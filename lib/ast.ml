type loc = {
  line : int;
  col : int;
}

type literal =
  | Lit_string of string
  | Lit_int of int
  | Lit_float of float
  | Lit_bool of bool
  | Lit_array of literal list

type args =
  | Args_none
  | Args_single of literal
  | Args_named of (string * literal) list

type directive = {
  name : string;
  args : args;
  loc : loc;
}

type annotation = {
  name : string;
  args : args;
  loc : loc;
}

type primitive =
  | String
  | Int
  | Int64
  | Float
  | Byte
  | Bool
  | Binary
  | Uuid
  | Uuid_v7
  | Void

type typ =
  | Prim of primitive
  | Named of string
  | Ref of string
  | Option of typ
  | Tuple of typ list
  | List of typ
  | Set of typ
  | Map of typ * typ

type field = {
  annotations : annotation list;
  typ : typ;
  name : string;
  optional : bool;
  loc : loc;
}

type enum_member = {
  annotations : annotation list;
  name : string;
  loc : loc;
}

type param = {
  typ : typ;
  name : string;
}

type method_decl = {
  annotations : annotation list;
  name : string;
  params : param list;
  return_type : typ;
  raises : string list;
  loc : loc;
}

type decl =
  | Struct of {
      annotations : annotation list;
      name : string;
      fields : field list;
      loc : loc;
    }
  | Enum of {
      annotations : annotation list;
      name : string;
      members : enum_member list;
      loc : loc;
    }
  | Union of {
      annotations : annotation list;
      name : string;
      variants : typ list;
      loc : loc;
    }
  | Service of {
      annotations : annotation list;
      name : string;
      methods : method_decl list;
      loc : loc;
    }
  | Const of {
      name : string;
      value : literal;
      loc : loc;
    }

type file = {
  directives : directive list;
  declarations : decl list;
}
