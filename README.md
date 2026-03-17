# Vessel

A strict, contract-first IDL compiler. OCaml parser, TypeScript/Bun code generator.

Write a small schema, generate boring scaffolding, handwrite the logic that matters.

## Example

```
#version("1.0")
#target("typescript")

struct Company {
  string name;
  string description?;
}

struct Job {
  string title;
  *Company company;
}

enum EmploymentType {
  FULL_TIME;
  CONTRACT;
}

union MediaContent = Photo | Video | Audio;

@crud
@rest("/api/v1/companies")
service Company {
  @context("hiring")
  add(*Company company, Job job) *Job raises ValidationError;

  remove(*Company company, *Job job) void raises NotFound, Unauthorized;

  @paginate
  listJobs(*Company company) List<*Job>;
}
```

## Types

| IDL | TypeScript | Rust |
|---|---|---|
| `string` | `string` | `String` |
| `int` | `number` | `i32` |
| `int64` | `bigint` | `i64` |
| `float` | `number` | `f64` |
| `byte` | `number` | `u8` |
| `bool` | `boolean` | `bool` |
| `binary` | `Uint8Array` | `Vec<u8>` |
| `uuid` | `string` (branded) | `Uuid` |
| `uuid_v7` | `string` (branded) | `Uuid` |
| `void` | `void` | `()` |
| `List<T>` | `T[]` | `Vec<T>` |
| `Map<K, V>` | `Record<K, V>` | `HashMap<K, V>` |
| `Option<T>` | `T \| undefined` | `Option<T>` |
| `*Type` | `TypeId` (branded string) | `TypeId` |

## Syntax at a glance

**Struct** — fields with types, `?` marks optional:
```
struct Profile {
  string name;
  string phone?;       // sugar for Option<string>
  Option<string> bio;  // explicit form
}
```

**Enum** — closed set, SCREAMING_CASE members:
```
enum Status { ACTIVE; INACTIVE; }
```

**Union** — sum type, maps to TS union:
```
union Result = Success | Error;
```

**Service** — methods with optional `raises`:
```
service Users {
  get(*User id) User raises NotFound;
  list() List<User>;
}
```

**References** — `*Type` is a typed handle (UUID/ID), not a value:
```
*Company  // reference to a Company, not an embedded Company
```

**Map keys** — must be primitive or `*Ref`:
```
Map<string, int>       // ok
Map<*Profile, Dialog>  // ok (keyed by reference/ID)
Map<Profile, string>   // error
```

**Directives** — file-level metadata with `#`, before any declarations:
```
#version("1.0")
#target("typescript")
#config(debug=true, timeout=30)
```

**Annotations** — declaration-level metadata with `@`:
```
@crud                                    // bare
@rest(base="/api/v1")                        // single value
@validate(min=1, max=100, required=true) // named key=value pairs
```

## Build

Requires OCaml 5+ and dune 3+.

```
dune build
dune exec idlc -- schemas/demo.idl
dune runtest
```

## Project layout

```
idl.peg          PEG grammar spec
lib/ast.ml       AST types
lib/parser.ml    recursive-descent parser
lib/printer.ml   AST pretty-printer
bin/main.ml      CLI entry point
test/            parser tests
schemas/         example .idl files
```
