# Vessel

Define your shapes once. Generate the glue. Write the real logic by hand.

Vessel is a strict, contract-first IDL toolchain written in OCaml. It parses a small DSL into a typed AST, applies annotation-driven transforms, and generates deterministic code for target surfaces such as TypeScript, Rust, JSON Schema, and OpenAPI.

The generated code is not the product. The generated code is the shell:

- request and response shaping
- validation wiring
- transport boilerplate
- dispatch glue
- typed adapters around handwritten entrypoints

Generated files should be disposable and never edited by hand.

## Why

Most service code is boring glue:

- HTTP bindings
- CLI argument extraction
- schema wiring
- repetitive handler surfaces
- type marker plumbing

Vessel exists to generate that strict, repetitive layer so the handwritten code can stay focused on actual behavior.

## Mental Model

One file defines:

- shared domain shapes at file scope
- one or more `service` surfaces over those shapes

A `service` is an operation surface, not just a REST controller. In real projects that can mean:

- `service Api`
- `service SolanaSmartContracts`
- `service Engine`
- `service CLI`

The same shapes can be projected into different generated glue depending on annotations and target.

## Example

```idl
#version("1.0")

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

## Syntax Snapshot

Structs:

```idl
struct Profile {
  string name;
  string phone?;
  Option<string> bio;
}
```

Enums:

```idl
enum Status {
  ACTIVE;
  INACTIVE;
}
```

Unions:

```idl
union Result = Success | Error;
```

Services:

```idl
service Users {
  get(*User id) User raises NotFound;
  list() List<User>;
}
```

References:

```idl
*Company
```

`*Type` is a typed handle or ID, not an embedded value.

Annotations:

```idl
@crud
@rest(base="/api/v1")
@validate(min=1, max=100, required=true)
```

## Type Mapping

| IDL | TypeScript | Rust |
|---|---|---|
| `string` | `string` | `String` |
| `int` | `number` | `i32` |
| `int64` | `bigint` | `i64` |
| `float` | `number` | `f64` |
| `byte` | `number` | `u8` |
| `bool` | `boolean` | `bool` |
| `binary` | `Uint8Array` | `Vec<u8>` |
| `uuid` | `string` | `String` |
| `uuid_v7` | `string` | `String` |
| `void` | `void` | `()` |
| `List<T>` | `T[]` | `Vec<T>` |
| `Map<K, V>` | `Record<K, V>` | `HashMap<K, V>` |
| `Option<T>` | `T \| undefined` | `Option<T>` |
| `*Type` | `TypeId` | `TypeId` |

Generated type markers use Vessel-specific machine metadata such as `__T_CODEGEN_VESSEL`.

## What Exists Today

Current pipeline:

`DSL -> AST -> AST transforms -> validation -> target resolution -> codegen`

Current outputs:

- TypeScript types
- Rust types
- Zod schemas
- JSON Schema
- OpenAPI

Current extension pass:

- `@crud`

## CLI

Requires OCaml 5+ and dune 3+.

Build:

```sh
dune build
```

Compile a schema:

```sh
dune exec idlc -- compile schemas/demo.idl
```

Choose an output directory:

```sh
dune exec idlc -- compile schemas/demo.idl --out ./build
```

Choose a target:

```sh
dune exec idlc -- compile schemas/demo.idl --target ts
```

Print the parsed AST back into normalized syntax:

```sh
dune exec idlc -- print schemas/demo.idl
```

Run tests:

```sh
dune runtest
```

## Repo Layout

```text
idl.peg             PEG grammar reference
lib/ast.ml          AST types
lib/parser.ml       hand-written recursive-descent parser
lib/ext/            AST extension passes
lib/validate.ml     semantic validation
lib/gen/            target generators
lib/pipeline.ml     end-to-end compilation pipeline
bin/main.ml         CLI
schemas/            example schemas
test/               tests
```
