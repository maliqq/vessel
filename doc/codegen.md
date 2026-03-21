# Vessel — Code Generation

## Purpose

Vessel code generation is for strict, deterministic glue.

The generator should produce the boring shell around handwritten logic:

- type definitions
- schema definitions
- route descriptions
- handler or trait surfaces
- transport and adapter boilerplate

The generator should not be treated as a place for handwritten business logic.

Generated code is expected to be:

- auto-generated
- disposable
- reproducible
- not edited by hand

## Pipeline

Current flow:

```text
IDL source -> Parser -> AST -> AST transforms -> Validation -> Target resolution -> Code generation -> Emit
```

More concretely:

1. Parse source into the shared AST
2. Apply AST-to-AST extensions such as `@crud`
3. Validate the resulting AST
4. Resolve target-specific annotations
5. Generate target output
6. Write generated files

There is no IR or lowered representation between the parsed AST and the target generators.

## Current Targets

Current implemented outputs:

- TypeScript types
- Zod schemas
- JSON Schema
- OpenAPI
- Rust types

## Type Mapping

### TypeScript

| IDL | TypeScript |
|---|---|
| `string` | `string` |
| `int` | `number` |
| `int64` | `bigint` |
| `float` | `number` |
| `byte` | `number` |
| `bool` | `boolean` |
| `binary` | `Uint8Array` |
| `uuid` | `string` |
| `uuid_v7` | `string` |
| `void` | `void` |
| `List<T>` | `T[]` |
| `Set<T>` | `Set<T>` |
| `Map<K, V>` | `Record<K, V>` |
| `Option<T>` | `T \| undefined` |
| `Tuple<A, B>` | `[A, B]` |
| `*Type` | `TypeId` |

Reference IDs are generated as branded string-like aliases. The current generator uses Vessel-specific marker metadata rather than a plain `__brand` field name.

### Rust

| IDL | Rust |
|---|---|
| `string` | `String` |
| `int` | `i32` |
| `int64` | `i64` |
| `float` | `f64` |
| `byte` | `u8` |
| `bool` | `bool` |
| `binary` | `Vec<u8>` |
| `uuid` | `String` |
| `uuid_v7` | `String` |
| `void` | `()` |
| `List<T>` | `Vec<T>` |
| `Set<T>` | `HashSet<T>` |
| `Map<K, V>` | `HashMap<K, V>` |
| `Option<T>` | `Option<T>` |
| `Tuple<A, B>` | `(A, B)` |
| `*Type` | `TypeId` |

Rust output is intentionally straightforward reflection of the AST:

- structs become `pub struct`
- enums become `pub enum`
- unions become `pub enum`
- services become `pub trait`
- refs become `pub type FooId = String;`

## Reference Types

`*Company` means a typed reference or handle, not an embedded value.

In generated code that becomes a generated ID type, for example:

- TypeScript: `CompanyId`
- Rust: `CompanyId`

The representation is target-specific, but the semantic role is the same: a typed identity handle.

## What `@crud` Does

`@crud` is an AST extension pass, not a special-case generator hack.

It injects methods like:

- `create`
- `get`
- `update`
- `delete`
- `list`

Those generated methods then flow through validation and code generation the same way as handwritten service methods.

## Current Generator Behavior

### TypeScript types

The TypeScript generator currently emits:

- ref ID aliases
- constants
- interfaces for structs
- string-union types for enums
- union aliases
- `*Handlers` interfaces for services

Method `raises` clauses are currently emitted as comments on the generated handler methods.

### Zod schemas

The Zod generator currently emits:

- ref ID schemas
- struct schemas
- enum schemas
- union schemas
- request schemas for service methods with parameters

### JSON Schema

The JSON Schema generator currently emits:

- `$defs` entries for ref IDs
- struct definitions
- enum definitions
- union definitions
- const definitions

Services are not emitted into JSON Schema.

### OpenAPI

The OpenAPI generator currently emits:

- `paths` for services
- `components.schemas` for structs, enums, unions, and ref IDs

The current route mapping is convention-based:

- `create` -> `POST <base>`
- `list` -> `GET <base>`
- `get` -> `GET <base>/{id}`
- `update` -> `PUT <base>/{id}`
- `delete` -> `DELETE <base>/{id}`
- other methods -> `POST <base>/<method>`

This is intentionally simple. It is useful generated glue, not a full routing DSL.

### Rust

The Rust generator currently emits:

- `pub type` aliases for ref IDs
- `pub const` values
- `pub struct`
- `pub enum`
- `pub trait` handler surfaces for services

When needed, it also emits:

- `use std::collections::HashSet;`
- `use std::collections::HashMap;`

The Rust target is currently structural and minimal. It does not yet add derives, macros, lifetimes, or richer ecosystem-specific integrations.

## Service Model

`service` should be read as an operation surface, not as a REST-only concept.

That means codegen should remain flexible enough to support surfaces such as:

- API
- CLI
- engine adapters
- blockchain or smart-contract entrypoints

The shared file-level shapes define the domain vocabulary. Target-specific annotations and generators decide how each service surface is projected into glue code.

## Design Constraint

The code generator should stay deterministic and boring.

That means:

- no handwritten edits in generated files
- no business logic hiding inside generators
- no unnecessary target-specific magic
- no extra lowering layer or IR

The intended structure is:

```text
DSL -> AST -> AST transforms -> generated glue -> handwritten entrypoints
```

The generated layer mounts into handwritten functions or modules that own the meaningful behavior.
