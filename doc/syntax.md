# Vessel — Syntax Reference

## File structure

A `.idl` file has two sections: directives, then declarations.

```
#version("1.0")
#target("typescript")

// declarations follow
struct Foo { ... }
```

Directives must come before any declaration. Order of declarations is free.

## Comments

```
// line comment
```

No block comments. Parser ignores comment lines entirely.

## Directives

File-level metadata. Prefixed with `#`. Must appear before all declarations.

```
#version("1.0")
#target("typescript")
#import("common.idl")
#config(debug=true, timeout=30)
```

Three argument forms:

| Form | Example |
|---|---|
| Bare | `#experimental` |
| Single value | `#version("1.0")` |
| Named pairs | `#config(debug=true, timeout=30)` |

## Annotations

Declaration-level metadata. Prefixed with `@`. Attached to the declaration, field, or method that follows.

```
@crud
@rest(base="/api/v1")
@deprecated("use V2")
@validate(min=1, max=100, required=true)
```

Same three argument forms as directives. Can be stacked:

```
@crud
@rest(base="/api/v1/companies")
service Company { ... }
```

Allowed on: struct, enum, union, service, field, enum member, method.

## Literal values

Used in directive and annotation arguments.

| Type | Examples |
|---|---|
| String | `"hello"`, `"/api/v1"` |
| Int | `42`, `-1`, `0` |
| Float | `3.14`, `-0.5` |
| Bool | `true`, `false` |

## Struct

A named shape with typed fields.

```
struct Profile {
  string name;
  string headline;
  int age;
}
```

Fields follow the pattern: `Type name;`

## Optional fields

Postfix `?` marks a field as optional (sugar for `Option<T>`):

```
struct Profile {
  string name;
  string phone?;          // optional
  Option<string> bio;     // equivalent explicit form
}
```

Both forms are valid. `?` is preferred for brevity.

## Enum

A closed symbolic set. Members are `SCREAMING_CASE`.

```
enum EmploymentType {
  FULL_TIME;
  CONTRACT;
  TEMPORARY;
}
```

## Union

A sum type. Variants are `|`-separated types.

```
union MediaContent = Photo | Video | Audio;
union SearchResult = *Company | *Job;
```

## Service

A named group of methods. The operation surface of a domain.

```
@crud
@rest(base="/api/v1/companies")
service Company {
  add(*Company company, Job job) *Job;
  remove(*Company company, *Job job) void;
  listJobs(*Company company) List<*Job>;
}
```

Methods follow: `name(params) ReturnType;`

Parameters: `Type name` separated by commas.

## Raises

Methods can declare error types they may raise. Optional.

```
get(*Company id) Company raises NotFound;
update(*Company id, Company data) void raises NotFound, Unauthorized, ValidationError;
list() List<Company>;  // no raises
```

## Primitive types

All lowercase.

| Type | Description | TypeScript | Rust |
|---|---|---|---|
| `string` | UTF-8 text | `string` | `String` |
| `int` | 32-bit signed integer | `number` | `i32` |
| `int64` | 64-bit signed integer | `bigint` | `i64` |
| `float` | 64-bit float | `number` | `f64` |
| `byte` | unsigned byte | `number` | `u8` |
| `bool` | boolean | `boolean` | `bool` |
| `binary` | raw bytes | `Uint8Array` | `Vec<u8>` |
| `uuid` | UUID | `string` (branded) | `Uuid` |
| `uuid_v7` | UUIDv7 (sortable) | `string` (branded) | `Uuid` |
| `void` | no value | `void` | `()` |

## Container types

PascalCase. Parameterized with angle brackets.

| Type | Constraint | Example |
|---|---|---|
| `List<T>` | T is any type | `List<string>`, `List<*Job>` |
| `Map<K, V>` | K is primitive or `*Ref` | `Map<string, int>`, `Map<*Profile, Dialog>` |
| `Option<T>` | T is any type | `Option<string>`, `Option<*Company>` |

Map keys cannot be bare complex types. Use a reference instead:

```
Map<string, Profile>    // ok — primitive key
Map<*Profile, Dialog>   // ok — reference key (keyed by ID)
Map<Profile, string>    // error — bare complex key
```

## Reference types

`*Type` denotes a typed reference (handle/ID) to an existing object, not an embedded value.

```
struct Job {
  string title;
  *Company company;  // reference to Company, not an embedded Company
}
```

References are typically represented as UUIDs or opaque IDs. They generate branded ID types in codegen.

## Naming conventions

| Element | Convention | Example |
|---|---|---|
| Primitives | lowercase | `string`, `int`, `uuid` |
| Struct / Enum / Union | PascalCase | `Profile`, `EmploymentType` |
| Enum members | SCREAMING_CASE | `FULL_TIME`, `CONTRACT` |
| Fields / params / methods | snake_case or camelCase | `first_name`, `listJobs` |

## Grammar

The formal PEG grammar is in `idl.peg` at the project root.
