# Vessel — Code Generation

## Pipeline

```
IDL source → Parser → AST → Semantic validation → Code generation
```

1. **Parser** — tokenize, parse syntax, attach annotations, preserve source locations
2. **AST** — mirrors syntax (struct, enum, union, service, method, field, annotation, type)
3. **Semantic validation** — resolve named types, validate references, validate Map keys, expand `@crud`, detect duplicates
4. **Code generation** — emit TypeScript types, handler interfaces, route scaffolding, client SDK

## TypeScript type mapping

| IDL | TypeScript |
|---|---|
| `string` | `string` |
| `int` | `number` |
| `int64` | `bigint` |
| `float` | `number` |
| `byte` | `number` |
| `bool` | `boolean` |
| `binary` | `Uint8Array` |
| `uuid` | `string & { readonly __brand: "Uuid" }` |
| `uuid_v7` | `string & { readonly __brand: "UuidV7" }` |
| `void` | `void` |
| `List<T>` | `T[]` |
| `Map<K, V>` | `Record<K, V>` |
| `Option<T>` | `T \| undefined` |

## Reference types

`*Company` generates a branded ID type:

```typescript
export type CompanyId = string & { readonly __brand: "CompanyId" }
```

All `*Company` fields and params use `CompanyId` in generated code.

## Struct generation

IDL:
```
struct Company {
  string name;
  string description?;
  *Company parent?;
}
```

TypeScript:
```typescript
export interface Company {
  name: string
  description?: string
  parent?: CompanyId
}
```

## Enum generation

IDL:
```
enum EmploymentType {
  FULL_TIME;
  CONTRACT;
}
```

TypeScript:
```typescript
export type EmploymentType =
  | "FULL_TIME"
  | "CONTRACT"
```

## Union generation

IDL:
```
union MediaContent = Photo | Video | Audio;
```

TypeScript:
```typescript
export type MediaContent = Photo | Video | Audio
```

## @crud expansion

`@crud` on a service generates five standard methods:

| Generated method | HTTP | Route |
|---|---|---|
| `create(data: T): Promise<TId>` | `POST /` | create |
| `get(id: TId): Promise<T>` | `GET /:id` | read |
| `update(id: TId, data: T): Promise<void>` | `PUT /:id` | update |
| `delete(id: TId): Promise<void>` | `DELETE /:id` | delete |
| `list(): Promise<TId[]>` | `GET /` | list |

Where `T` is the struct matching the service name, and `TId` is its reference type.

## Handler interface generation

IDL:
```
@crud
service Company {
  add(*Company company, Job job) *Job raises ValidationError;
}
```

TypeScript:
```typescript
export interface CompanyHandlers {
  // @crud
  create(data: Company): Promise<CompanyId>
  get(id: CompanyId): Promise<Company>
  update(id: CompanyId, data: Company): Promise<void>
  delete(id: CompanyId): Promise<void>
  list(): Promise<CompanyId[]>

  // custom
  add(company: CompanyId, job: Job): Promise<JobId>
}
```

## Raises → error types

IDL:
```
get(*Company id) Company raises NotFound, Unauthorized;
```

TypeScript:
```typescript
get(id: CompanyId): Promise<Company>  // throws NotFound | Unauthorized
```

Error types are generated as discriminated unions or classes depending on codegen target.

## @rest route scaffolding

`@rest(base="/api/v1/companies")` generates HTTP route bindings.

| Method | HTTP | Route |
|---|---|---|
| `create` | `POST` | `/api/v1/companies` |
| `get` | `GET` | `/api/v1/companies/:id` |
| `update` | `PUT` | `/api/v1/companies/:id` |
| `delete` | `DELETE` | `/api/v1/companies/:id` |
| `list` | `GET` | `/api/v1/companies` |
| `add` | `POST` | `/api/v1/companies/:id/add` |

Custom methods become `POST /<base>/:id/<method_name>`.

Routes call into the handler interface. Bun example:

```typescript
app.post("/api/v1/companies", async (req) => {
  const data = await req.json()
  const id = await handlers.create(data)
  return Response.json({ id })
})
```

## Client SDK generation

Generated client mirrors the handler interface:

```typescript
const client = createCompanyClient("http://localhost:3000")

await client.create({ name: "Acme", description: "..." })
await client.get(companyId)
await client.update(companyId, { name: "Acme Inc" })
await client.delete(companyId)
await client.list()
await client.add(companyId, job)
```

## @paginate

Methods annotated with `@paginate` generate cursor-based pagination:

```typescript
list(cursor?: string, limit?: number): Promise<{
  items: CompanyId[]
  next_cursor?: string
}>
```

## OCaml module layout

```
lib/
  ast.ml          AST types
  parser.ml       recursive-descent parser
  printer.ml      AST pretty-printer
  semantics.ml    type resolution, validation (planned)
  crud.ml         @crud expansion (planned)
  ts_types.ml     TypeScript type generation (planned)
  ts_handlers.ml  handler interface generation (planned)
  bun_routes.ml   Bun route scaffolding (planned)
  ts_client.ml    client SDK generation (planned)
  emitter.ml      file output (planned)
```

## CLI

```
idlc compile schema.idl --out ./generated
```

Future:
```
idlc diff old.idl new.idl
idlc check schema.idl
```
