# Agents

## Code style

- Prioritize readability over density. Do not write "clever" compact OCaml — pay the extra lines.
- Extract inline lambdas and complex expressions into named local functions.
- Group related logic into clearly separated sections with blank lines and section comments.
- Prefer splitting large functions (e.g., `generate`) into smaller named helpers (`emit_structs`, `emit_enums`, etc.).
- When pattern matching produces more than a few lines per arm, extract each arm into its own function.
- Use `|>` pipe chaining for data flow. The pipeline should read top-to-bottom: `source |> parse |> expand |> generate |> emit`.
- Avoid nested calls like `f1(f2(f3(x)))`. Prefer `x |> f3 |> f2 |> f1`.
- High-level orchestration code (pipeline, CLI) should be a chain of transforms, not imperative let-bindings.

## Why OCaml

- OCaml is chosen for its ADTs (AST variants are the grammar), exhaustive pattern matching (add an AST node → compiler shows every place that needs updating), and natural recursive descent (PEG rules map 1:1 to `parse_*` functions).
- The pipeline leverages this: PEG grammar → recursive descent parser → AST (ADTs) → pattern-match transforms → pattern-match codegen per target.
- Dense OCaml becomes Perl. Do not exploit OCaml's expressiveness to compress code — use it to make the structure obvious. A 10-line pattern match with named helpers is better than a 3-line nested combinator chain.

## Architecture

- No IR. The pipeline is: `DSL → AST → transformers (enrich/modify the original AST) → target syntax`.
- Transformers operate on the same AST — they enrich or modify it, not lower it into a different representation.
- AST nodes are the opcodes. The original DSL may produce a subset of the full AST syntax. Transformers produce "hidden" AST nodes (e.g., @crud expanding into method declarations) that are processed identically to declared "public" syntax nodes by downstream generators.
- Do not introduce intermediate layers, bytecode, or lowered representations. This is a direct translator, not a compiler with optimization passes.

## Codegen philosophy

- Generated code is **glue** — deterministic wiring that is never edited by hand. It owns routing, validation, serialization, error mapping, and dispatch.
- Generated code calls **mount points** — user-written functions or handler objects with fully typed signatures derived from the schema. The handler signature is the contract.
- The boundary is strict: generated code is the frame, user code is the picture. A service declaration produces both sides: the glue (auto-generated) and the handler interface (the entrypoint shape the user implements).
- Service kind annotations (`@rest`, `@solana`, `@cli`, etc.) determine what kind of glue is generated. The types are shared across all services — one definition, multiple codegen targets.
- Each service kind produces its own transport-specific glue, but the pattern is always the same: deserialize input → validate → call user handler with typed args → serialize output.
