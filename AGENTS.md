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

## Architecture

- No IR. The pipeline is: `DSL → AST → transformers (enrich/modify the original AST) → target syntax`.
- Transformers operate on the same AST — they enrich or modify it, not lower it into a different representation.
- AST nodes are the opcodes. The original DSL may produce a subset of the full AST syntax. Transformers produce "hidden" AST nodes (e.g., @crud expanding into method declarations) that are processed identically to declared "public" syntax nodes by downstream generators.
- Do not introduce intermediate layers, bytecode, or lowered representations. This is a direct translator, not a compiler with optimization passes.
