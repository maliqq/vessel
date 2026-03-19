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
