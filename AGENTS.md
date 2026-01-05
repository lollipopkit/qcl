# AGENTS.md

Instructions for AI agents working in this repository.

## Project

QCL (Query Check Language) is a small DSL for evaluating access-control style expressions like `@req.user.role == 'admin' || @req.user.id in @record.granted` against JSON/YAML/TOML contexts.

## Common Commands

- Tests: `cargo test` (stdout: `cargo test -- --nocapture`)
- All features: `cargo test --features all`
- Format: `cargo fmt` (or `cargo fmt --check` in CI)
- Lint: `cargo clippy --all-targets --all-features -- -D warnings`
- Benchmarks: `cargo bench`

## Code Layout

- Tokenizer: `src/token.rs`
- Parser / AST: `src/ast.rs`
- Expression + evaluation: `src/expr.rs`
- Runtime values: `src/val.rs`
- Operators: `src/op.rs`
- Context deserialization: `src/de.rs`
- Unit tests: colocated as separate files (e.g. `src/expr_test.rs`)
- Integration tests: `tests/integration_test.rs`

## Conventions

- Prefer existing patterns and keep changes minimal; avoid introducing new dependencies unless required.
- Preserve feature flags (`json`, `yaml`, `toml`, `sem_arith`, `adv_arith`) and gate format-specific code accordingly.
- When changing parsing/eval behavior, update/add tests in the adjacent `*_test.rs` file and run `cargo test` (ideally with `--features all`).

## CLI Notes

The binary reads the context from stdin and the expression from argv, e.g.:

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
```
