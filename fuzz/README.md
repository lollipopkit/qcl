# qcl fuzzing

This directory contains a `cargo-fuzz` / libFuzzer setup for the highest-risk QCL surfaces:

- `expr_parse`: raw query parsing through `Expr::try_from`, with extra pressure on deep parentheses.
- `expr_eval`: end-to-end `Expr::try_from` + `expr.eval()` against explicit `default` or `auto` context parsing modes, with optional format forcing for JSON/YAML/TOML.
- `de_formats`: default JSON-only parsing, explicit `parse_auto()`, and JSON/YAML/TOML deserialization invariants.
- `deep_access`: generated deep `@field` / list-index access chains with exact leaf assertions plus fail-closed missing-path checks.
- `in_operator`: `in` membership semantics across supported string/list cases, plus negative coverage ensuring map operands are rejected.

## Prerequisite

```bash
cargo install cargo-fuzz
```

## Run

```bash
cargo fuzz run expr_parse fuzz/corpus/expr_parse -- -dict=fuzz/qcl.dict
cargo fuzz run expr_eval fuzz/corpus/expr_eval -- -dict=fuzz/qcl.dict
cargo fuzz run de_formats fuzz/corpus/de_formats -- -dict=fuzz/qcl.dict
cargo fuzz run deep_access fuzz/corpus/deep_access
cargo fuzz run in_operator fuzz/corpus/in_operator -- -dict=fuzz/qcl.dict
```

## Corpus conventions

- `expr_parse`: corpus entries are raw QCL expressions.
- `expr_eval`: corpus entries may be either `EXPR\n--CTX--\n<context>` for the safe default parser, or `EXPR\n--MODE--\ndefault|auto|json|yaml|toml\n--CTX--\n<context>` to force a specific parser path. The `auto` path is expected to fail closed on ambiguous payloads.
- `de_formats`: corpus entries follow `json|yaml|toml|default|auto\n--DATA--\n<payload>`.
  Missing mode now defaults to `default`; `default` hits `parse_with_format(None)`, and only `auto` hits explicit `parse_auto()`. Ambiguous inputs are allowed to be rejected in `auto` mode.
- `deep_access` / `in_operator`: any raw bytes are valid; the targets generate structured cases from them and also try an `arbitrary` decode when possible.
