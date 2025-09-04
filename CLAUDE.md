# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

QCL (Query Check Language) is a domain-specific language for access control evaluation, written in Rust. It allows parsing and evaluating expressions like `@req.user.role == 'admin' || @req.user.id in @record.granted` against JSON/YAML/TOML contexts.

## Common Development Commands

### Testing

- `cargo test` - Run all tests (unit + integration)
- `cargo test <test_name>` - Run specific test
- `cargo test -- --nocapture` - Run tests with stdout output (configured in Cargo.toml)

### Building

- `cargo build` - Debug build
- `cargo build --release` - Release build
- `cargo run -- <expression>` - Run CLI with expression (reads context from stdin)

### Benchmarking

- `cargo bench` - Run benchmarks (located in benches/)

### Feature Testing

- `cargo test --features all` - Test with all features enabled
- `cargo build --no-default-features --features yaml` - Build with only YAML support

## Architecture

### Core Components

1. **Tokenizer** (`src/token.rs`) - Lexical analysis, converts input string to tokens
2. **Parser** (`src/ast.rs`) - Recursive descent parser implementing the grammar
3. **AST/Expression** (`src/expr.rs`) - Expression tree with evaluation logic and caching
4. **Values** (`src/val.rs`) - Runtime value types (String, Int, Float, Bool, Nil, List, Map)
5. **Operations** (`src/op.rs`) - Binary/unary operators with type coercion
6. **Deserialization** (`src/de.rs`) - Context parsing from JSON/YAML/TOML

### Processing Flow

```
Input String -> Tokenizer -> Parser -> AST -> Evaluator -> Result
Context (JSON/YAML/TOML) -> Deserializer -> Val -> Evaluator
```

### Grammar (EBNF)

The language follows this precedence hierarchy:

- `or` (||) - lowest precedence
- `and` (&&)
- `cmp` (==, !=, <, >, <=, >=, in)
- `addsub` (+, -)
- `muldiv` (*, /, %)
- `unary` (!)
- `postfix` (field access with .)
- `primary` (literals, @context) - highest precedence

### Key Features

#### Expression Caching

The `Expr::parse_cached()` method uses `once_cell::sync::Lazy` for caching parsed expressions globally.

#### Context Access

- `@` prefix accesses context objects (e.g., `@req.user.name`)
- Context must be provided as `Val` (typically parsed from JSON/YAML/TOML)
- Use `expr.requested_ctx()` to discover required context keys

#### Feature Flags

- `json` (default) - JSON context support
- `yaml` - YAML context support  
- `toml` - TOML context support
- `sem_arith` (default) - Semantic arithmetic (3/2 = 1.5)
- `adv_arith` - Advanced arithmetic (Map + Map operations)

## Test Structure

- Unit tests are colocated (e.g., `src/expr_test.rs` for `src/expr.rs`)
- Integration tests in `tests/integration_test.rs`
- Tests follow `#[cfg(test)]` module pattern
- Benchmark tests in `benches/bench_main.rs`

## CLI Usage Pattern

The binary reads context from stdin and evaluates expressions:

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
```

Format can be explicitly specified with `--json`, `--yaml`, or `--toml` flags.
