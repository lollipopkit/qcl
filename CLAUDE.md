# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

QCL (Query Check Language) is a domain-specific language for access control evaluation, written in Rust. It allows parsing and evaluating expressions like `@req.user.role == 'admin' || @req.user.id in @record.granted` against JSON/YAML/TOML contexts. The project includes a core library, CLI tool, standard library modules, and a Language Server Protocol (LSP) implementation.

## Common Development Commands

### Testing

- `cargo test` - Run all tests (unit + integration)
- `cargo test <test_name>` - Run specific test
- `cargo test -- --nocapture` - Run tests with stdout output (configured in Cargo.toml)

### Building

- `cargo build` - Debug build
- `cargo build --release` - Release build
- `cargo run -- <expression>` - Run CLI with expression (reads context from stdin)
- `cargo run -- --stmt <program>` - Run CLI with statement program
- `cargo build -p qcl-lsp` - Build LSP server
- `cargo run -p qcl-lsp` - Run LSP server

### Benchmarking

- `cargo bench` - Run benchmarks (located in benches/)

### Feature Testing

- `cargo test --features all` - Test with all features enabled
- `cargo build --no-default-features --features yaml` - Build with only YAML support
- `cargo build --no-default-features --features stdlib-math,json` - Build with specific stdlib modules

## Architecture

### Workspace Structure

The project is organized as a Cargo workspace with the following members:

- **core/** (`qcl-core`) - Core language implementation (parser, evaluator, AST)
- **cli/** (`qcl-cli`) - Command-line interface for QCL execution
- **stdlib/** (`qcl-stdlib`) - Standard library modules (math, string, datetime, os, tcp)
- **lsp/** (`qcl-lsp`) - Language Server Protocol implementation

### Core Components

1. **Tokenizer** (`core/src/token.rs`) - Lexical analysis, converts input string to tokens
2. **Parser** (`core/src/ast.rs`) - Recursive descent parser implementing the grammar
3. **AST/Expression** (`core/src/expr.rs`) - Expression tree with evaluation logic and caching
4. **Values** (`core/src/val.rs`) - Runtime value types (String, Int, Float, Bool, Nil, List, Map)
5. **Operations** (`core/src/op.rs`) - Binary/unary operators with type coercion
6. **Deserialization** (`core/src/de.rs`) - Context parsing from JSON/YAML/TOML
7. **Statements** (`core/src/stmt.rs`) - Statement AST nodes for control flow and functions
8. **Statement Parser** (`core/src/stmt_parser.rs`) - Parser for statement programs
9. **Import System** (`core/src/import.rs`) - Module import and resolution system
10. **Module Registry** (`core/src/module.rs`) - Standard library module management

### Processing Flow

```
Expression Mode:
Input String -> Tokenizer -> Parser -> AST -> Evaluator -> Result
Context (JSON/YAML/TOML) -> Deserializer -> Val -> Evaluator

Statement Mode:
Input String -> Tokenizer -> Statement Parser -> Program AST -> Executor -> Result
Imports -> Module Registry -> Module Resolution -> Available Functions
```

### Grammar (EBNF)

The language supports both expressions and full statement programs:

#### Expression Grammar (precedence hierarchy):
- `or` (||) - lowest precedence
- `and` (&&)
- `cmp` (==, !=, <, >, <=, >=, in)
- `addsub` (+, -)
- `muldiv` (*, /, %)
- `unary` (!)
- `postfix` (field access with .)
- `primary` (literals, @context) - highest precedence

#### Statement Grammar:
```
program  ::= statement*
statement ::= import_stmt | if_stmt | while_stmt | let_stmt | assign_stmt | goto_stmt | label_stmt | break_stmt | continue_stmt | return_stmt | fn_stmt | expr_stmt | block_stmt
import_stmt ::= 'import' import_spec ';'
if_stmt  ::= 'if' '(' expr ')' statement ['else' statement]
while_stmt ::= 'while' '(' expr ')' statement
let_stmt ::= 'let' id [':' type] '=' expr ';'
assign_stmt ::= id '=' expr ';'
goto_stmt ::= 'goto' id ';'
label_stmt ::= id ':'
break_stmt ::= 'break' ';'
continue_stmt ::= 'continue' ';'
return_stmt ::= 'return' [expr] ';'
fn_stmt ::= 'fn' id '(' [id {',' id}] ')' block_stmt
expr_stmt ::= expr ';'
block_stmt ::= '{' statement* '}'
```

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

#### Standard Library Modules

The language includes a modular standard library system:

- `stdlib-math` - Mathematical functions (abs, sqrt, sin, cos, etc.)
- `stdlib-string` - String manipulation functions
- `stdlib-datetime` - Date and time operations
- `stdlib-os` - Operating system interface
- `stdlib-tcp` - TCP networking functions
- `stdlib-all` = All standard library modules combined

#### Import System

Supports multiple import syntaxes for modules:
- `import math;` - Import entire stdlib module
- `import "path/to/file.qcl";` - Import from file path
- `import { abs, sqrt } from math;` - Import specific functions
- `import * as math from math;` - Import as namespace
- `import math as m;` - Import module with alias

## Language Server Protocol (LSP)

The project includes a complete LSP implementation (`lsp/`) that provides:

- **Syntax Diagnostics**: Real-time error detection for QCL expressions and statement programs
- **Hover Information**: Shows type information, context references, and symbol counts
- **Code Completion**: Auto-complete for QCL keywords, operators, context variables, and standard library functions
- **Document Symbols**: Navigate through variables, functions, imports, and labels in QCL programs
- **Context Analysis**: Detects and analyzes context variable usage (@req, @record, etc.)

### LSP Architecture

- `main.rs`: Core LSP server implementation using tower-lsp
- `analyzer.rs`: QCL language analysis engine that provides expression/statement parsing, symbol extraction, context reference collection, and diagnostic generation

### LSP Usage

```bash
# Build LSP server
cargo build -p qcl-lsp

# Run LSP server (communicates via stdin/stdout)
cargo run -p qcl-lsp
```

## Test Structure

- Unit tests are colocated (e.g., `src/expr_test.rs` for `src/expr.rs`)
- Integration tests in `tests/integration_test.rs`
- Tests follow `#[cfg(test)]` module pattern
- Benchmark tests in `benches/bench_main.rs`

## CLI Usage Pattern

The binary reads context from stdin and supports both expression and statement modes:

### Expression Mode (default)
```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
```

### Statement Mode (--stmt flag)
```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- --stmt 'import math; let result = math.sqrt(@req.user.level); return result;'
```

### File Execution
```bash
# Execute QCL file
cargo run -- program.qcl

# With explicit context format
cargo run -- --json program.qcl
```

Format can be explicitly specified with `--json`, `--yaml`, or `--toml` flags. The CLI auto-detects format based on available features and includes security measures for file path validation.

## Security Features

The CLI includes robust security measures for file operations:

- **Path Validation**: Prevents directory traversal attacks (`../`)
- **Absolute Path Blocking**: Disallows absolute file paths
- **Filename Sanitization**: Uses `sanitize-filename` crate for safe filename handling
- **Character Filtering**: Blocks suspicious characters (null bytes, newlines, tabs)
- **Safe File Reading**: All file operations go through security validation

## Development Workflow

### Adding New Features

1. **Core Language Changes**: Modify components in `core/src/`
2. **Standard Library Functions**: Add modules in `stdlib/src/`
3. **CLI Enhancements**: Update `cli/src/main.rs`
4. **LSP Features**: Extend `lsp/src/analyzer.rs` and `lsp/src/main.rs`

### Testing Guidelines

- Write unit tests alongside implementation files (`*_test.rs`)
- Test both expression and statement parsing modes
- Validate security measures for file operations
- Use `cargo test --features all` for comprehensive testing

### Code Quality

- Follow Rust idioms and best practices
- Use `#[allow(dead_code)]` sparingly and with justification
- Maintain proper error handling with `anyhow::Result`
- Include comprehensive documentation for public APIs
- Follow the existing code style and patterns
