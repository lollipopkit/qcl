# QCL Language Server

A Language Server Protocol (LSP) implementation for the QCL (Query Check Language) domain-specific language.

## Features

- **Syntax Diagnostics**: Real-time error detection for QCL expressions and statement programs
- **Hover Information**: Shows type information, context references, and symbol counts
- **Code Completion**: Auto-complete for QCL keywords, operators, context variables, and standard library functions
- **Document Symbols**: Navigate through variables, functions, imports, and labels in QCL programs
- **Context Analysis**: Detects and analyzes context variable usage (@req, @record, etc.)

## Architecture

The LSP server consists of:

- `main.rs`: Core LSP server implementation using tower-lsp
- `analyzer.rs`: QCL language analysis engine that provides:
  - Expression and statement parsing
  - Symbol extraction (variables, functions, imports)
  - Context reference collection
  - Diagnostic generation

## Supported Language Features

### QCL Expressions
- Context access (`@req.user.role`)
- Arithmetic operations (`+`, `-`, `*`, `/`, `%`)
- Logical operations (`&&`, `||`, `!`)
- Comparison operations (`==`, `!=`, `<`, `>`, `<=`, `>=`, `in`)

### QCL Statements
- Variable declarations (`let x = value;`)
- Function definitions (`fn name(params) { body }`)
- Import statements (`import math;`, `import { abs } from math;`)
- Control flow (`if`, `while`, `break`, `continue`, `return`)
- Concurrency primitives (`go`, `select`, channel operations)

### Completions Provided

#### Keywords
- Control flow: `if`, `else`, `while`, `let`, `fn`, `return`, `break`, `continue`
- Imports: `import`, `from`, `as`
- Concurrency: `go`, `select`, `case`, `default`
- Literals: `true`, `false`, `nil`

#### Operators
- Comparison: `==`, `!=`, `<=`, `>=`
- Logical: `&&`, `||`
- Membership: `in`
- Channel: `<-`

#### Context Variables
- `@req.user.id`, `@req.user.role`, `@req.user.name`
- `@record.id`, `@record.owner`, `@record.granted`
- `@env`, `@time`

#### Standard Library Functions
- Math: `abs`, `sqrt`, `sin`, `cos`
- String: `len`, `substr`
- Concurrency: `make_chan`, `send`, `recv`

## Usage

### Building
```bash
cargo build -p qcl-lsp
```

### Running
```bash
cargo run -p qcl-lsp
```

The server communicates via stdin/stdout using the LSP JSON-RPC protocol.

### One‑shot File Analysis (CLI)

Analyze a single file from the command line and print JSON containing diagnostics, symbols, context references, and semantic tokens:

```bash
cargo run -p qcl-lsp -- --analyze path/to/file.qcl
```

Notes:
- The file path must be relative (no absolute paths or `..`).
- Output is prettified JSON suitable for piping to `jq`.

### Integration with Editors

#### VS Code
Create a VS Code extension that launches the LSP server:
```json
{
  "name": "qcl",
  "engines": { "vscode": "^1.50.0" },
  "contributes": {
    "languages": [{
      "id": "qcl",
      "extensions": [".qcl"]
    }]
  },
  "activationEvents": ["onLanguage:qcl"]
}
```

#### Neovim
Use nvim-lspconfig:
```lua
require'lspconfig'.configs.qcl = {
  default_config = {
    cmd = {'qcl-lsp'},
    filetypes = {'qcl'},
    root_dir = require('lspconfig.util').root_pattern('.git'),
  }
}
```

## Development

The LSP server leverages the QCL core library for parsing and analysis:
- Expression parsing via `qcl_core::expr::Expr`
- Statement parsing via `qcl_core::stmt_parser::StmtParser`
- Tokenization via `qcl_core::token::Tokenizer`

### Testing
Test the LSP server with a QCL file containing:
```qcl
// Expression example
@req.user.role == 'admin' && @req.user.level >= 5

// Statement program example
import math;
let result = math.sqrt(@req.user.score);
fn validate_user(user) {
    return user.role == 'admin' || user.level >= 10;
}
if (validate_user(@req.user)) {
    return true;
}
```
