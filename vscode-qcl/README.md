# QCL Language Support for VS Code

This extension provides language support for QCL (Query Check Language) in Visual Studio Code, including syntax highlighting and language server features.

## Language Server Implementation Guide

This extension implements the VS Code Language Server Protocol (LSP) based on the [official VS Code Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

### Language Server Architecture

The extension follows the standard VS Code Language Server pattern with two main components:

- **Language Client**: A normal VS Code extension written in TypeScript that has access to the VS Code API
- **Language Server**: The QCL language analysis tool (`qcl-lsp`) running in a separate process

### Key Benefits

1. **Native Language Integration**: The language server is implemented in Rust, not JavaScript/TypeScript
2. **Performance**: The server runs in a separate process, avoiding performance costs on VS Code's main thread
3. **Standardization**: Uses the Language Server Protocol (LSP) for standardized communication

### Implementation Structure

```
vscode-qcl/
├── src/
│   └── extension.ts          # Language Client implementation
├── syntaxes/
│   └── qcl.tmLanguage.json   # TextMate grammar for syntax highlighting
├── package.json              # Extension manifest
└── ...                       # Other configuration files
```

The QCL LSP server is built separately in the `lsp/` directory of the main QCL project.

## Features

- Syntax highlighting for QCL files
- Language Server Protocol (LSP) integration for:
  - Real-time error detection and diagnostics
  - Code completion
  - Hover information
  - Go to definition
  - Document symbols
  - Inlay hints (parameter + type hints)

## Status Bar and Inlay Hints

- The status bar shows QCL LSP state, including a spinner during analysis (Checking…). Click it for actions.
- Quick actions include restart/disable and toggles for inlay hints.
- Configure inlay hints via settings:
  - `qcl.lsp.inlayHints.enabled`
  - `qcl.lsp.inlayHints.parameters.enabled`
  - `qcl.lsp.inlayHints.types.enabled`
  - `qcl.lsp.inlayHints.throttleMs`

## Requirements

- The QCL LSP server (`qcl-lsp`) must be built and available in the system PATH or in the expected locations.

## Installation

1. Clone this repository
2. Install dependencies: `npm install`
3. Compile the extension: `npm run compile`
4. Build the QCL LSP server: `cargo build -p qcl-lsp`
5. Open the extension in VS Code and press F5 to run the extension

## Development

- `npm run compile`: Compile the TypeScript source
- `npm run watch`: Compile in watch mode

## QCL Language Features

The extension supports the QCL language with the following features:

### Syntax Highlighting
- Keywords: `if`, `else`, `while`, `let`, `return`, `fn`, `go`, `select`, `case`, `default`, `break`, `continue`, `import`, `from`, `as`, `in`
- Operators: `||`, `&&`, `==`, `!=`, `<=`, `>=`, `<`, `>`, `+`, `-`, `*`, `/`, `%`, `=`, `!`, `<-`
- Context access: `variable.path`
- Strings and numbers
- Comments

### Language Server Features
- Real-time error checking
- Code completion for keywords and functions
- Hover information for symbols
- Document symbols for navigation
- Context variable analysis

## License

MIT
