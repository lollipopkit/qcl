# VS Code Language Server Extension Tutorial

This tutorial provides a comprehensive guide to implementing Language Server Protocol (LSP) extensions for Visual Studio Code, based on the official [VS Code Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

## Table of Contents

1. [Why Language Server?](#why-language-server)
2. [Implementation Overview](#implementation-overview)
3. [Project Structure](#project-structure)
4. [Language Client Implementation](#language-client-implementation)
5. [Language Server Implementation](#language-server-implementation)
6. [LSP Features and Capabilities](#lsp-features-and-capabilities)
7. [Debugging Language Servers](#debugging-language-servers)
8. [Advanced Topics](#advanced-topics)
9. [Best Practices](#best-practices)

## Why Language Server?

Language Servers solve three common problems in language extension development:

### 1. Native Language Integration
- Language servers can be implemented in any programming language, not just JavaScript/TypeScript
- This allows leveraging existing language tools and libraries
- Enables use of specialized languages best suited for language analysis

### 2. Performance
- Language servers run in a separate process
- Avoids performance costs on VS Code's main thread
- Prevents blocking the UI during intensive analysis operations

### 3. Standardization
- Language Server Protocol (LSP) standardizes communication between editor and language tool
- One language tool can integrate with multiple editors that support LSP
- Reduces implementation effort for cross-editor support

## Implementation Overview

A language server extension in VS Code consists of two main parts:

### Language Client
- A normal VS Code extension written in JavaScript/TypeScript
- Has full access to the VS Code API
- Manages the lifecycle of the language server
- Handles communication between VS Code and the language server

### Language Server
- A separate process containing the actual language analysis tool
- Implements the Language Server Protocol
- Handles language-specific operations like parsing, analysis, etc.
- Can be written in any programming language

```
┌─────────────────┐    LSP Communication    ┌─────────────────┐
│   VS Code       │ ◄────────────────────► │ Language Server │
│   Extension     │                         │   (qcl-lsp)     │
│   (Client)      │                         │   (Rust)        │
└─────────────────┘                         └─────────────────┘
```

## Project Structure

The standard structure for a language server extension:

```
language-extension/
├── client/                  # Language Client
│   ├── src/
│   │   └── extension.ts     # Client entry point
│   ├── package.json
│   └── tsconfig.json
└── server/                  # Language Server
    ├── src/
    │   └── server.ts        # Server entry point
    ├── package.json
    └── tsconfig.json
```

For QCL, we use a modified structure since the server is implemented in Rust:

```
vscode-qcl/
├── src/
│   └── extension.ts          # Language Client (TypeScript)
├── syntaxes/
│   └── qcl.tmLanguage.json   # TextMate grammar
├── package.json              # Extension manifest
└── ../lsp/                   # Language Server (Rust)
    ├── src/
    │   ├── main.rs           # LSP server implementation
    │   └── analyzer.rs       # QCL language analysis
    └── Cargo.toml
```

## Language Client Implementation

The language client is implemented using the `vscode-languageclient` library.

### Basic Client Setup

```typescript
import * as path from 'path';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // Server options
    let serverModule = context.asAbsolutePath(
        path.join('server', 'out', 'server.js')
    );
    
    let serverOptions: ServerOptions = {
        run: { 
            module: serverModule, 
            transport: TransportKind.ipc 
        },
        debug: { 
            module: serverModule, 
            transport: TransportKind.ipc,
            options: { execArgv: ['--nolazy', '--inspect=6009'] }
        }
    };

    // Client options
    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ 
            scheme: 'file', 
            language: 'qcl' 
        }],
        synchronize: {
            fileEvents: [
                vscode.workspace.createFileSystemWatcher('**/.clientrc')
            ]
        }
    };

    // Create and start the client
    client = new LanguageClient(
        'qclLanguageServer',
        'QCL Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client
    client.start();
}
```

### QCL Client Implementation (Actual)

```typescript
import * as vscode from 'vscode';
import * as path from 'path';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscodeContext) {
    const serverOptions: ServerOptions = {
        command: 'qcl-lsp',
        args: [],
        transport: TransportKind.stdio
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'qcl' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher(
                '**/.qcl'
            )
        }
    };

    client = new LanguageClient(
        'qcl',
        'QCL Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
```

## Language Server Implementation

The language server implements the Language Server Protocol using appropriate libraries for the chosen language.

### Server Setup (TypeScript Example)

```typescript
import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    TextDocumentSyncKind,
    InitializeResult
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';

// Create connection and document manager
const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

connection.onInitialize((params: InitializeParams) => {
    const capabilities = params.capabilities;
    
    hasConfigurationCapability = !!(
        capabilities.workspace && 
        capabilities.workspace.configuration
    );
    
    hasWorkspaceFolderCapability = !!(
        capabilities.workspace && 
        capabilities.workspace.workspaceFolders
    );

    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            completionProvider: {
                resolveProvider: true
            },
            hoverProvider: true,
            documentSymbolProvider: true,
            definitionProvider: true
        }
    };
    
    return result;
});

// Start listening
documents.listen(connection);
connection.listen();
```

### QCL Server Implementation (Rust)

The QCL language server uses the `tower-lsp` crate:

```rust
use tower_lsp::{LspService, Server, Client};
use tower_lsp::jsonrpc::Result;
use serde_json::Value;
use async_trait::async_trait;
use std::sync::Arc;

struct Backend {
    client: Client,
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Incremental
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec!["@", "."].to_vec()),
                    ..Default::default()
                }),
                hover_provider: Some(true),
                document_symbol_provider: Some(true),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // Implementation for code completion
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        // Implementation for hover information
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        // Implementation for document symbols
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    
    let service = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, service).serve().await;
}
```

## LSP Features and Capabilities

### 1. Text Document Synchronization

```typescript
// Handle document changes
documents.onDidChangeContent(change => {
    validateDocument(change.document);
});

async function validateDocument(textDocument: TextDocument): Promise<void> {
    const text = textDocument.getText();
    const diagnostics = validateText(text);
    
    connection.sendDiagnostics({
        uri: textDocument.uri,
        diagnostics
    });
}
```

### 2. Code Completion

```typescript
// Register completion provider
connection.onCompletion(
    (_textDocumentPosition: TextDocumentPositionParams): Promise<CompletionItem[]> => {
        return Promise.resolve([
            {
                label: 'if',
                kind: CompletionItemKind.Keyword,
                data: 1
            },
            {
                label: 'while',
                kind: CompletionItemKind.Keyword,
                data: 2
            }
        ]);
    }
);
```

### 3. Hover Information

```typescript
connection.onHover(
    (params: TextDocumentPositionParams): Promise<Hover | null> => {
        const document = documents.get(params.textDocument.uri);
        if (!document) {
            return Promise.resolve(null);
        }
        
        const text = document.getText();
        const offset = document.offsetAt(params.position);
        const word = getWordAtPosition(text, offset);
        
        if (word) {
            return Promise.resolve({
                contents: {
                    kind: 'markdown',
                    value: `**${word}**\n\nType: ${getWordType(word)}`
                }
            });
        }
        
        return Promise.resolve(null);
    }
);
```

### 4. Document Symbols

```typescript
connection.onDocumentSymbol(
    (params: DocumentSymbolParams): Promise<DocumentSymbol[]> => {
        const document = documents.get(params.textDocument.uri);
        if (!document) {
            return Promise.resolve([]);
        }
        
        const text = document.getText();
        const symbols = extractSymbols(text);
        
        return Promise.resolve(symbols);
    }
);
```

### 5. Go to Definition

```typescript
connection.onDefinition(
    (params: TextDocumentPositionParams): Promise<Definition | null> => {
        const document = documents.get(params.textDocument.uri);
        if (!document) {
            return Promise.resolve(null);
        }
        
        const text = document.getText();
        const position = params.position;
        const definition = findDefinition(text, position);
        
        return Promise.resolve(definition);
    }
);
```

## Debugging Language Servers

### Client-Side Debugging

1. **Launch Configuration**:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Run Extension",
            "type": "extensionHost",
            "request": "launch",
            "runtimeExecutable": "${execPath}",
            "args": [
                "--extensionDevelopmentPath=${workspaceFolder}"
            ],
            "outFiles": [
                "${workspaceFolder}/client/out/**/*.js"
            ],
            "preLaunchTask": "${defaultBuildTask}"
        }
    ]
}
```

### Server-Side Debugging

For TypeScript servers:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Attach to Server",
            "type": "node",
            "request": "attach",
            "port": 6009,
            "restart": true,
            "outFiles": [
                "${workspaceFolder}/server/out/**/*.js"
            ]
        }
    ]
}
```

### Logging and Diagnostics

```typescript
// Enable logging
connection.console.log(`Starting language server...`);

// Send notifications to client
connection.sendNotification('custom/notification', {
    message: 'Server initialized successfully'
});

// Error handling
connection.onError(error => {
    console.error('Language server error:', error);
});
```

## Advanced Topics

### 1. Configuration Management

```typescript
// Handle configuration changes
connection.onDidChangeConfiguration(change => {
    if (hasConfigurationCapability) {
        // Revalidate all open text documents
        documents.all().forEach(validateDocument);
    } else {
        // Global settings changed
        documents.all().forEach(validateDocument);
    }
});
```

### 2. Workspace Folders

```typescript
// Handle workspace folders
connection.onInitialized(() => {
    if (hasWorkspaceFolderCapability) {
        connection.workspace.getWorkspaceFolders().then(folders => {
            if (folders) {
                folders.forEach(folder => {
                    setupWorkspaceFolder(folder);
                });
            }
        });
    }
});
```

### 3. Custom Notifications and Requests

```typescript
// Register custom notification
connection.onNotification('custom/analyze', params => {
    const result = performCustomAnalysis(params);
    connection.sendNotification('custom/analysisResult', result);
});

// Register custom request
connection.onRequest('custom/execute', params => {
    return executeCustomCommand(params);
});
```

### 4. Dynamic Feature Registration

```typescript
// Dynamically register capabilities
connection.onInitialized(() => {
    connection.client.register(
        DidChangeConfigurationNotification.type,
        undefined
    );
});
```

## Best Practices

### 1. Performance Optimization

- **Incremental Processing**: Only process changed parts of documents
- **Caching**: Cache analysis results when possible
- **Debouncing**: Debounce rapid document changes
- **Lazy Loading**: Load resources only when needed

```typescript
// Debounce document changes
const debouncedValidate = debounce(validateDocument, 500);
documents.onDidChangeContent(change => {
    debouncedValidate(change.document);
});
```

### 2. Error Handling

```typescript
// Robust error handling
try {
    const result = performOperation(params);
    return Promise.resolve(result);
} catch (error) {
    connection.console.error(`Operation failed: ${error}`);
    return Promise.reject(error);
}
```

### 3. Resource Management

```typescript
// Clean up resources
connection.onShutdown(() => {
    // Close file handles, database connections, etc.
    cleanupResources();
});
```

### 4. Testing

```typescript
// Unit testing language features
describe('Language Server', () => {
    it('should provide completions', () => {
        const result = getCompletions('if ');
        expect(result).toContainEqual(
            expect.objectContaining({ label: 'else' })
        );
    });
});
```

### 5. Version Compatibility

```typescript
// Check client capabilities
const clientCapabilities = initializeParams.capabilities;
if (clientCapabilities.textDocument?.completion?.completionItem?.snippetSupport) {
    // Enable snippet support
}
```

## Conclusion

The Language Server Protocol provides a powerful, standardized way to implement language features in VS Code. By separating the language analysis from the editor UI, you can:

1. Use any programming language for language analysis
2. Achieve better performance through process isolation
3. Support multiple editors with a single implementation
4. Leverage existing language tools and libraries

The QCL language server implementation demonstrates how to apply these patterns to create a robust, feature-rich language extension that provides syntax highlighting, error checking, code completion, and other advanced features for the QCL domain-specific language.