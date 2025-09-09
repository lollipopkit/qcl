import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log('QCL extension is now active!');

  // Register commands
  const startCommand = vscode.commands.registerCommand('qcl.startServer', () => {
    vscode.window.showInformationMessage('QCL Language Server started manually');
  });

  const restartCommand = vscode.commands.registerCommand('qcl.restartServer', async () => {
    if (client) {
      console.log('Restarting QCL Language Server...');
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage('QCL Language Server restarted');
    }
  });

  context.subscriptions.push(startCommand, restartCommand);

  // Get the path to the QCL LSP server
  const serverPath = getServerPath();

  console.log('Looking for QCL LSP server...');
  console.log('Server path found:', serverPath);

  // If the server path is not found, show an error and return
  if (!serverPath) {
    vscode.window.showErrorMessage(
      'QCL LSP server not found. Please build the QCL project first.'
    );
    return;
  }

  const serverOptions: ServerOptions = {
    command: serverPath,
    transport: TransportKind.stdio
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'qcl' }],
    synchronize: {
      configurationSection: 'qcl',
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.qcl')
    },
    // Initialize options for semantic highlighting
    initializationOptions: {
      // Enable semantic highlighting
      semanticHighlighting: true,
      // Custom configuration for QCL
      qcl: {
        enableSemanticTokens: true
      }
    }
  };

  client = new LanguageClient(
    'qcl',
    'QCL Language Server',
    serverOptions,
    clientOptions
  );

  console.log('Starting QCL Language Server...');
  console.log('Server path:', serverPath);
  console.log('Client options:', clientOptions);
  
  // Add error handling for the client itself
  client.onDidChangeState((event) => {
    console.log(`LSP client state change: ${event.oldState} -> ${event.newState}`);
  });
  
  // Add semantic highlighting event listeners
  client.onNotification('textDocument/semanticTokens', (params) => {
    console.log('Semantic tokens received:', params);
  });

  client.onRequest('textDocument/semanticTokens', (params) => {
    console.log('Semantic tokens requested:', params);
  });

  // Start with a timeout and proper error handling
  const startPromise = client.start();
  
  // Add a timeout to detect hanging
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error('LSP server start timeout after 10 seconds')), 10000);
  });
  
  Promise.race([startPromise, timeoutPromise])
    .then(() => {
      console.log('QCL Language Server started successfully');
      vscode.window.showInformationMessage('QCL Language Server started successfully');
      
      // Check if semantic highlighting is enabled
      const config = vscode.workspace.getConfiguration('editor');
      const semanticHighlighting = config.get('semanticHighlighting.enabled');
      console.log('Semantic highlighting enabled:', semanticHighlighting);
    })
    .catch((error) => {
      console.error('Failed to start QCL Language Server:', error);
      console.error('Error details:', JSON.stringify(error, null, 2));
      vscode.window.showErrorMessage('Failed to start QCL Language Server: ' + error.message);
      
      // Try to stop the client if it's in a bad state
      if (client) {
        client.stop().catch(stopError => {
          console.error('Error stopping client after failure:', stopError);
        });
      }
    });
}

function getServerPath(): string | undefined {
  // Try to find the qcl-lsp executable in different locations
  const possiblePaths = [
    // Check common build output directories
    path.join(__dirname, '..', '..', 'target', 'debug', 'qcl-lsp'),
    path.join(__dirname, '..', 'target', 'debug', 'qcl-lsp'),
    // Check if it's in the PATH
    'qcl-lsp',
    '~/.cargo/bin/qcl-lsp',
  ];

  console.log('Extension __dirname:', __dirname);
  console.log('Searching for qcl-lsp binary in paths:');
  
  for (const possiblePath of possiblePaths) {
    console.log('Checking:', possiblePath);
    try {
      if (fs.existsSync(possiblePath)) {
        console.log('Found qcl-lsp at:', possiblePath);
        // Test if the file is executable
        fs.accessSync(possiblePath, fs.constants.F_OK | fs.constants.X_OK);
        console.log('qcl-lsp is executable');
        return possiblePath;
      }
    } catch (error) {
      console.log('Error accessing', possiblePath, ':', error);
    }
  }

  console.log('qcl-lsp binary not found in any location');
  return undefined;
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}