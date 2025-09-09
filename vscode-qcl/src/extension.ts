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
  
  client.start().then(() => {
    console.log('QCL Language Server started successfully');
    vscode.window.showInformationMessage('QCL Language Server started successfully');
  }).catch((error) => {
    console.error('Failed to start QCL Language Server:', error);
    console.error('Error details:', JSON.stringify(error, null, 2));
    vscode.window.showErrorMessage('Failed to start QCL Language Server: ' + error.message);
  });
}

function getServerPath(): string | undefined {
  // Try to find the qcl-lsp executable in different locations
  const possiblePaths = [
    // Check if it's in the PATH
    'qcl-lsp',
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