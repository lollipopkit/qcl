import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';
import type { Middleware } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log('QCL extension is now active');

  // Register commands
  const startCommand = vscode.commands.registerCommand('qcl.startServer', async () => {
    if (!client) {
      vscode.window.showErrorMessage('QCL Language Server client not initialized');
      return;
    }
    try {
      await client.start();
      vscode.window.showInformationMessage('QCL Language Server started');
    } catch (e: any) {
      vscode.window.showErrorMessage('Failed to start QCL Language Server: ' + (e?.message || e));
    }
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

  // Check if LSP is enabled
  const config = vscode.workspace.getConfiguration('qcl.lsp');
  const lspEnabled = config.get<boolean>('enabled', true);
  const autoStart = config.get<boolean>('autoStart', true);
  
  if (!lspEnabled) {
    console.log('QCL LSP is disabled in configuration');
    return;
  }

  // Get the path to the QCL LSP server
  const customServerPath = config.get<string>('serverPath', '');
  const serverPath = customServerPath ? expandHome(customServerPath) : getServerPath();

  console.log('Looking for QCL LSP server...');
  console.log('Server path resolved to:', serverPath ?? 'PATH: qcl-lsp');

  // If the server path is not found, show an error and return
  if (!serverPath) {
    vscode.window.showErrorMessage(
      'QCL LSP server not found. Please build the QCL project first or configure a custom server path.'
    );
    return;
  }

  const serverOptions: ServerOptions = {
    command: serverPath,
    transport: TransportKind.stdio
  };

  const traceLevel = config.get<string>('trace', 'off');
  const isVerbose = traceLevel === 'verbose';
  const outputChannelEnabled = config.get<boolean>('outputChannel.enabled', false);
  const semanticTokensEnabled = config.get<boolean>('semanticTokens.enabled', true);
  const throttleMs = Math.max(0, Number(config.get<number>('semanticTokens.throttleMs', 40)) || 0);

  // Semantic tokens throttle/disable via client middleware
  const lastTokenReqAt = new Map<string, number>();
  const settings = { semanticTokensEnabled, throttleMs };
  const middleware: Middleware = {
    provideDocumentSemanticTokens(document, token, next) {
      if (!settings.semanticTokensEnabled) {
        if (isVerbose) console.log('Semantic tokens disabled (full)');
        return null;
      }
      if (settings.throttleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastTokenReqAt.get(key) || 0;
        if (now - last < settings.throttleMs) {
          if (isVerbose) console.log('Semantic tokens full throttled');
          return null;
        }
        lastTokenReqAt.set(key, now);
      }
      return next(document, token);
    },
    provideDocumentRangeSemanticTokens(document, range, token, next) {
      if (!settings.semanticTokensEnabled) {
        if (isVerbose) console.log('Semantic tokens disabled (range)');
        return null;
      }
      if (settings.throttleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastTokenReqAt.get(key) || 0;
        if (now - last < settings.throttleMs) {
          if (isVerbose) console.log('Semantic tokens range throttled');
          return null;
        }
        lastTokenReqAt.set(key, now);
      }
      return next(document, range, token);
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'qcl' }],
    synchronize: {
      configurationSection: 'qcl'
    },
    // Initialize options for semantic highlighting
    initializationOptions: {
      // Enable semantic highlighting
      semanticHighlighting: true,
      // Custom configuration for QCL
      qcl: {
        enableSemanticTokens: true
      }
    },
    outputChannelName: (outputChannelEnabled || isVerbose) ? 'QCL Language Server' : undefined,
    traceOutputChannel: traceLevel !== 'off' ? vscode.window.createOutputChannel('QCL Language Server Trace') : undefined,
    middleware
  };

  client = new LanguageClient(
    'qcl',
    'QCL Language Server',
    serverOptions,
    clientOptions
  );

  if (isVerbose) {
    console.log('Starting QCL Language Server...', serverPath);
  }
  
  // Add error handling for the client itself
  client.onDidChangeState((event) => {
    if (isVerbose) {
      console.log(`LSP client state change: ${event.oldState} -> ${event.newState}`);
    }
  });
  
  // Avoid heavy per-request logging of semantic tokens to prevent UI jank.
  // If verbose trace is enabled, log lightweight summaries only.
  if (isVerbose) {
    client.onNotification('textDocument/semanticTokens', (params: any) => {
      try {
        const count = Array.isArray(params?.data) ? params.data.length : (params?.data?.length ?? 'n/a');
        console.log('Semantic tokens notification (items):', count);
      } catch {
        console.log('Semantic tokens notification received');
      }
    });
    client.onRequest('textDocument/semanticTokens', (params: any) => {
      console.log('Semantic tokens request');
      return params;
    });
  }

  // Start with a timeout and proper error handling
  const startPromise = autoStart ? client.start() : Promise.resolve();
  
  // Add a timeout to detect hanging
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error('LSP server start timeout after 10 seconds')), 10000);
  });
  
  Promise.race([startPromise, timeoutPromise])
    .then(() => {
      if (isVerbose && autoStart) {
        console.log('QCL Language Server started successfully');
        // Check if semantic highlighting is enabled
        const editorConfig = vscode.workspace.getConfiguration('editor');
        const semanticHighlighting = editorConfig.get('semanticHighlighting.enabled');
        console.log('Semantic highlighting enabled:', semanticHighlighting);
      }
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
  // React to configuration changes at runtime
  context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(e => {
    if (e.affectsConfiguration('qcl.lsp.semanticTokens.enabled') || e.affectsConfiguration('qcl.lsp.semanticTokens.throttleMs')) {
      const cfg = vscode.workspace.getConfiguration('qcl.lsp');
      settings.semanticTokensEnabled = cfg.get<boolean>('semanticTokens.enabled', true);
      settings.throttleMs = Math.max(0, Number(cfg.get<number>('semanticTokens.throttleMs', 40)) || 0);
      if (isVerbose) console.log('Updated semantic tokens settings', settings);
    }
  }));
}

function getServerPath(): string | undefined {
  // Try to find the qcl-lsp executable in different locations
  const possiblePaths = [
    // Check common build output directories
    path.join(__dirname, '..', '..', 'target', 'debug', 'qcl-lsp'),
    path.join(__dirname, '..', 'target', 'debug', 'qcl-lsp'),
    path.join(__dirname, '..', '..', 'target', 'release', 'qcl-lsp'),
    path.join(__dirname, '..', 'target', 'release', 'qcl-lsp'),
    // Common user install
    expandHome('~/.cargo/bin/qcl-lsp'),
  ];

  // Reduce noisy logs unless verbose
  // console.log('Extension __dirname:', __dirname);
  // console.log('Searching for qcl-lsp binary in paths:');
  
  for (const possiblePath of possiblePaths) {
    if (!possiblePath) continue;
    try {
      if (fs.existsSync(possiblePath)) {
        // Test if the file is executable
        fs.accessSync(possiblePath, fs.constants.F_OK | fs.constants.X_OK);
        return possiblePath;
      }
    } catch {
      // ignore
    }
  }

  // Fall back to PATH resolution by returning command name
  return 'qcl-lsp';
}

function expandHome(p: string): string {
  if (!p) return '';
  if (p.startsWith('~')) {
    const home = process.env.HOME || process.env.USERPROFILE || '';
    return path.join(home, p.slice(1));
  }
  return p;
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
