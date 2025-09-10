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
let statusBarItem: vscode.StatusBarItem;
let isManuallyDisabled = false;

export function activate(context: vscode.ExtensionContext) {
  console.log('QCL extension is now active');

  // Create status bar item
  statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
  statusBarItem.text = '$(sync~spin) QCL LSP: Starting...';
  statusBarItem.tooltip = 'QCL Language Server is starting';
  statusBarItem.command = 'qcl.showStatusBarMenu';
  statusBarItem.show();
  context.subscriptions.push(statusBarItem);

  // Register commands
  const startCommand = vscode.commands.registerCommand('qcl.startServer', async () => {
    if (!client) {
      vscode.window.showErrorMessage('QCL Language Server client not initialized');
      return;
    }
    try {
      updateStatusBar('starting');
      await client.start();
      vscode.window.showInformationMessage('QCL Language Server started');
    } catch (e: any) {
      vscode.window.showErrorMessage('Failed to start QCL Language Server: ' + (e?.message || e));
    }
  });

  const restartCommand = vscode.commands.registerCommand('qcl.restartServer', async () => {
    if (client) {
      console.log('Restarting QCL Language Server...');
      updateStatusBar('starting');
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage('QCL Language Server restarted');
    }
  });

  const statusBarMenuCommand = vscode.commands.registerCommand('qcl.showStatusBarMenu', async () => {
    const items: vscode.QuickPickItem[] = [];
    
    if (isManuallyDisabled) {
      items.push({
        label: '$(play) Enable QCL LSP',
        description: 'Start the language server',
        detail: 'Enable QCL Language Server'
      });
    } else {
      items.push({
        label: '$(sync) Restart QCL LSP',
        description: 'Restart the language server',
        detail: 'Restart QCL Language Server'
      });
      
      items.push({
        label: '$(circle-slash) Disable QCL LSP',
        description: 'Temporarily disable (memory state)',
        detail: 'Disable QCL Language Server temporarily'
      });
    }
    
    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'QCL Language Server Actions',
      title: 'QCL Language Server'
    });
    
    if (!selected) return;
    
    if (selected.label.includes('Enable')) {
      isManuallyDisabled = false;
      await vscode.commands.executeCommand('qcl.startServer');
    } else if (selected.label.includes('Restart')) {
      await vscode.commands.executeCommand('qcl.restartServer');
    } else if (selected.label.includes('Disable')) {
      isManuallyDisabled = true;
      if (client) {
        await client.stop();
      }
      updateStatusBar('disabled');
      vscode.window.showInformationMessage('QCL Language Server disabled temporarily');
    }
  });

  context.subscriptions.push(startCommand, restartCommand, statusBarMenuCommand);

  // Check if LSP is enabled
  const config = vscode.workspace.getConfiguration('qcl.lsp');
  const lspEnabled = config.get<boolean>('enabled', true);
  const autoStart = config.get<boolean>('autoStart', true);
  
  if (!lspEnabled || isManuallyDisabled) {
    console.log('QCL LSP is disabled in configuration or manually disabled');
    updateStatusBar('disabled');
    return;
  }

  // Get the path to the QCL LSP server
  const customServerPath = config.get<string>('serverPath', '');
  const serverPath = customServerPath ? expandHome(customServerPath) : getServerPath();

  console.log('Looking for QCL LSP server...');
  console.log('Server path resolved to:', serverPath ?? 'PATH: qcl-lsp');

  // If the server path is not found, show an error and return
  if (!serverPath) {
    updateStatusBar('error', 'Server not found');
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

  // Scroll detection and enhanced throttling
  let isScrolling = false;
  let scrollTimeout: NodeJS.Timeout | undefined;
  let lastScrollTime = 0;
  
  // Track scroll events to detect when user is scrolling
  const scrollDetection = vscode.window.onDidChangeTextEditorVisibleRanges(() => {
    isScrolling = true;
    lastScrollTime = Date.now();
    
    // Clear previous timeout
    if (scrollTimeout) {
      clearTimeout(scrollTimeout);
    }
    
    // Set timeout to mark scrolling as finished
    scrollTimeout = setTimeout(() => {
      isScrolling = false;
    }, 300); // Wait 300ms after last scroll event
  });

  context.subscriptions.push(scrollDetection);

  // Enhanced semantic tokens middleware with scroll-aware throttling
  const lastTokenReqAt = new Map<string, number>();
  const rangeTokenReqAt = new Map<string, number>();
  const settings = { semanticTokensEnabled, throttleMs };
  
  const middleware: Middleware = {
    provideDocumentSemanticTokens(document, token, next) {
      if (!settings.semanticTokensEnabled) {
        if (isVerbose) console.log('Semantic tokens disabled (full)');
        return null;
      }
      
      // Apply stricter throttling during scrolling
      const effectiveThrottleMs = isScrolling ? settings.throttleMs * 2 : settings.throttleMs;
      
      if (effectiveThrottleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastTokenReqAt.get(key) || 0;
        if (now - last < effectiveThrottleMs) {
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
      
      // Much stricter throttling for range requests during scrolling
      const key = `${document.uri.toString()}-${range.start.line}-${range.end.line}`;
      const now = Date.now();
      const last = rangeTokenReqAt.get(key) || 0;
      
      // Skip range requests entirely during scrolling if requested recently
      if (isScrolling && (now - lastScrollTime < 500)) {
        if (now - last < settings.throttleMs * 3) {
          if (isVerbose) console.log('Semantic tokens range skipped during scrolling');
          return null;
        }
      }
      
      // Normal throttling for non-scrolling scenarios
      if (settings.throttleMs > 0) {
        if (now - last < settings.throttleMs) {
          if (isVerbose) console.log('Semantic tokens range throttled');
          return null;
        }
      }
      
      rangeTokenReqAt.set(key, now);
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
    
    // Update status bar based on state
    switch (event.newState) {
      case 1: // Starting
        updateStatusBar('starting');
        break;
      case 2: // Running
        updateStatusBar('running');
        break;
      case 3: // Stopped
        updateStatusBar('stopped');
        break;
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
      updateStatusBar('running');
    })
    .catch((error) => {
      console.error('Failed to start QCL Language Server:', error);
      console.error('Error details:', JSON.stringify(error, null, 2));
      vscode.window.showErrorMessage('Failed to start QCL Language Server: ' + error.message);
      updateStatusBar('error', 'Start failed');
      
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

function updateStatusBar(state: string, customMessage?: string) {
  if (!statusBarItem) {
    return;
  }
  
  switch (state) {
    case 'starting':
      statusBarItem.text = '$(sync~spin) QCL LSP: Starting...';
      statusBarItem.tooltip = 'QCL Language Server is starting';
      break;
    case 'running':
      statusBarItem.text = '$(check) QCL LSP: Running';
      statusBarItem.tooltip = 'QCL Language Server is running';
      break;
    case 'stopped':
      statusBarItem.text = '$(circle-slash) QCL LSP: Stopped';
      statusBarItem.tooltip = 'QCL Language Server is stopped';
      break;
    case 'error':
      statusBarItem.text = '$(error) QCL LSP: Error';
      statusBarItem.tooltip = customMessage ? `QCL Language Server error: ${customMessage}` : 'QCL Language Server error';
      break;
    case 'disabled':
      statusBarItem.text = '$(circle-slash) QCL LSP: Disabled';
      statusBarItem.tooltip = isManuallyDisabled ? 'QCL Language Server is temporarily disabled (click to enable)' : 'QCL Language Server is disabled in settings';
      break;
    default:
      statusBarItem.text = '$(question) QCL LSP: Unknown';
      statusBarItem.tooltip = 'QCL Language Server status unknown';
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  updateStatusBar('stopped');
  return client.stop();
}
