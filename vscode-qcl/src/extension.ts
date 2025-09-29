import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  RevealOutputChannelOn,
  State as ClientState,
} from 'vscode-languageclient/node';
import type { Middleware } from 'vscode-languageclient/node';
import { execFile } from 'child_process';

let client: LanguageClient;
let statusBarItem: vscode.StatusBarItem;
let isManuallyDisabled = false;
let checkInFlight = 0;
let checkIdleTimer: NodeJS.Timeout | undefined;

// Runtime settings snapshot (kept in sync with workspace configuration)
const runtime = {
  semanticTokensEnabled: true,
  semanticTokensThrottleMs: 40,
  inlayHintsEnabled: true,
  inlayHintsThrottleMs: 25,
  inlayHintsShowParameters: true,
  inlayHintsShowTypes: true,
};

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

      // Inline menu toggles for inlay hints
      items.push({
        label: `${runtime.inlayHintsEnabled ? '$(eye)' : '$(eye-closed)'} Toggle Inlay Hints`,
        description: runtime.inlayHintsEnabled ? 'Disable inline hints' : 'Enable inline hints',
        detail: `Parameters: ${runtime.inlayHintsShowParameters ? 'on' : 'off'}, Types: ${runtime.inlayHintsShowTypes ? 'on' : 'off'}`,
      });
      items.push({
        label: `${runtime.inlayHintsShowParameters ? '$(check)' : '$(circle-slash)'} Parameter Hints`,
        description: 'Show argument names in calls',
        detail: 'Editor inlay hints: parameters',
      });
      items.push({
        label: `${runtime.inlayHintsShowTypes ? '$(check)' : '$(circle-slash)'} Type Hints`,
        description: 'Show inferred types for declarations',
        detail: 'Editor inlay hints: types',
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
    } else if (selected.label.includes('Toggle Inlay Hints')) {
      runtime.inlayHintsEnabled = !runtime.inlayHintsEnabled;
      await vscode.workspace.getConfiguration('qcl.lsp').update('inlayHints.enabled', runtime.inlayHintsEnabled, vscode.ConfigurationTarget.Workspace);
      vscode.window.showInformationMessage(`QCL Inlay Hints ${runtime.inlayHintsEnabled ? 'enabled' : 'disabled'}`);
      // Trigger refresh
      await vscode.commands.executeCommand('editor.action.inlineHints.refresh');
    } else if (selected.label.includes('Parameter Hints')) {
      runtime.inlayHintsShowParameters = !runtime.inlayHintsShowParameters;
      await vscode.workspace.getConfiguration('qcl.lsp').update('inlayHints.parameters.enabled', runtime.inlayHintsShowParameters, vscode.ConfigurationTarget.Workspace);
      vscode.window.showInformationMessage(`QCL Parameter Hints ${runtime.inlayHintsShowParameters ? 'enabled' : 'disabled'}`);
      await vscode.commands.executeCommand('editor.action.inlineHints.refresh');
    } else if (selected.label.includes('Type Hints')) {
      runtime.inlayHintsShowTypes = !runtime.inlayHintsShowTypes;
      await vscode.workspace.getConfiguration('qcl.lsp').update('inlayHints.types.enabled', runtime.inlayHintsShowTypes, vscode.ConfigurationTarget.Workspace);
      vscode.window.showInformationMessage(`QCL Type Hints ${runtime.inlayHintsShowTypes ? 'enabled' : 'disabled'}`);
      await vscode.commands.executeCommand('editor.action.inlineHints.refresh');
    }
  });

  context.subscriptions.push(startCommand, restartCommand, statusBarMenuCommand);

  // Analyze current file via qcl-lsp --analyze (uses relative, sanitized path)
  const analyzeCommand = vscode.commands.registerCommand('qcl.analyzeCurrentFile', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'qcl') {
      vscode.window.showWarningMessage('Open a QCL file to analyze.');
      return;
    }
    const ws = vscode.workspace.workspaceFolders?.[0];
    if (!ws) {
      vscode.window.showWarningMessage('Open a workspace folder to run analysis.');
      return;
    }

    const abs = editor.document.uri.fsPath;
    const root = ws.uri.fsPath;
    let rel = path.relative(root, abs);
    // Normalize to POSIX-like separators for CLI and guard against .. or absolute
    rel = rel.split(path.sep).join('/');
    if (!rel || rel.startsWith('..') || path.isAbsolute(rel) || rel.includes('..')) {
      vscode.window.showErrorMessage('Refusing to analyze: file must be inside the workspace and use a safe relative path.');
      return;
    }

    const pick = await vscode.window.showQuickPick([
      { label: 'Full JSON', description: 'Show full analysis output' },
      { label: 'Errors Only', description: 'List only errors' }
    ], { title: 'QCL Analyze Current File' });
    if (!pick) return;

    const serverPath = getServerPath();
    if (!serverPath) {
      vscode.window.showErrorMessage('QCL LSP server binary not found. Build the project or configure qcl.lsp.serverPath.');
      return;
    }

    const args = ['--analyze'];
    if (pick.label.startsWith('Errors')) args.push('--errors-only');
    args.push(rel);

    const out = vscode.window.createOutputChannel('QCL Analysis');
    out.clear();
    out.show(true);
    out.appendLine(`Running: ${serverPath} ${args.join(' ')}`);
    execFile(serverPath, args, { cwd: root }, (err, stdout, stderr) => {
      if (err) {
        out.appendLine('--- Error ---');
        out.appendLine(String(err.message || err));
      }
      if (stderr && stderr.trim().length) {
        out.appendLine('--- Stderr ---');
        out.appendLine(stderr);
      }
      if (stdout && stdout.trim().length) {
        out.appendLine('--- Output ---');
        out.appendLine(stdout);
      }
    });
  });
  context.subscriptions.push(analyzeCommand);

  // Check if LSP is enabled
  const config = vscode.workspace.getConfiguration('qcl.lsp');
  const lspEnabled = config.get<boolean>('enabled', true);
  const autoStart = config.get<boolean>('autoStart', true);
  // Load runtime settings from configuration
  runtime.semanticTokensEnabled = config.get<boolean>('semanticTokens.enabled', true);
  runtime.semanticTokensThrottleMs = Math.max(0, Number(config.get<number>('semanticTokens.throttleMs', 40)) || 0);
  runtime.inlayHintsEnabled = config.get<boolean>('inlayHints.enabled', true);
  runtime.inlayHintsThrottleMs = Math.max(0, Number(config.get<number>('inlayHints.throttleMs', 25)) || 0);
  runtime.inlayHintsShowParameters = config.get<boolean>('inlayHints.parameters.enabled', true);
  runtime.inlayHintsShowTypes = config.get<boolean>('inlayHints.types.enabled', true);
  
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
  const semanticTokensEnabled = runtime.semanticTokensEnabled;
  const throttleMs = runtime.semanticTokensThrottleMs;

  // Lightweight, per-document throttle map (separate for tokens and hints)
  const lastTokenReqAt = new Map<string, number>();
  const lastInlayReqAt = new Map<string, number>();
  const settings = { semanticTokensEnabled, throttleMs };
  
  const middleware: Middleware = {
    // Surface diagnostics flow to toggle checking status when results arrive
    handleDiagnostics(uri, diagnostics, next) {
      try {
        // Diagnostics arrived: clear any pending checking indicator
        endChecking();
      } finally {
        next(uri, diagnostics);
      }
    },
    provideDocumentSemanticTokens(document, token, next) {
      if (!settings.semanticTokensEnabled) {
        if (isVerbose) console.log('Semantic tokens disabled (full)');
        return null;
      }
      beginChecking('semanticTokens(full)');
      if (settings.throttleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastTokenReqAt.get(key) || 0;
        if (now - last < settings.throttleMs) {
          if (isVerbose) console.log('Semantic tokens full throttled');
          endChecking();
          return null;
        }
        lastTokenReqAt.set(key, now);
      }
      const result = next(document, token);
      if (result && typeof (result as any).then === 'function') {
        return (result as Promise<any>)
          .finally(() => endChecking());
      } else {
        endChecking();
        return result as any;
      }
    },
    provideDocumentRangeSemanticTokens(document, range, token, next) {
      if (!settings.semanticTokensEnabled) {
        if (isVerbose) console.log('Semantic tokens disabled (range)');
        return null;
      }
      beginChecking('semanticTokens(range)');
      if (settings.throttleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastTokenReqAt.get(key) || 0;
        if (now - last < settings.throttleMs) {
          if (isVerbose) console.log('Semantic tokens range throttled');
          endChecking();
          return null;
        }
        lastTokenReqAt.set(key, now);
      }
      const result = next(document, range, token);
      if (result && typeof (result as any).then === 'function') {
        return (result as Promise<any>)
          .finally(() => endChecking());
      } else {
        endChecking();
        return result as any;
      }
    },
    // Inlay hints: show checking spinner and support throttling + filtering
    provideInlayHints(document, range, token, next) {
      if (!runtime.inlayHintsEnabled) {
        return null;
      }
      beginChecking('inlayHints');
      if (runtime.inlayHintsThrottleMs > 0) {
        const key = document.uri.toString();
        const now = Date.now();
        const last = lastInlayReqAt.get(key) || 0;
        if (now - last < runtime.inlayHintsThrottleMs) {
          endChecking();
          return null;
        }
        lastInlayReqAt.set(key, now);
      }
      const res = next(document, range, token);
      const filter = (hints: vscode.InlayHint[] | null | undefined) => {
        if (!hints) return hints;
        const wantParams = runtime.inlayHintsShowParameters;
        const wantTypes = runtime.inlayHintsShowTypes;
        return hints.filter(h => {
          const kind = (h.kind ?? vscode.InlayHintKind.Type);
          if (kind === vscode.InlayHintKind.Parameter) return wantParams;
          if (kind === vscode.InlayHintKind.Type) return wantTypes;
          return true;
        });
      };
      if (res && typeof (res as any).then === 'function') {
        return (res as Promise<vscode.InlayHint[] | null | undefined>)
          .then(filter)
          .finally(() => endChecking());
      } else {
        try {
          return filter(res as any);
        } finally {
          endChecking();
        }
      }
    },
    // If the server emits WorkDone progress, reflect it in the status bar
    handleWorkDoneProgress(token, params, next) {
      try {
        if (params && (params as any).kind) {
          const kind = (params as any).kind as 'begin' | 'report' | 'end';
          if (kind === 'begin') {
            beginChecking('progress');
          } else if (kind === 'end') {
            endChecking();
          }
        }
      } finally {
        next(token, params);
      }
    }
  };

  // React to configuration changes
  context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(e => {
    if (!e.affectsConfiguration('qcl.lsp')) return;
    const cfg = vscode.workspace.getConfiguration('qcl.lsp');
    runtime.semanticTokensEnabled = cfg.get<boolean>('semanticTokens.enabled', true);
    runtime.semanticTokensThrottleMs = Math.max(0, Number(cfg.get<number>('semanticTokens.throttleMs', 40)) || 0);
    runtime.inlayHintsEnabled = cfg.get<boolean>('inlayHints.enabled', true);
    runtime.inlayHintsThrottleMs = Math.max(0, Number(cfg.get<number>('inlayHints.throttleMs', 25)) || 0);
    runtime.inlayHintsShowParameters = cfg.get<boolean>('inlayHints.parameters.enabled', true);
    runtime.inlayHintsShowTypes = cfg.get<boolean>('inlayHints.types.enabled', true);
    // Soft nudge so users see status change quickly
    nudgeChecking();
  }));

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
    // Never auto-reveal the output unless user explicitly opens it
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    traceOutputChannel: traceLevel !== 'off' ? vscode.window.createOutputChannel('QCL Language Server Trace') : undefined,
    middleware
  };

  // Create and attach an output channel only when enabled/verbose
  if (outputChannelEnabled || isVerbose) {
    const outputChannel = vscode.window.createOutputChannel('QCL Language Server');
    clientOptions.outputChannel = outputChannel;
  }

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
      case ClientState.Starting:
        updateStatusBar('starting');
        break;
      case ClientState.Running:
        updateStatusBar('running');
        break;
      case ClientState.Stopped:
        updateStatusBar('stopped');
        break;
    }
  });
  
  // Avoid extra semantic token logging to keep UI responsive

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
  
  // Mark as checking when QCL documents change or save; diagnostics will clear it
  context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(e => {
    if (e.document.languageId === 'qcl') {
      nudgeChecking();
    }
  }));
  context.subscriptions.push(vscode.workspace.onDidSaveTextDocument(doc => {
    if (doc.languageId === 'qcl') {
      nudgeChecking();
    }
  }));
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
  const exe = process.platform === 'win32' ? '.exe' : '';
  const possiblePaths = [
    // Check common build output directories
    path.join(__dirname, '..', '..', 'target', 'debug', `qcl-lsp${exe}`),
    path.join(__dirname, '..', 'target', 'debug', `qcl-lsp${exe}`),
    path.join(__dirname, '..', '..', 'target', 'release', `qcl-lsp${exe}`),
    path.join(__dirname, '..', 'target', 'release', `qcl-lsp${exe}`),
    // Common user install
    expandHome(`~/.cargo/bin/qcl-lsp${exe}`),
  ];

  // Reduce noisy logs unless verbose
  // console.log('Extension __dirname:', __dirname);
  // console.log('Searching for qcl-lsp binary in paths:');
  
  for (const possiblePath of possiblePaths) {
    if (!possiblePath) continue;
    try {
      if (fs.existsSync(possiblePath)) {
        // Test if the file is executable (skip X_OK on Windows)
        const mode = process.platform === 'win32'
          ? fs.constants.F_OK
          : (fs.constants.F_OK | fs.constants.X_OK);
        fs.accessSync(possiblePath, mode);
        return possiblePath;
      }
    } catch {
      // ignore
    }
  }

  // Fall back to PATH resolution by returning command name
  return process.platform === 'win32' ? 'qcl-lsp.exe' : 'qcl-lsp';
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
    case 'checking':
      statusBarItem.text = '$(sync~spin) QCL LSP: Checking...';
      statusBarItem.tooltip = 'QCL Language Server is analyzing/validating';
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

function beginChecking(_reason?: string) {
  if (!statusBarItem || isManuallyDisabled) {
    return;
  }
  checkInFlight++;
  if (checkInFlight === 1) {
    if (checkIdleTimer) {
      clearTimeout(checkIdleTimer);
      checkIdleTimer = undefined;
    }
    updateStatusBar('checking');
  }
}

function endChecking() {
  if (!statusBarItem) return;
  if (checkInFlight > 0) checkInFlight--;
  if (checkInFlight === 0) {
    if (checkIdleTimer) clearTimeout(checkIdleTimer);
    // Small delay to avoid flicker if more work immediately follows
    checkIdleTimer = setTimeout(() => updateStatusBar('running'), 150);
  }
}

// UI-only nudge to show 'Checking…' without affecting the in-flight counter.
function nudgeChecking() {
  if (!statusBarItem || isManuallyDisabled) return;
  if (checkInFlight === 0) {
    if (checkIdleTimer) {
      clearTimeout(checkIdleTimer);
      checkIdleTimer = undefined;
    }
    updateStatusBar('checking');
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  updateStatusBar('stopped');
  return client.stop();
}
