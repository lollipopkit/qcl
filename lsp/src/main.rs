use dashmap::DashMap;
use regex::Regex;
use ropey::Rope;
use serde::Deserialize;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use tokio::sync::Semaphore;
use tokio::time::{sleep, Duration};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification::Progress as ProgressNotification, request::WorkDoneProgressCreate};
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;
use twox_hash::XxHash64;

mod analyzer;
use analyzer::{AnalysisResult, QclAnalyzer};
use qcl_core::{token::Span as CoreSpan, token::Token as CoreToken};

// Hard cap on number of semantic tokens sent to the client to avoid
// excessive payloads and UI work on very large files.
const MAX_SEMANTIC_TOKENS: usize = 12000;

#[cfg(test)]
mod bench_test;
#[cfg(test)]
mod inlay_hint_test;

#[derive(Debug, Default)]
struct Document {
    content: Rope,
    // LSP version, used to invalidate caches on change
    version: i32,
    // Cached results to avoid repeated parsing/tokenization per request
    cached_analysis: Option<Arc<AnalysisResult>>,
    cached_semantic_tokens: Option<Arc<Vec<SemanticToken>>>,
    // Range-based cache for better scrolling performance (version+range keyed)
    cached_range_tokens: HashMap<String, Arc<Vec<SemanticToken>>>,
    // Inlay hints cache keyed by version+range+settings
    cached_inlay_hints: HashMap<String, Arc<Vec<InlayHint>>>,
    // Last tokens/result_id actually sent to the client (for delta)
    last_sent_semantic_tokens: Option<Arc<Vec<SemanticToken>>>,
    last_sent_result_id: Option<String>,
    // Monotonic counter to produce unique result_id per document state
    tokens_result_counter: u64,
    // Debounce token incremented on each edit; used to coalesce diagnostics work
    debounce_seq: u64,
    // Last content hash for more intelligent cache invalidation
    _last_content_hash: Option<u64>,
}

struct QclLanguageServer {
    client: Client,
    documents: Arc<DashMap<Url, Document>>,
    analyzer: std::sync::Mutex<QclAnalyzer>,
    config: std::sync::Mutex<ServerConfig>,
    // Limit concurrent heavy computations (tokens/hints) to avoid CPU spikes while scrolling
    compute_limiter: std::sync::Mutex<Arc<Semaphore>>,
}

impl QclLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
            analyzer: std::sync::Mutex::new(QclAnalyzer::new()),
            config: std::sync::Mutex::new(ServerConfig::default()),
            compute_limiter: std::sync::Mutex::new(Arc::new(Semaphore::new(2))),
        }
    }

    async fn validate_document(&self, uri: &Url) -> Vec<Diagnostic> {
        match self.get_or_compute_analysis(uri).await {
            Some(analysis) => analysis.diagnostics.clone(),
            None => Vec::new(),
        }
    }

    async fn get_hover_info(&self, uri: &Url, _position: Position) -> Option<Hover> {
        // Snapshot content for position lookup
        let (content, offset) = {
            let doc = self.documents.get(uri)?;
            let off = position_to_char_idx(&doc.content, _position);
            (doc.content.to_string(), off)
        };

        // Tokenize with spans (cached) and find token at offset
        let (tokens, spans) = {
            // Prefer using the shared analyzer to leverage cache
            if let Ok(mut analyzer) = self.analyzer.lock() {
                match analyzer.tokenize_with_spans_cached(&content) {
                    Ok(pair) => pair,
                    Err(_) => return None,
                }
            } else {
                return None;
            }
        };

        if let Some((idx, _token)) = find_token_at_offset(&spans, &tokens, offset) {
            let hover_text = describe_token_hover(&tokens, &spans, idx);
            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: None,
            });
        }

        // Fallback: surface a minimal file-level hint if available
        if let Some(analysis) = self.get_or_compute_analysis(uri).await {
            if !analysis.identifier_roots.is_empty() {
                let hover_text = format!("Identifier roots: {:?}", analysis.identifier_roots);
                return Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                    range: None,
                });
            }
        }
        None
    }

    fn get_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // QCL keywords
        let keywords = [
            "if", "else", "while", "let", "fn", "return", "break", "continue", "import", "from", "as", "go", "select",
            "case", "default", "true", "false", "nil", "spawn", "chan", "send", "recv",
        ];

        for keyword in keywords {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("QCL keyword".to_string()),
                ..Default::default()
            });
        }

        // Operators
        let operators = ["==", "!=", "<=", ">=", "&&", "||", "in", "<-"];
        for op in operators {
            items.push(CompletionItem {
                label: op.to_string(),
                kind: Some(CompletionItemKind::OPERATOR),
                detail: Some("QCL operator".to_string()),
                ..Default::default()
            });
        }

        // Context access via '@' removed

        // Standard library functions (if available)
        let stdlib_functions = [
            ("print", "Global function - print without newline"),
            ("println", "Global function - print with newline"),
            ("panic", "Global function - raise runtime error"),
        ];

        for (func, desc) in stdlib_functions {
            items.push(CompletionItem {
                label: func.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(desc.to_string()),
                ..Default::default()
            });
        }

        items
    }
}

// ----------------------
// Server configuration
// ----------------------

#[derive(Debug, Clone)]
struct ServerConfig {
    inlay_hints_enabled: bool,
    inlay_hints_parameters: bool,
    inlay_hints_types: bool,
    // performance tuning
    max_concurrent: usize,
    range_token_cache_limit: usize,
    inlay_hint_cache_limit: usize,
    inlay_scan_margin_lines: usize,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            inlay_hints_enabled: true,
            inlay_hints_parameters: true,
            inlay_hints_types: true,
            max_concurrent: 2,
            range_token_cache_limit: 64,
            inlay_hint_cache_limit: 64,
            inlay_scan_margin_lines: 3,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct QclLspConfigSection {
    #[serde(default)]
    inlay_hints: InlayHintsConfig,
    #[serde(default)]
    performance: PerformanceConfig,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct InlayHintsConfig {
    #[serde(default)]
    enabled: Option<bool>,
    #[serde(default)]
    parameters: InlayKindConfig,
    #[serde(default)]
    types: InlayKindConfig,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct InlayKindConfig {
    enabled: Option<bool>,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct PerformanceConfig {
    #[serde(default)]
    max_concurrent: Option<usize>,
    #[serde(default)]
    range_token_cache_limit: Option<usize>,
    #[serde(default)]
    inlay_hint_cache_limit: Option<usize>,
    #[serde(default)]
    inlay_scan_margin_lines: Option<usize>,
}

impl QclLanguageServer {
    async fn load_config(&self) {
        // Ask client for the 'qcl.lsp' section
        let items = vec![ConfigurationItem {
            scope_uri: None,
            section: Some("qcl.lsp".to_string()),
        }];
        if let Ok(values) = self.client.configuration(items).await {
            if let Some(val) = values.into_iter().next() {
                if let Ok(cfg) = serde_json::from_value::<QclLspConfigSection>(val) {
                    let mut guard = self.config.lock().unwrap();
                    // Defaults are true unless explicitly disabled
                    guard.inlay_hints_enabled = cfg.inlay_hints.enabled.unwrap_or(true);
                    guard.inlay_hints_parameters = cfg.inlay_hints.parameters.enabled.unwrap_or(true);
                    guard.inlay_hints_types = cfg.inlay_hints.types.enabled.unwrap_or(true);
                    // Performance tuning with fallbacks to sane defaults
                    if let Some(v) = cfg.performance.max_concurrent.filter(|v| *v > 0) {
                        guard.max_concurrent = v;
                    }
                    if let Some(v) = cfg.performance.range_token_cache_limit.filter(|v| *v > 0) {
                        guard.range_token_cache_limit = v;
                    }
                    if let Some(v) = cfg.performance.inlay_hint_cache_limit.filter(|v| *v > 0) {
                        guard.inlay_hint_cache_limit = v;
                    }
                    if let Some(v) = cfg.performance.inlay_scan_margin_lines.filter(|v| *v > 0) {
                        guard.inlay_scan_margin_lines = v;
                    }
                    // Rebuild semaphore to apply new concurrency
                    let permits = guard.max_concurrent.max(1);
                    if let Ok(mut sem_arc) = self.compute_limiter.lock() {
                        *sem_arc = Arc::new(Semaphore::new(permits));
                    }
                }
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for QclLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!("QCL Language Server initializing with params: {:?}", params.root_uri);

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Switch to INCREMENTAL now that we apply ranges with UTF-16 mapping
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                    identifier: Some("qcl".to_string()),
                    inter_file_dependencies: false,
                    workspace_diagnostics: false,
                    work_done_progress_options: Default::default(),
                })),
                semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
                    SemanticTokensOptions {
                        work_done_progress_options: Default::default(),
                        legend: SemanticTokensLegend {
                            token_types: vec![
                                SemanticTokenType::COMMENT,
                                SemanticTokenType::KEYWORD,
                                SemanticTokenType::VARIABLE,
                                SemanticTokenType::FUNCTION,
                                SemanticTokenType::STRING,
                                SemanticTokenType::NUMBER,
                                SemanticTokenType::OPERATOR,
                                SemanticTokenType::PARAMETER,
                                SemanticTokenType::PROPERTY,
                                SemanticTokenType::NAMESPACE,
                                SemanticTokenType::TYPE,
                            ],
                            token_modifiers: vec![
                                SemanticTokenModifier::DECLARATION,
                                SemanticTokenModifier::DEFINITION,
                                SemanticTokenModifier::READONLY,
                                SemanticTokenModifier::STATIC,
                            ],
                        },
                        // Enable range-based semantic tokens so the editor can request
                        // only the visible region while typing for better responsiveness
                        range: Some(true),
                        // Enable delta to reduce payloads and UI work
                        full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                    },
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                document_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
                    work_done_progress_options: Default::default(),
                    resolve_provider: Some(false),
                }))),
                // Workspace capabilities left default; client will still send configuration changes
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "QCL Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("QCL Language Server initialized");
        let _ = self
            .client
            .log_message(MessageType::INFO, "QCL Language Server started")
            .await;
        // Load initial configuration from client
        self.load_config().await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("QCL Language Server shutting down");
        Ok(())
    }

    async fn did_change_configuration(&self, _params: DidChangeConfigurationParams) {
        // Reload configuration when the client notifies of changes
        self.load_config().await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let document = Document {
            content: Rope::from_str(&params.text_document.text),
            version: params.text_document.version,
            cached_analysis: None,
            cached_semantic_tokens: None,
            cached_range_tokens: HashMap::new(),
            cached_inlay_hints: HashMap::new(),
            last_sent_semantic_tokens: None,
            last_sent_result_id: None,
            tokens_result_counter: 0,
            debounce_seq: 0,
            _last_content_hash: Some(compute_content_hash(&params.text_document.text)),
        };

        self.documents.insert(uri.clone(), document);
        // Warm up analysis cache on open to keep subsequent requests fast
        let _ = self.validate_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Apply all changes (supports both full and incremental)
        {
            let mut entry = self.documents.entry(uri.clone()).or_default();
            // Ensure version is monotonic (but still update even if not; clients may resend)
            entry.version = version;

            if params.content_changes.len() == 1 && params.content_changes[0].range.is_none() {
                // Full text replacement
                let change = params.content_changes.into_iter().next().unwrap();
                entry.content = Rope::from_str(&change.text);
            } else {
                // Incremental changes
                let changes = params.content_changes;
                for change in changes {
                    apply_incremental_change_rope(&mut entry.content, &change);
                }
            }

            // Invalidate caches and bump debounce seq
            entry.cached_analysis = None;
            entry.cached_semantic_tokens = None;
            entry.cached_range_tokens.clear();
            entry.cached_inlay_hints.clear();
            entry.debounce_seq = entry.debounce_seq.wrapping_add(1);
        }

        // Periodically clear analyzer caches to prevent memory growth
        if self.documents.len() > 50 {
            if let Ok(mut analyzer) = self.analyzer.lock() {
                analyzer.clear_caches();
            }
        }

        // Debounced diagnostics (no token prewarm to keep edits snappy)
        self.schedule_diagnostics_and_warmup(uri, version, 250).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        Ok(self.get_hover_info(uri, position).await)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut items = self.get_completions();

        // Add identifier-aware and stdlib-aware completions based on current line
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(uri) {
            let line_idx = position.line as usize;
            if line_idx < doc.content.len_lines() {
                let line = doc.content.line(line_idx).to_string();
                let line_start_char = doc.content.line_to_char(line_idx);
                let abs_char = position_to_char_idx(&doc.content, position);
                let within_line = abs_char.saturating_sub(line_start_char).min(line.chars().count());
                let line_prefix: String = line.chars().take(within_line).collect();
                let line_suffix: String = line.chars().skip(within_line).collect();

                // '@' context completions removed

                // Regexes for import/from and module dot access
                let import_re = Regex::new(r"(?:^|\s)import\s+([A-Za-z_]\w*)?$").ok();
                let from_re = Regex::new(r"(?:^|\s)from\s+([A-Za-z_]\w*)?$").ok();
                let moddot_re = Regex::new(r"([A-Za-z_]\w*)\.$").ok();
                // import { ... } cursor inside braces; capture content before cursor
                let import_brace_re = Regex::new(r"(?:^|\s)import\s*\{([^}]*)$").ok();
                // In the suffix, look for '} from <module>' after the cursor
                let suffix_from_re = Regex::new(r"^\s*\}?\s*from\s+([A-Za-z_]\w*)").ok();
                // import "<path> cursor inside quotes
                let import_path_re = Regex::new(r#"(?:^|\s)import\s+\"([^\"]*)$"#).ok();

                if let Ok(mut analyzer) = self.analyzer.lock() {
                    // Suggest module names after `import` or `from`
                    if let Some(re) = &import_re {
                        if let Some(caps) = re.captures(&line_prefix) {
                            let typed = caps.get(1).map(|m| m.as_str()).unwrap_or("");
                            let modules = analyzer.list_stdlib_modules();
                            for m in modules.into_iter().filter(|m| m.starts_with(typed)) {
                                items.push(CompletionItem {
                                    label: m,
                                    kind: Some(CompletionItemKind::MODULE),
                                    detail: Some("QCL stdlib module".to_string()),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                    if let Some(re) = &from_re {
                        if let Some(caps) = re.captures(&line_prefix) {
                            let typed = caps.get(1).map(|m| m.as_str()).unwrap_or("");
                            let modules = analyzer.list_stdlib_modules();
                            for m in modules.into_iter().filter(|m| m.starts_with(typed)) {
                                items.push(CompletionItem {
                                    label: m,
                                    kind: Some(CompletionItemKind::MODULE),
                                    detail: Some("QCL stdlib module".to_string()),
                                    ..Default::default()
                                });
                            }
                        }
                    }

                    // Suggest exports after `alias.` where alias is an imported module
                    if let Some(re) = &moddot_re {
                        if let Some(caps) = re.captures(&line_prefix) {
                            let alias = caps.get(1).map(|m| m.as_str()).unwrap_or("");
                            let full_content = doc.content.to_string();
                            let alias_map = analyzer.collect_import_aliases(&full_content);
                            if let Some(module_name) = alias_map.get(alias) {
                                if let Some(exports) = analyzer.list_module_exports(module_name) {
                                    for e in exports {
                                        items.push(CompletionItem {
                                            label: e,
                                            kind: Some(CompletionItemKind::FUNCTION),
                                            detail: Some(format!("{}.{}", module_name, alias)),
                                            ..Default::default()
                                        });
                                    }
                                }
                            }
                        }
                    }

                    // Suggest exports inside `import { … } from <module>`
                    if let (Some(br_re), Some(sf_re)) = (&import_brace_re, &suffix_from_re) {
                        if let Some(br_caps) = br_re.captures(&line_prefix) {
                            if let Some(sf_caps) = sf_re.captures(&line_suffix) {
                                let module_name = sf_caps.get(1).map(|m| m.as_str()).unwrap_or("");
                                if let Some(mut exports) = analyzer.list_module_exports(module_name) {
                                    // Determine typed prefix within braces
                                    let raw = br_caps.get(1).map(|m| m.as_str()).unwrap_or("");
                                    let last = raw.split(',').next_back().unwrap_or("").trim();
                                    let typed = last.split_whitespace().last().unwrap_or("");
                                    if !typed.is_empty() {
                                        exports.retain(|e| e.starts_with(typed));
                                    }
                                    for e in exports {
                                        items.push(CompletionItem {
                                            label: e,
                                            kind: Some(CompletionItemKind::FUNCTION),
                                            detail: Some(format!("from {}", module_name)),
                                            ..Default::default()
                                        });
                                    }
                                }
                            }
                        }
                    }

                    // Suggest file paths inside import "..."
                    if let Some(re) = &import_path_re {
                        if let Some(caps) = re.captures(&line_prefix) {
                            let typed = caps.get(1).map(|m| m.as_str()).unwrap_or("");
                            // Determine base directories
                            let mut base_dirs = Vec::new();
                            if let Ok(mut p) = uri.to_file_path() {
                                if p.pop() {
                                    base_dirs.push(p.clone());
                                    base_dirs.push(p.join("lib"));
                                    base_dirs.push(p.join("modules"));
                                }
                            }
                            // Split typed into dir and file prefix
                            let (dir_part, file_prefix) = if let Some(pos) = typed.rfind('/') {
                                (&typed[..pos], &typed[pos + 1..])
                            } else {
                                ("", typed)
                            };
                            for base in base_dirs {
                                let root = if dir_part.is_empty() {
                                    base.clone()
                                } else {
                                    base.join(dir_part)
                                };
                                if let Ok(entries) = std::fs::read_dir(&root) {
                                    for e in entries.flatten() {
                                        if let Ok(ft) = e.file_type() {
                                            let name = e.file_name().to_string_lossy().to_string();
                                            if name.starts_with(file_prefix) {
                                                let rel = if dir_part.is_empty() {
                                                    name.clone()
                                                } else {
                                                    format!("{}/{}", dir_part, name)
                                                };
                                                let (label, kind) = if ft.is_dir() {
                                                    (format!("{}/", rel), CompletionItemKind::FOLDER)
                                                } else {
                                                    (rel, CompletionItemKind::FILE)
                                                };
                                                items.push(CompletionItem {
                                                    label,
                                                    kind: Some(kind),
                                                    detail: Some("File path".to_string()),
                                                    ..Default::default()
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Generic identifier/path completions based on current prefix
                    // Extract a simple alnum/underscore/dot suffix from the current line prefix
                    let prefix: String = {
                        let mut collected: Vec<char> = Vec::new();
                        for ch in line_prefix.chars().rev() {
                            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '.' {
                                collected.push(ch);
                            } else {
                                break;
                            }
                        }
                        collected.reverse();
                        collected.into_iter().collect()
                    };
                    if !prefix.is_empty() {
                        let var_items = analyzer.get_var_completions(&prefix);
                        if !var_items.is_empty() {
                            let existing: std::collections::HashSet<String> =
                                items.iter().map(|ci| ci.label.clone()).collect();
                            for it in var_items {
                                if !existing.contains(&it.label) {
                                    items.push(it);
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let mut actions: Vec<CodeActionOrCommand> = Vec::new();

        // Snapshot document content for textual replacements
        let content = if let Some(doc) = self.documents.get(uri) {
            doc.content.to_string()
        } else {
            String::new()
        };

        let rope = ropey::Rope::from_str(&content);
        for diag in &params.context.diagnostics {
            let code = diag.code.as_ref().and_then(|c| match c {
                NumberOrString::String(s) => Some(s.as_str()),
                _ => None,
            });
            if code == Some("qcl_file_not_found") || diag.message.starts_with("File not found:") {
                // Extract quoted string at diagnostic range
                let start = position_to_char_idx(&rope, diag.range.start);
                let end = position_to_char_idx(&rope, diag.range.end);
                let slice: String = if start < end && end <= rope.len_chars() {
                    rope.slice(start..end).to_string()
                } else {
                    String::new()
                };
                let current = slice.trim_matches('"');

                let mut candidates: Vec<String> = Vec::new();
                if !current.ends_with(".qcl") {
                    candidates.push(format!("{}.qcl", current));
                }
                if !current.starts_with("./") && !current.starts_with('/') {
                    candidates.push(format!("./{}", current));
                }
                for prefix in ["lib/", "modules/"] {
                    if !current.starts_with(prefix) {
                        candidates.push(format!("{}{}", prefix, current));
                        if !current.ends_with(".qcl") {
                            candidates.push(format!("{}{}.qcl", prefix, current));
                        }
                    }
                }

                for cand in candidates {
                    let new_text = format!("\"{}\"", cand);
                    let edit = TextEdit {
                        range: diag.range,
                        new_text,
                    };
                    let we = WorkspaceEdit {
                        changes: Some(std::collections::HashMap::from([(uri.clone(), vec![edit])])),
                        ..Default::default()
                    };
                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Use path: {}", cand),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diag.clone()]),
                        edit: Some(we),
                        command: None,
                        is_preferred: None,
                        disabled: None,
                        data: None,
                    }));
                }
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn diagnostic(&self, params: DocumentDiagnosticParams) -> Result<DocumentDiagnosticReportResult> {
        let uri = &params.text_document.uri;
        let diagnostics = self.validate_document(uri).await;

        Ok(DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
            RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: diagnostics,
                },
            },
        )))
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        if let Some(analysis) = self.get_or_compute_analysis(uri).await {
            if !analysis.symbols.is_empty() {
                return Ok(Some(DocumentSymbolResponse::Nested(analysis.symbols.clone())));
            }
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get document content to find symbol at position
        let content = {
            let doc = match self.documents.get(uri) {
                Some(doc) => doc,
                None => return Ok(None),
            };
            doc.content.to_string()
        };

        // Find the symbol at the cursor position
        if let Some(symbol_name) = self.find_symbol_at_position(&content, position).await {
            // Find all references to this symbol in the document
            let locations = self.find_all_references(&content, &symbol_name, uri).await;

            if !locations.is_empty() {
                return Ok(Some(locations));
            }
        }

        Ok(None)
    }

    async fn document_highlight(&self, params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let content = if let Some(doc) = self.documents.get(uri) {
            doc.content.to_string()
        } else {
            String::new()
        };
        if let Some(symbol) = self.find_symbol_at_position(&content, position).await {
            let locs = self.find_all_references(&content, &symbol, uri).await;
            if !locs.is_empty() {
                let highlights = locs
                    .into_iter()
                    .map(|loc| DocumentHighlight {
                        range: loc.range,
                        kind: Some(DocumentHighlightKind::TEXT),
                    })
                    .collect();
                return Ok(Some(highlights));
            }
        }
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name.clone();

        // Basic identifier validation: letters, digits, underscore; not starting with digit
        let is_valid_name = {
            let mut chars = new_name.chars();
            match chars.next() {
                Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
                }
                _ => false,
            }
        };
        if !is_valid_name {
            return Ok(None);
        }

        // Snapshot content
        let content = if let Some(doc) = self.documents.get(uri) {
            doc.content.to_string()
        } else {
            String::new()
        };

        // Find symbol name at position
        let Some(symbol_name) = self.find_symbol_at_position(&content, position).await else {
            return Ok(None);
        };
        // '@' context paths removed

        // Prefer precise scope-restricted references using resolver + spans
        let locations = {
            // Tokenize to compute function body line ranges
            if let Ok((tokens, spans)) = qcl_core::token::Tokenizer::tokenize_enhanced_with_spans(&content) {
                let _analyzer = crate::analyzer::QclAnalyzer::default();
                // Try to find definition precisely to determine scope
                if let Some(def_loc) = self
                    .find_definition_precise(&content, &symbol_name, position, uri)
                    .await
                {
                    let fbodies = crate::analyzer::QclAnalyzer::scan_function_blocks(&tokens, &spans);
                    // Identify if this def is inside a function body by comparing lines (0-based)
                    let def_line0 = def_loc.range.start.line;
                    // Build line ranges for each function body
                    let mut body_line_ranges: Vec<(u32, u32)> = Vec::new();
                    for fb in &fbodies {
                        let s_line = spans.get(fb.body_start_idx).map(|s| s.start.line).unwrap_or(1);
                        let e_line = spans.get(fb.body_end_idx).map(|s| s.end.line).unwrap_or(s_line);
                        body_line_ranges.push((s_line.saturating_sub(1), e_line.saturating_sub(1)));
                    }
                    // Determine selected scope range (line-based)
                    let scope_range: Option<(u32, u32)> = body_line_ranges
                        .iter()
                        .find(|(s, e)| def_line0 >= *s && def_line0 <= *e)
                        .cloned();
                    let all = self.find_all_references(&content, &symbol_name, uri).await;
                    if let Some((sline, eline)) = scope_range {
                        // Keep only references within the function body
                        all.into_iter()
                            .filter(|loc| loc.range.start.line >= sline && loc.range.end.line <= eline)
                            .collect()
                    } else {
                        // Top-level definition: include all references across the document
                        all
                    }
                } else {
                    // Fall back to full-document references
                    self.find_all_references(&content, &symbol_name, uri).await
                }
            } else {
                self.find_all_references(&content, &symbol_name, uri).await
            }
        };
        if locations.is_empty() {
            return Ok(None);
        }
        let edits: Vec<TextEdit> = locations
            .into_iter()
            .map(|loc| TextEdit {
                range: loc.range,
                new_text: new_name.clone(),
            })
            .collect();
        let mut changes = std::collections::HashMap::new();
        changes.insert(uri.clone(), edits);
        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
    }

    async fn prepare_rename(&self, params: TextDocumentPositionParams) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let position = params.position;
        // Snapshot content
        let (content, line_text) = if let Some(doc) = self.documents.get(uri) {
            let rope = &doc.content;
            let line_idx = position.line as usize;
            let line = if line_idx < rope.len_lines() {
                rope.line(line_idx).to_string()
            } else {
                String::new()
            };
            (rope.to_string(), line)
        } else {
            (String::new(), String::new())
        };

        let Some(symbol_name) = self.find_symbol_at_position(&content, position).await else {
            return Ok(None);
        };
        if symbol_name.is_empty() {
            return Ok(None);
        }

        // Compute the word range on the line around the cursor
        let mut start = position.character as usize;
        let mut end = position.character as usize;
        let chars: Vec<char> = line_text.chars().collect();
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }
        let range = Range::new(
            Position::new(position.line, start as u32),
            Position::new(position.line, end as u32),
        );
        Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
            range,
            placeholder: symbol_name,
        }))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Snapshot content
        let content = if let Some(doc) = self.documents.get(uri) {
            doc.content.to_string()
        } else {
            String::new()
        };

        // Heuristically find the function name and active parameter index
        let (func_name, active_param) = infer_call_at_position(&content, position);
        if func_name.is_empty() {
            return Ok(None);
        }

        // Collect signatures from built-ins and current document definitions
        let mut signatures: Vec<SignatureInformation> = Vec::new();

        // Built-ins and selected stdlib functions/meta-methods
        match func_name.as_str() {
            "print" => signatures.push(sig(
                "print(fmt, ...args)",
                ["fmt", "...args"].as_slice(),
                "Global function - print without newline",
            )),
            "println" => signatures.push(sig(
                "println(fmt, ...args)",
                ["fmt", "...args"].as_slice(),
                "Global function - print with newline",
            )),
            "panic" => signatures.push(sig(
                "panic(message)",
                ["message"].as_slice(),
                "Global function - raise runtime error",
            )),
            // iter module
            "enumerate" => signatures.push(sig(
                "enumerate(list)",
                ["list"].as_slice(),
                "iter: Add 0-based index to each element; returns list of [index, value]",
            )),
            "range" => {
                signatures.push(sig(
                    "range(end)",
                    ["end"].as_slice(),
                    "iter: Generate [0, 1, ..., end-1]",
                ));
                signatures.push(sig(
                    "range(start, end)",
                    ["start", "end"].as_slice(),
                    "iter: Generate [start, ..., end) with step 1",
                ));
                signatures.push(sig(
                    "range(start, end, step)",
                    ["start", "end", "step"].as_slice(),
                    "iter: Generate arithmetic progression with given step (nonzero)",
                ));
            }
            "zip" => signatures.push(sig(
                "zip(list1, list2)",
                ["list1", "list2"].as_slice(),
                "iter: Pair elements into [a[i], b[i]] up to the shortest length",
            )),
            "take" => signatures.push(sig(
                "take(list, n)",
                ["list", "n"].as_slice(),
                "iter: First n elements (n <= 0 returns [])",
            )),
            "skip" => signatures.push(sig(
                "skip(list, n)",
                ["list", "n"].as_slice(),
                "iter: Elements after skipping first n (n <= 0 returns original)",
            )),
            "chain" => signatures.push(sig(
                "chain(list1, list2)",
                ["list1", "list2"].as_slice(),
                "iter: Concatenate two lists",
            )),
            "flatten" => signatures.push(sig(
                "flatten(list)",
                ["list"].as_slice(),
                "iter: Flatten one nesting level (non-lists pass through)",
            )),
            "unique" => signatures.push(sig(
                "unique(list)",
                ["list"].as_slice(),
                "iter: Stable de-duplicate preserving first occurrences",
            )),
            "chunk" => signatures.push(sig(
                "chunk(list, size)",
                ["list", "size"].as_slice(),
                "iter: Split into chunks of positive size",
            )),
            // list meta-methods and module functions (common ones)
            "map" => {
                signatures.push(sig(
                    "map(list, func)",
                    ["list", "func(value)"].as_slice(),
                    "Apply function to each element; returns transformed list",
                ));
                signatures.push(sig(
                    "list.map(func)",
                    ["func(value)"].as_slice(),
                    "Meta-method variant of map",
                ));
            }
            "filter" => {
                signatures.push(sig(
                    "filter(list, predicate)",
                    ["list", "predicate(value)"].as_slice(),
                    "Keep elements where predicate returns true (nil/false treated as false)",
                ));
                signatures.push(sig(
                    "list.filter(predicate)",
                    ["predicate(value)"].as_slice(),
                    "Meta-method variant of filter",
                ));
            }
            "reduce" => {
                signatures.push(sig(
                    "reduce(list, init, func)",
                    ["list", "init", "func(acc, value)"].as_slice(),
                    "Fold elements into an accumulator",
                ));
                signatures.push(sig(
                    "list.reduce(init, func)",
                    ["init", "func(acc, value)"].as_slice(),
                    "Meta-method variant of reduce",
                ));
            }
            "push" => signatures.push(sig(
                "push(list, value)",
                ["list", "value"].as_slice(),
                "Return a new list with value appended",
            )),
            "concat" => signatures.push(sig(
                "concat(list, other)",
                ["list", "other"].as_slice(),
                "Concatenate two lists",
            )),
            "join" => signatures.push(sig(
                "join(list<string>, delimiter)",
                ["list", "delimiter"].as_slice(),
                "Join list of strings with delimiter",
            )),
            "get" => signatures.push(sig(
                "get(list, index)",
                ["list", "index"].as_slice(),
                "Safe index access; returns value or nil",
            )),
            "first" => signatures.push(sig("first(list)", ["list"].as_slice(), "First element or nil")),
            "last" => signatures.push(sig("last(list)", ["list"].as_slice(), "Last element or nil")),
            "len" => signatures.push(sig(
                "len(value)",
                ["value"].as_slice(),
                "Length of list/map/string (where applicable)",
            )),
            _ => {}
        }

        // Scan current document for fn definitions matching the name
        let re = Regex::new(&format!(r"(?m)\bfn\s+{}\s*\(([^)]*)\)", regex::escape(&func_name))).unwrap();
        for caps in re.captures_iter(&content) {
            if let Some(params_m) = caps.get(1) {
                let params_str = params_m.as_str();
                let params_list: Vec<String> = params_str
                    .split(',')
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                    .map(|s| s.split(':').next().unwrap_or("").trim().to_string())
                    .collect();
                let label = format!("{}({})", func_name, params_list.join(", "));
                signatures.push(sig_owned(label, params_list, "User-defined function"));
            }
        }

        if signatures.is_empty() {
            return Ok(None);
        }

        let active = active_param.unwrap_or(0).min(
            signatures
                .first()
                .and_then(|s| s.parameters.as_ref())
                .map(|v| v.len().saturating_sub(1))
                .unwrap_or(0),
        ) as u32;
        Ok(Some(SignatureHelp {
            signatures,
            active_signature: Some(0),
            active_parameter: Some(active),
        }))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get document content to find symbol at position
        let content = {
            let doc = match self.documents.get(uri) {
                Some(doc) => doc,
                None => return Ok(None),
            };
            doc.content.to_string()
        };

        // Find the symbol at the cursor position
        if let Some(symbol_name) = self.find_symbol_at_position(&content, position).await {
            // Prefer precise resolver-based decl spans
            if let Some(definition_location) = self
                .find_definition_precise(&content, &symbol_name, position, uri)
                .await
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(definition_location)));
            }
            // Fallback: heuristic text scan
            if let Some(definition_location) = self.find_definition(&content, &symbol_name, uri).await {
                return Ok(Some(GotoDefinitionResponse::Scalar(definition_location)));
            }
        }

        Ok(None)
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        let mut lenses: Vec<CodeLens> = Vec::new();
        // Lens: Analyze file
        lenses.push(CodeLens {
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            command: Some(Command {
                title: "Analyze file".to_string(),
                command: "qcl.analyzeCurrentFile".to_string(),
                arguments: None,
            }),
            data: None,
        });

        // Lens: Identifier roots used (if any)
        if let Some(analysis) = self.get_or_compute_analysis(uri).await {
            if !analysis.identifier_roots.is_empty() {
                let mut keys: Vec<_> = analysis.identifier_roots.iter().cloned().collect();
                keys.sort();
                let preview = if keys.len() <= 3 {
                    keys.join(", ")
                } else {
                    format!("{}, … ({} total)", keys[0..3].join(", "), keys.len())
                };
                lenses.push(CodeLens {
                    range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                    command: Some(Command {
                        title: format!("Identifier roots: {}", preview),
                        command: "qcl.showStatusBarMenu".to_string(),
                        arguments: None,
                    }),
                    data: None,
                });
            }
        }

        Ok(Some(lenses))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let options = params.options;
        let content = if let Some(doc) = self.documents.get(uri) {
            doc.content.to_string()
        } else {
            String::new()
        };
        let formatted = format_qcl(&content, &options);
        if formatted == content {
            return Ok(Some(vec![]));
        }
        // Full document replacement
        let rope = Rope::from_str(&content);
        let end = Position::new(
            rope.len_lines().saturating_sub(1) as u32,
            rope.line(rope.len_lines().saturating_sub(1)).len_chars() as u32,
        );
        let edit = TextEdit {
            range: Range::new(Position::new(0, 0), end),
            new_text: formatted,
        };
        Ok(Some(vec![edit]))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let (content, version, cached_opt) = if let Some(doc) = self.documents.get(uri) {
            let cfg = self.config.lock().unwrap().clone();
            let key = format!(
                "v{}:{}:{}-{}:{}:p{}:t{}",
                doc.version,
                params.range.start.line,
                params.range.start.character,
                params.range.end.line,
                params.range.end.character,
                cfg.inlay_hints_parameters as u8,
                cfg.inlay_hints_types as u8
            );
            if let Some(cached) = doc.cached_inlay_hints.get(&key) {
                return Ok(Some((**cached).clone()));
            }
            (doc.content.to_string(), doc.version, Some(key))
        } else {
            (String::new(), 0, None)
        };
        // Apply server-side configuration for inlay hints
        let cfg = self.config.lock().unwrap().clone();
        if !cfg.inlay_hints_enabled || content.is_empty() {
            return Ok(None);
        }

        // Limit concurrent heavy computations
        let sem = self.compute_limiter.lock().unwrap().clone();
        let _permit = sem.acquire().await.ok();

        let want_params = cfg.inlay_hints_parameters;
        let want_types = cfg.inlay_hints_types;
        let margin = cfg.inlay_scan_margin_lines;
        let range = params.range;
        let computed = tokio::task::spawn_blocking(move || {
            let mut hints: Vec<InlayHint> = Vec::new();
            if want_params {
                hints.extend(compute_inlay_hints_with_margin(&content, range, margin));
            }
            if want_types {
                // Tokenize once and reuse across individual computations
                if let Ok((tokens, spans)) = qcl_core::token::Tokenizer::tokenize_enhanced_with_spans(&content) {
                    let analyzer = QclAnalyzer::new();
                    let mut h1 = analyzer.compute_type_inlay_hints_from_tokens(&tokens, &spans, range);
                    let mut h2 = analyzer.compute_define_type_hints_from_tokens(&tokens, &spans, range);
                    let mut h3 = analyzer.compute_function_return_type_hints_from_tokens(&tokens, &spans, range);
                    hints.append(&mut h1);
                    hints.append(&mut h2);
                    hints.append(&mut h3);
                }
            }
            hints
        })
        .await
        .ok()
        .unwrap_or_default();

        // Filter kinds based on config flags
        let filtered: Vec<InlayHint> = computed
            .into_iter()
            .filter(|h| match h.kind.unwrap_or(InlayHintKind::TYPE) {
                InlayHintKind::PARAMETER => want_params,
                InlayHintKind::TYPE => want_types,
                _ => true,
            })
            .collect();
        // Cache by version+range+settings
        if let (Some(key), Some(mut doc)) = (cached_opt, self.documents.get_mut(uri)) {
            if doc.version == version {
                if doc.cached_inlay_hints.len() >= 64 {
                    doc.cached_inlay_hints.clear();
                }
                doc.cached_inlay_hints.insert(key, Arc::new(filtered.clone()));
            }
        }
        Ok((!filtered.is_empty()).then_some(filtered))
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        // Compute or fetch tokens for current doc state
        let tokens_arc = match self.get_or_generate_semantic_tokens(uri).await {
            Some(t) => t,
            None => return Ok(None),
        };

        // Clamp payload size for responsiveness and store the clamped baseline
        let clamped: Vec<SemanticToken> = (*tokens_arc).iter().take(MAX_SEMANTIC_TOKENS).cloned().collect();
        let clamped_arc = Arc::new(clamped.clone());

        // Produce a fresh result_id tied to current version/counter
        let result_id = {
            if let Some(mut doc) = self.documents.get_mut(uri) {
                doc.tokens_result_counter = doc.tokens_result_counter.wrapping_add(1);
                let id = format!("v{}-g{}", doc.version, doc.tokens_result_counter);
                doc.last_sent_semantic_tokens = Some(clamped_arc);
                doc.last_sent_result_id = Some(id.clone());
                Some(id)
            } else {
                None
            }
        };

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id,
            data: clamped,
        })))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = &params.text_document.uri;
        // Snapshot and versioned range cache lookup
        let (slice_string, range, version) = if let Some(doc) = self.documents.get(uri) {
            let key = format!(
                "v{}:{}:{}-{}:{}",
                doc.version,
                params.range.start.line,
                params.range.start.character,
                params.range.end.line,
                params.range.end.character
            );
            if let Some(cached) = doc.cached_range_tokens.get(&key) {
                // Return cached result immediately
                let data = (**cached).clone();
                return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                    result_id: None,
                    data,
                })));
            }
            let start_char = position_to_char_idx(&doc.content, params.range.start);
            let end_char = position_to_char_idx(&doc.content, params.range.end);
            let s = start_char.min(doc.content.len_chars());
            let e = end_char.min(doc.content.len_chars()).max(s);
            let slice_string = doc.content.slice(s..e).to_string();
            (slice_string, params.range, doc.version)
        } else {
            return Ok(None);
        };

        // Limit concurrent heavy computations
        let sem = self.compute_limiter.lock().unwrap().clone();
        let _permit = sem.acquire().await.ok();

        // Generate range tokens off the async runtime
        let generated = tokio::task::spawn_blocking(move || {
            let analyzer = QclAnalyzer::new();
            analyzer.generate_semantic_tokens_in_range(&slice_string, range)
        })
        .await
        .ok()
        .unwrap_or_default();

        // Store in versioned range cache if still applicable
        if let Some(mut doc) = self.documents.get_mut(uri) {
            if doc.version == version {
                let key = format!(
                    "v{}:{}:{}-{}:{}",
                    version, range.start.line, range.start.character, range.end.line, range.end.character
                );
                let limit = self.config.lock().unwrap().range_token_cache_limit.max(1);
                if doc.cached_range_tokens.len() >= limit {
                    doc.cached_range_tokens.clear();
                }
                doc.cached_range_tokens.insert(key, Arc::new(generated.clone()));
            }
        }

        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: generated,
        })))
    }

    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        let uri = &params.text_document.uri;

        // Compute fresh tokens for current doc state
        let new_tokens_full = match self.get_or_generate_semantic_tokens(uri).await {
            Some(t) => t,
            None => return Ok(None),
        };
        // Clamp to match what we send to clients
        let new_tokens: Vec<SemanticToken> = (*new_tokens_full).iter().take(MAX_SEMANTIC_TOKENS).cloned().collect();

        // Read previous baseline (last sent) and id
        let (prev_tokens_opt, prev_id_opt) = if let Some(doc) = self.documents.get(uri) {
            (doc.last_sent_semantic_tokens.clone(), doc.last_sent_result_id.clone())
        } else {
            (None, None)
        };

        // If client's previousResultId doesn't match our last sent id, fall back to full tokens
        let prev_id_matches = if let Some(server_prev) = prev_id_opt.clone() {
            params.previous_result_id == server_prev
        } else {
            false
        };

        // Compute new result_id and update last_sent baseline (store clamped)
        let new_result_id = if let Some(mut doc) = self.documents.get_mut(uri) {
            doc.tokens_result_counter = doc.tokens_result_counter.wrapping_add(1);
            let id = format!("v{}-g{}", doc.version, doc.tokens_result_counter);
            doc.last_sent_semantic_tokens = Some(Arc::new(new_tokens.clone()));
            doc.last_sent_result_id = Some(id.clone());
            Some(id)
        } else {
            None
        };

        if !prev_id_matches {
            // Resync: send full tokens
            return Ok(Some(SemanticTokensFullDeltaResult::Tokens(SemanticTokens {
                result_id: new_result_id,
                data: new_tokens.clone(),
            })));
        }

        // Compute a compact delta with a single edit using common prefix/suffix
        let prev_tokens = match prev_tokens_opt {
            Some(p) => p,
            None => {
                return Ok(Some(SemanticTokensFullDeltaResult::Tokens(SemanticTokens {
                    result_id: new_result_id,
                    data: new_tokens,
                })));
            }
        };

        let prev_vec: Vec<SemanticToken> = (*prev_tokens).clone();
        let (cp, cs, delete_count) = common_prefix_suffix_delete_count(&prev_vec, &new_tokens);
        if delete_count == 0 {
            // No structural change; in theory could return empty edits
            return Ok(Some(SemanticTokensFullDeltaResult::TokensDelta(SemanticTokensDelta {
                result_id: new_result_id,
                edits: vec![],
            })));
        }

        let insert_slice: Vec<SemanticToken> = new_tokens[cp..(new_tokens.len() - cs)].to_vec();
        let edit = SemanticTokensEdit {
            start: cp as u32,
            delete_count: delete_count as u32,
            data: Some(insert_slice),
        };
        Ok(Some(SemanticTokensFullDeltaResult::TokensDelta(SemanticTokensDelta {
            result_id: new_result_id,
            edits: vec![edit],
        })))
    }
}

fn compute_content_hash(content: &str) -> u64 {
    let mut hasher = XxHash64::default();
    content.hash(&mut hasher);
    hasher.finish()
}

impl QclLanguageServer {
    // Get cached analysis or compute and store it atomically
    async fn get_or_compute_analysis(&self, uri: &Url) -> Option<Arc<AnalysisResult>> {
        // Fast path: try read cache
        if let Some(doc) = self.documents.get(uri) {
            if let Some(cached) = doc.cached_analysis.clone() {
                return Some(cached);
            }
        }

        // Snapshot content/version/debounce without long-held locks
        let (content_snapshot, version_snapshot, seq_snapshot) = {
            let doc = self.documents.get(uri)?;
            (doc.content.to_string(), doc.version, doc.debounce_seq)
        };

        // Compute analysis off the async runtime to avoid blocking
        let content_for_compute = content_snapshot.clone();
        // Compute base_dir from URI for file import resolution
        let base_dir = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.parent().map(|p| p.to_path_buf()));
        let computed_result = tokio::task::spawn_blocking(move || {
            let mut analyzer = QclAnalyzer::new();
            if let Some(b) = base_dir {
                analyzer.set_base_dir(b);
            }
            analyzer.analyze(&content_for_compute)
        })
        .await
        .ok()?;
        let computed = Arc::new(computed_result);

        // Store if still applicable; otherwise return computed as-is (caller can re-request)
        if let Some(mut doc) = self.documents.get_mut(uri) {
            if doc.version == version_snapshot && doc.debounce_seq == seq_snapshot {
                doc.cached_analysis = Some(computed.clone());
            }
        }
        Some(computed)
    }

    // Get cached semantic tokens or compute and store
    async fn get_or_generate_semantic_tokens(&self, uri: &Url) -> Option<Arc<Vec<SemanticToken>>> {
        if let Some(doc) = self.documents.get(uri) {
            if let Some(cached) = doc.cached_semantic_tokens.clone() {
                return Some(cached);
            }
        }

        let (content_snapshot, version_snapshot, seq_snapshot) = {
            let doc = self.documents.get(uri)?;
            (doc.content.to_string(), doc.version, doc.debounce_seq)
        };

        // Generate tokens off the async runtime to avoid blocking
        let content_for_tokens = content_snapshot.clone();
        let base_dir = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.parent().map(|p| p.to_path_buf()));
        // Limit concurrent heavy computations
        let sem = self.compute_limiter.lock().unwrap().clone();
        let _permit = sem.acquire().await.ok();
        // Generate tokens off the async runtime to avoid blocking
        let generated_result = tokio::task::spawn_blocking(move || {
            let mut analyzer = QclAnalyzer::new();
            if let Some(b) = base_dir {
                analyzer.set_base_dir(b);
            }
            analyzer.generate_semantic_tokens(&content_for_tokens)
        })
        .await
        .ok()?;
        let generated = Arc::new(generated_result);

        if let Some(mut doc) = self.documents.get_mut(uri) {
            if doc.version == version_snapshot && doc.debounce_seq == seq_snapshot {
                doc.cached_semantic_tokens = Some(generated.clone());
            }
        }
        Some(generated)
    }

    async fn schedule_diagnostics_and_warmup(&self, uri: Url, scheduled_version: i32, delay_ms: u64) {
        let documents = self.documents.clone();
        let client = self.client.clone();
        tokio::spawn(async move {
            sleep(Duration::from_millis(delay_ms)).await;

            // Check debounce token to ensure no new edits have occurred
            let (content_snapshot, seq_snapshot, version_snapshot) = if let Some(doc) = documents.get(&uri) {
                (doc.content.to_string(), doc.debounce_seq, doc.version)
            } else {
                return;
            };

            // Create and begin a work-done progress to surface checking state in clients
            let token = NumberOrString::String(format!("qcl:diag:{}", uri));
            let _ = client
                .send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams { token: token.clone() })
                .await;
            let _ = client
                .send_notification::<ProgressNotification>(ProgressParams {
                    token: token.clone(),
                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(WorkDoneProgressBegin {
                        title: "QCL: Checking".to_string(),
                        cancellable: Some(false),
                        message: Some(uri.to_string()),
                        percentage: None,
                    })),
                })
                .await;

            // Compute analysis on snapshot off the runtime thread.
            // Avoid generating full semantic tokens here; that is computed lazily
            // when the editor explicitly asks for tokens, which keeps edits responsive.
            let content_for_compute = content_snapshot.clone();
            let base_dir = uri
                .to_file_path()
                .ok()
                .and_then(|p| p.parent().map(|p| p.to_path_buf()));
            let analysis = match tokio::task::spawn_blocking(move || {
                let mut analyzer = QclAnalyzer::new();
                if let Some(b) = base_dir {
                    analyzer.set_base_dir(b);
                }
                analyzer.analyze(&content_for_compute)
            })
            .await
            {
                Ok(pair) => pair,
                Err(_) => {
                    let _ = client
                        .send_notification::<ProgressNotification>(ProgressParams {
                            token: token.clone(),
                            value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                                message: Some("Analysis cancelled".to_string()),
                            })),
                        })
                        .await;
                    return;
                }
            };

            // Try to store caches if document still matches snapshot
            if let Some(mut doc) = documents.get_mut(&uri) {
                if doc.debounce_seq == seq_snapshot
                    && doc.version == scheduled_version
                    && doc.version == version_snapshot
                {
                    doc.cached_analysis = Some(Arc::new(analysis));
                    // Do not precompute tokens here to avoid heavy work after each edit.
                    // Tokens will be generated on demand by semanticTokens requests.
                }
            }

            // End work-done progress
            let _ = client
                .send_notification::<ProgressNotification>(ProgressParams {
                    token: token.clone(),
                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                        message: Some("Diagnostics updated".to_string()),
                    })),
                })
                .await;
        });
    }

    /// Find the symbol name at the given position in the content
    async fn find_symbol_at_position(&self, content: &str, position: Position) -> Option<String> {
        // Convert position to character offset
        let lines: Vec<&str> = content.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_offset = position.character as usize;

        if char_offset >= line.len() {
            return None;
        }

        // Try to tokenize and find the token at the position
        if let Ok(mut analyzer) = self.analyzer.lock() {
            if let Ok((tokens, spans)) = analyzer.tokenize_with_spans_cached(content) {
                // Convert line/column position to absolute character offset
                let mut absolute_offset = 0;
                for (i, line_text) in lines.iter().enumerate() {
                    if i == position.line as usize {
                        absolute_offset += char_offset;
                        break;
                    }
                    absolute_offset += line_text.len() + 1; // +1 for newline
                }

                // Find token at this offset
                if let Some((_, token)) = find_token_at_offset(&spans, &tokens, absolute_offset) {
                    use qcl_core::token::Token;
                    if let Token::Id(name) = token {
                        return Some(name);
                    }
                }
            }
        }

        // Fallback: extract identifier at cursor position using simple text analysis
        let chars: Vec<char> = line.chars().collect();
        if char_offset >= chars.len() {
            return None;
        }

        // Find the start and end of the identifier at the cursor
        let mut start = char_offset;
        let mut end = char_offset;

        // Move start backwards to find beginning of identifier
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        // Move end forwards to find end of identifier
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        if start < end {
            let symbol: String = chars[start..end].iter().collect();
            if !symbol.is_empty() {
                return Some(symbol);
            }
        }

        None
    }

    /// Find all references to the given symbol in the content
    async fn find_all_references(&self, content: &str, symbol_name: &str, uri: &Url) -> Vec<Location> {
        let mut locations = Vec::new();
        let lines: Vec<&str> = content.lines().collect();

        {
            // Handle regular identifiers (variables, functions)
            for (line_idx, line) in lines.iter().enumerate() {
                let mut start = 0;
                while let Some(pos) = line[start..].find(symbol_name) {
                    let absolute_pos = start + pos;
                    let chars: Vec<char> = line.chars().collect();

                    // Check if this is a whole word match (not part of another identifier)
                    let is_word_start = absolute_pos == 0
                        || !chars
                            .get(absolute_pos - 1)
                            .map(|c| c.is_alphanumeric() || *c == '_')
                            .unwrap_or(false);
                    let is_word_end = absolute_pos + symbol_name.len() >= chars.len()
                        || !chars
                            .get(absolute_pos + symbol_name.len())
                            .map(|c| c.is_alphanumeric() || *c == '_')
                            .unwrap_or(false);

                    if is_word_start && is_word_end {
                        let range = Range::new(
                            Position::new(line_idx as u32, absolute_pos as u32),
                            Position::new(line_idx as u32, (absolute_pos + symbol_name.len()) as u32),
                        );
                        locations.push(Location::new(uri.clone(), range));
                    }
                    start = absolute_pos + 1;
                }
            }
        }

        locations
    }

    /// Find the definition location of a symbol in the content
    async fn find_definition(&self, content: &str, symbol_name: &str, uri: &Url) -> Option<Location> {
        let lines: Vec<&str> = content.lines().collect();

        // '@' context access removed; proceed with regular identifiers

        // Look for symbol definitions in the document
        for (line_idx, line) in lines.iter().enumerate() {
            let trimmed = line.trim();

            // Check for variable declaration: "let symbol_name ="
            if trimmed.starts_with("let ") && trimmed.contains(&format!("{} =", symbol_name)) {
                if let Some(pos) = line.find(&format!("let {}", symbol_name)) {
                    let range = Range::new(
                        Position::new(line_idx as u32, (pos + 4) as u32), // Skip "let "
                        Position::new(line_idx as u32, (pos + 4 + symbol_name.len()) as u32),
                    );
                    return Some(Location::new(uri.clone(), range));
                }
            }

            // Check for function declaration: "fn symbol_name("
            if trimmed.starts_with("fn ") && trimmed.contains(&format!("{}(", symbol_name)) {
                if let Some(pos) = line.find(&format!("fn {}", symbol_name)) {
                    let range = Range::new(
                        Position::new(line_idx as u32, (pos + 3) as u32), // Skip "fn "
                        Position::new(line_idx as u32, (pos + 3 + symbol_name.len()) as u32),
                    );
                    return Some(Location::new(uri.clone(), range));
                }
            }

            // Check for label declaration: "symbol_name:"
            if trimmed.starts_with(&format!("{}:", symbol_name)) {
                if let Some(pos) = line.find(&format!("{}:", symbol_name)) {
                    let range = Range::new(
                        Position::new(line_idx as u32, pos as u32),
                        Position::new(line_idx as u32, (pos + symbol_name.len()) as u32),
                    );
                    return Some(Location::new(uri.clone(), range));
                }
            }

            // Check for import statements: "import symbol_name" or "from symbol_name"
            if (trimmed.starts_with("import ") && trimmed.contains(&format!("import {}", symbol_name)))
                || (trimmed.starts_with("from ") && trimmed.contains(&format!("from {}", symbol_name)))
            {
                if let Some(pos) = line.find(symbol_name) {
                    let range = Range::new(
                        Position::new(line_idx as u32, pos as u32),
                        Position::new(line_idx as u32, (pos + symbol_name.len()) as u32),
                    );
                    return Some(Location::new(uri.clone(), range));
                }
            }
        }

        None
    }

    /// More precise definition finder using slot resolver + scanned spans.
    async fn find_definition_precise(
        &self,
        content: &str,
        symbol_name: &str,
        pos: Position,
        uri: &Url,
    ) -> Option<Location> {
        // Tokenize with spans
        let (tokens, spans) = match qcl_core::token::Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(p) => p,
            Err(_) => return None,
        };
        // Parse statements (we only need structure to drive resolver)
        let mut parser = qcl_core::stmt::stmt_parser::StmtParser::new_with_spans(&tokens, &spans);
        let program = parser.parse_program_with_enhanced_errors(content).ok()?;
        // Resolve and enrich with spans
        let mut resolver = qcl_core::resolve::slots::SlotResolver::new();
        let resolution = resolver.resolve_program_slots(&program);
        let analyzer = crate::analyzer::QclAnalyzer::default();
        let enriched = analyzer.enrich_layout_spans(&resolution.root, &tokens, &spans);
        // Find function blocks to locate the innermost function for position
        let fblocks = crate::analyzer::QclAnalyzer::scan_function_blocks(&tokens, &spans);
        // Compute an approximate offset based on line/column
        let cursor_line = pos.line + 1;
        let cursor_col = pos.character + 1;
        // Attempt to pick the first span on the target line for offset estimation
        let mut cursor_offset = 0usize;
        for sp in &spans {
            if sp.start.line == cursor_line {
                cursor_offset = sp.start.offset + (cursor_col.saturating_sub(sp.start.column)) as usize;
                break;
            }
        }
        // Determine if inside a function body and pick that child layout
        let mut candidate_spans: Vec<qcl_core::token::Span> = Vec::new();
        let mut pick_child: Option<usize> = None;
        for (i, fb) in fblocks.iter().enumerate() {
            let s = spans.get(fb.body_start_idx)?.start.offset;
            let e = spans.get(fb.body_end_idx)?.end.offset;
            if cursor_offset >= s && cursor_offset <= e {
                pick_child = Some(i);
                break;
            }
        }
        if let Some(ci) = pick_child {
            if let Some(child) = enriched.children.get(ci) {
                for d in &child.decls {
                    if d.name == symbol_name {
                        if let Some(sp) = &d.span {
                            candidate_spans.push(sp.clone());
                        }
                    }
                }
            }
            // Fallback to top-level decls if not found in child scope (e.g., function names)
            if candidate_spans.is_empty() {
                for d in &enriched.decls {
                    if d.name == symbol_name {
                        if let Some(sp) = &d.span {
                            candidate_spans.push(sp.clone());
                        }
                    }
                }
            }
        } else {
            for d in &enriched.decls {
                if d.name == symbol_name {
                    if let Some(sp) = &d.span {
                        candidate_spans.push(sp.clone());
                    }
                }
            }
        }
        if let Some(sp) = candidate_spans.first() {
            let range = Range::new(
                Position::new(sp.start.line - 1, sp.start.column - 1),
                Position::new(sp.end.line - 1, sp.end.column - 1),
            );
            return Some(Location::new(uri.clone(), range));
        }
        None
    }
}

// Compute common prefix and suffix lengths between two token arrays and the delete count in the old array.
fn common_prefix_suffix_delete_count(old: &[SemanticToken], new: &[SemanticToken]) -> (usize, usize, usize) {
    let mut cp = 0usize;
    let min_len = old.len().min(new.len());
    while cp < min_len && semantic_token_eq(&old[cp], &new[cp]) {
        cp += 1;
    }

    // If completely equal
    if cp == old.len() && old.len() == new.len() {
        return (cp, 0, 0);
    }

    let mut cs = 0usize;
    while cs < (old.len() - cp)
        && cs < (new.len() - cp)
        && semantic_token_eq(&old[old.len() - 1 - cs], &new[new.len() - 1 - cs])
    {
        cs += 1;
    }
    let delete_count = old.len().saturating_sub(cp + cs);
    (cp, cs, delete_count)
}

fn semantic_token_eq(a: &SemanticToken, b: &SemanticToken) -> bool {
    a.delta_line == b.delta_line
        && a.delta_start == b.delta_start
        && a.length == b.length
        && a.token_type == b.token_type
        && a.token_modifiers_bitset == b.token_modifiers_bitset
}

// Convert LSP UTF-16 position to Rope char index (scalar values), clamped to line end.
fn position_to_char_idx(text: &Rope, pos: Position) -> usize {
    let line_idx = pos.line as usize;
    if line_idx >= text.len_lines() {
        return text.len_chars();
    }
    let line_start_char = text.line_to_char(line_idx);
    let line_slice = text.line(line_idx);
    let target_utf16 = pos.character as usize;

    // Fast path: ASCII-only line where UTF-16 units == chars
    if let Some(s) = line_slice.as_str() {
        if s.is_ascii() {
            let len_chars = s.len(); // bytes == chars for ASCII
            let clamped = target_utf16.min(len_chars);
            return line_start_char + clamped;
        }
    }
    let mut seen_utf16 = 0usize;
    let mut chars_in_line = 0usize;
    for ch in line_slice.chars() {
        let u16_len = ch.len_utf16();
        if seen_utf16 + u16_len > target_utf16 {
            break;
        }
        seen_utf16 += u16_len;
        chars_in_line += 1;
        if seen_utf16 == target_utf16 {
            break;
        }
    }
    line_start_char + chars_in_line
}

fn apply_incremental_change_rope(text: &mut Rope, change: &TextDocumentContentChangeEvent) {
    if let Some(range) = &change.range {
        let start_char = position_to_char_idx(text, range.start);
        let end_char = position_to_char_idx(text, range.end);
        let (s, e) = if start_char <= end_char {
            (start_char, end_char)
        } else {
            (end_char, start_char)
        };
        if s != e {
            text.remove(s..e);
        }
        if !change.text.is_empty() {
            text.insert(s, &change.text);
        }
    } else {
        // Full replacement fallback
        *text = Rope::from_str(&change.text);
    }
}

// Find the token covering the given absolute char offset using half-open [start,end) spans.
fn find_token_at_offset(spans: &[CoreSpan], tokens: &[CoreToken], offset: usize) -> Option<(usize, CoreToken)> {
    for (i, span) in spans.iter().enumerate() {
        if offset >= span.start.offset && offset < span.end.offset {
            return Some((i, tokens[i].clone()));
        }
    }
    None
}

// Build a concise, position-aware hover message for a token.
fn describe_token_hover(tokens: &[CoreToken], _spans: &[CoreSpan], idx: usize) -> String {
    use CoreToken as T;
    let tok = &tokens[idx];

    // Attempt to extract full member path when hovering on identifiers or dot segments
    // Legacy '@' context path hover removed

    match tok {
        T::Id(name) => {
            // Heuristic: function call if next token is '('
            let is_call = tokens.get(idx + 1).map(|t| matches!(t, T::LParen)).unwrap_or(false);
            if is_call {
                // Provide stdlib hover if known
                if let Some((sig, doc)) = stdlib_func_hover(name) {
                    format!("{}\n{}", sig, doc)
                } else {
                    format!("Function call: {}(…)", name)
                }
            } else {
                format!("Identifier: {}", name)
            }
        }
        T::Str(s) => format!("String literal: \"{}\"", s),
        T::Int(i) => format!("Integer: {}", i),
        T::Float(f) => format!("Float: {}", f),
        T::Bool(b) => format!("Boolean: {}", b),
        T::Nil => "Nil literal".to_string(),

        // Keywords
        T::If => "Keyword: if".to_string(),
        T::Else => "Keyword: else".to_string(),
        T::While => "Keyword: while".to_string(),
        T::Let => "Keyword: let".to_string(),
        T::Break => "Keyword: break".to_string(),
        T::Continue => "Keyword: continue".to_string(),
        T::Return => "Keyword: return".to_string(),
        T::Fn => "Keyword: fn".to_string(),
        T::Import => "Keyword: import".to_string(),
        T::From => "Keyword: from".to_string(),
        T::As => "Keyword: as".to_string(),

        // Operators and punctuation
        T::Eq => "Operator: ==".to_string(),
        T::Ne => "Operator: !=".to_string(),
        T::Ge => "Operator: >=".to_string(),
        T::Le => "Operator: <=".to_string(),
        T::Gt => "Operator: >".to_string(),
        T::Lt => "Operator: <".to_string(),
        T::And => "Operator: &&".to_string(),
        T::Or => "Operator: ||".to_string(),
        T::Not => "Operator: !".to_string(),
        T::In => "Operator: in".to_string(),
        T::Assign => "Operator: =".to_string(),
        T::Add => "Operator: +".to_string(),
        T::Sub => "Operator: -".to_string(),
        T::Mul => "Operator: *".to_string(),
        T::Div => "Operator: /".to_string(),
        T::Mod => "Operator: %".to_string(),
        T::Dot => "Accessor: .".to_string(),
        T::Colon => "Symbol: :".to_string(),
        T::Comma => "Symbol: ,".to_string(),
        T::Semicolon => "Symbol: ;".to_string(),
        // '@' token removed from lexer
        T::LParen => "Symbol: (".to_string(),
        T::RParen => "Symbol: )".to_string(),
        T::LBrace => "Symbol: {".to_string(),
        T::RBrace => "Symbol: }".to_string(),
        T::LBracket => "Symbol: [".to_string(),
        T::RBracket => "Symbol: ]".to_string(),
        T::For => "Keyword: for".to_string(),
        T::Range => "Operator: ..".to_string(),
        T::RangeInclusive => "Operator: ..=".to_string(),

        // Concurrency keywords
        T::Spawn => "Concurrency: spawn".to_string(),
        T::Chan => "Concurrency: chan".to_string(),
        T::Send => "Concurrency: send".to_string(),
        T::Recv => "Concurrency: recv".to_string(),
        T::Select => "Concurrency: select".to_string(),
        T::Case => "Concurrency: case".to_string(),
        T::Default => "Concurrency: default".to_string(),
        T::Arrow => "Symbol: =>".to_string(),
        T::LeftArrow => "Symbol: <=".to_string(),
        T::OptionalDot => "Operator: ?.".to_string(),
        T::NullishCoalescing => "Operator: ??".to_string(),
        T::TemplateString(_) => "Formatted string".to_string(),
        // Type system tokens
        T::Type => "Keyword: type".to_string(),
        T::Trait => "Keyword: trait".to_string(),
        T::Impl => "Keyword: impl".to_string(),
        T::Pipe => "Operator: |".to_string(),
        T::Question => "Operator: ?".to_string(),
        T::FnArrow => "Operator: ->".to_string(),
        T::AddAssign => "Operator: +=".to_string(),
        T::SubAssign => "Operator: -=".to_string(),
        T::MulAssign => "Operator: *=".to_string(),
        T::DivAssign => "Operator: /=".to_string(),
        T::ModAssign => "Operator: %=".to_string(),
        T::Match => "Keyword: match".to_string(),
    }
}

fn stdlib_func_hover(name: &str) -> Option<(&'static str, &'static str)> {
    // Minimal doc table for common stdlib functions
    match name {
        // globals
        "print" => Some(("print(fmt, ...args)", "Print without newline")),
        "println" => Some(("println(fmt, ...args)", "Print with newline")),
        "panic" => Some(("panic(message)", "Raise runtime error with message")),

        // iter
        "enumerate" => Some(("enumerate(list)", "Return [[0, x0], [1, x1], ...]")),
        "range" => Some(("range([start,] end [, step])", "Generate integer range (step != 0)")),
        "zip" => Some(("zip(list1, list2)", "Pair elements up to the shortest length")),
        "take" => Some(("take(list, n)", "First n elements")),
        "skip" => Some(("skip(list, n)", "Drop first n elements")),
        "chain" => Some(("chain(list1, list2)", "Concatenate lists")),
        "flatten" => Some(("flatten(list)", "Flatten one nesting level")),
        "unique" => Some(("unique(list)", "Stable de-duplicate")),
        "chunk" => Some(("chunk(list, size)", "Split into chunks of positive size")),

        // list (meta-methods commonly used)
        "map" => Some(("map(list, func) | list.map(func)", "Apply func to each element")),
        "filter" => Some((
            "filter(list, pred) | list.filter(pred)",
            "Keep elements where pred returns true",
        )),
        "reduce" => Some((
            "reduce(list, init, func) | list.reduce(init, func)",
            "Fold elements into accumulator",
        )),
        "push" => Some(("push(list, value)", "Append value (returns new list)")),
        "concat" => Some(("concat(list, other)", "Concatenate two lists")),
        "join" => Some(("join(list<string>, delim)", "Join strings with delimiter")),
        "get" => Some(("get(list, index)", "Safe index access; returns value or nil")),
        "first" => Some(("first(list)", "First element or nil")),
        "last" => Some(("last(list)", "Last element or nil")),
        "len" => Some(("len(value)", "Length of list/map/string")),
        _ => None,
    }
}

// Infer the function call under the cursor and the active parameter index.
fn infer_call_at_position(content: &str, position: Position) -> (String, Option<usize>) {
    let lines: Vec<&str> = content.lines().collect();
    let line_idx = position.line as usize;
    if line_idx >= lines.len() {
        return (String::new(), None);
    }
    let col = position.character as isize;
    let line = lines[line_idx];
    let prefix = &line[..line.len().min(col.max(0) as usize)];

    // Walk backwards to find the nearest '(' and count commas for active parameter
    let mut depth = 0i32;
    let mut commas = 0usize;
    for (i, ch) in prefix.chars().rev().enumerate() {
        match ch {
            ')' => depth += 1,
            '(' => {
                if depth == 0 {
                    // Identify the function name before '('
                    let start = prefix.len().saturating_sub(i + 1);
                    let before = &prefix[..start];
                    let fname = before
                        .trim_end()
                        .chars()
                        .rev()
                        .take_while(|c| c.is_alphanumeric() || *c == '_')
                        .collect::<String>()
                        .chars()
                        .rev()
                        .collect::<String>();
                    return (fname, Some(commas));
                } else {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => commas += 1,
            _ => {}
        }
    }
    (String::new(), None)
}

fn sig(label: &str, params: &[&str], doc: &str) -> SignatureInformation {
    SignatureInformation {
        label: label.to_string(),
        documentation: Some(Documentation::String(doc.to_string())),
        parameters: Some(
            params
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple((*p).to_string()),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    }
}

fn sig_owned(label: String, params: Vec<String>, doc: &str) -> SignatureInformation {
    SignatureInformation {
        label,
        documentation: Some(Documentation::String(doc.to_string())),
        parameters: Some(
            params
                .into_iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(p),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    }
}

fn format_qcl(input: &str, options: &FormattingOptions) -> String {
    // Simple indentation formatter based on braces and parentheses.
    let mut out = String::with_capacity(input.len() + 16);
    let use_spaces = options.insert_spaces;
    let tab_size = options.tab_size.clamp(1, 8) as usize;
    let mut indent = 0isize;

    for raw_line in input.lines() {
        let line = raw_line.trim();
        // Reduce indent if line starts with a closing token
        let leading_closers = line
            .chars()
            .take_while(|c| c.is_whitespace() || *c == '}' || *c == ')' || *c == ']')
            .filter(|c| *c == '}' || *c == ')' || *c == ']')
            .count();
        if leading_closers > 0 && indent > 0 {
            indent -= leading_closers as isize;
            if indent < 0 {
                indent = 0;
            }
        }

        // Emit indentation
        if use_spaces {
            for _ in 0..(indent.max(0) as usize * tab_size) {
                out.push(' ');
            }
        } else {
            for _ in 0..indent.max(0) {
                out.push('\t');
            }
        }
        out.push_str(line);
        out.push('\n');

        // Adjust indent increases for next line
        let mut delta = 0isize;
        for ch in line.chars() {
            match ch {
                '{' | '(' | '[' => delta += 1,
                '}' | ')' | ']' => delta -= 1,
                _ => {}
            }
        }
        indent += delta;
        if indent < 0 {
            indent = 0;
        }
    }

    // Preserve trailing newline convention similar to input
    out
}

#[cfg(test)]
pub(crate) fn compute_inlay_hints(content: &str, range: Range) -> Vec<InlayHint> {
    // Default margin for tests and callers not providing a margin
    compute_inlay_hints_with_margin(content, range, 3)
}

fn compute_inlay_hints_with_margin(content: &str, range: Range, margin_lines: usize) -> Vec<InlayHint> {
    // Collect function parameter names from local fn definitions
    let mut defs: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    if let Ok(re) = Regex::new(r"(?m)\bfn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)") {
        for caps in re.captures_iter(content) {
            let name = caps.get(1).map(|m| m.as_str()).unwrap_or("").to_string();
            let params_str = caps.get(2).map(|m| m.as_str()).unwrap_or("");
            let params: Vec<String> = params_str
                .split(',')
                .map(|s| s.trim())
                .filter(|s| !s.is_empty())
                .map(|s| s.split(':').next().unwrap_or("").trim().to_string())
                .collect();
            if !name.is_empty() {
                defs.insert(name, params);
            }
        }
    }
    // Built-ins
    defs.entry("print".to_string())
        .or_insert_with(|| vec!["fmt".into(), "...args".into()]);
    defs.entry("println".to_string())
        .or_insert_with(|| vec!["fmt".into(), "...args".into()]);
    defs.entry("panic".to_string())
        .or_insert_with(|| vec!["message".into()]);

    // Precompute line starts for mapping offsets to (line,col)
    let mut line_starts: Vec<usize> = Vec::new();
    line_starts.push(0);
    for (i, b) in content.as_bytes().iter().enumerate() {
        if *b == b'\n' {
            line_starts.push(i + 1);
        }
    }
    let within_range = |ofs: usize| -> bool {
        let mut line = 0usize;
        for (idx, start) in line_starts.iter().enumerate() {
            if *start > ofs {
                break;
            }
            line = idx;
        }
        let line_u = line as u32;
        line_u >= range.start.line && line_u <= range.end.line
    };

    let mut hints = Vec::new();
    let bytes = content.as_bytes();
    // Compute a scanning window around the requested range to avoid scanning the entire file
    let total_lines = line_starts.len();
    let start_line = range.start.line as usize;
    let end_line = range.end.line as usize;
    let scan_start_line = start_line.saturating_sub(margin_lines);
    let scan_end_line = (end_line + margin_lines).min(total_lines.saturating_sub(1));
    let scan_start_byte = line_starts.get(scan_start_line).copied().unwrap_or(0);
    let scan_end_byte = if scan_end_line + 1 < total_lines {
        line_starts[scan_end_line + 1]
    } else {
        content.len()
    };

    let mut i = scan_start_byte;
    let mut in_string: Option<u8> = None;
    let mut in_line_comment = false;
    while i < bytes.len() && i < scan_end_byte {
        if bytes[i] == b'\n' {
            in_line_comment = false;
            i += 1;
            continue;
        }
        if in_line_comment {
            i += 1;
            continue;
        }
        if in_string.is_none() && i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'/' {
            in_line_comment = true;
            i += 2;
            continue;
        }
        if let Some(q) = in_string {
            if bytes[i] == b'\\' {
                i = (i + 2).min(bytes.len());
                continue;
            }
            if bytes[i] == q {
                in_string = None;
                i += 1;
                continue;
            }
            i += 1;
            continue;
        } else if bytes[i] == b'"' || bytes[i] == b'\'' {
            in_string = Some(bytes[i]);
            i += 1;
            continue;
        }

        if i >= scan_end_byte {
            break;
        }
        if !(bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
            i += 1;
            continue;
        }
        let name_start = i;
        while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1;
        }
        let name_end = i;
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() || bytes[i] != b'(' {
            continue;
        }

        // Skip function definitions like `fn name(`
        let mut j = name_start;
        while j > 0 && bytes[j - 1].is_ascii_whitespace() {
            j -= 1;
        }
        let is_fn_def = if j >= 2 {
            let kw = &bytes[j - 2..j];
            kw == b"fn" && (j < 3 || !bytes[j - 3].is_ascii_alphanumeric() && bytes[j - 3] != b'_')
        } else {
            false
        };
        if is_fn_def {
            i += 1;
            continue;
        }

        let name = &content[name_start..name_end];
        let Some(params) = defs.get(name).cloned() else {
            i += 1;
            continue;
        };

        // Scan arguments across lines
        let mut pos = i + 1;
        let mut arg_index = 0usize;
        let mut depth = 0i32;
        let mut str_state: Option<u8> = None;
        let mut line_comment = false;
        let mut arg_start = pos;
        while pos < bytes.len() {
            let ch = bytes[pos];
            if ch == b'\n' {
                line_comment = false;
                pos += 1;
                continue;
            }
            if line_comment {
                pos += 1;
                continue;
            }
            if str_state.is_none() && pos + 1 < bytes.len() && bytes[pos] == b'/' && bytes[pos + 1] == b'/' {
                line_comment = true;
                pos += 2;
                continue;
            }
            if let Some(q) = str_state {
                if ch == b'\\' {
                    pos = (pos + 2).min(bytes.len());
                    continue;
                }
                if ch == q {
                    str_state = None;
                    pos += 1;
                    continue;
                }
                pos += 1;
                continue;
            } else if ch == b'"' || ch == b'\'' {
                str_state = Some(ch);
                pos += 1;
                continue;
            }

            match ch as char {
                '(' | '[' | '{' => {
                    if ch == b'(' && depth == 0 {
                        // Attempt nested call parsing just before this '('
                        let mut l = pos;
                        // skip whitespace between ident and '('
                        while l > 0 && bytes[l - 1].is_ascii_whitespace() {
                            l -= 1;
                        }
                        let mut k = l;
                        while k > 0 && (bytes[k - 1].is_ascii_alphanumeric() || bytes[k - 1] == b'_') {
                            k -= 1;
                        }
                        if k < l {
                            let nested_name = &content[k..l];
                            if let Some(nested_params) = defs.get(nested_name).cloned() {
                                // Parse nested args from pos+1
                                let mut np = pos + 1;
                                let mut ndepth = 0i32;
                                let mut nstr: Option<u8> = None;
                                let mut nline_comment = false;
                                let mut nstart = np;
                                let mut nidx = 0usize;
                                while np < bytes.len() {
                                    let nch = bytes[np];
                                    if nch == b'\n' {
                                        nline_comment = false;
                                        np += 1;
                                        continue;
                                    }
                                    if nline_comment {
                                        np += 1;
                                        continue;
                                    }
                                    if nstr.is_none()
                                        && np + 1 < bytes.len()
                                        && bytes[np] == b'/'
                                        && bytes[np + 1] == b'/'
                                    {
                                        nline_comment = true;
                                        np += 2;
                                        continue;
                                    }
                                    if let Some(q) = nstr {
                                        if nch == b'\\' {
                                            np = (np + 2).min(bytes.len());
                                            continue;
                                        }
                                        if nch == q {
                                            nstr = None;
                                            np += 1;
                                            continue;
                                        }
                                        np += 1;
                                        continue;
                                    } else if nch == b'"' || nch == b'\'' {
                                        nstr = Some(nch);
                                        np += 1;
                                        continue;
                                    }

                                    match nch as char {
                                        '(' | '[' | '{' => {
                                            ndepth += 1;
                                        }
                                        ')' => {
                                            if ndepth == 0 {
                                                let (hp, ok) = first_sig_pos(content, nstart, np);
                                                if ok && nidx < nested_params.len() && within_range(hp) {
                                                    hints.push(make_param_hint(&nested_params[nidx], hp, &line_starts));
                                                }
                                                break;
                                            } else {
                                                ndepth -= 1;
                                            }
                                        }
                                        ',' => {
                                            if ndepth == 0 {
                                                let (hp, ok) = first_sig_pos(content, nstart, np);
                                                if ok && nidx < nested_params.len() && within_range(hp) {
                                                    hints.push(make_param_hint(&nested_params[nidx], hp, &line_starts));
                                                }
                                                nidx += 1;
                                                nstart = np + 1;
                                            }
                                        }
                                        _ => {}
                                    }
                                    np += 1;
                                }
                            }
                        }
                    }
                    depth += 1;
                }
                ')' => {
                    if depth == 0 {
                        let (hint_pos, ok) = first_sig_pos(content, arg_start, pos);
                        if ok && arg_index < params.len() && within_range(hint_pos) {
                            hints.push(make_param_hint(&params[arg_index], hint_pos, &line_starts));
                        }
                        i = pos + 1; // advance outer scanner
                        break;
                    } else {
                        depth -= 1;
                    }
                }
                ',' => {
                    if depth == 0 {
                        let (hint_pos, ok) = first_sig_pos(content, arg_start, pos);
                        if ok && arg_index < params.len() && within_range(hint_pos) {
                            hints.push(make_param_hint(&params[arg_index], hint_pos, &line_starts));
                        }
                        arg_index += 1;
                        arg_start = pos + 1;
                    }
                }
                _ => {}
            }
            pos += 1;
        }
        if pos >= bytes.len() {
            i = pos;
        }
    }
    hints
}

fn first_sig_pos(content: &str, start: usize, end: usize) -> (usize, bool) {
    let slice = &content[start..end];
    let mut acc = 0usize;
    for ch in slice.chars() {
        if !ch.is_whitespace() {
            return (start + acc, true);
        }
        acc += ch.len_utf8();
    }
    (start, false)
}

fn make_param_hint(param: &str, ofs: usize, line_starts: &[usize]) -> InlayHint {
    let mut line = 0usize;
    for (idx, start) in line_starts.iter().enumerate() {
        if *start > ofs {
            break;
        }
        line = idx;
    }
    let col = ofs - line_starts[line];
    InlayHint {
        position: Position::new(line as u32, col as u32),
        label: InlayHintLabel::from(format!("{}:", param)),
        kind: Some(InlayHintKind::PARAMETER),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: Some(false),
        data: None,
    }
}

// replaced by multi-line aware helpers above

// Context path extraction using legacy prefix has been removed

#[tokio::main]
async fn main() {
    // Check CLI args for one-shot analysis mode before starting LSP server
    if let Some(output) = try_cli_analyze().unwrap_or_else(|e| {
        eprintln!("qcl-lsp analyze error: {e}");
        std::process::exit(2);
    }) {
        // Print JSON result to stdout and exit
        println!("{}", output);
        return;
    }

    // Initialize tracing to stderr to avoid interfering with LSP protocol on stdout
    tracing_subscriber::fmt().with_writer(std::io::stderr).init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(QclLanguageServer::new);

    // Start the server
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn try_cli_analyze() -> anyhow::Result<Option<String>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 {
        return Ok(None);
    }

    // Look for `--analyze <file>` anywhere in the args
    if let Some(i) = args.iter().position(|a| a == "--analyze") {
        // Find the file path (next non-flag argument after --analyze)
        let mut path_index = i + 1;
        while path_index < args.len() && args[path_index].starts_with("--") {
            path_index += 1;
        }

        let path = args.get(path_index).cloned().ok_or_else(|| {
            anyhow::anyhow!("Usage: qcl-lsp --analyze [--errors-only] <relative-file-path>\n  --analyze <file>     : Full analysis with JSON output\n  --errors-only        : Show only errors in simple format")
        })?;

        // Check if --errors-only flag is present
        let errors_only = args.iter().any(|a| a == "--errors-only");

        // Read file content with safety checks
        let content = read_file_content(&path)?;

        // Run analysis using the same analyzer used by the LSP
        let mut analyzer = QclAnalyzer::new();
        let analysis = analyzer.analyze(&content);

        if errors_only {
            // Only output errors in a simplified format
            let errors: Vec<String> = analysis
                .diagnostics
                .iter()
                .filter(|d| d.severity == Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR))
                .map(|d| {
                    format!(
                        "Line {}:{}: {}",
                        d.range.start.line + 1,
                        d.range.start.character + 1,
                        d.message
                    )
                })
                .collect();

            if errors.is_empty() {
                return Ok(Some("No errors found".to_string()));
            } else {
                return Ok(Some(errors.join("\n")));
            }
        } else {
            // Full analysis output
            let tokens = analyzer.generate_semantic_tokens(&content);

            // Convert HashSet to Vec for deterministic JSON output
            let mut id_roots: Vec<String> = analysis.identifier_roots.iter().cloned().collect();
            id_roots.sort();

            // Map tokens to simple arrays to avoid requiring serde on LSP types
            let tokens_simple: Vec<[u32; 5]> = tokens
                .iter()
                .map(|t| {
                    [
                        t.delta_line,
                        t.delta_start,
                        t.length,
                        t.token_type,
                        t.token_modifiers_bitset,
                    ]
                })
                .collect();

            let output = serde_json::json!({
                "diagnostics": analysis.diagnostics,
                "symbols": analysis.symbols,
                "identifier_roots": id_roots,
                "semantic_tokens": tokens_simple
            });
            return Ok(Some(serde_json::to_string_pretty(&output)?));
        }
    }

    Ok(None)
}

fn is_safe_path(path: &str) -> bool {
    use std::path::{Component, Path};
    let path = Path::new(path);

    if path.as_os_str().is_empty() {
        return false;
    }
    if path.is_absolute() {
        return false;
    }
    if path.components().any(|c| c == Component::ParentDir) {
        return false;
    }

    // Basic sanitization similar to CLI: block control chars and normalize components
    let s = path.to_string_lossy();
    let suspicious = ['\0', '\n', '\r', '\t'];
    if s.chars().any(|c| suspicious.contains(&c)) {
        return false;
    }
    // Reject Windows drive-like prefixes in a relative string (e.g. "C:\\...")
    if s.len() >= 2 {
        let bytes = s.as_bytes();
        if bytes[1] == b':' {
            return false;
        }
    }
    true
}

fn read_file_content(path: &str) -> anyhow::Result<String> {
    if !is_safe_path(path) {
        return Err(anyhow::anyhow!("Unsafe file path: {}", path));
    }
    std::fs::read_to_string(path).map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))
}
