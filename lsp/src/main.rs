use dashmap::DashMap;
use ropey::Rope;
use std::collections::HashMap;
use std::sync::Arc;
use std::hash::{Hash, Hasher};
use tokio::time::{sleep, Duration};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;
use twox_hash::XxHash64;
use regex::Regex;

mod analyzer;
use analyzer::{AnalysisResult, QclAnalyzer};
use qcl_core::{error::Span as CoreSpan, token::Token as CoreToken};

// Hard cap on number of semantic tokens sent to the client to avoid
// excessive payloads and UI work on very large files.
const MAX_SEMANTIC_TOKENS: usize = 12000;

#[cfg(test)]
mod bench_test;


#[derive(Debug, Default)]
struct Document {
    content: Rope,
    // LSP version, used to invalidate caches on change
    version: i32,
    // Cached results to avoid repeated parsing/tokenization per request
    cached_analysis: Option<Arc<AnalysisResult>>,
    cached_semantic_tokens: Option<Arc<Vec<SemanticToken>>>,
    // Range-based cache for better scrolling performance
    _cached_range_tokens: HashMap<String, Arc<Vec<SemanticToken>>>,
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
}

impl QclLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
            analyzer: std::sync::Mutex::new(QclAnalyzer::new()),
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
            if !analysis.context_references.is_empty() {
                let hover_text = format!("Context keys: {:?}", analysis.context_references);
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
            "if", "else", "while", "let", "fn", "return", "break", "continue", "goto", "import",
            "from", "as", "go", "select", "case", "default", "true", "false", "nil",
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

        // Context access
        items.push(CompletionItem {
            label: "@".to_string(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some("Context access".to_string()),
            documentation: Some(Documentation::String(
                "Access context variables (e.g., @req.user.role)".to_string(),
            )),
            ..Default::default()
        });

        // Standard library functions (if available)
        let stdlib_functions = [
            ("abs", "Math function - absolute value"),
            ("sqrt", "Math function - square root"),
            ("sin", "Math function - sine"),
            ("cos", "Math function - cosine"),
            ("len", "String/Collection function - length"),
            ("substr", "String function - substring"),
            ("make_chan", "Concurrency function - create channel"),
            ("send", "Channel function - send value"),
            ("recv", "Channel function - receive value"),
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

#[tower_lsp::async_trait]
impl LanguageServer for QclLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!(
            "QCL Language Server initializing with params: {:?}",
            params.root_uri
        );

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Switch to INCREMENTAL now that we apply ranges with UTF-16 mapping
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["@".to_string(), ".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("qcl".to_string()),
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        work_done_progress_options: Default::default(),
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
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
                    ),
                ),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo { name: "QCL Language Server".to_string(), version: Some("0.1.0".to_string()) }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("QCL Language Server initialized");
        let _ = self
            .client
            .log_message(MessageType::INFO, "QCL Language Server started")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("QCL Language Server shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let document = Document {
            content: Rope::from_str(&params.text_document.text),
            version: params.text_document.version,
            cached_analysis: None,
            cached_semantic_tokens: None,
            _cached_range_tokens: HashMap::new(),
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
            let mut entry = self
                .documents
                .entry(uri.clone())
                .or_default();
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
            entry.debounce_seq = entry.debounce_seq.wrapping_add(1);
        }

        // Periodically clear analyzer caches to prevent memory growth
        if self.documents.len() > 50 {
            if let Ok(mut analyzer) = self.analyzer.lock() {
                analyzer.clear_caches();
            }
        }

        // Debounced diagnostics (no token prewarm to keep edits snappy)
        self.schedule_diagnostics_and_warmup(uri, version, 250)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        Ok(self.get_hover_info(uri, position).await)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut items = self.get_completions();

        // Add context-specific and stdlib-aware completions based on current line
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

                // Provide '@' context completions
                if let Ok(mut analyzer) = self.analyzer.lock() {
                    let context_items = analyzer.get_context_completions("@");
                    items.extend(context_items);
                }

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
                                    let last = raw.split(',').last().unwrap_or("").trim();
                                    let typed = last
                                        .split_whitespace()
                                        .last()
                                        .unwrap_or("");
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
                                let root = if dir_part.is_empty() { base.clone() } else { base.join(dir_part) };
                                if let Ok(entries) = std::fs::read_dir(&root) {
                                    for e in entries.flatten() {
                                        if let Ok(ft) = e.file_type() {
                                            let name = e.file_name().to_string_lossy().to_string();
                                            if name.starts_with(file_prefix) {
                                                let rel = if dir_part.is_empty() { name.clone() } else { format!("{}/{}", dir_part, name) };
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
                    let edit = TextEdit { range: diag.range, new_text };
                    let mut we = WorkspaceEdit::default();
                    we.changes = Some(std::collections::HashMap::from([(uri.clone(), vec![edit]) ]));
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

        if actions.is_empty() { Ok(None) } else { Ok(Some(actions)) }
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let uri = &params.text_document.uri;
        let diagnostics = self.validate_document(uri).await;

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: diagnostics,
                },
            }),
        ))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        if let Some(analysis) = self.get_or_compute_analysis(uri).await {
            if !analysis.symbols.is_empty() {
                return Ok(Some(DocumentSymbolResponse::Nested(
                    analysis.symbols.clone(),
                )));
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
            // Find the definition of this symbol in the document
            if let Some(definition_location) = self.find_definition(&content, &symbol_name, uri).await {
                return Ok(Some(GotoDefinitionResponse::Scalar(definition_location)));
            }
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        // Compute or fetch tokens for current doc state
        let tokens_arc = match self.get_or_generate_semantic_tokens(uri).await {
            Some(t) => t,
            None => return Ok(None),
        };

        // Clamp payload size for responsiveness and store the clamped baseline
        let clamped: Vec<SemanticToken> = (*tokens_arc)
            .iter()
            .take(MAX_SEMANTIC_TOKENS)
            .cloned()
            .collect();
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
        // Snapshot content; slice only the requested range to reduce copying
        let (slice_string, range) = if let Some(doc) = self.documents.get(uri) {
            let start_char = position_to_char_idx(&doc.content, params.range.start);
            let end_char = position_to_char_idx(&doc.content, params.range.end);
            let s = start_char.min(doc.content.len_chars());
            let e = end_char.min(doc.content.len_chars()).max(s);
            let slice_string = doc.content.slice(s..e).to_string();
            (slice_string, params.range)
        } else {
            return Ok(None);
        };

        // Generate range tokens off the async runtime
        let generated = tokio::task::spawn_blocking(move || {
            let analyzer = QclAnalyzer::new();
            analyzer.generate_semantic_tokens_in_range(&slice_string, range)
        })
        .await
        .ok()
        .unwrap_or_default();

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
        let new_tokens: Vec<SemanticToken> = (*new_tokens_full)
            .iter()
            .take(MAX_SEMANTIC_TOKENS)
            .cloned()
            .collect();

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
            if let Some(b) = base_dir { analyzer.set_base_dir(b); }
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
        let generated_result = tokio::task::spawn_blocking(move || {
            let mut analyzer = QclAnalyzer::new();
            if let Some(b) = base_dir { analyzer.set_base_dir(b); }
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

    async fn schedule_diagnostics_and_warmup(
        &self,
        uri: Url,
        scheduled_version: i32,
        delay_ms: u64,
    ) {
        let documents = self.documents.clone();
        tokio::spawn(async move {
            sleep(Duration::from_millis(delay_ms)).await;

            // Check debounce token to ensure no new edits have occurred
            let (content_snapshot, seq_snapshot, version_snapshot) =
                if let Some(doc) = documents.get(&uri) {
                    (doc.content.to_string(), doc.debounce_seq, doc.version)
                } else {
                    return;
                };

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
                if let Some(b) = base_dir { analyzer.set_base_dir(b); }
                analyzer.analyze(&content_for_compute)
            })
            .await
            {
                Ok(pair) => pair,
                Err(_) => return,
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
                if let Some((idx, token)) = find_token_at_offset(&spans, &tokens, absolute_offset) {
                    use qcl_core::token::Token;
                    match token {
                        Token::Id(name) => return Some(name),
                        Token::At => {
                            // Handle context access - extract full path
                            if let Some(path) = extract_context_path(&tokens, idx) {
                                return Some(path);
                            }
                        }
                        _ => {}
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

        // Handle context access patterns (like @req.user.role)
        if symbol_name.starts_with('@') {
            for (line_idx, line) in lines.iter().enumerate() {
                let mut start = 0;
                while let Some(pos) = line[start..].find(symbol_name) {
                    let absolute_pos = start + pos;
                    let range = Range::new(
                        Position::new(line_idx as u32, absolute_pos as u32),
                        Position::new(line_idx as u32, (absolute_pos + symbol_name.len()) as u32),
                    );
                    locations.push(Location::new(uri.clone(), range));
                    start = absolute_pos + 1;
                }
            }
        } else {
            // Handle regular identifiers (variables, functions, labels)
            for (line_idx, line) in lines.iter().enumerate() {
                let mut start = 0;
                while let Some(pos) = line[start..].find(symbol_name) {
                    let absolute_pos = start + pos;
                    let chars: Vec<char> = line.chars().collect();
                    
                    // Check if this is a whole word match (not part of another identifier)
                    let is_word_start = absolute_pos == 0 || 
                        !chars.get(absolute_pos - 1).map(|c| c.is_alphanumeric() || *c == '_').unwrap_or(false);
                    let is_word_end = absolute_pos + symbol_name.len() >= chars.len() ||
                        !chars.get(absolute_pos + symbol_name.len()).map(|c| c.is_alphanumeric() || *c == '_').unwrap_or(false);
                    
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

        // For context access patterns (@req.user.role), there's no single definition - they're contextual
        if symbol_name.starts_with('@') {
            return None;
        }

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
            if (trimmed.starts_with("import ") && trimmed.contains(&format!("import {}", symbol_name))) ||
               (trimmed.starts_with("from ") && trimmed.contains(&format!("from {}", symbol_name))) {
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
}

// Compute common prefix and suffix lengths between two token arrays and the delete count in the old array.
fn common_prefix_suffix_delete_count(
    old: &[SemanticToken],
    new: &[SemanticToken],
) -> (usize, usize, usize) {
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
fn find_token_at_offset(
    spans: &[CoreSpan],
    tokens: &[CoreToken],
    offset: usize,
) -> Option<(usize, CoreToken)> {
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

    // Attempt to extract full context path when hovering on @, identifiers or dot segments within it
    if matches!(tok, T::At | T::Id(_) | T::Int(_) ) {
        if let Some(path) = extract_context_path(tokens, idx) {
            // Root key is the first segment after '@'
            let root = path
                .trim_start_matches('@')
                .split('.')
                .next()
                .unwrap_or("");
            return if root.is_empty() {
                format!("Context path: {}", path)
            } else {
                format!("Context path: {}\nRoot key: {}", path, root)
            };
        }
    }

    match tok {
        T::Id(name) => {
            // Heuristic: function call if next token is '('
            let is_call = tokens.get(idx + 1).map(|t| matches!(t, T::LParen)).unwrap_or(false);
            if is_call {
                format!("Function call: {}(…)", name)
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
        T::Goto => "Keyword: goto".to_string(),
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
        T::At => "Context root: @".to_string(),
        T::LParen => "Symbol: (".to_string(),
        T::RParen => "Symbol: )".to_string(),
        T::LBrace => "Symbol: {".to_string(),
        T::RBrace => "Symbol: }".to_string(),
        T::LBracket => "Symbol: [".to_string(),
        T::RBracket => "Symbol: ]".to_string(),
    }
}

// Given a token index that is part of an @context path, reconstruct the full path string
fn extract_context_path(tokens: &[CoreToken], idx: usize) -> Option<String> {
    use CoreToken as T;
    // Find the nearest '@' to the left of or at idx
    let mut at_pos: Option<usize> = None;
    let mut j = idx as isize;
    while j >= 0 {
        match &tokens[j as usize] {
            T::At => {
                at_pos = Some(j as usize);
                break;
            }
            T::Id(_) | T::Int(_) | T::Dot => {
                j -= 1;
                continue;
            }
            _ => break,
        }
    }
    let start = at_pos?;
    let mut s = String::from("@");
    let mut k = start + 1;
    // Optional first segment right after '@'
    if let Some(seg) = tokens.get(k) {
        match seg {
            T::Id(name) => {
                s.push_str(name);
                k += 1;
            }
            T::Int(n) => {
                s.push_str(&n.to_string());
                k += 1;
            }
            _ => {}
        }
    }
    // Then repeat (. segment)
    loop {
        match (tokens.get(k), tokens.get(k + 1)) {
            (Some(T::Dot), Some(T::Id(name))) => {
                s.push('.');
                s.push_str(name);
                k += 2;
            }
            (Some(T::Dot), Some(T::Int(n))) => {
                s.push('.');
                s.push_str(&n.to_string());
                k += 2;
            }
            _ => break,
        }
    }
    Some(s)
}

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
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .init();

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
            let errors: Vec<String> = analysis.diagnostics
                .iter()
                .filter(|d| d.severity == Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR))
                .map(|d| {
                    format!("Line {}:{}: {}", 
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
            let mut context_refs: Vec<String> = analysis.context_references.iter().cloned().collect();
            context_refs.sort();

            // Map tokens to simple arrays to avoid requiring serde on LSP types
            let tokens_simple: Vec<[u32; 5]> = tokens
                .iter()
                .map(|t| [
                    t.delta_line,
                    t.delta_start,
                    t.length,
                    t.token_type,
                    t.token_modifiers_bitset,
                ])
                .collect();

            let output = serde_json::json!({
                "diagnostics": analysis.diagnostics,
                "symbols": analysis.symbols,
                "context_references": context_refs,
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
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))
}
