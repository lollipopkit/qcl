use dashmap::DashMap;
use ropey::Rope;
use std::sync::Arc;
use tokio::time::{sleep, Duration};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;

mod analyzer;
use analyzer::{AnalysisResult, QclAnalyzer};

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
    // Debounce token incremented on each edit; used to coalesce diagnostics work
    debounce_seq: u64,
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
        // Use cached or computed analysis; avoid recompute on hot path
        let analysis = self.get_or_compute_analysis(uri).await?;

        if !analysis.context_references.is_empty() {
            let hover_text = format!(
                "QCL Code\n\nContext references: {:?}\n\nSymbols: {}",
                analysis.context_references,
                analysis.symbols.len()
            );
            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: None,
            });
        }

        if !analysis.symbols.is_empty() {
            let hover_text = format!("QCL Code\n\nSymbols: {}", analysis.symbols.len());
            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: None,
            });
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
                            range: Some(true),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
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
            debounce_seq: 0,
        };

        self.documents.insert(uri.clone(), document);

        // Compute diagnostics once and populate cache
        let diagnostics = self.validate_document(&uri).await;
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Apply all changes (supports both full and incremental)
        {
            let mut entry = self
                .documents
                .entry(uri.clone())
                .or_insert_with(Document::default);
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

        // Debounced diagnostics + cache warmup
        self.schedule_diagnostics_and_warmup(uri, version, 150)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        Ok(self.get_hover_info(uri, position).await)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut items = self.get_completions();

        // Add context-specific completions if triggered by '@'
        let uri = &params.text_document_position.text_document.uri;
        if self.documents.get(uri).is_some() {
            // Get current line context for better completions
            if let Ok(mut analyzer) = self.analyzer.lock() {
                let context_items = analyzer.get_context_completions("@");
                items.extend(context_items);
            }
        }

        Ok(Some(CompletionResponse::Array(items)))
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        if let Some(tokens) = self.get_or_generate_semantic_tokens(uri).await {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: (*tokens).clone(),
            })));
        }
        Ok(None)
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
        let computed_result = tokio::task::spawn_blocking(move || {
            let mut analyzer = QclAnalyzer::new();
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
        let generated_result = tokio::task::spawn_blocking(move || {
            let analyzer = QclAnalyzer::new();
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
        let client = self.client.clone();
        tokio::spawn(async move {
            sleep(Duration::from_millis(delay_ms)).await;

            // Check debounce token to ensure no new edits have occurred
            let (content_snapshot, seq_snapshot, version_snapshot) =
                if let Some(doc) = documents.get(&uri) {
                    (doc.content.to_string(), doc.debounce_seq, doc.version)
                } else {
                    return;
                };

            // Compute analysis and tokens on snapshot off the runtime thread
            let content_for_compute = content_snapshot.clone();
            let (analysis, tokens) = match tokio::task::spawn_blocking(move || {
                let mut analyzer = QclAnalyzer::new();
                let analysis = analyzer.analyze(&content_for_compute);
                let tokens = analyzer.generate_semantic_tokens(&content_for_compute);
                (analysis, tokens)
            })
            .await
            {
                Ok(pair) => pair,
                Err(_) => return,
            };

            // Publish diagnostics if still current
            let diagnostics_to_publish = analysis.diagnostics.clone();

            // Try to store caches if document still matches snapshot
            if let Some(mut doc) = documents.get_mut(&uri) {
                if doc.debounce_seq == seq_snapshot
                    && doc.version == scheduled_version
                    && doc.version == version_snapshot
                {
                    doc.cached_analysis = Some(Arc::new(analysis));
                    doc.cached_semantic_tokens = Some(Arc::new(tokens));
                }
            }

            // Always publish diagnostics for the uri (latest client will override older results)
            let _ = client
                .publish_diagnostics(uri.clone(), diagnostics_to_publish, None)
                .await;
        });
    }
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

#[tokio::main]
async fn main() {
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
