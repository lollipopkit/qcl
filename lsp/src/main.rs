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
use qcl_core::{error::Span as CoreSpan, token::Token as CoreToken};

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
                            // Disable range-based semantic tokens to reduce UI churn during scroll
                            range: Some(false),
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

        // Periodically clear analyzer caches to prevent memory growth
        if self.documents.len() > 50 {
            if let Ok(mut analyzer) = self.analyzer.lock() {
                analyzer.clear_caches();
            }
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
fn describe_token_hover(tokens: &[CoreToken], spans: &[CoreSpan], idx: usize) -> String {
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
        T::Go => "Keyword: go".to_string(),
        T::Chan => "Keyword: chan".to_string(),
        T::Select => "Keyword: select".to_string(),
        T::Case => "Keyword: case".to_string(),
        T::Default => "Keyword: default".to_string(),
        T::MakeChan => "Function: make_chan".to_string(),

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
        T::Send => "Channel op: <- (send)".to_string(),
        T::Recv => "Channel op: <- (recv)".to_string(),
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
        let path = args.get(i + 1).cloned().ok_or_else(|| {
            anyhow::anyhow!("Usage: qcl-lsp --analyze <relative-file-path>")
        })?;

        // Read file content with safety checks
        let content = read_file_content(&path)?;

        // Run analysis using the same analyzer used by the LSP
        let mut analyzer = QclAnalyzer::new();
        let analysis = analyzer.analyze(&content);
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
    Ok(std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))?)
}
