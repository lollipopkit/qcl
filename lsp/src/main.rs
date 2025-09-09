use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;

mod analyzer;
use analyzer::QclAnalyzer;

#[derive(Debug)]
struct Document {
    content: String,
    #[allow(dead_code)] // Keep for future version tracking
    version: i32,
}

struct QclLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, Document>>>,
    analyzer: QclAnalyzer,
}

impl QclLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            analyzer: QclAnalyzer::new(),
        }
    }

    async fn validate_document(&self, uri: &Url) -> Vec<Diagnostic> {
        let documents = self.documents.read().await;
        let Some(document) = documents.get(uri) else {
            return Vec::new();
        };

        let analysis = self.analyzer.analyze(&document.content);
        analysis.diagnostics
    }

    async fn get_hover_info(&self, uri: &Url, _position: Position) -> Option<Hover> {
        let documents = self.documents.read().await;
        let document = documents.get(uri)?;
        let content = &document.content;

        let analysis = self.analyzer.analyze(content);
        
        if !analysis.context_references.is_empty() {
            let hover_text = format!(
                "QCL Code\n\nContext references: {:?}\n\nSymbols: {}",
                analysis.context_references,
                analysis.symbols.len()
            );
            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: Some(Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))),
            });
        }

        if !analysis.symbols.is_empty() {
            let hover_text = format!("QCL Code\n\nSymbols: {}", analysis.symbols.len());
            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: Some(Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))),
            });
        }

        None
    }

    fn get_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // QCL keywords
        let keywords = [
            "if", "else", "while", "let", "fn", "return", "break", "continue",
            "goto", "import", "from", "as", "go", "select", "case", "default",
            "true", "false", "nil"
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
            documentation: Some(Documentation::String("Access context variables (e.g., @req.user.role)".to_string())),
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
        info!("QCL Language Server initializing with params: {:?}", params.root_uri);
        
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
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
                    }
                )),
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
                        range: Some(false),
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                    }
                )),
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
        let _ = self.client
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
            content: params.text_document.text,
            version: params.text_document.version,
        };

        self.documents.write().await.insert(uri.clone(), document);
        
        let diagnostics = self.validate_document(&uri).await;
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        if let Some(change) = params.content_changes.into_iter().next() {
            let document = Document {
                content: change.text,
                version: params.text_document.version,
            };

            self.documents.write().await.insert(uri.clone(), document);
            
            let diagnostics = self.validate_document(&uri).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
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
        let documents = self.documents.read().await;
        if let Some(_document) = documents.get(uri) {
            // Get current line context for better completions
            let context_items = self.analyzer.get_context_completions("@");
            items.extend(context_items);
        }
        
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn diagnostic(&self, params: DocumentDiagnosticParams) -> Result<DocumentDiagnosticReportResult> {
        let uri = &params.text_document.uri;
        let diagnostics = self.validate_document(uri).await;
        
        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(
                RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diagnostics,
                    },
                }
            )
        ))
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let documents = self.documents.read().await;
        
        if let Some(document) = documents.get(uri) {
            let analysis = self.analyzer.analyze(&document.content);
            if !analysis.symbols.is_empty() {
                return Ok(Some(DocumentSymbolResponse::Nested(analysis.symbols)));
            }
        }
        
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let documents = self.documents.read().await;
        
        if let Some(document) = documents.get(uri) {
            let tokens = self.analyzer.generate_semantic_tokens(&document.content);
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        
        Ok(None)
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