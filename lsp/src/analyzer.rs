use qcl_core::{
    ast::Parser as ExprParser, expr::Expr, import::ImportStmt, stmt::Stmt, stmt_parser::StmtParser,
    token::Tokenizer, val::Val,
};
use std::collections::HashSet;
use tower_lsp::lsp_types::*;

/// Result of analyzing QCL code, containing diagnostics, symbols, and context references
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    pub diagnostics: Vec<Diagnostic>,
    pub symbols: Vec<DocumentSymbol>,
    pub context_references: HashSet<String>,
}

/// QCL Language analyzer for providing LSP functionality
pub struct QclAnalyzer;

impl QclAnalyzer {
    /// Create a new QCL analyzer
    pub fn new() -> Self {
        Self
    }

    /// Analyze QCL code and return diagnostics, symbols, and context references
    pub fn analyze(&self, content: &str) -> AnalysisResult {
        let mut result = AnalysisResult {
            diagnostics: Vec::new(),
            symbols: Vec::new(),
            context_references: HashSet::new(),
        };

        // Try parsing as expression first
        let tokens = match Tokenizer::tokenize(content) {
            Ok(tokens) => tokens,
            Err(tokenize_err) => {
                result.diagnostics.push(Diagnostic::new(
                    Range::new(Position::new(0, 0), Position::new(0, content.len() as u32)),
                    Some(DiagnosticSeverity::ERROR),
                    None,
                    Some("qcl".to_string()),
                    format!("Tokenization error: {}", tokenize_err),
                    None,
                    None,
                ));
                return result;
            }
        };

        let mut expr_parser = ExprParser::new(&tokens);
        match expr_parser.parse() {
            Ok(expr) => {
                // Collect context references
                result.context_references = expr.requested_ctx();

                // Add expression symbol
                let symbol = DocumentSymbol {
                    name: "expression".to_string(),
                    detail: Some("QCL Expression".to_string()),
                    kind: SymbolKind::CONSTANT,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range: Range::new(Position::new(0, 0), Position::new(0, content.len() as u32)),
                    selection_range: Range::new(
                        Position::new(0, 0),
                        Position::new(0, content.len() as u32),
                    ),
                    children: None,
                };
                result.symbols.push(symbol);
            }
            Err(expr_err) => {
                // Try parsing as statement program
                let mut stmt_parser = StmtParser::new(&tokens);
                match stmt_parser.parse_program() {
                    Ok(program) => {
                        // Analyze statements for symbols and context references
                        self.analyze_statements(&program.statements, &mut result);
                    }
                    Err(stmt_err) => {
                        // Both parsing attempts failed
                        result.diagnostics.push(Diagnostic::new(
                            Range::new(Position::new(0, 0), Position::new(0, content.len() as u32)),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            Some("qcl".to_string()),
                            format!(
                                "Parse error - Expression: {}, Statement: {}",
                                expr_err, stmt_err
                            ),
                            None,
                            None,
                        ));
                    }
                }
            }
        }

        result
    }

    fn analyze_statements(&self, statements: &[Box<Stmt>], result: &mut AnalysisResult) {
        for (i, stmt) in statements.iter().enumerate() {
            match stmt.as_ref() {
                Stmt::Let { name, .. } => {
                    result.symbols.push(DocumentSymbol {
                        name: name.clone(),
                        detail: Some("Variable declaration".to_string()),
                        kind: SymbolKind::VARIABLE,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                        selection_range: Range::new(
                            Position::new(i as u32, 0),
                            Position::new(i as u32, 100),
                        ),
                        children: None,
                    });
                }
                Stmt::Function { name, params, .. } => {
                    result.symbols.push(DocumentSymbol {
                        name: name.clone(),
                        detail: Some(format!("Function({})", params.join(", "))),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                        selection_range: Range::new(
                            Position::new(i as u32, 0),
                            Position::new(i as u32, 100),
                        ),
                        children: None,
                    });
                }
                Stmt::Import(import_stmt) => {
                    let import_name = match import_stmt {
                        ImportStmt::Module { module } => module.clone(),
                        ImportStmt::File { path } => path.clone(),
                        ImportStmt::Items { source, .. } => match source {
                            qcl_core::import::ImportSource::Module(name) => name.clone(),
                            qcl_core::import::ImportSource::File(path) => path.clone(),
                        },
                        ImportStmt::Namespace { source, .. } => match source {
                            qcl_core::import::ImportSource::Module(name) => name.clone(),
                            qcl_core::import::ImportSource::File(path) => path.clone(),
                        },
                        ImportStmt::ModuleAlias { module, .. } => module.clone(),
                    };
                    result.symbols.push(DocumentSymbol {
                        name: format!("import {}", import_name),
                        detail: Some("Import statement".to_string()),
                        kind: SymbolKind::MODULE,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                        selection_range: Range::new(
                            Position::new(i as u32, 0),
                            Position::new(i as u32, 100),
                        ),
                        children: None,
                    });
                }
                Stmt::Label { name } => {
                    result.symbols.push(DocumentSymbol {
                        name: format!("{}:", name),
                        detail: Some("Label".to_string()),
                        kind: SymbolKind::KEY,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                        selection_range: Range::new(
                            Position::new(i as u32, 0),
                            Position::new(i as u32, 100),
                        ),
                        children: None,
                    });
                }
                _ => {}
            }
        }
    }

    /// Get context-aware completions for the given prefix
    pub fn get_context_completions(&self, prefix: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Common context patterns
        let common_contexts = [
            ("@req", "Request object"),
            ("@req.user", "User information"),
            ("@req.user.id", "User ID"),
            ("@req.user.role", "User role"),
            ("@req.user.name", "User name"),
            ("@record", "Record object"),
            ("@record.id", "Record ID"),
            ("@record.owner", "Record owner"),
            ("@record.granted", "Granted users list"),
            ("@env", "Environment variables"),
            ("@time", "Current timestamp"),
        ];

        for (context, desc) in common_contexts {
            if context.starts_with(prefix) {
                items.push(CompletionItem {
                    label: context.to_string(),
                    kind: Some(CompletionItemKind::PROPERTY),
                    detail: Some(desc.to_string()),
                    ..Default::default()
                });
            }
        }

        items
    }

    /// Validate context access in an expression against provided context
    #[allow(dead_code)] // Reserved for future use
    pub fn validate_context_access(
        &self,
        expr_result: &Result<Expr, String>,
        context: Option<&Val>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        if let Ok(expr) = expr_result {
            let required_ctx = expr.requested_ctx();

            if let Some(ctx) = context {
                // Check if required context keys are available
                for ctx_key in &required_ctx {
                    if !self.context_has_key(ctx, ctx_key) {
                        diagnostics.push(Diagnostic::new(
                            Range::new(Position::new(0, 0), Position::new(0, 100)),
                            Some(DiagnosticSeverity::WARNING),
                            None,
                            Some("qcl".to_string()),
                            format!("Context key '{}' not found in provided context", ctx_key),
                            None,
                            None,
                        ));
                    }
                }
            } else if !required_ctx.is_empty() {
                diagnostics.push(Diagnostic::new(
                    Range::new(Position::new(0, 0), Position::new(0, 100)),
                    Some(DiagnosticSeverity::INFORMATION),
                    None,
                    Some("qcl".to_string()),
                    format!("Expression requires context: {:?}", required_ctx),
                    None,
                    None,
                ));
            }
        }

        diagnostics
    }

    #[allow(dead_code)] // Helper for validate_context_access
    fn context_has_key(&self, context: &Val, key: &str) -> bool {
        // Simple key existence check - traverse dot notation
        let parts: Vec<&str> = key.split('.').collect();
        let mut current = context;

        for part in parts {
            match current {
                Val::Map(map) => {
                    if let Some(value) = map.get(part) {
                        current = value;
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
        }
        true
    }
}
