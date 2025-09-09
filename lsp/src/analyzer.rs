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
                // Try to get position info if it's a ParseError
                let range = if let Some(parse_err) = tokenize_err.downcast_ref::<qcl_core::error::ParseError>() {
                    if let Some(span) = &parse_err.span {
                        let start_pos = Position::new(span.start.line - 1, span.start.column - 1);
                        let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                        Range::new(start_pos, end_pos)
                    } else {
                        Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
                    }
                } else {
                    Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
                };
                
                result.diagnostics.push(Diagnostic::new(
                    range,
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
        match expr_parser.parse_with_enhanced_errors(content) {
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

                // Add context validation diagnostics if we can parse the expression again for validation
                let expr_result = Ok(expr);
                let context_diagnostics = self.validate_context_access(&expr_result, None);
                result.diagnostics.extend(context_diagnostics);
            }
            Err(expr_err) => {
                // Try parsing as statement program
                let mut stmt_parser = StmtParser::new(&tokens);
                match stmt_parser.parse_program_with_enhanced_errors(content) {
                    Ok(program) => {
                        // Analyze statements for symbols and context references
                        self.analyze_statements(&program.statements, &mut result);
                    }
                    Err(stmt_err) => {
                        // Both parsing attempts failed - prefer statement error for code containing statement keywords
                        let has_statement_keywords = content.contains("let ") || content.contains("if ") || 
                                                   content.contains("while ") || content.contains("return ") ||
                                                   content.contains("goto ") || content.contains("break") ||
                                                   content.contains("continue");
                        let parse_err = if has_statement_keywords { &stmt_err } else { &expr_err };
                        
                        let range = if let Some(span) = &parse_err.span {
                            let start_pos = Position::new(span.start.line - 1, span.start.column - 1);
                            let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                            Range::new(start_pos, end_pos)
                        } else {
                            Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
                        };
                        
                        result.diagnostics.push(Diagnostic::new(
                            range,
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            Some("qcl".to_string()),
                            parse_err.message.clone(),
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
    pub fn validate_context_access(
        &self,
        expr_result: &Result<Expr, anyhow::Error>,
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

#[cfg(test)]
mod tests {
    use super::*;
    use qcl_core::val::Val;
    use std::collections::HashMap;

    fn create_analyzer() -> QclAnalyzer {
        QclAnalyzer::new()
    }

    #[test]
    fn test_analyze_simple_expression() {
        let analyzer = create_analyzer();
        let result = analyzer.analyze("@req.user.role == 'admin'");

        // Should have context references
        assert!(result.context_references.contains("req"));

        // Should have expression symbol
        assert_eq!(result.symbols.len(), 1);
        assert_eq!(result.symbols[0].name, "expression");
        assert_eq!(result.symbols[0].kind, SymbolKind::CONSTANT);

        // Check that diagnostics include context requirement info (this is now expected behavior)
        let has_context_info = result.diagnostics.iter().any(|d| 
            d.severity == Some(DiagnosticSeverity::INFORMATION) && 
            d.message.contains("requires context"));
        assert!(has_context_info, "Expected context requirement diagnostic");
    }

    #[test]
    fn test_analyze_invalid_expression() {
        let analyzer = create_analyzer();
        let result = analyzer.analyze("@req.user.role == 'unterminated string");

        // Should have diagnostic for invalid expression (tokenization error due to unterminated string)
        assert!(!result.diagnostics.is_empty());
        assert_eq!(result.diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(result.diagnostics[0].message.contains("Tokenization error"));
    }

    #[test]
    fn test_analyze_statement_program() {
        let analyzer = create_analyzer();
        let code = r#"
            import math;
            let user_level = @req.user.level;
            fn calculate_score(base) {
                return math.sqrt(base * user_level);
            }
            start:
            let result = calculate_score(100);
        "#;
        let result = analyzer.analyze(code);

        // Should not have diagnostics for valid program
        assert!(result.diagnostics.is_empty());

        // Should have symbols for import, variable, function, and label
        assert!(result.symbols.len() >= 4);

        let symbol_names: Vec<&String> = result.symbols.iter().map(|s| &s.name).collect();
        assert!(symbol_names.contains(&&"import math".to_string()));
        assert!(symbol_names.contains(&&"user_level".to_string()));
        assert!(symbol_names.contains(&&"calculate_score".to_string()));
        assert!(symbol_names.contains(&&"start:".to_string()));
        assert!(symbol_names.contains(&&"result".to_string()));
    }

    #[test]
    fn test_get_context_completions() {
        let analyzer = create_analyzer();
        let completions = analyzer.get_context_completions("@req");

        // Should return completions that start with "@req"
        assert!(!completions.is_empty());
        
        let labels: Vec<&String> = completions.iter().map(|c| &c.label).collect();
        assert!(labels.contains(&&"@req".to_string()));
        assert!(labels.contains(&&"@req.user".to_string()));
        assert!(labels.contains(&&"@req.user.id".to_string()));
        assert!(labels.contains(&&"@req.user.role".to_string()));
        assert!(labels.contains(&&"@req.user.name".to_string()));
        
        // Should not include completions that don't match the prefix
        assert!(!labels.contains(&&"@record".to_string()));
    }

    #[test]
    fn test_validate_context_access_with_valid_context() {
        let analyzer = create_analyzer();
        
        // Create a context with req.user.role
        let mut user_map = HashMap::new();
        user_map.insert("role".to_string(), Val::Str("admin".to_string().into()));
        user_map.insert("id".to_string(), Val::Int(123));
        
        let mut req_map = HashMap::new();
        req_map.insert("user".to_string(), Val::Map(user_map.into()));
        
        let mut context_map = HashMap::new();
        context_map.insert("req".to_string(), Val::Map(req_map.into()));
        let context = Val::Map(context_map.into());

        // Parse expression that uses req.user.role  
        let tokens = qcl_core::token::Tokenizer::tokenize("@req.user.role == 'admin'").unwrap();
        let mut parser = qcl_core::ast::Parser::new(&tokens);
        let expr_result = parser.parse();

        let diagnostics = analyzer.validate_context_access(&expr_result, Some(&context));

        // Should have no diagnostics since context is valid
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_context_has_key() {
        let analyzer = create_analyzer();
        
        // Create nested context structure
        let mut inner_map = HashMap::new();
        inner_map.insert("name".to_string(), Val::Str("test".to_string().into()));
        
        let mut middle_map = HashMap::new();
        middle_map.insert("user".to_string(), Val::Map(inner_map.into()));
        
        let mut context_map = HashMap::new();
        context_map.insert("req".to_string(), Val::Map(middle_map.into()));
        let context = Val::Map(context_map.into());

        // Test existing nested key
        assert!(analyzer.context_has_key(&context, "req.user.name"));
        
        // Test non-existing key
        assert!(!analyzer.context_has_key(&context, "req.user.role"));
        assert!(!analyzer.context_has_key(&context, "req.admin"));
        assert!(!analyzer.context_has_key(&context, "nonexistent"));
    }
}
