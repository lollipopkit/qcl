use qcl_core::{
    ast::Parser as ExprParser, expr::Expr, import::ImportStmt, stmt::Stmt, stmt_parser::StmtParser,
    token::Tokenizer, val::Val,
};
use std::collections::HashSet;
use tower_lsp::lsp_types::*;

// Soft limits to keep LSP responsive on large/broken files
const MAX_SCAN_LINES: usize = 400; // max lines to line-scan
const MAX_SCAN_CHUNKS: usize = 300; // max logical chunks to scan
const MAX_DIAGNOSTICS: usize = 200; // cap diagnostics volume

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
        let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(pair) => pair,
            Err(parse_err) => {
                // If multi-line, try line-wise scanning to surface multiple errors
                if content.lines().count() > 1 {
                    let diags = self.scan_lines_for_diagnostics(content);
                    if !diags.is_empty() {
                        result.diagnostics = diags;
                        return result;
                    }
                }

                // Fallback: report the single tokenization error for the whole document
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
                    format!("Tokenization error: {}", parse_err.message),
                    None,
                    None,
                ));
                return result;
            }
        };

        let mut expr_parser = ExprParser::new_with_spans(&tokens, &spans);
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
                // Attempt expression-level recovery to surface multiple errors for pure expressions
                let expr_recover_errors =
                    ExprParser::recover_expression_errors(&tokens, &spans, content);
                // Try parsing as statement program
                let mut stmt_parser = StmtParser::new_with_spans(&tokens, &spans);
                match stmt_parser.parse_program_with_enhanced_errors(content) {
                    Ok(program) => {
                        // Analyze statements for symbols and context references
                        self.analyze_statements(&program.statements, &mut result);
                    }
                    Err(stmt_err) => {
                        // If we found expression-level errors and the content doesn't look like statements,
                        // prefer reporting these expression diagnostics.
                        let mut collected: Vec<Diagnostic> = Vec::new();
                        let has_statement_keywords = content.contains("let ")
                            || content.contains("if ")
                            || content.contains("while ")
                            || content.contains("return ")
                            || content.contains("goto ")
                            || content.contains("break")
                            || content.contains("continue");
                        if !expr_recover_errors.is_empty() && !has_statement_keywords {
                            for e in expr_recover_errors {
                                let range = if let Some(span) = &e.span {
                                    let start_pos =
                                        Position::new(span.start.line - 1, span.start.column - 1);
                                    let end_pos =
                                        Position::new(span.end.line - 1, span.end.column - 1);
                                    Range::new(start_pos, end_pos)
                                } else {
                                    Range::new(
                                        Position::new(0, 0),
                                        Position::new(0, content.len() as u32),
                                    )
                                };
                                collected.push(Diagnostic::new(
                                    range,
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    e.message,
                                    None,
                                    None,
                                ));
                            }
                        }

                        // First, attempt recovering parse to collect multiple errors with precise spans
                        let mut recover_parser = StmtParser::new_with_spans(&tokens, &spans);
                        let (stmts, errs) =
                            recover_parser.parse_program_recovering_with_enhanced_errors(content);
                        if !errs.is_empty() {
                            for e in errs {
                                let range = if let Some(span) = &e.span {
                                    let start_pos =
                                        Position::new(span.start.line - 1, span.start.column - 1);
                                    let end_pos =
                                        Position::new(span.end.line - 1, span.end.column - 1);
                                    Range::new(start_pos, end_pos)
                                } else {
                                    Range::new(
                                        Position::new(0, 0),
                                        Position::new(0, content.len() as u32),
                                    )
                                };
                                collected.push(Diagnostic::new(
                                    range,
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    e.message,
                                    None,
                                    None,
                                ));
                            }
                            // Even with errors, analyze statements to surface symbols and context refs
                            self.analyze_statements(&stmts, &mut result);
                        }

                        // If recovery yielded nothing (e.g., single token), try chunk-based scan then line-wise
                        if collected.is_empty() {
                            collected = self.scan_chunks_for_diagnostics(content);
                            if collected.is_empty() {
                                collected = self.scan_lines_for_diagnostics(content);
                            }
                        }

                        // If line scanning found nothing (e.g., single-line expression-like input),
                        // fall back to reporting the most relevant single error.
                        if collected.is_empty() {
                            // Both parsing attempts failed - prefer statement error for code containing statement keywords
                            let has_statement_keywords = content.contains("let ")
                                || content.contains("if ")
                                || content.contains("while ")
                                || content.contains("return ")
                                || content.contains("goto ")
                                || content.contains("break")
                                || content.contains("continue");
                            let parse_err = if has_statement_keywords {
                                &stmt_err
                            } else {
                                &expr_err
                            };

                            let range = if let Some(span) = &parse_err.span {
                                let start_pos =
                                    Position::new(span.start.line - 1, span.start.column - 1);
                                let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                                Range::new(start_pos, end_pos)
                            } else {
                                Range::new(
                                    Position::new(0, 0),
                                    Position::new(0, content.len() as u32),
                                )
                            };

                            collected.push(Diagnostic::new(
                                range,
                                Some(DiagnosticSeverity::ERROR),
                                None,
                                Some("qcl".to_string()),
                                parse_err.message.clone(),
                                None,
                                None,
                            ));
                        }

                        result.diagnostics.extend(collected.into_iter());
                    }
                }
            }
        }

        // Deduplicate diagnostics by range and message to reduce noise
        self.dedup_diagnostics(&mut result.diagnostics);

        result
    }

    /// Segment the document into logical chunks using a lightweight state machine:
    /// - Split at semicolons when not inside strings/comments and with paren/bracket depth 0
    /// - Split at closing '}' to capture full blocks (e.g., if/while/fn bodies)
    /// - Preserve multi-line strings and block comments
    fn segment_document(&self, content: &str) -> Vec<(usize, usize, usize)> {
        // Returns a list of (start_byte, end_byte, start_line_idx0)
        let mut chunks = Vec::new();
        if content.trim().is_empty() {
            return chunks;
        }

        let mut start_byte = 0usize;
        let mut start_line = 0usize; // 0-based

        let mut line = 0usize;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut in_block_comment = false;
        let mut in_line_comment = false;
        let mut in_string: Option<char> = None;
        let mut prev_was_backslash = false;

        let bytes = content.as_bytes();
        let mut i = 0usize;
        while i < bytes.len() {
            let b = bytes[i];
            let ch = b as char;

            // Track line numbers
            if ch == '\n' {
                line += 1;
                in_line_comment = false; // end of line comment
                prev_was_backslash = false;
                i += 1;
                continue;
            }

            if in_line_comment {
                i += 1;
                continue;
            }

            if in_block_comment {
                // Look for end of block comment '*/'
                if ch == '*' && i + 1 < bytes.len() && bytes[i + 1] as char == '/' {
                    in_block_comment = false;
                    i += 2;
                } else {
                    i += 1;
                }
                continue;
            }

            if let Some(q) = in_string {
                // Inside string; handle escapes
                if ch == q && !prev_was_backslash {
                    in_string = None;
                    prev_was_backslash = false;
                    i += 1;
                } else {
                    prev_was_backslash = ch == '\\' && !prev_was_backslash;
                    if !prev_was_backslash {
                        prev_was_backslash = false;
                    }
                    i += 1;
                }
                continue;
            }

            // Not inside string/comment
            // Handle comment starts
            if ch == '/' && i + 1 < bytes.len() {
                let n = bytes[i + 1] as char;
                if n == '/' {
                    in_line_comment = true;
                    i += 2;
                    continue;
                }
                if n == '*' {
                    in_block_comment = true;
                    i += 2;
                    continue;
                }
            }

            // Handle string start
            if ch == '"' || ch == '\'' {
                in_string = Some(ch);
                prev_was_backslash = false;
                i += 1;
                continue;
            }

            // Track nesting
            match ch {
                '(' => paren += 1,
                ')' => paren -= 1,
                '[' => bracket += 1,
                ']' => bracket -= 1,
                '{' => brace += 1,
                '}' => {
                    brace -= 1;
                    // A closing brace at depth 0 is a good chunk boundary
                    if paren == 0 && bracket == 0 && brace == 0 {
                        let end_byte = i + 1; // include '}'
                                              // Avoid empty whitespace-only chunks
                        if content[start_byte..end_byte].trim().len() > 0 {
                            chunks.push((start_byte, end_byte, start_line));
                            if chunks.len() >= MAX_SCAN_CHUNKS {
                                return chunks;
                            }
                        }
                        start_byte = end_byte;
                        start_line = line;
                    }
                }
                ';' => {
                    // Statement terminator outside paren/bracket nesting
                    if paren == 0 && bracket == 0 {
                        let end_byte = i + 1; // include ';'
                        if content[start_byte..end_byte].trim().len() > 0 {
                            chunks.push((start_byte, end_byte, start_line));
                            if chunks.len() >= MAX_SCAN_CHUNKS {
                                return chunks;
                            }
                        }
                        start_byte = end_byte;
                        start_line = line;
                    }
                }
                _ => {}
            }

            i += 1;
        }

        // Trailing chunk
        if start_byte < bytes.len() {
            let tail = &content[start_byte..];
            if tail.trim().len() > 0 {
                chunks.push((start_byte, bytes.len(), start_line));
            }
        }

        chunks
    }

    /// Chunk-based diagnostics scan. Attempts to parse multi-line logical chunks
    /// to surface multiple independent errors with better positions.
    fn scan_chunks_for_diagnostics(&self, content: &str) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        let chunks = self.segment_document(content);
        if chunks.is_empty() {
            return diags;
        }

        for (start_b, end_b, start_line) in chunks.into_iter().take(MAX_SCAN_CHUNKS) {
            let chunk = &content[start_b..end_b];
            if chunk.trim().is_empty() {
                continue;
            }

            // Prefer tokenization errors which carry precise spans
            match Tokenizer::tokenize_enhanced_with_spans(chunk) {
                Err(parse_err) => {
                    let range = if let Some(span) = &parse_err.span {
                        let start_pos = Position::new(
                            (start_line as u32) + (span.start.line - 1),
                            span.start.column.saturating_sub(1),
                        );
                        let end_pos = Position::new(
                            (start_line as u32) + (span.end.line - 1),
                            span.end.column.saturating_sub(1),
                        );
                        Range::new(start_pos, end_pos)
                    } else {
                        Range::new(
                            Position::new(start_line as u32, 0),
                            Position::new(start_line as u32, chunk.chars().count() as u32),
                        )
                    };

                    if diags.len() >= MAX_DIAGNOSTICS {
                        break;
                    }
                    diags.push(Diagnostic::new(
                        range,
                        Some(DiagnosticSeverity::ERROR),
                        None,
                        Some("qcl".to_string()),
                        format!("Tokenization error: {}", parse_err.message),
                        None,
                        None,
                    ));
                    continue;
                }
                Ok((chunk_tokens, chunk_spans)) => {
                    // Try parsing as statement program first to catch control structures
                    let mut sp = StmtParser::new_with_spans(&chunk_tokens, &chunk_spans);
                    match sp.parse_program_with_enhanced_errors(chunk) {
                        Ok(_) => {
                            // No statement-level error in this chunk; continue
                        }
                        Err(stmt_err) => {
                            let range = if let Some(span) = &stmt_err.span {
                                let start_pos = Position::new(
                                    (start_line as u32) + (span.start.line - 1),
                                    span.start.column.saturating_sub(1),
                                );
                                let end_pos = Position::new(
                                    (start_line as u32) + (span.end.line - 1),
                                    span.end.column.saturating_sub(1),
                                );
                                Range::new(start_pos, end_pos)
                            } else {
                                Range::new(
                                    Position::new(start_line as u32, 0),
                                    Position::new(start_line as u32, chunk.chars().count() as u32),
                                )
                            };

                            if diags.len() < MAX_DIAGNOSTICS {
                                diags.push(Diagnostic::new(
                                    range.clone(),
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    stmt_err.message.clone(),
                                    None,
                                    None,
                                ));
                            }

                            // Also try expression recovery for potentially multiple, more specific spans
                            let expr_errs = ExprParser::recover_expression_errors(
                                &chunk_tokens,
                                &chunk_spans,
                                chunk,
                            );
                            for ee in expr_errs {
                                if diags.len() >= MAX_DIAGNOSTICS {
                                    break;
                                }
                                let range2 = if let Some(span) = &ee.span {
                                    let start_pos = Position::new(
                                        (start_line as u32) + (span.start.line - 1),
                                        span.start.column.saturating_sub(1),
                                    );
                                    let end_pos = Position::new(
                                        (start_line as u32) + (span.end.line - 1),
                                        span.end.column.saturating_sub(1),
                                    );
                                    Range::new(start_pos, end_pos)
                                } else {
                                    range
                                };
                                diags.push(Diagnostic::new(
                                    range2,
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    ee.message.clone(),
                                    None,
                                    None,
                                ));
                            }
                        }
                    }
                }
            }
            if diags.len() >= MAX_DIAGNOSTICS {
                break;
            }
        }

        diags
    }

    /// Best-effort line-wise scan to accumulate multiple diagnostics.
    /// This helps surface multiple independent errors in a single document
    /// instead of stopping at the first parse failure.
    fn scan_lines_for_diagnostics(&self, content: &str) -> Vec<Diagnostic> {
        let mut diags = Vec::new();

        for (line_idx, line) in content.lines().enumerate().take(MAX_SCAN_LINES) {
            // Skip empty or whitespace-only lines to reduce noise
            if line.trim().is_empty() {
                continue;
            }

            // Try tokenizing the single line first to get precise position if it fails
            match Tokenizer::tokenize_enhanced_with_spans(line) {
                Err(parse_err) => {
                    if diags.len() >= MAX_DIAGNOSTICS {
                        break;
                    }
                    let range = if let Some(span) = &parse_err.span {
                        let start_pos =
                            Position::new(line_idx as u32, span.start.column.saturating_sub(1));
                        let end_pos =
                            Position::new(line_idx as u32, span.end.column.saturating_sub(1));
                        Range::new(start_pos, end_pos)
                    } else {
                        // Fallback: highlight whole line
                        Range::new(
                            Position::new(line_idx as u32, 0),
                            Position::new(line_idx as u32, line.chars().count() as u32),
                        )
                    };

                    diags.push(Diagnostic::new(
                        range,
                        Some(DiagnosticSeverity::ERROR),
                        None,
                        Some("qcl".to_string()),
                        format!("Tokenization error: {}", parse_err.message),
                        None,
                        None,
                    ));
                    continue; // Cannot parse further for this line
                }
                Ok((line_tokens, line_spans)) => {
                    // Try parsing this line as a (mini) program using the statement parser
                    let mut sp = StmtParser::new_with_spans(&line_tokens, &line_spans);
                    match sp.parse_program_with_enhanced_errors(line) {
                        Ok(_) => {
                            // No statement-level error on this line
                        }
                        Err(parse_err) => {
                            let range = if let Some(span) = &parse_err.span {
                                let start_pos = Position::new(
                                    line_idx as u32,
                                    span.start.column.saturating_sub(1),
                                );
                                let end_pos = Position::new(
                                    line_idx as u32,
                                    span.end.column.saturating_sub(1),
                                );
                                Range::new(start_pos, end_pos)
                            } else {
                                // Fallback: highlight whole line
                                Range::new(
                                    Position::new(line_idx as u32, 0),
                                    Position::new(line_idx as u32, line.chars().count() as u32),
                                )
                            };

                            diags.push(Diagnostic::new(
                                range,
                                Some(DiagnosticSeverity::ERROR),
                                None,
                                Some("qcl".to_string()),
                                parse_err.message.clone(),
                                None,
                                None,
                            ));

                            // Additionally, attempt expression recovery to collect more issues on this line
                            let expr_errs = ExprParser::recover_expression_errors(
                                &line_tokens,
                                &line_spans,
                                line,
                            );
                            for ee in expr_errs {
                                if diags.len() >= MAX_DIAGNOSTICS {
                                    break;
                                }
                                let range2 = if let Some(span) = &ee.span {
                                    let start_pos = Position::new(
                                        line_idx as u32,
                                        span.start.column.saturating_sub(1),
                                    );
                                    let end_pos = Position::new(
                                        line_idx as u32,
                                        span.end.column.saturating_sub(1),
                                    );
                                    Range::new(start_pos, end_pos)
                                } else {
                                    Range::new(
                                        Position::new(line_idx as u32, 0),
                                        Position::new(line_idx as u32, line.chars().count() as u32),
                                    )
                                };
                                diags.push(Diagnostic::new(
                                    range2,
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    ee.message.clone(),
                                    None,
                                    None,
                                ));
                            }
                        }
                    }
                }
            }
            if diags.len() >= MAX_DIAGNOSTICS {
                break;
            }
        }

        diags
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

    /// Generate semantic tokens for QCL code
    pub fn generate_semantic_tokens(&self, content: &str) -> Vec<SemanticToken> {
        // We'll first collect tokens with absolute positions, then convert to LSP delta encoding
        let mut tokens: Vec<SemanticToken> = Vec::new();
        let mut line_number = 0;

        // Define the legend indices (must match the legend in main.rs)
        const COMMENT_IDX: u32 = 0;
        const KEYWORD_IDX: u32 = 1;
        const VARIABLE_IDX: u32 = 2;
        const STRING_IDX: u32 = 4;
        const NUMBER_IDX: u32 = 5;
        const OPERATOR_IDX: u32 = 6;
        const PROPERTY_IDX: u32 = 8;

        let lines: Vec<&str> = content.lines().collect();

        // Track multi-line block comments
        let mut in_block_comment = false;

        for line in lines {
            let mut char_index = 0;
            let chars: Vec<char> = line.chars().collect();
            let len = chars.len();

            while char_index < len {
                let c = chars[char_index];

                // Skip whitespace
                if c.is_whitespace() {
                    char_index += 1;
                    continue;
                }

                // Handle block comments spanning multiple lines
                if in_block_comment {
                    // Search for end of block comment '*/' in the current line
                    let mut j = char_index;
                    let mut end_found = false;
                    while j + 1 < len {
                        if chars[j] == '*' && chars[j + 1] == '/' {
                            // Emit from current position to the end of '*/'
                            let comment_len = (j + 2) - char_index;
                            tokens.push(self.create_token(
                                line_number,
                                char_index,
                                comment_len,
                                COMMENT_IDX,
                                0,
                            ));
                            // Move past '*/' and exit block comment
                            char_index = j + 2;
                            in_block_comment = false;
                            end_found = true;
                            break;
                        }
                        j += 1;
                    }
                    if !end_found {
                        // Entire rest of line is comment
                        tokens.push(self.create_token(
                            line_number,
                            char_index,
                            len - char_index,
                            COMMENT_IDX,
                            0,
                        ));
                        // Proceed to next line still inside block comment
                        break;
                    }
                    // Continue scanning the remainder of the line after closing the block comment
                    continue;
                }

                // Handle line comments: // ...
                if c == '/' && char_index + 1 < len && chars[char_index + 1] == '/' {
                    let comment_start = char_index;
                    // Everything to end of line is a comment
                    tokens.push(self.create_token(
                        line_number,
                        comment_start,
                        len - comment_start,
                        COMMENT_IDX,
                        0,
                    ));
                    break;
                }

                // Handle block comment start: /* ... */
                if c == '/' && char_index + 1 < len && chars[char_index + 1] == '*' {
                    let comment_start = char_index;
                    // Look for closing */ on the same line first
                    let mut j = char_index + 2;
                    let mut closed_here = false;
                    while j + 1 < len {
                        if chars[j] == '*' && chars[j + 1] == '/' {
                            // Found end on the same line
                            let comment_len = (j + 2) - comment_start;
                            tokens.push(self.create_token(
                                line_number,
                                comment_start,
                                comment_len,
                                COMMENT_IDX,
                                0,
                            ));
                            char_index = j + 2;
                            closed_here = true;
                            break;
                        }
                        j += 1;
                    }
                    if !closed_here {
                        // Rest of line is comment; continue block comment on next lines
                        tokens.push(self.create_token(
                            line_number,
                            comment_start,
                            len - comment_start,
                            COMMENT_IDX,
                            0,
                        ));
                        in_block_comment = true;
                        break;
                    }
                    // Continue scanning after end of block comment on same line
                    continue;
                }

                // Handle hash-style comments (# ...) for legacy compatibility
                if c == '#' {
                    let comment_start = char_index;
                    // Everything to end of line is a comment
                    tokens.push(self.create_token(
                        line_number,
                        comment_start,
                        len - comment_start,
                        COMMENT_IDX,
                        0,
                    ));
                    break;
                }

                // Handle strings
                if c == '"' || c == '\'' {
                    let string_start = char_index;
                    let quote_char = c;
                    char_index += 1;

                    while char_index < len && chars[char_index] != quote_char {
                        if chars[char_index] == '\\' && char_index + 1 < len {
                            char_index += 2;
                        } else {
                            char_index += 1;
                        }
                    }

                    if char_index < len && chars[char_index] == quote_char {
                        char_index += 1;
                    }

                    tokens.push(self.create_token(
                        line_number,
                        string_start,
                        char_index - string_start,
                        STRING_IDX,
                        0,
                    ));
                    continue;
                }

                // Handle numbers
                if c.is_ascii_digit() {
                    let num_start = char_index;
                    while char_index < len
                        && (chars[char_index].is_ascii_digit() || chars[char_index] == '.')
                    {
                        char_index += 1;
                    }

                    tokens.push(self.create_token(
                        line_number,
                        num_start,
                        char_index - num_start,
                        NUMBER_IDX,
                        0,
                    ));
                    continue;
                }

                // Handle identifiers and keywords
                if c.is_alphabetic() || c == '_' {
                    let ident_start = char_index;
                    while char_index < len
                        && (chars[char_index].is_alphanumeric() || chars[char_index] == '_')
                    {
                        char_index += 1;
                    }

                    let identifier: String = chars[ident_start..char_index].iter().collect();

                    // Check for keywords
                    let token_idx = match identifier.as_str() {
                        "if" | "else" | "while" | "let" | "fn" | "return" | "break"
                        | "continue" | "goto" | "import" | "from" | "as" | "go" | "select"
                        | "case" | "default" | "true" | "false" | "nil" => KEYWORD_IDX,
                        _ => VARIABLE_IDX,
                    };

                    tokens.push(self.create_token(
                        line_number,
                        ident_start,
                        char_index - ident_start,
                        token_idx,
                        0,
                    ));
                    continue;
                }

                // Handle context access (@)
                if c == '@' {
                    tokens.push(self.create_token(line_number, char_index, 1, PROPERTY_IDX, 0));
                    char_index += 1;
                    continue;
                }

                // Handle operators
                if c == '=' || c == '!' || c == '<' || c == '>' || c == '&' || c == '|' || c == '-'
                {
                    let op_start = char_index;

                    // Handle multi-character operators
                    if char_index + 1 < len {
                        let next_char = chars[char_index + 1];
                        match (c, next_char) {
                            ('=', '=')
                            | ('!', '=')
                            | ('<', '=')
                            | ('>', '=')
                            | ('&', '&')
                            | ('|', '|')
                            | ('-', '>') => {
                                char_index += 2;
                                tokens.push(self.create_token(
                                    line_number,
                                    op_start,
                                    2,
                                    OPERATOR_IDX,
                                    0,
                                ));
                                continue;
                            }
                            _ => {}
                        }
                    }

                    // Single character operator
                    char_index += 1;
                    tokens.push(self.create_token(line_number, op_start, 1, OPERATOR_IDX, 0));
                    continue;
                }

                // Other operators and punctuation
                if "+-*/%.,;(){}[]".contains(c) {
                    let token_idx = match c {
                        '+' | '-' | '*' | '/' | '%' => OPERATOR_IDX,
                        '.' => PROPERTY_IDX,
                        _ => OPERATOR_IDX,
                    };

                    tokens.push(self.create_token(line_number, char_index, 1, token_idx, 0));
                    char_index += 1;
                    continue;
                }

                char_index += 1;
            }

            line_number += 1;
        }

        // Convert absolute positions to delta-encoded positions required by LSP
        let mut result: Vec<SemanticToken> = Vec::with_capacity(tokens.len());
        let mut prev_line: u32 = 0;
        let mut prev_start: u32 = 0;
        let mut first = true;

        for t in tokens.into_iter() {
            let line = t.delta_line; // stored absolute line
            let start = t.delta_start; // stored absolute start
            let delta_line = if first {
                line
            } else {
                line.saturating_sub(prev_line)
            };
            let delta_start = if first || delta_line != 0 {
                start
            } else {
                start.saturating_sub(prev_start)
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: t.length,
                token_type: t.token_type,
                token_modifiers_bitset: t.token_modifiers_bitset,
            });

            prev_line = line;
            prev_start = start;
            first = false;
        }

        result
    }

    /// Generate semantic tokens for a specific LSP range (best-effort).
    /// Note: range is interpreted using UTF-16 columns per LSP spec.
    pub fn generate_semantic_tokens_in_range(
        &self,
        content_slice: &str,
        range: Range,
    ) -> Vec<SemanticToken> {
        // Helper to convert UTF-16 column to char index for a single line
        fn utf16_to_char_idx(line: &str, utf16_col: u32) -> usize {
            let mut seen = 0usize;
            for (i, ch) in line.chars().enumerate() {
                let w = ch.len_utf16();
                if seen + w > utf16_col as usize {
                    return i;
                }
                seen += w;
                if seen == utf16_col as usize {
                    return i + 1;
                }
            }
            line.chars().count()
        }

        let start_line_abs = range.start.line as usize;
        let end_line_abs = range.end.line as usize;
        let start_utf16 = range.start.character;
        let end_utf16 = range.end.character;

        // We'll first collect tokens with absolute positions, then convert to LSP delta encoding
        let mut tokens: Vec<SemanticToken> = Vec::new();

        // Define the legend indices (must match the legend in main.rs)
        const COMMENT_IDX: u32 = 0;
        const KEYWORD_IDX: u32 = 1;
        const VARIABLE_IDX: u32 = 2;
        const STRING_IDX: u32 = 4;
        const NUMBER_IDX: u32 = 5;
        const OPERATOR_IDX: u32 = 6;
        const PROPERTY_IDX: u32 = 8;

        let lines: Vec<&str> = content_slice.lines().collect();
        if lines.is_empty() {
            return Vec::new();
        }
        let first_local = 0usize;
        let last_local = lines.len().saturating_sub(1);

        // Track multi-line block comments inside the processed window only
        let mut in_block_comment = false;

        for (local_idx, line) in lines.iter().enumerate() {
            let line_number = (start_line_abs + local_idx) as u32;
            let mut char_index = 0usize;
            let chars: Vec<char> = line.chars().collect();
            let len = chars.len();

            // Compute char bounds for clamping tokens on boundary lines
            let start_char_bound = if local_idx == first_local {
                utf16_to_char_idx(line, start_utf16)
            } else {
                0
            };
            let end_char_bound = if local_idx == last_local {
                utf16_to_char_idx(line, end_utf16).max(start_char_bound)
            } else {
                len
            };

            while char_index < len {
                let c = chars[char_index];

                // Skip whitespace
                if c.is_whitespace() {
                    char_index += 1;
                    continue;
                }

                // Handle block comments spanning multiple lines
                if in_block_comment {
                    // Search for end of block comment '*/' in the current line
                    let mut j = char_index;
                    while j + 1 < len {
                        if chars[j] == '*' && chars[j + 1] == '/' {
                            // emit block until here if within bounds
                            let start = char_index.max(start_char_bound);
                            let length = if j + 2 > start {
                                (j + 2).saturating_sub(start)
                            } else {
                                0
                            };
                            if length > 0 && start < end_char_bound {
                                let capped_len = length.min(end_char_bound.saturating_sub(start));
                                tokens.push(self.create_token(
                                    line_number,
                                    start,
                                    capped_len,
                                    COMMENT_IDX,
                                    0,
                                ));
                            }
                            char_index = j + 2;
                            in_block_comment = false;
                            break;
                        }
                        j += 1;
                    }
                    if in_block_comment {
                        // whole rest of line is a comment
                        let start = char_index.max(start_char_bound);
                        if start < end_char_bound {
                            let capped_len = end_char_bound - start;
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                COMMENT_IDX,
                                0,
                            ));
                        }
                        break;
                    }
                    continue;
                }

                // Line comments
                if c == '/' && char_index + 1 < len && chars[char_index + 1] == '/' {
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len = end_char_bound - start;
                        tokens.push(self.create_token(
                            line_number,
                            start,
                            capped_len,
                            COMMENT_IDX,
                            0,
                        ));
                    }
                    break;
                }
                // Block comment start
                if c == '/' && char_index + 1 < len && chars[char_index + 1] == '*' {
                    in_block_comment = true;
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len = (char_index + 2).saturating_sub(start);
                        if capped_len > 0 {
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                COMMENT_IDX,
                                0,
                            ));
                        }
                    }
                    char_index += 2;
                    continue;
                }

                // Strings (single quoted or double quoted)
                if c == '"' || c == '\'' {
                    let mut j = char_index + 1;
                    while j < len {
                        if chars[j] == c && chars[j - 1] != '\\' {
                            break;
                        }
                        j += 1;
                    }
                    let end = if j < len { j + 1 } else { len };
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = end.saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len =
                                capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                STRING_IDX,
                                0,
                            ));
                        }
                    }
                    char_index = end;
                    continue;
                }

                // Numbers
                if c.is_ascii_digit() {
                    let mut j = char_index + 1;
                    while j < len && (chars[j].is_ascii_digit() || chars[j] == '.') {
                        j += 1;
                    }
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = j.saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len =
                                capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                NUMBER_IDX,
                                0,
                            ));
                        }
                    }
                    char_index = j;
                    continue;
                }

                // Identifiers and keywords, variables (@xxx)
                if c.is_ascii_alphabetic() || c == '_' || c == '@' {
                    let mut j = char_index + 1;
                    while j < len
                        && (chars[j].is_ascii_alphanumeric() || chars[j] == '_' || chars[j] == '.')
                    {
                        j += 1;
                    }
                    let slice: String = chars[char_index..j].iter().collect();
                    let token_idx = if slice == "if"
                        || slice == "else"
                        || slice == "while"
                        || slice == "let"
                        || slice == "fn"
                        || slice == "return"
                        || slice == "break"
                        || slice == "continue"
                        || slice == "goto"
                        || slice == "import"
                        || slice == "from"
                        || slice == "as"
                        || slice == "go"
                        || slice == "select"
                        || slice == "case"
                        || slice == "default"
                        || slice == "true"
                        || slice == "false"
                        || slice == "nil"
                    {
                        KEYWORD_IDX
                    } else if slice.starts_with('@') {
                        VARIABLE_IDX
                    } else {
                        // Detect property access segments after '.' within identifier handling
                        VARIABLE_IDX
                    };
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = j.saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len =
                                capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                token_idx,
                                0,
                            ));
                        }
                    }
                    char_index = j;
                    continue;
                }

                // Operators
                if "=!<>|&-".contains(c) {
                    let op_start = char_index;
                    if char_index + 1 < len {
                        let next_char = chars[char_index + 1];
                        match (c, next_char) {
                            ('=', '=')
                            | ('!', '=')
                            | ('<', '=')
                            | ('>', '=')
                            | ('&', '&')
                            | ('|', '|')
                            | ('-', '>') => {
                                let start = op_start.max(start_char_bound);
                                if start < end_char_bound {
                                    let capped_len_total = (op_start + 2).saturating_sub(start);
                                    if capped_len_total > 0 {
                                        let capped_len = capped_len_total
                                            .min(end_char_bound.saturating_sub(start));
                                        tokens.push(self.create_token(
                                            line_number,
                                            start,
                                            capped_len,
                                            OPERATOR_IDX,
                                            0,
                                        ));
                                    }
                                }
                                char_index += 2;
                                continue;
                            }
                            _ => {}
                        }
                    }
                    let start = op_start.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = (op_start + 1).saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len =
                                capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                OPERATOR_IDX,
                                0,
                            ));
                        }
                    }
                    char_index += 1;
                    continue;
                }

                // Other operators and punctuation
                if "+-*/%.,;(){}[]".contains(c) {
                    let token_idx = match c {
                        '+' | '-' | '*' | '/' | '%' => OPERATOR_IDX,
                        '.' => PROPERTY_IDX,
                        _ => OPERATOR_IDX,
                    };
                    let start = char_index.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = (char_index + 1).saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len =
                                capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(
                                line_number,
                                start,
                                capped_len,
                                token_idx,
                                0,
                            ));
                        }
                    }
                    char_index += 1;
                    continue;
                }

                char_index += 1;
            }
        }

        // Convert absolute positions to delta-encoded positions required by LSP
        let mut result: Vec<SemanticToken> = Vec::with_capacity(tokens.len());
        let mut prev_line: u32 = 0;
        let mut prev_start: u32 = 0;
        let mut first = true;
        for t in tokens.into_iter() {
            let line = t.delta_line;
            let start = t.delta_start;
            let delta_line = if first {
                line
            } else {
                line.saturating_sub(prev_line)
            };
            let delta_start = if first || delta_line != 0 {
                start
            } else {
                start.saturating_sub(prev_start)
            };
            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: t.length,
                token_type: t.token_type,
                token_modifiers_bitset: t.token_modifiers_bitset,
            });
            prev_line = line;
            prev_start = start;
            first = false;
        }
        result
    }

    fn create_token(
        &self,
        line: u32,
        start_char: usize,
        length: usize,
        token_type_idx: u32,
        modifiers: u32,
    ) -> SemanticToken {
        SemanticToken {
            delta_line: line,                  // line number (0-based)
            delta_start: start_char as u32,    // start character (0-based)
            length: length as u32,             // token length
            token_type: token_type_idx,        // token type index
            token_modifiers_bitset: modifiers, // token modifiers
        }
    }

    fn dedup_diagnostics(&self, diagnostics: &mut Vec<Diagnostic>) {
        diagnostics.sort_by(|a, b| {
            let ra = &a.range;
            let rb = &b.range;
            (
                ra.start.line,
                ra.start.character,
                ra.end.line,
                ra.end.character,
                a.message.clone(),
            )
                .cmp(&(
                    rb.start.line,
                    rb.start.character,
                    rb.end.line,
                    rb.end.character,
                    b.message.clone(),
                ))
        });
        diagnostics.dedup_by(|a, b| a.range == b.range && a.message == b.message);
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
        let has_context_info = result.diagnostics.iter().any(|d| {
            d.severity == Some(DiagnosticSeverity::INFORMATION)
                && d.message.contains("requires context")
        });
        assert!(has_context_info, "Expected context requirement diagnostic");
    }

    #[test]
    fn test_analyze_invalid_expression() {
        let analyzer = create_analyzer();
        let result = analyzer.analyze("@req.user.role == 'unterminated string");

        // Should have diagnostic for invalid expression (tokenization error due to unterminated string)
        assert!(!result.diagnostics.is_empty());
        assert_eq!(
            result.diagnostics[0].severity,
            Some(DiagnosticSeverity::ERROR)
        );
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

    #[test]
    fn test_generate_semantic_tokens_simple_expression() {
        let analyzer = create_analyzer();
        let content = "@req.user.role == 'admin'";
        let tokens = analyzer.generate_semantic_tokens(content);

        // Define the legend indices for testing
        const OPERATOR_IDX: u32 = 6;
        const STRING_IDX: u32 = 4;
        const PROPERTY_IDX: u32 = 8;

        // Should have tokens for: @, req, ., user, ., role, ==, 'admin'
        assert!(!tokens.is_empty());

        // Check that we have a keyword token for '==' (operator)
        let mut found_operator = false;
        let mut found_string = false;
        let mut found_property = false;

        for token in &tokens {
            if token.token_type == OPERATOR_IDX {
                found_operator = true;
            } else if token.token_type == STRING_IDX {
                found_string = true;
            } else if token.token_type == PROPERTY_IDX {
                found_property = true;
            }
        }

        assert!(found_operator, "Should find operator token");
        assert!(found_string, "Should find string token");
        assert!(found_property, "Should find property token for '@'");
    }

    #[test]
    fn test_generate_semantic_tokens_statement_program() {
        let analyzer = create_analyzer();
        let content = r#"
            let user_level = @req.user.level;
            if user_level > 5 {
                return "admin";
            }
        "#;
        let tokens = analyzer.generate_semantic_tokens(content);

        // Define the legend indices for testing
        const KEYWORD_IDX: u32 = 1;

        // Should have tokens for keywords, variables, operators, etc.
        assert!(!tokens.is_empty());

        // Check for specific tokens
        let mut found_let = false;
        let mut found_if = false;
        let mut found_return = false;

        for token in &tokens {
            if token.token_type == KEYWORD_IDX {
                // This is a simplified check - in a real implementation,
                // we'd need to look at the actual content
                found_let = true;
                found_if = true;
                found_return = true;
            }
        }

        // Should find keywords
        assert!(
            found_let || found_if || found_return,
            "Should find keyword tokens"
        );
    }

    #[test]
    fn test_generate_semantic_tokens_with_comments() {
        let analyzer = create_analyzer();
        let content = r#"
            // This is a comment
            let x = 42;
        "#;
        let tokens = analyzer.generate_semantic_tokens(content);

        // Define the legend indices for testing
        const COMMENT_IDX: u32 = 0;

        // Should have tokens including comment
        assert!(!tokens.is_empty());

        // Check for comment token
        let mut found_comment = false;
        for token in &tokens {
            if token.token_type == COMMENT_IDX {
                found_comment = true;
                break;
            }
        }

        assert!(found_comment, "Should find comment token");
    }

    #[test]
    fn test_generate_semantic_tokens_with_numbers() {
        let analyzer = create_analyzer();
        let content = "let x = 42 + 3.14;";
        let tokens = analyzer.generate_semantic_tokens(content);

        // Define the legend indices for testing
        const NUMBER_IDX: u32 = 5;

        // Should have tokens including numbers
        assert!(!tokens.is_empty());

        // Check for number tokens
        let mut found_number = false;
        for token in &tokens {
            if token.token_type == NUMBER_IDX {
                found_number = true;
                break;
            }
        }

        assert!(found_number, "Should find number token");
    }
}
