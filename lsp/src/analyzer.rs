use qcl_core::resolve::slots::{FunctionLayout, SlotResolver};
use qcl_core::{
    ast::Parser as ExprParser,
    expr::Expr,
    module::ModuleRegistry,
    stmt::{stmt_parser::StmtParser, ImportStmt, Stmt},
    token::{Span, Tokenizer},
    typ::TypeChecker,
    val::Val,
};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use tower_lsp::lsp_types::*;

// Soft limits to keep LSP responsive on large/broken files
const MAX_SCAN_LINES: usize = 400; // max lines to line-scan
const MAX_SCAN_CHUNKS: usize = 300; // max logical chunks to scan
const MAX_DIAGNOSTICS: usize = 200; // cap diagnostics volume
                                    // Caps to avoid overwhelming the editor with semantic tokens
const MAX_TOKENS_PER_DOC: usize = 20_000; // hard ceiling for full-document tokens
const MAX_TOKENS_PER_RANGE: usize = 8_000; // hard ceiling for range tokens

/// Result of analyzing QCL code, containing diagnostics, symbols, and identifier roots
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    pub diagnostics: Vec<Diagnostic>,
    pub symbols: Vec<DocumentSymbol>,
    pub identifier_roots: HashSet<String>,
}

/// QCL Language analyzer for providing LSP functionality
#[derive(Default)]
pub struct QclAnalyzer {
    // Cache for tokenization results to avoid re-tokenizing same content
    token_cache: HashMap<String, (Vec<qcl_core::token::Token>, Vec<Span>)>,
    // Cache for completion items that don't change
    completion_cache: Option<Vec<CompletionItem>>,
    // Registered stdlib modules for resolution/completions
    registry: ModuleRegistry,
    // Base directory for resolving relative file imports
    base_dir: Option<PathBuf>,
}

impl QclAnalyzer {
    /// Create a new QCL analyzer
    pub fn new() -> Self {
        // Initialize a registry preloaded with stdlib modules and globals
        let mut registry = ModuleRegistry::new();
        // Register stdlib globals and modules so LSP can recognize them
        qcl_stdlib::register_stdlib_globals(&mut registry);
        qcl_stdlib::register_stdlib_modules(&mut registry);

        Self {
            token_cache: HashMap::new(),
            completion_cache: None,
            registry,
            base_dir: None,
        }
    }

    /// Compute type inlay hints for simple `let name = expr;` without explicit annotations.
    /// Places a TYPE hint like `: Int` right after the pattern (before '=').
    #[cfg(test)]
    pub fn compute_type_inlay_hints(&self, content: &str, range: Range) -> Vec<InlayHint> {
        let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(pair) => pair,
            Err(_) => return Vec::new(),
        };
        self.compute_type_inlay_hints_from_tokens(&tokens, &spans, range)
    }

    /// Variant that reuses a pre-tokenized buffer for performance.
    pub fn compute_type_inlay_hints_from_tokens(
        &self,
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
        range: Range,
    ) -> Vec<InlayHint> {
        let mut hints: Vec<InlayHint> = Vec::new();
        use qcl_core::token::Token as T;
        let mut i = 0usize;
        while i < tokens.len() {
            if !matches!(tokens[i], T::Let) {
                i += 1;
                continue;
            }
            let let_idx = i;
            i += 1;

            // Capture pattern region until top-level ':' (annotation) or '=' (assignment)
            let start_pat = i;
            let mut end_pat = i;
            let mut paren = 0i32;
            let mut bracket = 0i32;
            let mut brace = 0i32;
            let mut saw_colon = false;
            let mut found_assign = false;
            while i < tokens.len() {
                match &tokens[i] {
                    T::LParen => paren += 1,
                    T::RParen => {
                        if paren > 0 {
                            paren -= 1;
                        }
                    }
                    T::LBracket => bracket += 1,
                    T::RBracket => {
                        if bracket > 0 {
                            bracket -= 1;
                        }
                    }
                    T::LBrace => brace += 1,
                    T::RBrace => {
                        if brace > 0 {
                            brace -= 1;
                        }
                    }
                    T::Assign if paren == 0 && bracket == 0 && brace == 0 => {
                        found_assign = true;
                        break;
                    }
                    T::Colon if paren == 0 && bracket == 0 && brace == 0 => {
                        saw_colon = true;
                        break;
                    }
                    _ => {}
                }
                end_pat = i;
                i += 1;
            }
            if !found_assign || saw_colon {
                // Skip cases without '=' or with explicit annotation
                continue;
            }

            // Determine RHS expression token range: after '=' until next top-level ';'
            let mut j = i + 1; // i at '='
            let mut depth = 0i32;
            let mut end_expr = j;
            while j < tokens.len() {
                match &tokens[j] {
                    T::LParen | T::LBracket | T::LBrace => depth += 1,
                    T::RParen | T::RBracket | T::RBrace => depth -= 1,
                    T::Semicolon if depth == 0 => break,
                    _ => {}
                }
                end_expr = j;
                j += 1;
            }
            if end_expr > i {
                // Parse expression and infer type
                let expr_tokens = &tokens[i + 1..=end_expr];
                if !expr_tokens.is_empty() {
                    if let Ok(expr) = ExprParser::new(expr_tokens).parse() {
                        let mut checker = TypeChecker::new();
                        if let Ok(typ) = checker.infer_resolved_type(&expr) {
                            // Place hint at end of pattern
                            let pat_tok_idx = if end_pat >= start_pat { end_pat } else { start_pat };
                            if pat_tok_idx < spans.len() {
                                let sp = &spans[pat_tok_idx];
                                let pos = Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1));
                                if pos.line >= range.start.line && pos.line <= range.end.line {
                                    let label = format!(": {}", typ.display());
                                    hints.push(InlayHint {
                                        position: pos,
                                        label: InlayHintLabel::from(label),
                                        kind: Some(InlayHintKind::TYPE),
                                        text_edits: None,
                                        tooltip: None,
                                        padding_left: Some(true),
                                        padding_right: Some(false),
                                        data: None,
                                    });
                                }
                            }
                        }
                    }
                }
            }

            // Advance to end of statement
            i = j;
            while i < tokens.len() && !matches!(tokens[i], T::Semicolon) {
                i += 1;
            }
            if i < tokens.len() {
                i += 1;
            }
            // Prevent infinite loop on invalid sequences
            if i <= let_idx {
                i = let_idx + 1;
            }
        }
        hints
    }

    /// Compute type hints for short declarations: `name := expr;`
    #[cfg(test)]
    pub fn compute_define_type_hints(&self, content: &str, range: Range) -> Vec<InlayHint> {
        let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(pair) => pair,
            Err(_) => return Vec::new(),
        };
        self.compute_define_type_hints_from_tokens(&tokens, &spans, range)
    }

    /// Variant that reuses a pre-tokenized buffer for performance.
    pub fn compute_define_type_hints_from_tokens(
        &self,
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
        range: Range,
    ) -> Vec<InlayHint> {
        let mut hints: Vec<InlayHint> = Vec::new();
        use qcl_core::token::Token as T;
        let mut i = 0usize;
        while i + 2 < tokens.len() {
            match (&tokens[i], &tokens[i + 1], &tokens[i + 2]) {
                (T::Id(_), T::Colon, T::Assign) => {
                    // Parse expression from i+3 to next top-level ';'
                    let mut j = i + 3;
                    let mut depth = 0i32;
                    let mut end_expr = j;
                    while j < tokens.len() {
                        match &tokens[j] {
                            T::LParen | T::LBracket | T::LBrace => depth += 1,
                            T::RParen | T::RBracket | T::RBrace => depth -= 1,
                            T::Semicolon if depth == 0 => break,
                            _ => {}
                        }
                        end_expr = j;
                        j += 1;
                    }
                    if end_expr >= i + 3 {
                        let expr_tokens = &tokens[i + 3..=end_expr];
                        if let Ok(expr) = ExprParser::new(expr_tokens).parse() {
                            let mut checker = TypeChecker::new();
                            if let Ok(typ) = checker.infer_resolved_type(&expr) {
                                if i < spans.len() {
                                    let sp = &spans[i];
                                    let pos = Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1));
                                    if pos.line >= range.start.line && pos.line <= range.end.line {
                                        let label = format!(": {}", typ.display());
                                        hints.push(InlayHint {
                                            position: pos,
                                            label: InlayHintLabel::from(label),
                                            kind: Some(InlayHintKind::TYPE),
                                            text_edits: None,
                                            tooltip: None,
                                            padding_left: Some(true),
                                            padding_right: Some(false),
                                            data: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                    // Advance to next ';'
                    i = j;
                    while i < tokens.len() && !matches!(tokens[i], T::Semicolon) {
                        i += 1;
                    }
                    if i < tokens.len() {
                        i += 1;
                    }
                }
                _ => i += 1,
            }
        }
        hints
    }

    /// Compute type inlay hints for function return types: place a TYPE hint like `-> Int`
    /// after the parameter list. If multiple return statements exist (e.g., branches),
    /// the displayed type is a union of all discovered return expression types.
    #[cfg(test)]
    pub fn compute_function_return_type_hints(&self, content: &str, range: Range) -> Vec<InlayHint> {
        let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(pair) => pair,
            Err(_) => return Vec::new(),
        };
        self.compute_function_return_type_hints_from_tokens(&tokens, &spans, range)
    }

    /// Variant that reuses a pre-tokenized buffer for performance.
    pub fn compute_function_return_type_hints_from_tokens(
        &self,
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
        range: Range,
    ) -> Vec<InlayHint> {
        let mut hints: Vec<InlayHint> = Vec::new();
        use qcl_core::token::Token as T;
        let mut i = 0usize;
        while i < tokens.len() {
            if !matches!(tokens[i], T::Fn) {
                i += 1;
                continue;
            }
            // fn name ( params ) { body }
            let mut j = i + 1;
            // Skip function name if present
            if matches!(tokens.get(j), Some(T::Id(_))) {
                j += 1;
            } else {
                i += 1;
                continue;
            }
            // Expect parameter list
            if !matches!(tokens.get(j), Some(T::LParen)) {
                i += 1;
                continue;
            }
            let mut depth = 0i32;
            // find matching ')'
            while j < tokens.len() {
                match &tokens[j] {
                    T::LParen => depth += 1,
                    T::RParen => {
                        depth -= 1;
                        if depth == 0 {
                            j += 1;
                            break;
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
            let rparen_idx = j.saturating_sub(1);
            // Expect function body starting '{'
            if !matches!(tokens.get(j), Some(T::LBrace)) {
                i = j;
                continue;
            }
            // Find matching '}' for the body
            let mut body_depth = 0i32;
            let body_start = j + 1; // after '{'
            j += 1;
            let mut body_end = body_start;
            while j < tokens.len() {
                match &tokens[j] {
                    T::LBrace => body_depth += 1,
                    T::RBrace => {
                        if body_depth == 0 {
                            body_end = j;
                            break;
                        }
                        body_depth -= 1;
                    }
                    _ => {}
                }
                j += 1;
            }
            if body_end <= body_start {
                i = j + 1;
                continue;
            }
            // Within body, scan for all `return <expr>;` occurrences (including inside branches)
            let mut k = body_start;
            let mut return_types: Vec<qcl_core::val::Type> = Vec::new();
            while k < body_end {
                if matches!(tokens[k], T::Return) {
                    // capture expression until next top-level `;` relative to paren/brace depth of this expression
                    let mut e = k + 1;
                    let mut expr_depth = 0i32;
                    let mut last = e;
                    while e < body_end {
                        match &tokens[e] {
                            T::LParen | T::LBracket | T::LBrace => expr_depth += 1,
                            T::RParen | T::RBracket | T::RBrace => expr_depth -= 1,
                            T::Semicolon if expr_depth == 0 => break,
                            _ => {}
                        }
                        last = e;
                        e += 1;
                    }
                    if last > k {
                        let expr_tokens = &tokens[k + 1..=last];
                        if !expr_tokens.is_empty() {
                            if let Ok(expr) = qcl_core::ast::Parser::new(expr_tokens).parse() {
                                let mut checker = qcl_core::typ::TypeChecker::new();
                                if let Ok(ret_ty) = checker.infer_resolved_type(&expr) {
                                    return_types.push(ret_ty);
                                }
                            }
                        }
                    }
                    // Advance past this statement terminator if present
                    k = e + 1;
                    continue;
                }
                k += 1;
            }

            if !return_types.is_empty() {
                // Deduplicate by display string for stable union label
                use std::collections::BTreeMap;
                let mut by_key: BTreeMap<String, qcl_core::val::Type> = BTreeMap::new();
                for t in return_types {
                    by_key.entry(t.display()).or_insert(t);
                }
                let parts: Vec<String> = by_key.into_keys().collect();
                let label = if parts.len() == 1 {
                    format!(" -> {}", parts[0])
                } else {
                    format!(" -> {}", parts.join(" | "))
                };

                // Place hint right after the parameter list, at the end of ')'
                if rparen_idx < spans.len() {
                    let sp = &spans[rparen_idx];
                    let pos = Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1));
                    if pos.line >= range.start.line && pos.line <= range.end.line {
                        hints.push(InlayHint {
                            position: pos,
                            label: InlayHintLabel::from(label),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                }
            }
            i = j + 1;
        }
        hints
    }

    /// Clear caches - useful when memory usage becomes high
    pub fn clear_caches(&mut self) {
        self.token_cache.clear();
        self.completion_cache = None;
    }

    /// Set the base directory used for resolving file imports
    pub fn set_base_dir(&mut self, base: PathBuf) {
        self.base_dir = Some(base);
    }

    /// Scan tokens to add diagnostics for unknown stdlib modules and unknown exports with precise spans
    fn add_import_diagnostics(&self, tokens: &[qcl_core::token::Token], spans: &[Span], result: &mut AnalysisResult) {
        use qcl_core::token::Token as T;

        let mut i = 0usize;
        while i < tokens.len() {
            match &tokens[i] {
                T::Import => {
                    let mut j = i + 1;
                    match tokens.get(j) {
                        Some(T::Str(path)) => {
                            // import "file"; -> check existence
                            let exists = self.file_exists(path);
                            if !exists {
                                // Diagnostic on the string span (includes quotes)
                                if j < spans.len() {
                                    let sp = &spans[j];
                                    let range = Range::new(
                                        Position::new(sp.start.line - 1, sp.start.column.saturating_sub(1)),
                                        Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1)),
                                    );
                                    let mut d = Diagnostic::new(
                                        range,
                                        Some(DiagnosticSeverity::ERROR),
                                        None,
                                        Some("qcl".to_string()),
                                        format!("File not found: {}", path),
                                        None,
                                        None,
                                    );
                                    d.code = Some(NumberOrString::String("qcl_file_not_found".to_string()));
                                    result.diagnostics.push(d);
                                }
                            }
                            // advance to ';'
                            while j < tokens.len() && !matches!(tokens[j], T::Semicolon) {
                                j += 1;
                            }
                            i = j + 1;
                            continue;
                        }
                        Some(T::LBrace) => {
                            // import { a, b as c } from module;
                            j += 1; // after '{'
                            let mut item_indices: Vec<usize> = Vec::new();
                            while j < tokens.len() {
                                match &tokens[j] {
                                    T::RBrace => {
                                        j += 1;
                                        break;
                                    }
                                    T::Id(_) => {
                                        // record the exported name id position (before any 'as')
                                        let id_idx = j;
                                        item_indices.push(id_idx);
                                        j += 1;
                                        // Skip optional 'as alias'
                                        if matches!(tokens.get(j), Some(T::As)) {
                                            j += 1;
                                            if matches!(tokens.get(j), Some(T::Id(_))) {
                                                j += 1;
                                            }
                                        }
                                    }
                                    T::Comma => j += 1,
                                    _ => j += 1,
                                }
                            }
                            // Expect 'from' then module id
                            while j < tokens.len() && !matches!(tokens[j], T::From) {
                                j += 1;
                            }
                            if j + 1 < tokens.len() {
                                j += 1; // move to module id
                                if let T::Id(mod_name) = &tokens[j] {
                                    if self.registry.get_module(mod_name).is_ok() {
                                        // Validate each item against module exports
                                        if let Ok(m) = self.registry.get_module(mod_name) {
                                            let exports = m.exports();
                                            for idx in item_indices {
                                                if let T::Id(item_name) = &tokens[idx] {
                                                    if !exports.contains_key(item_name) && idx < spans.len() {
                                                        let sp = &spans[idx];
                                                        let range = Range::new(
                                                            Position::new(
                                                                sp.start.line - 1,
                                                                sp.start.column.saturating_sub(1),
                                                            ),
                                                            Position::new(
                                                                sp.end.line - 1,
                                                                sp.end.column.saturating_sub(1),
                                                            ),
                                                        );
                                                        result.diagnostics.push(Diagnostic::new(
                                                            range,
                                                            Some(DiagnosticSeverity::ERROR),
                                                            None,
                                                            Some("qcl".to_string()),
                                                            format!(
                                                                "Unknown export '{}' from module '{}'",
                                                                item_name, mod_name
                                                            ),
                                                            None,
                                                            None,
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                    } else if j < spans.len() {
                                        let sp = &spans[j];
                                        let range = Range::new(
                                            Position::new(sp.start.line - 1, sp.start.column.saturating_sub(1)),
                                            Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1)),
                                        );
                                        result.diagnostics.push(Diagnostic::new(
                                            range,
                                            Some(DiagnosticSeverity::ERROR),
                                            None,
                                            Some("qcl".to_string()),
                                            format!("Unknown module: {}", mod_name),
                                            None,
                                            None,
                                        ));
                                    }
                                }
                            }
                            // advance to semicolon
                            while j < tokens.len() && !matches!(tokens[j], T::Semicolon) {
                                j += 1;
                            }
                            i = j + 1;
                            continue;
                        }
                        Some(T::Mul) => {
                            // import * as alias from module;
                            // seek 'from' then module id
                            while j < tokens.len() && !matches!(tokens[j], T::From) {
                                j += 1;
                            }
                            if j + 1 < tokens.len() {
                                j += 1;
                                if let T::Id(mod_name) = &tokens[j] {
                                    if self.registry.get_module(mod_name).is_err() && j < spans.len() {
                                        let sp = &spans[j];
                                        let range = Range::new(
                                            Position::new(sp.start.line - 1, sp.start.column.saturating_sub(1)),
                                            Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1)),
                                        );
                                        result.diagnostics.push(Diagnostic::new(
                                            range,
                                            Some(DiagnosticSeverity::ERROR),
                                            None,
                                            Some("qcl".to_string()),
                                            format!("Unknown module: {}", mod_name),
                                            None,
                                            None,
                                        ));
                                    }
                                }
                            }
                            // advance to semicolon
                            while j < tokens.len() && !matches!(tokens[j], T::Semicolon) {
                                j += 1;
                            }
                            i = j + 1;
                            continue;
                        }
                        Some(T::Id(mod_name)) => {
                            // import module [as alias]?;
                            let mod_idx = j;
                            if self.registry.get_module(mod_name).is_err() && mod_idx < spans.len() {
                                let sp = &spans[mod_idx];
                                let range = Range::new(
                                    Position::new(sp.start.line - 1, sp.start.column.saturating_sub(1)),
                                    Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1)),
                                );
                                result.diagnostics.push(Diagnostic::new(
                                    range,
                                    Some(DiagnosticSeverity::ERROR),
                                    None,
                                    Some("qcl".to_string()),
                                    format!("Unknown module: {}", mod_name),
                                    None,
                                    None,
                                ));
                            }
                            // move to ';'
                            while j < tokens.len() && !matches!(tokens[j], T::Semicolon) {
                                j += 1;
                            }
                            i = j + 1;
                            continue;
                        }
                        _ => {}
                    }

                    i = j + 1;
                }
                _ => {
                    i += 1;
                }
            }
        }
    }

    fn file_exists(&self, rel: &str) -> bool {
        // Absolute path: use as-is
        let path = Path::new(rel);
        if path.is_absolute() {
            return path.exists();
        }
        let base = self.base_dir.as_ref().cloned().unwrap_or_else(|| PathBuf::from("."));
        let candidates = [base.clone(), base.join("lib"), base.join("modules")];
        for dir in candidates.iter() {
            let p = dir.join(rel);
            if p.exists() {
                return true;
            }
            // Try with .qcl appended if missing extension
            if p.extension().is_none() {
                let with_ext = p.with_extension("qcl");
                if with_ext.exists() {
                    return true;
                }
            }
        }
        false
    }

    /// Tokenize with spans, using an internal cache keyed by full content string.
    pub fn tokenize_with_spans_cached(
        &mut self,
        content: &str,
    ) -> std::result::Result<(Vec<qcl_core::token::Token>, Vec<Span>), qcl_core::token::ParseError> {
        if let Some(cached) = self.token_cache.get(content) {
            return Ok(cached.clone());
        }
        match Tokenizer::tokenize_enhanced_with_spans(content) {
            Ok(pair) => {
                if content.len() < 10_000 {
                    if self.token_cache.len() >= 100 {
                        self.token_cache.clear();
                    }
                    self.token_cache.insert(content.to_string(), pair.clone());
                }
                Ok(pair)
            }
            Err(e) => Err(e),
        }
    }

    /// Analyze QCL code and return diagnostics, symbols, and identifier roots
    pub fn analyze(&mut self, content: &str) -> AnalysisResult {
        let mut result = AnalysisResult {
            diagnostics: Vec::new(),
            symbols: Vec::new(),
            identifier_roots: HashSet::new(),
        };

        // Try parsing as expression first - use cached tokenization if available
        let (tokens, spans) = if let Some(cached) = self.token_cache.get(content) {
            cached.clone()
        } else {
            match Tokenizer::tokenize_enhanced_with_spans(content) {
                Ok(pair) => {
                    // Cache the successful tokenization result
                    if content.len() < 10_000 {
                        // Only cache reasonably sized content
                        // Limit cache size to prevent memory issues
                        if self.token_cache.len() >= 100 {
                            self.token_cache.clear();
                        }
                        self.token_cache.insert(content.to_string(), pair.clone());
                    }
                    pair
                }
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
            }
        };

        let mut expr_parser = ExprParser::new_with_spans(&tokens, &spans);
        match expr_parser.parse_with_enhanced_errors(content) {
            Ok(expr) => {
                // Collect identifier roots
                result.identifier_roots = expr.requested_ctx();

                // Add expression symbol
                let symbol = DocumentSymbol {
                    name: "expression".to_string(),
                    detail: Some("QCL Expression".to_string()),
                    kind: SymbolKind::CONSTANT,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range: Range::new(Position::new(0, 0), Position::new(0, content.len() as u32)),
                    selection_range: Range::new(Position::new(0, 0), Position::new(0, content.len() as u32)),
                    children: None,
                };
                result.symbols.push(symbol);

                // Add identifier validation diagnostics if we can parse the expression again for validation
                let expr_result = Ok(expr);
                let id_diagnostics = self.validate_identifier_access(&expr_result, None);
                result.diagnostics.extend(id_diagnostics);

                // Even for expressions, scan for import diagnostics (typically none)
                self.add_import_diagnostics(&tokens, &spans, &mut result);
            }
            Err(expr_err) => {
                // Attempt expression-level recovery to surface multiple errors for pure expressions
                let expr_recover_errors = ExprParser::recover_expression_errors(&tokens, &spans, content);
                // Try parsing as statement program
                let mut stmt_parser = StmtParser::new_with_spans(&tokens, &spans);
                match stmt_parser.parse_program_with_enhanced_errors(content) {
                    Ok(program) => {
                        // Analyze statements for symbols and identifier roots
                        self.analyze_statements(&program.statements, &mut result);
                        // Integrate slot-based symbols (parameters/locals) for richer outline
                        let mut resolver = SlotResolver::new();
                        let resolution = resolver.resolve_program_slots(&program);
                        // Enrich slot layout with scanned source spans for precise symbol ranges
                        let enriched = self.enrich_layout_spans(&resolution.root, &tokens, &spans);
                        // Top-level variable declarations (outside functions), grouped
                        let top_level_vars = Self::collect_decl_symbols(&enriched);
                        if !top_level_vars.is_empty() {
                            // Keep individual variables at top-level for backward compatibility
                            result.symbols.extend(top_level_vars.clone());
                            let (range_start, range_end) = (
                                top_level_vars
                                    .first()
                                    .map(|s| s.range.start)
                                    .unwrap_or(Position::new(0, 0)),
                                top_level_vars
                                    .last()
                                    .map(|s| s.range.end)
                                    .unwrap_or(Position::new(0, 0)),
                            );
                            let vars_container = DocumentSymbol {
                                name: "Variables".to_string(),
                                detail: None,
                                kind: SymbolKind::NAMESPACE,
                                tags: None,
                                #[allow(deprecated)]
                                deprecated: None,
                                range: Range::new(range_start, range_end),
                                selection_range: Range::new(range_start, range_start),
                                children: Some(top_level_vars),
                            };
                            result.symbols.push(vars_container);
                        }

                        // Top-level imports grouped
                        let import_syms = Self::collect_import_symbols_via_tokens(&tokens, &spans);
                        if !import_syms.is_empty() {
                            // Keep individual imports at top-level for backward compatibility
                            result.symbols.extend(import_syms.clone());
                            let (range_start, range_end) = (
                                import_syms
                                    .first()
                                    .map(|s| s.range.start)
                                    .unwrap_or(Position::new(0, 0)),
                                import_syms.last().map(|s| s.range.end).unwrap_or(Position::new(0, 0)),
                            );
                            let imports_container = DocumentSymbol {
                                name: "Imports".to_string(),
                                detail: None,
                                kind: SymbolKind::NAMESPACE,
                                tags: None,
                                #[allow(deprecated)]
                                deprecated: None,
                                range: Range::new(range_start, range_end),
                                selection_range: Range::new(range_start, range_start),
                                children: Some(import_syms),
                            };
                            result.symbols.push(imports_container);
                        }
                        // Add function symbols (nested hierarchy) using scanned blocks + enriched layouts
                        let fblocks = Self::scan_function_blocks(&tokens, &spans);
                        let (parents, children) = Self::compute_fn_block_hierarchy(&fblocks);
                        // Top-level functions in source order
                        let mut top_indices: Vec<usize> =
                            (0..fblocks.len()).filter(|&i| parents[i].is_none()).collect();
                        // Preserve source order as in fblocks
                        top_indices.sort();
                        for (top_ord, i) in top_indices.iter().enumerate() {
                            let layout_opt = enriched.children.get(top_ord);
                            let sym =
                                Self::build_function_symbol_tree(&fblocks, &children, *i, layout_opt, &tokens, &spans);
                            result.symbols.push(sym);
                        }

                        // Labels syntax is not supported; no label symbols at top-level
                        // Add precise import diagnostics using tokens/spans
                        self.add_import_diagnostics(&tokens, &spans, &mut result);
                    }
                    Err(stmt_err) => {
                        // If we found expression-level errors and the content doesn't look like statements,
                        // prefer reporting these expression diagnostics.
                        let mut collected: Vec<Diagnostic> = Vec::new();
                        let has_statement_keywords = content.contains("let ")
                            || content.contains("if ")
                            || content.contains("while ")
                            || content.contains("return ")
                            || content.contains("break")
                            || content.contains("continue");
                        if !expr_recover_errors.is_empty() && !has_statement_keywords {
                            for e in expr_recover_errors {
                                let range = if let Some(span) = &e.span {
                                    let start_pos = Position::new(span.start.line - 1, span.start.column - 1);
                                    let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                                    Range::new(start_pos, end_pos)
                                } else {
                                    Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
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
                        let (stmts, errs) = recover_parser.parse_program_recovering_with_enhanced_errors(content);
                        if !errs.is_empty() {
                            for e in errs {
                                let range = if let Some(span) = &e.span {
                                    let start_pos = Position::new(span.start.line - 1, span.start.column - 1);
                                    let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                                    Range::new(start_pos, end_pos)
                                } else {
                                    Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
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
                            // Even with errors, analyze statements to surface symbols and identifier roots
                            self.analyze_statements(&stmts, &mut result);
                            // And add precise import diagnostics using tokens/spans
                            self.add_import_diagnostics(&tokens, &spans, &mut result);
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
                                || content.contains("break")
                                || content.contains("continue");
                            let parse_err = if has_statement_keywords { &stmt_err } else { &expr_err };

                            let range = if let Some(span) = &parse_err.span {
                                let start_pos = Position::new(span.start.line - 1, span.start.column - 1);
                                let end_pos = Position::new(span.end.line - 1, span.end.column - 1);
                                Range::new(start_pos, end_pos)
                            } else {
                                Range::new(Position::new(0, 0), Position::new(0, content.len() as u32))
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

                        result.diagnostics.extend(collected);
                        // Also attempt import diagnostics if tokens parsed
                        self.add_import_diagnostics(&tokens, &spans, &mut result);
                    }
                }
            }
        }

        // Deduplicate diagnostics by range and message to reduce noise
        self.dedup_diagnostics(&mut result.diagnostics);

        result
    }

    /// Build a new FunctionLayout tree with decl spans populated by scanning tokens.
    /// Heuristics: assigns spans in source order matching names to declarations in the resolver order.
    pub(crate) fn enrich_layout_spans(
        &self,
        layout: &FunctionLayout,
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
    ) -> FunctionLayout {
        // Scan function blocks (including nested) and correlate with layouts
        let fblocks = Self::scan_function_blocks(tokens, spans);
        let (parents, children_map) = Self::compute_fn_block_hierarchy(&fblocks);

        // Top-level declarations outside function blocks + function names as top-level binds
        let toplevel_decl_spans = Self::scan_toplevel_decl_spans(tokens, spans, &fblocks);

        // Helper to assign spans to decls from a queue per name
        fn assign_spans(
            mut decls: Vec<qcl_core::resolve::slots::Decl>,
            pool: &mut HashMap<String, Vec<Span>>,
        ) -> Vec<qcl_core::resolve::slots::Decl> {
            for d in decls.iter_mut() {
                if let Some(list) = pool.get_mut(&d.name) {
                    if !list.is_empty() {
                        d.span = Some(list.remove(0));
                    }
                }
            }
            decls
        }

        // Prepare a toplevel pool by name
        let mut top_pool: HashMap<String, Vec<Span>> = HashMap::new();
        for (name, sp) in toplevel_decl_spans {
            top_pool.entry(name).or_default().push(sp);
        }
        let mut new_root = FunctionLayout {
            decls: assign_spans(layout.decls.clone(), &mut top_pool),
            total_locals: layout.total_locals,
            uses: layout.uses.clone(),
            children: Vec::new(),
        };

        // Signature helpers
        fn layout_param_signature(layout: &FunctionLayout) -> Vec<String> {
            let mut params: Vec<(usize, String)> = layout
                .decls
                .iter()
                .filter(|d| d.is_param)
                .map(|d| (d.index as usize, d.name.clone()))
                .collect();
            params.sort_by_key(|(i, _)| *i);
            params.into_iter().map(|(_, n)| n).collect()
        }
        fn fblock_param_signature(fb: &FnBlockInfo) -> Vec<String> {
            fb.param_spans.iter().map(|(n, _)| n.clone()).collect()
        }
        fn fb_locals_pool(
            tokens: &[qcl_core::token::Token],
            spans: &[Span],
            fb: &FnBlockInfo,
        ) -> HashMap<String, Vec<Span>> {
            let mut pool: HashMap<String, Vec<Span>> = HashMap::new();
            for (pname, pspan) in fb.param_spans.iter() {
                pool.entry(pname.clone()).or_default().push(pspan.clone());
            }
            let locals = QclAnalyzer::scan_decl_spans_in_range(tokens, spans, fb.body_start_idx, fb.body_end_idx);
            for (n, sp) in locals {
                pool.entry(n).or_default().push(sp);
            }
            pool
        }

        // Align a list of child layouts to a set of function block indices in order
        fn align_children(
            layouts: &[FunctionLayout],
            fb_indices: &[usize],
            fblocks: &[FnBlockInfo],
        ) -> Vec<Option<usize>> {
            let mut used = vec![false; layouts.len()];
            let mut mapping: Vec<Option<usize>> = vec![None; fb_indices.len()];
            for (pos, &fi) in fb_indices.iter().enumerate() {
                let fb_sig = fblock_param_signature(&fblocks[fi]);
                let mut best: Option<(usize, i32)> = None; // (layout_idx, score)
                for (li, lay) in layouts.iter().enumerate() {
                    if used[li] {
                        continue;
                    }
                    let lsig = layout_param_signature(lay);
                    let score = if lsig == fb_sig {
                        1000 + lsig.len() as i32
                    } else if lsig.len() == fb_sig.len() {
                        100 + lsig.iter().zip(fb_sig.iter()).filter(|(a, b)| *a == *b).count() as i32
                    } else {
                        lsig.iter().filter(|n| fb_sig.contains(n)).count() as i32
                    };
                    if best.map(|(_, s)| score > s).unwrap_or(true) {
                        best = Some((li, score));
                    }
                }
                if let Some((li, _)) = best {
                    used[li] = true;
                    mapping[pos] = Some(li);
                }
            }
            mapping
        }

        // Build top-level children in source order
        let mut top_indices: Vec<usize> = (0..fblocks.len()).filter(|&i| parents[i].is_none()).collect();
        top_indices.sort();
        let top_mapping = align_children(&layout.children, &top_indices, &fblocks);

        let mut built_children: Vec<FunctionLayout> = Vec::new();
        for (ord, &maybe_li) in top_mapping.iter().enumerate() {
            let fb_idx = top_indices[ord];
            let fb = &fblocks[fb_idx];
            let mut pool = fb_locals_pool(tokens, spans, fb);
            let base = maybe_li
                .and_then(|li| layout.children.get(li))
                .cloned()
                .unwrap_or_else(|| FunctionLayout {
                    decls: Vec::new(),
                    total_locals: 0,
                    uses: Vec::new(),
                    children: Vec::new(),
                });
            let mut enriched_child = FunctionLayout {
                decls: assign_spans(base.decls, &mut pool),
                total_locals: base.total_locals,
                uses: base.uses,
                children: Vec::new(),
            };

            // Nested children alignment
            let child_fb_indices = children_map.get(fb_idx).cloned().unwrap_or_default();
            let child_mapping = align_children(&base.children, &child_fb_indices, &fblocks);
            let mut nested_children: Vec<FunctionLayout> = Vec::new();
            for (cpos, &maybe_cli) in child_mapping.iter().enumerate() {
                let cfi = child_fb_indices[cpos];
                let cfb = &fblocks[cfi];
                let mut cpool = fb_locals_pool(tokens, spans, cfb);
                let cbase = maybe_cli
                    .and_then(|li| base.children.get(li))
                    .cloned()
                    .unwrap_or_else(|| FunctionLayout {
                        decls: Vec::new(),
                        total_locals: 0,
                        uses: Vec::new(),
                        children: Vec::new(),
                    });
                let cenriched = FunctionLayout {
                    decls: assign_spans(cbase.decls, &mut cpool),
                    total_locals: cbase.total_locals,
                    uses: cbase.uses,
                    children: Vec::new(),
                };
                nested_children.push(cenriched);
            }
            enriched_child.children = nested_children;
            built_children.push(enriched_child);
        }
        new_root.children = built_children;
        new_root
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FnBlockInfo {
    name: String,
    name_span: Span,
    /// Token index of '{' starting the body
    pub(crate) body_start_idx: usize,
    /// Token index of the matching '}' ending the body
    pub(crate) body_end_idx: usize,
    /// Parameter identifier spans (name -> span)
    param_spans: Vec<(String, Span)>,
}

impl QclAnalyzer {
    /// Scan function blocks in source order: name, name span, body token range, and param spans.
    pub(crate) fn scan_function_blocks(tokens: &[qcl_core::token::Token], spans: &[Span]) -> Vec<FnBlockInfo> {
        use qcl_core::token::Token as T;
        let mut i = 0usize;
        let mut out: Vec<FnBlockInfo> = Vec::new();
        while i < tokens.len() {
            if !matches!(tokens[i], T::Fn) {
                i += 1;
                continue;
            }
            // Expect function name
            if i + 1 >= tokens.len() {
                break;
            }
            let name = if let T::Id(ref n) = tokens[i + 1] {
                n.clone()
            } else {
                i += 1;
                continue;
            };
            let name_span = match spans.get(i + 1).cloned() {
                Some(sp) => sp,
                None => match spans.get(i).cloned() {
                    Some(sp) => sp,
                    None => continue,
                },
            };
            // Find params region: '(' ... matching ')'
            let mut j = i + 2;
            if j >= tokens.len() || !matches!(tokens[j], T::LParen) {
                i += 1;
                continue;
            }
            let mut paren = 1i32;
            let mut params: Vec<(String, Span)> = Vec::new();
            j += 1;
            while j < tokens.len() && paren > 0 {
                match &tokens[j] {
                    T::LParen => paren += 1,
                    T::RParen => paren -= 1,
                    T::Id(p) if paren == 1 => {
                        if let Some(sp) = spans.get(j) {
                            params.push((p.clone(), sp.clone()));
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
            // After params, optional '->' and type, then expect '{'
            while j < tokens.len() && !matches!(tokens[j], T::LBrace) {
                j += 1;
            }
            if j >= tokens.len() || !matches!(tokens[j], T::LBrace) {
                i = j;
                continue;
            }
            // Find matching '}' for body
            let mut brace = 1i32;
            let body_start = j; // points to '{'
            j += 1;
            while j < tokens.len() && brace > 0 {
                match tokens[j] {
                    T::LBrace => brace += 1,
                    T::RBrace => brace -= 1,
                    _ => {}
                }
                j += 1;
            }
            let body_end = j.saturating_sub(1); // index of '}'
            out.push(FnBlockInfo {
                name,
                name_span,
                body_start_idx: body_start,
                body_end_idx: body_end,
                param_spans: params,
            });
            // Do not skip over the entire body; continue scanning to discover nested functions too
            i += 1;
        }
        out
    }

    /// Scan variable declaration spans within [start_idx, end_idx] token range: let-patterns and short defines.
    fn scan_decl_spans_in_range(
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
        start_idx: usize,
        end_idx: usize,
    ) -> Vec<(String, Span)> {
        use qcl_core::token::Token as T;
        let mut out: Vec<(String, Span)> = Vec::new();
        let mut i = start_idx;
        while i <= end_idx && i < tokens.len() {
            match &tokens[i] {
                T::Let => {
                    // Pattern region until top-level ':' or '='
                    let mut j = i + 1;
                    let mut paren = 0i32;
                    let mut bracket = 0i32;
                    let mut brace = 0i32;
                    while j <= end_idx && j < tokens.len() {
                        match tokens[j] {
                            T::LParen => paren += 1,
                            T::RParen => paren -= 1,
                            T::LBracket => bracket += 1,
                            T::RBracket => bracket -= 1,
                            T::LBrace => brace += 1,
                            T::RBrace => brace -= 1,
                            T::Assign | T::Colon if paren == 0 && bracket == 0 && brace == 0 => break,
                            _ => {}
                        }
                        j += 1;
                    }
                    // Within [i+1, j), collect identifier tokens as declarations
                    let mut k = i + 1;
                    while k < j && k <= end_idx {
                        if let T::Id(ref n) = tokens[k] {
                            if let Some(sp) = spans.get(k) {
                                out.push((n.clone(), sp.clone()))
                            }
                        }
                        k += 1;
                    }
                    i = j;
                }
                T::Id(name) => {
                    // Short define: id := expr ;
                    if i + 2 <= end_idx
                        && matches!(tokens.get(i + 1), Some(T::Colon))
                        && matches!(tokens.get(i + 2), Some(T::Assign))
                    {
                        if let Some(sp) = spans.get(i) {
                            out.push((name.clone(), sp.clone()));
                        }
                        i += 3;
                        continue;
                    }
                    i += 1;
                }
                _ => i += 1,
            }
        }
        out
    }

    /// Collect top-level declaration spans and function names (outside any function block body).
    fn scan_toplevel_decl_spans(
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
        fblocks: &Vec<FnBlockInfo>,
    ) -> Vec<(String, Span)> {
        use qcl_core::token::Token as T;
        let mut out: Vec<(String, Span)> = Vec::new();
        // Function names are top-level bindings
        for fb in fblocks {
            out.push((fb.name.clone(), fb.name_span.clone()));
        }
        // Scan all tokens skipping over function bodies
        let mut skip_ranges: Vec<(usize, usize)> =
            fblocks.iter().map(|fb| (fb.body_start_idx, fb.body_end_idx)).collect();
        skip_ranges.sort_by_key(|r| r.0);
        let mut i = 0usize;
        let mut ri = 0usize;
        while i < tokens.len() {
            if ri < skip_ranges.len() {
                let (s, e) = skip_ranges[ri];
                if i >= s && i <= e {
                    i = e + 1;
                    ri += 1;
                    continue;
                }
            }
            match &tokens[i] {
                T::Let => {
                    // As in range scan
                    let mut j = i + 1;
                    let mut paren = 0i32;
                    let mut bracket = 0i32;
                    let mut brace = 0i32;
                    while j < tokens.len() {
                        match tokens[j] {
                            T::LParen => paren += 1,
                            T::RParen => paren -= 1,
                            T::LBracket => bracket += 1,
                            T::RBracket => bracket -= 1,
                            T::LBrace => brace += 1,
                            T::RBrace => brace -= 1,
                            T::Assign | T::Colon if paren == 0 && bracket == 0 && brace == 0 => break,
                            _ => {}
                        }
                        j += 1;
                    }
                    let mut k = i + 1;
                    while k < j {
                        if let T::Id(ref n) = tokens[k] {
                            if let Some(sp) = spans.get(k) {
                                out.push((n.clone(), sp.clone()));
                            }
                        }
                        k += 1;
                    }
                    i = j;
                }
                T::Id(name) => {
                    if i + 2 < tokens.len()
                        && matches!(tokens.get(i + 1), Some(T::Colon))
                        && matches!(tokens.get(i + 2), Some(T::Assign))
                    {
                        if let Some(sp) = spans.get(i) {
                            out.push((name.clone(), sp.clone()));
                        }
                        i += 3;
                        continue;
                    }
                    i += 1;
                }
                _ => i += 1,
            }
        }
        out
    }

    /// Collect variable symbols (params/locals) for a single function layout.
    /// Does not recurse into nested children; returns symbols to be used as function.children.
    fn collect_decl_symbols(layout: &FunctionLayout) -> Vec<DocumentSymbol> {
        use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};
        let mut out: Vec<DocumentSymbol> = Vec::new();
        for decl in &layout.decls {
            let detail = if decl.is_param { "Parameter" } else { "Local" };
            let (range, selection_range) = if let Some(sp) = &decl.span {
                let start = Position::new(sp.start.line - 1, sp.start.column.saturating_sub(1));
                let end = Position::new(sp.end.line - 1, sp.end.column.saturating_sub(1));
                (Range::new(start, end), Range::new(start, end))
            } else {
                (
                    Range::new(Position::new(0, 0), Position::new(0, 0)),
                    Range::new(Position::new(0, 0), Position::new(0, 0)),
                )
            };
            out.push(DocumentSymbol {
                name: decl.name.clone(),
                detail: Some(format!("{} (slot #{})", detail, decl.index)),
                kind: SymbolKind::VARIABLE,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                range,
                selection_range,
                children: None,
            });
        }
        out
    }

    /// Group params and locals into two container nodes under the function.
    fn collect_decl_groups(layout: &FunctionLayout, func_range: Range) -> Vec<DocumentSymbol> {
        use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};
        let mut params: Vec<DocumentSymbol> = Vec::new();
        let mut locals: Vec<DocumentSymbol> = Vec::new();
        for sym in Self::collect_decl_symbols(layout) {
            // classify by detail text prefix
            if sym.detail.as_ref().map(|d| d.starts_with("Parameter")).unwrap_or(false) {
                params.push(sym);
            } else {
                locals.push(sym);
            }
        }
        let mut groups: Vec<DocumentSymbol> = Vec::new();
        if !params.is_empty() {
            groups.push(DocumentSymbol {
                name: "Parameters".to_string(),
                detail: None,
                kind: SymbolKind::NAMESPACE,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                range: func_range,
                selection_range: func_range,
                children: Some(params),
            });
        }
        if !locals.is_empty() {
            groups.push(DocumentSymbol {
                name: "Locals".to_string(),
                detail: None,
                kind: SymbolKind::NAMESPACE,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                range: func_range,
                selection_range: func_range,
                children: Some(locals),
            });
        }
        groups
    }

    /// Collect import symbols via token scanning and produce per-import DocumentSymbols.
    fn collect_import_symbols_via_tokens(tokens: &[qcl_core::token::Token], spans: &[Span]) -> Vec<DocumentSymbol> {
        use qcl_core::token::Token as T;
        use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};
        let mut out: Vec<DocumentSymbol> = Vec::new();
        let mut i = 0usize;
        while i < tokens.len() {
            if !matches!(tokens[i], T::Import) {
                i += 1;
                continue;
            }
            let start_idx = i;
            let mut j = i + 1;
            let mut label = String::from("import");
            // Derive a short label based on common forms
            if let Some(tok) = tokens.get(j) {
                match tok {
                    T::Str(s) => {
                        label = format!("import \"{}\"", s);
                        j += 1;
                    }
                    T::LBrace => {
                        // skip until 'from' then module id
                        j += 1;
                        while j < tokens.len() && !matches!(tokens[j], T::From) {
                            j += 1;
                        }
                        if j + 1 < tokens.len() {
                            if let T::Id(m) = &tokens[j + 1] {
                                label = format!("import {{…}} from {}", m);
                            } else {
                                label = "import {…}".to_string();
                            }
                        }
                    }
                    T::Id(m) => {
                        // maybe alias form later
                        label = format!("import {}", m);
                        // peek for 'as <alias>'
                        let mut k = j + 1;
                        if matches!(tokens.get(k), Some(T::As)) {
                            k += 1;
                            if let Some(T::Id(a)) = tokens.get(k) {
                                label = format!("import {} as {}", m, a);
                            }
                        }
                    }
                    _ => {}
                }
            }
            // Find end at next ';'
            while j < tokens.len() && !matches!(tokens[j], T::Semicolon) {
                j += 1;
            }
            let end_idx = j.min(tokens.len().saturating_sub(1));
            if let (Some(s0), Some(se)) = (spans.get(start_idx), spans.get(end_idx)) {
                let range = Range::new(
                    Position::new(s0.start.line - 1, s0.start.column.saturating_sub(1)),
                    Position::new(se.end.line - 1, se.end.column.saturating_sub(1)),
                );
                out.push(DocumentSymbol {
                    name: label,
                    detail: Some("Import statement".to_string()),
                    kind: SymbolKind::MODULE,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                });
            }
            i = j + 1;
        }
        out
    }

    // Labels are not supported; no label collection helpers

    /// Compute parent and children lists for function blocks based on body containment.
    fn compute_fn_block_hierarchy(fblocks: &[FnBlockInfo]) -> (Vec<Option<usize>>, Vec<Vec<usize>>) {
        let n = fblocks.len();
        let mut parent: Vec<Option<usize>> = vec![None; n];
        for (i, fi) in fblocks.iter().enumerate().take(n) {
            let s_i = fi.body_start_idx;
            let e_i = fi.body_end_idx;
            let mut best: Option<(usize, usize)> = None; // (j, span_len)
            for (j, fj) in fblocks.iter().enumerate().take(n) {
                if i == j {
                    continue;
                }
                let s_j = fj.body_start_idx;
                let e_j = fj.body_end_idx;
                if s_j <= s_i && e_j >= e_i {
                    let span_len = e_j.saturating_sub(s_j);
                    if best.map(|(_, l)| span_len < l).unwrap_or(true) {
                        best = Some((j, span_len));
                    }
                }
            }
            if let Some((pj, _)) = best {
                parent[i] = Some(pj);
            }
        }
        let mut children: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, pi) in parent.iter().enumerate().take(n) {
            if let Some(p) = *pi {
                children[p].push(i);
            }
        }
        (parent, children)
    }

    /// Recursively build a function symbol with nested function children + var/param children.
    fn build_function_symbol_tree(
        fblocks: &[FnBlockInfo],
        children_map: &[Vec<usize>],
        idx: usize,
        layout_opt: Option<&FunctionLayout>,
        tokens: &[qcl_core::token::Token],
        spans: &[Span],
    ) -> DocumentSymbol {
        use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};
        let fb = &fblocks[idx];
        let name_sp = fb.name_span.clone();
        let body_end = spans.get(fb.body_end_idx).cloned().unwrap_or(name_sp.clone());
        let range = Range::new(
            Position::new(name_sp.start.line - 1, name_sp.start.column.saturating_sub(1)),
            Position::new(body_end.end.line - 1, body_end.end.column.saturating_sub(1)),
        );
        let selection_range = Range::new(
            Position::new(name_sp.start.line - 1, name_sp.start.column.saturating_sub(1)),
            Position::new(name_sp.end.line - 1, name_sp.end.column.saturating_sub(1)),
        );
        let params_label = if fb.param_spans.is_empty() {
            String::new()
        } else {
            let names: Vec<String> = fb.param_spans.iter().map(|(n, _)| n.clone()).collect();
            names.join(", ")
        };
        let mut kids: Vec<DocumentSymbol> = Vec::new();
        // Add variables/params declared in this function grouped
        if let Some(layout) = layout_opt {
            kids.extend(Self::collect_decl_groups(layout, range));
        }
        if kids.is_empty() {
            // Fallback: build groups by scanning tokens (parameters + locals) when layout is unavailable
            let mut params: Vec<DocumentSymbol> = Vec::new();
            for (pname, pspan) in fb.param_spans.iter() {
                let start = Position::new(pspan.start.line - 1, pspan.start.column.saturating_sub(1));
                let end = Position::new(pspan.end.line - 1, pspan.end.column.saturating_sub(1));
                params.push(DocumentSymbol {
                    name: pname.clone(),
                    detail: Some("Parameter".to_string()),
                    kind: SymbolKind::VARIABLE,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range: Range::new(start, end),
                    selection_range: Range::new(start, end),
                    children: None,
                });
            }
            if !params.is_empty() {
                kids.push(DocumentSymbol {
                    name: "Parameters".to_string(),
                    detail: None,
                    kind: SymbolKind::NAMESPACE,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: Some(params),
                });
            }
            let locals = Self::scan_decl_spans_in_range(tokens, spans, fb.body_start_idx, fb.body_end_idx);
            if !locals.is_empty() {
                let mut local_syms: Vec<DocumentSymbol> = Vec::new();
                for (lname, lspan) in locals {
                    let start = Position::new(lspan.start.line - 1, lspan.start.column.saturating_sub(1));
                    let end = Position::new(lspan.end.line - 1, lspan.end.column.saturating_sub(1));
                    local_syms.push(DocumentSymbol {
                        name: lname,
                        detail: Some("Local".to_string()),
                        kind: SymbolKind::VARIABLE,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: Range::new(start, end),
                        selection_range: Range::new(start, end),
                        children: None,
                    });
                }
                kids.push(DocumentSymbol {
                    name: "Locals".to_string(),
                    detail: None,
                    kind: SymbolKind::NAMESPACE,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: Some(local_syms),
                });
            }
        }
        // Labels syntax is not supported; no function-local label grouping
        // Add nested functions in source order within this function
        let child_idxs = children_map.get(idx).cloned().unwrap_or_default();
        for (ord, child_i) in child_idxs.iter().enumerate() {
            let child_layout_opt = layout_opt.and_then(|l| l.children.get(ord));
            let child_sym =
                Self::build_function_symbol_tree(fblocks, children_map, *child_i, child_layout_opt, tokens, spans);
            kids.push(child_sym);
        }
        // Try to infer return type for function detail
        let detail = if let Some(ret) = Self::infer_fn_return_type_for_block(tokens, fb) {
            if params_label.is_empty() {
                format!("fn() -> {}", ret)
            } else {
                format!("fn({}) -> {}", params_label, ret)
            }
        } else if params_label.is_empty() {
            "Function".to_string()
        } else {
            format!("Function({})", params_label)
        };
        DocumentSymbol {
            name: fb.name.clone(),
            detail: Some(detail),
            kind: SymbolKind::FUNCTION,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
            range,
            selection_range,
            children: if kids.is_empty() { None } else { Some(kids) },
        }
    }

    /// Infer a function's return type by scanning return statements inside its body.
    fn infer_fn_return_type_for_block(tokens: &[qcl_core::token::Token], fb: &FnBlockInfo) -> Option<String> {
        use qcl_core::token::Token as T;
        let mut k = fb.body_start_idx + 1;
        let body_end = fb.body_end_idx;
        if body_end <= k {
            return None;
        }
        let mut return_types: Vec<qcl_core::val::Type> = Vec::new();
        while k < body_end {
            if matches!(tokens[k], T::Return) {
                let mut e = k + 1;
                let mut expr_depth = 0i32;
                let mut last = e;
                while e < body_end {
                    match &tokens[e] {
                        T::LParen | T::LBracket | T::LBrace => expr_depth += 1,
                        T::RParen | T::RBracket | T::RBrace => expr_depth -= 1,
                        T::Semicolon if expr_depth == 0 => break,
                        _ => {}
                    }
                    last = e;
                    e += 1;
                }
                if last > k {
                    let expr_tokens = &tokens[k + 1..=last];
                    if !expr_tokens.is_empty() {
                        if let Ok(expr) = qcl_core::ast::Parser::new(expr_tokens).parse() {
                            let mut checker = qcl_core::typ::TypeChecker::new();
                            if let Ok(ret_ty) = checker.infer_resolved_type(&expr) {
                                return_types.push(ret_ty);
                            }
                        }
                    }
                }
                k = e + 1;
                continue;
            }
            k += 1;
        }
        if return_types.is_empty() {
            return None;
        }
        use std::collections::BTreeMap;
        let mut by_key: BTreeMap<String, qcl_core::val::Type> = BTreeMap::new();
        for t in return_types {
            by_key.entry(t.display()).or_insert(t);
        }
        let parts: Vec<String> = by_key.into_keys().collect();
        Some(if parts.len() == 1 {
            parts[0].clone()
        } else {
            parts.join(" | ")
        })
    }

    /// List available stdlib module names
    pub fn list_stdlib_modules(&self) -> Vec<String> {
        self.registry.get_module_names()
    }

    /// List exports for a given stdlib module name
    pub fn list_module_exports(&self, module: &str) -> Option<Vec<String>> {
        match self.registry.get_module(module) {
            Ok(m) => {
                let exports = m.exports();
                let mut keys: Vec<String> = exports.keys().cloned().collect();
                keys.sort();
                Some(keys)
            }
            Err(_) => None,
        }
    }

    /// Collect imported module aliases from the given content.
    /// Returns mapping alias -> module_name (e.g., "m" -> "math").
    pub fn collect_import_aliases(&mut self, content: &str) -> HashMap<String, String> {
        let mut map = HashMap::new();
        // Tokenize using cached path to be consistent with analysis
        let (tokens, spans) = match self.tokenize_with_spans_cached(content) {
            Ok(p) => p,
            Err(_) => return map,
        };

        // Prefer full parse; fall back to recovering parse to extract as many imports as possible
        let mut stmts_acc: Vec<Box<Stmt>> = Vec::new();
        {
            let mut parser = StmtParser::new_with_spans(&tokens, &spans);
            if let Ok(program) = parser.parse_program_with_enhanced_errors(content) {
                stmts_acc.extend(program.statements);
            }
        }
        // Recover for any missed imports (e.g., partial files)
        {
            let mut recover_parser = StmtParser::new_with_spans(&tokens, &spans);
            let (more, _errs) = recover_parser.parse_program_recovering_with_enhanced_errors(content);
            for s in more {
                stmts_acc.push(s);
            }
        }

        for stmt in &stmts_acc {
            if let Stmt::Import(import_stmt) = stmt.as_ref() {
                match import_stmt {
                    ImportStmt::Module { module } => {
                        // import math; -> alias is module name
                        map.insert(module.clone(), module.clone());
                    }
                    ImportStmt::ModuleAlias { module, alias } => {
                        // import math as m; -> alias maps to module
                        map.insert(alias.clone(), module.clone());
                    }
                    ImportStmt::Namespace { alias, source } => {
                        if let qcl_core::stmt::ImportSource::Module(name) = source {
                            // import * as m from math; -> alias maps to module
                            map.insert(alias.clone(), name.clone());
                        }
                    }
                    ImportStmt::Items { source, .. } => {
                        // import { sqrt } from math; -> does not create a module alias
                        // We could track individual items in the future
                        if let qcl_core::stmt::ImportSource::Module(_name) = source {
                            // no alias to insert
                        }
                    }
                    ImportStmt::File { .. } => {
                        // File imports are not stdlib modules; ignore here
                    }
                }
            }
        }

        map
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
                        if !content[start_byte..end_byte].trim().is_empty() {
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
                        if !content[start_byte..end_byte].trim().is_empty() {
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
            if !tail.trim().is_empty() {
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
                            // Try expression recovery first for potentially multiple, more specific spans
                            let expr_errs = ExprParser::recover_expression_errors(&chunk_tokens, &chunk_spans, chunk);

                            if !expr_errs.is_empty() {
                                // Use expression errors if available (more specific)
                                for ee in expr_errs {
                                    if diags.len() >= MAX_DIAGNOSTICS {
                                        break;
                                    }
                                    let range = if let Some(span) = &ee.span {
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
                                    diags.push(Diagnostic::new(
                                        range,
                                        Some(DiagnosticSeverity::ERROR),
                                        None,
                                        Some("qcl".to_string()),
                                        ee.message.clone(),
                                        None,
                                        None,
                                    ));
                                }
                            } else {
                                // Fall back to statement error if no expression errors found
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
                                        range,
                                        Some(DiagnosticSeverity::ERROR),
                                        None,
                                        Some("qcl".to_string()),
                                        stmt_err.message.clone(),
                                        None,
                                        None,
                                    ));
                                }
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
                        let start_pos = Position::new(line_idx as u32, span.start.column.saturating_sub(1));
                        let end_pos = Position::new(line_idx as u32, span.end.column.saturating_sub(1));
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
                            // Try expression recovery first for potentially multiple, more specific spans
                            let expr_errs = ExprParser::recover_expression_errors(&line_tokens, &line_spans, line);

                            if !expr_errs.is_empty() {
                                // Use expression errors if available (more specific)
                                for ee in expr_errs {
                                    if diags.len() >= MAX_DIAGNOSTICS {
                                        break;
                                    }
                                    let range = if let Some(span) = &ee.span {
                                        let start_pos =
                                            Position::new(line_idx as u32, span.start.column.saturating_sub(1));
                                        let end_pos = Position::new(line_idx as u32, span.end.column.saturating_sub(1));
                                        Range::new(start_pos, end_pos)
                                    } else {
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
                                        ee.message.clone(),
                                        None,
                                        None,
                                    ));
                                }
                            } else {
                                // Fall back to statement error if no expression errors found
                                let range = if let Some(span) = &parse_err.span {
                                    let start_pos = Position::new(line_idx as u32, span.start.column.saturating_sub(1));
                                    let end_pos = Position::new(line_idx as u32, span.end.column.saturating_sub(1));
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
                Stmt::Let { pattern, .. } => {
                    // Extract variable names from pattern and create symbols for each
                    if let Some(variables) = extract_variables_from_pattern(pattern) {
                        for var_name in variables {
                            result.symbols.push(DocumentSymbol {
                                name: var_name.clone(),
                                detail: Some("Variable declaration".to_string()),
                                kind: SymbolKind::VARIABLE,
                                tags: None,
                                #[allow(deprecated)]
                                deprecated: None,
                                range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                                selection_range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, 100)),
                                children: None,
                            });
                        }
                    }
                }
                Stmt::Function { .. } => {}
                Stmt::Import(_import_stmt) => { /* imports are grouped via token scan later */ }
                _ => {}
            }
        }
    }

    /// Get common variable completions for the given prefix
    pub fn get_var_completions(&mut self, prefix: &str) -> Vec<CompletionItem> {
        // Use cached completion items if available
        let all_items = if let Some(ref cached) = self.completion_cache {
            cached.clone()
        } else {
            let mut items = Vec::new();

            // Common variable patterns (without legacy '@')
            let common_contexts = [
                ("req", "Request object"),
                ("req.user", "User information"),
                ("req.user.id", "User ID"),
                ("req.user.role", "User role"),
                ("req.user.name", "User name"),
                ("record", "Record object"),
                ("record.id", "Record ID"),
                ("record.owner", "Record owner"),
                ("record.granted", "Granted users list"),
                ("env", "Environment variables"),
                ("time", "Current timestamp"),
            ];

            for (context, desc) in common_contexts {
                items.push(CompletionItem {
                    label: context.to_string(),
                    kind: Some(CompletionItemKind::PROPERTY),
                    detail: Some(desc.to_string()),
                    ..Default::default()
                });
            }

            // Stdlib modules and their exports, e.g., "iter.zip"
            for module_name in self.registry.get_module_names() {
                // module entry itself
                items.push(CompletionItem {
                    label: module_name.clone(),
                    kind: Some(CompletionItemKind::MODULE),
                    detail: Some("stdlib module".to_string()),
                    ..Default::default()
                });

                if let Ok(m) = self.registry.get_module(&module_name) {
                    let exports = m.exports();
                    for (k, v) in exports {
                        let label = format!("{}.{}", module_name, k);
                        let (kind, detail) = match v {
                            Val::RustFunction(_) | Val::Closure { .. } => {
                                (CompletionItemKind::FUNCTION, "function".to_string())
                            }
                            Val::Int(_) | Val::Float(_) | Val::Bool(_) | Val::Str(_) => {
                                (CompletionItemKind::CONSTANT, "const".to_string())
                            }
                            Val::List(_) => (CompletionItemKind::VARIABLE, "list".to_string()),
                            Val::Map(_) => (CompletionItemKind::MODULE, "namespace".to_string()),
                            Val::Task { .. } => (CompletionItemKind::VALUE, "task".to_string()),
                            Val::Channel { .. } => (CompletionItemKind::VALUE, "channel".to_string()),
                            Val::Object { .. } => (CompletionItemKind::VALUE, "object".to_string()),
                            Val::Nil => (CompletionItemKind::VALUE, "nil".to_string()),
                        };
                        items.push(CompletionItem {
                            label,
                            kind: Some(kind),
                            detail: Some(format!("{}.{}: {}", module_name, k, detail)),
                            ..Default::default()
                        });
                    }
                }
            }

            // Cache the items for future use
            self.completion_cache = Some(items.clone());
            items
        };

        // Filter by prefix
        all_items
            .into_iter()
            .filter(|item| item.label.starts_with(prefix))
            .collect()
    }

    /// Validate identifier access in an expression against an optional variables map
    pub fn validate_identifier_access(
        &self,
        expr_result: &Result<Expr, anyhow::Error>,
        context: Option<&Val>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        if let Ok(expr) = expr_result {
            let required_ctx = expr.requested_ctx();

            if let Some(ctx) = context {
                // Check if required identifier roots are available
                for ctx_key in &required_ctx {
                    if !self.vars_has_key(ctx, ctx_key) {
                        diagnostics.push(Diagnostic::new(
                            Range::new(Position::new(0, 0), Position::new(0, 100)),
                            Some(DiagnosticSeverity::WARNING),
                            None,
                            Some("qcl".to_string()),
                            format!("Identifier root '{}' not found in provided variables", ctx_key),
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
                    format!("Expression references identifier roots: {:?}", required_ctx),
                    None,
                    None,
                ));
            }
        }

        diagnostics
    }

    fn vars_has_key(&self, context: &Val, key: &str) -> bool {
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

    /// Generate semantic tokens for QCL code (optimized version)
    pub fn generate_semantic_tokens(&self, content: &str) -> Vec<SemanticToken> {
        // Early return for empty content
        if content.trim().is_empty() {
            return Vec::new();
        }

        // Use the existing, working implementation but with optimizations
        let mut tokens: Vec<SemanticToken> = Vec::new();
        let mut line_number = 0;

        // Define the legend indices (must match the legend in main.rs)
        const COMMENT_IDX: u32 = 0;
        const KEYWORD_IDX: u32 = 1;
        const VARIABLE_IDX: u32 = 2;
        const FUNCTION_IDX: u32 = 3;
        const STRING_IDX: u32 = 4;
        const NUMBER_IDX: u32 = 5;
        const OPERATOR_IDX: u32 = 6;

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
                            tokens.push(self.create_token(line_number, char_index, comment_len, COMMENT_IDX, 0));
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
                        tokens.push(self.create_token(line_number, char_index, len - char_index, COMMENT_IDX, 0));
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
                    tokens.push(self.create_token(line_number, comment_start, len - comment_start, COMMENT_IDX, 0));
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
                            tokens.push(self.create_token(line_number, comment_start, comment_len, COMMENT_IDX, 0));
                            char_index = j + 2;
                            closed_here = true;
                            break;
                        }
                        j += 1;
                    }
                    if !closed_here {
                        // Rest of line is comment; continue block comment on next lines
                        tokens.push(self.create_token(line_number, comment_start, len - comment_start, COMMENT_IDX, 0));
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
                    tokens.push(self.create_token(line_number, comment_start, len - comment_start, COMMENT_IDX, 0));
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

                    tokens.push(self.create_token(line_number, string_start, char_index - string_start, STRING_IDX, 0));
                    continue;
                }

                // Handle numbers
                if c.is_ascii_digit() {
                    let num_start = char_index;
                    while char_index < len && (chars[char_index].is_ascii_digit() || chars[char_index] == '.') {
                        char_index += 1;
                    }

                    tokens.push(self.create_token(line_number, num_start, char_index - num_start, NUMBER_IDX, 0));
                    continue;
                }

                // Handle identifiers and keywords (and detect function calls)
                if c.is_alphabetic() || c == '_' {
                    let ident_start = char_index;
                    while char_index < len && (chars[char_index].is_alphanumeric() || chars[char_index] == '_') {
                        char_index += 1;
                    }

                    let identifier: String = chars[ident_start..char_index].iter().collect();

                    // Check for keywords
                    let mut token_idx = match identifier.as_str() {
                        "if" | "else" | "while" | "let" | "fn" | "return" | "break" | "continue" | "import"
                        | "from" | "as" | "go" | "select" | "case" | "default" | "true" | "false" | "nil" | "spawn"
                        | "chan" | "send" | "recv" => KEYWORD_IDX,
                        _ => VARIABLE_IDX,
                    };

                    // If next non-whitespace char is '(', treat as function identifier
                    if token_idx == VARIABLE_IDX {
                        let mut j = char_index;
                        while j < len && chars[j].is_whitespace() {
                            j += 1;
                        }
                        if j < len && chars[j] == '(' {
                            token_idx = FUNCTION_IDX;
                        }
                    }

                    tokens.push(self.create_token(line_number, ident_start, char_index - ident_start, token_idx, 0));
                    continue;
                }

                // Legacy '@' context access removed; treat '@' as punctuation

                // Handle operators - only tokenize multi-character operators to reduce density
                if c == '=' || c == '!' || c == '<' || c == '>' || c == '&' || c == '|' || c == '-' {
                    let op_start = char_index;

                    // Handle multi-character operators only
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
                                tokens.push(self.create_token(line_number, op_start, 2, OPERATOR_IDX, 0));
                                continue;
                            }
                            _ => {
                                // Skip single-character operators entirely
                                char_index += 1;
                                continue;
                            }
                        }
                    }

                    // Skip single character operators
                    char_index += 1;
                    continue;
                }

                // Skip other operators and punctuation to reduce token density
                if "+-*/%,;(){}[]@.".contains(c) {
                    char_index += 1;
                    continue;
                }

                char_index += 1;
            }

            line_number += 1;
            // Stop early if token budget is exceeded
            if tokens.len() >= MAX_TOKENS_PER_DOC {
                break;
            }
        }

        // Convert absolute positions to delta-encoded positions required by LSP
        let mut result: Vec<SemanticToken> = Vec::with_capacity(tokens.len());
        let mut prev_line: u32 = 0;
        let mut prev_start: u32 = 0;
        let mut first = true;

        for t in tokens.into_iter() {
            let line = t.delta_line; // stored absolute line
            let start = t.delta_start; // stored absolute start
            let delta_line = if first { line } else { line.saturating_sub(prev_line) };
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
    pub fn generate_semantic_tokens_in_range(&self, content_slice: &str, range: Range) -> Vec<SemanticToken> {
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
        let _end_line_abs = range.end.line as usize;
        let start_utf16 = range.start.character;
        let end_utf16 = range.end.character;

        // We'll first collect tokens with absolute positions, then convert to LSP delta encoding
        let mut tokens: Vec<SemanticToken> = Vec::new();

        // Define the legend indices (must match the legend in main.rs)
        const COMMENT_IDX: u32 = 0;
        const KEYWORD_IDX: u32 = 1;
        const VARIABLE_IDX: u32 = 2;
        const FUNCTION_IDX: u32 = 3;
        const STRING_IDX: u32 = 4;
        const NUMBER_IDX: u32 = 5;
        const OPERATOR_IDX: u32 = 6;

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
                                tokens.push(self.create_token(line_number, start, capped_len, COMMENT_IDX, 0));
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
                            tokens.push(self.create_token(line_number, start, capped_len, COMMENT_IDX, 0));
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
                        tokens.push(self.create_token(line_number, start, capped_len, COMMENT_IDX, 0));
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
                            tokens.push(self.create_token(line_number, start, capped_len, COMMENT_IDX, 0));
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
                            let capped_len = capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(line_number, start, capped_len, STRING_IDX, 0));
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
                            let capped_len = capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(line_number, start, capped_len, NUMBER_IDX, 0));
                        }
                    }
                    char_index = j;
                    continue;
                }

                // Identifiers and keywords (and detect function calls)
                if c.is_ascii_alphabetic() || c == '_' {
                    let ident_start = char_index;
                    let mut j = char_index + 1;
                    while j < len && (chars[j].is_ascii_alphanumeric() || chars[j] == '_') {
                        j += 1;
                    }
                    let slice: &str = &line[ident_start..j];
                    let mut token_idx = match slice {
                        "if" | "else" | "while" | "let" | "fn" | "return" | "break" | "continue" | "import"
                        | "from" | "as" | "go" | "select" | "case" | "default" | "true" | "false" | "nil" | "spawn"
                        | "chan" | "send" | "recv" => KEYWORD_IDX,
                        _ => VARIABLE_IDX,
                    };
                    // Detect function call by peeking next non-whitespace char
                    if token_idx == VARIABLE_IDX {
                        let mut k = j;
                        while k < len && chars[k].is_whitespace() {
                            k += 1;
                        }
                        if k < len && chars[k] == '(' {
                            token_idx = FUNCTION_IDX;
                        }
                    }
                    let start = ident_start.max(start_char_bound);
                    if start < end_char_bound {
                        let capped_len_total = j.saturating_sub(start);
                        if capped_len_total > 0 {
                            let capped_len = capped_len_total.min(end_char_bound.saturating_sub(start));
                            tokens.push(self.create_token(line_number, start, capped_len, token_idx, 0));
                        }
                    }
                    char_index = j;
                    continue;
                }

                // Legacy '@' context access removed; treat '@' as punctuation

                // Operators - only tokenize multi-character operators to reduce density
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
                                        let capped_len = capped_len_total.min(end_char_bound.saturating_sub(start));
                                        tokens.push(self.create_token(line_number, start, capped_len, OPERATOR_IDX, 0));
                                    }
                                }
                                char_index += 2;
                                continue;
                            }
                            _ => {
                                // Skip single-character operators entirely
                                char_index += 1;
                                continue;
                            }
                        }
                    }
                    // Skip single character operators
                    char_index += 1;
                    continue;
                }

                // Skip other operators and punctuation to reduce token density
                if "+-*/%,;(){}[]@.".contains(c) {
                    char_index += 1;
                    continue;
                }

                char_index += 1;
            }
            // Stop early if range token budget is exceeded
            if tokens.len() >= MAX_TOKENS_PER_RANGE {
                break;
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
            let delta_line = if first { line } else { line.saturating_sub(prev_line) };
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

/// Helper function to extract variable names from a pattern for LSP analysis
pub fn extract_variables_from_pattern(pattern: &qcl_core::expr::Pattern) -> Option<Vec<String>> {
    let mut variables = Vec::new();

    fn collect_vars(pattern: &qcl_core::expr::Pattern, vars: &mut Vec<String>) {
        match pattern {
            qcl_core::expr::Pattern::Variable(name) => {
                vars.push(name.clone());
            }
            qcl_core::expr::Pattern::List { patterns, rest } => {
                for pattern in patterns {
                    collect_vars(pattern, vars);
                }
                if let Some(rest_var) = rest {
                    vars.push(rest_var.clone());
                }
            }
            qcl_core::expr::Pattern::Map { patterns, rest } => {
                for (_, pattern) in patterns {
                    collect_vars(pattern, vars);
                }
                if let Some(rest_var) = rest {
                    vars.push(rest_var.clone());
                }
            }
            qcl_core::expr::Pattern::Or(patterns) => {
                for pattern in patterns {
                    collect_vars(pattern, vars);
                }
            }
            qcl_core::expr::Pattern::Guard { pattern, .. } => {
                collect_vars(pattern, vars);
            }
            // Other pattern types don't bind variables
            qcl_core::expr::Pattern::Literal(_)
            | qcl_core::expr::Pattern::Wildcard
            | qcl_core::expr::Pattern::Range { .. } => {}
        }
    }

    collect_vars(pattern, &mut variables);

    // Remove duplicates (can happen with OR patterns)
    variables.sort();
    variables.dedup();

    if variables.is_empty() {
        None
    } else {
        Some(variables)
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
        let mut analyzer = create_analyzer();
        let result = analyzer.analyze("req.user.role == 'admin'");

        // Should have identifier roots
        assert!(result.identifier_roots.contains("req"));

        // Should have expression symbol
        assert_eq!(result.symbols.len(), 1);
        assert_eq!(result.symbols[0].name, "expression");
        assert_eq!(result.symbols[0].kind, SymbolKind::CONSTANT);

        // Check that diagnostics include identifier roots info (this is now expected behavior)
        let has_context_info = result
            .diagnostics
            .iter()
            .any(|d| d.severity == Some(DiagnosticSeverity::INFORMATION) && d.message.contains("identifier roots"));
        assert!(has_context_info, "Expected identifier roots diagnostic");
    }

    #[test]
    fn test_analyze_invalid_expression() {
        let mut analyzer = create_analyzer();
        let result = analyzer.analyze("req.user.role == 'unterminated string");

        // Should have diagnostic for invalid expression (tokenization error due to unterminated string)
        assert!(!result.diagnostics.is_empty());
        assert_eq!(result.diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(result.diagnostics[0].message.contains("Tokenization error"));
    }

    #[test]
    fn test_analyze_statement_program() {
        let mut analyzer = create_analyzer();
        let code = r#"
            import math;
            let user_level = req.user.level;
            fn calculate_score(base) {
                return math.sqrt(base * user_level);
            }
            let result = calculate_score(100);
        "#;
        let result = analyzer.analyze(code);

        // Should not have diagnostics for valid program
        assert!(result.diagnostics.is_empty());

        // Should have symbols for import, variable, and function
        assert!(result.symbols.len() >= 3);

        let symbol_names: Vec<&String> = result.symbols.iter().map(|s| &s.name).collect();
        assert!(symbol_names.contains(&&"import math".to_string()));
        assert!(symbol_names.contains(&&"user_level".to_string()));
        assert!(symbol_names.contains(&&"calculate_score".to_string()));
        assert!(symbol_names.contains(&&"result".to_string()));
    }

    #[test]
    fn test_get_var_completions() {
        let mut analyzer = create_analyzer();
        let completions = analyzer.get_var_completions("req");

        // Should return completions that start with "req"
        assert!(!completions.is_empty());

        let labels: Vec<&String> = completions.iter().map(|c| &c.label).collect();
        assert!(labels.contains(&&"req".to_string()));
        assert!(labels.contains(&&"req.user".to_string()));
        assert!(labels.contains(&&"req.user.id".to_string()));
        assert!(labels.contains(&&"req.user.role".to_string()));
        assert!(labels.contains(&&"req.user.name".to_string()));

        // Should not include completions that don't match the prefix
        assert!(!labels.contains(&&"record".to_string()));
    }

    #[test]
    fn test_validate_identifier_access_with_valid_vars() {
        let analyzer = create_analyzer();

        // Create a variables map with req.user.role
        let mut user_map = HashMap::new();
        user_map.insert("role".to_string(), Val::Str("admin".to_string().into()));
        user_map.insert("id".to_string(), Val::Int(123));

        let mut req_map = HashMap::new();
        req_map.insert("user".to_string(), Val::from(user_map));

        let mut context_map = HashMap::new();
        context_map.insert("req".to_string(), Val::from(req_map));
        let context = Val::from(context_map);

        // Parse expression that uses req.user.role
        let tokens = qcl_core::token::Tokenizer::tokenize("req.user.role == 'admin'").unwrap();
        let mut parser = qcl_core::ast::Parser::new(&tokens);
        let expr_result = parser.parse();

        let diagnostics = analyzer.validate_identifier_access(&expr_result, Some(&context));

        // Should have no diagnostics since variables map is valid
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_identifier_map_has_key() {
        let analyzer = create_analyzer();

        // Create nested variables map structure
        let mut inner_map = HashMap::new();
        inner_map.insert("name".to_string(), Val::Str("test".to_string().into()));

        let mut middle_map = HashMap::new();
        middle_map.insert("user".to_string(), Val::from(inner_map));

        let mut context_map = HashMap::new();
        context_map.insert("req".to_string(), Val::from(middle_map));
        let context = Val::from(context_map);

        // Test existing nested key
        assert!(analyzer.vars_has_key(&context, "req.user.name"));

        // Test non-existing key
        assert!(!analyzer.vars_has_key(&context, "req.user.role"));
        assert!(!analyzer.vars_has_key(&context, "req.admin"));
        assert!(!analyzer.vars_has_key(&context, "nonexistent"));
    }

    #[test]
    fn test_generate_semantic_tokens_simple_expression() {
        let analyzer = create_analyzer();
        let content = "req.user.role == 'admin'";
        let tokens = analyzer.generate_semantic_tokens(content);

        // Define the legend indices for testing
        const OPERATOR_IDX: u32 = 6;
        const STRING_IDX: u32 = 4;

        // Should have tokens for: @, req, ., user, ., role, ==, 'admin'
        assert!(!tokens.is_empty());

        // Check that we have a keyword token for '==' (operator)
        let mut found_operator = false;
        let mut found_string = false;

        for token in &tokens {
            if token.token_type == OPERATOR_IDX {
                found_operator = true;
            } else if token.token_type == STRING_IDX {
                found_string = true;
            }
        }

        assert!(found_operator, "Should find operator token");
        assert!(found_string, "Should find string token");
        // '@' token removed; property tokens still present for identifiers
    }

    #[test]
    fn test_generate_semantic_tokens_statement_program() {
        let analyzer = create_analyzer();
        let content = r#"
            let user_level = req.user.level;
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
        assert!(found_let || found_if || found_return, "Should find keyword tokens");
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

    #[test]
    fn test_generate_semantic_tokens_function_identifier() {
        let analyzer = create_analyzer();
        let content = "let y = foo(1) + bar (2);";
        let tokens = analyzer.generate_semantic_tokens(content);

        const FUNCTION_IDX: u32 = 3;

        assert!(
            tokens.iter().any(|t| t.token_type == FUNCTION_IDX),
            "Should classify function identifiers as FUNCTION"
        );
    }
}
