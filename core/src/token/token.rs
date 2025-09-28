use std::fmt::Debug;

use anyhow::{Result, anyhow};
use crate::token::{ParseError, Position, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Dot,       // .
    OptionalDot, // ?.
    Colon,     // :
    Comma,     // ,
    Semicolon, // ;
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    ModAssign, // %=
    Nil,       // nil
    Eq,        // ==
    Ne,        // !=
    Gt,        // >
    Lt,        // <
    Ge,        // >=
    Le,        // <=
    In,        // in
    And,       // &&
    Or,        // ||
    Not,       // !
    Add,       // +
    Sub,       // -
    Mul,       // *
    Div,       // /
    Mod,       // %
    At,        // @
    // Statement keywords
    If,       // if
    Else,     // else
    While,    // while
    Let,      // let
    Break,    // break
    Continue, // continue
    Return,   // return
    Fn,       // fn (function definition)
    For,      // for (for loop)
    Range,    // .. (range operator)
    RangeInclusive, // ..= (inclusive range operator)
    // Concurrency keywords
    Spawn,     // spawn
    Chan,      // chan
    Send,      // send
    Recv,      // recv
    Select,    // select
    Case,      // case
    Default,   // default
    Arrow,         // =>
    LeftArrow,     // <-
    NullishCoalescing, // ??
    TemplateString(String),   // Formatted string content with ${...}
    // Import keywords
    Import,      // import
    From,        // from
    As,          // as
    // Type system keywords
    Type,        // type (for type aliases)
    Trait,       // trait
    Impl,        // impl
    // Type operators
    Pipe,        // | (for union types)
    Question,    // ? (for optional types)
    FnArrow,     // -> (for function types)
    Str(String), // "abc"
    Int(i64),    // 1
    Float(f64),  // 1.1
    Bool(bool),  // true, false
    Id(String),  // identifier
}

/// [chars] and [idx] can be used for syntax error reporting.
pub struct Tokenizer {
    chars: Vec<char>,
    idx: usize,
    len: usize,
    pub tokens: Vec<Token>,
    pub token_spans: Vec<Span>,
    line: u32,
    column: u32,
    input: String,
}

impl Tokenizer {
    pub fn tokenize(s: &str) -> Result<Vec<Token>> {
        let mut t = Tokenizer {
            chars: s.chars().collect(),
            idx: 0,
            len: s.chars().count(), // More accurate than s.len() for Unicode
            tokens: Vec::with_capacity(s.len() / 4), // Preallocate a reasonable size
            token_spans: Vec::with_capacity(s.len() / 4),
            line: 1,
            column: 1,
            input: s.to_string(),
        };
        t.parse()?;
        Ok(t.tokens)
    }

    /// Tokenize with enhanced error information (line/column span) for LSP
    pub fn tokenize_enhanced(s: &str) -> std::result::Result<Vec<Token>, ParseError> {
        let mut t = Tokenizer::new_enhanced(s);
        match t.parse() {
            Ok(()) => Ok(t.tokens),
            Err(err) => {
                // Attach precise position to the error using the tokenizer's current cursor
                Err(t.enhanced_error(&format!("{}", err)))
            }
        }
    }

    /// Tokenize and return tokens with precise spans aligned by index
    pub fn tokenize_enhanced_with_spans(
        s: &str,
    ) -> std::result::Result<(Vec<Token>, Vec<Span>), ParseError> {
        let mut t = Tokenizer::new_enhanced(s);
        match t.parse() {
            Ok(()) => Ok((t.tokens, t.token_spans)),
            Err(err) => Err(t.enhanced_error(&format!("{}", err))),
        }
    }

    /// Get enhanced error message with position information for LSP
    pub fn enhanced_error(&self, msg: &str) -> ParseError {
        let position = Position::new(self.line, self.column, self.idx);
        ParseError::with_position(msg.to_string(), position)
    }

    /// Create a tokenizer with enhanced error reporting
    pub fn new_enhanced(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            idx: 0,
            len: input.chars().count(),
            tokens: Vec::with_capacity(input.len() / 4),
            token_spans: Vec::with_capacity(input.len() / 4),
            line: 1,
            column: 1,
            input: input.to_string(),
        }
    }

    /// Get current position
    pub fn current_position(&self) -> Position {
        Position::new(self.line, self.column, self.idx)
    }

    fn eof(&self) -> bool {
        self.idx >= self.len
    }

    fn expect(&mut self, s: &str) -> bool {
        let start_idx = self.idx;
        let start_line = self.line;
        let start_column = self.column;

        for c in s.chars() {
            if self.idx >= self.len || self.chars[self.idx] != c {
                // Reset position if match failed
                self.idx = start_idx;
                self.line = start_line;
                self.column = start_column;
                return false;
            }
            self.advance_char();
        }
        true
    }

    fn err<T: AsRef<str>>(&self, msg: T) -> String {
        // Collect near 10(max) chars around the error position
        let r_idx = if self.idx + 5 < self.len {
            self.idx + 5
        } else {
            self.len
        };
        let l_idx = self.idx.saturating_sub(5);
        let r_idx = if r_idx > self.len { self.len } else { r_idx };
        let chars = &self.chars[l_idx..r_idx];
        let chars: String = chars.iter().collect();
        let c = self.chars.get(self.idx);
        let ctx = if let Some(&c) = c {
            format!("'{}' at index {}, near '{}'", c, self.idx, chars)
        } else {
            format!("at end, near '{}'", chars)
        };

        // Use the stored input for better context if needed
        let line_context = self.get_line_context();
        format!(
            "Syntax error:\n{} ({})\nLine {}: {}",
            msg.as_ref(),
            ctx,
            self.line,
            line_context
        )
    }

    /// Get the current line from input for error context
    fn get_line_context(&self) -> String {
        let lines: Vec<&str> = self.input.lines().collect();
        if self.line > 0 && (self.line as usize) <= lines.len() {
            lines[self.line as usize - 1].to_string()
        } else {
            "".to_string()
        }
    }

    fn advance_char(&mut self) {
        if !self.eof() && self.chars[self.idx] == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.idx += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.idx < self.len && self.chars[self.idx].is_whitespace() {
            self.advance_char();
        }
    }

    fn skip_line_comment(&mut self) -> Result<()> {
        // Skip to end of line
        while !self.eof() {
            let c = self.chars[self.idx];
            if c == '\n' {
                self.advance_char();
                break;
            }
            self.advance_char();
        }
        Ok(())
    }

    fn skip_block_comment(&mut self) -> Result<()> {
        // Skip past /*
        self.advance_char();
        self.advance_char();

        while !self.eof() {
            let c = self.chars[self.idx];
            if c == '*' && self.idx + 1 < self.len && self.chars[self.idx + 1] == '/' {
                self.advance_char();
                self.advance_char();
                return Ok(());
            }
            self.advance_char();
        }

        Err(anyhow!(self.err("Block comment not closed")))
    }

    fn parse_str(&mut self) -> Result<()> {
        // Supports formatted strings inside '"' or '\'' using ${...}
        let mut content = String::new();
        let start_pos = self.current_position();
        let quote = self.chars[self.idx];
        self.advance_char(); // skip opening quote

        let mut in_expr = false;
        let mut brace_depth = 0;
        let mut is_template = false;

        while !self.eof() {
            let c = self.chars[self.idx];

            if !in_expr && c == quote {
                // End of string
                self.advance_char(); // skip closing quote
                let end_pos = self.current_position();
                if is_template {
                    self.push_with_span(Token::TemplateString(content), start_pos, end_pos);
                } else {
                    self.push_with_span(Token::Str(content), start_pos, end_pos);
                }
                return Ok(());
            } else if !in_expr
                && c == '$'
                && self.idx + 1 < self.len
                && self.chars[self.idx + 1] == '{'
            {
                // Start of interpolation: ${...}
                is_template = true;
                content.push_str("${");
                self.advance_char(); // skip '$'
                self.advance_char(); // skip '{'
                in_expr = true;
                brace_depth = 1;
            } else if in_expr {
                // Collect expression content with brace balancing
                content.push(c);
                if c == '{' {
                    brace_depth += 1;
                } else if c == '}' {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        in_expr = false;
                    }
                }
                self.advance_char();
            } else if c == '\\' && self.idx + 1 < self.len {
                // Handle escape sequences
                self.advance_char(); // skip backslash
                if !self.eof() {
                    let escaped_char = self.chars[self.idx];
                    match escaped_char {
                        'n' => content.push('\n'),
                        'r' => content.push('\r'),
                        't' => content.push('\t'),
                        '\\' => content.push('\\'),
                        '\'' => content.push('\''),
                        '"' => content.push('"'),
                        '$' => content.push('$'),
                        '0' => content.push('\0'),
                        _ => {
                            // For unknown escape sequences, keep the backslash and the character
                            content.push('\\');
                            content.push(escaped_char);
                        }
                    }
                    self.advance_char();
                } else {
                    return Err(anyhow!(self.err("Incomplete escape sequence at end of string")));
                }
            } else {
                content.push(c);
                self.advance_char();
            }
        }

        Err(anyhow!(self.err("String not closed")))
    }

    /// Parse Rust-style raw string literals: r"...", r#"..."#, r##"..."##, ...
    /// - Supports multi-line
    /// - No escapes or interpolation; contents are verbatim
    /// Attempts to parse at current 'r'; if pattern doesn't match, restores cursor and returns Err.
    fn parse_raw_str(&mut self) -> Result<()> {
        let start_pos = self.current_position();

        // Save to restore if not a raw string
        let save_idx = self.idx;
        let save_line = self.line;
        let save_col = self.column;

        if self.eof() || self.chars[self.idx] != 'r' {
            return Err(anyhow!(self.err("Expect 'r' for raw string")));
        }

        // Count hashes after 'r'
        let mut i = self.idx + 1;
        let mut hashes: usize = 0;
        while i < self.len && self.chars[i] == '#' {
            hashes += 1;
            i += 1;
        }
        if i >= self.len || self.chars[i] != '"' {
            // Not a valid raw string start; restore and signal
            self.idx = save_idx;
            self.line = save_line;
            self.column = save_col;
            return Err(anyhow!("not a raw string start"));
        }

        // Consume r + #* + opening quote
        self.advance_char(); // 'r'
        for _ in 0..hashes { self.advance_char(); }
        self.advance_char(); // '"'

        let mut content = String::new();
        while !self.eof() {
            let c = self.chars[self.idx];
            if c == '"' {
                // Check for closing delimiter '"' followed by exactly `hashes` '#'
                let mut j = self.idx + 1;
                let mut k = 0usize;
                while k < hashes && j < self.len && self.chars[j] == '#' {
                    k += 1;
                    j += 1;
                }
                if k == hashes {
                    // Consume closing
                    self.advance_char(); // '"'
                    for _ in 0..hashes { self.advance_char(); }
                    let end_pos = self.current_position();
                    self.push_with_span(Token::Str(content), start_pos, end_pos);
                    return Ok(());
                }
                // Not a terminator; include '"'
                content.push('"');
                self.advance_char();
            } else {
                content.push(c);
                self.advance_char();
            }
        }

        Err(anyhow!(self.err("Raw string not closed")))
    }

    // Note: backtick-delimited template strings are not supported anymore.

    /// eg.:
    /// - @a -> [At, Id("a")]
    /// - @a.b -> [At, Id("a"), Dot, Id("b")]
    /// - @a.0.1 -> [At, Id("a"), Dot, Int(0), Dot, Int(1)]
    fn parse_num(&mut self) -> Result<()> {
        let mut num = String::new();
        let start_pos = self.current_position();
        let mut dot_count = 0;
        let mut has_exp = false;
        
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_ascii_digit() {
                num.push(c);
                self.advance_char();
            } else if c == '.' {
                // Check if this is part of a range operator (..)
                if self.idx + 1 < self.len && self.chars[self.idx + 1] == '.' {
                    // This is a range operator, don't include the dot in the number
                    break;
                }
                if dot_count > 0 {
                    return Err(anyhow!(self.err("Invalid float, multiple '.'")));
                }
                if has_exp {
                    return Err(anyhow!(self.err("Invalid float, '.' after exponent")));
                }
                num.push(c);
                self.advance_char();
                dot_count += 1;
            } else if (c == 'e' || c == 'E') && !num.is_empty() && !has_exp {
                // Scientific notation exponent
                num.push(c);
                self.advance_char();
                has_exp = true;
                
                // Check for optional sign after 'e'/'E'
                if !self.eof() {
                    let next_c = self.chars[self.idx];
                    if next_c == '+' || next_c == '-' {
                        num.push(next_c);
                        self.advance_char();
                    }
                }
            } else if (c == '-' || c == '+') && num.is_empty() {
                // Leading sign
                num.push(c);
                self.advance_char();
            } else {
                break;
            }
        }

        if num.ends_with('.') {
            return Err(anyhow!(self.err("Invalid float, ends with '.'")));
        }
        if num.ends_with('e') || num.ends_with('E') || num.ends_with('+') || num.ends_with('-') {
            return Err(anyhow!(self.err("Invalid number, incomplete exponent")));
        }

        let num = if num.contains('.') || num.contains('e') || num.contains('E') {
            match num.parse() {
                Ok(f) => Token::Float(f),
                Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid float"), num)),
            }
        } else {
            match num.parse() {
                Ok(i) => Token::Int(i),
                Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid int"), num)),
            }
        };
        let end_pos = self.current_position();
        self.push_with_span(num, start_pos, end_pos);
        Ok(())
    }

    fn parse_id(&mut self) -> Result<()> {
        let mut id = String::new();
        let start_pos = self.current_position();
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_alphanumeric() || c == '_' || c == '-' {
                id.push(c);
                self.advance_char();
            } else {
                break;
            }
        }
        // Guard against empty identifiers (e.g., unknown unicode punctuation like '；').
        // If we didn't consume any character, report an unknown character to avoid
        // non-advancing loops that can blow up memory.
        if id.is_empty() {
            return Err(anyhow!(self.err("Invalid identifier start or unknown character")));
        }
        let end_pos = self.current_position();
        self.push_with_span(Token::Id(id), start_pos, end_pos);
        Ok(())
    }

    fn parse_keywords(&mut self) -> Result<()> {
        fn match_kw(t: &mut Tokenizer, kw: &str) -> Option<Span> {
            let start = t.current_position();
            if t.expect(kw) {
                // Check if the next character is part of an identifier
                // If so, this is not a keyword but part of an identifier
                if !t.eof() {
                    let next_char = t.chars[t.idx];
                    if next_char.is_alphanumeric() || next_char == '_' || next_char == '-' {
                        // Reset position since this is not a standalone keyword
                        t.idx = start.offset;
                        t.line = start.line;
                        t.column = start.column;
                        return None;
                    }
                }
                let end = t.current_position();
                Some(Span::new(start, end))
            } else {
                None
            }
        }

        if let Some(sp) = match_kw(self, "true") {
            self.push_span_only(Token::Bool(true), sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "false") {
            self.push_span_only(Token::Bool(false), sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "nil") {
            self.push_span_only(Token::Nil, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "in") {
            self.push_span_only(Token::In, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "if") {
            self.push_span_only(Token::If, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "else") {
            self.push_span_only(Token::Else, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "while") {
            self.push_span_only(Token::While, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "let") {
            self.push_span_only(Token::Let, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "break") {
            self.push_span_only(Token::Break, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "continue") {
            self.push_span_only(Token::Continue, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "return") {
            self.push_span_only(Token::Return, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "fn") {
            self.push_span_only(Token::Fn, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "import") {
            self.push_span_only(Token::Import, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "from") {
            self.push_span_only(Token::From, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "as") {
            self.push_span_only(Token::As, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "for") {
            self.push_span_only(Token::For, sp);
            return Ok(());
        }
        // Concurrency keywords
        if let Some(sp) = match_kw(self, "spawn") {
            self.push_span_only(Token::Spawn, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "chan") {
            self.push_span_only(Token::Chan, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "send") {
            self.push_span_only(Token::Send, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "recv") {
            self.push_span_only(Token::Recv, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "select") {
            self.push_span_only(Token::Select, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "case") {
            self.push_span_only(Token::Case, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "default") {
            self.push_span_only(Token::Default, sp);
            return Ok(());
        }
        // Type system keywords
        if let Some(sp) = match_kw(self, "type") {
            self.push_span_only(Token::Type, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "trait") {
            self.push_span_only(Token::Trait, sp);
            return Ok(());
        }
        if let Some(sp) = match_kw(self, "impl") {
            self.push_span_only(Token::Impl, sp);
            return Ok(());
        }

        self.parse_id()
    }

    /// - `@a.(@b - 1)` -> [At, Id("a"), Dot, LParen, At, Id("b"), Sub, Int(1), RParen]
    /// - `@a` -> [At, Id("a")]
    fn parse_at_list(&mut self) -> Result<()> {
        let at_start = self.current_position();
        if self.expect("@") {
            let end = self.current_position();
            self.push_with_span(Token::At, at_start, end);
        } else {
            return Err(anyhow!(self.err("Expect '@'")));
        }

        while !self.eof() {
            let c = self.chars[self.idx];
            let is_field = c.is_alphanumeric() || c == '_' || c == '-';
            let is_num = c.is_ascii_digit();
            if is_field && !is_num {
                self.parse_id()?;
                continue;
            }
            if is_num {
                self.parse_int()?;
                continue;
            }

            let dot_start = self.current_position();
            if self.expect(".") {
                let end = self.current_position();
                self.push_with_span(Token::Dot, dot_start, end);
                continue;
            }
            break;
        }
        Ok(())
    }

    fn parse_int(&mut self) -> Result<()> {
        // Record span for integers parsed in contexts like @a.0 or .123
        let start_pos = self.current_position();
        let mut num = String::new();
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_ascii_digit() {
                num.push(c);
                self.advance_char();
            } else {
                break;
            }
        }
        let parsed = match num.parse() {
            Ok(i) => Token::Int(i),
            Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid int"), num)),
        };
        let end_pos = self.current_position();
        self.push_with_span(parsed, start_pos, end_pos);
        Ok(())
    }

    fn parse_punctuations(&mut self) -> Result<()> {
        let c = self.chars[self.idx];
            match c {
                '(' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::LParen, start, end);
                Ok(())
            }
            ')' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::RParen, start, end);
                Ok(())
            }
            '{' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::LBrace, start, end);
                Ok(())
            }
            '}' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::RBrace, start, end);
                Ok(())
            }
            '[' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::LBracket, start, end);
                Ok(())
            }
            ']' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::RBracket, start, end);
                Ok(())
            }
            ':' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::Colon, start, end);
                Ok(())
            }
            ',' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::Comma, start, end);
                Ok(())
            }
            ';' => {
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::Semicolon, start, end);
                Ok(())
            }
            '.' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&'.') = next {
                    // Check for ..= or ..
                    let third = self.chars.get(self.idx + 2);
                    if let Some(&'=') = third {
                        // Inclusive range operator ..=
                        let start = self.current_position();
                        self.advance_char(); // consume first .
                        self.advance_char(); // consume second .
                        self.advance_char(); // consume =
                        let end = self.current_position();
                        self.push_with_span(Token::RangeInclusive, start, end);
                        return Ok(());
                    } else {
                        // Range operator ..
                        let start = self.current_position();
                        self.advance_char(); // consume first .
                        self.advance_char(); // consume second .
                        let end = self.current_position();
                        self.push_with_span(Token::Range, start, end);
                        return Ok(());
                    }
                }
                if let Some(&c) = next
                    && c.is_ascii_digit()
                {
                    let start = self.current_position();
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Dot, start, end);
                    // To avoid confusion with Dot in float, only parse int here
                    return self.parse_int();
                }
                let start = self.current_position();
                self.advance_char();
                let end = self.current_position();
                self.push_with_span(Token::Dot, start, end);
                Ok(())
            }
            '?' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&'.') = next {
                    // Optional chaining operator ?.
                    let start = self.current_position();
                    self.advance_char(); // consume ?
                    self.advance_char(); // consume .
                    let end = self.current_position();
                    self.push_with_span(Token::OptionalDot, start, end);
                    Ok(())
                } else if let Some(&'?') = next {
                    // Nullish coalescing operator ??
                    let start = self.current_position();
                    self.advance_char(); // consume first ?
                    self.advance_char(); // consume second ?
                    let end = self.current_position();
                    self.push_with_span(Token::NullishCoalescing, start, end);
                    Ok(())
                } else {
                    // Single ? for optional types
                    let start = self.current_position();
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Question, start, end);
                    Ok(())
                }
            }
            '&' => {
                let start = self.current_position();
                if self.expect("&&") {
                    let end = self.current_position();
                    self.push_with_span(Token::And, start, end);
                    Ok(())
                } else {
                    Err(anyhow!(self.err("Expect '&&'")))
                }
            }
            '|' => {
                let start = self.current_position();
                self.advance_char(); // Consume the first |

                // Check if this is || (could be logical OR or empty-closure)
                if self.idx < self.len && self.chars[self.idx] == '|' {
                    self.advance_char(); // Consume the second |
                    let end = self.current_position();

                    // Disambiguate by looking behind at the previous non-whitespace char.
                    // If the previous significant char indicates we're in the middle of an
                    // expression (identifier, literal, closing bracket/paren/brace), treat as OR.
                    // If we're at expression start or after a delimiter like '=', '(', '{', ',', ';',
                    // treat as an empty-parameter closure "|| expr".
                    let mut prev_idx = start.offset.saturating_sub(1);
                    while prev_idx > 0 && self.chars[prev_idx].is_whitespace() {
                        prev_idx = prev_idx.saturating_sub(1);
                    }
                    let prev_char = if start.offset == 0 {
                        None
                    } else {
                        Some(self.chars[prev_idx])
                    };

                    let is_after_expr = matches!(
                        prev_char,
                        Some(')' | ']' | '}' | '"' | '\'' | '`')
                    ) || matches!(prev_char, Some(c) if c.is_alphanumeric());

                    let is_after_delim = matches!(
                        prev_char,
                        None | Some('=' | '(' | '{' | ',' | ';' | ':')
                    );

                    if is_after_delim && !is_after_expr {
                        // Empty-parameter closure context: emit two Pipe tokens with spans
                        let mid_pos = Position::new(start.line, start.column + 1, start.offset + 1);
                        self.push_with_span(Token::Pipe, start, mid_pos.clone());
                        self.push_with_span(Token::Pipe, mid_pos, end);
                    } else {
                        // Logical OR
                        self.push_with_span(Token::Or, start, end);
                    }
                } else {
                    // Single | for union types or closure start
                    let end = self.current_position();
                    self.push_with_span(Token::Pipe, start, end);
                }
                Ok(())
            }
            '+' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next
                    && c.is_ascii_digit()
                {
                    return self.parse_num();
                }
                let start = self.current_position();
                if self.expect("+=") {
                    let end = self.current_position();
                    self.push_with_span(Token::AddAssign, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Add, start, end);
                    Ok(())
                }
            }
            '-' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next
                    && c.is_ascii_digit()
                {
                    return self.parse_num();
                }
                let start = self.current_position();
                if self.expect("->") {
                    // Function type arrow
                    let end = self.current_position();
                    self.push_with_span(Token::FnArrow, start, end);
                    Ok(())
                } else if self.expect("-=") {
                    let end = self.current_position();
                    self.push_with_span(Token::SubAssign, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Sub, start, end);
                    Ok(())
                }
            }
            '*' => {
                let start = self.current_position();
                if self.expect("*=") {
                    let end = self.current_position();
                    self.push_with_span(Token::MulAssign, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Mul, start, end);
                    Ok(())
                }
            }
            '/' => {
                if self.expect("//") {
                    // Skip single-line comment
                    self.skip_line_comment()?;
                } else if self.expect("/*") {
                    // Skip block comment
                    self.skip_block_comment()?;
                } else {
                    let start = self.current_position();
                    if self.expect("/=") {
                        let end = self.current_position();
                        self.push_with_span(Token::DivAssign, start, end);
                    } else {
                        self.advance_char();
                        let end = self.current_position();
                        self.push_with_span(Token::Div, start, end);
                    }
                }
                Ok(())
            }
            '%' => {
                let start = self.current_position();
                if self.expect("%=") {
                    let end = self.current_position();
                    self.push_with_span(Token::ModAssign, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Mod, start, end);
                    Ok(())
                }
            }
            '@' => self.parse_at_list(),
            '=' => {
                let start = self.current_position();
                if self.expect("==") {
                    let end = self.current_position();
                    self.push_with_span(Token::Eq, start, end);
                    Ok(())
                } else if self.expect("=>") {
                    let end = self.current_position();
                    self.push_with_span(Token::Arrow, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Assign, start, end);
                    Ok(())
                }
            }
            '!' => {
                let start = self.current_position();
                if self.expect("!=") {
                    let end = self.current_position();
                    self.push_with_span(Token::Ne, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Not, start, end);
                    Ok(())
                }
            }
            '>' => {
                let start = self.current_position();
                if self.expect(">=") {
                    let end = self.current_position();
                    self.push_with_span(Token::Ge, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Gt, start, end);
                    Ok(())
                }
            }
            '<' => {
                let start = self.current_position();
                if self.expect("<=") {
                    let end = self.current_position();
                    self.push_with_span(Token::Le, start, end);
                    Ok(())
                } else if self.expect("<-") {
                    let end = self.current_position();
                    self.push_with_span(Token::LeftArrow, start, end);
                    Ok(())
                } else {
                    self.advance_char();
                    let end = self.current_position();
                    self.push_with_span(Token::Lt, start, end);
                    Ok(())
                }
            }
            _ => Err(anyhow!(self.err("Unknown punctuation"))),
        }
    }

    fn parse(&mut self) -> Result<()> {
        while !self.eof() {
            self.skip_whitespace();
            if self.eof() {
                break;
            }
            let c = self.chars[self.idx];
            match c {
                '"' | '\'' => {
                    self.parse_str()?;
                }
                // Try Rust-style raw strings when encountering 'r'
                'r' => {
                    if let Err(_) = self.parse_raw_str() {
                        // Fallback to keywords/identifiers starting with 'r'
                        self.parse_keywords()?;
                    }
                }
                '0'..='9' => {
                    self.parse_num()?;
                }
                // Keywords: true false nil if else while let break continue return goto fn for as ...
                // Also: go, select/case/default
                // NOTE: include starting letters for all keywords so they route to parse_keywords.
                't' | 'f' | 'n' | 'i' | 'e' | 'w' | 'l' | 'b' | 'c' | 'g' | 's' | 'd' | 'a' => {
                    self.parse_keywords()?;
                }
                _ => {
                    if self.is_punctuation(c) {
                        self.parse_punctuations()?;
                    } else {
                        self.parse_id()?;
                    }
                }
            }
        }
        Ok(())
    }

    fn is_punctuation(&self, c: char) -> bool {
        matches!(
            c,
            '(' | ')'
                | '{'
                | '}'
                | '['
                | ']'
                | '.'
                | '?'
                | ':'
                | ','
                | ';'
                | '&'
                | '|'
                | '+'
                | '-'
                | '*'
                | '/'
                | '%'
                | '@'
                | '='
                | '!'
                | '>'
                | '<'
        )
    }
}

impl Tokenizer {
    fn push_with_span(
        &mut self,
        token: Token,
        start: Position,
        end: Position,
    ) {
        self.tokens.push(token);
        self.token_spans.push(Span::new(start, end));
    }

    fn push_span_only(&mut self, token: Token, span: Span) {
        self.tokens.push(token);
        self.token_spans.push(span);
    }
}
