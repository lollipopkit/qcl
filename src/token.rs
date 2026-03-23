use alloc::{format, string::String, sync::Arc, vec::Vec};
use core::fmt::Debug;

use crate::error::{Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,           // (
    RParen,           // )
    LBrace,           // {
    RBrace,           // }
    LBracket,         // [
    RBracket,         // ]
    Dot,              // .
    Colon,            // :
    Comma,            // ,
    Semicolon,        // ;
    Nil,              // nil
    Eq,               // ==
    Ne,               // !=
    Gt,               // >
    Lt,               // <
    Ge,               // >=
    Le,               // <=
    In,               // in
    And,              // &&
    Or,               // ||
    Not,              // !
    Add,              // +
    Sub,              // -
    Mul,              // *
    Div,              // /
    Mod,              // %
    At,               // @
    Question,         // ?
    QuestionQuestion, // ??
    Str(Arc<str>),    // "abc"
    Int(i64),         // 1
    Float(f64),       // 1.1
    Bool(bool),       // true, false
    Id(Arc<str>),     // identifier
}

/// [chars] and [idx] can be used for syntax error reporting.
pub struct Tokenizer {
    chars: Vec<char>,
    idx: usize,
    len: usize,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(s: &str) -> Result<Vec<Token>> {
        let chars: Vec<char> = s.chars().collect();
        let len = chars.len();
        let mut t = Tokenizer {
            chars,
            idx: 0,
            len,
            tokens: Vec::with_capacity(s.len() / 4), // Preallocate a reasonable size
        };
        t.parse()?;
        Ok(t.tokens)
    }

    fn eof(&self) -> bool {
        self.idx >= self.len
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.chars.get(self.idx + offset).copied()
    }

    fn err<T: AsRef<str>>(&self, msg: T) -> String {
        // Compute line:col from idx by scanning from start
        let mut line = 1usize;
        let mut col = 1usize;
        for i in 0..self.idx.min(self.len) {
            if self.chars[i] == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
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
            format!("'{}' at {}:{}, near '{}'", c, line, col, chars)
        } else {
            format!("at end ({}:{}), near '{}'", line, col, chars)
        };
        format!("Syntax error:\n{} ({})", msg.as_ref(), ctx)
    }

    fn skip_whitespace(&mut self) {
        while self.idx < self.len && self.chars[self.idx].is_whitespace() {
            self.idx += 1;
        }
    }

    fn is_id_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_id_continue(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '-'
    }

    fn parse_str(&mut self) -> Result<()> {
        let mut s = String::new();
        let quote = self.chars[self.idx];
        self.idx += 1;

        while !self.eof() {
            let c = self.chars[self.idx];
            match c {
                '\\' => {
                    self.idx += 1;
                    if self.eof() {
                        return Err(Error::Tokenize(self.err("Invalid escape sequence")));
                    }

                    let escaped = match self.chars[self.idx] {
                        '\\' => '\\',
                        '"' => '"',
                        '\'' => '\'',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        'u' => {
                            self.idx += 1;
                            if self.idx + 4 > self.len {
                                return Err(Error::Tokenize(self.err("Invalid \\uXXXX escape, need 4 hex digits")));
                            }
                            let hex: String = self.chars[self.idx..self.idx + 4].iter().collect();
                            let code = u32::from_str_radix(&hex, 16)
                                .map_err(|_| Error::Tokenize(self.err(format!("Invalid unicode escape: \\u{hex}"))))?;
                            let ch = char::from_u32(code).ok_or_else(|| {
                                Error::Tokenize(self.err(format!("Invalid unicode codepoint: \\u{hex}")))
                            })?;
                            self.idx += 4;
                            s.push(ch);
                            continue;
                        }
                        other => {
                            return Err(Error::Tokenize(
                                self.err(format!("Unsupported escape sequence: \\{other}")),
                            ));
                        }
                    };
                    s.push(escaped);
                    self.idx += 1;
                }
                c if c == quote => {
                    self.idx += 1;
                    self.tokens.push(Token::Str(Arc::<str>::from(s)));
                    return Ok(());
                }
                _ => {
                    s.push(c);
                    self.idx += 1;
                }
            }
        }

        Err(Error::Tokenize(self.err("String not closed")))
    }

    /// Check whether a sign (+/-) should be treated as the start of a numeric literal
    /// rather than a binary operator. A sign is a prefix when there is no preceding
    /// value-producing token.
    fn sign_starts_number(&self) -> bool {
        match self.tokens.last() {
            None => true, // beginning of input
            Some(tok) => !matches!(
                tok,
                Token::Int(_)
                    | Token::Float(_)
                    | Token::Str(_)
                    | Token::Bool(_)
                    | Token::Nil
                    | Token::Id(_)
                    | Token::RParen
                    | Token::RBracket
                    | Token::RBrace
            ),
        }
    }

    /// eg.:
    /// - @a -> [At, Id("a")]
    /// - @a.b -> [At, Id("a"), Dot, Id("b")]
    /// - @a.0.1 -> [At, Id("a"), Dot, Int(0), Dot, Int(1)]
    fn parse_num(&mut self) -> Result<()> {
        let start_idx = self.idx;

        // Handle optional sign prefix
        if !self.eof() && (self.chars[self.idx] == '-' || self.chars[self.idx] == '+') {
            self.idx += 1;
        }

        // Check for hex (0x) or octal (0o) prefix
        if !self.eof()
            && self.chars[self.idx] == '0'
            && self.idx + 1 < self.len
            && (self.chars[self.idx + 1] == 'x'
                || self.chars[self.idx + 1] == 'X'
                || self.chars[self.idx + 1] == 'o'
                || self.chars[self.idx + 1] == 'O')
        {
            self.idx = start_idx;
            return self.parse_int();
        }

        // Reset idx to after sign (or start) for normal decimal parsing
        self.idx = start_idx;
        let mut dot_count = 0;
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_ascii_digit() {
                self.idx += 1;
            } else if c == '.' {
                if dot_count > 0 {
                    return Err(Error::Tokenize(self.err("Invalid float, multiple '.'")));
                }
                self.idx += 1;
                dot_count += 1;
            } else if (c == '-' || c == '+') && self.idx == start_idx {
                self.idx += 1;
            } else {
                break;
            }
        }

        if self.idx > start_idx && self.chars[self.idx - 1] == '.' {
            return Err(Error::Tokenize(self.err("Invalid float, ends with '.'")));
        }

        let num_str: String = self.chars[start_idx..self.idx].iter().collect();

        let num = if dot_count > 0 {
            match num_str.parse() {
                Ok(f) => Token::Float(f),
                Err(_) => return Err(Error::Tokenize(format!("{}: {}", self.err("Invalid float"), num_str))),
            }
        } else {
            match num_str.parse() {
                Ok(i) => Token::Int(i),
                Err(_) => return Err(Error::Tokenize(format!("{}: {}", self.err("Invalid int"), num_str))),
            }
        };
        self.tokens.push(num);
        Ok(())
    }

    fn parse_id(&mut self) -> Result<()> {
        if self.eof() || !Self::is_id_start(self.chars[self.idx]) {
            return Err(Error::Tokenize(self.err("Invalid identifier start")));
        }

        let start_idx = self.idx;
        self.idx = self.scan_id_end(start_idx);
        self.push_id_token(start_idx, self.idx);
        Ok(())
    }

    fn scan_id_end(&self, start_idx: usize) -> usize {
        let mut end = start_idx + 1;
        while end < self.len && Self::is_id_continue(self.chars[end]) {
            end += 1;
        }
        end
    }

    fn push_id_token(&mut self, start_idx: usize, end_idx: usize) {
        let id: String = self.chars[start_idx..end_idx].iter().collect();
        self.tokens.push(Token::Id(Arc::<str>::from(id)));
    }

    fn parse_ident_or_keyword(&mut self) -> Result<()> {
        if self.eof() || !Self::is_id_start(self.chars[self.idx]) {
            return Err(Error::Tokenize(self.err("Invalid identifier start")));
        }

        let start_idx = self.idx;
        let end_idx = self.scan_id_end(start_idx);
        self.idx = end_idx;

        let token = match &self.chars[start_idx..end_idx] {
            ['t', 'r', 'u', 'e'] => Token::Bool(true),
            ['f', 'a', 'l', 's', 'e'] => Token::Bool(false),
            ['n', 'i', 'l'] => Token::Nil,
            ['i', 'n'] => Token::In,
            _ => {
                self.push_id_token(start_idx, end_idx);
                return Ok(());
            }
        };
        self.tokens.push(token);
        Ok(())
    }

    /// - `@a.(@b - 1)` -> [At, Id("a"), Dot, LParen, At, Id("b"), Sub, Int(1), RParen]
    /// - `@a` -> [At, Id("a")]
    fn parse_at_list(&mut self) -> Result<()> {
        self.idx += 1;
        self.tokens.push(Token::At);

        while !self.eof() {
            let c = self.chars[self.idx];
            if Self::is_id_start(c) {
                self.parse_id()?;
                continue;
            }
            if c.is_ascii_digit() {
                self.parse_int()?;
                continue;
            }
            if matches!(c, '+' | '-') && self.tokens.last().is_some_and(|tok| tok == &Token::Dot) {
                if self.peek(1).is_some_and(|next| next.is_ascii_digit()) {
                    self.parse_int()?;
                    continue;
                }
            }

            if c == '.' {
                self.idx += 1;
                self.tokens.push(Token::Dot);
                continue;
            }
            break;
        }
        Ok(())
    }

    fn parse_int(&mut self) -> Result<()> {
        let start_idx = self.idx;

        if !self.eof() && (self.chars[self.idx] == '-' || self.chars[self.idx] == '+') {
            self.idx += 1;
        }

        if !self.eof()
            && self.chars[self.idx] == '0'
            && self.idx + 1 < self.len
            && (self.chars[self.idx + 1] == 'x'
                || self.chars[self.idx + 1] == 'X'
                || self.chars[self.idx + 1] == 'o'
                || self.chars[self.idx + 1] == 'O')
        {
            let is_hex = self.chars[self.idx + 1] == 'x' || self.chars[self.idx + 1] == 'X';
            let radix = if is_hex { 16 } else { 8 };
            self.idx += 2; // skip '0x' or '0o'
            let digits_start = self.idx;
            while !self.eof() {
                let c = self.chars[self.idx];
                let valid = if is_hex {
                    c.is_ascii_hexdigit()
                } else {
                    matches!(c, '0'..='7')
                };
                if valid {
                    self.idx += 1;
                } else {
                    break;
                }
            }
            if self.idx == digits_start {
                let label = if is_hex { "hex" } else { "octal" };
                return Err(Error::Tokenize(self.err(format!("Invalid {label} literal, no digits"))));
            }
            let digits: String = self.chars[digits_start..self.idx].iter().collect();
            let val = i64::from_str_radix(&digits, radix).map_err(|_| {
                Error::Tokenize(self.err(format!("Invalid int: 0{}{}", if is_hex { "x" } else { "o" }, digits)))
            })?;
            let val = if start_idx < self.chars.len() && self.chars[start_idx] == '-' {
                val.checked_neg()
                    .ok_or_else(|| Error::Tokenize(self.err("Integer overflow")))?
            } else {
                val
            };
            self.tokens.push(Token::Int(val));
            return Ok(());
        }

        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_ascii_digit() {
                self.idx += 1;
            } else {
                break;
            }
        }

        let num_str: String = self.chars[start_idx..self.idx].iter().collect();
        let num = match num_str.parse() {
            Ok(i) => i,
            Err(_) => return Err(Error::Tokenize(format!("{}: {}", self.err("Invalid int"), num_str))),
        };
        self.tokens.push(Token::Int(num));
        Ok(())
    }

    fn parse_punctuations(&mut self) -> Result<()> {
        let c = self.chars[self.idx];
        match c {
            '(' => {
                self.idx += 1;
                self.tokens.push(Token::LParen);
                Ok(())
            }
            ')' => {
                self.idx += 1;
                self.tokens.push(Token::RParen);
                Ok(())
            }
            '{' => {
                self.idx += 1;
                self.tokens.push(Token::LBrace);
                Ok(())
            }
            '}' => {
                self.idx += 1;
                self.tokens.push(Token::RBrace);
                Ok(())
            }
            '[' => {
                self.idx += 1;
                self.tokens.push(Token::LBracket);
                Ok(())
            }
            ']' => {
                self.idx += 1;
                self.tokens.push(Token::RBracket);
                Ok(())
            }
            ':' => {
                self.idx += 1;
                self.tokens.push(Token::Colon);
                Ok(())
            }
            ',' => {
                self.idx += 1;
                self.tokens.push(Token::Comma);
                Ok(())
            }
            ';' => {
                self.idx += 1;
                self.tokens.push(Token::Semicolon);
                Ok(())
            }
            '.' => {
                self.idx += 1;
                self.tokens.push(Token::Dot);
                if self.starts_int_literal() {
                    return self.parse_int();
                }
                Ok(())
            }
            '&' => {
                if self.peek(1) == Some('&') {
                    self.idx += 2;
                    self.tokens.push(Token::And);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '&&'")))
                }
            }
            '|' => {
                if self.peek(1) == Some('|') {
                    self.idx += 2;
                    self.tokens.push(Token::Or);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '||'")))
                }
            }
            '+' => {
                if self.sign_starts_number() && self.peek(1).is_some_and(|next| next.is_ascii_digit()) {
                    return self.parse_num();
                }
                self.idx += 1;
                self.tokens.push(Token::Add);
                Ok(())
            }
            '-' => {
                if self.sign_starts_number() && self.peek(1).is_some_and(|next| next.is_ascii_digit()) {
                    return self.parse_num();
                }
                self.idx += 1;
                self.tokens.push(Token::Sub);
                Ok(())
            }
            '*' => {
                self.idx += 1;
                self.tokens.push(Token::Mul);
                Ok(())
            }
            '/' => {
                if self.peek(1) == Some('*') {
                    // Multi-line comment with nesting support
                    self.idx += 2;
                    let mut depth = 1usize;
                    while !self.eof() && depth > 0 {
                        if self.chars[self.idx] == '/' && self.peek(1) == Some('*') {
                            depth += 1;
                            self.idx += 2;
                        } else if self.chars[self.idx] == '*' && self.peek(1) == Some('/') {
                            depth -= 1;
                            self.idx += 2;
                        } else {
                            self.idx += 1;
                        }
                    }
                    if depth > 0 {
                        return Err(Error::Tokenize(self.err("Unterminated block comment")));
                    }
                } else if self.peek(1) == Some('/') {
                    self.idx += 2;
                    // Skip single-line comment
                    while !self.eof() {
                        let c = self.chars[self.idx];
                        if c == '\n' {
                            self.idx += 1;
                            break;
                        }
                        self.idx += 1;
                    }
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Div);
                }
                Ok(())
            }
            '%' => {
                self.idx += 1;
                self.tokens.push(Token::Mod);
                Ok(())
            }
            '@' => self.parse_at_list(),
            '=' => {
                if self.peek(1) == Some('=') {
                    self.idx += 2;
                    self.tokens.push(Token::Eq);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '=='")))
                }
            }
            '!' => {
                if self.peek(1) == Some('=') {
                    self.idx += 2;
                    self.tokens.push(Token::Ne);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Not);
                    Ok(())
                }
            }
            '>' => {
                if self.peek(1) == Some('=') {
                    self.idx += 2;
                    self.tokens.push(Token::Ge);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Gt);
                    Ok(())
                }
            }
            '<' => {
                if self.peek(1) == Some('=') {
                    self.idx += 2;
                    self.tokens.push(Token::Le);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Lt);
                    Ok(())
                }
            }
            '?' => {
                if self.peek(1) == Some('?') {
                    self.idx += 2;
                    self.tokens.push(Token::QuestionQuestion);
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Question);
                }
                Ok(())
            }
            _ => Err(Error::Tokenize(self.err("Unknown punctuation"))),
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
                '0'..='9' => {
                    self.parse_num()?;
                }
                _ => {
                    if self.is_punctuation(c) {
                        self.parse_punctuations()?;
                    } else if Self::is_id_start(c) {
                        self.parse_ident_or_keyword()?;
                    } else {
                        return Err(Error::Tokenize(self.err("Invalid identifier start")));
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
                | '?'
        )
    }

    fn starts_int_literal(&self) -> bool {
        if self.eof() {
            return false;
        }

        let c = self.chars[self.idx];
        if c.is_ascii_digit() {
            return true;
        }

        if matches!(c, '+' | '-') {
            return self.peek(1).is_some_and(|next| next.is_ascii_digit());
        }

        false
    }
}
