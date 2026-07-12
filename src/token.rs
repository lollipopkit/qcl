use alloc::{format, string::String, sync::Arc, vec::Vec};
use core::fmt::Debug;

use crate::error::{Error, Result};

/// Cap on up-front token-vector allocation. `s.len() / 4` is a fine estimate for
/// normal expressions, but a huge (possibly untrusted) input would otherwise
/// force a multi-hundred-MB reservation before a single token is produced. The
/// vector still grows on demand past this for genuinely large token streams.
const MAX_TOKEN_PREALLOC: usize = 4096;

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

/// Byte-oriented tokenizer. `input`/`idx` index UTF-8 bytes directly, avoiding a
/// full `Vec<char>` copy of the source; identifiers and string literals without
/// escapes are sliced straight out of `input` with no intermediate allocation.
/// `idx` is always kept on a UTF-8 char boundary.
pub struct Tokenizer<'a> {
    input: &'a str,
    bytes: &'a [u8],
    idx: usize,
    len: usize,
    pub tokens: Vec<Token>,
}

impl<'a> Tokenizer<'a> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(s: &str) -> Result<Vec<Token>> {
        let mut t = Tokenizer {
            input: s,
            bytes: s.as_bytes(),
            idx: 0,
            len: s.len(),
            tokens: Vec::with_capacity((s.len() / 4).min(MAX_TOKEN_PREALLOC)),
        };
        t.parse()?;
        Ok(t.tokens)
    }

    #[inline]
    fn eof(&self) -> bool {
        self.idx >= self.len
    }

    /// Current byte. Only call when `!self.eof()`.
    #[inline]
    fn cur_byte(&self) -> u8 {
        self.bytes[self.idx]
    }

    #[inline]
    fn byte_at(&self, i: usize) -> Option<u8> {
        self.bytes.get(i).copied()
    }

    /// Decode the char starting at `byte_idx` (must be a char boundary).
    #[inline]
    fn char_at(&self, byte_idx: usize) -> Option<char> {
        self.input.get(byte_idx..).and_then(|s| s.chars().next())
    }

    #[inline]
    fn cur_char(&self) -> Option<char> {
        self.char_at(self.idx)
    }

    /// The char immediately after the current one. All call sites have an ASCII
    /// (single-byte) current char, so `idx + 1` is a valid boundary there.
    #[inline]
    fn peek1(&self) -> Option<char> {
        self.char_at(self.idx + 1)
    }

    fn err<T: AsRef<str>>(&self, msg: T) -> String {
        // Compute line:col from idx by scanning from start
        let mut line = 1usize;
        let mut col = 1usize;
        let scanned = self.idx.min(self.len);
        for c in self.input[..scanned].chars() {
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        // Collect near ~10 chars around the error position (char-boundary safe)
        let l_idx = self.char_window_start(5);
        let r_idx = self.char_window_end(5);
        let near = &self.input[l_idx..r_idx];
        let c = self.cur_char();
        let ctx = if let Some(c) = c {
            format!("'{}' at {}:{}, near '{}'", c, line, col, near)
        } else {
            format!("at end ({}:{}), near '{}'", line, col, near)
        };
        format!("Syntax error:\n{} ({})", msg.as_ref(), ctx)
    }

    /// Byte offset ~`n` chars before `idx`, clamped to a char boundary.
    fn char_window_start(&self, n: usize) -> usize {
        let mut idx = self.idx.min(self.len);
        for _ in 0..n {
            if idx == 0 {
                break;
            }
            idx -= 1;
            while idx > 0 && !self.input.is_char_boundary(idx) {
                idx -= 1;
            }
        }
        idx
    }

    /// Byte offset ~`n` chars after `idx`, clamped to a char boundary.
    fn char_window_end(&self, n: usize) -> usize {
        let mut idx = self.idx.min(self.len);
        for _ in 0..n {
            if idx >= self.len {
                break;
            }
            idx += 1;
            while idx < self.len && !self.input.is_char_boundary(idx) {
                idx += 1;
            }
        }
        idx
    }

    fn skip_whitespace(&mut self) {
        while !self.eof() {
            let c = self.cur_char().unwrap();
            if c.is_whitespace() {
                self.idx += c.len_utf8();
            } else {
                break;
            }
        }
    }

    fn is_id_start(c: char) -> bool {
        if c.is_ascii() {
            c.is_ascii_alphabetic() || c == '_'
        } else {
            c.is_alphabetic() || c == '_'
        }
    }

    fn is_id_continue(c: char) -> bool {
        if c.is_ascii() {
            c.is_ascii_alphanumeric() || c == '_' || c == '-'
        } else {
            c.is_alphanumeric() || c == '_' || c == '-'
        }
    }

    fn parse_str(&mut self) -> Result<()> {
        let mut s = String::new();
        let quote = self.cur_char().unwrap(); // ' or " (ASCII)
        self.idx += 1;

        while !self.eof() {
            let c = self.cur_char().unwrap();
            match c {
                '\\' => {
                    self.idx += 1;
                    if self.eof() {
                        return Err(Error::Tokenize(self.err("Invalid escape sequence")));
                    }

                    let next = self.cur_char().unwrap();
                    let escaped = match next {
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
                            // Validate on raw bytes first so the &str slice below is
                            // guaranteed to land on a char boundary.
                            let hb = &self.bytes[self.idx..self.idx + 4];
                            if !hb.iter().all(|b| b.is_ascii_hexdigit()) {
                                let shown = core::str::from_utf8(hb).unwrap_or("????");
                                return Err(Error::Tokenize(self.err(format!("Invalid unicode escape: \\u{shown}"))));
                            }
                            let hex = &self.input[self.idx..self.idx + 4];
                            let code = u32::from_str_radix(hex, 16)
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
                    self.idx += 1; // escape letter is ASCII
                }
                c if c == quote => {
                    self.idx += 1;
                    self.tokens.push(Token::Str(Arc::<str>::from(s)));
                    return Ok(());
                }
                _ => {
                    s.push(c);
                    self.idx += c.len_utf8();
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
        if !self.eof() && matches!(self.cur_byte(), b'-' | b'+') {
            self.idx += 1;
        }

        // Check for hex (0x) or octal (0o) prefix
        if !self.eof()
            && self.cur_byte() == b'0'
            && self
                .byte_at(self.idx + 1)
                .is_some_and(|b| matches!(b, b'x' | b'X' | b'o' | b'O'))
        {
            self.idx = start_idx;
            return self.parse_int();
        }

        // Reset idx to after sign (or start) for normal decimal parsing
        self.idx = start_idx;
        let mut dot_count = 0;
        while !self.eof() {
            let b = self.cur_byte();
            if b.is_ascii_digit() {
                self.idx += 1;
            } else if b == b'.' {
                if dot_count > 0 {
                    return Err(Error::Tokenize(self.err("Invalid float, multiple '.'")));
                }
                self.idx += 1;
                dot_count += 1;
            } else if matches!(b, b'-' | b'+') && self.idx == start_idx {
                self.idx += 1;
            } else {
                break;
            }
        }

        if self.idx > start_idx && self.bytes[self.idx - 1] == b'.' {
            return Err(Error::Tokenize(self.err("Invalid float, ends with '.'")));
        }

        let num_str = &self.input[start_idx..self.idx];

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
        if self.eof() || !self.cur_char().is_some_and(Self::is_id_start) {
            return Err(Error::Tokenize(self.err("Invalid identifier start")));
        }

        let start_idx = self.idx;
        self.idx = self.scan_id_end(start_idx);
        self.push_id_token(start_idx, self.idx);
        Ok(())
    }

    fn scan_id_end(&self, start_idx: usize) -> usize {
        // Caller guarantees the char at start_idx is an id-start.
        let first = self.char_at(start_idx).unwrap();
        let mut end = start_idx + first.len_utf8();
        while end < self.len {
            let c = self.char_at(end).unwrap();
            if Self::is_id_continue(c) {
                end += c.len_utf8();
            } else {
                break;
            }
        }
        end
    }

    fn push_id_token(&mut self, start_idx: usize, end_idx: usize) {
        let id = &self.input[start_idx..end_idx];
        self.tokens.push(Token::Id(Arc::<str>::from(id)));
    }

    fn parse_ident_or_keyword(&mut self) -> Result<()> {
        if self.eof() || !self.cur_char().is_some_and(Self::is_id_start) {
            return Err(Error::Tokenize(self.err("Invalid identifier start")));
        }

        let start_idx = self.idx;
        let end_idx = self.scan_id_end(start_idx);
        self.idx = end_idx;

        let token = match &self.input[start_idx..end_idx] {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "nil" => Token::Nil,
            "in" => Token::In,
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
        self.idx += 1; // '@' is ASCII
        self.tokens.push(Token::At);

        while !self.eof() {
            let c = self.cur_char().unwrap();
            if Self::is_id_start(c) {
                self.parse_id()?;
                continue;
            }
            if c.is_ascii_digit() {
                self.parse_int()?;
                continue;
            }
            if matches!(c, '+' | '-')
                && self.tokens.last().is_some_and(|tok| tok == &Token::Dot)
                && self.peek1().is_some_and(|next| next.is_ascii_digit())
            {
                self.parse_int()?;
                continue;
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

        if !self.eof() && matches!(self.cur_byte(), b'-' | b'+') {
            self.idx += 1;
        }

        if !self.eof()
            && self.cur_byte() == b'0'
            && self
                .byte_at(self.idx + 1)
                .is_some_and(|b| matches!(b, b'x' | b'X' | b'o' | b'O'))
        {
            let is_hex = matches!(self.byte_at(self.idx + 1), Some(b'x') | Some(b'X'));
            let radix = if is_hex { 16 } else { 8 };
            self.idx += 2; // skip '0x' or '0o'
            let digits_start = self.idx;
            while !self.eof() {
                let b = self.cur_byte();
                let valid = if is_hex {
                    b.is_ascii_hexdigit()
                } else {
                    matches!(b, b'0'..=b'7')
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
            let digits = &self.input[digits_start..self.idx];
            let val = i64::from_str_radix(digits, radix).map_err(|_| {
                Error::Tokenize(self.err(format!("Invalid int: 0{}{}", if is_hex { "x" } else { "o" }, digits)))
            })?;
            let val = if self.byte_at(start_idx) == Some(b'-') {
                val.checked_neg()
                    .ok_or_else(|| Error::Tokenize(self.err("Integer overflow")))?
            } else {
                val
            };
            self.tokens.push(Token::Int(val));
            return Ok(());
        }

        while !self.eof() {
            let b = self.cur_byte();
            if b.is_ascii_digit() {
                self.idx += 1;
            } else {
                break;
            }
        }

        let num_str = &self.input[start_idx..self.idx];
        let num = match num_str.parse() {
            Ok(i) => i,
            Err(_) => return Err(Error::Tokenize(format!("{}: {}", self.err("Invalid int"), num_str))),
        };
        self.tokens.push(Token::Int(num));
        Ok(())
    }

    fn parse_punctuations(&mut self) -> Result<()> {
        let c = self.cur_char().unwrap();
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
                if self.peek1() == Some('&') {
                    self.idx += 2;
                    self.tokens.push(Token::And);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '&&'")))
                }
            }
            '|' => {
                if self.peek1() == Some('|') {
                    self.idx += 2;
                    self.tokens.push(Token::Or);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '||'")))
                }
            }
            '+' => {
                if self.sign_starts_number() && self.peek1().is_some_and(|next| next.is_ascii_digit()) {
                    return self.parse_num();
                }
                self.idx += 1;
                self.tokens.push(Token::Add);
                Ok(())
            }
            '-' => {
                if self.sign_starts_number() && self.peek1().is_some_and(|next| next.is_ascii_digit()) {
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
                if self.peek1() == Some('*') {
                    // Multi-line comment with nesting support
                    self.idx += 2;
                    let mut depth = 1usize;
                    while !self.eof() && depth > 0 {
                        if self.cur_byte() == b'/' && self.peek1() == Some('*') {
                            depth += 1;
                            self.idx += 2;
                        } else if self.cur_byte() == b'*' && self.peek1() == Some('/') {
                            depth -= 1;
                            self.idx += 2;
                        } else {
                            // Comment body may hold multibyte UTF-8; advance a full char.
                            let ch = self.cur_char().unwrap();
                            self.idx += ch.len_utf8();
                        }
                    }
                    if depth > 0 {
                        return Err(Error::Tokenize(self.err("Unterminated block comment")));
                    }
                } else if self.peek1() == Some('/') {
                    self.idx += 2;
                    // Skip single-line comment
                    while !self.eof() {
                        if self.cur_byte() == b'\n' {
                            self.idx += 1;
                            break;
                        }
                        let ch = self.cur_char().unwrap();
                        self.idx += ch.len_utf8();
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
                if self.peek1() == Some('=') {
                    self.idx += 2;
                    self.tokens.push(Token::Eq);
                    Ok(())
                } else {
                    Err(Error::Tokenize(self.err("Expect '=='")))
                }
            }
            '!' => {
                if self.peek1() == Some('=') {
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
                if self.peek1() == Some('=') {
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
                if self.peek1() == Some('=') {
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
                if self.peek1() == Some('?') {
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
            let c = self.cur_char().unwrap();
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

        let b = self.cur_byte();
        if b.is_ascii_digit() {
            return true;
        }

        if matches!(b, b'+' | b'-') {
            return self.peek1().is_some_and(|next| next.is_ascii_digit());
        }

        false
    }
}
