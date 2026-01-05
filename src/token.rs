use std::fmt::Debug;

use anyhow::{Result, anyhow};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    LBracket,    // [
    RBracket,    // ]
    Dot,         // .
    Colon,       // :
    Comma,       // ,
    Semicolon,   // ;
    Nil,         // nil
    Eq,          // ==
    Ne,          // !=
    Gt,          // >
    Lt,          // <
    Ge,          // >=
    Le,          // <=
    In,          // in
    And,         // &&
    Or,          // ||
    Not,         // !
    Add,         // +
    Sub,         // -
    Mul,         // *
    Div,         // /
    Mod,         // %
    At,          // @
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
}

impl Tokenizer {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(s: &str) -> Result<Vec<Token>> {
        let chars: Vec<char> = s.chars().collect();
        let len = chars.len();
        let mut t = Tokenizer {
            chars,
            idx: 0,
            len,                                     // More accurate than s.len() for Unicode
            tokens: Vec::with_capacity(s.len() / 4), // Preallocate a reasonable size
        };
        t.parse()?;
        Ok(t.tokens)
    }

    fn eof(&self) -> bool {
        self.idx >= self.len
    }

    fn expect(&mut self, s: &str) -> bool {
        let mut idx = 0;
        for c in s.chars() {
            let self_idx = self.idx + idx;
            if self_idx >= self.len {
                return false;
            }
            if self.chars[self_idx] != c {
                return false;
            }
            idx += 1;
        }
        self.idx += idx;
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
        format!("Syntax error:\n{} ({})", msg.as_ref(), ctx)
    }

    fn skip_whitespace(&mut self) {
        while self.idx < self.len && self.chars[self.idx].is_whitespace() {
            self.idx += 1;
        }
    }

    fn parse_str(&mut self) -> Result<()> {
        let mut s = String::new();
        let quote = self.chars[self.idx];
        self.idx += 1;

        // Find the end quote position first to optimize allocation
        let mut end_idx = self.idx;
        let mut found = false;

        while end_idx < self.len {
            if self.chars[end_idx] == quote {
                found = true;
                break;
            }
            end_idx += 1;
        }

        if !found {
            return Err(anyhow!(self.err("String not closed")));
        }

        // Now extract the string content all at once
        s.extend(self.chars[self.idx..end_idx].iter());
        self.idx = end_idx + 1; // Skip past the closing quote

        self.tokens.push(Token::Str(s));
        Ok(())
    }

    /// eg.:
    /// - @a -> [At, Id("a")]
    /// - @a.b -> [At, Id("a"), Dot, Id("b")]
    /// - @a.0.1 -> [At, Id("a"), Dot, Int(0), Dot, Int(1)]
    fn parse_num(&mut self) -> Result<()> {
        let start_idx = self.idx;
        let mut dot_count = 0;
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_ascii_digit() {
                self.idx += 1;
            } else if c == '.' {
                if dot_count > 0 {
                    return Err(anyhow!(self.err("Invalid float, multiple '.'")));
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
            return Err(anyhow!(self.err("Invalid float, ends with '.'")));
        }

        let num_str: String = self.chars[start_idx..self.idx].iter().collect();

        let num = if dot_count > 0 {
            match num_str.parse() {
                Ok(f) => Token::Float(f),
                Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid float"), num_str)),
            }
        } else {
            match num_str.parse() {
                Ok(i) => Token::Int(i),
                Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid int"), num_str)),
            }
        };
        self.tokens.push(num);
        Ok(())
    }

    fn parse_id(&mut self) -> Result<()> {
        let start_idx = self.idx;
        while !self.eof() {
            let c = self.chars[self.idx];
            if !(c.is_alphanumeric() || c == '_' || c == '-') {
                break;
            }
            self.idx += 1;
        }
        let id: String = self.chars[start_idx..self.idx].iter().collect();
        self.tokens.push(Token::Id(id));
        Ok(())
    }

    fn parse_keywords(&mut self) -> Result<()> {
        if self.expect("true") {
            self.tokens.push(Token::Bool(true));
            Ok(())
        } else if self.expect("false") {
            self.tokens.push(Token::Bool(false));
            Ok(())
        } else if self.expect("nil") {
            self.tokens.push(Token::Nil);
            Ok(())
        } else if self.expect("in") {
            self.tokens.push(Token::In);
            Ok(())
        } else {
            self.parse_id()
        }
    }

    /// - `@a.(@b - 1)` -> [At, Id("a"), Dot, LParen, At, Id("b"), Sub, Int(1), RParen]
    /// - `@a` -> [At, Id("a")]
    fn parse_at_list(&mut self) -> Result<()> {
        if self.expect("@") {
            self.tokens.push(Token::At);
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

            if self.expect(".") {
                self.tokens.push(Token::Dot);
                continue;
            }
            break;
        }
        Ok(())
    }

    fn parse_int(&mut self) -> Result<()> {
        let start_idx = self.idx;
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
            Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid int"), num_str)),
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
                if let Some(&c) = self.chars.get(self.idx + 1)
                    && c.is_ascii_digit()
                {
                    self.idx += 1;
                    self.tokens.push(Token::Dot);
                    // To avoid confusion with Dot in float, only parse int here
                    return self.parse_int();
                }
                self.idx += 1;
                self.tokens.push(Token::Dot);
                Ok(())
            }
            '&' => {
                if self.expect("&&") {
                    self.tokens.push(Token::And);
                    Ok(())
                } else {
                    Err(anyhow!(self.err("Expect '&&'")))
                }
            }
            '|' => {
                if self.expect("||") {
                    self.tokens.push(Token::Or);
                    Ok(())
                } else {
                    Err(anyhow!(self.err("Expect '||'")))
                }
            }
            '+' => {
                if let Some(&c) = self.chars.get(self.idx + 1)
                    && c.is_ascii_digit()
                {
                    return self.parse_num();
                }
                self.idx += 1;
                self.tokens.push(Token::Add);
                Ok(())
            }
            '-' => {
                if let Some(&c) = self.chars.get(self.idx + 1)
                    && c.is_ascii_digit()
                {
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
                if self.expect("//") {
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
                if self.expect("==") {
                    self.tokens.push(Token::Eq);
                    Ok(())
                } else {
                    Err(anyhow!(self.err("Expect '=='")))
                }
            }
            '!' => {
                if self.expect("!=") {
                    self.tokens.push(Token::Ne);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Not);
                    Ok(())
                }
            }
            '>' => {
                if self.expect(">=") {
                    self.tokens.push(Token::Ge);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Gt);
                    Ok(())
                }
            }
            '<' => {
                if self.expect("<=") {
                    self.tokens.push(Token::Le);
                    Ok(())
                } else {
                    self.idx += 1;
                    self.tokens.push(Token::Lt);
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
                '0'..='9' => {
                    self.parse_num()?;
                }
                // true false nil in
                't' | 'f' | 'n' | 'i' => {
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
