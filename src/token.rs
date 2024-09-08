use std::fmt::Debug;

use anyhow::{anyhow, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,      // (
    RParen,      // )
    Dot,         // .
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
    pub fn new(s: &str) -> Result<Vec<Token>> {
        let tokens = Vec::new();
        let mut t = Tokenizer {
            chars: s.chars().collect(),
            idx: 0,
            len: s.len(),
            tokens,
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
        let l_idx = if self.idx > 5 { self.idx - 5 } else { 0 };
        let r_idx = if r_idx > self.len { self.len } else { r_idx };
        let chars = &self.chars[l_idx..r_idx];
        let chars: String = chars.iter().collect();
        let c = self.chars.get(self.idx);
        let ctx = if let Some(&c) = c {
            format!("'{}' at index {}, near '{}'", c, self.idx, chars)
        } else {
            format!("at end, near '{}'", chars)
        };
        format!(
            "Syntax error:\n{} ({})\nParsed tokens: {:?}",
            msg.as_ref(),
            ctx,
            &self.tokens
        )
    }

    fn skip_whitespace(&mut self) {
        while !self.eof() {
            if !self.chars[self.idx].is_whitespace() {
                break;
            }
            self.idx += 1;
        }
    }

    fn parse_str(&mut self) -> Result<()> {
        let mut s = String::new();
        let quote = self.chars[self.idx];
        self.idx += 1;
        let mut end = false;
        while !self.eof() {
            let c = self.chars[self.idx];
            if c == quote {
                self.idx += 1;
                end = true;
                break;
            }
            s.push(c);
            self.idx += 1;
        }
        if !end {
            return Err(anyhow!(self.err("String not closed")));
        }
        self.tokens.push(Token::Str(s));
        Ok(())
    }

    /// eg.:
    /// - @a -> [At, Id("a")]
    /// - @a.b -> [At, Id("a"), Dot, Id("b")]
    /// - @a.0.1 -> [At, Id("a"), Dot, Int(0), Dot, Int(1)]
    ///
    /// TODO: fix the bug that `@a.0` is parsed as `At, Id("a"), Float(0)`
    fn parse_num(&mut self) -> Result<()> {
        let mut num = String::new();
        let mut dot_count = 0;
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_digit(10) {
                num.push(c);
                self.idx += 1;
            } else if c == '.' {
                if dot_count > 0 {
                    return Err(anyhow!(self.err("Invalid float")));
                }
                num.push(c);
                self.idx += 1;
                dot_count += 1;
            } else if (c == '-' || c == '+') && num.is_empty() {
                num.push(c);
                self.idx += 1;
            } else {
                break;
            }
        }
        let num = if num.contains('.') {
            match num.parse() {
                Ok(f) => Token::Float(f),
                Err(_) => return Err(anyhow!(self.err("Invalid float"))),
            }
        } else {
            match num.parse() {
                Ok(i) => Token::Int(i),
                Err(_) => return Err(anyhow!(self.err("Invalid int"))),
            }
        };
        self.tokens.push(num);
        Ok(())
    }

    fn parse_id(&mut self) -> Result<()> {
        let mut id = String::new();
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_alphanumeric() || c == '_' || c == '-' {
                id.push(c);
                self.idx += 1;
            } else {
                break;
            }
        }
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

    fn parse_at_list(&mut self) -> Result<()> {
        if self.expect("@") {
            self.tokens.push(Token::At);
        } else {
            return Err(anyhow!(self.err("Expect '@'")));
        }

        let tokens_len = self.tokens.len();
        while !self.eof() {
            let c = self.chars[self.idx];
            let tokens_added = self.tokens.len() > tokens_len;
            let is_field = c.is_alphanumeric() || c == '_' || c == '-';
            if !tokens_added && !is_field {
                return Err(anyhow!(self.err("Expect field after '@'")));
            }
            if c == '.' {
                self.idx += 1;
                self.tokens.push(Token::Dot);
                continue;
            }
            let is_num = c.is_digit(10);
            if is_field && !is_num {
                self.parse_id()?;
                continue;
            }
            if is_num {
                self.parse_int()?
            } else if c.is_whitespace() {
                break;
            }
        }
        Ok(())
    }

    fn parse_int(&mut self) -> Result<()> {
        let mut num = String::new();
        while !self.eof() {
            let c = self.chars[self.idx];
            if c.is_digit(10) {
                num.push(c);
                self.idx += 1;
            } else {
                break;
            }
        }
        let num = match num.parse() {
            Ok(i) => i,
            Err(_) => return Err(anyhow!("{}: {}", self.err("Invalid int"), num)),
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
            '.' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next {
                    if c.is_digit(10) {
                        return self.parse_num();
                    }
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
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next {
                    if c.is_digit(10) {
                        return self.parse_num();
                    }
                }
                self.idx += 1;
                self.tokens.push(Token::Add);
                Ok(())
            }
            '-' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next {
                    if c.is_digit(10) {
                        return self.parse_num();
                    }
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
                self.idx += 1;
                self.tokens.push(Token::Div);
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
            _ => self.parse_id(),
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
                    self.parse_punctuations()?;
                }
            }
        }
        Ok(())
    }
}
