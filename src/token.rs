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

impl Token {
    pub fn same_type(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn has_value(&self) -> bool {
        match self {
            Token::Str(_) | Token::Int(_) | Token::Float(_) | Token::Bool(_) | Token::Id(_) => true,
            _ => false,
        }
    }
}

/// [chars] and [idx] can be used for syntax error reporting.
pub struct Tokenizer {
    chars: Vec<char>,
    idx: usize,
    len: usize,
}

impl Tokenizer {
    pub fn new(s: &str) -> Tokenizer {
        Tokenizer {
            chars: s.chars().collect(),
            idx: 0,
            len: s.len(),
        }
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

    fn err<T: AsRef<str> + Debug>(&self, msg: T) -> String {
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
        format!("Syntax error: {:?} ({})", msg, ctx)
    }

    fn skip_whitespace(&mut self) {
        while !self.eof() {
            if !self.chars[self.idx].is_whitespace() {
                break;
            }
            self.idx += 1;
        }
    }

    fn parse_str(&mut self) -> Result<Token> {
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
        Ok(Token::Str(s))
    }

    fn parse_num(&mut self) -> Result<Token> {
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
        Ok(num)
    }

    fn parse_id(&mut self) -> Result<Token> {
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
        Ok(Token::Id(id))
    }

    fn parse_keywords(&mut self) -> Result<Token> {
        if self.expect("true") {
            Ok(Token::Bool(true))
        } else if self.expect("false") {
            Ok(Token::Bool(false))
        } else if self.expect("nil") {
            Ok(Token::Nil)
        } else if self.expect("in") {
            Ok(Token::In)
        } else {
            self.parse_id()
        }
    }

    fn parse_punctuations(&mut self) -> Result<Token> {
        let c = self.chars[self.idx];
        match c {
            '(' => {
                self.idx += 1;
                Ok(Token::LParen)
            }
            ')' => {
                self.idx += 1;
                Ok(Token::RParen)
            }
            '.' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next {
                    if c.is_digit(10) {
                        return self.parse_num();
                    }
                }
                self.idx += 1;
                Ok(Token::Dot)
            }
            '&' => {
                if self.expect("&&") {
                    Ok(Token::And)
                } else {
                    Err(anyhow!(self.err("Expect '&&'")))
                }
            }
            '|' => {
                if self.expect("||") {
                    Ok(Token::Or)
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
                Ok(Token::Add)
            }
            '-' => {
                let next = self.chars.get(self.idx + 1);
                if let Some(&c) = next {
                    if c.is_digit(10) {
                        return self.parse_num();
                    }
                }
                self.idx += 1;
                Ok(Token::Sub)
            }
            '*' => {
                self.idx += 1;
                Ok(Token::Mul)
            }
            '/' => {
                self.idx += 1;
                Ok(Token::Div)
            }
            '%' => {
                self.idx += 1;
                Ok(Token::Mod)
            }
            '@' => {
                self.idx += 1;
                Ok(Token::At)
            }
            '=' => {
                if self.expect("==") {
                    Ok(Token::Eq)
                } else {
                    Err(anyhow!(self.err("Expect '=='")))
                }
            }
            '!' => {
                if self.expect("!=") {
                    Ok(Token::Ne)
                } else {
                    self.idx += 1;
                    Ok(Token::Not)
                }
            }
            '>' => {
                if self.expect(">=") {
                    Ok(Token::Ge)
                } else {
                    self.idx += 1;
                    Ok(Token::Gt)
                }
            }
            '<' => {
                if self.expect("<=") {
                    Ok(Token::Le)
                } else {
                    self.idx += 1;
                    Ok(Token::Lt)
                }
            }
            _ => self.parse_id(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        while !self.eof() {
            self.skip_whitespace();
            if self.eof() {
                break;
            }
            let c = self.chars[self.idx];
            match c {
                '"' | '\'' => {
                    tokens.push(self.parse_str()?);
                }
                '0'..='9' => {
                    tokens.push(self.parse_num()?);
                }
                // true false nil in
                't' | 'f' | 'n' | 'i' => {
                    tokens.push(self.parse_keywords()?);
                }
                _ => {
                    tokens.push(self.parse_punctuations()?);
                }
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut t1 = Tokenizer::new(r#"1.3+*/@ %==  "str1" 'str2' true false nil "#);
        let e1 = vec![
            Token::Float(1.3),
            Token::Add,
            Token::Mul,
            Token::Div,
            Token::At,
            Token::Mod,
            Token::Eq,
            Token::Str("str1".to_string()),
            Token::Str("str2".to_string()),
            Token::Bool(true),
            Token::Bool(false),
            Token::Nil,
        ];
        assert_eq!(t1.parse().unwrap(), e1);
    }

    #[test]
    fn punctuations() {
        let mut t2 = Tokenizer::new(">=<= && || == != ! > <");
        let e2 = vec![
            Token::Ge,
            Token::Le,
            Token::And,
            Token::Or,
            Token::Eq,
            Token::Ne,
            Token::Not,
            Token::Gt,
            Token::Lt,
        ];
        assert_eq!(t2.parse().unwrap(), e2);
    }

    #[test]
    fn ids() {
        let mut t3 = Tokenizer::new("id1 id_2 id-3");
        let e3 = vec![
            Token::Id("id1".to_string()),
            Token::Id("id_2".to_string()),
            Token::Id("id-3".to_string()),
        ];
        assert_eq!(t3.parse().unwrap(), e3);
    }

    #[test]
    fn unclosed_str() {
        let mut t4 = Tokenizer::new(r#""str"#);
        let res = t4.parse();
        assert!(res.is_err());
        //println!("{}", res.unwrap_err());
    }

    #[test]
    fn num() {
        let mut t5 = Tokenizer::new("1.2.3");
        assert!(t5.parse().is_err());

        let mut t5 = Tokenizer::new("-1.0 +1.2");
        let e5 = vec![Token::Float(-1.0), Token::Float(1.2)];
        assert_eq!(t5.parse().unwrap(), e5);
    }

    #[test]
    fn keywords() {
        let mut t6 = Tokenizer::new(">true false nil in");
        let e6 = vec![
            Token::Gt,
            Token::Bool(true),
            Token::Bool(false),
            Token::Nil,
            Token::In,
        ];
        assert_eq!(t6.parse().unwrap(), e6);
    }

    #[test]
    fn token_eq() {
        assert_eq!(Token::Str("a".to_string()), Token::Str("a".to_string()));
        assert_eq!(Token::Int(1), Token::Int(1));
        assert_eq!(Token::Float(1.0), Token::Float(1.0));
        assert_eq!(Token::Bool(true), Token::Bool(true));
        assert_eq!(Token::Nil, Token::Nil);
        assert_ne!(Token::Str("a".to_string()), Token::Str("b".to_string()));
        assert_ne!(Token::Int(1), Token::Int(2));
        assert_ne!(Token::Float(1.0), Token::Float(2.0));
        assert_ne!(Token::Bool(true), Token::Bool(false));
        assert_ne!(Token::Nil, Token::Bool(false));
    }

    #[test]
    fn at_query() {
        let mut t = Tokenizer::new("@req.user.age >= 18");
        let e = vec![
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("age".to_string()),
            Token::Ge,
            Token::Int(18),
        ];
        assert_eq!(t.parse().unwrap(), e);
    }

    #[test]
    fn real_query() {
        let query = r#"
        (
            @req.user.id == @record.user.id && @record.time > 1700000
        ) 
        ||
        @req.user.role == 'admin'
        "#;
        let mut t = Tokenizer::new(query);
        let e = vec![
            Token::LParen,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::Eq,
            Token::At,
            Token::Id("record".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::And,
            Token::At,
            Token::Id("record".to_string()),
            Token::Dot,
            Token::Id("time".to_string()),
            Token::Gt,
            Token::Int(1700000),
            Token::RParen,
            Token::Or,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("role".to_string()),
            Token::Eq,
            Token::Str("admin".to_string()),
        ];
        assert_eq!(t.parse().unwrap(), e);
    }
}
