use crate::{expr::{BinOp, Expr, UnaryOp}, token::Token, val::Val};
use anyhow::{anyhow, Result};

pub(crate) struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
    exp: Expr,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Expr> {
        while !self.eof() {
            self.exp = self.parse_or()?;
        }
        Ok(self.exp.clone())
    }

    fn parse_or(&mut self) -> Result<Expr> {
        let mut expr = self.parse_and()?;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::Or => {
                    self.pos += 1;
                    let right = self.parse_and()?;
                    expr = Expr::Or(Box::new(expr), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr> {
        let mut expr = self.parse_cmp()?;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::And => {
                    self.pos += 1;
                    let right = self.parse_cmp()?;
                    expr = Expr::And(Box::new(expr), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_cmp(&mut self) -> Result<Expr> {
        let mut expr = self.parse_add_sub()?;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Eq => BinOp::Eq,
                Token::Ne => BinOp::Ne,
                Token::Gt => BinOp::Gt,
                Token::Lt => BinOp::Lt,
                Token::Ge => BinOp::Ge,
                Token::Le => BinOp::Le,
                Token::In => BinOp::In,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_add_sub()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_add_sub(&mut self) -> Result<Expr> {
        let mut expr = self.parse_mul_div()?;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Add => BinOp::Add,
                Token::Sub => BinOp::Sub,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_mul_div()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_mul_div(&mut self) -> Result<Expr> {
        let mut expr = self.parse_unary()?;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Mul => BinOp::Mul,
                Token::Div => BinOp::Div,
                Token::Mod => BinOp::Mod,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_unary()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        let token = &self.tokens[self.pos];
        match token {
            Token::Not => {
                self.pos += 1;
                let expr = self.parse_primary()?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        let token = &self.tokens[self.pos];
        let expr = match token {
            Token::Nil => {
                self.pos += 1;
                Expr::Nil
            }
            Token::Bool(b) => {
                self.pos += 1;
                Expr::Bool(*b)
            }
            Token::Int(i) => {
                self.pos += 1;
                Expr::Int(*i)
            }
            Token::Float(f) => {
                self.pos += 1;
                Expr::Float(*f)
            }
            Token::Str(s) => {
                self.pos += 1;
                Expr::Str(s.clone())
            }
            Token::At => self.parse_at()?,
            _ => self.parse_paren()?,
        };
        Ok(expr)
    }

    fn parse_paren(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] == Token::LParen {
            self.pos += 1;
            let expr = self.parse_or()?;
            if self.tokens[self.pos] != Token::RParen {
                let msg = format!("Expecting ')', found {:?}", self.tokens[self.pos]);
                return Err(anyhow!(self.err(&msg)));
            }
            self.pos += 1;
            Ok(Expr::Paren(Box::new(expr)))
        } else {
            self.parse_or()
        }
    }

    fn parse_at(&mut self) -> Result<Expr> {
        let mut paths = vec![];
        if self.tokens[self.pos] != Token::At {
            let msg = format!("Expecting @, found {:?}", self.tokens[self.pos]);
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;
        if self.eof() {
            return Err(anyhow!(self.err("Expecting field after '@'")));
        }
        while !self.eof() {
            let path = self.parse_field()?;
            paths.push(path);
            if self.eof() {
                break;
            }
            if self.tokens[self.pos] != Token::Dot {
                break;
            }
            self.pos += 1;
        }
        Ok(Expr::At(paths))
    }

    fn parse_field(&mut self) -> Result<Val> {
        let token = &self.tokens[self.pos];
        let field = match token {
            Token::Id(id) => Val::Str(id.clone()),
            Token::Int(i) => Val::Int(*i),
            _ => {
                let msg = format!("Expecting string or int, found {:?}", token);
                return Err(anyhow!(self.err(&msg)));
            }
        };
        self.pos += 1;
        Ok(field)
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            exp: Expr::Nil,
        }
    }

    fn eof(&self) -> bool {
        self.pos >= self.len
    }

    fn err(&self, msg: &str) -> String {
        let r_idx = if self.pos + 5 < self.len {
            self.pos + 5
        } else {
            self.len
        };
        let l_idx = if self.pos > 5 { self.pos - 5 } else { 0 };
        let r_idx = if r_idx > self.len { self.len } else { r_idx };
        let chars = &self.tokens[l_idx..r_idx];
        let chars: Vec<_> = chars.iter().collect();
        let c = self.tokens.get(self.pos);
        let ctx = if let Some(c) = c {
            format!("'{:?}' at index {}, near '{:?}'", c, self.pos, chars)
        } else {
            format!("at end, near '{:?}'", chars)
        };
        format!("Syntax error: {} ({})", msg, ctx)
    }
}
