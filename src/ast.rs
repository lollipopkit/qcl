use crate::{
    expr::Expr,
    op::{BinOp, UnaryOp},
    token::Token,
    val::Val,
};
use anyhow::{anyhow, Result};

pub(crate) struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Expr> {
        if self.eof() {
            return Ok(Expr::Val(Val::Nil));
        }

        let exp = self.parse_expr()?;

        if !self.eof() {
            return Err(anyhow!(self.err("Unexpected tokens at end")));
        }

        Ok(exp)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_or()
    }

    /// - `expr || expr`
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

    /// `expr && expr`
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

    /// - `expr == expr`
    /// - `expr != expr`
    /// ...
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

    /// - `expr + expr`
    /// - `expr - expr`
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

    /// - `expr * expr`
    /// - `expr / expr`
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

    /// - `!expr`
    /// - `expr`
    fn parse_unary(&mut self) -> Result<Expr> {
        let token = &self.tokens[self.pos];
        match token {
            Token::Not => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)))
            }
            _ => self.parse_primary(),
        }
    }

    /// - `nil`
    /// - `true`
    /// - `false`
    /// - `1`
    /// - `1.2`
    /// - `"str"`
    fn parse_primary(&mut self) -> Result<Expr> {
        let token = &self.tokens[self.pos];
        let expr = match token {
            Token::Nil => {
                self.pos += 1;
                Expr::Val(Val::Nil)
            }
            Token::Bool(b) => {
                self.pos += 1;
                Expr::Val(Val::Bool(*b))
            }
            Token::Int(i) => {
                self.pos += 1;
                Expr::Val(Val::Int(*i))
            }
            Token::Float(f) => {
                self.pos += 1;
                Expr::Val(Val::Float(*f))
            }
            Token::Str(s) => {
                self.pos += 1;
                Expr::Val(Val::Str(s.to_owned()))
            }
            Token::At => self.parse_at()?,
            _ => self.parse_paren()?,
        };
        Ok(expr)
    }

    /// - `(expr)`
    /// - `expr`
    fn parse_paren(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] == Token::LParen {
            self.pos += 1;
            let expr = self.parse_expr()?;
            if self.tokens[self.pos] != Token::RParen {
                let msg = format!("Expecting ')', found {:?}", self.tokens[self.pos]);
                return Err(anyhow!(self.err(&msg)));
            }
            self.pos += 1;
            Ok(Expr::Paren(Box::new(expr)))
        } else {
            // This is where the recursion issue was - we need a terminal case
            match &self.tokens[self.pos] {
                Token::Id(id) => {
                    let expr = Expr::Val(Val::Str(id.clone()));
                    self.pos += 1;
                    Ok(expr)
                }
                _ => {
                    let msg = format!("Unexpected token: {:?}", self.tokens[self.pos]);
                    Err(anyhow!(self.err(&msg)))
                }
            }
        }
    }

    /// Parse a field accessor in an @ expression
    fn parse_field_accessor(&mut self) -> Result<Expr> {
        match &self.tokens[self.pos] {
            Token::Id(id) => {
                let expr = Expr::Val(Val::Str(id.clone()));
                self.pos += 1;
                Ok(expr)
            }
            Token::Int(i) => {
                let expr = Expr::Val(Val::Int(*i));
                self.pos += 1;
                Ok(expr)
            }
            Token::LParen => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                if self.tokens[self.pos] != Token::RParen {
                    let msg = format!("Expecting ')', found {:?}", self.tokens[self.pos]);
                    return Err(anyhow!(self.err(&msg)));
                }
                self.pos += 1;
                Ok(expr)
            }
            Token::At => self.parse_at(),
            _ => {
                let msg = format!(
                    "Unexpected token in field accessor: {:?}",
                    self.tokens[self.pos]
                );
                Err(anyhow!(self.err(&msg)))
            }
        }
    }

    /// - `@user.name`
    /// - `@user.emails.0.company`
    /// - `@user.subscribers.(@record.sender).name`
    /// - `@(1 + "1")`
    fn parse_at(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] != Token::At {
            let msg = format!("Expecting @, found {:?}", self.tokens[self.pos]);
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        if self.eof() {
            return Err(anyhow!(self.err("Expecting field after '@'")));
        }

        // Pre-allocate a reasonable size for the paths vector
        let mut paths = Vec::with_capacity(4);

        while !self.eof() {
            // Check the first path must be Str
            if paths.is_empty() {
                let first = &self.tokens[self.pos];
                match first {
                    Token::Id(_) | Token::LParen => {}
                    _ => {
                        let msg = format!("Expecting field name, found {:?}", first);
                        return Err(anyhow!(self.err(&msg)));
                    }
                }
            }

            paths.push(Box::new(self.parse_field_accessor()?));

            if self.eof() || self.tokens[self.pos] != Token::Dot {
                break;
            }
            self.pos += 1;
        }

        Ok(Expr::At(paths))
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
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
