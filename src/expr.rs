use std::fmt::{Debug, Display};

use anyhow::{anyhow, Result};

use crate::{token::Token, val::Val};

/// Grammar:
/// exp     ::= paren
/// paren   ::= {‘(’} or {‘)’}
/// or      ::= and {’||’ and}
/// and     ::= cmp {’&&’ cmp}
/// cmp     ::= addsub {(‘<’ | ‘>’ | ‘<=’ | ‘>=’ | ‘!=’ | ‘==’) addsub}
/// addsub  ::= muldiv {(‘+’ | ‘-’) muldiv}
/// muldiv  ::= unary {(‘*’ | ‘/’ | ‘%’) unary}
/// unary   ::= {‘!’} primary
/// primary ::= nil | false | true | int | float | string | at
/// at      ::= ‘@’ field {‘.’ field}
/// field   ::= id | int
///
///
/// Details:
/// - @expr
///   + All accessible objects of `@` expr are maps actually.
///   + You can use `.` to access the fields of the map.
///   + @req: Request object, `@req.user` is the user object.
///   + @record: Record object, `@record` is the record object.
///   + All valid objects are defined in the [Context]
/// - number are considered as `f64`.
/// - bool can be `true` or `false`.
/// - String
///   + can be wrapped with `""` or `''`.
///   + max length is 64.
/// - nil
///   + ONLY [Option::None] and [Result::Err] are `nil`.
///   + zero value of all types are NOT `nil`.
///
/// Examples:
/// - `@req.user.age >= 18`
/// - `@req.user.name == "Alice" && @record.status == "active"`
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Expr {
    /// expr == expr
    Bin(Box<Expr>, BinOp, Box<Expr>),
    /// !expr
    Unary(UnaryOp, Box<Expr>),
    /// expr && expr
    And(Box<Expr>, Box<Expr>),
    /// expr || expr
    Or(Box<Expr>, Box<Expr>),
    /// expr.field
    /// field can be string or int
    At(Vec<Val>),
    // (expr)
    Paren(Box<Expr>),
    Bool(bool),
    Float(f64),
    Int(i64),
    Str(String),
    Nil,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Bin(left, op, right) => write!(f, "({} {:?} {})", left, op, right),
            Expr::Unary(op, expr) => write!(f, "({:?} {})", op, expr),
            Expr::And(left, right) => write!(f, "({} && {})", left, right),
            Expr::Or(left, right) => write!(f, "({} || {})", left, right),
            Expr::At(paths) => {
                let paths: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                write!(f, "@{}", paths.join("."))
            }
            Expr::Paren(expr) => write!(f, "({})", expr),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Float(fl) => write!(f, "{}", fl),
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
    Not,
    At,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    In,
}

pub(crate) struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    len: usize,
    exp: Expr,
}

impl Parser {
    pub fn parse(&mut self) -> Result<Expr> {
        while !self.eof() {
            self.exp = self.parse_paren()?;
        }
        Ok(self.exp.clone())
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
            _ => {
                let msg = format!("Expecting primary, found {:?}", token);
                return Err(anyhow!(self.err(&msg)));
            }
        };
        Ok(expr)
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

    fn parse_op(&mut self) -> Result<BinOp> {
        let token = &self.tokens[self.pos];
        let op = match token {
            Token::Add => BinOp::Add,
            Token::Sub => BinOp::Sub,
            Token::Mul => BinOp::Mul,
            Token::Div => BinOp::Div,
            Token::Mod => BinOp::Mod,
            Token::Eq => BinOp::Eq,
            Token::Ne => BinOp::Ne,
            Token::Gt => BinOp::Gt,
            Token::Lt => BinOp::Lt,
            Token::Ge => BinOp::Ge,
            Token::Le => BinOp::Le,
            Token::In => BinOp::In,
            _ => {
                let msg = format!("Expecting operator, found {:?}", token);
                return Err(anyhow!(self.err(&msg)));
            }
        };
        self.pos += 1;
        Ok(op)
    }
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
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
