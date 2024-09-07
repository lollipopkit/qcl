use std::fmt::Debug;

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

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
    Not,
    At,
}

impl UnaryOp {
    fn eval(&self, expr: &Expr, ctx: &Val) -> Result<Val> {
        match self {
            UnaryOp::Not => {
                let res = expr.eval(ctx)?;
                match res {
                    Val::Bool(b) => Ok(Val::Bool(!b)),
                    _ => Err(anyhow!("Invalid operand: `!{:?}`", res)),
                }
            }
            // TODO
            UnaryOp::At => expr.eval(ctx),
        }
    }
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

impl BinOp {
    fn is_arith(&self) -> bool {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => true,
            _ => false,
        }
    }

    fn is_cmp(&self) -> bool {
        match self {
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le | BinOp::In => {
                true
            }
            _ => false,
        }
    }

    fn arith(&self, l: &Val, r: &Val) -> Result<Val> {
        match self {
            BinOp::Add => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l + r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l + r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l + (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) + r)),
                (Val::Str(l), Val::Str(r)) => Ok(Val::Str(format!("{}{}", l, r))),
                _ => Err(anyhow!("Invalid operands {:?} + {:?}", l, r)),
            },
            BinOp::Sub => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l - r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l - r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l - (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) - r)),
                _ => Err(anyhow!("Invalid operands {:?} - {:?}", l, r)),
            },
            BinOp::Mul => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l * r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l * r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l * (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) * r)),
                _ => Err(anyhow!("Invalid operands {:?} * {:?}", l, r)),
            },
            BinOp::Div => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l / r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l / r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l / (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) / r)),
                _ => Err(anyhow!("Invalid operands {:?} / {:?}", l, r)),
            },
            BinOp::Mod => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l % r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l % r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l % (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) % r)),
                _ => Err(anyhow!("Invalid operands {:?} % {:?}", l, r)),
            },
            _ => Err(anyhow!("Invalid arith op {:?}", self)),
        }
    }

    fn cmp(&self, l: &Val, r: &Val) -> Result<bool> {
        match self {
            BinOp::Eq => Ok(l == r),
            BinOp::Ne => Ok(l != r),
            BinOp::Gt => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(l > r),
                (Val::Float(l), Val::Float(r)) => Ok(l > r),
                (Val::Str(l), Val::Str(r)) => Ok(l > r),
                (Val::Int(l), Val::Float(r)) => Ok((*l as f64) > *r),
                (Val::Float(l), Val::Int(r)) => Ok(*l > (*r as f64)),
                _ => Err(anyhow!("Invalid operands {:?} > {:?}", l, r)),
            },
            BinOp::Lt => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(l < r),
                (Val::Float(l), Val::Float(r)) => Ok(l < r),
                (Val::Str(l), Val::Str(r)) => Ok(l < r),
                (Val::Int(l), Val::Float(r)) => Ok((*l as f64) < *r),
                (Val::Float(l), Val::Int(r)) => Ok(*l < (*r as f64)),
                _ => Err(anyhow!("Invalid operands {:?} < {:?}", l, r)),
            },
            BinOp::Ge => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(l >= r),
                (Val::Float(l), Val::Float(r)) => Ok(l >= r),
                (Val::Str(l), Val::Str(r)) => Ok(l >= r),
                (Val::Int(l), Val::Float(r)) => Ok((*l as f64) >= *r),
                (Val::Float(l), Val::Int(r)) => Ok(*l >= (*r as f64)),
                _ => Err(anyhow!("Invalid operands {:?} >= {:?}", l, r)),
            },
            BinOp::Le => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(l <= r),
                (Val::Float(l), Val::Float(r)) => Ok(l <= r),
                (Val::Str(l), Val::Str(r)) => Ok(l <= r),
                (Val::Int(l), Val::Float(r)) => Ok((*l as f64) <= *r),
                (Val::Float(l), Val::Int(r)) => Ok(*l <= (*r as f64)),
                _ => Err(anyhow!("Invalid operands {:?} <= {:?}", l, r)),
            },
            BinOp::In => match (l, r) {
                (Val::Str(l), Val::Str(r)) => Ok(l.contains(r)),
                _ => Err(anyhow!("Invalid operands {:?} in {:?}", l, r)),
            },
            _ => Err(anyhow!("Invalid cmp op {:?}", self)),
        }
    }

    fn eval(&self, l: &Expr, r: &Expr, ctx: &Val) -> Result<Val> {
        let l = l.eval(ctx)?;
        let r = r.eval(ctx)?;

        if self.is_arith() {
            self.arith(&l, &r)
        } else if self.is_cmp() {
            let res = self.cmp(&l, &r)?;
            Ok(Val::Bool(res))
        } else {
            Err(anyhow!("Invalid op {:?}", self))
        }
    }
}

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(left, op, right) => op.eval(left, right, ctx),
            Expr::Unary(op, expr) => Ok(op.eval(expr, ctx)?),
            Expr::Bool(value) => Ok(Val::Bool(*value)),
            Expr::And(e1, e2) => {
                let left = e1.eval(ctx)?;
                if let Val::Bool(false) = left {
                    return Ok(Val::Bool(false));
                }
                let right = e2.eval(ctx)?;
                if let Val::Bool(false) = right {
                    return Ok(Val::Bool(false));
                }
                Ok(Val::Bool(true))
            }
            Expr::Or(e1, e2) => {
                let left = e1.eval(ctx)?;
                if let Val::Bool(true) = left {
                    return Ok(Val::Bool(true));
                }
                let right = e2.eval(ctx)?;
                if let Val::Bool(true) = right {
                    return Ok(Val::Bool(true));
                }
                Ok(Val::Bool(false))
            }
            Expr::At(paths) => {
                let mut val = ctx;
                for path in paths {
                    val = match val.access(path) {
                        Val::Nil => return Ok(Val::Nil),
                        val => val,
                    }
                }
                // TODO: no clone
                Ok(val.clone())
            }
            Expr::Float(_) | Expr::Str(_) | Expr::Int(_) => {
                Err(anyhow!("Can't eval on {:?}", self))
            }
            Expr::Paren(expr) => Ok(expr.eval(ctx)?),
            Expr::Nil => Ok(Val::Nil),
        }
    }
}

pub(crate) struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    len: usize,
}

impl Parser {
    pub fn parse(&mut self) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();
        while !self.eof() {
            let expr = self.parse_paren()?;
            exprs.push(expr);
        }
        Ok(exprs)
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
            self.pos += 1;
            if self.tokens[self.pos] != Token::Dot {
                break;
            }
        }
        Ok(Expr::At(paths))
    }

    fn parse_field(&mut self) -> Result<Val> {
        let token = &self.tokens[self.pos];
        println!("field token: {:?}", token);
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
    fn new(tokens: Vec<Token>) -> Self {
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
        // Collect near 10(max) chars around the error position
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let tokens = vec![
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("age".to_string()),
            Token::Gt,
            Token::Int(18),
        ];
        let expr = Expr::Bin(
            Box::new(Expr::At(vec![
                Val::Str("req".to_string()),
                Val::Str("user".to_string()),
                Val::Str("age".to_string()),
            ])),
            BinOp::Gt,
            Box::new(Expr::Int(18)),
        );
        println!("expr: {:?}", Parser::new(tokens).parse().unwrap());
    }
}
