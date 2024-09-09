use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

use anyhow::{anyhow, Result};

use crate::{
    ast::Parser,
    token::Tokenizer,
    val::{err_op, Val},
};

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
/// - int / float are considered as `i64 / f64`.
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
pub enum Expr {
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
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinOp {
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

impl UnaryOp {
    fn eval(&self, expr: &Expr, ctx: &Val) -> Result<Val> {
        match self {
            UnaryOp::Not => {
                let res = expr.eval(ctx)?;
                match res {
                    Val::Bool(b) => Ok(Val::Bool(!b)),
                    _ => Err(anyhow!("Invalid operand: !{res}")),
                }
            }
        }
    }
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
            BinOp::Add => l + r,
            BinOp::Sub => l - r,
            BinOp::Mul => l * r,
            BinOp::Div => l / r,
            BinOp::Mod => l % r,
            _ => err_op(l, self, r),
        }
    }

    fn cmp_inner(&self, l: &Val, r: &Val) -> Result<Ordering> {
        match l.partial_cmp(r) {
            Some(ord) => Ok(ord),
            None => err_op(l, self, r),
        }
    }

    fn cmp(&self, l: &Val, r: &Val) -> Result<bool> {
        match self {
            BinOp::Eq => Ok(l == r),
            BinOp::Ne => Ok(l != r),
            BinOp::Gt => {
                let ord = self.cmp_inner(l, r)?;
                Ok(ord == Ordering::Greater)
            }
            BinOp::Lt => {
                let ord = self.cmp_inner(l, r)?;
                Ok(ord == Ordering::Less)
            }
            BinOp::Ge => {
                let ord = self.cmp_inner(l, r)?;
                Ok(ord == Ordering::Greater || ord == Ordering::Equal)
            }
            BinOp::Le => {
                let ord = self.cmp_inner(l, r)?;
                Ok(ord == Ordering::Less || ord == Ordering::Equal)
            }
            BinOp::In => match (l, r) {
                (Val::Str(l), Val::Str(r)) => Ok(r.contains(l)),
                (Val::List(l), Val::List(r)) => Ok(r.iter().all(|v| l.contains(v))),
                (Val::List(l), r) => Ok(l.contains(r)),
                (Val::Map(l), Val::Str(s)) => Ok(l.contains_key(s)),
                _ => err_op(l, self, r),
            },
            _ => err_op(l, self, r),
        }
    }

    fn eval(&self, l: &Expr, r: &Expr, ctx: &Val) -> Result<Val> {
        let l = l.eval(ctx)?;
        let r = r.eval(ctx)?;

        if self.is_arith() {
            self.arith(&l, &r)
        } else if self.is_cmp() {
            Ok(Val::Bool(self.cmp(&l, &r)?))
        } else {
            Err(anyhow!("Invalid eval: {l} {self:?} {r}"))
        }
    }
}

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(l, op, r) => op.eval(l, r, ctx),
            Expr::Unary(op, expr) => Ok(op.eval(expr, ctx)?),
            Expr::And(e1, e2) => {
                let l = e1.eval(ctx)?;
                // For performance, we can short-circuit the evaluation.
                if let Val::Bool(false) = l {
                    return Ok(false.into());
                }
                let r = e2.eval(ctx)?;
                if let Val::Bool(false) = r {
                    return Ok(false.into());
                }
                match (&l, &r) {
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(true)),
                    _ => Err(anyhow!("Invalid operands: {l} && {r}")),
                }
            }
            Expr::Or(e1, e2) => {
                let l = e1.eval(ctx)?;
                // For performance, we can short-circuit the evaluation.
                if let Val::Bool(true) = l {
                    return Ok(Val::Bool(true));
                }
                let r = e2.eval(ctx)?;
                if let Val::Bool(true) = r {
                    return Ok(Val::Bool(true));
                }
                match (&l, &r) {
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => Err(anyhow!("Invalid operands {l} || {r}")),
                }
            }
            Expr::At(paths) => {
                let mut val = ctx;
                for path in paths {
                    val = match val.access(path) {
                        Ok(v) => v,
                        Err(e) => {
                            let msg = format!("Invalid path: @{path}, err {e:?}");
                            return Err(anyhow!(msg));
                        }
                    }
                }
                // TODO: no clone
                Ok(val.clone())
            }
            Expr::Float(_) | Expr::Str(_) | Expr::Int(_) | Expr::Bool(_) => {
                let v: Val = self.try_into()?;
                Ok(v)
            }
            Expr::Paren(expr) => Ok(expr.eval(ctx)?),
            Expr::Nil => Ok(Val::Nil),
        }
    }
}

impl TryInto<Val> for &Expr {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Val> {
        match self {
            Expr::Float(f) => Ok(Val::Float(*f)),
            Expr::Int(i) => Ok(Val::Int(*i)),
            Expr::Str(s) => Ok(Val::Str(s.clone())),
            Expr::Bool(b) => Ok(Val::Bool(*b)),
            Expr::Nil => Ok(Val::Nil),
            _ => {
                let msg = format!("Can't convert Expr::{:?} to Val", self);
                Err(anyhow!(msg))
            }
        }
    }
}

fn into_expr<S: AsRef<str>>(s: S) -> Result<Expr> {
    let tokens = Tokenizer::new(s.as_ref())?;
    let expr = Parser::new(&tokens).parse()?;
    Ok(expr)
}

impl TryFrom<&str> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl TryFrom<String> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Bin(left, op, right) => write!(f, "{left} {op:?} {right}"),
            Expr::Unary(op, expr) => write!(f, "{op:?}{expr}"),
            Expr::And(left, right) => write!(f, "{left} && {right}"),
            Expr::Or(left, right) => write!(f, "{left} || {right}"),
            Expr::At(paths) => {
                let paths: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                write!(f, "@{}", paths.join("."))
            }
            Expr::Paren(expr) => write!(f, "{expr}"),
            Expr::Bool(b) => write!(f, "{b}"),
            Expr::Float(fl) => write!(f, "{fl}"),
            Expr::Int(i) => write!(f, "{i}"),
            Expr::Str(s) => write!(f, "{s}"),
            Expr::Nil => write!(f, "nil"),
        }
    }
}
