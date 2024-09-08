use std::fmt::{Debug, Display};

use anyhow::{anyhow, Result};

use crate::{ast::Parser, token::Tokenizer, val::Val};

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
                let res = expr.exec(ctx)?;
                match res {
                    Val::Bool(b) => Ok(Val::Bool(!b)),
                    _ => Err(anyhow!("Invalid operand: `!{:?}`", res)),
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
            BinOp::Add => match (l, r) {
                (Val::Int(l), Val::Int(r)) => Ok(Val::Int(l + r)),
                (Val::Float(l), Val::Float(r)) => Ok(Val::Float(l + r)),
                (Val::Float(l), Val::Int(r)) => Ok(Val::Float(l + (*r as f64))),
                (Val::Int(l), Val::Float(r)) => Ok(Val::Float((*l as f64) + r)),
                (Val::Str(l), Val::Str(r)) => Ok(Val::Str(format!("{}{}", l, r))),
                (Val::Map(l), Val::Map(r)) => {
                    let mut res = l.clone();
                    let r = r.iter().map(|(k, v)| (k.clone(), v.clone()));
                    res.extend(r);
                    Ok(Val::Map(res))
                }
                (Val::List(l), Val::List(r)) => {
                    let mut res = l.clone();
                    res.extend(r.iter().cloned());
                    Ok(Val::List(res))
                }
                (Val::Str(l), Val::Int(r)) => Ok(Val::Str(format!("{}{}", l, r))),
                (Val::Int(l), Val::Str(r)) => Ok(Val::Str(format!("{}{}", l, r))),
                (Val::Str(l), Val::Float(r)) => Ok(Val::Str(format!("{}{}", l, r))),
                (Val::Float(l), Val::Str(r)) => Ok(Val::Str(format!("{}{}", l, r))),
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
                (_, Val::Nil) => Ok(false),
                (Val::List(l), Val::List(r)) => Ok(r.iter().all(|v| l.contains(v))),
                _ => Err(anyhow!("Invalid operands {:?} in {:?}", l, r)),
            },
            _ => Err(anyhow!("Invalid cmp op {:?}", self)),
        }
    }

    fn exec(&self, l: &Expr, r: &Expr, ctx: &Val) -> Result<Val> {
        let l = l.exec(ctx)?;
        let r = r.exec(ctx)?;

        if self.is_arith() {
            self.arith(&l, &r)
        } else if self.is_cmp() {
            Ok(Val::Bool(self.cmp(&l, &r)?))
        } else {
            Err(anyhow!("Invalid op {:?}", self))
        }
    }
}

impl Expr {
    pub fn exec(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(left, op, right) => op.exec(left, right, ctx),
            Expr::Unary(op, expr) => Ok(op.eval(expr, ctx)?),
            Expr::And(e1, e2) => {
                let left = e1.exec(ctx)?;
                // For performance, we can short-circuit the evaluation.
                if let Val::Bool(false) = left {
                    return Ok(Val::Bool(false));
                }
                let right = e2.exec(ctx)?;
                if let Val::Bool(false) = right {
                    return Ok(Val::Bool(false));
                }
                match (&left, &right) {
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(true)),
                    _ => Err(anyhow!("Invalid operands {:?} && {:?}", left, right)),
                }
            }
            Expr::Or(e1, e2) => {
                let left = e1.exec(ctx)?;
                // For performance, we can short-circuit the evaluation.
                if let Val::Bool(true) = left {
                    return Ok(Val::Bool(true));
                }
                let right = e2.exec(ctx)?;
                if let Val::Bool(true) = right {
                    return Ok(Val::Bool(true));
                }
                match (&left, &right) {
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => Err(anyhow!("Invalid operands {:?} || {:?}", left, right)),
                }
            }
            Expr::At(paths) => {
                let mut val = ctx;
                for path in paths {
                    val = match val.access(path) {
                        Ok(v) => v,
                        Err(e) => {
                            let msg = format!("Invalid path {:?}, got {:?}", path, e);
                            return Err(anyhow!(msg))
                        },
                    }
                }
                // TODO: no clone
                Ok(val.clone())
            }
            Expr::Float(_) | Expr::Str(_) | Expr::Int(_) | Expr::Bool(_) => {
                Ok(self.to_val().unwrap_or(Val::Nil))
            }
            Expr::Paren(expr) => Ok(expr.exec(ctx)?),
            Expr::Nil => Ok(Val::Nil),
        }
    }

    fn to_val(&self) -> Option<Val> {
        match self {
            Expr::Float(f) => Some(Val::Float(*f)),
            Expr::Int(i) => Some(Val::Int(*i)),
            Expr::Str(s) => Some(Val::Str(s.clone())),
            Expr::Bool(b) => Some(Val::Bool(*b)),
            _ => None,
        }
    }
}

pub trait IntoExpr {
    fn into_expr(self) -> Result<Expr, anyhow::Error>;
}

impl<S: AsRef<str>> IntoExpr for S {
    fn into_expr(self) -> Result<Expr, anyhow::Error> {
        let tokens = Tokenizer::new(self.as_ref())?;
        let expr = Parser::new(&tokens).parse()?;
        Ok(expr)
    }
}

impl TryFrom<&str> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.into_expr()
    }
}

impl TryFrom<String> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.into_expr()
    }
}
