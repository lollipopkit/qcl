use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use anyhow::{anyhow, Result};

use crate::{
    ast::Parser,
    op::{err_op, BinOp, UnaryOp},
    token::Tokenizer,
    val::Val,
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

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(l, op, r) => op.eval(l, r, ctx),
            Expr::Unary(op, expr) => op.eval(expr, ctx),
            Expr::And(e1, e2) => {
                let l = e1.eval(ctx)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(false) = l {
                    return Ok(Val::Bool(false));
                }
                let r = e2.eval(ctx)?;
                match (&l, &r) {
                    (Val::Bool(true), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "&&", &r),
                }
            }
            Expr::Or(e1, e2) => {
                let l = e1.eval(ctx)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(true) = l {
                    return Ok(Val::Bool(true));
                }
                let r = e2.eval(ctx)?;
                match (&l, &r) {
                    (Val::Bool(_), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "||", &r),
                }
            }
            Expr::At(paths) => {
                if paths.is_empty() {
                    return Ok(Val::Nil);
                }

                let mut val = ctx;
                for path in paths {
                    val = match val.access(path) {
                        Some(v) => v,
                        None => return Ok(Val::Nil),
                    }
                }
                // Return a clone only at the end of evaluation to reduce allocations
                Ok(val.clone())
            }
            Expr::Float(f) => Ok(Val::Float(*f)),
            Expr::Str(s) => Ok(Val::Str(s.clone())),
            Expr::Int(i) => Ok(Val::Int(*i)),
            Expr::Bool(b) => Ok(Val::Bool(*b)),
            Expr::Paren(expr) => expr.eval(ctx),
            Expr::Nil => Ok(Val::Nil),
        }
    }

    /// Get the requested context names from the expression.
    pub fn requested_ctx(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        self.collect_ctx_names(&mut names);
        names
    }

    // Helper method to collect context names recursively
    fn collect_ctx_names(&self, names: &mut HashSet<String>) {
        match self {
            Expr::At(paths) => {
                if let Some(Val::Str(s)) = paths.first() {
                    names.insert(s.clone());
                }
            }
            Expr::Bin(l, _, r) => {
                l.collect_ctx_names(names);
                r.collect_ctx_names(names);
            }
            Expr::Unary(_, expr) => {
                expr.collect_ctx_names(names);
            }
            Expr::And(l, r) | Expr::Or(l, r) => {
                l.collect_ctx_names(names);
                r.collect_ctx_names(names);
            }
            Expr::Paren(expr) => {
                expr.collect_ctx_names(names);
            }
            _ => {}
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
