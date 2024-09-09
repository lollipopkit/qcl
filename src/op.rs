use core::cmp::Ordering;
use core::fmt::Debug;
use std::fmt::Display;

use anyhow::{anyhow, Result};

use crate::{expr::Expr, val::Val};

pub(crate) fn err_op<T: Display, R>(l: &Val, op: T, r: &Val) -> Result<R> {
    Err(anyhow!("Invalid op: {l} {op} {r}"))
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

impl UnaryOp {
    pub(crate) fn eval(&self, expr: &Expr, ctx: &Val) -> Result<Val> {
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

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
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

    pub(crate) fn eval(&self, l: &Expr, r: &Expr, ctx: &Val) -> Result<Val> {
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Ge => write!(f, ">="),
            BinOp::Le => write!(f, "<="),
            BinOp::In => write!(f, "in"),
        }
    }
}
