use anyhow::{anyhow, Ok, Result};

use crate::{expr::{BinOp, Expr, UnaryOp}, val::Val};

pub struct Executor {
    expr: Expr,
}

impl Executor {
    pub fn new(raw: &str) -> Result<Self> {
        let executor = Self { expr: Expr::Bool(true) };
        Ok(executor)
    }

    pub fn eval(&self, ctx: &Val) -> Result<bool> {
        let val = self.expr.eval(ctx)?;
        match val {
            Val::Bool(b) => Ok(b),
            _ => Err(anyhow!("Invalid result {:?}", val)),
        }
    }
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
