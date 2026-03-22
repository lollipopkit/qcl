use core::cmp::Ordering;
use core::fmt::Debug;
use std::collections::HashSet;
use std::fmt::Display;

use anyhow::{Result, anyhow};

use crate::{expr::Expr, val::Val};

const LARGE_LIST_MEMBERSHIP_THRESHOLD: usize = 32;

fn list_contains(list: &[Val], needle: &Val) -> bool {
    match needle {
        Val::Int(i) => list.iter().any(|v| matches!(v, Val::Int(x) if x == i)),
        Val::Str(s) => list
            .iter()
            .any(|v| matches!(v, Val::Str(t) if t.as_ref() == s.as_ref())),
        Val::Bool(b) => list.iter().any(|v| matches!(v, Val::Bool(x) if x == b)),
        _ => list.contains(needle),
    }
}

#[derive(Default)]
struct MembershipIndex<'a> {
    ints: HashSet<i64>,
    strs: HashSet<&'a str>,
    has_true: bool,
    has_false: bool,
    fallback: Vec<&'a Val>,
}

impl<'a> MembershipIndex<'a> {
    fn new(list: &'a [Val]) -> Self {
        let mut index = Self {
            ints: HashSet::with_capacity(list.len()),
            strs: HashSet::with_capacity(list.len()),
            has_true: false,
            has_false: false,
            fallback: Vec::new(),
        };

        for value in list {
            match value {
                Val::Int(i) => {
                    index.ints.insert(*i);
                }
                Val::Str(s) => {
                    index.strs.insert(s.as_ref());
                }
                Val::Bool(true) => {
                    index.has_true = true;
                }
                Val::Bool(false) => {
                    index.has_false = true;
                }
                _ => index.fallback.push(value),
            }
        }

        index
    }

    fn contains(&self, needle: &Val) -> bool {
        match needle {
            Val::Int(i) => self.ints.contains(i),
            Val::Str(s) => self.strs.contains(s.as_ref()),
            Val::Bool(true) => self.has_true,
            Val::Bool(false) => self.has_false,
            _ => self.fallback.contains(&needle),
        }
    }
}

fn list_contains_indexed(list: &[Val], needle: &Val) -> bool {
    if list.len() <= LARGE_LIST_MEMBERSHIP_THRESHOLD {
        return list_contains(list, needle);
    }

    MembershipIndex::new(list).contains(needle)
}

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
    pub(crate) fn is_arith(&self) -> bool {
        matches!(self, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod)
    }

    pub(crate) fn is_cmp(&self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le | BinOp::In
        )
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

    pub(crate) fn cmp(&self, l: &Val, r: &Val) -> Result<bool> {
        if matches!(l, Val::Missing) || matches!(r, Val::Missing) {
            return match self {
                BinOp::Eq | BinOp::Ne => Ok(false),
                _ => err_op(l, self, r),
            };
        }

        match self {
            BinOp::Eq => Ok(l == r),
            BinOp::Ne => Ok(l != r),
            BinOp::In => match (l, r) {
                (Val::Str(l), Val::Str(r)) => Ok(r.as_ref().contains(l.as_ref())),

                // All elements in l must be in r
                (Val::List(l), Val::List(r)) => {
                    if l.is_empty() {
                        return Ok(true);
                    }

                    if r.is_empty() {
                        return Ok(false);
                    }

                    if l.len() <= LARGE_LIST_MEMBERSHIP_THRESHOLD || r.len() <= LARGE_LIST_MEMBERSHIP_THRESHOLD {
                        return Ok((**l).iter().all(|x| list_contains(r, x)));
                    }

                    let index = MembershipIndex::new(r);
                    Ok((**l).iter().all(|x| index.contains(x)))
                }

                // Single element membership
                (_, Val::List(r)) => Ok(list_contains_indexed(r, l)),

                _ => err_op(l, self, r),
            },
            _ => {
                // For other comparison operators, we need ordering
                let ord = match l.partial_cmp(r) {
                    Some(ord) => ord,
                    None => return err_op(l, self, r),
                };

                match self {
                    BinOp::Gt => Ok(ord == Ordering::Greater),
                    BinOp::Lt => Ok(ord == Ordering::Less),
                    BinOp::Ge => Ok(ord != Ordering::Less),
                    BinOp::Le => Ok(ord != Ordering::Greater),
                    _ => err_op(l, self, r),
                }
            }
        }
    }

    pub(crate) fn eval(&self, l: &Expr, r: &Expr, ctx: &Val) -> Result<Val> {
        // For comparison operators, we can optimize by only evaluating the left side first
        if self.is_cmp() && matches!(self, BinOp::Eq | BinOp::Ne) {
            let l_val = l.eval(ctx)?;

            // Missing-field comparisons are fail-closed.
            if matches!(l_val, Val::Missing) {
                return Ok(Val::Bool(false));
            }

            // Short-circuit for nil comparisons
            match (&l_val, self) {
                (Val::Nil, BinOp::Eq) => {
                    let r_val = r.eval(ctx)?;
                    if matches!(r_val, Val::Missing) {
                        return Ok(Val::Bool(false));
                    }
                    return Ok(Val::Bool(matches!(r_val, Val::Nil)));
                }
                (Val::Nil, BinOp::Ne) => {
                    let r_val = r.eval(ctx)?;
                    if matches!(r_val, Val::Missing) {
                        return Ok(Val::Bool(false));
                    }
                    return Ok(Val::Bool(!matches!(r_val, Val::Nil)));
                }
                _ => {}
            }

            let r_val = r.eval(ctx)?;
            return Ok(Val::Bool(self.cmp(&l_val, &r_val)?));
        }

        // For arithmetic operations
        let l_val = l.eval(ctx)?;
        let r_val = r.eval(ctx)?;

        if self.is_arith() {
            self.arith(&l_val, &r_val)
        } else if self.is_cmp() {
            Ok(Val::Bool(self.cmp(&l_val, &r_val)?))
        } else {
            Err(anyhow!("Invalid eval: {l_val} {self:?} {r_val}"))
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
