use alloc::{format, vec::Vec};
use core::cmp::Ordering;
use core::fmt::{Debug, Display};
use hashbrown::HashSet;

use crate::{
    error::{Error, Result},
    expr::Expr,
    val::Val,
};

const LARGE_LIST_MEMBERSHIP_THRESHOLD: usize = 16;
const LIST_IN_LIST_INDEX_MIN_PROBES: usize = 128;

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
        // Count actual Int/Str entries first so each HashSet is preallocated
        // to the needed size instead of the full list length.
        let mut count_ints = 0;
        let mut count_strs = 0;
        for value in list {
            match value {
                Val::Int(_) => count_ints += 1,
                Val::Str(_) => count_strs += 1,
                _ => {}
            }
        }

        let mut index = Self {
            ints: HashSet::with_capacity(count_ints),
            strs: HashSet::with_capacity(count_strs),
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

pub(crate) fn err_op<T: Display, R>(l: &Val, op: T, r: &Val) -> Result<R> {
    Err(Error::Eval(format!("Invalid op: {l} {op} {r}")))
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl UnaryOp {
    pub(crate) fn eval(&self, expr: &Expr, ctx: &Val) -> Result<Val> {
        match self {
            UnaryOp::Not => {
                let res = expr.eval(ctx)?;
                match res {
                    Val::Bool(b) => Ok(Val::Bool(!b)),
                    _ => Err(Error::Eval(format!("Invalid operand: !{res}"))),
                }
            }
            UnaryOp::Neg => {
                let res = expr.eval(ctx)?;
                match res {
                    Val::Int(i) => i
                        .checked_neg()
                        .map(Val::Int)
                        .ok_or_else(|| Error::Eval(format!("Integer overflow: -{i}"))),
                    Val::Float(f) => Ok(Val::Float(-f)),
                    _ => Err(Error::Eval(format!("Invalid operand: -{res}"))),
                }
            }
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Neg => write!(f, "-"),
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

                    if r.len() > LARGE_LIST_MEMBERSHIP_THRESHOLD && l.len() > LIST_IN_LIST_INDEX_MIN_PROBES {
                        let index = MembershipIndex::new(r);
                        return Ok((**l).iter().all(|x| index.contains(x)));
                    }

                    Ok((**l).iter().all(|x| list_contains(r, x)))
                }

                // Single element membership. A linear scan is always optimal
                // here: a MembershipIndex costs O(n) up front (hashing every
                // element) and only pays off across many lookups, but each `in`
                // rebuilds it - so the indexed path was strictly slower than
                // scanning for a single needle.
                (_, Val::List(r)) => Ok(list_contains(r, l)),

                // Map key membership: "key" in {"key": value}
                (Val::Str(s), Val::Map(m)) => Ok(m.contains_key(s.as_ref())),

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
        let l_val = l.eval(ctx)?;
        let r_val = r.eval(ctx)?;

        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => self.arith(&l_val, &r_val),
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le | BinOp::In => {
                Ok(Val::Bool(self.cmp(&l_val, &r_val)?))
            }
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
