use core::ops::{Add, Sub};
use std::{collections::HashMap, fmt::Debug, ops::{Div, Mul, Rem}};

use anyhow::{anyhow, Result};

use crate::expr::BinOp;

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Str(String),
    Int(i64), // Since most arch are 64 bit, we can use i64 for int
    Float(f64),
    Bool(bool),
    Map(Box<HashMap<String, Val>>),
    List(Box<Vec<Val>>),
    Nil,
}

pub(crate) fn err_op<T: Debug, R>(l: &Val, op: T, r: &Val) -> Result<R> {
    Err(anyhow!("Invalid operation: {l} {op:?} {r}"))
}

impl Add for &Val {
    type Output = Result<Val>;

    /// - Str + Num may leads to unexpected behavior.
    /// - List can + Val, but Val + List is not supported.
    /// - Map can + Map, but Map can't + Val, since the value of the map is not defined.
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok(Val::Int(a + b)),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a + b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a + *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 + b)),
            (Val::Str(a), Val::Str(b)) => {
                let mut res = String::with_capacity(a.len() + b.len());
                res.push_str(a);
                res.push_str(b);
                Ok(Val::Str(res))
            }
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
            (Val::List(l), r) => {
                let mut res = l.clone();
                res.push(r.clone());
                Ok(Val::List(res))
            }
            _ => err_op(self, BinOp::Add, other),
        }
    }
}

impl Sub for &Val {
    type Output = Result<Val>;

    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok(Val::Int(a - b)),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a - b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a - *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 - b)),
            (Val::List(l), Val::List(r)) => {
                // Semantically, remove vals both inside [l] and [r]
                let mut res = l.clone();
                for val in r.iter() {
                    if let Some(idx) = res.iter().position(|v| v == val) {
                        res.remove(idx);
                    }
                }
                Ok(Val::List(res))
            }
            (Val::List(l), r) => {
                let mut res = l.clone();
                if let Some(idx) = res.iter().position(|v| v == r) {
                    res.remove(idx);
                }
                Ok(Val::List(res))
            }
            (Val::Map(l), Val::Map(r)) => {
                let mut res = l.clone();
                for (k, _) in r.iter() {
                    res.remove(k);
                }
                Ok(Val::Map(res))
            }
            (Val::Map(l), r) => {
                if let Val::Str(k) = r {
                    let mut res = l.clone();
                    res.remove(k);
                    return Ok(Val::Map(res))
                }
                err_op(self, BinOp::Sub, other)
            }
            _ => err_op(self, BinOp::Sub, other),
        }
    }
}

impl Mul for &Val {
    type Output = Result<Val>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok(Val::Int(a * b)),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a * b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a * *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 * b)),
            _ => err_op(self, BinOp::Mul, other),
        }
    }
}

impl Div for &Val {
    type Output = Result<Val>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok(Val::Int(a / b)),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a / b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a / *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 / b)),
            _ => err_op(self, BinOp::Div, other),
        }
    }
}

impl Rem for &Val {
    type Output = Result<Val>;

    fn rem(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok(Val::Int(a % b)),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a % b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a % *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 % b)),
            _ => err_op(self, BinOp::Mod, other),
        }
    }
}

impl Val {
    pub(crate) fn access(&self, field: &Val) -> Result<&Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => {
                if let Some(val) = m.get(s) {
                    return Ok(val);
                }
            }
            (Val::List(l), Val::Int(i)) => {
                if let Some(val) = l.get(*i as usize) {
                    return Ok(val);
                }
            }
            _ => {}
        }
        Err(anyhow!("Cant access field {} on {}", field, self))
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Val::Str(s.into())
    }
}

impl From<&str> for Val {
    fn from(s: &str) -> Self {
        Val::Str(s.into())
    }
}

impl From<i64> for Val {
    fn from(i: i64) -> Self {
        Val::Int(i)
    }
}

impl From<f64> for Val {
    fn from(f: f64) -> Self {
        Val::Float(f)
    }
}

impl From<bool> for Val {
    fn from(b: bool) -> Self {
        Val::Bool(b)
    }
}

impl<V, S> From<HashMap<S, V>> for Val
where
    V: Into<Val>,
    S: AsRef<str>,
{
    fn from(m: HashMap<S, V>) -> Self {
        let inner = m
            .into_iter()
            .map(|(k, v)| (k.as_ref().into(), v.into()))
            .collect();
        Val::Map(Box::new(inner))
    }
}

impl<T> From<Vec<T>> for Val
where
    T: Into<Val>,
{
    fn from(v: Vec<T>) -> Self {
        let v = v.into_iter().map(Into::into).collect();
        Val::List(Box::new(v))
    }
}

impl<T> From<Option<T>> for Val
where
    T: Into<Val>,
{
    fn from(o: Option<T>) -> Self {
        match o {
            Some(v) => v.into(),
            None => Val::Nil,
        }
    }
}

impl From<()> for Val {
    fn from(_: ()) -> Self {
        Val::Nil
    }
}

impl From<serde_json::Value> for Val {
    fn from(val: serde_json::Value) -> Self {
        match val {
            serde_json::Value::String(s) => Val::Str(s.into()),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Val::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Val::Float(f)
                } else {
                    Val::Nil
                }
            }
            serde_json::Value::Bool(b) => Val::Bool(b),
            serde_json::Value::Array(a) => {
                let v = a.into_iter().map(Val::from).collect();
                Val::List(Box::new(v))
            }
            serde_json::Value::Object(o) => {
                let m = o
                    .into_iter()
                    .map(|(k, v)| (k.into(), Val::from(v)))
                    .collect();
                Val::Map(Box::new(m))
            }
            serde_json::Value::Null => Val::Nil,
        }
    }
}

impl Default for Val {
    fn default() -> Self {
        Val::Nil
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => a.partial_cmp(b),
            (Val::Float(a), Val::Float(b)) => a.partial_cmp(b),
            (Val::Int(a), Val::Float(b)) => (*a as f64).partial_cmp(b),
            (Val::Float(a), Val::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Val::Str(a), Val::Str(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl core::fmt::Display for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Val::Int(i) => write!(f, "{i}"),
            Val::Float(fl) => write!(f, "{fl}"),
            Val::Bool(b) => write!(f, "{b}"),
            Val::Str(s) => write!(f, "{s}"),
            Val::Map(m) => write!(f, "{m:?}"),
            Val::List(l) => write!(f, "{l:?}"),
            Val::Nil => write!(f, "nil"),
        }
    }
}
