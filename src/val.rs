use std::collections::HashMap;

use anyhow::{anyhow, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Str(String),
    // Since most arch are 64 bit, we can use i64 for int
    Int(i64),
    Float(f64),
    Bool(bool),
    Map(Box<HashMap<String, Val>>),
    List(Box<Vec<Val>>),
    Nil,
}

impl Val {
    pub(crate) fn access(&self, field: &Val) -> Result<&Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => {
                if let Some(val) = m.get(s) {
                    return Ok(val)
                }
            }
            (Val::List(l), Val::Int(i)) => {
                if let Some(val) = l.get(*i as usize) {
                    return Ok(val)
                }
            }
            _ => {}
        }
        Err(anyhow!("Cant access field {} on {}", field, self))
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Val::Str(s)
    }
}

impl From<&str> for Val {
    fn from(s: &str) -> Self {
        Val::Str(s.to_string())
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

impl<V> From<HashMap<String, V>> for Val
where
    V: Into<Val>,
{
    fn from(m: HashMap<String, V>) -> Self {
        let inner = m.into_iter().map(|(k, v)| (k, v.into())).collect();
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
            serde_json::Value::String(s) => Val::Str(s),
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
            },
            serde_json::Value::Object(o) => {
                let m = o.into_iter().map(|(k, v)| (k, Val::from(v))).collect();
                Val::Map(Box::new(m))
            },
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
            (Val::Str(a), Val::Str(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl core::fmt::Display for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Val::Int(i) => write!(f, "{}", i),
            Val::Float(fl) => write!(f, "{}", fl),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Str(s) => write!(f, "{}", s),
            Val::Map(m) => write!(f, "{:?}", m),
            Val::List(l) => write!(f, "{:?}", l),
            Val::Nil => write!(f, "nil"),
        }
    }
}