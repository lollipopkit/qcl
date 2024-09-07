use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Val {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Map(HashMap<String, Val>),
    List(Vec<Val>),
    Nil,
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

impl Val {
    pub(crate) fn access(&self, field: &Val) -> Option<&Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => m.get(s),
            (Val::List(l), Val::Int(i)) => l.get(*i as usize),
            _ => None,
        }
    }

    pub fn from_json(val: serde_json::Value) -> Val {
        match val {
            serde_json::Value::String(s) => Val::Str(s),
            serde_json::Value::Number(n) => {
                if n.is_i64() {
                    Val::Int(n.as_i64().unwrap())
                } else if n.is_f64() {
                    Val::Float(n.as_f64().unwrap())
                } else {
                    Val::Nil
                }
            }
            serde_json::Value::Bool(b) => Val::Bool(b),
            serde_json::Value::Array(a) => Val::List(a.into_iter().map(Val::from_json).collect()),
            serde_json::Value::Object(o) => Val::Map(
                o.into_iter()
                    .map(|(k, v)| (k, Val::from_json(v)))
                    .collect(),
            ),
            serde_json::Value::Null => Val::Nil,
        }
    }
}
