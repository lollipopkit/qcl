use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
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
    pub fn access(&self, field: &Val) -> &Val {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => m.get(s).unwrap_or(&Val::Nil),
            (Val::List(l), Val::Int(i)) => l.get(*i as usize).unwrap_or(&Val::Nil),
            _ => &Val::Nil,
        }
    }
}
