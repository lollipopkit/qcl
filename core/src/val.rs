use core::ops::{Add, Sub};
use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Div, Mul, Rem},
    sync::Arc,
};

use crate::concurrency::{Channel, GoroutineHandle};

use anyhow::{Result, anyhow};
use serde::{Serialize, Serializer};

use crate::op::{BinOp, err_op};

/// Type for Rust functions that can be called from QCL
pub type RustFunction = fn(args: &[Val], env: &crate::stmt::Environment, ctx: &Val) -> Result<Val>;

#[derive(Debug, Clone, Default)]
pub enum Val {
    /// String type, wrapped in Arc<str> for efficient cloning
    Str(Arc<str>),
    Int(i64), // Since most arch are 64 bit, we can use i64 for int
    Float(f64),
    Bool(bool),
    /// Map type, wrapped in Arc<HashMap> to avoid deep cloning
    Map(Arc<HashMap<String, Val>>),
    /// List type, wrapped in Arc<Vec> for efficient cloning
    List(Arc<Vec<Val>>),
    /// Closure - contains parameters and body with captured environment
    Closure {
        params: Arc<Vec<String>>,
        body: Arc<crate::stmt::Stmt>,
        /// Captured environment for closure support
        env: Arc<crate::stmt::Environment>,
    },
    /// Rust function - contains a function pointer that can be called
    RustFunction(RustFunction),
    /// Channel for goroutine communication
    Channel(Channel),
    /// Goroutine handle for managing spawned tasks
    Goroutine(GoroutineHandle),
    #[default]
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    List,
    Map,
    Function,
    Channel,
    Goroutine,
    Nil,
}

impl Type {
    pub fn parse(s: &str) -> Option<Type> {
        match s {
            "Int" => Some(Type::Int),
            "Float" => Some(Type::Float),
            "String" => Some(Type::String),
            "Bool" => Some(Type::Bool),
            "List" => Some(Type::List),
            "Map" => Some(Type::Map),
            "Function" => Some(Type::Function),
            "Channel" => Some(Type::Channel),
            "Goroutine" => Some(Type::Goroutine),
            "Nil" => Some(Type::Nil),
            _ => None,
        }
    }

    pub fn validate(&self, val: &Val) -> Result<()> {
        let matches = matches!(
            (self, val),
            (Type::Int, Val::Int(_))
                | (Type::Float, Val::Float(_))
                | (Type::String, Val::Str(_))
                | (Type::Bool, Val::Bool(_))
                | (Type::List, Val::List(_))
                | (Type::Map, Val::Map(_))
                | (Type::Function, Val::Closure { .. } | Val::RustFunction(_))
                | (Type::Channel, Val::Channel(_))
                | (Type::Goroutine, Val::Goroutine(_))
                | (Type::Nil, Val::Nil)
        );

        if matches {
            Ok(())
        } else {
            Err(anyhow!(
                "Type mismatch: expected {:?}, got {:?}",
                self,
                val.type_name()
            ))
        }
    }
}

impl Val {
    pub fn type_name(&self) -> &'static str {
        match self {
            Val::Str(_) => "String",
            Val::Int(_) => "Int",
            Val::Float(_) => "Float",
            Val::Bool(_) => "Bool",
            Val::Map(_) => "Map",
            Val::List(_) => "List",
            Val::Closure { .. } => "Function",
            Val::RustFunction(_) => "Function",
            Val::Channel(_) => "Channel",
            Val::Goroutine(_) => "Goroutine",
            Val::Nil => "Nil",
        }
    }

    /// Call this value as a function with the given arguments
    pub fn call(&self, args: &[Val], env: &crate::stmt::Environment, ctx: &Val) -> Result<Val> {
        match self {
            Val::Closure {
                params,
                body,
                env: _,
            } => {
                // Check parameter count
                if args.len() != params.len() {
                    return Err(anyhow!(
                        "Function expects {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // Create new scope for function execution using the provided environment
                let mut call_env = env.clone();
                call_env.push_scope();

                // Bind parameters to arguments
                for (param, arg_val) in params.iter().zip(args.iter()) {
                    call_env.define(param.clone(), arg_val.clone());
                }

                // Execute function body
                match body.execute(&mut call_env, ctx)? {
                    crate::stmt::ControlFlow::Return(val) => Ok(val),
                    _ => Ok(Val::Nil), // Functions return nil by default
                }
            }
            Val::RustFunction(func) => {
                // Call the Rust function directly
                func(args, env, ctx)
            }
            _ => Err(anyhow!("{} is not a function", self.type_name())),
        }
    }
    pub(crate) fn access(&self, field: &Val) -> Option<&Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => m.get(s.as_ref()),
            (Val::List(l), Val::Int(i)) => {
                if *i < 0 {
                    return None;
                }
                l.get(*i as usize)
            }
            _ => None,
        }
    }

    /// Efficient string concatenation using Cow to avoid intermediate allocations
    fn concat_strings(a: &str, b: &str) -> Val {
        if a.is_empty() {
            Val::Str(Arc::from(b))
        } else if b.is_empty() {
            Val::Str(Arc::from(a))
        } else {
            let mut result = String::with_capacity(a.len() + b.len());
            result.push_str(a);
            result.push_str(b);
            Val::Str(Arc::from(result.as_str()))
        }
    }
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
            (Val::Str(a), Val::Str(b)) => Ok(Val::concat_strings(a.as_ref(), b.as_ref())),
            #[cfg(feature = "adv_arith")]
            (Val::Str(a), Val::Int(b)) => {
                let b_str = b.to_string();
                Ok(Val::concat_strings(a.as_ref(), &b_str))
            }
            #[cfg(feature = "adv_arith")]
            (Val::Str(a), Val::Float(b)) => {
                let b_str = b.to_string();
                Ok(Val::concat_strings(a.as_ref(), &b_str))
            }
            #[cfg(feature = "adv_arith")]
            (Val::Int(a), Val::Str(b)) => {
                let a_str = a.to_string();
                Ok(Val::concat_strings(&a_str, b.as_ref()))
            }
            #[cfg(feature = "adv_arith")]
            (Val::Float(a), Val::Str(b)) => {
                let a_str = a.to_string();
                Ok(Val::concat_strings(&a_str, b.as_ref()))
            }
            #[cfg(feature = "adv_arith")]
            (Val::Map(l), Val::Map(r)) => {
                // Map + Map: merge with right side overriding left side for same keys
                // Use with_capacity for better performance
                let mut merged = HashMap::with_capacity(l.len() + r.len());
                // First insert all from left map
                for (k, v) in l.iter() {
                    merged.insert(k.clone(), v.clone());
                }
                // Then insert from right map (overriding duplicates)
                for (k, v) in r.iter() {
                    merged.insert(k.clone(), v.clone());
                }
                Ok(merged.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::List(l), Val::List(r)) => {
                // Use with_capacity for better performance
                let mut merged = Vec::with_capacity(l.len() + r.len());
                merged.extend(l.iter().cloned());
                merged.extend(r.iter().cloned());
                Ok(merged.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::List(l), r) => {
                // Use with_capacity for better performance
                let mut new_list = Vec::with_capacity(l.len() + 1);
                new_list.extend(l.iter().cloned());
                new_list.push(r.clone());
                Ok(new_list.into())
            }
            _ => err_op(self, BinOp::Add, other),
        }
    }
}

impl Sub for &Val {
    type Output = Result<Val>;

    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok((a - b).into()),
            (Val::Float(a), Val::Float(b)) => Ok((a - b).into()),
            (Val::Float(a), Val::Int(b)) => Ok((a - *b as f64).into()),
            (Val::Int(a), Val::Float(b)) => Ok((*a as f64 - b).into()),
            #[cfg(feature = "adv_arith")]
            (Val::List(l), Val::List(r)) => {
                let mut result = Vec::with_capacity(l.len());
                'outer: for left_val in l.iter() {
                    for right_val in r.iter() {
                        if left_val == right_val {
                            continue 'outer; // Skip this element
                        }
                    }
                    result.push(left_val.clone());
                }
                Ok(result.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::List(l), r) => {
                let mut result = Vec::with_capacity(l.len());
                let mut found = false;
                for val in l.iter() {
                    if !found && val == r {
                        found = true; // Skip first occurrence
                        continue;
                    }
                    result.push(val.clone());
                }
                Ok(result.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::Map(l), Val::Map(r)) => {
                let mut result = HashMap::with_capacity(l.len());
                for (k, v) in l.iter() {
                    if !r.contains_key(k) {
                        result.insert(k.clone(), v.clone());
                    }
                }
                Ok(result.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::Map(l), r) => {
                if let Val::Str(k) = r {
                    let mut result = HashMap::with_capacity(l.len());
                    for (existing_k, v) in l.iter() {
                        if existing_k != k.as_ref() {
                            result.insert(existing_k.clone(), v.clone());
                        }
                    }
                    return Ok(result.into());
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
            (Val::Int(a), Val::Int(b)) => Ok((a * b).into()),
            (Val::Float(a), Val::Float(b)) => Ok((a * b).into()),
            (Val::Float(a), Val::Int(b)) => Ok((a * *b as f64).into()),
            (Val::Int(a), Val::Float(b)) => Ok((*a as f64 * b).into()),
            _ => err_op(self, BinOp::Mul, other),
        }
    }
}

impl Div for &Val {
    type Output = Result<Val>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            #[cfg(feature = "sem_arith")]
            (Val::Int(a), Val::Int(b)) => {
                let res = (*a as f64) / (*b as f64);
                if res.fract() == 0.0 {
                    Ok((res as i64).into())
                } else {
                    Ok(res.into())
                }
            }
            #[cfg(not(feature = "sem_arith"))]
            (Val::Int(a), Val::Int(b)) => Ok((a / b).into()),
            (Val::Float(a), Val::Float(b)) => Ok((a / b).into()),
            (Val::Float(a), Val::Int(b)) => Ok((a / *b as f64).into()),
            (Val::Int(a), Val::Float(b)) => Ok((*a as f64 / b).into()),
            _ => err_op(self, BinOp::Div, other),
        }
    }
}

impl Rem for &Val {
    type Output = Result<Val>;

    fn rem(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => Ok((a % b).into()),
            (Val::Float(a), Val::Float(b)) => Ok((a % b).into()),
            (Val::Float(a), Val::Int(b)) => Ok((a % *b as f64).into()),
            (Val::Int(a), Val::Float(b)) => Ok((*a as f64 % b).into()),
            _ => err_op(self, BinOp::Mod, other),
        }
    }
}

impl From<String> for Val {
    #[inline]
    fn from(s: String) -> Self {
        Val::Str(Arc::from(s.as_str()))
    }
}

impl From<&str> for Val {
    #[inline]
    fn from(s: &str) -> Self {
        Val::Str(Arc::from(s))
    }
}

impl From<i64> for Val {
    #[inline]
    fn from(i: i64) -> Self {
        Val::Int(i)
    }
}

impl From<f64> for Val {
    #[inline]
    fn from(f: f64) -> Self {
        Val::Float(f)
    }
}

impl From<bool> for Val {
    #[inline]
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
            .map(|(k, v)| (k.as_ref().to_string(), v.into()))
            .collect();
        Val::Map(Arc::new(inner))
    }
}

impl<T> From<Vec<T>> for Val
where
    T: Into<Val>,
{
    fn from(v: Vec<T>) -> Self {
        let v = v.into_iter().map(Into::into).collect();
        Val::List(Arc::new(v))
    }
}

impl<T> From<Box<T>> for Val
where
    T: Into<Val>,
{
    fn from(b: Box<T>) -> Self {
        (*b).into()
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

// Clone is derived for Val enum

#[cfg(feature = "json")]
impl From<serde_json::Value> for Val {
    fn from(val: serde_json::Value) -> Self {
        match val {
            serde_json::Value::String(s) => Val::Str(Arc::from(s.as_str())),
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
                Val::List(Arc::new(v))
            }
            serde_json::Value::Object(o) => {
                let m = o.into_iter().map(|(k, v)| (k, Val::from(v))).collect();
                Val::Map(Arc::new(m))
            }
            serde_json::Value::Null => Val::Nil,
        }
    }
}

#[cfg(feature = "yaml")]
impl From<serde_yaml::Value> for Val {
    fn from(val: serde_yaml::Value) -> Self {
        match val {
            serde_yaml::Value::String(s) => Val::Str(Arc::from(s.as_str())),
            serde_yaml::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Val::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Val::Float(f)
                } else {
                    Val::Nil
                }
            }
            serde_yaml::Value::Bool(b) => Val::Bool(b),
            serde_yaml::Value::Sequence(a) => {
                let v = a.into_iter().map(Val::from).collect();
                Val::List(Arc::new(v))
            }
            serde_yaml::Value::Mapping(o) => {
                let m = o
                    .into_iter()
                    .filter_map(|(k, v)| {
                        if let serde_yaml::Value::String(key) = k {
                            Some((key, Val::from(v)))
                        } else {
                            None
                        }
                    })
                    .collect();
                Val::Map(Arc::new(m))
            }
            serde_yaml::Value::Null => Val::Nil,
            serde_yaml::Value::Tagged(tagged) => Val::from(tagged.value),
        }
    }
}

impl Val {
    #[cfg(feature = "json")]
    pub fn try_from<T>(val: T) -> Result<Self>
    where
        T: serde::Serialize,
    {
        Ok(serde_json::to_value(val)?.into())
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Val::Str(a), Val::Str(b)) => a == b,
            (Val::Int(a), Val::Int(b)) => a == b,
            (Val::Float(a), Val::Float(b)) => a == b,
            (Val::Bool(a), Val::Bool(b)) => a == b,
            (Val::Map(a), Val::Map(b)) => a == b,
            (Val::List(a), Val::List(b)) => a == b,
            (
                Val::Closure {
                    params: params_a,
                    body: body_a,
                    env: env_a,
                },
                Val::Closure {
                    params: params_b,
                    body: body_b,
                    env: env_b,
                },
            ) => params_a == params_b && Arc::ptr_eq(body_a, body_b) && Arc::ptr_eq(env_a, env_b),
            (Val::RustFunction(a), Val::RustFunction(b)) => {
                // Use fn_addr_eq for meaningful function pointer comparison
                std::ptr::fn_addr_eq(*a, *b)
            }
            (Val::Channel(a), Val::Channel(b)) => a == b,
            (Val::Goroutine(a), Val::Goroutine(b)) => a == b,
            (Val::Nil, Val::Nil) => true,
            _ => false,
        }
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

impl Serialize for Val {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Val::Str(s) => serializer.serialize_str(s.as_ref()),
            Val::Int(i) => serializer.serialize_i64(*i),
            Val::Float(f) => serializer.serialize_f64(*f),
            Val::Bool(b) => serializer.serialize_bool(*b),
            Val::Map(m) => (**m).serialize(serializer),
            Val::List(l) => (**l).serialize(serializer),
            Val::Closure { .. } | Val::RustFunction(_) => {
                // Functions can't be serialized, use placeholder
                serializer.serialize_str("<function>")
            }
            Val::Channel(_) => {
                // Channels can't be serialized, use placeholder
                serializer.serialize_str("<channel>")
            }
            Val::Goroutine(g) => {
                // Serialize goroutine as its ID
                serializer.serialize_str(&format!("<goroutine:{}>", g.id()))
            }
            Val::Nil => serializer.serialize_unit(),
        }
    }
}

impl core::fmt::Display for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Val::Int(i) => write!(f, "{i}"),
            Val::Float(fl) => write!(f, "{fl}"),
            Val::Bool(b) => write!(f, "{b}"),
            Val::Str(s) => write!(f, "{}", s.as_ref()),
            Val::Map(m) => {
                // Avoid serialization errors by using debug fallback
                #[cfg(feature = "json")]
                match serde_json::to_string(&**m) {
                    Ok(s) => write!(f, "{}", s),
                    Err(_) => write!(f, "{:?}", m),
                }
                #[cfg(not(feature = "json"))]
                write!(f, "{:?}", m)
            }
            Val::List(l) => {
                #[cfg(feature = "json")]
                match serde_json::to_string(&**l) {
                    Ok(s) => write!(f, "{}", s),
                    Err(_) => write!(f, "{:?}", l),
                }
                #[cfg(not(feature = "json"))]
                write!(f, "{:?}", l)
            }
            Val::Closure { params, .. } => {
                write!(f, "fn({})", params.join(", "))
            }
            Val::RustFunction(_) => {
                write!(f, "<native function>")
            }
            Val::Channel(_) => {
                write!(f, "<channel>")
            }
            Val::Goroutine(g) => {
                write!(f, "<goroutine:{}>", g.id())
            }
            Val::Nil => write!(f, "nil"),
        }
    }
}
