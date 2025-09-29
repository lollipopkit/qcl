use core::ops::{Add, Sub};
use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Div, Mul, Rem},
    sync::Arc,
};

use anyhow::{Result, anyhow};
use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};

use crate::op::{BinOp, err_op};

/// Type for Rust functions that can be called from QCL
pub type RustFunction = fn(args: &[Val], env: &crate::stmt::Environment, ctx: &Val) -> Result<Val>;

#[derive(Debug, Default)]
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
    /// Task - represents a concurrent task
    Task {
        id: u64,
        /// The result value if task is completed (for non-concurrent mode)
        value: Option<Box<Val>>,
    },
    /// Channel - represents a communication channel
    Channel {
        id: u64,
        capacity: Option<i64>,
        inner_type: Box<Type>,
    },
    /// Runtime object with named type and fields
    Object {
        type_name: Arc<str>,
        fields: Arc<HashMap<String, Val>>,
    },
    #[default]
    Nil,
}

impl Clone for Val {
    fn clone(&self) -> Self {
        match self {
            Val::Str(s) => Val::Str(s.clone()),
            Val::Int(i) => Val::Int(*i),
            Val::Float(f) => Val::Float(*f),
            Val::Bool(b) => Val::Bool(*b),
            Val::Map(m) => Val::Map(m.clone()),
            Val::List(l) => Val::List(l.clone()),
            Val::Closure { params, body, env } => Val::Closure {
                params: params.clone(),
                body: body.clone(),
                env: env.clone(),
            },
            Val::RustFunction(f) => Val::RustFunction(*f),
            Val::Task { id, value } => {
                // For tasks, we clone the ID and value
                Val::Task {
                    id: *id,
                    value: value.clone(),
                }
            }
            Val::Channel {
                id,
                capacity,
                inner_type,
            } => Val::Channel {
                id: *id,
                capacity: *capacity,
                inner_type: inner_type.clone(),
            },
            Val::Object { type_name, fields } => Val::Object {
                type_name: type_name.clone(),
                fields: fields.clone(),
            },
            Val::Nil => Val::Nil,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive types
    Int,
    Float,
    String,
    Bool,
    Nil,

    /// Generic container types
    List(Box<Type>), // List<T>
    Map(Box<Type>, Box<Type>), // Map<K, V>

    /// Function type with parameters and return type
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    /// Concurrency types
    Task(Box<Type>),
    Channel(Box<Type>),

    /// Union types: Int | String
    Union(Vec<Type>),

    /// Optional types: ?Int (sugar for Int | Nil)
    Optional(Box<Type>),

    /// Type variables for inference (prefixed with ')
    Variable(String),

    /// Custom named types
    Named(String),

    /// Generic type with parameters: List<T>, Map<K, V>
    Generic {
        name: String,
        params: Vec<Type>,
    },

    /// Any type (top type)
    Any,
}

impl Type {
    pub fn parse(s: &str) -> Option<Type> {
        let s = s.trim();

        // Handle primitive types
        match s {
            "Int" => return Some(Type::Int),
            "Float" => return Some(Type::Float),
            "String" => return Some(Type::String),
            "Bool" => return Some(Type::Bool),
            "Nil" => return Some(Type::Nil),
            "Any" => return Some(Type::Any),
            _ => {}
        }

        // Handle optional types: ?Int
        if let Some(inner) = s.strip_prefix('?') {
            return Type::parse(inner).map(|t| Type::Optional(Box::new(t)));
        }

        // Handle type variables: 'T, 'K, 'V
        if s.starts_with('\'') && s.len() > 1 {
            return Some(Type::Variable(s[1..].to_string()));
        }

        // Handle union types: Int | String | Nil
        if s.contains(" | ") {
            let types: Vec<Type> = s.split(" | ").filter_map(Type::parse).collect();
            if !types.is_empty() {
                return Some(Type::Union(types));
            }
        }

        // Handle generic types with angle brackets
        if let Some(open) = s.find('<')
            && let Some(close) = s.rfind('>')
        {
            let base = &s[..open];
            let params_str = &s[open + 1..close];

            // Parse type parameters
            let params: Vec<Type> = if params_str.is_empty() {
                vec![]
            } else {
                params_str
                    .split(',')
                    .map(str::trim)
                    .filter_map(Type::parse)
                    .collect()
            };

            // Handle specific generic types
            match base {
                "List" => {
                    if params.len() == 1 {
                        return Some(Type::List(Box::new(params[0].clone())));
                    }
                }
                "Map" => {
                    if params.len() == 2 {
                        return Some(Type::Map(
                            Box::new(params[0].clone()),
                            Box::new(params[1].clone()),
                        ));
                    }
                }
                "Task" => {
                    if params.len() == 1 {
                        return Some(Type::Task(Box::new(params[0].clone())));
                    }
                }
                "Channel" => {
                    if params.len() == 1 {
                        return Some(Type::Channel(Box::new(params[0].clone())));
                    }
                }
                _ => {
                    // Generic custom type
                    return Some(Type::Generic {
                        name: base.to_string(),
                        params,
                    });
                }
            }
        }

        // Handle function types: (Int, String) -> Bool
        if s.contains("->") {
            let parts: Vec<&str> = s.splitn(2, "->").collect();
            if parts.len() == 2 {
                let params_str = parts[0].trim();
                let return_str = parts[1].trim();

                // Parse parameters
                let params = if params_str.starts_with('(') && params_str.ends_with(')') {
                    let inner = &params_str[1..params_str.len() - 1];
                    if inner.is_empty() {
                        vec![]
                    } else {
                        inner
                            .split(',')
                            .map(str::trim)
                            .filter_map(Type::parse)
                            .collect()
                    }
                } else {
                    vec![]
                };

                // Parse return type
                if let Some(return_type) = Type::parse(return_str) {
                    return Some(Type::Function {
                        params,
                        return_type: Box::new(return_type),
                    });
                }
            }
        }

        // Handle bare List and Map as generic types
        match s {
            "List" => Some(Type::List(Box::new(Type::Any))),
            "Map" => Some(Type::Map(Box::new(Type::Any), Box::new(Type::Any))),
            _ => {
                // Assume it's a named custom type
                if s.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    Some(Type::Named(s.to_string()))
                } else {
                    None
                }
            }
        }
    }

    pub fn validate(&self, val: &Val) -> Result<()> {
        match (self, val) {
            // Primitive types
            (Type::Int, Val::Int(_)) => Ok(()),
            (Type::Float, Val::Float(_)) => Ok(()),
            (Type::String, Val::Str(_)) => Ok(()),
            (Type::Bool, Val::Bool(_)) => Ok(()),
            (Type::Nil, Val::Nil) => Ok(()),

            // Any type accepts everything
            (Type::Any, _) => Ok(()),

            // Generic container types
            (Type::List(elem_type), Val::List(list)) => {
                // Validate all elements match the expected type
                for item in list.iter() {
                    elem_type.validate(item)?;
                }
                Ok(())
            }
            (Type::Map(key_type, val_type), Val::Map(map)) => {
                // Validate all keys and values match expected types
                for (k, v) in map.iter() {
                    let key_val = Val::Str(k.as_str().into());
                    key_type.validate(&key_val)?;
                    val_type.validate(v)?;
                }
                Ok(())
            }

            // Function types
            (Type::Function { .. }, Val::Closure { .. }) => Ok(()),
            (Type::Function { .. }, Val::RustFunction(_)) => Ok(()),

            // Concurrency types
            (Type::Task(inner_type), Val::Task { value, .. }) => {
                if let Some(v) = value {
                    inner_type.validate(v)?;
                }
                Ok(())
            }
            (
                Type::Channel(inner_type),
                Val::Channel {
                    inner_type: actual_type,
                    ..
                },
            ) => {
                if inner_type.as_ref() == actual_type.as_ref() {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "Channel type mismatch: expected {:?}, got {:?}",
                        inner_type,
                        actual_type
                    ))
                }
            }

            // Union types - value must match at least one type in the union
            (Type::Union(types), val) => {
                for typ in types {
                    if typ.validate(val).is_ok() {
                        return Ok(());
                    }
                }
                Err(anyhow!(
                    "Union type mismatch: value {:?} doesn't match any of {:?}",
                    val.type_name(),
                    types
                ))
            }

            // Optional types - value must be Nil or match the inner type
            (Type::Optional(_inner_type), Val::Nil) => Ok(()),
            (Type::Optional(inner_type), val) => inner_type.validate(val),

            // Type variables and named types are handled by the type checker
            (Type::Variable(_), _) => Ok(()), // Always valid during inference
            (Type::Named(_), _) => Ok(()),    // Validated by type registry

            // Generic types are validated by the type system
            (Type::Generic { .. }, _) => Ok(()),

            // Type mismatch
            (expected, actual) => Err(anyhow!(
                "Type mismatch: expected {:?}, got {:?}",
                expected,
                actual.type_name()
            )),
        }
    }

    /// Get a display representation of the type
    pub fn display(&self) -> String {
        match self {
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Nil => "Nil".to_string(),
            Type::Any => "Any".to_string(),
            Type::List(elem) => format!("List<{}>", elem.display()),
            Type::Map(k, v) => format!("Map<{}, {}>", k.display(), v.display()),
            Type::Function {
                params,
                return_type,
            } => {
                let param_strs: Vec<String> = params.iter().map(|p| p.display()).collect();
                format!("({}) -> {}", param_strs.join(", "), return_type.display())
            }
            Type::Task(inner) => format!("Task<{}>", inner.display()),
            Type::Channel(inner) => format!("Channel<{}>", inner.display()),
            Type::Union(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| t.display()).collect();
                type_strs.join(" | ")
            }
            Type::Optional(inner) => format!("?{}", inner.display()),
            Type::Variable(name) => format!("'{}", name),
            Type::Named(name) => name.clone(),
            Type::Generic { name, params } => {
                if params.is_empty() {
                    name.clone()
                } else {
                    let param_strs: Vec<String> = params.iter().map(|p| p.display()).collect();
                    format!("{}<{}>", name, param_strs.join(", "))
                }
            }
        }
    }

    /// Check if this type can be assigned to another type (subtyping)
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            // Any type is assignable to Any
            (_, Type::Any) => true,
            // Same types are assignable
            (a, b) if a == b => true,
            // Optional types: T is assignable to ?T
            (inner, Type::Optional(expected_inner)) => inner.is_assignable_to(expected_inner),
            // Union types: T is assignable to Union if T is assignable to any member
            (t, Type::Union(union_types)) => union_types.iter().any(|ut| t.is_assignable_to(ut)),
            // Union member is assignable to union
            (Type::Union(union_types), target) => {
                union_types.iter().all(|ut| ut.is_assignable_to(target))
            }
            // Generic containers with covariant element types
            (Type::List(a), Type::List(b)) => a.is_assignable_to(b),
            (Type::Map(ak, av), Type::Map(bk, bv)) => {
                ak.is_assignable_to(bk) && av.is_assignable_to(bv)
            }
            // Function types (contravariant parameters, covariant return)
            (
                Type::Function {
                    params: a_params,
                    return_type: a_ret,
                },
                Type::Function {
                    params: b_params,
                    return_type: b_ret,
                },
            ) => {
                if a_params.len() != b_params.len() {
                    false
                } else {
                    // Parameters are contravariant
                    let params_compatible = b_params
                        .iter()
                        .zip(a_params.iter())
                        .all(|(b_param, a_param)| b_param.is_assignable_to(a_param));
                    // Return type is covariant
                    let return_compatible = a_ret.is_assignable_to(b_ret);
                    params_compatible && return_compatible
                }
            }
            // Concurrency types
            (Type::Task(a), Type::Task(b)) => a.is_assignable_to(b),
            (Type::Channel(a), Type::Channel(b)) => a.is_assignable_to(b),
            // No other assignability rules
            _ => false,
        }
    }

    /// Check if this type contains any type variables
    pub fn contains_variables(&self) -> bool {
        match self {
            Type::Variable(_) => true,
            Type::List(inner)
            | Type::Optional(inner)
            | Type::Task(inner)
            | Type::Channel(inner) => inner.contains_variables(),
            Type::Map(k, v) => k.contains_variables() || v.contains_variables(),
            Type::Function {
                params,
                return_type,
            } => params.iter().any(|p| p.contains_variables()) || return_type.contains_variables(),
            Type::Union(types) => types.iter().any(|t| t.contains_variables()),
            Type::Generic { params, .. } => params.iter().any(|p| p.contains_variables()),
            _ => false,
        }
    }

    /// Substitute type variables with concrete types
    pub fn substitute(&self, substitutions: &HashMap<String, Type>) -> Type {
        match self {
            Type::Variable(name) => substitutions
                .get(name)
                .cloned()
                .unwrap_or_else(|| self.clone()),
            Type::List(inner) => Type::List(Box::new(inner.substitute(substitutions))),
            Type::Map(k, v) => Type::Map(
                Box::new(k.substitute(substitutions)),
                Box::new(v.substitute(substitutions)),
            ),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params.iter().map(|p| p.substitute(substitutions)).collect(),
                return_type: Box::new(return_type.substitute(substitutions)),
            },
            Type::Optional(inner) => Type::Optional(Box::new(inner.substitute(substitutions))),
            Type::Task(inner) => Type::Task(Box::new(inner.substitute(substitutions))),
            Type::Channel(inner) => Type::Channel(Box::new(inner.substitute(substitutions))),
            Type::Union(types) => {
                Type::Union(types.iter().map(|t| t.substitute(substitutions)).collect())
            }
            Type::Generic { name, params } => Type::Generic {
                name: name.clone(),
                params: params.iter().map(|p| p.substitute(substitutions)).collect(),
            },
            _ => self.clone(),
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
            Val::Task { .. } => "Task",
            Val::Channel { .. } => "Channel",
            Val::Object { .. } => "Object",
            Val::Nil => "Nil",
        }
    }

    /// Construct a runtime object of a named custom type
    pub fn object<T: AsRef<str>>(type_name: T, fields: HashMap<String, Val>) -> Val {
        Val::Object {
            type_name: Arc::from(type_name.as_ref()),
            fields: Arc::new(fields),
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
    pub(crate) fn access(&self, field: &Val) -> Option<Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => m.get(s.as_ref()).cloned(),
            (Val::List(l), Val::Int(i)) => {
                if *i < 0 {
                    return None;
                }
                l.get(*i as usize).cloned()
            }
            (Val::List(l), Val::Str(s)) if s.as_ref() == "len" => Some(Val::Int(l.len() as i64)),
            (Val::Str(s), Val::Str(field)) if field.as_ref() == "len" => {
                Some(Val::Int(s.len() as i64))
            }
            (Val::Object { fields, .. }, Val::Str(s)) => fields.get(s.as_ref()).cloned(),
            (Val::Task { value, .. }, Val::Str(s)) if s.as_ref() == "value" => match value {
                Some(v) => Some((**v).clone()),
                None => Some(Val::Nil),
            },
            (
                Val::Channel {
                    capacity,
                    inner_type,
                    ..
                },
                Val::Str(s),
            ) => match s.as_ref() {
                "capacity" => Some(Val::Int(capacity.unwrap_or(0))),
                "type" => Some(Val::Str(format!("{:?}", inner_type).into())),
                _ => None,
            },
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

impl From<(u64, Val)> for Val {
    fn from((id, value): (u64, Val)) -> Self {
        Val::Task {
            id,
            value: Some(Box::new(value)),
        }
    }
}

impl From<(u64, i64, Type)> for Val {
    fn from((id, capacity, inner_type): (u64, i64, Type)) -> Self {
        Val::Channel {
            id,
            capacity: Some(capacity),
            inner_type: Box::new(inner_type),
        }
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
            (
                Val::Task {
                    id: id_a,
                    value: value_a,
                },
                Val::Task {
                    id: id_b,
                    value: value_b,
                },
            ) => id_a == id_b && value_a == value_b,
            (
                Val::Channel {
                    id: id_a,
                    capacity: cap_a,
                    inner_type: type_a,
                },
                Val::Channel {
                    id: id_b,
                    capacity: cap_b,
                    inner_type: type_b,
                },
            ) => id_a == id_b && cap_a == cap_b && type_a == type_b,
            (
                Val::Object {
                    type_name: t1,
                    fields: f1,
                },
                Val::Object {
                    type_name: t2,
                    fields: f2,
                },
            ) => t1 == t2 && f1 == f2,
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
            Val::Task { value, .. } => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("type", "task")?;
                map.serialize_entry("value", value)?;
                map.end()
            }
            Val::Channel {
                capacity,
                inner_type,
                ..
            } => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "channel")?;
                map.serialize_entry("capacity", capacity)?;
                map.serialize_entry("inner_type", &format!("{:?}", inner_type))?;
                map.end()
            }
            Val::Object { type_name, fields } => {
                let mut map = serializer.serialize_map(Some(fields.len() + 1))?;
                map.serialize_entry("__type", type_name.as_ref())?;
                for (k, v) in fields.iter() {
                    map.serialize_entry(k, v)?;
                }
                map.end()
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
            Val::Task { id, value } => match value {
                Some(v) => write!(f, "Task(id={}, value={})", id, v),
                None => write!(f, "Task(id={}, pending)", id),
            },
            Val::Channel {
                id,
                capacity,
                inner_type,
            } => {
                write!(
                    f,
                    "Channel(id={}, capacity={}, type={:?})",
                    id,
                    capacity.unwrap_or(0),
                    inner_type
                )
            }
            Val::Object { type_name, fields } => {
                write!(f, "Object(type={}, fields={:?})", type_name, fields)
            }
            Val::Nil => write!(f, "nil"),
        }
    }
}
