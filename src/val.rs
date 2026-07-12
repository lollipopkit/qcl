use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use core::{
    fmt::Debug,
    ops::{Add, Div, Mul, Rem, Sub},
};
use hashbrown::HashMap;

use crate::{
    error::{Error, Result},
    op::{BinOp, err_op},
};
use serde::{Serialize, Serializer};

#[cfg(feature = "adv_arith")]
use hashbrown::{HashSet, hash_map::Entry};

#[derive(Debug, Clone, PartialEq, Default)]
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
    /// Nil represents the absence of a value, similar to `null` in JSON or `None` in Rust.
    #[default]
    Nil,
}

/// Formats an `i64` into a fixed stack buffer so a map lookup by integer index
/// (`a.1`) can borrow a `&str` key without allocating. i64 decimal is at most
/// 20 bytes ("-9223372036854775808").
struct IntKey {
    buf: [u8; 20],
    len: usize,
}

impl core::fmt::Write for IntKey {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let bytes = s.as_bytes();
        let end = self.len + bytes.len();
        if end > self.buf.len() {
            return Err(core::fmt::Error);
        }
        self.buf[self.len..end].copy_from_slice(bytes);
        self.len = end;
        Ok(())
    }
}

impl IntKey {
    #[inline]
    fn new(n: i64) -> Self {
        use core::fmt::Write as _;
        let mut key = IntKey { buf: [0; 20], len: 0 };
        // Infallible: an i64 never needs more than 20 bytes.
        let _ = write!(key, "{n}");
        key
    }

    #[inline]
    fn as_str(&self) -> &str {
        // Only ASCII digits and '-' are written, so this is always valid UTF-8.
        core::str::from_utf8(&self.buf[..self.len]).unwrap_or("")
    }
}

impl Val {
    pub(crate) fn access(&self, field: &Val) -> Option<&Val> {
        match (self, field) {
            (Val::Map(m), Val::Str(s)) => m.get(s.as_ref()),
            // `a.1` is equivalent to `a."1"`: map keys are string-valued, so an
            // integer index is normalized to its decimal string form.
            (Val::Map(m), Val::Int(i)) => {
                let key = IntKey::new(*i);
                m.get(key.as_str())
            }
            (Val::List(l), Val::Int(i)) => {
                let idx = if *i < 0 {
                    let adjusted = l.len() as i64 + *i;
                    if adjusted < 0 {
                        return None;
                    }
                    usize::try_from(adjusted).ok()?
                } else {
                    usize::try_from(*i).ok()?
                };
                l.get(idx)
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
            Val::Str(Arc::<str>::from(result.into_boxed_str()))
        }
    }
}

fn checked_int_value(op: BinOp, a: i64, b: i64, value: Option<i64>) -> Result<Val> {
    value
        .map(Val::Int)
        .ok_or_else(|| Error::Eval(format!("Invalid op: {a} {op} {b}")))
}

#[cfg(feature = "adv_arith")]
fn list_minus_list_indexed(left: &[Val], right: &[Val]) -> Val {
    if left.is_empty() {
        return Val::List(Arc::new(Vec::new()));
    }
    if right.is_empty() {
        return Val::List(Arc::new(left.to_vec()));
    }

    if left.len() <= 32 || right.len() <= 32 {
        let mut result = left.to_vec();
        result.retain(|left_val| !right.iter().any(|right_val| left_val == right_val));
        return Val::List(Arc::new(result));
    }

    let mut int_set = HashSet::with_capacity(right.len());
    let mut str_set: HashSet<&str> = HashSet::with_capacity(right.len());
    let mut bool_set = HashSet::with_capacity(right.len());
    let mut complex_right = Vec::new();

    for right_val in right {
        match right_val {
            Val::Int(i) => {
                int_set.insert(*i);
            }
            Val::Str(s) => {
                str_set.insert(s.as_ref());
            }
            Val::Bool(b) => {
                bool_set.insert(*b);
            }
            _ => complex_right.push(right_val),
        }
    }

    let mut result = left.to_vec();
    result.retain(|left_val| match left_val {
        Val::Int(i) => !int_set.contains(i),
        Val::Str(s) => !str_set.contains(s.as_ref()),
        Val::Bool(b) => !bool_set.contains(b),
        _ => !complex_right.contains(&left_val),
    });

    Val::List(Arc::new(result))
}

impl Add for &Val {
    type Output = Result<Val>;

    /// - Str + Num may leads to unexpected behavior.
    /// - List can + Val, but Val + List is not supported.
    /// - Map can + Map, but Map can't + Val, since the value of the map is not defined.
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => a
                .checked_add(*b)
                .map(Val::Int)
                .ok_or_else(|| Error::Eval(format!("Integer overflow: {a} + {b}"))),
            (Val::Float(a), Val::Float(b)) => Ok(Val::Float(a + b)),
            (Val::Float(a), Val::Int(b)) => Ok(Val::Float(a + *b as f64)),
            (Val::Int(a), Val::Float(b)) => Ok(Val::Float(*a as f64 + b)),
            (Val::Str(a), Val::Str(b)) => {
                if a.is_empty() {
                    Ok(Val::Str(Arc::clone(b)))
                } else if b.is_empty() {
                    Ok(Val::Str(Arc::clone(a)))
                } else {
                    Ok(Val::concat_strings(a.as_ref(), b.as_ref()))
                }
            }
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
                // Map + Map: right side overrides left side for same keys.
                if r.is_empty() {
                    return Ok(self.clone());
                }
                if l.is_empty() {
                    return Ok(other.clone());
                }
                if l.len() >= r.len()
                    && r
                        .iter()
                        .all(|(k, v)| matches!(l.get(k), Some(existing) if existing == v))
                {
                    return Ok(self.clone());
                }

                // Prefer cloning the larger side to avoid re-hashing every entry.
                if l.len() >= r.len() {
                    let mut merged = (**l).clone();
                    for (k, v) in r.iter() {
                        merged.insert(k.clone(), v.clone());
                    }
                    Ok(merged.into())
                } else {
                    let mut merged = (**r).clone();
                    for (k, v) in l.iter() {
                        match merged.entry(k.clone()) {
                            Entry::Vacant(e) => {
                                e.insert(v.clone());
                            }
                            Entry::Occupied(_) => {}
                        }
                    }
                    Ok(merged.into())
                }
            }
            #[cfg(feature = "adv_arith")]
            (Val::List(l), Val::List(r)) => {
                let mut merged = (**l).clone();
                merged.reserve(r.len());
                merged.extend(r.iter().cloned());
                Ok(merged.into())
            }
            #[cfg(feature = "adv_arith")]
            (Val::List(l), r) => {
                let mut new_list = (**l).clone();
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
            (Val::Int(a), Val::Int(b)) => a
                .checked_sub(*b)
                .map(Val::Int)
                .ok_or_else(|| Error::Eval(format!("Integer overflow: {a} - {b}"))),
            (Val::Float(a), Val::Float(b)) => Ok((a - b).into()),
            (Val::Float(a), Val::Int(b)) => Ok((a - *b as f64).into()),
            (Val::Int(a), Val::Float(b)) => Ok((*a as f64 - b).into()),
            #[cfg(feature = "adv_arith")]
            (Val::List(l), Val::List(r)) => Ok(list_minus_list_indexed(l, r)),
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
                if r.is_empty() {
                    return Ok(self.clone());
                }

                if r.len() * 4 <= l.len() {
                    let mut result = (**l).clone();
                    for key in r.keys() {
                        result.remove(key);
                    }
                    Ok(result.into())
                } else {
                    let mut result = HashMap::with_capacity(l.len());
                    for (k, v) in l.iter() {
                        if !r.contains_key(k) {
                            result.insert(k.clone(), v.clone());
                        }
                    }
                    Ok(result.into())
                }
            }
            #[cfg(feature = "adv_arith")]
            (Val::Map(l), r) => {
                if let Val::Str(k) = r {
                    if !l.contains_key(k.as_ref()) {
                        return Ok(self.clone());
                    }

                    let mut result = (**l).clone();
                    result.remove(k.as_ref());
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
            (Val::Int(a), Val::Int(b)) => a
                .checked_mul(*b)
                .map(Val::Int)
                .ok_or_else(|| Error::Eval(format!("Integer overflow: {a} * {b}"))),
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
            (_, Val::Int(0)) => Err(Error::Eval("Division by zero".to_string())),
            (_, Val::Float(f)) if *f == 0.0 => Err(Error::Eval("Division by zero".to_string())),
            #[cfg(feature = "sem_arith")]
            (Val::Int(a), Val::Int(b)) => {
                let div = checked_int_value(BinOp::Div, *a, *b, a.checked_div(*b))?;
                let rem = checked_int_value(BinOp::Mod, *a, *b, a.checked_rem(*b))?;
                if rem == Val::Int(0) {
                    Ok(div)
                } else {
                    Ok((*a as f64 / *b as f64).into())
                }
            }
            #[cfg(not(feature = "sem_arith"))]
            (Val::Int(a), Val::Int(b)) => checked_int_value(BinOp::Div, *a, *b, a.checked_div(*b)),
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
            (_, Val::Int(0)) => Err(Error::Eval("Modulo by zero".to_string())),
            (_, Val::Float(f)) if *f == 0.0 => Err(Error::Eval("Modulo by zero".to_string())),
            (Val::Int(a), Val::Int(b)) => checked_int_value(BinOp::Mod, *a, *b, a.checked_rem(*b)),
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
        Val::Str(Arc::<str>::from(s.into_boxed_str()))
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
        let inner = m.into_iter().map(|(k, v)| (k.as_ref().to_string(), v.into())).collect();
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

/// Depth cap for `serde_*::Value` -> `Val` conversion, mirroring the deserialize
/// guard. These `From` impls recurse directly (they don't go through the
/// depth-bounded `ValVisitor`), so a deeply nested `Value` — e.g. built by
/// `Val::try_from` via `serde_json::to_value`, which itself has no text-parse
/// recursion limit — would otherwise overflow the stack. Subtrees deeper than
/// this are converted to `Nil` (fail-safe truncation rather than a crash).
#[cfg(any(feature = "json", feature = "yaml"))]
const MAX_VALUE_CONVERSION_DEPTH: usize = 128;

#[cfg(feature = "json")]
impl From<serde_json::Value> for Val {
    fn from(val: serde_json::Value) -> Self {
        json_value_to_val(val, 0)
    }
}

#[cfg(feature = "json")]
fn json_value_to_val(val: serde_json::Value, depth: usize) -> Val {
    match val {
        serde_json::Value::String(s) => Val::Str(Arc::<str>::from(s.into_boxed_str())),
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
            if depth >= MAX_VALUE_CONVERSION_DEPTH {
                return Val::Nil;
            }
            let v = a.into_iter().map(|e| json_value_to_val(e, depth + 1)).collect();
            Val::List(Arc::new(v))
        }
        serde_json::Value::Object(o) => {
            if depth >= MAX_VALUE_CONVERSION_DEPTH {
                return Val::Nil;
            }
            let m = o
                .into_iter()
                .map(|(k, v)| (k, json_value_to_val(v, depth + 1)))
                .collect();
            Val::Map(Arc::new(m))
        }
        serde_json::Value::Null => Val::Nil,
    }
}

#[cfg(feature = "yaml")]
impl From<serde_yaml::Value> for Val {
    fn from(val: serde_yaml::Value) -> Self {
        yaml_value_to_val(val, 0)
    }
}

#[cfg(feature = "yaml")]
fn yaml_value_to_val(val: serde_yaml::Value, depth: usize) -> Val {
    match val {
        serde_yaml::Value::String(s) => Val::Str(Arc::<str>::from(s.into_boxed_str())),
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
            if depth >= MAX_VALUE_CONVERSION_DEPTH {
                return Val::Nil;
            }
            let v = a.into_iter().map(|e| yaml_value_to_val(e, depth + 1)).collect();
            Val::List(Arc::new(v))
        }
        serde_yaml::Value::Mapping(o) => {
            if depth >= MAX_VALUE_CONVERSION_DEPTH {
                return Val::Nil;
            }
            let m = o
                .into_iter()
                .filter_map(|(k, v)| {
                    if let serde_yaml::Value::String(key) = k {
                        Some((key, yaml_value_to_val(v, depth + 1)))
                    } else {
                        None
                    }
                })
                .collect();
            Val::Map(Arc::new(m))
        }
        serde_yaml::Value::Null => Val::Nil,
        serde_yaml::Value::Tagged(tagged) => {
            // A `Tagged` wraps another value and can itself nest, so it must
            // count toward the budget to keep the recursion bounded.
            if depth >= MAX_VALUE_CONVERSION_DEPTH {
                return Val::Nil;
            }
            yaml_value_to_val(tagged.value, depth + 1)
        }
    }
}

impl Val {
    #[cfg(feature = "json")]
    pub fn try_from<T>(val: T) -> Result<Self>
    where
        T: serde::Serialize,
    {
        Ok(serde_json::to_value(val)
            .map_err(|e| Error::Eval(e.to_string()))?
            .into())
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
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
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
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
            Val::Map(m) => fmt_map(f, m),
            Val::List(l) => fmt_list(f, l),
            Val::Nil => write!(f, "nil"),
        }
    }
}

fn fmt_map(f: &mut core::fmt::Formatter<'_>, m: &Arc<HashMap<String, Val>>) -> core::fmt::Result {
    f.write_str("{")?;
    let mut first = true;
    for (key, value) in m.iter() {
        if !first {
            f.write_str(",")?;
        }
        first = false;
        fmt_json_str(f, key)?;
        f.write_str(":")?;
        fmt_json_value(f, value)?;
    }
    f.write_str("}")
}

fn fmt_list(f: &mut core::fmt::Formatter<'_>, l: &Arc<Vec<Val>>) -> core::fmt::Result {
    f.write_str("[")?;
    let mut first = true;
    for value in l.iter() {
        if !first {
            f.write_str(",")?;
        }
        first = false;
        fmt_json_value(f, value)?;
    }
    f.write_str("]")
}

fn fmt_json_value(f: &mut core::fmt::Formatter<'_>, value: &Val) -> core::fmt::Result {
    match value {
        Val::Str(s) => fmt_json_str(f, s),
        Val::Int(i) => write!(f, "{i}"),
        Val::Float(fl) => write!(f, "{fl}"),
        Val::Bool(b) => write!(f, "{b}"),
        Val::Map(m) => fmt_map(f, m),
        Val::List(l) => fmt_list(f, l),
        Val::Nil => f.write_str("null"),
    }
}

fn fmt_json_str(f: &mut core::fmt::Formatter<'_>, value: &str) -> core::fmt::Result {
    f.write_str("\"")?;
    for ch in value.chars() {
        match ch {
            '"' => f.write_str("\\\"")?,
            '\\' => f.write_str("\\\\")?,
            '\n' => f.write_str("\\n")?,
            '\r' => f.write_str("\\r")?,
            '\t' => f.write_str("\\t")?,
            '\u{08}' => f.write_str("\\b")?,
            '\u{0C}' => f.write_str("\\f")?,
            ch if ch.is_control() => write!(f, "\\u{:04x}", ch as u32)?,
            ch => write!(f, "{ch}")?,
        }
    }
    f.write_str("\"")
}
