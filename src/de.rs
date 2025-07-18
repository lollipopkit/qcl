use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use serde::de::{Deserialize, Deserializer, Visitor, SeqAccess, MapAccess};
use crate::val::Val;

/// Custom Visitor for deserializing any JSON value to Val enum
struct ValVisitor;

impl<'de> Visitor<'de> for ValVisitor {
    type Value = Val;
    
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a JSON value of any type")
    }
    
    fn visit_bool<E>(self, value: bool) -> Result<Val, E> {
        Ok(Val::Bool(value))
    }
    
    fn visit_i64<E>(self, value: i64) -> Result<Val, E> {
        Ok(Val::Int(value))
    }
    
    fn visit_u64<E>(self, value: u64) -> Result<Val, E> {
        // Convert u64 to i64 if possible, otherwise to f64
        if value <= i64::MAX as u64 {
            Ok(Val::Int(value as i64))
        } else {
            Ok(Val::Float(value as f64))
        }
    }
    
    fn visit_f64<E>(self, value: f64) -> Result<Val, E> {
        Ok(Val::Float(value))
    }
    
    fn visit_str<E>(self, value: &str) -> Result<Val, E> {
        Ok(Val::Str(Arc::from(value)))
    }
    
    fn visit_string<E>(self, value: String) -> Result<Val, E> {
        Ok(Val::Str(Arc::from(value.as_str())))
    }
    
    fn visit_none<E>(self) -> Result<Val, E> {
        Ok(Val::Nil)
    }
    
    fn visit_unit<E>(self) -> Result<Val, E> {
        Ok(Val::Nil)
    }
    
    fn visit_seq<A>(self, mut seq: A) -> Result<Val, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut elements = Vec::new();
        while let Some(elem) = seq.next_element::<Val>()? {
            elements.push(elem);
        }
        Ok(Val::List(Arc::new(elements)))
    }
    
    fn visit_map<M>(self, mut map_access: M) -> Result<Val, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut map = HashMap::new();
        while let Some((key, value)) = map_access.next_entry::<String, Val>()? {
            map.insert(key, value);
        }
        Ok(Val::Map(Arc::new(map)))
    }
}

impl<'de> Deserialize<'de> for Val {
    fn deserialize<D>(deserializer: D) -> Result<Val, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(ValVisitor)
    }
}

/// Direct JSON string to Val conversion avoiding intermediate serde_json::Value
pub fn from_json_str(input: &str) -> anyhow::Result<Val> {
    serde_json::from_str::<Val>(input).map_err(|e| anyhow::anyhow!(e))
}