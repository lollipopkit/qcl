//! Serialize a `serde::Serialize` value directly into a [`Val`], without a
//! JSON text round-trip or an intermediate `serde_json::Value`.
//!
//! This mirrors [`crate::de::ValVisitor`] in the opposite direction: each
//! `serialize_*` call builds the corresponding `Val` variant. It lets callers
//! that already own a typed Rust value (struct, enum, `HashMap`, `Vec`) build
//! a `Val` context in a single pass, avoiding both serialization-to-text and
//! the double allocation of `serde_json::Value` -> `Val`.
//!
//! # Representation notes
//! Because `Val` has no enum concept, serde enum variants are flattened:
//! - unit variant `Foo::A` -> `Val::Str("A")`
//! - newtype variant `Foo::A(x)` -> `Val::Map({"A": x})`
//! - tuple/struct variants -> the inner sequence/map (variant name dropped)
//! - `serialize_bytes` -> `Val::List` of ints

use alloc::{
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use core::fmt;
use hashbrown::HashMap;
use serde::ser::{
    Error as SerErrorTrait, Serialize, SerializeMap, SerializeSeq, SerializeStruct,
    SerializeStructVariant, SerializeTuple, SerializeTupleStruct, SerializeTupleVariant,
    Serializer,
};

use crate::val::Val;

/// Error raised while serializing into a `Val`.
#[derive(Debug, Clone, PartialEq)]
pub struct SerError(pub String);

impl fmt::Display for SerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl SerError {
    fn new(msg: impl Into<String>) -> Self {
        SerError(msg.into())
    }
}

impl SerErrorTrait for SerError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        SerError(msg.to_string())
    }
}

#[cfg(feature = "std")]
impl std::error::Error for SerError {}

/// Serialize a `serde::Serialize` value into a [`Val`].
///
/// This is the single-pass alternative to `serde_json::to_value(value).into()`
/// (which builds an intermediate `serde_json::Value` and then re-allocates
/// every node into a `Val`).
pub fn to_val<T: Serialize + ?Sized>(value: &T) -> Result<Val, SerError> {
    value.serialize(&ValSerializer)
}

/// The serializer that builds a [`Val`].
pub struct ValSerializer;

impl Serializer for &ValSerializer {
    type Ok = Val;
    type Error = SerError;
    type SerializeSeq = CompoundSeq;
    type SerializeTuple = CompoundSeq;
    type SerializeTupleStruct = CompoundSeq;
    type SerializeTupleVariant = CompoundSeq;
    type SerializeMap = CompoundMap;
    type SerializeStruct = CompoundMap;
    type SerializeStructVariant = CompoundMap;

    fn serialize_bool(self, v: bool) -> Result<Val, SerError> {
        Ok(Val::Bool(v))
    }
    fn serialize_i8(self, v: i8) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_i16(self, v: i16) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_i32(self, v: i32) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_i64(self, v: i64) -> Result<Val, SerError> {
        Ok(Val::Int(v))
    }
    fn serialize_u8(self, v: u8) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_u16(self, v: u16) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_u32(self, v: u32) -> Result<Val, SerError> {
        Ok(Val::Int(v as i64))
    }
    fn serialize_u64(self, v: u64) -> Result<Val, SerError> {
        Ok(if v <= i64::MAX as u64 {
            Val::Int(v as i64)
        } else {
            Val::Float(v as f64)
        })
    }
    fn serialize_f32(self, v: f32) -> Result<Val, SerError> {
        Ok(Val::Float(v as f64))
    }
    fn serialize_f64(self, v: f64) -> Result<Val, SerError> {
        Ok(Val::Float(v))
    }
    fn serialize_char(self, c: char) -> Result<Val, SerError> {
        let mut s = String::new();
        s.push(c);
        Ok(Val::Str(Arc::from(s)))
    }
    fn serialize_str(self, s: &str) -> Result<Val, SerError> {
        Ok(Val::Str(Arc::from(s)))
    }
    fn serialize_bytes(self, b: &[u8]) -> Result<Val, SerError> {
        let v: Vec<Val> = b.iter().map(|&x| Val::Int(x as i64)).collect();
        Ok(Val::List(Arc::new(v)))
    }
    fn serialize_none(self) -> Result<Val, SerError> {
        Ok(Val::Nil)
    }
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Val, SerError> {
        value.serialize(self)
    }
    fn serialize_unit(self) -> Result<Val, SerError> {
        Ok(Val::Nil)
    }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Val, SerError> {
        Ok(Val::Nil)
    }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _idx: u32,
        variant: &'static str,
    ) -> Result<Val, SerError> {
        Ok(Val::Str(Arc::from(variant)))
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Val, SerError> {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _idx: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Val, SerError> {
        let inner = value.serialize(&ValSerializer)?;
        let mut m = HashMap::with_capacity(1);
        m.insert(variant.to_string(), inner);
        Ok(Val::Map(Arc::new(m)))
    }
    fn serialize_seq(self, len: Option<usize>) -> Result<CompoundSeq, SerError> {
        Ok(CompoundSeq {
            elems: Vec::with_capacity(len.unwrap_or(0)),
        })
    }
    fn serialize_tuple(self, len: usize) -> Result<CompoundSeq, SerError> {
        Ok(CompoundSeq {
            elems: Vec::with_capacity(len),
        })
    }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<CompoundSeq, SerError> {
        Ok(CompoundSeq {
            elems: Vec::with_capacity(len),
        })
    }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<CompoundSeq, SerError> {
        Ok(CompoundSeq {
            elems: Vec::with_capacity(len),
        })
    }
    fn serialize_map(self, len: Option<usize>) -> Result<CompoundMap, SerError> {
        Ok(CompoundMap {
            map: HashMap::with_capacity(len.unwrap_or(0)),
            next_key: None,
        })
    }
    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<CompoundMap, SerError> {
        Ok(CompoundMap {
            map: HashMap::with_capacity(len),
            next_key: None,
        })
    }
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<CompoundMap, SerError> {
        Ok(CompoundMap {
            map: HashMap::with_capacity(len),
            next_key: None,
        })
    }
}

/// Builds a [`Val::List`] from a seq/tuple.
pub struct CompoundSeq {
    elems: Vec<Val>,
}

impl SerializeSeq for CompoundSeq {
    type Ok = Val;
    type Error = SerError;
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), SerError> {
        self.elems.push(value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::List(Arc::new(self.elems)))
    }
}

impl SerializeTuple for CompoundSeq {
    type Ok = Val;
    type Error = SerError;
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), SerError> {
        self.elems.push(value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::List(Arc::new(self.elems)))
    }
}

impl SerializeTupleStruct for CompoundSeq {
    type Ok = Val;
    type Error = SerError;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), SerError> {
        self.elems.push(value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::List(Arc::new(self.elems)))
    }
}

impl SerializeTupleVariant for CompoundSeq {
    type Ok = Val;
    type Error = SerError;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), SerError> {
        self.elems.push(value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::List(Arc::new(self.elems)))
    }
}

/// Builds a [`Val::Map`] from a map/struct.
pub struct CompoundMap {
    map: HashMap<String, Val>,
    next_key: Option<String>,
}

impl CompoundMap {
    /// Map keys must be string-valued (matching `Val::Map`), so a non-string
    /// primitive is normalized to its string form, mirroring `Val::access`.
    fn store_key(&mut self, key: Val) -> Result<(), SerError> {
        let k = match key {
            Val::Str(s) => s.to_string(),
            Val::Int(i) => i.to_string(),
            Val::Float(f) => f.to_string(),
            Val::Bool(b) => b.to_string(),
            _ => return Err(SerError::new("map key must be a primitive (str/int/float/bool)")),
        };
        self.next_key = Some(k);
        Ok(())
    }
}

impl SerializeMap for CompoundMap {
    type Ok = Val;
    type Error = SerError;
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), SerError> {
        let key_val = key.serialize(&ValSerializer)?;
        self.store_key(key_val)
    }
    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), SerError> {
        let key = self
            .next_key
            .take()
            .ok_or_else(|| SerError::new("serialize_value called without a matching serialize_key"))?;
        self.map.insert(key, value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::Map(Arc::new(self.map)))
    }
}

impl SerializeStruct for CompoundMap {
    type Ok = Val;
    type Error = SerError;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), SerError> {
        self.map
            .insert(key.to_string(), value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::Map(Arc::new(self.map)))
    }
}

impl SerializeStructVariant for CompoundMap {
    type Ok = Val;
    type Error = SerError;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), SerError> {
        self.map
            .insert(key.to_string(), value.serialize(&ValSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Val, SerError> {
        Ok(Val::Map(Arc::new(self.map)))
    }
}
