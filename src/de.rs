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

/// Direct YAML string to Val conversion avoiding intermediate serde_yaml::Value
pub fn from_yaml_str(input: &str) -> anyhow::Result<Val> {
    serde_yaml::from_str::<Val>(input).map_err(|e| anyhow::anyhow!(e))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Format {
    Json,
    Yaml,
}

/// Automatically detect format based on content
pub fn detect_format(input: &str) -> Format {
    let trimmed = input.trim();
    
    // Empty input defaults to JSON
    if trimmed.is_empty() {
        return Format::Json;
    }
    
    // Check for obvious JSON markers
    if (trimmed.starts_with('{') && trimmed.ends_with('}')) ||
       (trimmed.starts_with('[') && trimmed.ends_with(']')) {
        return Format::Json;
    }
    
    // Check for obvious YAML markers
    if trimmed.contains("---") ||  // YAML document separator
       trimmed.contains("...") ||  // YAML document end
       has_yaml_indicators(trimmed) {
        return Format::Yaml;
    }
    
    // Try parsing as JSON first (faster and more common)
    if serde_json::from_str::<serde_json::Value>(input).is_ok() {
        return Format::Json;
    }
    
    // Try parsing as YAML
    if serde_yaml::from_str::<serde_yaml::Value>(input).is_ok() {
        return Format::Yaml;
    }
    
    // Default to JSON if both fail
    Format::Json
}

/// Check for YAML-specific indicators
fn has_yaml_indicators(input: &str) -> bool {
    for line in input.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        
        // Look for YAML key-value patterns without quotes
        if trimmed.contains(':') && !trimmed.starts_with('"') && !trimmed.starts_with('{') {
            // Check if it's a YAML-style key: value (not JSON "key": value)
            if let Some(colon_pos) = trimmed.find(':') {
                let key_part = &trimmed[..colon_pos];
                // YAML keys often don't have quotes and can contain spaces/special chars
                if !key_part.starts_with('"') && !key_part.starts_with('\'') {
                    return true;
                }
            }
        }
        
        // Look for YAML list indicators
        if trimmed.starts_with("- ") || trimmed.starts_with("-\t") {
            return true;
        }
        
        // Look for YAML multi-line indicators
        if trimmed.ends_with("|") || trimmed.ends_with(">") {
            return true;
        }
    }
    
    false
}

/// Parse input using automatic format detection or specified format
pub fn parse_with_format(input: &str, format_override: Option<Format>) -> anyhow::Result<Val> {
    let format = format_override.unwrap_or_else(|| detect_format(input));
    
    match format {
        Format::Json => from_json_str(input),
        Format::Yaml => from_yaml_str(input),
    }
}