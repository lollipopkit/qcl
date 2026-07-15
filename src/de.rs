use crate::val::Val;
use hashbrown::{HashMap, HashSet};
use serde::de::{
    Deserialize, DeserializeSeed, Deserializer, Error as DeError, IgnoredAny, MapAccess, SeqAccess,
    Visitor,
};
use std::fmt;
use std::sync::Arc;

/// Cap on container nesting during deserialization. Deeply nested input would
/// otherwise recurse (visit_seq/visit_map) and, once built, a deep `Val` would
/// overflow the stack when dropped/traversed. serde_json/yaml/toml each have
/// their own recursion limits, but this also guards backends that don't (e.g.
/// serde_wasm_bindgen) and keeps the resulting `Val` depth bounded.
const MAX_DESERIALIZE_DEPTH: usize = 128;

/// Bound preallocation from a possibly attacker-influenced `size_hint`.
const MAX_PREALLOC: usize = 4096;

/// Seed carrying the current nesting depth so recursion can be bounded.
struct ValSeed {
    depth: usize,
}

impl<'de> DeserializeSeed<'de> for ValSeed {
    type Value = Val;

    fn deserialize<D>(self, deserializer: D) -> Result<Val, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(ValVisitor { depth: self.depth })
    }
}

/// Custom Visitor for deserializing any JSON/YAML/TOML value to a `Val`.
struct ValVisitor {
    depth: usize,
}

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
        Ok(Val::from(value))
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
        if self.depth >= MAX_DESERIALIZE_DEPTH {
            return Err(A::Error::custom("input nesting is too deep"));
        }
        let size_hint = seq.size_hint().unwrap_or(0).min(MAX_PREALLOC);
        let mut elements = Vec::with_capacity(size_hint);
        while let Some(elem) = seq.next_element_seed(ValSeed { depth: self.depth + 1 })? {
            elements.push(elem);
        }
        Ok(Val::List(Arc::new(elements)))
    }

    fn visit_map<M>(self, mut map_access: M) -> Result<Val, M::Error>
    where
        M: MapAccess<'de>,
    {
        if self.depth >= MAX_DESERIALIZE_DEPTH {
            return Err(M::Error::custom("input nesting is too deep"));
        }
        let size_hint = map_access.size_hint().unwrap_or(0).min(MAX_PREALLOC);
        let mut map = HashMap::with_capacity(size_hint);
        while let Some(key) = map_access.next_key::<String>()? {
            let value = map_access.next_value_seed(ValSeed { depth: self.depth + 1 })?;
            map.insert(key, value);
        }
        Ok(Val::Map(Arc::new(map)))
    }
}

/// Top-level map visitor that drops entries whose key is not in `keep`.
/// Nested values reuse the regular [`ValVisitor`] (no filtering below the top
/// level). Used by [`from_json_str_keep`].
struct FilteredMapVisitor<'a> {
    depth: usize,
    keep: &'a HashSet<String>,
}

impl<'de> Visitor<'de> for FilteredMapVisitor<'_> {
    type Value = Val;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a JSON object")
    }

    fn visit_map<M>(self, mut map_access: M) -> Result<Val, M::Error>
    where
        M: MapAccess<'de>,
    {
        if self.depth >= MAX_DESERIALIZE_DEPTH {
            return Err(M::Error::custom("input nesting is too deep"));
        }
        let mut map =
            HashMap::with_capacity(map_access.size_hint().unwrap_or(0).min(MAX_PREALLOC));
        while let Some(key) = map_access.next_key::<String>()? {
            if self.keep.contains(&key) {
                let value = map_access.next_value_seed(ValSeed { depth: self.depth + 1 })?;
                map.insert(key, value);
            } else {
                // Skip the value without building a Val for it.
                let _: IgnoredAny = map_access.next_value()?;
            }
        }
        Ok(Val::Map(Arc::new(map)))
    }
}

impl<'de> Deserialize<'de> for Val {
    fn deserialize<D>(deserializer: D) -> Result<Val, D::Error>
    where
        D: Deserializer<'de>,
    {
        ValSeed { depth: 0 }.deserialize(deserializer)
    }
}

/// Direct JSON string to Val conversion avoiding intermediate serde_json::Value
#[cfg(feature = "json")]
pub fn from_json_str(input: &str) -> crate::error::Result<Val> {
    serde_json::from_str::<Val>(input).map_err(|e| crate::error::Error::Deserialize(e.to_string()))
}

/// Parse JSON into a `Val`, keeping only the top-level keys listed in `keep`.
///
/// Values under skipped top-level keys are not materialized - they are parsed
/// with `IgnoredAny` - so a large context where an expression references only
/// a few top-level fields avoids most of the allocation cost. Filtering is
/// top-level only; nested maps are parsed in full.
///
/// If `keep` is empty, or the top level is not a JSON object, no filtering is
/// applied (full parse). The empty-keep default prevents a caller from
/// accidentally dropping every field.
#[cfg(feature = "json")]
pub fn from_json_str_keep(input: &str, keep: &HashSet<String>) -> crate::error::Result<Val> {
    let mut de = serde_json::Deserializer::from_str(input);
    let is_object = input
        .trim_start()
        .as_bytes()
        .first()
        .is_some_and(|&b| b == b'{');
    let result = if keep.is_empty() || !is_object {
        de.deserialize_any(ValVisitor { depth: 0 })
    } else {
        de.deserialize_map(FilteredMapVisitor { depth: 0, keep })
    };
    result.map_err(|e| crate::error::Error::Deserialize(e.to_string()))
}

/// Parse JSON into a `Val`, keeping only the top-level keys referenced by any
/// of `exprs` (via [`crate::expr::Expr::requested_ctx`]). Convenience wrapper
/// over [`from_json_str_keep`] that merges the context names from all expressions.
#[cfg(feature = "json")]
pub fn from_json_str_for_exprs(
    input: &str,
    exprs: &[&crate::expr::Expr],
) -> crate::error::Result<Val> {
    let mut keep = HashSet::new();
    for expr in exprs {
        for name in expr.requested_ctx() {
            keep.insert(name);
        }
    }
    from_json_str_keep(input, &keep)
}

/// Direct YAML string to Val conversion avoiding intermediate serde_yaml::Value
#[cfg(feature = "yaml")]
pub fn from_yaml_str(input: &str) -> crate::error::Result<Val> {
    serde_yaml::from_str::<Val>(input).map_err(|e| crate::error::Error::Deserialize(e.to_string()))
}

/// Direct TOML string to Val conversion avoiding intermediate toml::Value
#[cfg(feature = "toml")]
pub fn from_toml_str(input: &str) -> crate::error::Result<Val> {
    toml::from_str::<Val>(input).map_err(|e| crate::error::Error::Deserialize(e.to_string()))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Format {
    #[cfg(feature = "json")]
    Json,
    #[cfg(feature = "yaml")]
    Yaml,
    #[cfg(feature = "toml")]
    Toml,
}

pub const fn default_format() -> Format {
    #[cfg(feature = "json")]
    {
        Format::Json
    }
    #[cfg(all(feature = "yaml", not(feature = "json")))]
    {
        Format::Yaml
    }
    #[cfg(all(feature = "toml", not(feature = "json"), not(feature = "yaml")))]
    {
        Format::Toml
    }
}

fn detect_format_confident(input: &str) -> Option<Format> {
    let trimmed = input.trim();

    // Empty input defaults to first available format
    if trimmed.is_empty() {
        return Some(default_format());
    }

    // Check for obvious JSON markers
    #[cfg(feature = "json")]
    if (trimmed.starts_with('{') && trimmed.ends_with('}')) || (trimmed.starts_with('[') && trimmed.ends_with(']')) {
        return Some(Format::Json);
    }

    // Check for obvious YAML markers
    #[cfg(feature = "yaml")]
    if has_yaml_document_markers(trimmed) || has_yaml_indicators(trimmed) {
        return Some(Format::Yaml);
    }

    // Check for obvious TOML markers
    #[cfg(feature = "toml")]
    if has_toml_indicators(trimmed) {
        return Some(Format::Toml);
    }

    // JSON scalars such as `null`, `true`, `42`, and quoted strings do not have structural markers.
    // Keep this path cheap so large plain-text inputs do not pay for a full JSON parse.
    #[cfg(feature = "json")]
    if looks_like_json_scalar(trimmed) {
        return Some(Format::Json);
    }

    None
}

/// Automatically detect format based on content
pub fn detect_format(input: &str) -> Format {
    detect_format_confident(input).unwrap_or_else(default_format)
}

/// Parse input by trying supported formats, returning the detected format and parsed value.
pub fn parse_auto(input: &str) -> crate::error::Result<(Format, Val)> {
    let format = detect_format_confident(input).ok_or_else(|| {
        crate::error::Error::Deserialize(
            "Auto-detect requires explicit format selection or recognizable JSON/YAML/TOML markers".to_string(),
        )
    })?;
    let value = match format {
        #[cfg(feature = "json")]
        Format::Json => from_json_str(input),
        #[cfg(feature = "yaml")]
        Format::Yaml => from_yaml_str(input),
        #[cfg(feature = "toml")]
        Format::Toml => from_toml_str(input),
    }?;
    Ok((format, value))
}

/// Check for YAML document marker lines like `---` and `...`
#[cfg(feature = "yaml")]
fn has_yaml_document_markers(input: &str) -> bool {
    input.lines().any(|line| matches!(line.trim(), "---" | "..."))
}

/// Check for YAML-specific indicators
#[cfg(feature = "yaml")]
pub fn has_yaml_indicators(input: &str) -> bool {
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
                let value_part = &trimmed[colon_pos + 1..];
                // YAML keys often don't have quotes and can contain spaces/special chars
                if !key_part.starts_with('"')
                    && !key_part.starts_with('\'')
                    && (value_part.is_empty() || value_part.starts_with(' ') || value_part.starts_with('\t'))
                {
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

/// Check for TOML-specific indicators
#[cfg(feature = "toml")]
pub fn has_toml_indicators(input: &str) -> bool {
    for line in input.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // Look for TOML section headers [section]
        if trimmed.starts_with('[') && trimmed.ends_with(']') && trimmed.len() > 2 {
            return true;
        }

        // Look for TOML table arrays [[table]]
        if trimmed.starts_with("[[") && trimmed.ends_with("]]") && trimmed.len() > 4 {
            return true;
        }

        // Look for TOML key = value patterns (with equals sign)
        if trimmed.contains(" = ") || trimmed.contains("=") {
            // Check if it's a simple key = value pattern
            if let Some(eq_pos) = trimmed.find('=') {
                let key_part = trimmed[..eq_pos].trim();
                let value_part = trimmed[eq_pos + 1..].trim();

                // TOML keys are usually unquoted identifiers or quoted strings
                // Values can be strings, numbers, booleans, arrays, etc.
                if !key_part.is_empty() && !value_part.is_empty() {
                    // Check if key looks like a TOML identifier
                    if key_part
                        .chars()
                        .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
                        || (key_part.starts_with('"') && key_part.ends_with('"'))
                        || (key_part.starts_with('\'') && key_part.ends_with('\''))
                    {
                        return true;
                    }
                }
            }
        }
    }

    false
}

#[cfg(feature = "json")]
fn looks_like_json_scalar(input: &str) -> bool {
    matches!(input, "null" | "true" | "false")
        || (input.starts_with('"') && input.ends_with('"'))
        || looks_like_json_number(input)
}

#[cfg(feature = "json")]
fn looks_like_json_number(input: &str) -> bool {
    let bytes = input.as_bytes();
    if bytes.is_empty() {
        return false;
    }

    let mut i = 0usize;
    if bytes[i] == b'-' {
        i += 1;
        if i == bytes.len() {
            return false;
        }
    }

    match bytes[i] {
        b'0' => {
            i += 1;
        }
        b'1'..=b'9' => {
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
        }
        _ => return false,
    }

    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        if i == bytes.len() || !bytes[i].is_ascii_digit() {
            return false;
        }
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
    }

    if i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
        i += 1;
        if i < bytes.len() && matches!(bytes[i], b'+' | b'-') {
            i += 1;
        }
        if i == bytes.len() || !bytes[i].is_ascii_digit() {
            return false;
        }
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
    }

    i == bytes.len()
}

/// Parse input using automatic format detection or specified format
pub fn parse_with_format(input: &str, format_override: Option<Format>) -> crate::error::Result<Val> {
    if let Some(format) = format_override {
        return match format {
            #[cfg(feature = "json")]
            Format::Json => from_json_str(input),
            #[cfg(feature = "yaml")]
            Format::Yaml => from_yaml_str(input),
            #[cfg(feature = "toml")]
            Format::Toml => from_toml_str(input),
        };
    }

    #[cfg(feature = "json")]
    {
        from_json_str(input)
    }
    #[cfg(all(feature = "yaml", not(feature = "json")))]
    {
        from_yaml_str(input)
    }
    #[cfg(all(feature = "toml", not(feature = "json"), not(feature = "yaml")))]
    {
        from_toml_str(input)
    }
}
