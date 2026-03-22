#![allow(dead_code)]

use arbitrary::Arbitrary;
use qcl::val::Val;

pub const MAX_EXPR_LEN: usize = 4096;
pub const MAX_TEXT_LEN: usize = 8192;
pub const MAX_DEPTH: usize = 256;
pub const MAX_SEGMENTS: usize = 64;
pub const MAX_LIST_LEN: usize = 16;
pub const MAX_STRING_LEN: usize = 96;
pub const MAX_FIELD_LEN: usize = 24;

pub fn lossy_text(data: &[u8], max_len: usize) -> String {
    let bounded = &data[..data.len().min(max_len)];
    String::from_utf8_lossy(bounded).into_owned()
}

pub fn split_once_bytes<'a>(data: &'a [u8], delimiter: &[u8]) -> Option<(&'a [u8], &'a [u8])> {
    if delimiter.is_empty() || data.len() < delimiter.len() {
        return None;
    }

    let pos = data.windows(delimiter.len()).position(|window| window == delimiter)?;
    let left = &data[..pos];
    let right = &data[pos + delimiter.len()..];
    Some((left, right))
}

pub fn truncate_chars(input: &str, max_len: usize) -> String {
    input.chars().take(max_len).collect()
}

pub fn normalize_expr(input: String) -> String {
    truncate_chars(&input, MAX_EXPR_LEN)
}

pub fn normalize_text(input: &str) -> String {
    truncate_chars(input, MAX_TEXT_LEN)
}

pub fn default_expr() -> String {
    r#"@req.user.role == "admin" || @req.user.id in @record.granted"#.to_string()
}

pub fn default_ctx() -> String {
    r#"{"req":{"user":{"role":"admin","id":7}},"record":{"owner":7,"granted":[7,8,9]},"flags":{"active":true}}"#
        .to_string()
}

pub fn build_deep_parens(depth: usize, body: &str) -> String {
    let bounded_depth = depth.min(MAX_DEPTH);
    let mut expr = String::with_capacity((bounded_depth * 2) + body.len());
    for _ in 0..bounded_depth {
        expr.push('(');
    }
    expr.push_str(body);
    for _ in 0..bounded_depth {
        expr.push(')');
    }
    expr
}

pub fn finite_f64(value: f64) -> f64 {
    if value.is_finite() { value } else { 0.0 }
}

pub fn sanitize_field(raw: &str, fallback: &str) -> String {
    let mut out = String::new();
    for ch in raw.chars() {
        let keep = ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | ' ' | '&' | '=' | ':' | '/' | '.');
        if keep {
            out.push(ch);
        }
        if out.len() >= MAX_FIELD_LEN {
            break;
        }
    }

    if out.is_empty() { fallback.to_string() } else { out }
}

pub fn is_bare_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
}

pub fn render_root(name: &str, force_quote: bool) -> String {
    if force_quote || !is_bare_identifier(name) {
        format!("@\"{}\"", name)
    } else {
        format!("@{}", name)
    }
}

pub fn render_field(name: &str, force_quote: bool) -> String {
    if force_quote || !is_bare_identifier(name) {
        format!("\"{}\"", name)
    } else {
        name.to_string()
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub enum Atom {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
}

impl Atom {
    pub fn into_val(self) -> Val {
        match self {
            Atom::Nil => Val::Nil,
            Atom::Bool(v) => Val::Bool(v),
            Atom::Int(v) => Val::Int(v),
            Atom::Float(v) => Val::Float(finite_f64(v)),
            Atom::Str(v) => Val::from(truncate_chars(&v, MAX_STRING_LEN)),
        }
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub enum PathSegment {
    Field(String),
    Index(u8),
}

#[derive(Arbitrary, Clone, Debug)]
pub enum ParseMode {
    Raw,
    DeepParens,
    WrappedQuery,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct ParseInput {
    pub mode: ParseMode,
    pub expr: String,
    pub depth: u16,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct DeepAccessInput {
    pub root: String,
    pub segments: Vec<PathSegment>,
    pub leaf: Atom,
    pub quote_fields: bool,
    pub missing_tail: u8,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct StringInStringCase {
    pub needle: String,
    pub haystack: String,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct ScalarInListCase {
    pub needle: Atom,
    pub haystack: Vec<Atom>,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct ListInListCase {
    pub needles: Vec<Atom>,
    pub haystack: Vec<Atom>,
}

#[derive(Arbitrary, Clone, Debug)]
pub struct MapKeyCase {
    pub needle: Atom,
    pub entries: Vec<(Atom, Atom)>,
}

#[derive(Arbitrary, Clone, Debug)]
pub enum InCase {
    StringInString(StringInStringCase),
    ScalarInList(ScalarInListCase),
    ListInList(ListInListCase),
    MapKey(MapKeyCase),
}
