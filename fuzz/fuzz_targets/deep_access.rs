#![no_main]

mod common;

use std::{collections::HashMap, sync::Arc};

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use qcl::{expr::Expr, val::Val};

#[derive(Clone, Debug)]
enum Segment {
    Field(String),
    Index(usize),
}

fn sanitize_bare_identifier(raw: &str, fallback: &str) -> String {
    let sanitized = common::sanitize_field(raw, fallback);
    let mut out = String::with_capacity(sanitized.len().min(common::MAX_FIELD_LEN));

    for (idx, ch) in sanitized.chars().enumerate() {
        let keep = if idx == 0 {
            ch.is_ascii_alphabetic() || ch == '_'
        } else {
            ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
        };

        if keep {
            out.push(ch);
        }

        if out.len() >= common::MAX_FIELD_LEN {
            break;
        }
    }

    if out.is_empty() {
        out.push('_');
        for ch in fallback.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' {
                out.push(ch);
            }
            if out.len() >= common::MAX_FIELD_LEN {
                break;
            }
        }
    }

    if matches!(out.as_str(), "true" | "false" | "nil" | "in") {
        out.push('_');
    }

    out
}

fn normalize_field_name(raw: &str, fallback: &str, quote_fields: bool) -> String {
    if quote_fields {
        common::sanitize_field(raw, fallback)
    } else {
        sanitize_bare_identifier(raw, fallback)
    }
}

fn fallback_input(data: &[u8]) -> common::DeepAccessInput {
    let root = common::lossy_text(data, 12);
    let segments = data
        .iter()
        .copied()
        .skip(1)
        .take(8)
        .enumerate()
        .map(|(idx, byte)| {
            if byte % 3 == 0 {
                common::PathSegment::Index(byte % 6)
            } else {
                common::PathSegment::Field(format!("field_{idx}_{byte}"))
            }
        })
        .collect();

    let leaf = match data.first().copied().unwrap_or(0) % 5 {
        0 => common::Atom::Nil,
        1 => common::Atom::Bool(true),
        2 => common::Atom::Int(i64::from(data.get(2).copied().unwrap_or(7))),
        3 => common::Atom::Float(f64::from(data.get(3).copied().unwrap_or(9)) / 10.0),
        _ => common::Atom::Str("leaf".to_string()),
    };

    common::DeepAccessInput {
        root,
        segments,
        leaf,
        quote_fields: data.get(1).copied().unwrap_or(0) % 2 == 1,
        missing_tail: data.get(2).copied().unwrap_or(11),
    }
}

fn decode_input(data: &[u8]) -> common::DeepAccessInput {
    let mut u = Unstructured::new(data);
    common::DeepAccessInput::arbitrary(&mut u).unwrap_or_else(|_| fallback_input(data))
}

fn normalize_segments(raw_segments: Vec<common::PathSegment>, quote_fields: bool) -> Vec<Segment> {
    raw_segments
        .into_iter()
        .take(common::MAX_SEGMENTS)
        .enumerate()
        .map(|(idx, segment)| match segment {
            common::PathSegment::Field(name) => Segment::Field(normalize_field_name(
                name.as_str(),
                format!("field_{idx}").as_str(),
                quote_fields,
            )),
            common::PathSegment::Index(index) => Segment::Index(usize::from(index % 8)),
        })
        .collect()
}

fn build_context(root: &str, segments: &[Segment], leaf: Val) -> Val {
    let mut current = leaf;

    for segment in segments.iter().rev() {
        current = match segment {
            Segment::Field(name) => {
                let mut map = HashMap::new();
                map.insert(name.clone(), current);
                Val::Map(Arc::new(map))
            }
            Segment::Index(index) => {
                let mut list = vec![Val::Nil; index + 1];
                list[*index] = current;
                Val::List(Arc::new(list))
            }
        };
    }

    let mut map = HashMap::new();
    map.insert(root.to_string(), current);
    Val::Map(Arc::new(map))
}

fn append_segment(expr: &mut String, segment: &Segment, quote_fields: bool) {
    expr.push('.');
    match segment {
        Segment::Field(name) => expr.push_str(common::render_field(name.as_str(), quote_fields).as_str()),
        Segment::Index(index) => expr.push_str(index.to_string().as_str()),
    }
}

fn build_expr(root: &str, segments: &[Segment], quote_fields: bool) -> String {
    let mut expr = common::render_root(root, quote_fields);
    for segment in segments {
        append_segment(&mut expr, segment, quote_fields);
    }
    expr
}

fn missing_segment(byte: u8) -> Segment {
    if byte % 2 == 0 {
        Segment::Field(format!("missing_{}", byte % 13))
    } else {
        Segment::Index(usize::from((byte % 8) + 8))
    }
}

fuzz_target!(|data: &[u8]| {
    let input = decode_input(data);
    let root = normalize_field_name(input.root.as_str(), "root", input.quote_fields);
    let segments = normalize_segments(input.segments, input.quote_fields);
    let leaf = input.leaf.into_val();
    let ctx = build_context(root.as_str(), &segments, leaf.clone());
    let expr_text = build_expr(root.as_str(), &segments, input.quote_fields);

    let expr = Expr::try_from(expr_text.as_str()).expect("generated deep access expression must parse");
    let value = expr.eval(&ctx).expect("generated deep access expression must evaluate");
    assert_eq!(value, leaf);

    let requested = expr.requested_ctx();
    assert!(requested.contains(root.as_str()));

    let mut missing_expr = expr_text;
    let tail = missing_segment(input.missing_tail);
    append_segment(&mut missing_expr, &tail, input.quote_fields);
    let missing = Expr::try_from(missing_expr.as_str()).expect("generated missing-path expression must parse");
    assert_eq!(
        missing
            .eval(&ctx)
            .expect("missing path evaluation should remain deterministic"),
        Val::Nil
    );
});
