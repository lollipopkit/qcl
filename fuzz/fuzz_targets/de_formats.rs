#![no_main]

mod common;

use libfuzzer_sys::fuzz_target;
use qcl::{
    de::{self, Format},
    val::Val,
};

const DELIMITER: &[u8] = b"\n--DATA--\n";

fn decode_mode_and_body(data: &[u8]) -> (String, String) {
    if let Some((mode_bytes, body_bytes)) = common::split_once_bytes(data, DELIMITER) {
        let mode = common::lossy_text(mode_bytes, 16);
        let body = common::lossy_text(body_bytes, common::MAX_TEXT_LEN);
        return (mode, body);
    }

    ("default".to_string(), common::lossy_text(data, common::MAX_TEXT_LEN))
}

fn vals_equivalent(left: &Val, right: &Val) -> bool {
    match (left, right) {
        (Val::Str(a), Val::Str(b)) => a == b,
        (Val::Int(a), Val::Int(b)) => a == b,
        (Val::Float(a), Val::Float(b)) => a == b || (a.is_nan() && b.is_nan()),
        (Val::Bool(a), Val::Bool(b)) => a == b,
        (Val::Map(a), Val::Map(b)) => {
            a.len() == b.len()
                && a.iter()
                    .all(|(key, left_val)| b.get(key).is_some_and(|right_val| vals_equivalent(left_val, right_val)))
        }
        (Val::List(a), Val::List(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(left_val, right_val)| vals_equivalent(left_val, right_val))
        }
        (Val::Nil, Val::Nil) => true,
        _ => false,
    }
}

fn contains_non_finite_float(value: &Val) -> bool {
    match value {
        Val::Float(v) => !v.is_finite(),
        Val::Map(entries) => entries.values().any(contains_non_finite_float),
        Val::List(items) => items.iter().any(contains_non_finite_float),
        _ => false,
    }
}

fn contains_float(value: &Val) -> bool {
    match value {
        Val::Float(_) => true,
        Val::Map(entries) => entries.values().any(contains_float),
        Val::List(items) => items.iter().any(contains_float),
        _ => false,
    }
}

fuzz_target!(|data: &[u8]| {
    let (mode, body) = decode_mode_and_body(data);
    let body = common::normalize_text(body.as_str());

    let _ = de::detect_format(body.as_str());

    if let Ok((format, value)) = de::parse_auto(body.as_str()) {
        let explicit = de::parse_with_format(body.as_str(), Some(format))
            .expect("auto-detected format should succeed when parsed explicitly");
        assert!(
            vals_equivalent(&explicit, &value),
            "auto-detected parse mismatch:\nleft: {:?}\nright: {:?}",
            explicit,
            value
        );

        if contains_float(&value)
            && !contains_non_finite_float(&value)
            && let Ok(json) = serde_json::to_string(&value)
        {
            let roundtrip = de::from_json_str(json.as_str())
                .expect("Val serialized to JSON should roundtrip through from_json_str");
            assert!(
                vals_equivalent(&roundtrip, &value),
                "JSON roundtrip mismatch:\nleft: {:?}\nright: {:?}",
                roundtrip,
                value
            );
        }
    }

    let mode = mode.trim().to_ascii_lowercase();
    let _ = match mode.as_str() {
        "json" => de::parse_with_format(body.as_str(), Some(Format::Json)),
        "yaml" => de::parse_with_format(body.as_str(), Some(Format::Yaml)),
        "toml" => de::parse_with_format(body.as_str(), Some(Format::Toml)),
        "default" => {
            let default = de::parse_with_format(body.as_str(), None);
            if let Ok(value) = &default {
                let explicit = de::parse_with_format(body.as_str(), Some(de::default_format()))
                    .expect("default parser should match explicit default_format parser");
                assert!(
                    vals_equivalent(&explicit, value),
                    "default parser mismatch:\nleft: {:?}\nright: {:?}",
                    explicit,
                    value
                );
            }
            default
        }
        "auto" => de::parse_auto(body.as_str()).map(|(_, value)| value),
        _ => de::parse_with_format(body.as_str(), None),
    };
});
