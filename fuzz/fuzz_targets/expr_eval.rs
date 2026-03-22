#![no_main]

mod common;

use libfuzzer_sys::fuzz_target;
use qcl::{
    de::{self, Format},
    expr::Expr,
    val::Val,
};

const MODE_DELIMITER: &[u8] = b"\n--MODE--\n";
const CTX_DELIMITER: &[u8] = b"\n--CTX--\n";

fn decode_payload(data: &[u8]) -> (String, String, String) {
    if let Some((expr_bytes, rest)) = common::split_once_bytes(data, MODE_DELIMITER) {
        if let Some((mode_bytes, ctx_bytes)) = common::split_once_bytes(rest, CTX_DELIMITER) {
            let expr = common::lossy_text(expr_bytes, common::MAX_EXPR_LEN);
            let mode = common::lossy_text(mode_bytes, 16);
            let ctx = common::lossy_text(ctx_bytes, common::MAX_TEXT_LEN);
            let expr = if expr.trim().is_empty() {
                common::default_expr()
            } else {
                expr
            };
            let ctx = if ctx.trim().is_empty() {
                common::default_ctx()
            } else {
                ctx
            };
            return (expr, mode, ctx);
        }
    }

    if let Some((expr_bytes, ctx_bytes)) = common::split_once_bytes(data, CTX_DELIMITER) {
        let expr = common::lossy_text(expr_bytes, common::MAX_EXPR_LEN);
        let ctx = common::lossy_text(ctx_bytes, common::MAX_TEXT_LEN);
        let expr = if expr.trim().is_empty() {
            common::default_expr()
        } else {
            expr
        };
        let ctx = if ctx.trim().is_empty() {
            common::default_ctx()
        } else {
            ctx
        };
        return (expr, "default".to_string(), ctx);
    }

    let expr = common::lossy_text(data, common::MAX_EXPR_LEN);
    let expr = if expr.trim().is_empty() {
        common::default_expr()
    } else {
        expr
    };
    (expr, "default".to_string(), common::default_ctx())
}

fn stable_eval(expr: &Expr, ctx: &Val) -> Result<Val, String> {
    expr.eval(ctx).map_err(|err| err.to_string())
}

fuzz_target!(|data: &[u8]| {
    let (expr_text, mode, ctx_text) = decode_payload(data);

    let Ok(ctx) = (match mode.trim().to_ascii_lowercase().as_str() {
        "json" => de::parse_with_format(ctx_text.as_str(), Some(Format::Json)),
        "yaml" => de::parse_with_format(ctx_text.as_str(), Some(Format::Yaml)),
        "toml" => de::parse_with_format(ctx_text.as_str(), Some(Format::Toml)),
        "auto" => de::parse_auto(ctx_text.as_str()).map(|(_, value)| value),
        "default" => de::parse_with_format(ctx_text.as_str(), None),
        _ => de::parse_with_format(ctx_text.as_str(), None),
    }) else {
        return;
    };
    let Ok(expr) = Expr::try_from(expr_text.as_str()) else {
        return;
    };

    let first = stable_eval(&expr, &ctx);
    let second = stable_eval(&expr, &ctx);
    assert_eq!(first, second);

    if let Ok(value) = &first {
        let _ = value.to_string();
    }

    if expr.requested_ctx().is_empty() {
        let without_ctx = stable_eval(&expr, &Val::Nil);
        assert_eq!(first, without_ctx);
    }
});
