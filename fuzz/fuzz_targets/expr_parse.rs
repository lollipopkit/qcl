#![no_main]

mod common;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use qcl::{expr::Expr, token::Tokenizer};

fn probe_candidate(candidate: &str) {
    let _ = Tokenizer::new(candidate);

    if let Ok(expr) = Expr::try_from(candidate) {
        let reparsed = Expr::try_from(candidate).expect("successful Expr::try_from result should be reproducible");
        assert_eq!(expr, reparsed);
        let _ = expr.requested_ctx();
    }
}

fn build_candidate(data: &[u8]) -> String {
    let mut u = Unstructured::new(data);
    if let Ok(input) = common::ParseInput::arbitrary(&mut u) {
        let body = {
            let candidate = common::normalize_expr(input.expr);
            if candidate.trim().is_empty() {
                common::default_expr()
            } else {
                candidate
            }
        };

        return match input.mode {
            common::ParseMode::Raw => body,
            common::ParseMode::DeepParens => common::build_deep_parens(usize::from(input.depth), body.as_str()),
            common::ParseMode::WrappedQuery => {
                let wrapped = format!("({body}) && @flags.active");
                common::build_deep_parens(usize::from(input.depth), wrapped.as_str())
            }
        };
    }

    let fallback = common::lossy_text(data, common::MAX_EXPR_LEN);
    if fallback.trim().is_empty() {
        common::default_expr()
    } else {
        fallback
    }
}

fuzz_target!(|data: &[u8]| {
    let raw = common::lossy_text(data, common::MAX_EXPR_LEN);
    if !raw.trim().is_empty() {
        probe_candidate(raw.as_str());
    }

    let candidate = build_candidate(data);
    if candidate != raw {
        probe_candidate(candidate.as_str());
    }
});
