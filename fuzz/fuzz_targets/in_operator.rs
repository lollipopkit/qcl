#![no_main]

mod common;

use std::{collections::HashMap, sync::Arc};

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use qcl::{expr::Expr, val::Val};

fn list_contains(list: &[Val], needle: &Val) -> bool {
    list.iter().any(|value| value == needle)
}

fn atoms_to_vals(atoms: Vec<common::Atom>) -> Vec<Val> {
    atoms
        .into_iter()
        .take(common::MAX_LIST_LEN)
        .map(common::Atom::into_val)
        .collect()
}

fn fallback_case(data: &[u8]) -> common::InCase {
    match data.first().copied().unwrap_or(0) % 4 {
        0 => common::InCase::StringInString(common::StringInStringCase {
            needle: "adm".to_string(),
            haystack: common::lossy_text(data, 32),
        }),
        1 => common::InCase::ScalarInList(common::ScalarInListCase {
            needle: common::Atom::Int(7),
            haystack: vec![common::Atom::Int(1), common::Atom::Int(7), common::Atom::Int(9)],
        }),
        2 => common::InCase::ListInList(common::ListInListCase {
            needles: vec![
                common::Atom::Str("read".to_string()),
                common::Atom::Str("write".to_string()),
            ],
            haystack: vec![
                common::Atom::Str("read".to_string()),
                common::Atom::Str("write".to_string()),
                common::Atom::Str("delete".to_string()),
            ],
        }),
        _ => common::InCase::MapKey(common::MapKeyCase {
            needle: common::Atom::Int(42),
            entries: vec![
                (common::Atom::Int(42), common::Atom::Bool(true)),
                (common::Atom::Str("user".to_string()), common::Atom::Int(1)),
            ],
        }),
    }
}

fn decode_case(data: &[u8]) -> common::InCase {
    let mut u = Unstructured::new(data);
    common::InCase::arbitrary(&mut u).unwrap_or_else(|_| fallback_case(data))
}

fn eval_membership(lhs: Val, rhs: Val) -> Result<bool, String> {
    let mut ctx = HashMap::new();
    ctx.insert("lhs".to_string(), lhs);
    ctx.insert("rhs".to_string(), rhs);
    let ctx = Val::Map(Arc::new(ctx));
    let expr = Expr::try_from("@lhs in @rhs").expect("fixed in-operator expression must parse");
    let result = match expr.eval(&ctx).map_err(|err| err.to_string())? {
        Val::Bool(value) => value,
        other => panic!("expected boolean result from in operator, got {other:?}"),
    };
    Ok(result)
}

fuzz_target!(|data: &[u8]| {
    match decode_case(data) {
        common::InCase::StringInString(case) => {
            let needle = common::truncate_chars(case.needle.as_str(), common::MAX_STRING_LEN);
            let haystack = common::truncate_chars(case.haystack.as_str(), common::MAX_STRING_LEN);
            let expected = haystack.contains(needle.as_str());
            let actual = eval_membership(Val::from(needle), Val::from(haystack))
                .expect("string membership should remain supported");
            assert_eq!(actual, expected);
        }
        common::InCase::ScalarInList(case) => {
            let needle = case.needle.into_val();
            let haystack = atoms_to_vals(case.haystack);
            let expected = list_contains(&haystack, &needle);
            let actual =
                eval_membership(needle, Val::List(Arc::new(haystack))).expect("scalar-in-list should remain supported");
            assert_eq!(actual, expected);
        }
        common::InCase::ListInList(case) => {
            let needles = atoms_to_vals(case.needles);
            let haystack = atoms_to_vals(case.haystack);
            let expected = needles.iter().all(|needle| list_contains(&haystack, needle));
            let actual = eval_membership(Val::List(Arc::new(needles)), Val::List(Arc::new(haystack)))
                .expect("list-in-list should remain supported");
            assert_eq!(actual, expected);
        }
        common::InCase::MapKey(case) => {
            let needle = case.needle.into_val();
            let mut map = HashMap::new();
            for (key_atom, value_atom) in case.entries.into_iter().take(common::MAX_LIST_LEN) {
                let key = key_atom.into_val();
                let Some(key) = (match key {
                    Val::Str(s) => Some(s.as_ref().to_string()),
                    Val::Int(i) => Some(i.to_string()),
                    Val::Float(f) => Some(f.to_string()),
                    Val::Bool(b) => Some(b.to_string()),
                    Val::Nil | Val::Map(_) | Val::List(_) => None,
                }) else {
                    continue;
                };
                map.insert(key, value_atom.into_val());
            }

            let actual = eval_membership(needle.clone(), Val::Map(Arc::new(map.clone())));
            match &needle {
                Val::Str(s) => {
                    let expected = map.contains_key(s.as_ref());
                    let actual = actual.expect("string-in-map should be supported");
                    assert_eq!(actual, expected);
                }
                _ => {
                    // Non-string keys against maps should still error
                    assert!(actual.is_err(), "non-string key in map should fail: {needle:?}");
                }
            }
        }
    }
});
