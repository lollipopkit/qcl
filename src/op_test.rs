#[cfg(test)]
mod tests {
    use crate::{expr::Expr, op::BinOp, val::Val};
    use std::{collections::HashMap, sync::Arc};

    #[cfg(feature = "json")]
    use serde_json::json;

    macro_rules! test_op {
        ($name:ident, $op:tt, $l:expr, $r:expr, $res:expr) => {
            #[test]
            #[cfg(feature = "json")]
            fn $name() {
                let ctx = json!({
                    "req": {"user": {"name": "lk", "age": 18}},
                    "list": [1, 2, 3],
                    "list-2": [1]
                });

                let l: Expr = $l.try_into().unwrap();
                let r: Expr = $r.try_into().unwrap();
                let res: Val = $res.into();
                assert_eq!(BinOp::$op.eval(&l, &r, &ctx.into()).unwrap(), res);
            }
        };
    }

    test_op!(add, Add, "@list.0", "1", 2);
    #[cfg(feature = "adv_arith")]
    test_op!(num_str_add, Add, "@list.0", "'str'", "1str");
    test_op!(sub, Sub, "@list.0", "1", 0);
    test_op!(mul, Mul, "@list.0", "2", 2);
    #[cfg(feature = "sem_arith")]
    test_op!(div, Div, "@list.2", "2", 1.5);
    #[cfg(not(feature = "sem_arith"))]
    test_op!(div, Div, "@list.2", "2", 1);
    #[cfg(feature = "adv_arith")]
    test_op!(list_add_val, Add, "@list", "4", vec![1, 2, 3, 4]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_add_list, Add, "@list", "@list-2", vec![1, 2, 3, 1]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_val, Sub, "@list", "2", vec![1, 3]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_list, Sub, "@list", "@list-2", vec![2, 3]);

    // Tests with literal expressions
    #[cfg(feature = "adv_arith")]
    #[cfg(feature = "json")]
    #[test]
    fn literal_list_operations() {
        let ctx: Val = json!({}).into();

        let l: Expr = "[1, 2, 3]".try_into().unwrap();
        let r: Expr = "[4, 5]".try_into().unwrap();

        let result = BinOp::Add.eval(&l, &r, &ctx).unwrap();
        let expected: Val = vec![1, 2, 3, 4, 5].into();
        assert_eq!(result, expected);
    }

    #[cfg(feature = "adv_arith")]
    #[cfg(feature = "json")]
    #[test]
    fn literal_map_operations() {
        let ctx: Val = json!({}).into();

        let l: Expr = r#"{"a": 1, "b": 2}"#.try_into().unwrap();
        let r: Expr = r#"{"c": 3, "a": 4}"#.try_into().unwrap();

        let result = BinOp::Add.eval(&l, &r, &ctx).unwrap();

        // The result should be a map with "a": 4, "b": 2, "c": 3
        if let Val::Map(map) = result {
            assert_eq!(map.get("a"), Some(&Val::Int(4)));
            assert_eq!(map.get("b"), Some(&Val::Int(2)));
            assert_eq!(map.get("c"), Some(&Val::Int(3)));
        } else {
            panic!("Expected map result");
        }
    }

    #[test]
    #[cfg(feature = "json")]
    fn comparison_with_literals() {
        let ctx = json!({
            "user": {"name": "Alice", "age": 25}
        })
        .into();

        // Compare with literal list
        let l: Expr = "@user.age".try_into().unwrap();
        let r: Expr = "25".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(true));

        // Test 'in' operator with literal list
        let l: Expr = "25".try_into().unwrap();
        let r: Expr = "[20, 25, 30]".try_into().unwrap();
        let result = BinOp::In.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(true));

        let l: Expr = "35".try_into().unwrap();
        let r: Expr = "[20, 25, 30]".try_into().unwrap();
        let result = BinOp::In.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));

        // Map membership is intentionally rejected to avoid ACL-shape confusion.
        let l: Expr = r#""name""#.try_into().unwrap();
        let r: Expr = r#"{"name": "Alice", "age": 25}"#.try_into().unwrap();
        assert!(BinOp::In.eval(&l, &r, &ctx).is_err());
    }

    #[test]
    #[cfg(feature = "json")]
    fn missing_membership_is_fail_closed_but_primitive_in_is_rejected() {
        let ctx = json!({
            "lhs": 1,
            "map": {"1": true}
        })
        .into();
        let ctx_missing = json!({
            "lhs": 1,
            "rhs": 5
        })
        .into();

        let l: Expr = "@lhs".try_into().unwrap();
        let r: Expr = "@map".try_into().unwrap();
        assert!(BinOp::In.eval(&l, &r, &ctx).is_err());

        let r_missing: Expr = "@missing".try_into().unwrap();
        assert!(BinOp::In.eval(&l, &r_missing, &ctx_missing).is_err());

        let l_missing: Expr = "@missing".try_into().unwrap();
        let r_list: Expr = "[1, 2, 3]".try_into().unwrap();
        assert_eq!(
            BinOp::In.eval(&l_missing, &r_list, &ctx_missing).unwrap(),
            Val::Bool(false)
        );

        let r: Expr = "@rhs".try_into().unwrap();
        assert!(BinOp::In.eval(&l, &r, &ctx_missing).is_err());
    }

    #[test]
    #[cfg(feature = "json")]
    fn nested_literal_comparisons() {
        let ctx: Val = json!({}).into();

        // Compare nested lists
        let l: Expr = "[[1, 2], [3, 4]]".try_into().unwrap();
        let r: Expr = "[[1, 2], [3, 4]]".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(true));

        let l: Expr = "[[1, 2], [3, 4]]".try_into().unwrap();
        let r: Expr = "[[1, 2], [3, 5]]".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));

        // Compare nested maps
        let l: Expr = r#"{"user": {"name": "Alice"}}"#.try_into().unwrap();
        let r: Expr = r#"{"user": {"name": "Alice"}}"#.try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(true));

        let l: Expr = r#"{"user": {"name": "Alice"}}"#.try_into().unwrap();
        let r: Expr = r#"{"user": {"name": "Bob"}}"#.try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));
    }

    #[test]
    #[cfg(feature = "json")]
    fn mixed_type_comparisons() {
        let ctx: Val = json!({}).into();

        // List vs non-list
        let l: Expr = "[1, 2, 3]".try_into().unwrap();
        let r: Expr = "123".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));

        // Map vs non-map
        let l: Expr = r#"{"a": 1}"#.try_into().unwrap();
        let r: Expr = "1".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));

        // Empty structures
        let l: Expr = "[]".try_into().unwrap();
        let r: Expr = "{}".try_into().unwrap();
        let result = BinOp::Eq.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Bool(false));
    }

    #[test]
    #[cfg(feature = "json")]
    fn arithmetic_with_context_and_literals() {
        let ctx = json!({
            "base": 10,
            "multiplier": 2
        })
        .into();

        // Add context value to literal list
        #[cfg(feature = "adv_arith")]
        {
            let l: Expr = "[1, 2, 3]".try_into().unwrap();
            let r: Expr = "@base".try_into().unwrap();
            let result = BinOp::Add.eval(&l, &r, &ctx).unwrap();
            let expected: Val = vec![1, 2, 3, 10].into();
            assert_eq!(result, expected);
        }

        // Multiply literal with context
        let l: Expr = "5".try_into().unwrap();
        let r: Expr = "@multiplier".try_into().unwrap();
        let result = BinOp::Mul.eval(&l, &r, &ctx).unwrap();
        assert_eq!(result, Val::Int(10));
    }

    #[test]
    fn large_single_value_membership_uses_indexed_lookup() {
        let haystack = Val::List(Arc::new((0..64).map(Val::Int).collect()));

        assert!(BinOp::In.cmp(&Val::Int(42), &haystack).unwrap());
        assert!(!BinOp::In.cmp(&Val::Int(99), &haystack).unwrap());

        let haystack = Val::List(Arc::new(vec![
            Val::Bool(false),
            Val::Str("alpha".into()),
            Val::Bool(true),
            Val::Str("needle".into()),
            Val::Int(1),
            Val::Int(2),
            Val::Int(3),
            Val::Int(4),
            Val::Int(5),
            Val::Int(6),
            Val::Int(7),
            Val::Int(8),
            Val::Int(9),
            Val::Int(10),
            Val::Int(11),
            Val::Int(12),
            Val::Int(13),
            Val::Int(14),
            Val::Int(15),
            Val::Int(16),
            Val::Int(17),
            Val::Int(18),
            Val::Int(19),
            Val::Int(20),
            Val::Int(21),
            Val::Int(22),
            Val::Int(23),
            Val::Int(24),
            Val::Int(25),
            Val::Int(26),
            Val::Int(27),
            Val::Int(28),
            Val::Int(29),
            Val::Int(30),
            Val::Int(31),
            Val::Int(32),
            Val::Int(33),
            Val::Int(34),
            Val::Int(35),
            Val::Int(36),
        ]));

        assert!(BinOp::In.cmp(&Val::Str("needle".into()), &haystack).unwrap());
        assert!(BinOp::In.cmp(&Val::Bool(true), &haystack).unwrap());
        assert!(!BinOp::In.cmp(&Val::Str("missing".into()), &haystack).unwrap());
    }

    #[test]
    fn large_mixed_type_list_membership_keeps_semantics() {
        let mut nested_map = HashMap::new();
        nested_map.insert("k".to_string(), Val::Int(7));
        let nested_map = Val::Map(Arc::new(nested_map));
        let nested_list = Val::List(Arc::new(vec![Val::Int(1), Val::Int(2)]));

        let lhs = Val::List(Arc::new(vec![
            Val::Int(41),
            Val::Str("needle".into()),
            Val::Bool(true),
            nested_list.clone(),
            nested_map.clone(),
        ]));

        let rhs = Val::List(Arc::new(vec![
            Val::Int(0),
            Val::Int(1),
            Val::Int(2),
            Val::Int(3),
            Val::Int(4),
            Val::Int(5),
            Val::Int(6),
            Val::Int(7),
            Val::Int(8),
            Val::Int(9),
            Val::Int(10),
            Val::Int(11),
            Val::Int(12),
            Val::Int(13),
            Val::Int(14),
            Val::Int(15),
            Val::Int(16),
            Val::Int(17),
            Val::Int(18),
            Val::Int(19),
            Val::Int(20),
            Val::Int(21),
            Val::Int(22),
            Val::Int(23),
            Val::Int(24),
            Val::Int(25),
            Val::Int(26),
            Val::Int(27),
            Val::Int(28),
            Val::Int(29),
            Val::Int(30),
            Val::Int(31),
            Val::Int(32),
            Val::Int(33),
            Val::Int(34),
            Val::Int(35),
            Val::Int(36),
            Val::Int(37),
            Val::Int(38),
            Val::Int(39),
            Val::Int(40),
            Val::Int(41),
            Val::Str("needle".into()),
            Val::Bool(false),
            Val::Bool(true),
            nested_list,
            nested_map,
        ]));

        assert!(BinOp::In.cmp(&lhs, &rhs).unwrap());

        let lhs_missing = Val::List(Arc::new(vec![
            Val::Int(41),
            Val::Str("needle".into()),
            Val::Bool(true),
            Val::List(Arc::new(vec![Val::Int(9), Val::Int(9)])),
        ]));
        assert!(!BinOp::In.cmp(&lhs_missing, &rhs).unwrap());
    }
}
