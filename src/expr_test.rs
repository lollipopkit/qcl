#[cfg(test)]
mod test {
    use anyhow::Result;
    #[cfg(feature = "json")]
    use serde_json::json;
    use std::collections::HashSet;
    use std::sync::Mutex;

    use crate::{
        expr::{
            Expr, parse_cache_entries_per_shard_for_tests, parse_cache_entry_count_for_tests,
            parse_cache_shard_count_for_tests, parse_cache_shard_idx_for_tests, reset_parse_cache_for_tests,
        },
        val::Val,
    };

    static PARSE_CACHE_TEST_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    #[cfg(feature = "json")]
    fn simple() {
        expect("@pub", true);
        expect("@user.name + 'pt'", "lkpt");
        expect("@user.age + @list.0 == 19", true);

        #[cfg(feature = "adv_arith")]
        expect("@user.name + @user.age", "lk18");

        #[cfg(feature = "adv_arith")]
        expect("@list + @list-2", vec![1, 2, 3, 2]);

        #[cfg(feature = "adv_arith")]
        expect("@list - @list-2", vec![1, 3]);

        #[cfg(feature = "sem_arith")]
        expect("@list.2 / 2", 1.5);

        #[cfg(not(feature = "sem_arith"))]
        expect("@list.2 / 2", 1);

        panic("@user.name + @list");

        #[cfg(not(feature = "adv_arith"))]
        panic("@user.name + @list-2.0");
    }

    #[test]
    #[cfg(feature = "json")]
    fn complex_expressions() {
        // Nested arithmetic operations
        expect("(@user.age + 2) * 3", 60);

        // Parenthesized expressions
        expect("@user.age * (2 + 1)", 54);

        // Multiple operators with precedence
        expect("@user.age + 2 * 3", 24);

        // Comparison operators
        expect("@user.age > 17", true);
        expect("@user.age < 19", true);
        expect("@user.age >= 18", true);
        expect("@user.age <= 18", true);
        expect("@user.age == 18", true);
        expect("@user.age != 19", true);
    }

    #[test]
    #[cfg(feature = "json")]
    fn logical_operators() {
        // AND operator
        expect("@pub && @user.age > 17", true);
        expect("@pub && @user.age > 20", false);

        // OR operator
        expect("@user.age > 20 || @pub", true);
        expect("@user.age > 20 || @user.name == 'john'", false);

        // Complex logical expressions
        expect("@pub && (@user.age > 17 || @user.name == 'john')", true);
        expect("@pub || (@user.age < 17 && @user.name == 'john')", true);

        // Short-circuit evaluation
        expect("false && @nonexistent.field", false);
        expect("true || @nonexistent.field", true);
    }

    #[test]
    #[cfg(feature = "json")]
    fn unary_operations() {
        // Logical NOT
        expect("!@pub", false);
        expect("!false", true);

        // Double negation
        expect("!!@pub", true);

        // NOT with expressions
        expect("!(@user.age > 20)", true);
    }

    #[test]
    #[cfg(feature = "json")]
    fn map_and_list_access() {
        // Nested map access
        expect("@nested.level1.level2", "value");

        // List access with variable index
        expect("@list.(@index)", 2);

        // Access with expressions
        // `index-1` is an Id, but `index - 1` is a BinOp
        expect("@list.(@index - 1)", 1);

        // Access with complex expressions
        #[cfg(feature = "adv_arith")]
        expect("@list-2.(2 - 2) + @user.name", "2lk");
    }

    #[test]
    #[cfg(feature = "json")]
    fn list_literals() {
        // Empty list
        expect("[]", Vec::<Val>::new());

        // Simple list
        expect("[1, 2, 3]", vec![1, 2, 3]);

        // Mixed types
        expect(
            r#"[1, "hello", true]"#,
            vec![Val::Int(1), Val::Str("hello".into()), Val::Bool(true)],
        );

        // Nested lists
        expect("[[1, 2], [3, 4]]", vec![vec![1, 2], vec![3, 4]]);

        // List with expressions
        expect("[1 + 2, 3 * 4]", vec![3, 12]);

        // List with context access
        expect("[@user.age, @list.0]", vec![18, 1]);
    }

    #[test]
    #[cfg(feature = "json")]
    fn map_literals() {
        use std::collections::HashMap;

        // Empty map
        expect("{}", HashMap::<String, Val>::new());

        // Simple map
        let mut expected = HashMap::new();
        expected.insert("name".to_string(), Val::Str("Alice".into()));
        expected.insert("age".to_string(), Val::Int(30));
        expect(r#"{"name": "Alice", "age": 30}"#, expected);

        // Map with expressions
        let mut expected = HashMap::new();
        expected.insert("sum".to_string(), Val::Int(5));
        expected.insert("product".to_string(), Val::Int(6));
        expect(r#"{"sum": 2 + 3, "product": 2 * 3}"#, expected);

        // Map with context access
        let mut expected = HashMap::new();
        expected.insert("user_name".to_string(), Val::Str("lk".into()));
        expected.insert("user_age".to_string(), Val::Int(18));
        expect(r#"{"user_name": @user.name, "user_age": @user.age}"#, expected);

        // Map with different key types
        let mut expected = HashMap::new();
        expected.insert("42".to_string(), Val::Str("number".into()));
        expected.insert("true".to_string(), Val::Str("bool".into()));
        expected.insert("key".to_string(), Val::Str("string".into()));
        expect(r#"{42: "number", true: "bool", "key": "string"}"#, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn map_literal_key_stringification_constant_and_runtime() {
        use std::collections::HashMap;

        let mut folded_expected = HashMap::new();
        folded_expected.insert("folded".to_string(), Val::Str("string".into()));
        folded_expected.insert("42".to_string(), Val::Str("number".into()));
        folded_expected.insert("true".to_string(), Val::Str("bool".into()));
        expect(r#"{"folded": "string", 42: "number", true: "bool"}"#, folded_expected);

        let mut runtime_expected = HashMap::new();
        runtime_expected.insert("lk".to_string(), Val::Str("string".into()));
        runtime_expected.insert("18".to_string(), Val::Str("number".into()));
        runtime_expected.insert("true".to_string(), Val::Str("bool".into()));
        expect(
            r#"{@user.name: "string", @user.age: "number", @pub: "bool"}"#,
            runtime_expected,
        );
    }

    #[test]
    #[cfg(feature = "json")]
    fn map_literal_invalid_key_types_error() {
        panic(r#"{[1, 2]: "invalid"}"#);
        panic(r#"{{}: "invalid"}"#);
        panic(r#"{@list: "invalid"}"#);
        panic(r#"{@nested: "invalid"}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn constant_folding_large_list_and_map() {
        let list_expr = format!("[{}]", (0..128).map(|i| i.to_string()).collect::<Vec<_>>().join(", "));
        let parsed_list = Expr::try_from(list_expr.as_str()).unwrap();
        let expected_list = Val::List((0..128).map(Val::Int).collect::<Vec<_>>().into());
        assert_eq!(parsed_list, Expr::Val(expected_list));

        let map_expr = format!(
            "{{{}}}",
            (0..128).map(|i| format!("{i}: {i}")).collect::<Vec<_>>().join(", ")
        );
        let parsed_map = Expr::try_from(map_expr.as_str()).unwrap();
        let expected_map = Val::Map(
            (0..128)
                .map(|i| (i.to_string(), Val::Int(i)))
                .collect::<std::collections::HashMap<_, _>>()
                .into(),
        );
        assert_eq!(parsed_map, Expr::Val(expected_map));
    }

    #[test]
    #[cfg(feature = "json")]
    fn nested_structures() {
        use std::collections::HashMap;

        // List of maps
        let mut map1 = HashMap::new();
        map1.insert("name".to_string(), Val::Str("Alice".into()));
        map1.insert("age".to_string(), Val::Int(30));

        let mut map2 = HashMap::new();
        map2.insert("name".to_string(), Val::Str("Bob".into()));
        map2.insert("age".to_string(), Val::Int(25));

        expect(
            r#"[{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]"#,
            vec![Val::Map(map1.into()), Val::Map(map2.into())],
        );

        // Map with lists
        let mut expected = HashMap::new();
        expected.insert(
            "numbers".to_string(),
            Val::List(vec![Val::Int(1), Val::Int(2), Val::Int(3)].into()),
        );
        expected.insert("active".to_string(), Val::Bool(true));
        expect(r#"{"numbers": [1, 2, 3], "active": true}"#, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn literal_access() {
        // Access elements from list literals
        expect("[1, 2, 3].1", 2);
        expect(r#"["hello", "world"].0"#, "hello");

        // Access fields from map literals
        expect(r#"{"name": "Alice", "age": 30}.name"#, "Alice");
        expect(r#"{"name": "Alice", "age": 30}.age"#, 30);

        // Nested access
        expect(r#"[{"name": "Alice"}, {"name": "Bob"}].0.name"#, "Alice");
        expect(r#"{"users": [1, 2, 3]}.users.1"#, 2);
    }

    #[test]
    #[cfg(feature = "json")]
    fn trailing_commas() {
        // List with trailing comma
        expect("[1, 2, 3,]", vec![1, 2, 3]);

        // Map with trailing comma
        use std::collections::HashMap;
        let mut expected = HashMap::new();
        expected.insert("a".to_string(), Val::Int(1));
        expected.insert("b".to_string(), Val::Int(2));
        expect(r#"{"a": 1, "b": 2,}"#, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn error_cases() {
        // Invalid map key types
        panic(r#"{[1, 2]: "invalid"}"#);
        panic(r#"{{}: "invalid"}"#);
    }

    #[test]
    fn test_requested_ctx() {
        let expr = Expr::try_from("@user.props.(@req.service) && @list.0 || @pub").unwrap();
        let names = expr.requested_ctx();

        let mut expected = HashSet::new();
        expected.insert("user".to_string());
        expected.insert("req".to_string());
        expected.insert("list".to_string());
        expected.insert("pub".to_string());

        assert_eq!(names, expected);

        // Test with list/map literals containing context access
        let expr = Expr::try_from(r#"[@user.name, @list.0] == {"name": @user.name, "first": @list.0}"#).unwrap();
        let names = expr.requested_ctx();

        let mut expected = HashSet::new();
        expected.insert("user".to_string());
        expected.insert("list".to_string());

        assert_eq!(names, expected);
    }

    #[test]
    fn test_is_ctx_independent() {
        assert!(
            Expr::try_from(r#"{"a": [1, 2, 3].0, "b": (1 + 2) * 3}"#)
                .unwrap()
                .is_ctx_independent()
        );
        assert!(!Expr::try_from("@user.name").unwrap().is_ctx_independent());
        assert!(
            !Expr::try_from(r#"[@user.name, {"k": 1}]"#)
                .unwrap()
                .is_ctx_independent()
        );
        assert!(!Expr::try_from(r#"({"k": @user.name})"#).unwrap().is_ctx_independent());
    }

    #[test]
    fn parse_cache_remains_functional_under_pressure() {
        let _guard = PARSE_CACHE_TEST_LOCK.lock().unwrap();
        reset_parse_cache_for_tests();

        for i in 0..5000 {
            let text = format!("{i} == {i}");
            let expr = Expr::parse_cached_arc(text.as_str()).unwrap();
            assert_eq!(expr.eval(&Val::Nil).unwrap(), Val::Bool(true));
        }

        let payload = "x".repeat(5000);
        let large = format!(r#""{payload}" == "{payload}""#);
        let expr = Expr::parse_cached_arc(large.as_str()).unwrap();
        assert_eq!(expr.eval(&Val::Nil).unwrap(), Val::Bool(true));
    }

    #[test]
    fn parse_cache_eviction_stays_local_to_shards() {
        let _guard = PARSE_CACHE_TEST_LOCK.lock().unwrap();
        reset_parse_cache_for_tests();

        let protected_expr = "1 == 1";
        let protected = Expr::parse_cached_arc(protected_expr).unwrap();
        let protected_shard = parse_cache_shard_idx_for_tests(protected_expr);
        let target_shard = (protected_shard + 1) % parse_cache_shard_count_for_tests();
        let hot_count = parse_cache_entries_per_shard_for_tests() + 64;

        let hot_exprs = collect_exprs_for_shard(target_shard, hot_count);
        for expr in &hot_exprs {
            let parsed = Expr::parse_cached_arc(expr).unwrap();
            assert_eq!(parsed.eval(&Val::Nil).unwrap(), Val::Bool(true));
        }

        let protected_again = Expr::parse_cached_arc(protected_expr).unwrap();
        assert!(std::sync::Arc::ptr_eq(&protected, &protected_again));
        assert!(parse_cache_entry_count_for_tests() <= parse_cache_entries_per_shard_for_tests() + 1);
    }

    #[test]
    fn parse_cache_uses_smooth_local_fifo_eviction() {
        let _guard = PARSE_CACHE_TEST_LOCK.lock().unwrap();
        reset_parse_cache_for_tests();

        let target_shard = 0usize;
        let exprs = collect_exprs_for_shard(target_shard, parse_cache_entries_per_shard_for_tests() + 2);

        let first = Expr::parse_cached_arc(exprs[0].as_str()).unwrap();
        for expr in &exprs[1..] {
            let parsed = Expr::parse_cached_arc(expr).unwrap();
            assert_eq!(parsed.eval(&Val::Nil).unwrap(), Val::Bool(true));
        }

        let first_again = Expr::parse_cached_arc(exprs[0].as_str()).unwrap();
        let newest_again = Expr::parse_cached_arc(exprs.last().unwrap().as_str()).unwrap();

        assert!(!std::sync::Arc::ptr_eq(&first, &first_again));
        assert!(std::sync::Arc::ptr_eq(
            &Expr::parse_cached_arc(exprs.last().unwrap().as_str()).unwrap(),
            &newest_again
        ));
        assert!(parse_cache_entry_count_for_tests() <= parse_cache_entries_per_shard_for_tests());
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_nil_handling() {
        expect("@nonexistent", None::<Val>);
        expect("@existing_null == nil", true);
        expect("@nonexistent == nil", true);
        expect("@nonexistent != nil", false);
        expect("@nonexistent != 1", true);
        expect("@nonexistent.field == nil", true);
        expect("nil", None::<Val>);
    }

    #[test]
    #[cfg(feature = "json")]
    fn nil_field_access() {
        let ctx: Val = json!({
            "req": {"user": {"id": 7}},
            "record": {}
        })
        .into();

        let expr = Expr::try_from("@req.user.id == @record.owner.id").unwrap();
        assert_eq!(expr.eval(&ctx).unwrap(), Val::Bool(false));

        let expr = Expr::try_from(r#"@req.user.status != "blocked""#).unwrap();
        assert_eq!(expr.eval(&ctx).unwrap(), Val::Bool(true));

        let expr = Expr::try_from(r#"@req.user.status != "blocked" && @req.user.id == 7"#).unwrap();
        assert_eq!(expr.eval(&ctx).unwrap(), Val::Bool(true));
    }

    #[test]
    #[cfg(feature = "json")]
    fn unsupported_in_membership_fails_closed() {
        panic(r#"@user.age in {"18": true}"#);

        #[cfg(feature = "adv_arith")]
        panic("@user.age in 21");
    }

    #[test]
    #[cfg(feature = "json")]
    fn zero_division_and_modulo_fail_closed() {
        let static_mod = Expr::try_from("1 % 0").unwrap();
        assert!(static_mod.eval(&Val::Nil).is_err());

        let static_div = Expr::try_from("1 / 0").unwrap();
        assert!(static_div.eval(&Val::Nil).is_err());

        let ctx: Val = json!({ "x": 1 }).into();

        let dynamic_mod = Expr::try_from("@x % 0").unwrap();
        assert!(dynamic_mod.eval(&ctx).is_err());

        let dynamic_div = Expr::try_from("@x / 0").unwrap();
        assert!(dynamic_div.eval(&ctx).is_err());
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_quoted_field_access() {
        // Basic quoted field access
        expect(r#"@"with.&=""#, true);

        // Nested quoted field access
        expect(r#"@req."user"."name""#, "lk");

        // Mixed quoted and unquoted access
        expect(r#"@user."name""#, "lk");
        expect(r#"@"user".name"#, "lk");
        expect(r#"@"user"."name""#, "lk");

        // Quoted field with special characters
        expect(r#"@"special-chars""#, "test-value");

        // Quoted field in complex expression
        expect(r#"@"with.&=" && @user.age > 17"#, true);
        expect(r#"@user."name" + "-suffix""#, "lk-suffix");

        // Quoted numeric field name
        expect(r#"@"123""#, "numeric-field");

        // Single quotes vs double quotes
        expect(r#"@'special-chars'"#, "test-value");
        expect(r#"@escaped."field\"name""#, "quoted-field");
        expect(r#"@escaped."path\\segment""#, "backslash-field");
        expect(r#"@escaped.'field\'name'"#, "single-quoted-field");
    }

    #[cfg(feature = "json")]
    fn with_ctx(rule: &str) -> Result<Val> {
        let ctx: Val = json!({
            "user": {"name": "lk", "age": 18},
            "req": {"user": {"name": "lk"}},
            "list": [1, 2, 3],
            "list-2": [2],
            "pub": true,
            "index": 1,
            "existing_null": null,
            "nested": {
                "level1": {
                    "level2": "value"
                }
            },
            "with.&=": true,
            "special-chars": "test-value",
            "123": "numeric-field",
            "escaped": {
                "field\"name": "quoted-field",
                "path\\segment": "backslash-field",
                "field'name": "single-quoted-field"
            }
        })
        .into();
        let expr = Expr::try_from(rule)?;
        expr.eval(&ctx)
    }

    #[cfg(feature = "json")]
    fn expect<V: Into<Val>>(rule: &str, val: V) {
        let res = with_ctx(rule);
        assert_eq!(res.unwrap(), val.into());
    }

    #[cfg(feature = "json")]
    fn panic(rule: &str) {
        let res = with_ctx(rule);
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{}", err);
    }

    fn collect_exprs_for_shard(target_shard: usize, count: usize) -> Vec<String> {
        let mut exprs = Vec::with_capacity(count);
        let mut i = 0usize;
        let mut attempts = 0usize;
        let max_attempts = count.saturating_mul(10_000).max(10_000);

        while exprs.len() < count {
            attempts += 1;
            assert!(
                attempts <= max_attempts,
                "failed to collect expressions for shard {target_shard} after {attempts} attempts"
            );

            let expr = format!("{i} == {i}");
            if parse_cache_shard_idx_for_tests(expr.as_str()) == target_shard {
                exprs.push(expr);
            }
            i += 1;
        }

        exprs
    }
}
