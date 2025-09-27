#[cfg(test)]
mod test {
    use anyhow::Result;
    #[cfg(feature = "json")]
    use serde_json::json;
    use std::collections::HashSet;

    use crate::{expr::Expr, val::Val};

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
    fn ternary_operator() {
        // Basic boolean conditions
        expect("true ? 1 : 2", 1);
        expect("false ? 1 : 2", 2);

        // With context
        expect("@pub ? @user.name : 'guest'", "lk");

        // Short-circuit: only selected branch should evaluate
        expect("false ? (@nonexistent.field) : 42", 42);

        // Precedence with arithmetic on else branch
        expect("true ? 1 : 2 + 3", 1);
        expect("false ? 1 : 2 + 3", 5);

        // Nested ternary
        expect("true ? (false ? 1 : 2) : 3", 2);

        // Nullish inside else branch
        expect("false ? 1 : (nil ?? 5)", 5);

        // Ternary as map key requires parentheses to avoid ambiguity with ':'
        use std::collections::HashMap;
        let mut expected = HashMap::new();
        expected.insert("x".to_string(), Val::Int(1));
        expect("{('a' == 'a' ? 'x' : 'y'): 1}", expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn nullish_coalescing_operations() {
        // Basic nullish coalescing with nil values
        expect("@nonexistent.field ?? 'default'", "default");
        expect("@user.name ?? 'default'", "lk");
        expect("nil ?? 'fallback'", "fallback");
        expect("'actual' ?? 'fallback'", "actual");
        
        // Numeric nullish coalescing
        expect("@nonexistent.age ?? 18", 18);
        expect("@user.age ?? 100", 18);
        
        // Boolean nullish coalescing
        expect("@nonexistent.active ?? true", true);
        expect("@pub ?? false", true);
        
        // Complex expressions with nullish coalescing
        expect("@user.nonexistent ?? @user.name ?? 'unknown'", "lk");
        expect("@user.name ?? @user.age ?? 'fallback'", "lk");
        
        // Nested nullish coalescing with other operators
        expect("(@nonexistent.value ?? 5) + 10", 15);
        expect("(@user.name ?? 'guest') == 'lk'", true);
        expect("@user.name ?? ('guest' == 'lk')", "lk");
        
        // Constant folding
        expect("'hello' ?? 'world'", "hello");
        expect("nil ?? 'constant'", "constant");
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
        expect(
            r#"{"user_name": @user.name, "user_age": @user.age}"#,
            expected,
        );

        // Map with different key types
        let mut expected = HashMap::new();
        expected.insert("42".to_string(), Val::Str("number".into()));
        expected.insert("true".to_string(), Val::Str("bool".into()));
        expected.insert("key".to_string(), Val::Str("string".into()));
        expect(r#"{42: "number", true: "bool", "key": "string"}"#, expected);
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
    fn bracket_index_access() {
        // List indexing with brackets
        expect("[1, 2, 3][1]", 2);
        expect(r#"["hello", "world"][0]"#, "hello");

        // Map indexing with string key
        expect(r#"{"name": "Alice", "age": 30}["name"]"#, "Alice");

        // Context access with brackets
        expect("@list[0]", 1);
        expect(r#"@nested["level1"]["level2"]"#, "value");

        // Mixed bracket and dot access
        expect(r#"@nested["level1"].level2"#, "value");
        expect(r#"{ "a": [10, 20, 30] }["a"][2]"#, 30);
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
        let expr =
            Expr::try_from(r#"[@user.name, @list.0] == {"name": @user.name, "first": @list.0}"#)
                .unwrap();
        let names = expr.requested_ctx();

        let mut expected = HashSet::new();
        expected.insert("user".to_string());
        expected.insert("list".to_string());

        assert_eq!(names, expected);

        // Test nullish coalescing context collection
        let expr = Expr::try_from("@user.name ?? @person.name ?? 'default'").unwrap();
        let names = expr.requested_ctx();

        let mut expected = HashSet::new();
        expected.insert("user".to_string());
        expected.insert("person".to_string());

        assert_eq!(names, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_nil_handling() {
        expect("@nonexistent == nil", true);
        expect("@nonexistent.field == nil", true);
        expect("nil", None::<Val>);
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
    }

    #[test]
    #[cfg(feature = "json")]
    fn optional_chaining() {
        // Test basic optional chaining - should return "lk" when user exists
        expect("@req?.user?.name", "lk");
        
        // Test optional chaining with nil - should return nil when intermediate is nil
        let ctx: Val = json!({
            "req": null
        }).into();
        let expr = Expr::try_from("@req?.user?.name").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::Nil);

        // Test optional chaining mixed with regular access
        expect("@req?.user.name", "lk");
        
        // Test optional chaining where intermediate field doesn't exist - should return nil
        let ctx: Val = json!({
            "req": {}
        }).into();
        let expr = Expr::try_from("@req?.user?.name").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::Nil);
        
        // Test optional chaining on nested structures
        let ctx: Val = json!({
            "data": {
                "user": {
                    "profile": {
                        "email": "test@example.com"
                    }
                }
            }
        }).into();
        let expr = Expr::try_from("@data?.user?.profile?.email").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from("test@example.com"));
        
        // Test optional chaining where deeply nested field is nil
        let ctx: Val = json!({
            "data": {
                "user": {
                    "profile": null
                }
            }
        }).into();
        let expr = Expr::try_from("@data?.user?.profile?.email").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::Nil);

        // Test optional chaining with list access
        let ctx: Val = json!({
            "data": {
                "items": [
                    {"name": "first"},
                    {"name": "second"}
                ]
            }
        }).into();
        let expr = Expr::try_from("@data?.items?.0?.name").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from("first"));

        // Test optional chaining with expression evaluation mixed in
        let ctx: Val = json!({
            "data": {
                "user": {
                    "age": 25
                }
            }
        }).into();
        let expr = Expr::try_from("@data?.user?.age + 5").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from(30));
        
        // Test optional chaining in boolean expression
        let ctx: Val = json!({
            "data": {
                "user": {
                    "age": 25
                }
            }
        }).into();
        let expr = Expr::try_from("@data?.user?.age > 20").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from(true));

        // Test optional chaining where context root is nil
        let ctx: Val = json!(null).into();
        let expr = Expr::try_from("@data?.user?.name").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::Nil);

        // Optional chaining with bracket indexing on list
        let ctx: Val = json!({
            "data": {
                "items": [
                    {"name": "first"},
                    {"name": "second"}
                ]
            }
        }).into();
        let expr = Expr::try_from("@data?.items?[0]?.name").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from("first"));

        // Optional chaining with bracket indexing on map
        let ctx: Val = json!({ "user": {"name": "lk"} }).into();
        let expr = Expr::try_from("@user?[\"name\"]").unwrap();
        let result = expr.eval(&ctx).unwrap();
        assert_eq!(result, Val::from("lk"));
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
            "nested": {
                "level1": {
                    "level2": "value"
                }
            },
            "with.&=": true,
            "special-chars": "test-value",
            "123": "numeric-field"
        })
        .into();
        let expr = Expr::try_from(rule)?;
        expr.eval(&ctx)
    }

    #[cfg(feature = "json")]
    fn expect<V: Into<Val> + Clone>(rule: &str, val: V) {
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

    #[test]
    #[cfg(feature = "json")]
    fn range_expressions() {
        // Exclusive range
        expect("1..5", vec![1, 2, 3, 4]);

        // Inclusive range
        expect("1..=5", vec![1, 2, 3, 4, 5]);

        // Single element inclusive range
        expect("1..=1", vec![1]);

        // Empty exclusive range
        expect("5..5", Vec::<Val>::new());

        // Negative ranges
        expect("-3..=3", vec![-3, -2, -1, 0, 1, 2, 3]);
    }

    #[test]
    #[cfg(feature = "json")]
    fn closure_expressions() {
        // Test that closures parse and create closure values
        let res = with_ctx("|| 42");
        assert!(res.is_ok());
        let val = res.unwrap();
        match val {
            Val::Closure { params, body: _, .. } => {
                assert_eq!(params.len(), 0);
            }
            _ => panic!("Expected closure value"),
        }

        let res = with_ctx("|x| x + 1");
        assert!(res.is_ok());
        let val = res.unwrap();
        match val {
            Val::Closure { params, body: _, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
            }
            _ => panic!("Expected closure value"),
        }

        let res = with_ctx("|x, y| x * y");
        assert!(res.is_ok());
        let val = res.unwrap();
        match val {
            Val::Closure { params, body: _, .. } => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0], "x");
                assert_eq!(params[1], "y");
            }
            _ => panic!("Expected closure value"),
        }

        // Test closure call - this should work once the closure infrastructure is complete
        // For now, just test parsing
        let res = with_ctx("|| 5 + 3");
        assert!(res.is_ok()); // Should parse successfully
        let val = res.unwrap();
        match val {
            Val::Closure { params, .. } => {
                assert_eq!(params.len(), 0);
            }
            _ => panic!("Expected closure value"),
        }
    }

    #[test]
    #[cfg(feature = "json")]
    fn template_strings() {
        // Basic template string with no interpolation
        expect("`Hello, World!`", "Hello, World!");

        // Template string with simple variable interpolation using ${}
        expect("`Hello, ${@user.name}!`", "Hello, lk!");

        // Template string with multiple interpolations
        expect("`User ${@user.name} is ${@user.age} years old`", "User lk is 18 years old");

        // Template string with expressions
        expect("`Next year: ${@user.age + 1}`", "Next year: 19");

        // Template string with list access
        expect("`First item: ${@list.0}`", "First item: 1");

        // Template string with boolean expressions
        expect("`Is adult: ${@user.age >= 18}`", "Is adult: true");

        // Template string with arithmetic operations
        expect("`Sum: ${@list.0 + @list.1}`", "Sum: 3");

        // Template string with nested access
        expect("`Nested: ${@nested.level1.level2}`", "Nested: value");

        // Template string with special characters (escaped)
        expect("`Escaped: \\`backtick\\` and \\$dollar`", "Escaped: `backtick` and $dollar");

        // Template string with nil value
        expect("`Nil test: ${@nonexistent}`", "Nil test: nil");

        // Template string with complex expressions
        expect("`Calculation: ${(@user.age * 2) + 5}`", "Calculation: 41");

        // Empty template string
        expect("``", "");

        // Template string with only interpolation
        expect("`${@user.name}`", "lk");
    }

    #[test]
    #[cfg(feature = "json")]
    fn template_string_constant_folding() {
        // Test that template strings with constant expressions are folded
        let expr = Expr::try_from("`Hello \"World\"!`").unwrap();
        // Should fold to a single string constant during parsing
        if let Expr::Val(Val::Str(s)) = expr {
            assert_eq!(s.as_ref(), "Hello \"World\"!");
        } else {
            panic!("Template string with constants should be folded to Val");
        }

        // Test that template strings with variables are not folded
        let expr = Expr::try_from("`Hello ${@user.name}!`").unwrap();
        assert!(matches!(expr, Expr::TemplateString(_)));
    }

    #[test]
    #[cfg(feature = "json")]
    fn template_string_error_cases() {
        // Test unclosed template string
        panic("`Hello ${@user.name}");

        // Test invalid expression in template string
        panic("`Hello ${@user. + 1}!`");
    }

    #[test]
    #[cfg(feature = "json")]
    fn template_string_context_collection() {
        // Test that template strings correctly collect context requirements
        let expr = Expr::try_from("`Hello ${@user.name}, your items are ${@items.0} and ${@items.1}`").unwrap();
        let ctx_names = expr.requested_ctx();
        assert_eq!(ctx_names, HashSet::from(["user".to_string(), "items".to_string()]));
    }
}
