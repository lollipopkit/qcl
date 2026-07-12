#[cfg(test)]
mod test {
    use crate::{
        ast::Parser,
        expr::Expr,
        op::BinOp,
        token::{Token, Tokenizer},
        val::Val,
    };
    use std::sync::Arc;

    fn id(value: &str) -> Token {
        Token::Id(value.into())
    }

    #[test]
    fn basic() {
        let tokens = vec![
            Token::At,
            id("req"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("age"),
            Token::Gt,
            Token::Int(18),
        ];
        let expr = Expr::Bin(
            Box::new(Expr::At(vec![
                Box::new(Expr::Val("req".into())),
                Box::new(Expr::Val("user".into())),
                Box::new(Expr::Val("age".into())),
            ])),
            BinOp::Gt,
            Box::new(Expr::Val(18.into())),
        );
        let parsed = Parser::new(&tokens).parse().unwrap();
        assert_eq!(parsed, expr);
    }

    #[test]
    fn parser_reuses_token_string_allocations() {
        let tokens = Tokenizer::new(r#"@req.user == "admin""#).unwrap();

        let req_token = match &tokens[1] {
            Token::Id(value) => Arc::clone(value),
            other => panic!("unexpected token: {other:?}"),
        };
        let user_token = match &tokens[3] {
            Token::Id(value) => Arc::clone(value),
            other => panic!("unexpected token: {other:?}"),
        };
        let admin_token = match &tokens[5] {
            Token::Str(value) => Arc::clone(value),
            other => panic!("unexpected token: {other:?}"),
        };

        let parsed = Parser::new(&tokens).parse().unwrap();

        let Expr::Bin(left, BinOp::Eq, right) = parsed else {
            panic!("unexpected expr shape");
        };

        let Expr::At(paths) = *left else {
            panic!("unexpected left expr");
        };
        assert_eq!(paths.len(), 2);

        match &*paths[0] {
            Expr::Val(Val::Str(value)) => assert!(Arc::ptr_eq(value, &req_token)),
            other => panic!("unexpected req path: {other:?}"),
        }

        match &*paths[1] {
            Expr::Val(Val::Str(value)) => assert!(Arc::ptr_eq(value, &user_token)),
            other => panic!("unexpected user path: {other:?}"),
        }

        match *right {
            Expr::Val(Val::Str(value)) => assert!(Arc::ptr_eq(&value, &admin_token)),
            other => panic!("unexpected right expr: {other:?}"),
        }
    }

    #[test]
    fn paren() {
        let r = r#"
        (
            true
            ||
            false
        )
        "#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Paren(Box::new(Expr::Val(Val::Bool(true))));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn complex() {
        let r = r#"
        (
            @time != 0 
            ||
            @col.pub == true
        )
        &&
        @random > 0.5
        "#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::And(
            Box::new(Expr::Paren(Box::new(Expr::Or(
                Box::new(Expr::Bin(
                    Box::new(Expr::At(vec![Box::new(Expr::Val("time".into()))])),
                    BinOp::Ne,
                    Box::new(Expr::Val(0.into())),
                )),
                Box::new(Expr::Bin(
                    Box::new(Expr::At(vec![
                        Box::new(Expr::Val("col".into())),
                        Box::new(Expr::Val("pub".into())),
                    ])),
                    BinOp::Eq,
                    Box::new(Expr::Val(true.into())),
                )),
            )))),
            Box::new(Expr::Bin(
                Box::new(Expr::At(vec![Box::new(Expr::Val("random".into()))])),
                BinOp::Gt,
                Box::new(Expr::Val(0.5.into())),
            )),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn access_str_int_str() {
        let r = "@list.0.name";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("list".into())),
            Box::new(Expr::Val(0.into())),
            Box::new(Expr::Val("name".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn access_first_int_paths() {
        let r = "@1.2";

        let t = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&t).parse();
        assert!(parsed.is_err());
    }

    #[test]
    fn empty_list() {
        let r = "[]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn simple_list() {
        let r = "[1, 2, 3]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![Val::Int(1), Val::Int(2), Val::Int(3)])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_list() {
        let r = r#"[1, "hello", true]"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::Int(1),
            Val::Str("hello".into()),
            Val::Bool(true),
        ])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn list_with_expressions() {
        let r = "[1 + 2, 3 * 4]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![Val::Int(3), Val::Int(12)])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn nested_list() {
        let r = "[[1, 2], [3, 4]]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::List(Arc::new(vec![Val::Int(1), Val::Int(2)])),
            Val::List(Arc::new(vec![Val::Int(3), Val::Int(4)])),
        ])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn list_with_trailing_comma() {
        let r = "[1, 2, 3,]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![Val::Int(1), Val::Int(2), Val::Int(3)])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn empty_map() {
        let r = "{}";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::Map(Arc::new(hashbrown::HashMap::new())));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn simple_map() {
        let r = r#"{"name": "Alice", "age": 30}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = hashbrown::HashMap::new();
        expected_map.insert("name".to_string(), Val::Str("Alice".into()));
        expected_map.insert("age".to_string(), Val::Int(30));
        let expected = Expr::Val(Val::Map(Arc::new(expected_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_expressions() {
        let r = r#"{"sum": 1 + 2, "product": 3 * 4}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = hashbrown::HashMap::new();
        expected_map.insert("sum".to_string(), Val::Int(3));
        expected_map.insert("product".to_string(), Val::Int(12));
        let expected = Expr::Val(Val::Map(Arc::new(expected_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_different_key_types() {
        let r = r#"{42: "number", true: "bool", "key": "string"}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = hashbrown::HashMap::new();
        expected_map.insert("42".to_string(), Val::Str("number".into()));
        expected_map.insert("true".to_string(), Val::Str("bool".into()));
        expected_map.insert("key".to_string(), Val::Str("string".into()));
        let expected = Expr::Val(Val::Map(Arc::new(expected_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn nested_map() {
        let r = r#"{"user": {"name": "Alice", "age": 30}}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut inner_map = hashbrown::HashMap::new();
        inner_map.insert("name".to_string(), Val::Str("Alice".into()));
        inner_map.insert("age".to_string(), Val::Int(30));
        let mut outer_map = hashbrown::HashMap::new();
        outer_map.insert("user".to_string(), Val::Map(Arc::new(inner_map)));
        let expected = Expr::Val(Val::Map(Arc::new(outer_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_trailing_comma() {
        let r = r#"{"a": 1, "b": 2,}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = hashbrown::HashMap::new();
        expected_map.insert("a".to_string(), Val::Int(1));
        expected_map.insert("b".to_string(), Val::Int(2));
        let expected = Expr::Val(Val::Map(Arc::new(expected_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_structures() {
        let r = r#"[{"name": "Alice", "scores": [90, 85]}, {"name": "Bob", "scores": [88, 92]}]"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();

        let mut alice_map = hashbrown::HashMap::new();
        alice_map.insert("name".to_string(), Val::Str("Alice".into()));
        alice_map.insert(
            "scores".to_string(),
            Val::List(Arc::new(vec![Val::Int(90), Val::Int(85)])),
        );

        let mut bob_map = hashbrown::HashMap::new();
        bob_map.insert("name".to_string(), Val::Str("Bob".into()));
        bob_map.insert(
            "scores".to_string(),
            Val::List(Arc::new(vec![Val::Int(88), Val::Int(92)])),
        );

        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::Map(Arc::new(alice_map)),
            Val::Map(Arc::new(bob_map)),
        ])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn context_access_in_literals() {
        let r = r#"[@user.name, @user.age]"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::At(vec![
                Box::new(Expr::Val("user".into())),
                Box::new(Expr::Val("name".into())),
            ])),
            Box::new(Expr::At(vec![
                Box::new(Expr::Val("user".into())),
                Box::new(Expr::Val("age".into())),
            ])),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn invalid_list_syntax() {
        // Missing closing bracket
        let r = "[1, 2, 3";
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse();
        assert!(parsed.is_err());

        // Invalid separator
        let r = "[1; 2; 3]";
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse();
        assert!(parsed.is_err());

        // Missing element after comma
        let r = "[1,]";
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::List(Arc::new(vec![Val::Int(1)])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn invalid_map_syntax() {
        // Missing closing brace
        let r = r#"{"key": "value""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse();
        assert!(parsed.is_err());

        // Missing colon
        let r = r#"{"key" "value"}"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse();
        assert!(parsed.is_err());

        // Missing value
        let r = r#"{"key":}"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse();
        assert!(parsed.is_err());

        // Missing key after comma
        let r = r#"{"key": 1, }"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = hashbrown::HashMap::new();
        expected_map.insert("key".to_string(), Val::Int(1));
        let expected = Expr::Val(Val::Map(Arc::new(expected_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn dangling_operators_return_errors() {
        assert!(Expr::try_from("!").is_err());
        assert!(Expr::try_from("1 +").is_err());
        assert!(Expr::try_from("@a.").is_err());
    }

    #[test]
    fn deeply_nested_parentheses_are_rejected() {
        let mut expr = String::new();
        for _ in 0..300 {
            expr.push('(');
        }
        expr.push_str("true");
        for _ in 0..300 {
            expr.push(')');
        }

        assert!(Expr::try_from(expr.as_str()).is_err());
    }

    #[test]
    fn deeply_nested_dynamic_access_is_rejected() {
        let mut expr = "@a".to_string();
        for _ in 0..300 {
            expr = format!("@a.({expr})");
        }

        assert!(Expr::try_from(expr.as_str()).is_err());
    }

    #[test]
    fn overly_long_context_access_path_is_rejected() {
        let expr = format!("@a.{}", vec!["a"; 300].join("."));
        assert!(Expr::try_from(expr.as_str()).is_err());
    }

    #[test]
    fn deeply_nested_postfix_access_is_rejected() {
        // `nil.a.a.a...` builds a left-leaning Access spine; without a depth
        // bound this overflowed the stack in fold_constants/eval/Drop.
        let mut expr = "nil".to_string();
        for _ in 0..5000 {
            expr.push_str(".a");
        }
        let result = Expr::try_from(expr.as_str());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("too deep"));
    }

    #[test]
    fn deeply_multiplied_paren_chain_is_rejected() {
        // Parens (bounded) times binary chains (bounded) previously composed
        // into an unbounded AST spine that overflowed the stack.
        let mut expr = "1".to_string();
        for _ in 0..200 {
            expr = format!("({expr})");
            expr.push_str(&"*1".repeat(200));
        }
        let result = Expr::try_from(expr.as_str());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("too deep"));
    }

    #[test]
    fn long_binary_chain_is_rejected() {
        // A single very long operator chain builds a deep left-leaning tree.
        let expr = format!("1{}", "+1".repeat(5000));
        assert!(Expr::try_from(expr.as_str()).is_err());
    }

    #[test]
    fn nesting_at_exactly_max_depth_parses() {
        use crate::ast::MAX_PARSE_DEPTH;
        // `MAX_PARSE_DEPTH - 1` nested parens drive the depth guard to exactly
        // MAX_PARSE_DEPTH and must still parse; one more level is rejected.
        let boundary = MAX_PARSE_DEPTH - 1;
        let ok = format!("{}1{}", "(".repeat(boundary), ")".repeat(boundary));
        assert!(Expr::try_from(ok.as_str()).is_ok());

        let too_deep = format!("{}1{}", "(".repeat(MAX_PARSE_DEPTH), ")".repeat(MAX_PARSE_DEPTH));
        let result = Expr::try_from(too_deep.as_str());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("too deep"));
    }

    #[test]
    fn deeply_nested_ternary_is_rejected() {
        // Build: true ? true : true ? true : ... (chain of 300 ternaries)
        let mut expr = "true".to_string();
        for _ in 0..300 {
            expr = format!("true ? true : {expr}");
        }
        let result = Expr::try_from(expr.as_str());
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("nesting is too deep"),
            "expected nesting error, got: {err_msg}"
        );
    }

    #[test]
    fn quoted_field_access_simple() {
        // Basic quoted field access
        let r = r#"@"with.&=""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![Box::new(Expr::Val("with.&=".into()))]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_access_nested() {
        // Nested quoted field access
        let r = r#"@req."user"."name""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("req".into())),
            Box::new(Expr::Val("user".into())),
            Box::new(Expr::Val("name".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_quoted_unquoted_access() {
        // Mix of quoted and unquoted field access
        let r = r#"@req.user."special-field".data"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("req".into())),
            Box::new(Expr::Val("user".into())),
            Box::new(Expr::Val("special-field".into())),
            Box::new(Expr::Val("data".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_with_special_chars() {
        // Field name with various special characters
        let r = r#"@data."field-with@special#chars$""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("field-with@special#chars$".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_numeric_mixed() {
        // Mix of quoted fields, numeric indices, and regular fields
        let r = r#"@files.0."name".value"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("files".into())),
            Box::new(Expr::Val(0.into())),
            Box::new(Expr::Val("name".into())),
            Box::new(Expr::Val("value".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_in_expression() {
        // Quoted field access in comparison expression
        let r = r#"@config."debug-mode" == true"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Bin(
            Box::new(Expr::At(vec![
                Box::new(Expr::Val("config".into())),
                Box::new(Expr::Val("debug-mode".into())),
            ])),
            BinOp::Eq,
            Box::new(Expr::Val(true.into())),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_with_spaces() {
        // Field name with spaces
        let r = r#"@data."field with spaces""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("field with spaces".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_with_quotes_inside() {
        // Field name with single quotes inside double quotes
        let r = r#"@data."field's name""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("field's name".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_with_escaped_double_quote() {
        let r = r#"@data."field\"name""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("field\"name".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn quoted_field_with_escaped_backslash() {
        let r = r#"@data."path\\segment""#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("path\\segment".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn single_quoted_field_access() {
        // Using single quotes instead of double quotes
        let r = r#"@data.'special-field'"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("special-field".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn single_quoted_field_with_escape() {
        let r = r#"@data.'field\'name'"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::At(vec![
            Box::new(Expr::Val("data".into())),
            Box::new(Expr::Val("field'name".into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn complex_quoted_field_expression() {
        // Complex expression with multiple quoted fields
        let r = r#"@req."user-data"."is-active" && @config."debug-enabled" == false"#;
        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::And(
            Box::new(Expr::At(vec![
                Box::new(Expr::Val("req".into())),
                Box::new(Expr::Val("user-data".into())),
                Box::new(Expr::Val("is-active".into())),
            ])),
            Box::new(Expr::Bin(
                Box::new(Expr::At(vec![
                    Box::new(Expr::Val("config".into())),
                    Box::new(Expr::Val("debug-enabled".into())),
                ])),
                BinOp::Eq,
                Box::new(Expr::Val(false.into())),
            )),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn ternary_parse() {
        // condition ? true_expr : false_expr
        let ts = Tokenizer::new("@a ? 1 : 2").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Ternary(
            Box::new(Expr::At(vec![Box::new(Expr::Val("a".into()))])),
            Box::new(Expr::Val(1.into())),
            Box::new(Expr::Val(2.into())),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn ternary_precedence_below_or() {
        // Ternary condition can contain ||
        let ts = Tokenizer::new("@a || @b ? 1 : 2").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Ternary(
            Box::new(Expr::Or(
                Box::new(Expr::At(vec![Box::new(Expr::Val("a".into()))])),
                Box::new(Expr::At(vec![Box::new(Expr::Val("b".into()))])),
            )),
            Box::new(Expr::Val(1.into())),
            Box::new(Expr::Val(2.into())),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn coalesce_parse() {
        let ts = Tokenizer::new("@a ?? 42").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Coalesce(
            Box::new(Expr::At(vec![Box::new(Expr::Val("a".into()))])),
            Box::new(Expr::Val(42.into())),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn coalesce_chained() {
        // Left-associative: a ?? b ?? c => (a ?? b) ?? c
        let ts = Tokenizer::new("@a ?? @b ?? 0").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Coalesce(
            Box::new(Expr::Coalesce(
                Box::new(Expr::At(vec![Box::new(Expr::Val("a".into()))])),
                Box::new(Expr::At(vec![Box::new(Expr::Val("b".into()))])),
            )),
            Box::new(Expr::Val(0.into())),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn coalesce_precedence_with_or() {
        // ?? and || are at the same precedence level (coalesce sits between or and ternary)
        // @a || @b ?? @c  ==>  (@a || @b) ?? @c
        let ts = Tokenizer::new("@a || @b ?? @c").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Coalesce(
            Box::new(Expr::Or(
                Box::new(Expr::At(vec![Box::new(Expr::Val("a".into()))])),
                Box::new(Expr::At(vec![Box::new(Expr::Val("b".into()))])),
            )),
            Box::new(Expr::At(vec![Box::new(Expr::Val("c".into()))])),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn hex_octal_tokens_parse_as_int() {
        // 0xFF + 0o77 = 255 + 63 = 318, constant-folded at parse time
        let ts = Tokenizer::new("0xFF + 0o77").unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(318.into());
        assert_eq!(parsed, expected);
    }
}
