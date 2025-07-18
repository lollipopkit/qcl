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

    #[test]
    fn basic() {
        let tokens = vec![
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("age".to_string()),
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
        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::Int(1),
            Val::Int(2),
            Val::Int(3),
        ])));
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
        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::Int(3),
            Val::Int(12),
        ])));
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
        let expected = Expr::Val(Val::List(Arc::new(vec![
            Val::Int(1),
            Val::Int(2),
            Val::Int(3),
        ])));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn empty_map() {
        let r = "{}";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Val(Val::Map(Arc::new(std::collections::HashMap::new())));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn simple_map() {
        let r = r#"{"name": "Alice", "age": 30}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = std::collections::HashMap::new();
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
        let mut expected_map = std::collections::HashMap::new();
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
        let mut expected_map = std::collections::HashMap::new();
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
        let mut inner_map = std::collections::HashMap::new();
        inner_map.insert("name".to_string(), Val::Str("Alice".into()));
        inner_map.insert("age".to_string(), Val::Int(30));
        let mut outer_map = std::collections::HashMap::new();
        outer_map.insert("user".to_string(), Val::Map(Arc::new(inner_map)));
        let expected = Expr::Val(Val::Map(Arc::new(outer_map)));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_trailing_comma() {
        let r = r#"{"a": 1, "b": 2,}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let mut expected_map = std::collections::HashMap::new();
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
        
        let mut alice_map = std::collections::HashMap::new();
        alice_map.insert("name".to_string(), Val::Str("Alice".into()));
        alice_map.insert("scores".to_string(), Val::List(Arc::new(vec![Val::Int(90), Val::Int(85)])));
        
        let mut bob_map = std::collections::HashMap::new();
        bob_map.insert("name".to_string(), Val::Str("Bob".into()));
        bob_map.insert("scores".to_string(), Val::List(Arc::new(vec![Val::Int(88), Val::Int(92)])));
        
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
    }
}
