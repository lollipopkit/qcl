#[cfg(test)]
mod test {
    use crate::{
        ast::Parser,
        expr::Expr,
        op::BinOp,
        token::{Token, Tokenizer},
    };

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
        let expected = Expr::Paren(Box::new(Expr::Or(
            Box::new(Expr::Val(true.into())),
            Box::new(Expr::Val(false.into())),
        )));
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
        let expected = Expr::List(vec![]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn simple_list() {
        let r = "[1, 2, 3]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::Val(1.into())),
            Box::new(Expr::Val(2.into())),
            Box::new(Expr::Val(3.into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_list() {
        let r = r#"[1, "hello", true]"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::Val(1.into())),
            Box::new(Expr::Val("hello".into())),
            Box::new(Expr::Val(true.into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn list_with_expressions() {
        let r = "[1 + 2, 3 * 4]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::Bin(
                Box::new(Expr::Val(1.into())),
                BinOp::Add,
                Box::new(Expr::Val(2.into())),
            )),
            Box::new(Expr::Bin(
                Box::new(Expr::Val(3.into())),
                BinOp::Mul,
                Box::new(Expr::Val(4.into())),
            )),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn nested_list() {
        let r = "[[1, 2], [3, 4]]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(1.into())),
                Box::new(Expr::Val(2.into())),
            ])),
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(3.into())),
                Box::new(Expr::Val(4.into())),
            ])),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn list_with_trailing_comma() {
        let r = "[1, 2, 3,]";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::Val(1.into())),
            Box::new(Expr::Val(2.into())),
            Box::new(Expr::Val(3.into())),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn empty_map() {
        let r = "{}";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn simple_map() {
        let r = r#"{"name": "Alice", "age": 30}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![
            (
                Box::new(Expr::Val("name".into())),
                Box::new(Expr::Val("Alice".into())),
            ),
            (
                Box::new(Expr::Val("age".into())),
                Box::new(Expr::Val(30.into())),
            ),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_expressions() {
        let r = r#"{"sum": 1 + 2, "product": 3 * 4}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![
            (
                Box::new(Expr::Val("sum".into())),
                Box::new(Expr::Bin(
                    Box::new(Expr::Val(1.into())),
                    BinOp::Add,
                    Box::new(Expr::Val(2.into())),
                )),
            ),
            (
                Box::new(Expr::Val("product".into())),
                Box::new(Expr::Bin(
                    Box::new(Expr::Val(3.into())),
                    BinOp::Mul,
                    Box::new(Expr::Val(4.into())),
                )),
            ),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_different_key_types() {
        let r = r#"{42: "number", true: "bool", "key": "string"}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![
            (
                Box::new(Expr::Val(42.into())),
                Box::new(Expr::Val("number".into())),
            ),
            (
                Box::new(Expr::Val(true.into())),
                Box::new(Expr::Val("bool".into())),
            ),
            (
                Box::new(Expr::Val("key".into())),
                Box::new(Expr::Val("string".into())),
            ),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn nested_map() {
        let r = r#"{"user": {"name": "Alice", "age": 30}}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![(
            Box::new(Expr::Val("user".into())),
            Box::new(Expr::Map(vec![
                (
                    Box::new(Expr::Val("name".into())),
                    Box::new(Expr::Val("Alice".into())),
                ),
                (
                    Box::new(Expr::Val("age".into())),
                    Box::new(Expr::Val(30.into())),
                ),
            ])),
        )]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn map_with_trailing_comma() {
        let r = r#"{"a": 1, "b": 2,}"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::Map(vec![
            (
                Box::new(Expr::Val("a".into())),
                Box::new(Expr::Val(1.into())),
            ),
            (
                Box::new(Expr::Val("b".into())),
                Box::new(Expr::Val(2.into())),
            ),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_structures() {
        let r = r#"[{"name": "Alice", "scores": [90, 85]}, {"name": "Bob", "scores": [88, 92]}]"#;

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        let expected = Expr::List(vec![
            Box::new(Expr::Map(vec![
                (
                    Box::new(Expr::Val("name".into())),
                    Box::new(Expr::Val("Alice".into())),
                ),
                (
                    Box::new(Expr::Val("scores".into())),
                    Box::new(Expr::List(vec![
                        Box::new(Expr::Val(90.into())),
                        Box::new(Expr::Val(85.into())),
                    ])),
                ),
            ])),
            Box::new(Expr::Map(vec![
                (
                    Box::new(Expr::Val("name".into())),
                    Box::new(Expr::Val("Bob".into())),
                ),
                (
                    Box::new(Expr::Val("scores".into())),
                    Box::new(Expr::List(vec![
                        Box::new(Expr::Val(88.into())),
                        Box::new(Expr::Val(92.into())),
                    ])),
                ),
            ])),
        ]);
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
