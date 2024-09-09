#[cfg(test)]
mod test {
    use crate::{ast::Parser, expr::Expr, op::BinOp, token::{Token, Tokenizer}, val::Val};

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
                "req".into(),
                "user".into(),
                "age".into(),
            ])),
            BinOp::Gt,
            Box::new(Expr::Int(18)),
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
            Box::new(Expr::Bool(true)),
            Box::new(Expr::Bool(false)),
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
                    Box::new(Expr::At(vec!["time".into()])),
                    BinOp::Ne,
                    Box::new(Expr::Int(0)),
                )),
                Box::new(Expr::Bin(
                    Box::new(Expr::At(vec!["col".into(), "pub".into()])),
                    BinOp::Eq,
                    Box::new(Expr::Bool(true)),
                )),
            )))),
            Box::new(Expr::Bin(
                Box::new(Expr::At(vec!["random".into()])),
                BinOp::Gt,
                Box::new(Expr::Float(0.5)),
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
            "list".into(),
            Val::Int(0),
            "name".into(),
        ]);
        assert_eq!(parsed, expected);
    }
}