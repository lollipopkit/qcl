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
}
