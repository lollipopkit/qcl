#[cfg(test)]
mod test {
    use crate::{expr::{BinOp, Expr, Parser}, token::{Token, Tokenizer}, val::Val};

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
                Val::Str("req".to_string()),
                Val::Str("user".to_string()),
                Val::Str("age".to_string()),
            ])),
            BinOp::Gt,
            Box::new(Expr::Int(18)),
        );
        let parsed = Parser::new(tokens).parse().unwrap();
        println!("{:?}", parsed);
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

        let t = Tokenizer::new(r).parse().unwrap();
        let parsed = Parser::new(t).parse().unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn and_or() {
        let r = r#"
        (
            @time != 0 
            ||
            @col.pub == true
        )
        &&
        @random > 0.5
        "#;

        let t = Tokenizer::new(r).parse().unwrap();
        let parsed = Parser::new(t).parse().unwrap();
        println!("{:?}", parsed);
    }
}
