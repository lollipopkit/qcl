#[cfg(test)]
mod test {
    use crate::{ast::Parser, expr::{BinOp, Expr}, token::{Token, Tokenizer}, val::Val};

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
        let parsed = Parser::new(&tokens).parse().unwrap();
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

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
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

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn access_str_int_str() {
        let r = "@list.0.name";

        let ts = Tokenizer::new(r).unwrap();
        let parsed = Parser::new(&ts).parse().unwrap();
        println!("{:?}", parsed);
    }
}