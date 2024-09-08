#[cfg(test)]
mod tests {
    use crate::token::{Token, Tokenizer};

    #[test]
    #[should_panic]
    fn basic() {
        let t1 = Tokenizer::new(r#"1.3+*/@ %==  "str1" 'str2' true false nil "#);
        let e1 = vec![
            Token::Float(1.3),
            Token::Add,
            Token::Mul,
            Token::Div,
            Token::At,
            Token::Mod,
            Token::Eq,
            Token::Str("str1".to_string()),
            Token::Str("str2".to_string()),
            Token::Bool(true),
            Token::Bool(false),
            Token::Nil,
        ];
        assert_eq!(t1.unwrap(), e1);
    }

    #[test]
    fn punctuations() {
        let t2 = Tokenizer::new(">=<= && || == != ! > <");
        let e2 = vec![
            Token::Ge,
            Token::Le,
            Token::And,
            Token::Or,
            Token::Eq,
            Token::Ne,
            Token::Not,
            Token::Gt,
            Token::Lt,
        ];
        assert_eq!(t2.unwrap(), e2);
    }

    #[test]
    fn ids() {
        let t3 = Tokenizer::new("id1 id_2 id-3");
        let e3 = vec![
            Token::Id("id1".to_string()),
            Token::Id("id_2".to_string()),
            Token::Id("id-3".to_string()),
        ];
        assert_eq!(t3.unwrap(), e3);
    }

    #[test]
    fn unclosed_str() {
        let t = Tokenizer::new(r#""str"#);
        assert!(t.is_err());
    }

    #[test]
    fn num() {
        let t5 = Tokenizer::new("1.2.3");
        assert!(t5.is_err());

        let t5 = Tokenizer::new("-1.0 +1.2");
        let e5 = vec![Token::Float(-1.0), Token::Float(1.2)];
        assert_eq!(t5.unwrap(), e5);
    }

    #[test]
    fn keywords() {
        let t6 = Tokenizer::new(">true false nil in");
        let e6 = vec![
            Token::Gt,
            Token::Bool(true),
            Token::Bool(false),
            Token::Nil,
            Token::In,
        ];
        assert_eq!(t6.unwrap(), e6);
    }

    #[test]
    fn token_eq() {
        assert_eq!(Token::Str("a".to_string()), Token::Str("a".to_string()));
        assert_eq!(Token::Int(1), Token::Int(1));
        assert_eq!(Token::Float(1.0), Token::Float(1.0));
        assert_eq!(Token::Bool(true), Token::Bool(true));
        assert_eq!(Token::Nil, Token::Nil);
        assert_ne!(Token::Str("a".to_string()), Token::Str("b".to_string()));
        assert_ne!(Token::Int(1), Token::Int(2));
        assert_ne!(Token::Float(1.0), Token::Float(2.0));
        assert_ne!(Token::Bool(true), Token::Bool(false));
        assert_ne!(Token::Nil, Token::Bool(false));
    }

    #[test]
    fn at_query() {
        let t = Tokenizer::new("@req.user.age >= 18");
        let e = vec![
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("age".to_string()),
            Token::Ge,
            Token::Int(18),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn real_query() {
        let query = r#"
        (
            @req.user.id == @record.user.id && @record.time > 1700000
        ) 
        ||
        @req.user.role == 'admin'
        "#;
        let t = Tokenizer::new(query);
        let e = vec![
            Token::LParen,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::Eq,
            Token::At,
            Token::Id("record".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::And,
            Token::At,
            Token::Id("record".to_string()),
            Token::Dot,
            Token::Id("time".to_string()),
            Token::Gt,
            Token::Int(1700000),
            Token::RParen,
            Token::Or,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("role".to_string()),
            Token::Eq,
            Token::Str("admin".to_string()),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn list_access() {
        let t = Tokenizer::new("@list.0");
        let e = vec![
            Token::At,
            Token::Id("list".to_string()),
            Token::Dot,
            Token::Int(0),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn t1() {
        let t = Tokenizer::new("(@settings.active)");
        let e = vec![
            Token::At,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("age".to_string()),
            Token::Lt,
            Token::Int(18),
            Token::Or,
            Token::LParen,
            Token::At,
            Token::Id("settings".to_string()),
            Token::Dot,
            Token::Id("threshold".to_string()),
            Token::Gt,
            Token::Float(70.0),
            Token::And,
            Token::At,
            Token::Id("settings".to_string()),
            Token::Dot,
            Token::Id("active".to_string()),
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }
}
