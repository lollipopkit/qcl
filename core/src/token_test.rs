#[cfg(test)]
mod tests {
    use crate::token::{Token, Tokenizer};

    #[test]
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
    fn list_map_punctuations() {
        let t = Tokenizer::new("[]{}:,");
        let e = vec![
            Token::LBracket,
            Token::RBracket,
            Token::LBrace,
            Token::RBrace,
            Token::Colon,
            Token::Comma,
        ];
        assert_eq!(t.unwrap(), e);
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
        let t = Tokenizer::new("1.2.3");
        assert!(t.is_err());

        // Consider `.` as Dot if starts with `@`, otherwise Float
        // It's invalid in AST(The first path of At Expr must be Str), but valid in Tokenizer
        let t = Tokenizer::new("@1.2");
        assert!(t.is_ok());

        let t = Tokenizer::new("-1.0 +1.2");
        let e = vec![Token::Float(-1.0), Token::Float(1.2)];
        assert_eq!(t.unwrap(), e);
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
    fn test_return_keyword() {
        let tokens = Tokenizer::new("return").expect("Invalid tokens");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::Return);

        let tokens = Tokenizer::new("return 42;").expect("Invalid tokens");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], Token::Return);
        assert_eq!(tokens[1], Token::Int(42));
        assert_eq!(tokens[2], Token::Semicolon);
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

        let t = Tokenizer::new("@list.1.2");
        let e = vec![
            Token::At,
            Token::Id("list".to_string()),
            Token::Dot,
            Token::Int(1),
            Token::Dot,
            Token::Int(2),
        ];
        assert_eq!(t.unwrap(), e);
    }

    // Issue #1
    #[test]
    fn t1() {
        let t = Tokenizer::new("(@settings.active)");
        let e = vec![
            Token::LParen,
            Token::At,
            Token::Id("settings".to_string()),
            Token::Dot,
            Token::Id("active".to_string()),
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn empty_strings() {
        let t = Tokenizer::new(r#""""''"#);
        assert!(t.is_err());
    }

    #[test]
    fn complex_numbers() {
        let t = Tokenizer::new("-123 +456 -1.23 +4.56");
        let e = vec![
            Token::Int(-123),
            Token::Int(456),
            Token::Float(-1.23),
            Token::Float(4.56),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn invalid_numbers() {
        // Multiple dots in number
        assert!(Tokenizer::new("1.2.3").is_err());
        // Invalid float
        assert!(Tokenizer::new("1.a").is_err());
        // Just a dot
        let t = Tokenizer::new(".");
        assert_eq!(t.unwrap(), vec![Token::Dot]);
    }

    #[test]
    fn whitespace_handling() {
        let t = Tokenizer::new("  @req.user  .  id  ==  'test'  ");
        let e = vec![
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("user".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::Eq,
            Token::Str("test".to_string()),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn nested_expressions() {
        let t = Tokenizer::new("((@req.id == 123) && (@req.role == 'admin'))");
        let e = vec![
            Token::LParen,
            Token::LParen,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("id".to_string()),
            Token::Eq,
            Token::Int(123),
            Token::RParen,
            Token::And,
            Token::LParen,
            Token::At,
            Token::Id("req".to_string()),
            Token::Dot,
            Token::Id("role".to_string()),
            Token::Eq,
            Token::Str("admin".to_string()),
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn mixed_operators() {
        let t = Tokenizer::new("1 + 2 * 3 / 4 % 5");
        let e = vec![
            Token::Int(1),
            Token::Add,
            Token::Int(2),
            Token::Mul,
            Token::Int(3),
            Token::Div,
            Token::Int(4),
            Token::Mod,
            Token::Int(5),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn complex_path_access() {
        let t = Tokenizer::new("@users.0.name @items.1.tags.2");
        let e = vec![
            Token::At,
            Token::Id("users".to_string()),
            Token::Dot,
            Token::Int(0),
            Token::Dot,
            Token::Id("name".to_string()),
            Token::At,
            Token::Id("items".to_string()),
            Token::Dot,
            Token::Int(1),
            Token::Dot,
            Token::Id("tags".to_string()),
            Token::Dot,
            Token::Int(2),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn logic_operations() {
        let t = Tokenizer::new("!(@a in @b) && (@c || !@d)");
        let e = vec![
            Token::Not,
            Token::LParen,
            Token::At,
            Token::Id("a".to_string()),
            Token::In,
            Token::At,
            Token::Id("b".to_string()),
            Token::RParen,
            Token::And,
            Token::LParen,
            Token::At,
            Token::Id("c".to_string()),
            Token::Or,
            Token::Not,
            Token::At,
            Token::Id("d".to_string()),
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn nested_at() {
        let t = Tokenizer::new("@a.(@b.(@c))");
        let e = vec![
            Token::At,
            Token::Id("a".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("b".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("c".to_string()),
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@a.(@b.(@c.(@d)))");
        let e = vec![
            Token::At,
            Token::Id("a".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("b".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("c".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("d".to_string()),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@a.(@b - 1))");
        let e = vec![
            Token::At,
            Token::Id("a".to_string()),
            Token::Dot,
            Token::LParen,
            Token::At,
            Token::Id("b".to_string()),
            Token::Sub,
            Token::Int(1),
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn list_literals() {
        let t = Tokenizer::new("[1, 2, 3]");
        let e = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Comma,
            Token::Int(3),
            Token::RBracket,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"["hello", "world"]"#);
        let e = vec![
            Token::LBracket,
            Token::Str("hello".to_string()),
            Token::Comma,
            Token::Str("world".to_string()),
            Token::RBracket,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("[]");
        let e = vec![Token::LBracket, Token::RBracket];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn map_literals() {
        let t = Tokenizer::new(r#"{"key": "value"}"#);
        let e = vec![
            Token::LBrace,
            Token::Str("key".to_string()),
            Token::Colon,
            Token::Str("value".to_string()),
            Token::RBrace,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"{"a": 1, "b": 2}"#);
        let e = vec![
            Token::LBrace,
            Token::Str("a".to_string()),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            Token::Str("b".to_string()),
            Token::Colon,
            Token::Int(2),
            Token::RBrace,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("{}");
        let e = vec![Token::LBrace, Token::RBrace];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn complex_list_map() {
        let t = Tokenizer::new(r#"[{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]"#);
        let e = vec![
            Token::LBracket,
            Token::LBrace,
            Token::Str("name".to_string()),
            Token::Colon,
            Token::Str("Alice".to_string()),
            Token::Comma,
            Token::Str("age".to_string()),
            Token::Colon,
            Token::Int(30),
            Token::RBrace,
            Token::Comma,
            Token::LBrace,
            Token::Str("name".to_string()),
            Token::Colon,
            Token::Str("Bob".to_string()),
            Token::Comma,
            Token::Str("age".to_string()),
            Token::Colon,
            Token::Int(25),
            Token::RBrace,
            Token::RBracket,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"{"users": [1, 2, 3], "active": true}"#);
        let e = vec![
            Token::LBrace,
            Token::Str("users".to_string()),
            Token::Colon,
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Comma,
            Token::Int(3),
            Token::RBracket,
            Token::Comma,
            Token::Str("active".to_string()),
            Token::Colon,
            Token::Bool(true),
            Token::RBrace,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn trailing_commas() {
        let t = Tokenizer::new("[1, 2, 3,]");
        let e = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Comma,
            Token::Int(3),
            Token::Comma,
            Token::RBracket,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"{"a": 1, "b": 2,}"#);
        let e = vec![
            Token::LBrace,
            Token::Str("a".to_string()),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            Token::Str("b".to_string()),
            Token::Colon,
            Token::Int(2),
            Token::Comma,
            Token::RBrace,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn test_comment() {
        let t = Tokenizer::new("123 // 这是一个注释\n456");
        let e = vec![Token::Int(123), Token::Int(456)];
        assert_eq!(t.unwrap(), e);
    }
}
