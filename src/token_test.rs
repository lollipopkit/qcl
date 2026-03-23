#[cfg(test)]
mod tests {
    use crate::token::{Token, Tokenizer};

    fn id(value: &str) -> Token {
        Token::Id(value.into())
    }

    fn str_token(value: &str) -> Token {
        Token::Str(value.into())
    }

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
            str_token("str1"),
            str_token("str2"),
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
        let e3 = vec![id("id1"), id("id_2"), id("id-3")];
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
        let e = vec![Token::Float(-1.0), Token::Add, Token::Float(1.2)];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn keywords() {
        let t6 = Tokenizer::new(">true false nil in");
        let e6 = vec![Token::Gt, Token::Bool(true), Token::Bool(false), Token::Nil, Token::In];
        assert_eq!(t6.unwrap(), e6);
    }

    #[test]
    fn token_eq() {
        assert_eq!(str_token("a"), str_token("a"));
        assert_eq!(Token::Int(1), Token::Int(1));
        assert_eq!(Token::Float(1.0), Token::Float(1.0));
        assert_eq!(Token::Bool(true), Token::Bool(true));
        assert_eq!(Token::Nil, Token::Nil);
        assert_ne!(str_token("a"), str_token("b"));
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
            id("req"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("age"),
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
            id("req"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("id"),
            Token::Eq,
            Token::At,
            id("record"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("id"),
            Token::And,
            Token::At,
            id("record"),
            Token::Dot,
            id("time"),
            Token::Gt,
            Token::Int(1700000),
            Token::RParen,
            Token::Or,
            Token::At,
            id("req"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("role"),
            Token::Eq,
            str_token("admin"),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn list_access() {
        let t = Tokenizer::new("@list.0");
        let e = vec![Token::At, id("list"), Token::Dot, Token::Int(0)];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@list.1.2");
        let e = vec![
            Token::At,
            id("list"),
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
            id("settings"),
            Token::Dot,
            id("active"),
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
            Token::Add,
            Token::Int(456),
            Token::Sub,
            Token::Float(1.23),
            Token::Add,
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
    fn invalid_identifier_start_chars() {
        assert!(Tokenizer::new("^").is_err());
        assert!(Tokenizer::new("$").is_err());
        assert!(Tokenizer::new("~").is_err());
        assert!(Tokenizer::new("@a.^").is_err());
    }

    #[test]
    fn keyword_prefixes_are_plain_identifiers() {
        let t = Tokenizer::new("index truex nilx falsey in1");
        let e = vec![id("index"), id("truex"), id("nilx"), id("falsey"), id("in1")];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn keyword_spelling_in_at_paths_stays_identifier() {
        let t = Tokenizer::new("@true.in.@nil.false");
        let e = vec![
            Token::At,
            id("true"),
            Token::Dot,
            id("in"),
            Token::Dot,
            Token::At,
            id("nil"),
            Token::Dot,
            id("false"),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn strings_support_escapes() {
        let t = Tokenizer::new(r#""a\"b" 'c\'d' "e\\f" "line\nbreak""#);
        let e = vec![
            str_token("a\"b"),
            str_token("c'd"),
            str_token("e\\f"),
            str_token("line\nbreak"),
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn invalid_string_escapes_error() {
        assert!(Tokenizer::new(r#""\x""#).is_err());
        assert!(Tokenizer::new("\"dangling\\\\").is_err());
    }

    #[test]
    fn whitespace_handling() {
        let t = Tokenizer::new("  @req.user  .  id  ==  'test'  ");
        let e = vec![
            Token::At,
            id("req"),
            Token::Dot,
            id("user"),
            Token::Dot,
            id("id"),
            Token::Eq,
            str_token("test"),
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
            id("req"),
            Token::Dot,
            id("id"),
            Token::Eq,
            Token::Int(123),
            Token::RParen,
            Token::And,
            Token::LParen,
            Token::At,
            id("req"),
            Token::Dot,
            id("role"),
            Token::Eq,
            str_token("admin"),
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
            id("users"),
            Token::Dot,
            Token::Int(0),
            Token::Dot,
            id("name"),
            Token::At,
            id("items"),
            Token::Dot,
            Token::Int(1),
            Token::Dot,
            id("tags"),
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
            id("a"),
            Token::In,
            Token::At,
            id("b"),
            Token::RParen,
            Token::And,
            Token::LParen,
            Token::At,
            id("c"),
            Token::Or,
            Token::Not,
            Token::At,
            id("d"),
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn nested_at() {
        let t = Tokenizer::new("@a.(@b.(@c))");
        let e = vec![
            Token::At,
            id("a"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("b"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("c"),
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@a.(@b.(@c.(@d)))");
        let e = vec![
            Token::At,
            id("a"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("b"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("c"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("d"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@a.(@b - 1))");
        let e = vec![
            Token::At,
            id("a"),
            Token::Dot,
            Token::LParen,
            Token::At,
            id("b"),
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
            str_token("hello"),
            Token::Comma,
            str_token("world"),
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
            str_token("key"),
            Token::Colon,
            str_token("value"),
            Token::RBrace,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"{"a": 1, "b": 2}"#);
        let e = vec![
            Token::LBrace,
            str_token("a"),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            str_token("b"),
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
            str_token("name"),
            Token::Colon,
            str_token("Alice"),
            Token::Comma,
            str_token("age"),
            Token::Colon,
            Token::Int(30),
            Token::RBrace,
            Token::Comma,
            Token::LBrace,
            str_token("name"),
            Token::Colon,
            str_token("Bob"),
            Token::Comma,
            str_token("age"),
            Token::Colon,
            Token::Int(25),
            Token::RBrace,
            Token::RBracket,
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new(r#"{"users": [1, 2, 3], "active": true}"#);
        let e = vec![
            Token::LBrace,
            str_token("users"),
            Token::Colon,
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Comma,
            Token::Int(3),
            Token::RBracket,
            Token::Comma,
            str_token("active"),
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
            str_token("a"),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            str_token("b"),
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

    #[test]
    fn test_block_comment() {
        let t = Tokenizer::new("1 /* comment */ + 2");
        let e = vec![Token::Int(1), Token::Add, Token::Int(2)];
        assert_eq!(t.unwrap(), e);

        // Nested block comments
        let t = Tokenizer::new("1 /* outer /* inner */ still comment */ + 2");
        let e = vec![Token::Int(1), Token::Add, Token::Int(2)];
        assert_eq!(t.unwrap(), e);

        // Unterminated
        assert!(Tokenizer::new("1 /* oops").is_err());

        // Empty block comment
        let t = Tokenizer::new("1 /**/ + 2");
        let e = vec![Token::Int(1), Token::Add, Token::Int(2)];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn test_question_tokens() {
        let t = Tokenizer::new("1 ? 2 : 3");
        let e = vec![
            Token::Int(1),
            Token::Question,
            Token::Int(2),
            Token::Colon,
            Token::Int(3),
        ];
        assert_eq!(t.unwrap(), e);

        let t = Tokenizer::new("@x ?? 0");
        let e = vec![Token::At, id("x"), Token::QuestionQuestion, Token::Int(0)];
        assert_eq!(t.unwrap(), e);
    }

    #[test]
    fn test_hex_literals() {
        let t = Tokenizer::new("0xFF");
        assert_eq!(t.unwrap(), vec![Token::Int(255)]);

        let t = Tokenizer::new("0x0");
        assert_eq!(t.unwrap(), vec![Token::Int(0)]);

        let t = Tokenizer::new("0xDEAD");
        assert_eq!(t.unwrap(), vec![Token::Int(0xDEAD)]);

        // Case insensitive prefix
        let t = Tokenizer::new("0XAB");
        assert_eq!(t.unwrap(), vec![Token::Int(0xAB)]);

        // Invalid: no digits after 0x
        assert!(Tokenizer::new("0x").is_err());
    }

    #[test]
    fn test_octal_literals() {
        let t = Tokenizer::new("0o77");
        assert_eq!(t.unwrap(), vec![Token::Int(63)]);

        let t = Tokenizer::new("0o0");
        assert_eq!(t.unwrap(), vec![Token::Int(0)]);

        let t = Tokenizer::new("0O10");
        assert_eq!(t.unwrap(), vec![Token::Int(8)]);

        // Invalid: no digits after 0o
        assert!(Tokenizer::new("0o").is_err());
    }

    #[test]
    fn test_unicode_escape() {
        // Basic ASCII
        let t = Tokenizer::new(r#""\u0041""#);
        assert_eq!(t.unwrap(), vec![str_token("A")]);

        // Chinese character
        let t = Tokenizer::new(r#""\u4F60""#);
        assert_eq!(t.unwrap(), vec![str_token("你")]);

        // Emoji-range (BMP)
        let t = Tokenizer::new(r#""\u2764""#);
        assert_eq!(t.unwrap(), vec![str_token("❤")]);

        // Mixed with regular chars
        let t = Tokenizer::new(r#""hello\u0020world""#);
        assert_eq!(t.unwrap(), vec![str_token("hello world")]);

        // Invalid hex digits
        assert!(Tokenizer::new(r#""\uGGGG""#).is_err());

        // Too few digits (at end of string)
        assert!(Tokenizer::new(r#""\u00""#).is_err());
    }

    #[test]
    fn test_error_location() {
        let err = Tokenizer::new("1 + ^").unwrap_err();
        let msg = err.to_string();
        // Should contain line:col position info
        assert!(msg.contains("1:"), "error should contain line info: {msg}");
    }

    #[test]
    fn sign_after_value_is_operator() {
        // "1-2" should tokenize as [Int(1), Sub, Int(2)], not [Int(1), Int(-2)]
        let t = Tokenizer::new("1-2").unwrap();
        assert_eq!(t, vec![Token::Int(1), Token::Sub, Token::Int(2)]);

        // "1+2" should tokenize as [Int(1), Add, Int(2)]
        let t = Tokenizer::new("1+2").unwrap();
        assert_eq!(t, vec![Token::Int(1), Token::Add, Token::Int(2)]);

        // After identifier
        let t = Tokenizer::new("@x+1").unwrap();
        assert_eq!(
            t,
            vec![Token::At, id("x"), Token::Add, Token::Int(1)]
        );

        // After closing paren
        let t = Tokenizer::new("(1)-2").unwrap();
        assert_eq!(
            t,
            vec![Token::LParen, Token::Int(1), Token::RParen, Token::Sub, Token::Int(2)]
        );
    }

    #[test]
    fn sign_at_expr_start_is_prefix() {
        // At input start, sign is part of number
        let t = Tokenizer::new("-3").unwrap();
        assert_eq!(t, vec![Token::Int(-3)]);

        // After operator, sign is part of number
        let t = Tokenizer::new("1 + -3").unwrap();
        assert_eq!(t, vec![Token::Int(1), Token::Add, Token::Int(-3)]);

        // After open paren
        let t = Tokenizer::new("(-3)").unwrap();
        assert_eq!(t, vec![Token::LParen, Token::Int(-3), Token::RParen]);

        // After comma
        let t = Tokenizer::new("[-1, +2]").unwrap();
        assert_eq!(
            t,
            vec![Token::LBracket, Token::Int(-1), Token::Comma, Token::Int(2), Token::RBracket]
        );
    }
}
