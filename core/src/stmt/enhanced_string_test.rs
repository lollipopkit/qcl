#[cfg(test)]
mod tests {
    use crate::{
        stmt::StmtParser,
        val::Val,
        token::Tokenizer,
    };
    use std::sync::Arc;

    fn parse_and_execute_stmt(stmt_code: &str, ctx: &Val) -> Result<Val, anyhow::Error> {
        let tokens = Tokenizer::tokenize(stmt_code)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        program.execute(ctx)
    }

    #[test]
    fn test_enhanced_string_positional_parameters() {
        let ctx = Val::Map(Arc::new([
            ("name".to_string(), Val::Str("Alice".into())),
            ("age".to_string(), Val::Int(25)),
            ("items".to_string(), Val::List(Arc::new(vec![Val::Int(1), Val::Int(2)])))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "{0} is {1} years old and has {2} items";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Alice is 25 years old and has [1, 2] items".into()));
    }

    #[test]
    fn test_enhanced_string_named_parameters() {
        let ctx = Val::Map(Arc::new([
            ("user".to_string(), Val::Map(Arc::new([
                ("name".to_string(), Val::Str("Bob".into())),
                ("age".to_string(), Val::Int(30))
            ].into_iter().collect()))),
            ("data".to_string(), Val::List(Arc::new(vec![Val::Int(1), Val::Int(2), Val::Int(3)])))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "{user.name} is {user.age} years old. Data length: {data.len}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Bob is 30 years old. Data length: 3".into()));
    }

    #[test]
    fn test_enhanced_string_formatting_options() {
        let ctx = Val::Map(Arc::new([
            ("price".to_string(), Val::Float(123.4567)),
            ("count".to_string(), Val::Int(42)),
            ("pi".to_string(), Val::Float(3.14159)),
            ("message".to_string(), Val::Str("Hello".into()))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Price: {price:.2}, Count: {count:04d}, Pi: {pi:.3f}, Message: {message:10s}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Price: 123.46, Count: 0042, Pi: 3.142, Message: Hello     ".into()));
    }

    #[test]
    fn test_enhanced_string_mixed_syntax() {
        let ctx = Val::Map(Arc::new([
            ("user".to_string(), Val::Map(Arc::new([
                ("name".to_string(), Val::Str("Charlie".into())),
                ("age".to_string(), Val::Int(35))
            ].into_iter().collect()))),
            ("items".to_string(), Val::List(Arc::new(vec![Val::Int(1), Val::Int(2)])))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "User {user.name} (${user.age}) has {items.len} items";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("User Charlie (35) has 2 items".into()));
    }

    #[test]
    fn test_enhanced_string_error_handling() {
        let ctx = Val::Map(Arc::new([
            ("name".to_string(), Val::Str("Alice".into()))
        ].into_iter().collect()));

        // Test undefined variable
        let result = parse_and_execute_stmt(
            r#"
            let result = "Hello {undefined}";
            return result;
            "#,
            &ctx
        );

        assert!(result.is_err());

        // Test invalid format specifier
        let result = parse_and_execute_stmt(
            r#"
            let result = "Value: {name:invalid}";
            return result;
            "#,
            &ctx
        );

        assert!(result.is_err());
    }

    #[test]
    fn test_enhanced_string_expressions() {
        let ctx = Val::Map(Arc::new([
            ("a".to_string(), Val::Int(10)),
            ("b".to_string(), Val::Int(20)),
            ("numbers".to_string(), Val::List(Arc::new(vec![Val::Int(1), Val::Int(2), Val::Int(3)])))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Sum: {a + b}, Product: {a * b}, List sum: {numbers.sum}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Sum: 30, Product: 200, List sum: 6".into()));
    }

    #[test]
    fn test_enhanced_string_escape_sequences() {
        let ctx = Val::Map(Arc::new([
            ("value".to_string(), Val::Str("test".into()))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Escaped: \{\{ \}\} \\\\ \\" \\n {value}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Escaped: {} \\ \" \n test".into()));
    }

    #[test]
    fn test_enhanced_string_boolean_formatting() {
        let ctx = Val::Map(Arc::new([
            ("flag".to_string(), Val::Bool(true)),
            ("count".to_string(), Val::Int(0))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Flag: {flag:b}, Count is zero: {count == 0:b}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Flag: true, Count is zero: true".into()));
    }

    #[test]
    fn test_enhanced_string_scientific_notation() {
        let ctx = Val::Map(Arc::new([
            ("large".to_string(), Val::Float(1234567.89)),
            ("small".to_string(), Val::Float(0.000123))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Large: {large:e}, Small: {small:.2e}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Large: 1.23456789e6, Small: 1.23e-4".into()));
    }

    #[test]
    fn test_enhanced_string_hex_octal_formatting() {
        let ctx = Val::Map(Arc::new([
            ("value".to_string(), Val::Int(255)),
            ("neg_value".to_string(), Val::Int(-128))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Hex: {value:x}, Hex upper: {value:X}, Octal: {value:o}";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Hex: ff, Hex upper: FF, Octal: 377".into()));
    }

    #[test]
    fn test_enhanced_string_complex_expression() {
        let ctx = Val::Map(Arc::new([
            ("data".to_string(), Val::List(Arc::new(vec![Val::Int(1), Val::Int(2), Val::Int(3), Val::Int(4)]))),
            ("threshold".to_string(), Val::Int(2))
        ].into_iter().collect()));

        let result = parse_and_execute_stmt(
            r#"
            let result = "Values > {threshold}: {data.filter(|x| x > threshold).len} items";
            return result;
            "#,
            &ctx
        ).unwrap();

        assert_eq!(result, Val::Str("Values > 2: 2 items".into()));
    }
}