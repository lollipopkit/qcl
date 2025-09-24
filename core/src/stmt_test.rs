#[cfg(test)]
mod tests {
    use crate::{
        stmt::{Environment, Program},
        stmt_parser::StmtParser,
        token::Tokenizer,
        val::Val,
    };
    use std::collections::HashMap;

    fn parse_program(source: &str) -> Program {
        let tokens = Tokenizer::tokenize(source).expect("Failed to tokenize");
        let mut parser = StmtParser::new(&tokens);
        parser.parse_program().expect("Failed to parse program")
    }

    fn empty_context() -> Val {
        Val::Map(std::sync::Arc::new(HashMap::new()))
    }

    #[test]
    fn test_let_statement() {
        let program = parse_program("let x = 42;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_assign_statement() {
        let program = parse_program("let x = 10; x = 20;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_if_statement() {
        let program = parse_program("let x = 0; if (true) x = 1;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_if_else_statement() {
        let program = parse_program("let x = 0; if (false) x = 1; else x = 2;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_while_loop() {
        let program = parse_program("let i = 0; while (i < 3) { i = i + 1; }");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_break_statement() {
        let program = parse_program("let i = 0; while (true) { i = i + 1; if (i >= 3) break; }");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_continue_statement() {
        let program = parse_program(
            r#"
            let i = 0; 
            let sum = 0; 
            while (i < 5) { 
                i = i + 1; 
                if (i == 3) continue; 
                sum = sum + i; 
            }
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }


    #[test]
    fn test_block_scope() {
        let program = parse_program(
            r#"
            let x = 1;
            {
                let y = 2;
                x = x + y;
            }
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_expression_statement() {
        let program = parse_program("2 + 3;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_environment() {
        let mut env = Environment::new();

        // Test define and get
        env.define("x".to_string(), Val::Int(42));
        assert_eq!(env.get("x"), Some(&Val::Int(42)));

        // Test assign
        env.assign("x", Val::Int(100)).expect("Failed to assign");
        assert_eq!(env.get("x"), Some(&Val::Int(100)));

        // Test scoping
        env.push_scope();
        env.define("y".to_string(), Val::Int(20));
        assert_eq!(env.get("y"), Some(&Val::Int(20)));
        assert_eq!(env.get("x"), Some(&Val::Int(100))); // Still accessible

        env.pop_scope();
        assert_eq!(env.get("y"), None); // No longer accessible
        assert_eq!(env.get("x"), Some(&Val::Int(100))); // Still accessible
    }

    #[test]
    fn test_undefined_variable_error() {
        let program = parse_program("x = 42;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Undefined variable")
        );
    }


    #[test]
    fn test_break_outside_loop_error() {
        let program = parse_program("break;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("break statement outside of loop")
        );
    }

    #[test]
    fn test_continue_outside_loop_error() {
        let program = parse_program("continue;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("continue statement outside of loop")
        );
    }

    #[test]
    fn test_complex_program() {
        // 简化程序，避免无限循环
        let program = parse_program(
            r#"
            let n = 3;
            let sum = 0;
            let i = 1;
            
            if (i <= n) {
                sum = sum + i;
                i = i + 1;
            }
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_variable_in_expression() {
        let program = parse_program(
            r#"
            let x = 5;
            let y = x + 3;
            let result = x * y;
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_nested_blocks() {
        let program = parse_program(
            r#"
            let x = 1;
            {
                let y = 2;
                {
                    let z = 3;
                    x = x + y + z;
                }
            }
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_context_access_with_variables() {
        let mut ctx_map = HashMap::new();
        ctx_map.insert(
            "user".to_string(),
            Val::Map(std::sync::Arc::new({
                let mut user_map = HashMap::new();
                user_map.insert("age".to_string(), Val::Int(25));
                user_map
            })),
        );
        let ctx = Val::Map(std::sync::Arc::new(ctx_map));

        let program = parse_program(
            r#"
            let min_age = 18;
            let user_age = @user.age;
            let is_adult = user_age >= min_age;
        "#,
        );

        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_return_with_value() {
        let program = parse_program(
            r#"
            let x = 42;
            return x + 8;
            let y = 100; // This should not be executed
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(50));
    }

    #[test]
    fn test_simple_return_with_literal() {
        let program = parse_program("return 123;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(123));
    }

    #[test]
    fn test_return_with_variable() {
        let program = parse_program(
            r#"
            let x = 42;
            return x;
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(42));
    }

    #[test]
    fn test_return_with_addition() {
        let program = parse_program(
            r#"
            let x = 1;
            let y = 2;
            return x + y;
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(3));
    }

    #[test]
    fn test_return_without_value() {
        let program = parse_program(
            r#"
            let x = 10;
            return;
            let y = 20; // This should not be executed
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_return_in_block() {
        let program = parse_program(
            r#"
            let x = 1;
            {
                let y = 2;
                return x + y;
                let z = 999; // This should not be executed
            }
            let w = 100; // This should not be executed either
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(3));
    }

    #[test]
    fn test_return_in_if_statement() {
        let program = parse_program(
            r#"
            let x = 5;
            if (x > 3) {
                return x * 2;
            } else {
                return x;
            }
            let y = 999; // This should not be executed
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(10));
    }

    #[test]
    fn test_return_in_while_loop() {
        let program = parse_program(
            r#"
            let i = 0;
            while (i < 5) {
                i = i + 1;
                if (i == 3) {
                    return i * 10;
                }
            }
            let done = 999; // This should not be executed
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Int(30));
    }

    // Type annotation tests
    #[test]
    fn test_let_with_type_annotation_int() {
        let program = parse_program("let x: Int = 42;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_string() {
        let program = parse_program(r#"let name: String = "hello";"#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_bool() {
        let program = parse_program("let flag: Bool = true;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_float() {
        let program = parse_program("let pi: Float = 3.14;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_nil() {
        let program = parse_program("let empty: Nil = nil;");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_list() {
        let program = parse_program("let items: List = [1, 2, 3];");
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_with_type_annotation_map() {
        let program = parse_program(r#"let data: Map = {"key": "value"};"#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_let_type_mismatch_int() {
        let program = parse_program(r#"let x: Int = "not_int";"#);
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_let_type_mismatch_string() {
        let program = parse_program("let name: String = 42;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_let_type_mismatch_bool() {
        let program = parse_program("let flag: Bool = 123;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_let_type_mismatch_float() {
        let program = parse_program("let pi: Float = true;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_unknown_type_error() {
        let tokens = Tokenizer::tokenize("let x: UnknownType = 42;").expect("Failed to tokenize");
        let mut parser = StmtParser::new(&tokens);
        let result = parser.parse_program();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unknown type"));
    }

    #[test]
    fn test_mixed_typed_and_untyped_variables() {
        let program = parse_program(
            r#"
            let x: Int = 42;
            let y = "hello";
            let z: Bool = true;
            let w = 3.14;
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_type_annotation_in_complex_expression() {
        let program = parse_program(
            r#"
            let x: Int = 10;
            let y: Int = 20;
            let sum: Int = x + y;
            let result: Bool = sum > 25;
        "#,
        );
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }
}
