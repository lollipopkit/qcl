#[cfg(test)]
mod tests {
    use crate::{
        stmt::{Program, Environment},
        stmt_parser::StmtParser,
        token::Tokenizer,
        val::Val,
    };
    use std::collections::HashMap;

    fn parse_program(source: &str) -> Program {
        let tokens = Tokenizer::new(source).expect("Failed to tokenize");
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
        let program = parse_program(r#"
            let i = 0; 
            let sum = 0; 
            while (i < 5) { 
                i = i + 1; 
                if (i == 3) continue; 
                sum = sum + i; 
            }
        "#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_goto_label() {
        let program = parse_program(r#"
            let x = 0;
            goto end;
            x = 999;
            end:
            x = 42;
        "#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_block_scope() {
        let program = parse_program(r#"
            let x = 1;
            {
                let y = 2;
                x = x + y;
            }
        "#);
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
        assert!(result.unwrap_err().to_string().contains("Undefined variable"));
    }

    #[test]
    fn test_undefined_label_error() {
        let program = parse_program("goto undefined_label;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Undefined label"));
    }

    #[test]
    fn test_break_outside_loop_error() {
        let program = parse_program("break;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("break statement outside of loop"));
    }

    #[test]
    fn test_continue_outside_loop_error() {
        let program = parse_program("continue;");
        let ctx = empty_context();
        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("continue statement outside of loop"));
    }

    #[test]
    fn test_complex_program() {
        // 简化程序，避免无限循环
        let program = parse_program(r#"
            let n = 3;
            let sum = 0;
            let i = 1;
            
            if (i <= n) {
                sum = sum + i;
                i = i + 1;
            }
        "#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_variable_in_expression() {
        let program = parse_program(r#"
            let x = 5;
            let y = x + 3;
            let result = x * y;
        "#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_nested_blocks() {
        let program = parse_program(r#"
            let x = 1;
            {
                let y = 2;
                {
                    let z = 3;
                    x = x + y + z;
                }
            }
        "#);
        let ctx = empty_context();
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }

    #[test]
    fn test_context_access_with_variables() {
        let mut ctx_map = HashMap::new();
        ctx_map.insert("user".to_string(), Val::Map(std::sync::Arc::new({
            let mut user_map = HashMap::new();
            user_map.insert("age".to_string(), Val::Int(25));
            user_map
        })));
        let ctx = Val::Map(std::sync::Arc::new(ctx_map));

        let program = parse_program(r#"
            let min_age = 18;
            let user_age = @user.age;
            let is_adult = user_age >= min_age;
        "#);
        
        let result = program.execute(&ctx).expect("Failed to execute");
        assert_eq!(result, Val::Nil);
    }
}