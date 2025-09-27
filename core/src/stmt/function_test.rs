#[cfg(test)]
mod tests {
    use crate::{
        expr::Expr,
        stmt::{Environment, Stmt, stmt_parser::StmtParser},
        token::Tokenizer,
        val::Val,
    };
    use anyhow::Result;
    use std::sync::Arc;

    #[test]
    fn test_function_definition_parsing() -> Result<()> {
        let source = "fn add(a, b) { return a + b; }";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;

        if let Stmt::Function { name, params, body } = stmt {
            assert_eq!(name, "add");
            assert_eq!(params, vec!["a", "b"]);
            assert!(matches!(body.as_ref(), Stmt::Block { .. }));
        } else {
            panic!("Expected Function statement, got: {:?}", stmt);
        }

        Ok(())
    }

    #[test]
    fn test_function_no_params_parsing() -> Result<()> {
        let source = "fn hello() { return \"Hello, World!\"; }";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;

        if let Stmt::Function {
            name,
            params,
            body: _,
        } = stmt
        {
            assert_eq!(name, "hello");
            assert!(params.is_empty());
        } else {
            panic!("Expected Function statement");
        }

        Ok(())
    }

    #[test]
    fn test_function_call_parsing() -> Result<()> {
        let tokens = Tokenizer::tokenize("add(1, 2)")?;
        let mut parser = crate::ast::Parser::new(&tokens);
        let expr = parser.parse()?;

        if let Expr::CallExpr(expr, args) = expr {
            if let Expr::Var(name) = *expr {
                assert_eq!(name, "add");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].as_ref(), &Expr::Val(Val::Int(1)));
                assert_eq!(args[1].as_ref(), &Expr::Val(Val::Int(2)));
            } else {
                panic!("Expected variable as function target, got: {:?}", expr);
            }
        } else {
            panic!("Expected function call, got: {:?}", expr);
        }

        Ok(())
    }

    #[test]
    fn test_function_call_no_args_parsing() -> Result<()> {
        let tokens = Tokenizer::tokenize("hello()")?;
        let mut parser = crate::ast::Parser::new(&tokens);
        let expr = parser.parse()?;

        if let Expr::CallExpr(expr, args) = expr {
            if let Expr::Var(name) = *expr {
                assert_eq!(name, "hello");
                assert!(args.is_empty());
            } else {
                panic!("Expected variable as function target, got: {:?}", expr);
            }
        } else {
            panic!("Expected function call");
        }

        Ok(())
    }

    #[test]
    fn test_function_execution_simple() -> Result<()> {
        let source = "fn add(a, b) { return a + b; } return add(3, 4);";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Int(7));

        Ok(())
    }

    #[test]
    fn test_function_execution_no_params() -> Result<()> {
        let source = "fn greeting() { return \"Hello!\"; } return greeting();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Str(Arc::from("Hello!")));

        Ok(())
    }

    #[test]
    fn test_function_execution_with_variables() -> Result<()> {
        let source = r#"
            fn multiply(x, y) {
                let result = x * y;
                return result;
            }
            let a = 5;
            let b = 6;
            return multiply(a, b);
        "#;
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Int(30));

        Ok(())
    }

    #[test]
    fn test_function_parameter_scope() -> Result<()> {
        let source = r#"
            let x = 10;
            fn test(x) {
                return x + 1;
            }
            return test(5);
        "#;
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        // Should return 6 (5 + 1), not 11 (10 + 1)
        assert_eq!(result, Val::Int(6));

        Ok(())
    }

    #[test]
    fn test_function_returns_nil_by_default() -> Result<()> {
        let source = "fn test() { let x = 5; } return test();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Nil);

        Ok(())
    }

    #[test]
    fn test_recursive_function() -> Result<()> {
        let source = r#"
            fn factorial(n) {
                if (n <= 1) {
                    return 1;
                } else {
                    return n * factorial(n - 1);
                }
            }
            return factorial(5);
        "#;
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Int(120));

        Ok(())
    }

    #[test]
    fn test_function_with_context_access() -> Result<()> {
        let source = r#"
            fn getUserAge() {
                return @user.age;
            }
            return getUserAge();
        "#;
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let mut ctx_map = std::collections::HashMap::new();
        let mut user_map = std::collections::HashMap::new();
        user_map.insert("age".to_string(), Val::Int(25));
        ctx_map.insert("user".to_string(), Val::Map(Arc::new(user_map)));
        let ctx = Val::Map(Arc::new(ctx_map));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Int(25));

        Ok(())
    }

    #[test]
    fn test_function_call_with_wrong_arg_count() -> Result<()> {
        let source = "fn add(a, b) { return a + b; } add(1);";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(
            result
                .err()
                .unwrap()
                .to_string()
                .contains("expects 2 arguments")
        );

        Ok(())
    }

    #[test]
    fn test_undefined_function_call() -> Result<()> {
        let source = "nonexistent();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx);
        assert!(result.is_err());
        let error_msg = result.err().unwrap().to_string();
        assert!(error_msg.contains("Undefined variable: nonexistent"));

        Ok(())
    }

    #[test]
    fn test_calling_non_function() -> Result<()> {
        let source = "let x = 5; x();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx);
        assert!(result.is_err());
        assert!(
            result
                .err()
                .unwrap()
                .to_string()
                .contains("is not a function")
        );

        Ok(())
    }

    #[test]
    fn test_function_display_formatting() {
        let func_val = Val::Closure {
            params: Arc::new(vec!["x".to_string(), "y".to_string()]),
            body: Arc::new(Stmt::Empty),
            env: Arc::new(Environment::new()),
        };

        assert_eq!(func_val.to_string(), "fn(x, y)");
    }

    #[test]
    fn test_nested_function_calls() -> Result<()> {
        let source = r#"
            fn add(a, b) { return a + b; }
            fn multiply(x, y) { return x * y; }
            fn compute() { return add(multiply(2, 3), 4); }
            return compute();
        "#;
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Int(10)); // multiply(2, 3) = 6, add(6, 4) = 10

        Ok(())
    }
}
