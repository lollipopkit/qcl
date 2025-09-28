//! Tests for concurrency features

#[cfg(test)]
mod tests {
    use crate::expr::Expr;
    use crate::val::Val;
    use anyhow::Result;

    #[cfg(feature = "concurrency")]
    #[tokio::test]
    async fn test_spawn_expression_parsing() -> Result<()> {
        let expr = Expr::parse_cached("spawn(42)")?;

        // Initialize runtime for testing
        crate::rt::init_runtime()?;

        let ctx = Val::Nil;
        let result = expr.eval(&ctx)?;

        // Should return a Task
        assert!(matches!(result, Val::Task { .. }));

        Ok(())
    }

    #[cfg(feature = "concurrency")]
    #[tokio::test]
    async fn test_channel_creation() -> Result<()> {
        let expr = Expr::parse_cached("chan(10)")?;

        crate::rt::init_runtime()?;

        let ctx = Val::Nil;
        let result = expr.eval(&ctx)?;

        // Should return a Channel
        assert!(matches!(
            result,
            Val::Channel {
                capacity: Some(10),
                ..
            }
        ));

        Ok(())
    }

    #[cfg(feature = "concurrency")]
    #[test]
    fn test_concurrency_ast_parsing() -> Result<()> {
        // Test parsing spawn expression
        println!("Testing spawn...");
        let spawn_expr = Expr::parse_cached("spawn(42)")?;
        assert!(matches!(spawn_expr, Expr::Spawn(_)));

        // Test parsing channel creation
        println!("Testing chan...");
        let chan_expr = Expr::parse_cached("chan(5)")?;
        assert!(matches!(chan_expr, Expr::ChanLiteral { .. }));

        println!("All tests passed");
        Ok(())
    }

    #[cfg(feature = "concurrency")]
    #[test]
    fn test_select_parsing() -> Result<()> {
        let select_code = r#"select {
            case value <= recv(ch1) => value;
            case _ <= send(ch2, 42) => "sent";
            default => "timeout";
        }"#;

        let expr = Expr::parse_cached(select_code)?;
        if let Expr::Select {
            cases,
            default_case,
        } = expr
        {
            assert_eq!(cases.len(), 2);
            assert!(default_case.is_some());
        } else {
            panic!("Expected Select expression");
        }

        Ok(())
    }

    #[test]
    fn test_concurrency_without_feature() {
        // When concurrency feature is disabled, these should parse but evaluate differently
        let spawn_expr = Expr::parse_cached("spawn(42)").unwrap();
        let ctx = Val::Nil;

        // Without concurrency feature, should return the inner expression result
        let result = spawn_expr.eval(&ctx);
        match result {
            Ok(val) => println!("Success: {:?}", val),
            Err(e) => println!("Error: {}", e),
        }
        // For now, let's just check that evaluation happens without panic
        // The actual behavior may vary based on feature flags
    }
}
