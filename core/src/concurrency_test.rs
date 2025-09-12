#[cfg(test)]
mod tests {
    use crate::{
        concurrency::{Channel, next_goroutine_id},
        stmt::Stmt,
        stmt_parser::StmtParser,
        token::Tokenizer,
        val::Val,
    };
    use anyhow::Result;
    use std::sync::Arc;

    #[test]
    fn test_channel_creation() -> Result<()> {
        // Test unbuffered channel
        let ch = Channel::new();
        assert_eq!(ch.capacity, 0);

        // Test buffered channel
        let ch_buffered = Channel::with_capacity(5);
        assert_eq!(ch_buffered.capacity, 5);

        Ok(())
    }

    #[test]
    fn test_goroutine_id_generation() {
        let id1 = next_goroutine_id();
        let id2 = next_goroutine_id();
        assert_ne!(id1, id2);
        assert!(id2 > id1);
    }

    #[test]
    fn test_go_statement_parsing() -> Result<()> {
        let tokens = Tokenizer::tokenize("go { let x = 42; }")?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;

        match stmt {
            Stmt::Go { body } => {
                // The body should be a block statement
                if let Stmt::Block { statements } = body.as_ref() {
                    assert_eq!(statements.len(), 1);
                } else {
                    panic!("Expected block statement in go body");
                }
            }
            _ => panic!("Expected Go statement"),
        }

        Ok(())
    }

    #[test]
    fn test_channel_send_recv_parsing() -> Result<()> {
        // Test channel receive parsing
        let tokens = Tokenizer::tokenize("<-ch;")?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;

        match stmt {
            Stmt::ChannelRecv {
                variable,
                channel: _,
            } => {
                assert_eq!(variable, None);
            }
            _ => panic!("Expected ChannelRecv statement"),
        }

        // Test channel send parsing
        let tokens = Tokenizer::tokenize("ch <- 42;")?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;

        match stmt {
            Stmt::ChannelSend { channel: _, value: _ } => {}
            _ => panic!("Expected ChannelSend statement"),
        }

        // Test select parsing with :=
        let tokens = Tokenizer::tokenize("select { case v := <- ch: {} }")?;
        let mut parser = StmtParser::new(&tokens);
        let stmt = parser.parse_statement()?;
        match stmt {
            Stmt::Select { .. } => {}
            _ => panic!("Expected Select statement with := recv case"),
        }

        Ok(())
    }

    #[test]
    fn test_go_keyword_tokenization() -> Result<()> {
        // Test that go keyword is properly tokenized
        let tokens = Tokenizer::tokenize("go")?;
        assert_eq!(tokens.len(), 1);
        if let Some(token) = tokens.first() {
            match token {
                crate::token::Token::Go => {} // Success
                _ => panic!("Expected Go token, got {:?}", token),
            }
        }
        Ok(())
    }

    #[test]
    fn test_basic_channel_operations() -> Result<()> {
        let ch = Channel::new();

        // Test that try_recv returns None when channel is empty
        assert_eq!(ch.try_recv()?, None);

        // Test that try_send works (though with unbuffered channels it might fail)
        let test_val = Val::Int(42);

        // For unbuffered channels, try_send typically fails unless there's a concurrent receiver
        // So we just test that the operation doesn't panic
        let _ = ch.try_send(test_val.clone());

        Ok(())
    }

    #[test]
    fn test_channel_send_stmt_exec() -> Result<()> {
        // Prepare channel in env
        let ch = Channel::with_capacity(1);

        // Parse send statement
        let tokens = Tokenizer::tokenize("ch <- 7;")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        let received = ch.try_recv()?;
        match received {
            Some(Val::Int(i)) => assert_eq!(i, 7),
            other => panic!("Unexpected recv value: {:?}", other),
        }
        Ok(())
    }

    #[test]
    fn test_let_channel_recv_stmt_exec() -> Result<()> {
        // Prepare channel with a value
        let ch = Channel::with_capacity(1);
        ch.send(Val::Int(21))?;

        // Parse receive with let-binding
        let tokens = Tokenizer::tokenize("let v = <- ch;")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        // v should be defined to 21
        match env.get("v").cloned() {
            Some(Val::Int(n)) => assert_eq!(n, 21),
            other => panic!("Expected v == 21, got {:?}", other),
        }
        Ok(())
    }

    #[test]
    fn test_assign_channel_recv_stmt_exec() -> Result<()> {
        // Prepare channel with a value
        let ch = Channel::with_capacity(1);
        ch.send(Val::Int(33))?;

        // Parse receive with assignment
        let tokens = Tokenizer::tokenize("v = <- ch;")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        // v should be defined to 33
        match env.get("v").cloned() {
            Some(Val::Int(n)) => assert_eq!(n, 33),
            other => panic!("Expected v == 33, got {:?}", other),
        }
        Ok(())
    }

    #[test]
    fn test_define_short_assign_channel_recv_stmt_exec() -> Result<()> {
        // Prepare channel with a value
        let ch = Channel::with_capacity(1);
        ch.send(Val::Int(44))?;

        // Parse receive with short declaration
        let tokens = Tokenizer::tokenize("v := <- ch;")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        // v should be defined to 44
        match env.get("v").cloned() {
            Some(Val::Int(n)) => assert_eq!(n, 44),
            other => panic!("Expected v == 44, got {:?}", other),
        }
        Ok(())
    }

    #[test]
    fn test_define_short_assign_regular_expr_exec() -> Result<()> {
        let tokens = Tokenizer::tokenize("x := 1; y := x + 2;")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();

        let _ = program.execute_with_env(&ctx, &mut env)?;

        match env.get("y").cloned() {
            Some(Val::Int(n)) => assert_eq!(n, 3),
            other => panic!("Expected y == 3, got {:?}", other),
        }
        Ok(())
    }

    #[test]
    fn test_unary_recv_in_expression() -> Result<()> {
        // Prepare channel with a value 2
        let ch = Channel::with_capacity(1);
        ch.send(Val::Int(2))?;

        // Use receive as part of an expression
        let tokens = Tokenizer::tokenize("let v = 1 + (<- ch);")?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        match env.get("v").cloned() {
            Some(Val::Int(n)) => assert_eq!(n, 3),
            other => panic!("Expected v == 3, got {:?}", other),
        }

        Ok(())
    }

    #[test]
    fn test_select_blocks_until_ready() -> Result<()> {
        let ch = Channel::new(); // unbuffered: send blocks until recv

        // Spawn a sender after a small delay
        let ch_sender = ch.clone();
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(10));
            let _ = ch_sender.send(Val::Int(99));
        });

        // select that receives from ch and stores into variable v
        let program_text = "select { case v = <- ch: {} }";
        let tokens = Tokenizer::tokenize(program_text)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let mut env = crate::stmt::Environment::new();
        env.define("ch".to_string(), Val::Channel(ch.clone()));

        let _ = program.execute_with_env(&ctx, &mut env)?;

        // v should be defined
        let v = env.get("v").cloned();
        match v {
            Some(Val::Int(n)) => assert_eq!(n, 99),
            other => panic!("Expected v == 99, got {:?}", other),
        }

        Ok(())
    }

    #[test]
    fn test_program_with_goroutine() -> Result<()> {
        let program_text = r#"
            let x = 10;
            go {
                let y = x + 5;
            };
        "#;

        let tokens = Tokenizer::tokenize(program_text)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        // Should have 3 statements: let, go, and empty statement from trailing semicolon
        assert_eq!(program.statements.len(), 3);

        // Execute the program
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));
        let result = program.execute(&ctx)?;
        assert_eq!(result, Val::Nil);

        Ok(())
    }

    #[test]
    fn test_channel_types_in_val() {
        let ch = Channel::new();
        let ch_val = Val::Channel(ch);

        assert_eq!(ch_val.type_name(), "Channel");

        let goroutine_id = next_goroutine_id();
        let handle = std::thread::spawn(|| Ok(Val::Nil));
        let goroutine_handle = crate::concurrency::GoroutineHandle::new(handle, goroutine_id);
        let goroutine_val = Val::Goroutine(goroutine_handle);

        assert_eq!(goroutine_val.type_name(), "Goroutine");
    }
}
