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
