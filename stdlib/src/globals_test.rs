#[cfg(test)]
mod tests {
    use anyhow::Result;
    use qcl_core::{stmt::stmt_parser::StmtParser, token::Tokenizer, val::Val};
    use std::sync::Arc;

    #[test]
    fn test_global_printf_and_panic_available() -> Result<()> {
        // Program uses print/println (globals) and returns a value
        let source = "print(\"hello {}\", 1); println(\" world\"); return 42;";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(Default::default()));

        // Create registry, register modules + globals
        let mut registry = qcl_core::module::ModuleRegistry::new();
        crate::register_stdlib_modules(&mut registry);
        crate::register_stdlib_globals(&mut registry);

        // Create environment with this registry
        let resolver = std::sync::Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Int(42));
        Ok(())
    }

    #[test]
    fn test_global_panic_panics_with_backtrace() {
        let source = "panic(\"boom\");";
        let tokens = Tokenizer::tokenize(source).unwrap();
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program().unwrap();
        let ctx = Val::Map(Arc::new(Default::default()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        crate::register_stdlib_modules(&mut registry);
        crate::register_stdlib_globals(&mut registry);

        let resolver = std::sync::Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = program.execute_with_env(&ctx, &mut env);
        }));

        assert!(result.is_err(), "expected panic, but code did not panic");
    }
}
