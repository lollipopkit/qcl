use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct StringModule {
    functions: HashMap<String, Val>,
}

impl Default for StringModule {
    fn default() -> Self {
        Self::new()
    }
}

impl StringModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Register string functions as Rust functions
        functions.insert("len".to_string(), Val::RustFunction(Self::len));
        functions.insert("lower".to_string(), Val::RustFunction(Self::lower));
        functions.insert("upper".to_string(), Val::RustFunction(Self::upper));
        functions.insert("trim".to_string(), Val::RustFunction(Self::trim));
        functions.insert(
            "starts_with".to_string(),
            Val::RustFunction(Self::starts_with),
        );
        functions.insert("ends_with".to_string(), Val::RustFunction(Self::ends_with));
        functions.insert("contains".to_string(), Val::RustFunction(Self::contains));
        functions.insert("replace".to_string(), Val::RustFunction(Self::replace));
        functions.insert("substring".to_string(), Val::RustFunction(Self::substring));
        functions.insert("split".to_string(), Val::RustFunction(Self::split));
        functions.insert("join".to_string(), Val::RustFunction(Self::join));

        Self { functions }
    }

    /// Get string length
    fn len(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("len() takes exactly 1 argument"));
        }

        match &args[0] {
            Val::Str(s) => Ok(Val::Int(s.len() as i64)),
            _ => Err(anyhow::anyhow!("len() argument must be a string")),
        }
    }

    /// Convert to lowercase
    fn lower(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("lower() takes exactly 1 argument"));
        }

        match &args[0] {
            Val::Str(s) => Ok(Val::Str(s.to_lowercase().into())),
            _ => Err(anyhow::anyhow!("lower() argument must be a string")),
        }
    }

    /// Convert to uppercase
    fn upper(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("upper() takes exactly 1 argument"));
        }

        match &args[0] {
            Val::Str(s) => Ok(Val::Str(s.to_uppercase().into())),
            _ => Err(anyhow::anyhow!("upper() argument must be a string")),
        }
    }

    /// Trim whitespace from both ends
    fn trim(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("trim() takes exactly 1 argument"));
        }

        match &args[0] {
            Val::Str(s) => Ok(Val::Str(s.trim().into())),
            _ => Err(anyhow::anyhow!("trim() argument must be a string")),
        }
    }

    /// Check if string starts with prefix
    fn starts_with(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "starts_with() takes exactly 2 arguments: string, prefix"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "starts_with() first argument must be a string"
                ));
            }
        };

        let prefix = match &args[1] {
            Val::Str(p) => &**p,
            _ => {
                return Err(anyhow::anyhow!(
                    "starts_with() second argument must be a string"
                ));
            }
        };

        Ok(Val::Bool(string.starts_with(prefix)))
    }

    /// Check if string ends with suffix
    fn ends_with(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "ends_with() takes exactly 2 arguments: string, suffix"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "ends_with() first argument must be a string"
                ));
            }
        };

        let suffix = match &args[1] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "ends_with() second argument must be a string"
                ));
            }
        };

        Ok(Val::Bool(string.ends_with(suffix)))
    }

    /// Check if string contains substring
    fn contains(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "contains() takes exactly 2 arguments: string, substring"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "contains() first argument must be a string"
                ));
            }
        };

        let substring = match &args[1] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "contains() second argument must be a string"
                ));
            }
        };

        Ok(Val::Bool(string.contains(substring)))
    }

    /// Replace occurrences of substring
    fn replace(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 3 {
            return Err(anyhow::anyhow!(
                "replace() takes exactly 3 arguments: string, old, new"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("replace() first argument must be a string")),
        };

        let old = match &args[1] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "replace() second argument must be a string"
                ));
            }
        };

        let new = match &args[2] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("replace() third argument must be a string")),
        };

        Ok(Val::Str(string.replace(old, new).into()))
    }

    /// Extract substring
    fn substring(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 3 {
            return Err(anyhow::anyhow!(
                "substring() takes exactly 3 arguments: string, start, length"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => {
                return Err(anyhow::anyhow!(
                    "substring() first argument must be a string"
                ));
            }
        };

        let start = match &args[1] {
            Val::Int(i) => *i as usize,
            _ => {
                return Err(anyhow::anyhow!(
                    "substring() second argument must be an integer"
                ));
            }
        };

        let length = match &args[2] {
            Val::Int(i) => *i as usize,
            _ => {
                return Err(anyhow::anyhow!(
                    "substring() third argument must be an integer"
                ));
            }
        };

        if start > string.len() {
            return Err(anyhow::anyhow!("substring() start index out of bounds"));
        }

        let end = std::cmp::min(start + length, string.len());
        Ok(Val::Str(string[start..end].into()))
    }

    /// Split string by delimiter
    fn split(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "split() takes exactly 2 arguments: string, delimiter"
            ));
        }

        let string = match &args[0] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("split() first argument must be a string")),
        };

        let delimiter = match &args[1] {
            Val::Str(d) => &**d,
            _ => return Err(anyhow::anyhow!("split() second argument must be a string")),
        };

        let parts: Vec<Val> = if delimiter.is_empty() {
            string
                .chars()
                .map(|c| Val::Str(c.to_string().into()))
                .collect()
        } else {
            string
                .split(delimiter)
                .map(|s| Val::Str(s.into()))
                .collect()
        };

        Ok(Val::List(Arc::new(parts)))
    }

    /// Join list of strings with delimiter
    fn join(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "join() takes exactly 2 arguments: list, delimiter"
            ));
        }

        let list = match &args[0] {
            Val::List(l) => &**l,
            _ => return Err(anyhow::anyhow!("join() first argument must be a list")),
        };

        let delimiter = match &args[1] {
            Val::Str(d) => &**d,
            _ => return Err(anyhow::anyhow!("join() second argument must be a string")),
        };

        let mut strings = Vec::new();
        for item in list {
            match item {
                Val::Str(s) => strings.push(&**s),
                _ => return Err(anyhow::anyhow!("join() list must contain only strings")),
            }
        }

        Ok(Val::Str(strings.join(delimiter).into()))
    }
}

impl Module for StringModule {
    fn name(&self) -> &str {
        "string"
    }

    fn description(&self) -> &str {
        "String manipulation functions"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::register_stdlib_modules;
    use anyhow::Result;
    use qcl_core::{stmt::stmt_parser::StmtParser, token::Tokenizer, val::Val};
    use std::sync::Arc;

    #[test]
    fn test_string_len() -> Result<()> {
        let source = "import string; return string.len(\"hello\");";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        // Create registry and register stdlib modules
        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);

        // Create environment with stdlib modules
        let resolver =
            std::sync::Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Int(5));

        Ok(())
    }

    #[test]
    fn test_string_lower() -> Result<()> {
        let source = "import string; return string.lower(\"HELLO\");";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        // Create registry and register stdlib modules
        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);

        // Create environment with stdlib modules
        let resolver =
            std::sync::Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Str("hello".into()));

        Ok(())
    }
}
