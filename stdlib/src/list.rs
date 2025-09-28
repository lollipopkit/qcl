use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct ListModule {
    functions: HashMap<String, Val>,
}

impl Default for ListModule {
    fn default() -> Self {
        Self::new()
    }
}

impl ListModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Core list utilities
        functions.insert("len".to_string(), Val::RustFunction(Self::len));
        functions.insert("push".to_string(), Val::RustFunction(Self::push));
        functions.insert("concat".to_string(), Val::RustFunction(Self::concat));
        functions.insert("join".to_string(), Val::RustFunction(Self::join));
        functions.insert("get".to_string(), Val::RustFunction(Self::get));
        functions.insert("first".to_string(), Val::RustFunction(Self::first));
        functions.insert("last".to_string(), Val::RustFunction(Self::last));

        // Register as meta-methods for List
        qcl_core::val::methods::register_method("List", "len", Self::len);
        qcl_core::val::methods::register_method("List", "push", Self::push);
        qcl_core::val::methods::register_method("List", "concat", Self::concat);
        qcl_core::val::methods::register_method("List", "join", Self::join);
        qcl_core::val::methods::register_method("List", "get", Self::get);
        qcl_core::val::methods::register_method("List", "first", Self::first);
        qcl_core::val::methods::register_method("List", "last", Self::last);

        Self { functions }
    }

    fn len(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("len() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::List(l) => Ok(Val::Int(l.len() as i64)),
            _ => Err(anyhow::anyhow!("len() argument must be a list")),
        }
    }

    // Return a new list with value appended (immutable)
    fn push(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("push() takes exactly 2 arguments: list, value"));
        }
        match (&args[0], &args[1]) {
            (Val::List(l), v) => {
                let mut out = Vec::with_capacity(l.len() + 1);
                out.extend(l.iter().cloned());
                out.push(v.clone());
                Ok(Val::List(Arc::new(out)))
            }
            _ => Err(anyhow::anyhow!("push() first argument must be a list")),
        }
    }

    // Concatenate two lists
    fn concat(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("concat() takes exactly 2 arguments: list, other_list"));
        }
        match (&args[0], &args[1]) {
            (Val::List(a), Val::List(b)) => {
                let mut out = Vec::with_capacity(a.len() + b.len());
                out.extend(a.iter().cloned());
                out.extend(b.iter().cloned());
                Ok(Val::List(Arc::new(out)))
            }
            (Val::List(_), _) => Err(anyhow::anyhow!("concat() second argument must be a list")),
            _ => Err(anyhow::anyhow!("concat() first argument must be a list")),
        }
    }

    // Join a list of strings with a delimiter
    fn join(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "join() takes exactly 2 arguments: list<string>, delimiter"
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
        let mut strings: Vec<&str> = Vec::with_capacity(list.len());
        for item in list.iter() {
            match item {
                Val::Str(s) => strings.push(&**s),
                _ => return Err(anyhow::anyhow!("join() list must contain only strings")),
            }
        }
        Ok(Val::Str(strings.join(delimiter).into()))
    }

    // Safe index access: get(index) -> value|nil
    fn get(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("get() takes exactly 2 arguments: list, index"));
        }
        let list = match &args[0] {
            Val::List(l) => &**l,
            _ => return Err(anyhow::anyhow!("get() first argument must be a list")),
        };
        let idx = match &args[1] {
            Val::Int(i) => *i,
            _ => return Err(anyhow::anyhow!("get() index must be an integer")),
        };
        if idx < 0 {
            return Ok(Val::Nil);
        }
        let uidx = idx as usize;
        Ok(list.get(uidx).cloned().unwrap_or(Val::Nil))
    }

    fn first(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("first() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::List(l) => Ok(l.first().cloned().unwrap_or(Val::Nil)),
            _ => Err(anyhow::anyhow!("first() argument must be a list")),
        }
    }

    fn last(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("last() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::List(l) => Ok(l.last().cloned().unwrap_or(Val::Nil)),
            _ => Err(anyhow::anyhow!("last() argument must be a list")),
        }
    }
}

impl Module for ListModule {
    fn name(&self) -> &str {
        "list"
    }

    fn description(&self) -> &str {
        "List utilities and meta-methods"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        // Functions are available via module import; meta methods are registered above
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::register_stdlib_modules;
    use anyhow::Result;
    use qcl_core::{stmt::stmt_parser::StmtParser, token::Tokenizer};
    use std::sync::Arc;

    fn run(source: &str) -> Result<Val> {
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver = Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);
        program.execute_with_env(&ctx, &mut env)
    }

    #[test]
    fn test_list_len_method_sugar() -> Result<()> {
        let v = run("return [1,2,3].len();")?;
        assert_eq!(v, Val::Int(3));
        Ok(())
    }

    #[test]
    fn test_list_push_join() -> Result<()> {
        let v = run("return [\"a\", \"b\"].push(\"c\").join(\",\");")?;
        assert_eq!(v, Val::Str("a,b,c".into()));
        Ok(())
    }

    #[test]
    fn test_list_get_first_last() -> Result<()> {
        assert_eq!(run("return [10,20,30].get(1);")?, Val::Int(20));
        assert_eq!(run("return [10,20,30].get(5);")?, Val::Nil);
        assert_eq!(run("return [10,20,30].first();")?, Val::Int(10));
        assert_eq!(run("return [10,20,30].last();")?, Val::Int(30));
        Ok(())
    }
}

