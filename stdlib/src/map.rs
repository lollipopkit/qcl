use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct MapModule {
    functions: HashMap<String, Val>,
}

impl Default for MapModule {
    fn default() -> Self {
        Self::new()
    }
}

impl MapModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Core map utilities
        functions.insert("len".to_string(), Val::RustFunction(Self::len));
        functions.insert("keys".to_string(), Val::RustFunction(Self::keys));
        functions.insert("values".to_string(), Val::RustFunction(Self::values));
        functions.insert("has".to_string(), Val::RustFunction(Self::has));
        functions.insert("get".to_string(), Val::RustFunction(Self::get));

        // Register meta-methods for Map
        qcl_core::val::methods::register_method("Map", "len", Self::len);
        qcl_core::val::methods::register_method("Map", "keys", Self::keys);
        qcl_core::val::methods::register_method("Map", "values", Self::values);
        qcl_core::val::methods::register_method("Map", "has", Self::has);
        qcl_core::val::methods::register_method("Map", "get", Self::get);

        Self { functions }
    }

    fn len(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("len() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::Map(m) => Ok(Val::Int(m.len() as i64)),
            _ => Err(anyhow::anyhow!("len() argument must be a map")),
        }
    }

    fn keys(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("keys() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::Map(m) => {
                let mut out: Vec<Val> = Vec::with_capacity(m.len());
                for k in m.keys() {
                    out.push(Val::Str(k.as_str().into()));
                }
                Ok(Val::List(Arc::new(out)))
            }
            _ => Err(anyhow::anyhow!("keys() argument must be a map")),
        }
    }

    fn values(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("values() takes exactly 1 argument"));
        }
        match &args[0] {
            Val::Map(m) => {
                let mut out: Vec<Val> = Vec::with_capacity(m.len());
                for v in m.values() {
                    out.push(v.clone());
                }
                Ok(Val::List(Arc::new(out)))
            }
            _ => Err(anyhow::anyhow!("values() argument must be a map")),
        }
    }

    fn has(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("has() takes exactly 2 arguments: map, key"));
        }
        let map = match &args[0] {
            Val::Map(m) => &**m,
            _ => return Err(anyhow::anyhow!("has() first argument must be a map")),
        };
        let key = match &args[1] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("has() key must be a string")),
        };
        Ok(Val::Bool(map.contains_key(key)))
    }

    fn get(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("get() takes exactly 2 arguments: map, key"));
        }
        let map = match &args[0] {
            Val::Map(m) => &**m,
            _ => return Err(anyhow::anyhow!("get() first argument must be a map")),
        };
        let key = match &args[1] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("get() key must be a string")),
        };
        Ok(map.get(key).cloned().unwrap_or(Val::Nil))
    }
}

impl Module for MapModule {
    fn name(&self) -> &str {
        "map"
    }

    fn description(&self) -> &str {
        "Map utilities and meta-methods"
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
        let ctx = std::collections::HashMap::<String, Val>::new().into();

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver = Arc::new(qcl_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);
        program.execute_with_env(&ctx, &mut env)
    }

    #[test]
    fn test_map_len_keys_values_has_get() -> Result<()> {
        // len
        assert_eq!(run("return {\"a\":1, \"b\":2}.len();")?, Val::Int(2));
        // keys/values
        let keys = run("let m={\"a\":1, \"b\":2}; let ks = m.keys().join(\",\"); return ks;")?;
        // Order is not guaranteed; check either order
        match keys {
            Val::Str(s) if s.as_ref() == "a,b" || s.as_ref() == "b,a" => {}
            _ => panic!("unexpected keys output: {}", keys),
        }
        // has/get
        assert_eq!(
            run("let m={\"a\":1}; return m.has(\"a\");")?,
            Val::Bool(true)
        );
        assert_eq!(
            run("let m={\"a\":1}; return m.has(\"b\");")?,
            Val::Bool(false)
        );
        assert_eq!(run("let m={\"a\":1}; return m.get(\"a\");")?, Val::Int(1));
        assert_eq!(run("let m={\"a\":1}; return m.get(\"b\");")?, Val::Nil);
        Ok(())
    }
}
