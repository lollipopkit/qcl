use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::Val;
use qcl_core::val::de;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TomlModule {
    functions: HashMap<String, Val>,
}

impl Default for TomlModule {
    fn default() -> Self {
        Self::new()
    }
}

impl TomlModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("parse".to_string(), Val::RustFunction(parse));
        TomlModule { functions }
    }
}

impl Module for TomlModule {
    fn name(&self) -> &str {
        "toml"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

fn parse(args: &[Val], _env: &qcl_core::stmt::Environment) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("toml.parse(data) requires 1 argument"));
    }
    let s: String = match &args[0] {
        Val::Str(s) => s.as_ref().to_string(),
        v => v.to_string(),
    };
    de::parse_with_format(&s, Some(de::Format::Toml))
}
