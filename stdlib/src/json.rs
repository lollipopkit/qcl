use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::de;
use qcl_core::val::Val;
use std::collections::HashMap;

#[derive(Debug)]
pub struct JsonModule {
    functions: HashMap<String, Val>,
}

impl Default for JsonModule {
    fn default() -> Self {
        Self::new()
    }
}

impl JsonModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("parse".to_string(), Val::RustFunction(parse));
        JsonModule { functions }
    }
}

impl Module for JsonModule {
    fn name(&self) -> &str {
        "json"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

fn parse(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("json.parse(data) requires 1 argument"));
    }
    let s: String = match &args[0] {
        Val::Str(s) => s.as_ref().to_string(),
        v => v.to_string(),
    };
    de::parse_with_format(&s, Some(de::Format::Json))
}

