use std::collections::HashMap;
use crate::val::Val;
use crate::module::Module;
use anyhow::Result;
use std::sync::Arc;

#[cfg(feature = "stdlib-string")]
#[derive(Debug)]
pub struct StringModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-string")]
impl StringModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        
        // Register string functions as Maps
        functions.insert("len".to_string(), Self::create_function_map("string.len"));
        
        // Add more string functions as needed
        
        Self { functions }
    }
    
    fn create_function_map(name: &str) -> Val {
        let mut map = HashMap::new();
        map.insert("__type".to_string(), Val::Str("function".into()));
        map.insert("name".to_string(), Val::Str(name.into()));
        Val::Map(Arc::new(map))
    }
}

#[cfg(feature = "stdlib-string")]
impl Module for StringModule {
    fn name(&self) -> &str {
        "string"
    }
    
    fn description(&self) -> &str {
        "String manipulation functions"
    }
    
    fn register(&self, _registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }
    
    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}