use std::collections::HashMap;
use crate::val::Val;
use crate::module::Module;
use anyhow::Result;
use std::sync::Arc;

#[cfg(feature = "stdlib-math")]
#[derive(Debug)]
pub struct MathModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-math")]
impl MathModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        
        // Register math functions as Maps
        functions.insert("abs".to_string(), Self::create_function_map("math.abs"));
        functions.insert("sqrt".to_string(), Self::create_function_map("math.sqrt"));
        
        // Add more math functions as needed
        
        Self { functions }
    }
    
    fn create_function_map(name: &str) -> Val {
        let mut map = HashMap::new();
        map.insert("__type".to_string(), Val::Str("function".into()));
        map.insert("name".to_string(), Val::Str(name.into()));
        Val::Map(Arc::new(map))
    }
}

#[cfg(feature = "stdlib-math")]
impl Module for MathModule {
    fn name(&self) -> &str {
        "math"
    }
    
    fn description(&self) -> &str {
        "Mathematical functions and constants"
    }
    
    fn register(&self, _registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }
    
    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}