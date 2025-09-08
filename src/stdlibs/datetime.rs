use std::collections::HashMap;
use crate::val::Val;
use crate::module::Module;
use anyhow::Result;

#[cfg(feature = "stdlib-datetime")]
#[derive(Debug)]
pub struct DateTimeModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-datetime")]
impl DateTimeModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        
        // Register datetime functions as Rust functions
        functions.insert("now".to_string(), Val::RustFunction(Self::now));
        
        Self { functions }
    }
    
    /// Get current timestamp as Unix epoch
    fn now(args: &[Val], _env: &crate::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if !args.is_empty() {
            return Err(anyhow::anyhow!("now() takes no arguments"));
        }
        
        use chrono::{DateTime, Utc};
        let now: DateTime<Utc> = Utc::now();
        let timestamp = now.timestamp();
        Ok(Val::Int(timestamp))
    }
}

#[cfg(feature = "stdlib-datetime")]
impl Module for DateTimeModule {
    fn name(&self) -> &str {
        "datetime"
    }
    
    fn description(&self) -> &str {
        "Date and time functions"
    }
    
    fn register(&self, _registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }
    
    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}