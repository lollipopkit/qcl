#[cfg(feature = "stdlib-os")]
use crate::{module::Module, val::Val};
#[cfg(feature = "stdlib-os")]
use anyhow::Result;
#[cfg(feature = "stdlib-os")]
use std::collections::HashMap;

#[cfg(feature = "stdlib-debug")]
#[derive(Debug)]
pub struct DebugModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-debug")]
impl DebugModule {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }
}

#[cfg(feature = "stdlib-debug")]
impl Module for DebugModule {
    fn name(&self) -> &str {
        "debug"
    }

    fn description(&self) -> &str {
        "Debugging utilities"
    }

    fn register(&self, _registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}
