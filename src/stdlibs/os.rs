#[cfg(feature = "stdlib-os")]
use crate::{module::Module, val::Val};
#[cfg(feature = "stdlib-os")]
use anyhow::Result;
#[cfg(feature = "stdlib-os")]
use std::collections::HashMap;

#[cfg(feature = "stdlib-os")]
#[derive(Debug)]
pub struct OsModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-os")]
impl OsModule {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }
}

#[cfg(feature = "stdlib-os")]
impl Module for OsModule {
    fn name(&self) -> &str {
        "os"
    }

    fn description(&self) -> &str {
        "Operating system interface"
    }

    fn register(&self, _registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}
