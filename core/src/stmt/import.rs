use crate::module::ModuleRegistry;
use crate::val::Val;
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Import system for QCL - supports various import syntaxes and plugin-style module resolution
///
/// Supported import syntaxes:
/// 1. `import math;` - imports stdlib module 'math' with all exports
/// 2. `import "path/to/file.qcl";` - imports file with all exports  
/// 3. `import { abs, sqrt } from math;` - imports specific items from stdlib module
/// 4. `import { func as alias } from "file.qcl";` - imports with alias
/// 5. `import * as math from math;` - imports all as namespace
/// 6. `import math as m;` - imports entire module with alias
///
/// Import statement variants
#[derive(Debug, Clone, PartialEq)]
pub enum ImportStmt {
    /// `import module;` - import entire module
    Module { module: String },
    /// `import "path";` - import from file path
    File { path: String },
    /// `import { items } from source;` - import specific items
    Items {
        items: Vec<ImportItem>,
        source: ImportSource,
    },
    /// `import * as alias from source;` - import all as namespace
    Namespace { alias: String, source: ImportSource },
    /// `import module as alias;` - import module with alias
    ModuleAlias { module: String, alias: String },
}

/// Import source - either stdlib module or file path
#[derive(Debug, Clone, PartialEq)]
pub enum ImportSource {
    Module(String),
    File(String),
}

/// Individual import item with optional alias
#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

// Note: The Module trait and related functionality have been moved to module.rs
// This file now provides compatibility layer and file-based import functionality

/// Module resolver - handles finding and loading modules
#[derive(Debug)]
pub struct ModuleResolver {
    /// Standard library registry
    stdlib_registry: ModuleRegistry,
    /// Standard library modules cache
    stdlib_modules: HashMap<String, Val>,
    /// Loaded file modules (path -> module)
    file_modules: Arc<RwLock<HashMap<PathBuf, Val>>>,
    /// Search paths for module resolution
    search_paths: Vec<PathBuf>,
}

impl PartialEq for ModuleResolver {
    fn eq(&self, other: &Self) -> bool {
        // Compare only registry and search paths, ignoring caches
        self.stdlib_registry == other.stdlib_registry && self.search_paths == other.search_paths
    }
}

impl ModuleResolver {
    pub fn new() -> Self {
        Self::with_registry(ModuleRegistry::new())
    }

    /// Create a new resolver with a specific module registry
    pub fn with_registry(registry: ModuleRegistry) -> Self {
        Self {
            stdlib_registry: registry,
            stdlib_modules: HashMap::new(),
            file_modules: Arc::new(RwLock::new(HashMap::new())),
            search_paths: vec![
                PathBuf::from("."),         // Current directory
                PathBuf::from("./lib"),     // Local lib directory
                PathBuf::from("./modules"), // Local modules directory
            ],
        }
    }

    /// Get a globally registered builtin function (if any)
    pub fn get_builtin(&self, name: &str) -> Option<&Val> {
        self.stdlib_registry.get_builtin(name)
    }

    /// Add a search path for file resolution
    pub fn add_search_path(&mut self, path: impl Into<PathBuf>) {
        self.search_paths.push(path.into());
    }

    /// Resolve a module by name (stdlib modules)
    pub fn resolve_module(&self, name: &str) -> Result<Val> {
        // Try to get from stdlib registry first
        if let Ok(module) = self.stdlib_registry.get_module(name) {
            let exports = module.exports();
            return Ok(Val::Map(exports.into()));
        }

        // Check cache
        self.stdlib_modules
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("Module '{}' not found", name))
    }

    /// Resolve a file module - loads if not already cached
    pub fn resolve_file(&self, path: &str) -> Result<Val> {
        let resolved_path = self.resolve_file_path(path)?;

        // Check cache first
        if let Ok(cache) = self.file_modules.read()
            && let Some(module) = cache.get(&resolved_path)
        {
            return Ok(module.clone());
        }

        // Load and parse the file
        let module = self.load_file_module(&resolved_path)?;

        // Cache the loaded module
        if let Ok(mut cache) = self.file_modules.write() {
            cache.insert(resolved_path, module.clone());
        }

        Ok(module)
    }

    /// Resolve file path using search paths
    fn resolve_file_path(&self, path: &str) -> Result<PathBuf> {
        let path = Path::new(path);

        // Enforce security: only allow relative, sanitized paths (no absolute, no `..`).
        if !path.is_relative() {
            return Err(anyhow!(
                "Absolute paths are not allowed for imports: {}",
                path.display()
            ));
        }

        if path.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
            return Err(anyhow!(
                "Parent directory components ('..') are not allowed in imports: {}",
                path.display()
            ));
        }

        // Search in search paths
        for search_path in &self.search_paths {
            let full_path = search_path.join(path);
            if full_path.exists() {
                return Ok(full_path);
            }

            // Also try with .qcl extension
            let with_ext = full_path.with_extension("qcl");
            if with_ext.exists() {
                return Ok(with_ext);
            }
        }

        Err(anyhow!(
            "File not found: {} (searched in {:?})",
            path.display(),
            self.search_paths
        ))
    }

    /// Load and parse a file module (placeholder - will integrate with parser)
    fn load_file_module(&self, _path: &Path) -> Result<Val> {
        // TODO: Implement file parsing and module extraction
        // For now, return empty module as a map
        let empty_map = std::collections::HashMap::new();
        Ok(Val::Map(empty_map.into()))
    }
}

impl Default for ModuleResolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Import context - manages imported symbols in current scope
#[derive(Debug, Clone, PartialEq)]
pub struct ImportContext {
    /// Imported symbols: name -> value
    symbols: HashMap<String, Val>,
}

impl ImportContext {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    /// Execute an import statement
    pub fn execute_import(&mut self, import: &ImportStmt, resolver: &ModuleResolver) -> Result<()> {
        match import {
            ImportStmt::Module { module } => {
                let mod_def = resolver.resolve_module(module)?;
                // Import module as namespace - don't pollute global scope
                self.symbols.insert(module.clone(), mod_def);
            }
            ImportStmt::File { path } => {
                let mod_def = resolver.resolve_file(path)?;
                // Import file module as namespace using filename (without extension)
                let module_name = Path::new(path)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("module");
                self.symbols.insert(module_name.to_string(), mod_def);
            }
            ImportStmt::Items { items, source } => {
                let mod_def = match source {
                    ImportSource::Module(name) => resolver.resolve_module(name)?,
                    ImportSource::File(path) => resolver.resolve_file(path)?,
                };

                if let Val::Map(exports) = mod_def {
                    for item in items {
                        let export_value = exports
                            .get(&item.name)
                            .ok_or_else(|| anyhow!("Export '{}' not found in module", item.name))?;

                        let symbol_name = item.alias.as_ref().unwrap_or(&item.name);
                        self.symbols
                            .insert(symbol_name.clone(), export_value.clone());
                    }
                }
            }
            ImportStmt::Namespace { alias, source } => {
                let mod_def = match source {
                    ImportSource::Module(name) => resolver.resolve_module(name)?,
                    ImportSource::File(path) => resolver.resolve_file(path)?,
                };

                // The module is already a map, so we can use it directly
                self.symbols.insert(alias.clone(), mod_def);
            }
            ImportStmt::ModuleAlias { module, alias } => {
                let mod_def = resolver.resolve_module(module)?;
                // The module is already a map, so we can use it directly
                self.symbols.insert(alias.clone(), mod_def);
            }
        }
        Ok(())
    }

    /// Get imported symbol
    pub fn get_symbol(&self, name: &str) -> Option<&Val> {
        self.symbols.get(name)
    }

    /// Check if symbol exists
    pub fn has_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Get all symbols
    pub fn get_all_symbols(&self) -> &HashMap<String, Val> {
        &self.symbols
    }
}

impl Default for ImportContext {
    fn default() -> Self {
        Self::new()
    }
}

// Standard library module implementations have been moved to module.rs

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_import_stmt_variants() {
        let import = ImportStmt::Module {
            module: "math".to_string(),
        };
        assert!(matches!(import, ImportStmt::Module { .. }));

        let import = ImportStmt::Items {
            items: vec![ImportItem {
                name: "abs".to_string(),
                alias: None,
            }],
            source: ImportSource::Module("math".to_string()),
        };
        assert!(matches!(import, ImportStmt::Items { .. }));
    }

    #[test]
    fn test_module_resolver() {
        let resolver = ModuleResolver::new();

        // Test that nonexistent modules fail
        assert!(resolver.resolve_module("nonexistent").is_err());

        // Note: stdlib modules are now registered externally
        // The resolver starts with an empty registry
    }

    #[test]
    fn test_import_context() {
        let mut ctx = ImportContext::new();
        let resolver = ModuleResolver::new();

        let import = ImportStmt::Module {
            module: "nonexistent".to_string(),
        };

        // Test that nonexistent modules fail
        let result = ctx.execute_import(&import, &resolver);
        assert!(result.is_err());
    }

    #[test]
    fn test_resolve_file_path_security() {
        let resolver = ModuleResolver::new();

        // Absolute paths are rejected
        let abs = std::env::current_dir().unwrap();
        let abs_str = abs.to_string_lossy().to_string();
        assert!(resolver.resolve_file_path(&abs_str).is_err());

        // Parent directory components are rejected
        assert!(resolver.resolve_file_path("../foo.qcl").is_err());

        // Relative simple path that likely does not exist should return not found
        // (error message still OK but not due to security check)
        let rel = PathBuf::from("does_not_exist.qcl");
        assert!(resolver.resolve_file_path(&rel.to_string_lossy()).is_err());
    }
}
