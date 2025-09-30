use crate::module::ModuleRegistry;
use crate::stmt::Program;
use crate::stmt::stmt_parser::StmtParser;
use crate::token::Tokenizer;
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
#[derive(Debug, Clone)]
pub struct ModuleResolver {
    /// Standard library registry
    stdlib_registry: std::sync::Arc<ModuleRegistry>,
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
            stdlib_registry: std::sync::Arc::new(registry),
            stdlib_modules: HashMap::new(),
            file_modules: Arc::new(RwLock::new(HashMap::new())),
            // Prefer current directory; also allow `core/` for workspace runs.
            search_paths: vec![PathBuf::from("."), PathBuf::from("core")],
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
            return Ok(Val::from(exports));
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

    /// Resolve a module directly from source code string.
    /// Parses and executes the source in a fresh environment that shares this resolver,
    /// and returns the map of top-level definitions as the module exports.
    pub fn resolve_source(&self, src: &str) -> Result<Val> {
        // Tokenize with spans for better diagnostics
        let (tokens, spans) = Tokenizer::tokenize_enhanced_with_spans(src)
            .map_err(|e| anyhow!(e.to_string()))?;

        // Parse program with enhanced errors
        let mut parser = StmtParser::new_with_spans(&tokens, &spans);
        let program: Program = parser
            .parse_program_with_enhanced_errors(src)
            .map_err(|e| anyhow!(e.to_string()))?;

        // Execute in a fresh environment that shares this resolver
        let resolver = std::sync::Arc::new(self.clone());
        let mut env = crate::stmt::Environment::with_resolver(resolver);
        let _ = program.execute_with_env(&mut env)?;

        // Collect top-level definitions as exports
        let exports = env.export_symbols();
        Ok(Val::from(exports))
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

        // Candidate patterns (searched under each `search_paths` root):
        // 1) ${MOD_NAME}.qcl
        // 2) ${MOD_NAME}/mod.qcl
        // If the input already contains an extension, also allow it directly.
        let base = PathBuf::from(path);

        for root in &self.search_paths {
            // If the input already includes .qcl and exists under this root, accept it
            if base.extension().and_then(|s| s.to_str()) == Some("qcl") {
                let p = root.join(&base);
                if p.exists() {
                    return Ok(p);
                }
            }

            // Try ${MOD_NAME}.qcl
            let candidate1 = root.join(base.with_extension("qcl"));
            if candidate1.exists() {
                return Ok(candidate1);
            }

            // Try ${MOD_NAME}/mod.qcl
            let candidate2 = root.join(base.join("mod.qcl"));
            if candidate2.exists() {
                return Ok(candidate2);
            }
        }

        Err(anyhow!(
            "File not found for module '{}': expected '{}.qcl' or '{}/mod.qcl'",
            path.display(),
            path.display(),
            path.display()
        ))
    }

    /// Load and parse a file module into a namespace map
    fn load_file_module(&self, path: &Path) -> Result<Val> {
        // Read source then delegate to resolve_source
        let src = std::fs::read_to_string(path)?;
        self.resolve_source(&src)
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
                let module_name = Path::new(path).file_stem().and_then(|s| s.to_str()).unwrap_or("module");
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
                            .get(item.name.as_str())
                            .ok_or_else(|| anyhow!("Export '{}' not found in module", item.name))?;

                        let symbol_name = item.alias.as_ref().unwrap_or(&item.name);
                        self.symbols.insert(symbol_name.clone(), export_value.clone());
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

    #[test]
    fn test_resolve_source_basic() -> Result<()> {
        let resolver = ModuleResolver::new();
        let src = r#"
            let answer = 7;
            fn inc(x) { return x + 1; }
            let data = [1, 2, 3];
        "#;
        let module_val = resolver.resolve_source(src)?;

        match module_val {
            Val::Map(map) => {
                assert!(map.contains_key("answer"));
                assert!(map.contains_key("inc"));
                assert!(map.contains_key("data"));
                assert!(matches!(map.get("answer"), Some(Val::Int(7))));
            }
            other => panic!("Expected module map, got {:?}", other),
        }
        Ok(())
    }
}
