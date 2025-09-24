use anyhow::{Result, anyhow};
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;

#[derive(Debug)]
pub struct IterModule {
    functions: HashMap<String, Val>,
}

impl Default for IterModule {
    fn default() -> Self {
        Self::new()
    }
}

impl IterModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Register iterator functions as Rust functions
        functions.insert("enumerate".to_string(), Val::RustFunction(enumerate));
        functions.insert("range".to_string(), Val::RustFunction(range));

        Self { functions }
    }
}

impl Module for IterModule {
    fn name(&self) -> &'static str {
        "iter"
    }

    fn description(&self) -> &'static str {
        "Iterator utilities and functions for working with collections"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

/// enumerate - 为序列添加索引
/// enumerate([1, 2, 3]) => [[0, 1], [1, 2], [2, 3]]
pub fn enumerate(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("enumerate expects 1 argument, got {}", args.len()));
    }

    match &args[0] {
        Val::List(list) => {
            let enumerated: Vec<Val> = list
                .iter()
                .enumerate()
                .map(|(i, v)| Val::List(vec![Val::Int(i as i64), v.clone()].into()))
                .collect();
            Ok(Val::List(enumerated.into()))
        }
        _ => Err(anyhow!("enumerate expects a list, got {:?}", args[0])),
    }
}

/// range - 生成整数范围
/// range(5) => [0, 1, 2, 3, 4]
/// range(2, 5) => [2, 3, 4]
/// range(0, 10, 2) => [0, 2, 4, 6, 8]
pub fn range(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    let (start, end, step) = match args.len() {
        1 => (0, extract_int(&args[0])?, 1),
        2 => (extract_int(&args[0])?, extract_int(&args[1])?, 1),
        3 => (
            extract_int(&args[0])?,
            extract_int(&args[1])?,
            extract_int(&args[2])?,
        ),
        _ => return Err(anyhow!("range expects 1-3 arguments, got {}", args.len())),
    };

    if step == 0 {
        return Err(anyhow!("range step cannot be zero"));
    }

    let mut result = Vec::new();
    let mut current = start;

    if step > 0 {
        while current < end {
            result.push(Val::Int(current));
            current += step;
        }
    } else if step < 0 {
        while current > end {
            result.push(Val::Int(current));
            current += step;
        }
    }

    Ok(Val::List(result.into()))
}

/// Helper function to extract integer from Val
fn extract_int(val: &Val) -> Result<i64> {
    match val {
        Val::Int(i) => Ok(*i),
        _ => Err(anyhow!("Expected integer, got {:?}", val)),
    }
}
