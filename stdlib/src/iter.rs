use anyhow::{Result, anyhow};
use lkr_core::module::Module;
use lkr_core::val::Val;
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
        functions.insert("zip".to_string(), Val::RustFunction(zip));
        functions.insert("take".to_string(), Val::RustFunction(take));
        functions.insert("skip".to_string(), Val::RustFunction(skip));
        functions.insert("chain".to_string(), Val::RustFunction(chain));
        functions.insert("flatten".to_string(), Val::RustFunction(flatten));
        functions.insert("unique".to_string(), Val::RustFunction(unique));
        functions.insert("chunk".to_string(), Val::RustFunction(chunk));

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

    fn register(&self, _registry: &mut lkr_core::module::ModuleRegistry) -> Result<()> {
        // Don't register functions globally - they should be accessed via module.function()
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

/// enumerate - 为序列添加索引
/// enumerate([1, 2, 3]) => [[0, 1], [1, 2], [2, 3]]
pub fn enumerate(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
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
pub fn range(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    let (start, end, step) = match args.len() {
        1 => (0, extract_int(&args[0])?, 1),
        2 => (extract_int(&args[0])?, extract_int(&args[1])?, 1),
        3 => (extract_int(&args[0])?, extract_int(&args[1])?, extract_int(&args[2])?),
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

/// zip - pair elements from two lists by index
/// zip([1,2], ["a","b","c"]) => [[1,"a"], [2,"b"]]
pub fn zip(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("zip expects 2 arguments: list1, list2"));
    }
    let a = match &args[0] {
        Val::List(l) => l,
        _ => return Err(anyhow!("zip first argument must be a list")),
    };
    let b = match &args[1] {
        Val::List(l) => l,
        _ => return Err(anyhow!("zip second argument must be a list")),
    };
    let len = std::cmp::min(a.len(), b.len());
    let mut out = Vec::with_capacity(len);
    for i in 0..len {
        out.push(Val::List(vec![a[i].clone(), b[i].clone()].into()));
    }
    Ok(Val::List(out.into()))
}

/// take - take first n elements from list
pub fn take(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("take expects 2 arguments: list, n"));
    }
    let list = match &args[0] {
        Val::List(l) => l,
        _ => return Err(anyhow!("take first argument must be a list")),
    };
    let n = extract_int(&args[1])?;
    if n <= 0 {
        return Ok(Val::List(Vec::<Val>::new().into()));
    }
    let end = std::cmp::min(list.len(), n as usize);
    Ok(Val::List(list[0..end].to_vec().into()))
}

/// skip - skip first n elements from list
pub fn skip(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("skip expects 2 arguments: list, n"));
    }
    let list = match &args[0] {
        Val::List(l) => l,
        _ => return Err(anyhow!("skip first argument must be a list")),
    };
    let n = extract_int(&args[1])?;
    if n <= 0 {
        return Ok(Val::List(list.clone()));
    }
    let start = std::cmp::min(list.len(), n as usize);
    Ok(Val::List(list[start..].to_vec().into()))
}

/// chain - concatenate two lists
pub fn chain(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("chain expects 2 arguments: list1, list2"));
    }
    let a = match &args[0] {
        Val::List(l) => l,
        _ => return Err(anyhow!("chain first argument must be a list")),
    };
    let b = match &args[1] {
        Val::List(l) => l,
        _ => return Err(anyhow!("chain second argument must be a list")),
    };
    let mut out = Vec::with_capacity(a.len() + b.len());
    out.extend(a.iter().cloned());
    out.extend(b.iter().cloned());
    Ok(Val::List(out.into()))
}

/// flatten - flatten one level of nesting in a list
pub fn flatten(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("flatten expects 1 argument: list"));
    }
    let list = match &args[0] {
        Val::List(l) => l,
        _ => return Err(anyhow!("flatten argument must be a list")),
    };
    let mut out: Vec<Val> = Vec::new();
    for item in list.iter() {
        match item {
            Val::List(inner) => out.extend(inner.iter().cloned()),
            other => out.push(other.clone()),
        }
    }
    Ok(Val::List(out.into()))
}

/// unique - remove duplicates (O(n^2), stable)
pub fn unique(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("unique expects 1 argument: list"));
    }
    let list = match &args[0] {
        Val::List(l) => &**l,
        _ => return Err(anyhow!("unique argument must be a list")),
    };
    let mut out: Vec<Val> = Vec::with_capacity(list.len());
    'outer: for v in list.iter() {
        for seen in out.iter() {
            if seen == v {
                continue 'outer;
            }
        }
        out.push(v.clone());
    }
    Ok(Val::List(out.into()))
}

/// chunk - split list into chunks of given positive size
pub fn chunk(args: &[Val], _env: &lkr_core::stmt::Environment) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("chunk expects 2 arguments: list, size"));
    }
    let list = match &args[0] {
        Val::List(l) => &**l,
        _ => return Err(anyhow!("chunk first argument must be a list")),
    };
    let size = extract_int(&args[1])?;
    if size <= 0 {
        return Err(anyhow!("chunk size must be positive"));
    }
    let size = size as usize;
    let mut out: Vec<Val> = Vec::new();
    let mut i = 0usize;
    while i < list.len() {
        let end = std::cmp::min(i + size, list.len());
        out.push(Val::List(list[i..end].to_vec().into()));
        i = end;
    }
    Ok(Val::List(out.into()))
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::register_stdlib_modules;
    use anyhow::Result;
    use lkr_core::{stmt::stmt_parser::StmtParser, token::Tokenizer};
    use std::sync::Arc;

    fn run(source: &str) -> Result<Val> {
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;

        let mut registry = lkr_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver = std::sync::Arc::new(lkr_core::stmt::ModuleResolver::with_registry(registry));
        let mut env = lkr_core::stmt::Environment::with_resolver(resolver);
        program.execute_with_env(&mut env)
    }

    #[test]
    fn test_iter_zip() -> Result<()> {
        let v = run("import iter; return iter.zip([1,2], [\"a\",\"b\",\"c\"]);")?;
        assert_eq!(
            v,
            Val::List(Arc::from(vec![
                Val::List(vec![Val::Int(1), Val::Str("a".into())].into()),
                Val::List(vec![Val::Int(2), Val::Str("b".into())].into()),
            ]))
        );
        Ok(())
    }

    #[test]
    fn test_iter_take_skip_chain_flatten_unique_chunk() -> Result<()> {
        // take
        assert_eq!(
            run("import iter; return iter.take([1,2,3,4], 2);")?,
            Val::List(Arc::from(vec![Val::Int(1), Val::Int(2)]))
        );
        assert_eq!(
            run("import iter; return iter.take([1,2], 0);")?,
            Val::List(Arc::from(vec![]))
        );
        // skip
        assert_eq!(
            run("import iter; return iter.skip([1,2,3,4], 2);")?,
            Val::List(Arc::from(vec![Val::Int(3), Val::Int(4)]))
        );
        assert_eq!(
            run("import iter; return iter.skip([1,2], 10);")?,
            Val::List(Arc::from(vec![]))
        );
        // chain
        assert_eq!(
            run("import iter; return iter.chain([1,2], [3,4]);")?,
            Val::List(Arc::from(vec![Val::Int(1), Val::Int(2), Val::Int(3), Val::Int(4)]))
        );
        // flatten (one level)
        assert_eq!(
            run("import iter; return iter.flatten([[1,2],[3],4]);")?,
            Val::List(Arc::from(vec![Val::Int(1), Val::Int(2), Val::Int(3), Val::Int(4)]))
        );
        // unique (stable)
        assert_eq!(
            run("import iter; return iter.unique([1,2,1,3,2]);")?,
            Val::List(Arc::from(vec![Val::Int(1), Val::Int(2), Val::Int(3)]))
        );
        // chunk
        let chunks = run("import iter; return iter.chunk([1,2,3,4,5], 2);")?;
        match chunks {
            Val::List(l) => {
                assert_eq!(l.len(), 3);
            }
            _ => panic!("expected list of chunks"),
        }
        Ok(())
    }
}
