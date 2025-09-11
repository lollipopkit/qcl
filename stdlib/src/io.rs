use anyhow::Result;
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;
use std::io::{BufRead, Read, Write};
use std::sync::Arc;
use qcl_core::concurrency::Channel;
use std::thread;

fn make_stdin_object() -> Val {
    let mut methods = HashMap::new();
    methods.insert("read".to_string(), Val::RustFunction(stdin_read));
    methods.insert("read_line".to_string(), Val::RustFunction(stdin_read_line));
    methods.insert("read_all".to_string(), Val::RustFunction(stdin_read_all));
    methods.insert("lines".to_string(), Val::RustFunction(stdin_lines));
    methods.insert("read_stream".to_string(), Val::RustFunction(stdin_read_stream));
    methods.insert(
        "read_async".to_string(),
        Val::RustFunction(stdin_read_async),
    );
    methods.insert(
        "read_line_async".to_string(),
        Val::RustFunction(stdin_read_line_async),
    );
    // stdin flush is a no-op for convenience; returns true
    methods.insert("flush".to_string(), Val::RustFunction(stdin_flush));
    Val::Map(Arc::new(methods))
}

fn make_stdout_object() -> Val {
    let mut methods = HashMap::new();
    methods.insert("write".to_string(), Val::RustFunction(stdout_write));
    methods.insert("writeln".to_string(), Val::RustFunction(stdout_writeln));
    methods.insert("flush".to_string(), Val::RustFunction(stdout_flush));
    methods.insert("write_stream".to_string(), Val::RustFunction(stdout_write_stream));
    Val::Map(Arc::new(methods))
}

fn make_stderr_object() -> Val {
    let mut methods = HashMap::new();
    methods.insert("write".to_string(), Val::RustFunction(stderr_write));
    methods.insert("writeln".to_string(), Val::RustFunction(stderr_writeln));
    methods.insert("flush".to_string(), Val::RustFunction(stderr_flush));
    methods.insert("write_stream".to_string(), Val::RustFunction(stderr_write_stream));
    Val::Map(Arc::new(methods))
}

fn stdin_read(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() > 1 {
        return Err(anyhow::anyhow!(
            "stdin.read() takes at most 1 argument: [bytes]"
        ));
    }

    let mut handle = std::io::stdin().lock();
    if args.is_empty() {
        // default: read a single line
        let mut line = String::new();
        match handle.read_line(&mut line) {
            Ok(0) => Ok(Val::Nil), // EOF
            Ok(_) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(Val::Str(line.into()))
            }
            Err(e) => Err(anyhow::anyhow!("stdin read error: {}", e)),
        }
    } else {
        let n = match &args[0] {
            Val::Int(i) if *i >= 0 => *i as usize,
            _ => return Err(anyhow::anyhow!("bytes must be a non-negative integer")),
        };
        if n == 0 {
            return Ok(Val::Str("".into()));
        }
        let mut buf = vec![0u8; n];
        match handle.read(&mut buf) {
            Ok(0) => Ok(Val::Nil),
            Ok(read) => {
                buf.truncate(read);
                match String::from_utf8(buf) {
                    Ok(s) => Ok(Val::Str(s.into())),
                    Err(_) => Ok(Val::Nil),
                }
            }
            Err(e) => Err(anyhow::anyhow!("stdin read error: {}", e)),
        }
    }
}

fn stdin_read_line(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!("stdin.read_line() takes no arguments"));
    }
    stdin_read(&[], _env, _ctx)
}

fn stdin_flush(_args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    // No-op; included for API symmetry. Return true for convenience.
    Ok(Val::Bool(true))
}

fn stdin_read_all(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!("stdin.read_all() takes no arguments"));
    }
    let mut s = String::new();
    let res = std::io::stdin().lock().read_to_string(&mut s);
    match res {
        Ok(_) => Ok(Val::Str(s.into())),
        Err(e) => Err(anyhow::anyhow!("stdin read error: {}", e)),
    }
}

fn stdin_lines(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!("stdin.lines() takes no arguments"));
    }
    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        loop {
            let mut line = String::new();
            match handle.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => {
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    if ch_clone.send(Val::Str(line.into())).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        let _ = ch_clone.send(Val::Nil);
    });
    Ok(Val::Channel(ch))
}

fn stdin_read_stream(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if args.len() > 1 {
        return Err(anyhow::anyhow!(
            "stdin.read_stream() takes at most 1 argument: [buffer_size]"
        ));
    }
    let buffer_size = if args.is_empty() {
        1024usize
    } else {
        match &args[0] {
            Val::Int(n) if *n > 0 => *n as usize,
            _ => return Err(anyhow::anyhow!("buffer_size must be a positive integer")),
        }
    };
    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        loop {
            let mut buf = vec![0u8; buffer_size];
            match handle.read(&mut buf) {
                Ok(0) => break,
                Ok(read) => {
                    buf.truncate(read);
                    if let Ok(s) = String::from_utf8(buf) {
                        if ch_clone.send(Val::Str(s.into())).is_err() {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        let _ = ch_clone.send(Val::Nil);
    });
    Ok(Val::Channel(ch))
}

fn stdin_read_async(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if args.len() > 1 {
        return Err(anyhow::anyhow!(
            "stdin.read_async() takes at most 1 argument: [bytes]"
        ));
    }
    let mode_bytes = if args.is_empty() {
        None
    } else {
        match &args[0] {
            Val::Int(n) if *n >= 0 => Some(*n as usize),
            _ => return Err(anyhow::anyhow!("bytes must be a non-negative integer")),
        }
    };
    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        let result = if let Some(n) = mode_bytes {
            if n == 0 {
                Val::Str("".into())
            } else {
                let mut buf = vec![0u8; n];
                match handle.read(&mut buf) {
                    Ok(0) => Val::Nil,
                    Ok(read) => {
                        buf.truncate(read);
                        String::from_utf8(buf).map(|s| Val::Str(s.into())).unwrap_or(Val::Nil)
                    }
                    Err(_) => Val::Nil,
                }
            }
        } else {
            let mut line = String::new();
            match handle.read_line(&mut line) {
                Ok(0) => Val::Nil,
                Ok(_) => {
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    Val::Str(line.into())
                }
                Err(_) => Val::Nil,
            }
        };
        let _ = ch_clone.send(result);
        let _ = ch_clone.send(Val::Nil);
    });
    Ok(Val::Channel(ch))
}

fn stdin_read_line_async(
    args: &[Val],
    env: &qcl_core::stmt::Environment,
    ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!("stdin.read_line_async() takes no arguments"));
    }
    stdin_read_async(&[], env, ctx)
}

fn stdout_write(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("stdout.write() takes exactly 1 argument: string"));
    }
    let s = match &args[0] {
        Val::Str(s) => &**s,
        _ => return Err(anyhow::anyhow!("argument must be a string")),
    };
    let bytes = s.as_bytes();
    match std::io::stdout().lock().write_all(bytes) {
        Ok(_) => Ok(Val::Int(bytes.len() as i64)),
        Err(e) => Err(anyhow::anyhow!("stdout write error: {}", e)),
    }
}

fn stdout_writeln(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("stdout.writeln() takes exactly 1 argument: string"));
    }
    let s = match &args[0] {
        Val::Str(s) => &**s,
        _ => return Err(anyhow::anyhow!("argument must be a string")),
    };
    let mut handle = std::io::stdout().lock();
    handle
        .write_all(s.as_bytes())
        .and_then(|_| handle.write_all(b"\n"))
        .map_err(|e| anyhow::anyhow!("stdout write error: {}", e))?;
    Ok(Val::Int(s.as_bytes().len() as i64))
}

fn stdout_flush(_args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    match std::io::stdout().lock().flush() {
        Ok(_) => Ok(Val::Bool(true)),
        Err(e) => Err(anyhow::anyhow!("stdout flush error: {}", e)),
    }
}

fn stderr_write(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("stderr.write() takes exactly 1 argument: string"));
    }
    let s = match &args[0] {
        Val::Str(s) => &**s,
        _ => return Err(anyhow::anyhow!("argument must be a string")),
    };
    let bytes = s.as_bytes();
    match std::io::stderr().lock().write_all(bytes) {
        Ok(_) => Ok(Val::Int(bytes.len() as i64)),
        Err(e) => Err(anyhow::anyhow!("stderr write error: {}", e)),
    }
}

fn stderr_writeln(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("stderr.writeln() takes exactly 1 argument: string"));
    }
    let s = match &args[0] {
        Val::Str(s) => &**s,
        _ => return Err(anyhow::anyhow!("argument must be a string")),
    };
    let mut handle = std::io::stderr().lock();
    handle
        .write_all(s.as_bytes())
        .and_then(|_| handle.write_all(b"\n"))
        .map_err(|e| anyhow::anyhow!("stderr write error: {}", e))?;
    Ok(Val::Int(s.as_bytes().len() as i64))
}

fn stderr_flush(_args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    match std::io::stderr().lock().flush() {
        Ok(_) => Ok(Val::Bool(true)),
        Err(e) => Err(anyhow::anyhow!("stderr flush error: {}", e)),
    }
}

fn stdout_write_stream(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!(
            "stdout.write_stream() takes no arguments; send strings and end with nil"
        ));
    }

    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        while let Ok(v) = ch_clone.recv() {
            match v {
                Val::Str(s) => {
                    // Lock only per write to avoid blocking global stdout
                    if std::io::stdout().lock().write_all(s.as_bytes()).is_err() {
                        break;
                    }
                }
                Val::Nil => break,
                _ => break,
            }
        }
    });
    Ok(Val::Channel(ch))
}

fn stderr_write_stream(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if !args.is_empty() {
        return Err(anyhow::anyhow!(
            "stderr.write_stream() takes no arguments; send strings and end with nil"
        ));
    }

    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        while let Ok(v) = ch_clone.recv() {
            match v {
                Val::Str(s) => {
                    // Lock only per write to avoid blocking global stderr
                    if std::io::stderr().lock().write_all(s.as_bytes()).is_err() {
                        break;
                    }
                }
                Val::Nil => break,
                _ => break,
            }
        }
    });
    Ok(Val::Channel(ch))
}

fn io_read(args: &[Val], env: &qcl_core::stmt::Environment, ctx: &Val) -> Result<Val> {
    // Delegate to stdin.read for shared behavior
    stdin_read(args, env, ctx)
}

fn io_write(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    // Delegate to stdout.write
    stdout_write(args, _env, _ctx)
}

fn io_read_all(args: &[Val], env: &qcl_core::stmt::Environment, ctx: &Val) -> Result<Val> {
    stdin_read_all(args, env, ctx)
}

fn io_lines(args: &[Val], env: &qcl_core::stmt::Environment, ctx: &Val) -> Result<Val> {
    stdin_lines(args, env, ctx)
}

fn io_read_stream(args: &[Val], env: &qcl_core::stmt::Environment, ctx: &Val) -> Result<Val> {
    stdin_read_stream(args, env, ctx)
}

fn io_write_stream(
    args: &[Val],
    env: &qcl_core::stmt::Environment,
    ctx: &Val,
) -> Result<Val> {
    stdout_write_stream(args, env, ctx)
}

fn io_timeout(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("timeout() takes exactly 1 argument: milliseconds"));
    }
    let ms = match &args[0] {
        Val::Int(n) if *n >= 0 => *n as u64,
        _ => return Err(anyhow::anyhow!("milliseconds must be a non-negative integer")),
    };
    let ch = Channel::new();
    let ch_clone = ch.clone();
    thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(ms));
        let _ = ch_clone.send(Val::Bool(true));
        let _ = ch_clone.send(Val::Nil);
    });
    Ok(Val::Channel(ch))
}

#[derive(Debug)]
pub struct IoModule {
    functions: HashMap<String, Val>,
}

impl Default for IoModule {
    fn default() -> Self {
        Self::new()
    }
}

impl IoModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Top-level helpers
        functions.insert("read".to_string(), Val::RustFunction(io_read));
        functions.insert("write".to_string(), Val::RustFunction(io_write));
        functions.insert("read_all".to_string(), Val::RustFunction(io_read_all));
        functions.insert("lines".to_string(), Val::RustFunction(io_lines));
        functions.insert("read_stream".to_string(), Val::RustFunction(io_read_stream));
        functions.insert(
            "write_stream".to_string(),
            Val::RustFunction(io_write_stream),
        );
        functions.insert("timeout".to_string(), Val::RustFunction(io_timeout));

        // Stream handles
        functions.insert("stdin".to_string(), make_stdin_object());
        functions.insert("stdout".to_string(), make_stdout_object());
        functions.insert("stderr".to_string(), make_stderr_object());

        Self { functions }
    }
}

impl Module for IoModule {
    fn name(&self) -> &str {
        "io"
    }

    fn description(&self) -> &str {
        "Standard input/output streams and helpers"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::register_stdlib_modules;
    use anyhow::Result;
    use qcl_core::{stmt_parser::StmtParser, token::Tokenizer, val::Val};
    use std::sync::Arc;

    #[test]
    fn test_io_stdout_flush() -> Result<()> {
        let source = "import io; return io.stdout.flush();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver =
            std::sync::Arc::new(qcl_core::import::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Bool(true));
        Ok(())
    }

    #[test]
    fn test_io_write_returns_len() -> Result<()> {
        let source = "import io; return io.write(\"abc\");";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver =
            std::sync::Arc::new(qcl_core::import::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Int(3));
        Ok(())
    }

    #[test]
    fn test_io_stdin_flush_is_noop() -> Result<()> {
        let source = "import io; return io.stdin.flush();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver =
            std::sync::Arc::new(qcl_core::import::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        assert_eq!(result, Val::Bool(true));
        Ok(())
    }

    #[test]
    fn test_io_stdout_write_stream_returns_channel() -> Result<()> {
        let source = "import io; return io.stdout.write_stream();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver =
            std::sync::Arc::new(qcl_core::import::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        match result {
            Val::Channel(_) => Ok(()),
            _ => anyhow::bail!("expected Channel"),
        }
    }

    #[test]
    fn test_io_lines_returns_channel() -> Result<()> {
        let source = "import io; return io.lines();";
        let tokens = Tokenizer::tokenize(source)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let ctx = Val::Map(Arc::new(std::collections::HashMap::new()));

        let mut registry = qcl_core::module::ModuleRegistry::new();
        register_stdlib_modules(&mut registry);
        let resolver =
            std::sync::Arc::new(qcl_core::import::ModuleResolver::with_registry(registry));
        let mut env = qcl_core::stmt::Environment::with_resolver(resolver);

        let result = program.execute_with_env(&ctx, &mut env)?;
        match result {
            Val::Channel(_) => Ok(()),
            _ => anyhow::bail!("expected Channel"),
        }
    }
}
