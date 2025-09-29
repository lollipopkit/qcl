use anyhow::Result;
use qcl_core::{module::Module, val::Val};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone)]
struct EnvObject;

impl EnvObject {
    fn create() -> Val {
        let mut methods = HashMap::new();
        methods.insert("get".to_string(), Val::RustFunction(Self::get));
        methods.insert("set".to_string(), Val::RustFunction(Self::set));
        methods.insert("unset".to_string(), Val::RustFunction(Self::unset));
        methods.into()
    }

    fn get(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 && args.len() != 2 {
            return Err(anyhow::anyhow!(
                "env.get() takes 1 or 2 arguments: variable_name [, default_value]"
            ));
        }

        let var_name = match &args[0] {
            Val::Str(name) => &**name,
            _ => return Err(anyhow::anyhow!("first argument must be a string")),
        };

        // Get default value if provided
        let default_val = if args.len() == 2 {
            match &args[1] {
                Val::Str(val) => Some(&**val),
                Val::Nil => None,
                _ => return Err(anyhow::anyhow!("second argument must be a string or nil")),
            }
        } else {
            None
        };

        match std::env::var_os(var_name) {
            Some(value) => match value.into_string() {
                Ok(value_str) => Ok(Val::Str(value_str.into())),
                Err(_) => Ok(Val::Nil),
            },
            None => match default_val {
                Some(default) => Ok(Val::Str(default.into())),
                None => Ok(Val::Nil),
            },
        }
    }

    fn set(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "env.set() takes exactly 2 arguments: variable_name, value"
            ));
        }

        let var_name = match &args[0] {
            Val::Str(name) => &**name,
            _ => return Err(anyhow::anyhow!("first argument must be a string")),
        };

        let value = match &args[1] {
            Val::Str(val) => &**val,
            _ => return Err(anyhow::anyhow!("second argument must be a string")),
        };

        unsafe {
            std::env::set_var(var_name, value);
        }
        Ok(Val::Bool(true))
    }

    fn unset(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!(
                "env.unset() takes exactly 1 argument: variable_name"
            ));
        }

        let var_name = match &args[0] {
            Val::Str(name) => &**name,
            _ => return Err(anyhow::anyhow!("argument must be a string")),
        };

        unsafe {
            std::env::remove_var(var_name);
        }
        Ok(Val::Bool(true))
    }
}

struct DirObject;

impl DirObject {
    fn create() -> Val {
        let mut methods = HashMap::new();
        methods.insert("list".to_string(), Val::RustFunction(Self::list));
        methods.insert("temp".to_string(), Val::RustFunction(Self::temp_dir));
        methods.insert("current".to_string(), Val::RustFunction(Self::current_dir));
        methods.into()
    }

    fn list(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("dir.list() takes exactly 1 argument: path"));
        }

        let path = match &args[0] {
            Val::Str(p) => &**p,
            _ => return Err(anyhow::anyhow!("argument must be a string")),
        };

        let mut entries = Vec::new();
        match std::fs::read_dir(path) {
            Ok(read_dir) => {
                for entry in read_dir {
                    match entry {
                        Ok(dir_entry) => {
                            if let Some(name) = dir_entry.file_name().to_str() {
                                entries.push(Val::Str(name.into()))
                            }
                        }
                        Err(_) => continue,
                    }
                }
                Ok(Val::List(Arc::new(entries)))
            }
            Err(e) => Err(anyhow::anyhow!("failed to read directory: {}", e)),
        }
    }

    fn temp_dir(_args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        Ok(match std::env::temp_dir().into_os_string().into_string() {
            Ok(path) => Val::Str(path.into()),
            Err(_) => Val::Nil,
        })
    }

    fn current_dir(_args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        Ok(match std::env::current_dir() {
            Ok(path) => match path.into_os_string().into_string() {
                Ok(path_str) => Val::Str(path_str.into()),
                Err(_) => Val::Nil,
            },
            Err(_) => Val::Nil,
        })
    }
}

#[derive(Debug)]
pub struct OsModule {
    functions: HashMap<String, Val>,
}

impl Default for OsModule {
    fn default() -> Self {
        Self::new()
    }
}

impl OsModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Register os functions as Rust functions
        functions.insert("hostname".to_string(), Val::RustFunction(Self::hostname));
        functions.insert("arch".to_string(), Val::RustFunction(Self::arch));
        functions.insert("os".to_string(), Val::RustFunction(Self::os));
        functions.insert("exit".to_string(), Val::RustFunction(Self::exit));

        // Add env object
        functions.insert("env".to_string(), EnvObject::create());

        // Add dir object
        functions.insert("dir".to_string(), DirObject::create());

        Self { functions }
    }

    /// Get system hostname
    fn hostname(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if !args.is_empty() {
            return Err(anyhow::anyhow!("hostname() takes no arguments"));
        }

        match std::env::var_os("HOSTNAME") {
            Some(hostname) => match hostname.into_string() {
                Ok(hostname_str) => Ok(Val::Str(hostname_str.into())),
                Err(_) => Ok(Val::Str("localhost".into())),
            },
            None => match std::env::var_os("COMPUTERNAME") {
                Some(hostname) => match hostname.into_string() {
                    Ok(hostname_str) => Ok(Val::Str(hostname_str.into())),
                    Err(_) => Ok(Val::Str("localhost".into())),
                },
                None => Ok(Val::Str("localhost".into())),
            },
        }
    }

    /// Get system architecture
    fn arch(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if !args.is_empty() {
            return Err(anyhow::anyhow!("arch() takes no arguments"));
        }

        Ok(Val::Str(std::env::consts::ARCH.into()))
    }

    /// Get operating system
    fn os(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if !args.is_empty() {
            return Err(anyhow::anyhow!("os() takes no arguments"));
        }

        Ok(Val::Str(std::env::consts::OS.into()))
    }

    /// Exit the program
    fn exit(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
        if args.len() > 1 {
            return Err(anyhow::anyhow!(
                "exit() takes at most 1 argument: exit_code"
            ));
        }

        let exit_code = if args.is_empty() {
            0
        } else {
            match &args[0] {
                Val::Int(code) => *code as i32,
                _ => return Err(anyhow::anyhow!("exit code must be an integer")),
            }
        };

        std::process::exit(exit_code);
    }
}

impl Module for OsModule {
    fn name(&self) -> &str {
        "os"
    }

    fn description(&self) -> &str {
        "Operating system interface"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}
