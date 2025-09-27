pub mod concurrency_chan;
pub mod concurrency_task;
pub mod time;
pub mod datetime;
pub mod io;
pub mod iter;
pub mod math;
pub mod os;
pub mod string;
pub mod tcp;

#[cfg(test)]
mod globals_test;
#[cfg(test)]
mod tcp_test;

use qcl_core::module::ModuleRegistry;
use qcl_core::val::Val;

/// Register all stdlib modules with the given registry
pub fn register_stdlib_modules(registry: &mut ModuleRegistry) {
    registry.register_module("io", Box::new(io::IoModule::new()));
    registry.register_module("iter", Box::new(iter::IterModule::new()));
    registry.register_module("math", Box::new(math::MathModule::new()));
    registry.register_module("string", Box::new(string::StringModule::new()));
    registry.register_module("datetime", Box::new(datetime::DateTimeModule::new()));
    registry.register_module("os", Box::new(os::OsModule::new()));
    registry.register_module("tcp", Box::new(tcp::TcpModule::new()));

    // Register concurrency modules
    #[cfg(feature = "concurrency")]
    {
        registry.register_module("task", Box::new(concurrency_task::TaskModule::new()));
        registry.register_module("chan", Box::new(concurrency_chan::ChannelModule::new()));
        registry.register_module("time", Box::new(time::TimeModule::new()));
    }
}

/// Register global builtin functions available without import
/// - print(fmt, ...args): print formatted text without newline; returns nil
/// - println(fmt, ...args): print formatted text with newline; returns nil
/// - panic([msg]): raise a runtime error with optional message and backtrace
pub fn register_stdlib_globals(registry: &mut ModuleRegistry) {
    fn format_variadic(args: &[Val]) -> String {
        if args.is_empty() {
            return String::new();
        }
        if let Val::Str(fmt) = &args[0] {
            // Simple {} placeholder formatting; additional args appended with spaces
            let rest = &args[1..];
            let mut out = String::with_capacity(fmt.len() + rest.len() * 8);
            let chars: Vec<char> = fmt.chars().collect();
            let mut i = 0usize;
            let mut arg_idx = 0usize;
            while i < chars.len() {
                if chars[i] == '{' && i + 1 < chars.len() && chars[i + 1] == '}' {
                    if arg_idx < rest.len() {
                        out.push_str(&rest[arg_idx].to_string());
                        arg_idx += 1;
                    } else {
                        out.push('{');
                        out.push('}');
                    }
                    i += 2;
                } else {
                    out.push(chars[i]);
                    i += 1;
                }
            }
            // Append any remaining args separated by spaces
            if arg_idx < rest.len() {
                if !out.is_empty() {
                    out.push(' ');
                }
                for (j, v) in rest[arg_idx..].iter().enumerate() {
                    if j > 0 {
                        out.push(' ');
                    }
                    out.push_str(&v.to_string());
                }
            }
            out
        } else {
            // No format string; join all args by spaces
            let mut out = String::new();
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&v.to_string());
            }
            out
        }
    }

    fn print_fn(
        args: &[Val],
        _env: &qcl_core::stmt::Environment,
        _ctx: &Val,
    ) -> anyhow::Result<Val> {
        let out = format_variadic(args);
        print!("{}", out);
        Ok(Val::Nil)
    }

    fn println_fn(
        args: &[Val],
        _env: &qcl_core::stmt::Environment,
        _ctx: &Val,
    ) -> anyhow::Result<Val> {
        let out = format_variadic(args);
        println!("{}", out);
        Ok(Val::Nil)
    }

    fn panic_fn(
        args: &[Val],
        _env: &qcl_core::stmt::Environment,
        _ctx: &Val,
    ) -> anyhow::Result<Val> {
        // Compose message from all arguments for better diagnostics
        let mut msg = if args.is_empty() {
            "panic".to_string()
        } else {
            let mut s = String::new();
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                s.push_str(&v.to_string());
            }
            s
        };
        // Attach a backtrace explicitly so users always see it regardless of env var
        let bt = std::backtrace::Backtrace::force_capture();
        msg.push_str("\nBacktrace:\n");
        msg.push_str(&format!("{}", bt));
        panic!("{}", msg);
    }

    registry.register_builtin("print", Val::RustFunction(print_fn));
    registry.register_builtin("println", Val::RustFunction(println_fn));
    registry.register_builtin("panic", Val::RustFunction(panic_fn));
}
