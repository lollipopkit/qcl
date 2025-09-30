use std::io::{BufRead, IsTerminal};
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use qcl_core::stmt::ModuleResolver;
use qcl_core::{
    expr::Expr,
    module::ModuleRegistry,
    stmt::{self, stmt_parser::StmtParser},
    token::Tokenizer,
    val::{Val, de},
};

#[cfg(feature = "concurrency")]
use qcl_core::rt;

mod repl;

fn read_file_content(path: &str) -> anyhow::Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))
}

fn sanitize_rel_path(raw: &str) -> anyhow::Result<PathBuf> {
    let p = Path::new(raw);
    // Only allow strictly relative paths without any parent directory components
    if !p.is_relative() {
        return Err(anyhow::anyhow!(
            "Absolute paths are not allowed. Use a relative path."
        ));
    }

    for comp in p.components() {
        if matches!(comp, Component::ParentDir) {
            return Err(anyhow::anyhow!(
                "Parent directory components ('..') are not allowed in file paths."
            ));
        }
    }

    Ok(p.to_path_buf())
}

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    // Print usage only when no args and not in a terminal
    if args.len() < 2 && !std::io::stdin().is_terminal() {
        let formats = ["json", "yaml", "toml"];

        let format_str = formats.join("|");
        let flag_str = formats
            .iter()
            .map(|f| format!("--{}", f))
            .collect::<Vec<_>>()
            .join("|");

        eprintln!(
            "Usage: cat <{}> | {} [--repl] [{}] [--expr|--stmt] [--vm] <expr|program|file>",
            format_str, args[0], flag_str
        );
        eprintln!("  Format is auto-detected unless {} is specified", flag_str);
        eprintln!("  Default is expression mode, use --stmt for statement mode");
        eprintln!("  If a single argument is a file path, it will be executed");
        eprintln!("  Use --repl for interactive mode (or run with no args in a TTY)");
        eprintln!(
            "  Note: only relative, sanitized file paths are allowed (no '..', no absolute paths)"
        );
        std::process::exit(1);
    }

    let mut arg_idx = 1;
    let mut format_override = None;
    // Default to expression mode; fallback target is expr when not a file
    let mut is_statement_mode = false;
    let mut repl_mode = false;
    let mut use_vm = std::env::var("QCL_VM").is_ok();

    while arg_idx < args.len() {
        match args[arg_idx].as_str() {
            "--json" => {
                format_override = Some(de::Format::Json);
                arg_idx += 1;
            }
            "--yaml" => {
                format_override = Some(de::Format::Yaml);
                arg_idx += 1;
            }
            "--toml" => {
                format_override = Some(de::Format::Toml);
                arg_idx += 1;
            }
            "--stmt" => {
                is_statement_mode = true;
                arg_idx += 1;
            }
            "--expr" => {
                is_statement_mode = false;
                arg_idx += 1;
            }
            "--repl" => {
                repl_mode = true;
                arg_idx += 1;
            }
            "--vm" => {
                use_vm = true;
                arg_idx += 1;
            }
            _ => break,
        }
    }

    let raw = if std::io::stdin().is_terminal() {
        // If stdin is a terminal (interactive mode), don't wait for input
        String::new()
    } else {
        // If stdin is piped/redirected, read from it
        let raw = std::io::stdin()
            .lock()
            .lines()
            .collect::<Result<Vec<_>, _>>();

        match raw {
            Ok(lines) => lines.join("\n"),
            Err(_) => String::new(),
        }
    };

    let ctx: Val = if raw.is_empty() {
        Val::Map(Arc::new(Default::default()))
    } else {
        de::parse_with_format(&raw, format_override)?
    };

    // If --repl specified, or no remaining args and in terminal, enter REPL
    if repl_mode || (arg_idx >= args.len() && std::io::stdin().is_terminal()) {
        return repl::run(is_statement_mode, ctx);
    }

    if arg_idx >= args.len() {
        eprintln!("Error: No expression or program provided");
        std::process::exit(1);
    }

    let input_args = args[arg_idx..].to_vec();

    // Check if the first argument is a file path
    let input = if input_args.len() == 1 {
        let potential_file = &input_args[0];

        // If it looks like an existing file, enforce path safety then read.
        match std::fs::metadata(potential_file) {
            Ok(meta) if meta.is_file() => {
                // Enforce repository security policy: only relative, sanitized paths
                let safe = sanitize_rel_path(potential_file).map_err(|e| {
                    eprintln!("Error: {}", e);
                    e
                });
                match safe {
                    Ok(safe_path) => {
                        // When reading from file, default to statement mode
                        is_statement_mode = true;
                        let sp = safe_path.to_string_lossy().to_string();
                        read_file_content(&sp)?
                    }
                    Err(_) => std::process::exit(1),
                }
            }
            _ => {
                // Not an existing file, treat as expression/program
                input_args.join(" ")
            }
        }
    } else {
        // Multiple arguments, treat as expression/program
        input_args.join(" ")
    };
    // Initialize runtime for concurrency if needed (for both statement and expression modes)
    #[cfg(feature = "concurrency")]
    {
        if let Err(e) = rt::init_runtime() {
            eprintln!("Warning: Failed to initialize runtime: {}", e);
        }
    }

    let result = if is_statement_mode {
        // Use enhanced tokenizer with position tracking
        let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(&input) {
            Ok((tokens, spans)) => (tokens, spans),
            Err(parse_err) => {
                eprintln!("Error: {}", parse_err);
                std::process::exit(1);
            }
        };

        let mut parser = StmtParser::new_with_spans(&tokens, &spans);
        let program = match parser.parse_program_with_enhanced_errors(&input) {
            Ok(program) => program,
            Err(parse_err) => {
                eprintln!("Error: {}", parse_err);
                std::process::exit(1);
            }
        };
        // Prepare stdlib environment
        let mut registry = ModuleRegistry::new();
        qcl_stdlib::register_stdlib_globals(&mut registry);
        qcl_stdlib::register_stdlib_modules(&mut registry);
        let resolver = Arc::new(ModuleResolver::with_registry(registry));
        let mut env = stmt::Environment::with_resolver(resolver);

        if use_vm {
            #[cfg(feature = "vm")]
            {
                // Compile entire program block to bytecode and execute with VM
                let block = qcl_core::stmt::Stmt::Block { statements: program.statements.clone() };
                let func = qcl_core::vm::Compiler::new().compile_stmt(&block);
                let mut vm = qcl_core::vm::Vm::new();
                return vm.exec_with(&func, Some(&mut env), &ctx);
            }
            #[cfg(not(feature = "vm"))]
            {
                eprintln!("Warning: --vm specified but this binary was built without 'vm' feature; falling back to interpreter.");
            }
        }

        // Interpreter fallback
        program.execute_with_env(&ctx, &mut env)
    } else {
        // Expression mode
        if use_vm {
            #[cfg(feature = "vm")]
            {
                let expr = Expr::parse_cached_arc(&input)?;
                let compiler = qcl_core::vm::Compiler::new();
                let func = compiler.compile_expr(&expr);
                let mut vm = qcl_core::vm::Vm::new();
                vm.exec_with(&func, None, &ctx)
            }
            #[cfg(not(feature = "vm"))]
            {
                eprintln!("Warning: --vm specified (or QCL_VM set) but this binary was built without 'vm' feature; using interpreter.");
                let expr = Expr::parse_cached_arc(&input)?;
                expr.eval(&ctx)
            }
        } else {
            let expr = Expr::parse_cached_arc(&input)?;
            expr.eval(&ctx)
        }
    };

    // Shutdown runtime after execution
    #[cfg(feature = "concurrency")]
    rt::shutdown_runtime();

    match result {
        Ok(res) => {
            if !matches!(res, Val::Nil) {
                println!("{}", res);
            }
            Ok(())
        }
        Err(e) => Err(e),
    }
}
