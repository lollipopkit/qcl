use std::io::{BufRead, IsTerminal, Write};
use std::path::{Component, Path, PathBuf};
use std::{collections::HashMap, sync::Arc};

use qcl_core::stmt::ModuleResolver;
use qcl_core::{
    expr::Expr,
    ast::Parser as ExprParser,
    module::ModuleRegistry,
    stmt::{self, stmt_parser::StmtParser},
    token::Tokenizer,
    val::{Val, de},
};

#[cfg(feature = "concurrency")]
use qcl_core::rt;

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

fn print_repl_help() {
    eprintln!("Commands: :quit | :exit | :q, :help, :expr, :stmt");
    eprintln!(" - :expr    switch to expression mode");
    eprintln!(" - :stmt    switch to statement mode");
    eprintln!(" - :quit    exit REPL");
}

fn should_continue_multiline(buf: &str) -> bool {
    // Simple bracket/brace/paren balance check; continue if unbalanced or trailing '\\'
    let mut paren = 0i32;
    let mut brace = 0i32;
    let mut bracket = 0i32;
    for ch in buf.chars() {
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            _ => {}
        }
    }
    let trailing_backslash = buf.trim_end().ends_with('\\');
    paren > 0 || brace > 0 || bracket > 0 || trailing_backslash
}

fn run_repl(
    mut is_statement_mode: bool,
    ctx: Val,
) -> anyhow::Result<()> {
    // Initialize runtime for concurrency if enabled
    #[cfg(feature = "concurrency")]
    {
        if let Err(e) = rt::init_runtime() {
            eprintln!("Warning: Failed to initialize runtime: {}", e);
        }
    }

    // Prepare stdlib and environment (persist across statements)
    let mut registry = ModuleRegistry::new();
    qcl_stdlib::register_stdlib_globals(&mut registry);
    qcl_stdlib::register_stdlib_modules(&mut registry);
    let resolver = Arc::new(ModuleResolver::with_registry(registry));
    let mut env = stmt::Environment::with_resolver(resolver);

    let stdin = std::io::stdin();
    let mut buffer = String::new();

    print_repl_help();

    loop {
        // Prompt
        let prompt = if buffer.is_empty() {
            if is_statement_mode { "qcl(stmt)> " } else { "qcl(expr)> " }
        } else {
            "... "
        };
        print!("{}", prompt);
        std::io::stdout().flush().ok();

        buffer.clear();
        // If we already have a partial buffer (multiline), keep appending.
        let mut acc = String::new();
        loop {
            let mut line = String::new();
            if stdin.read_line(&mut line)? == 0 {
                // EOF: if no partial input, exit REPL gracefully
                println!();
                if acc.trim().is_empty() {
                    #[cfg(feature = "concurrency")]
                    rt::shutdown_runtime();
                    return Ok(());
                }
                break;
            }

            let trimmed = line.trim_end();
            if acc.is_empty() && trimmed.starts_with(':') {
                match trimmed {
                    ":quit" | ":exit" | ":q" => {
                        // Shutdown before exit
                        #[cfg(feature = "concurrency")]
                        rt::shutdown_runtime();
                        return Ok(());
                    }
                    ":help" => {
                        print_repl_help();
                        acc.clear();
                        break; // new prompt
                    }
                    ":expr" => {
                        is_statement_mode = false;
                        eprintln!("Switched to expression mode");
                        acc.clear();
                        break; // new prompt
                    }
                    ":stmt" => {
                        is_statement_mode = true;
                        eprintln!("Switched to statement mode");
                        acc.clear();
                        break; // new prompt
                    }
                    _ => {
                        eprintln!("Unknown command. Type :help for help.");
                        acc.clear();
                        break; // new prompt
                    }
                }
            }

            // Support line continuation via trailing '\\' (strip it)
            if trimmed.ends_with('\\') {
                acc.push_str(trimmed.strip_suffix('\\').unwrap_or(trimmed));
                acc.push('\n');
                print!("... ");
                std::io::stdout().flush().ok();
                continue;
            }

            acc.push_str(trimmed);
            acc.push('\n');
            if !is_statement_mode || !should_continue_multiline(&acc) {
                break;
            }

            // request next line for multiline in stmt mode
            print!("... ");
            std::io::stdout().flush().ok();
        }

        if acc.trim().is_empty() {
            continue;
        }

        let result = if is_statement_mode {
            let src = acc;
            let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(&src) {
                Ok((tokens, spans)) => (tokens, spans),
                Err(parse_err) => {
                    eprintln!("Error: {}", parse_err);
                    continue;
                }
            };

            let mut parser = StmtParser::new_with_spans(&tokens, &spans);
            let program = match parser.parse_program_with_enhanced_errors(&src) {
                Ok(program) => program,
                Err(parse_err) => {
                    eprintln!("Error: {}", parse_err);
                    continue;
                }
            };

            program.execute_with_env(&ctx, &mut env)
        } else {
            let line_src = acc;
            let (tokens, spans) = match Tokenizer::tokenize_enhanced_with_spans(&line_src) {
                Ok((tokens, spans)) => (tokens, spans),
                Err(parse_err) => {
                    eprintln!("Error: {}", parse_err);
                    continue;
                }
            };
            let mut p = ExprParser::new_with_spans(&tokens, &spans);
            let expr = match p.parse_with_enhanced_errors(&line_src) {
                Ok(expr) => expr,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    continue;
                }
            };
            expr.eval(&ctx)
        };

        match result {
            Ok(res) => {
                if !matches!(res, Val::Nil) {
                    println!("{}", res);
                }
            }
            Err(e) => eprintln!("Error: {}", e),
        }
    }
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
            "Usage: cat <{}> | {} [--repl] [{}] [--expr|--stmt] <expr|program|file>",
            format_str, args[0], flag_str
        );
        eprintln!("  Format is auto-detected unless {} is specified", flag_str);
        eprintln!("  Default is statement mode, use --expr for expression mode");
        eprintln!("  If a single argument is a file path, it will be executed");
        eprintln!("  Use --repl for interactive mode (or run with no args in a TTY)");
        eprintln!(
            "  Note: only relative, sanitized file paths are allowed (no '..', no absolute paths)"
        );
        std::process::exit(1);
    }

    let mut arg_idx = 1;
    let mut format_override = None;
    let mut is_statement_mode = true;
    let mut repl_mode = false;

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
        Val::Map(Arc::new(HashMap::new()))
    } else {
        de::parse_with_format(&raw, format_override)?
    };

    // If --repl specified, or no remaining args and in terminal, enter REPL
    if repl_mode || (arg_idx >= args.len() && std::io::stdin().is_terminal()) {
        return run_repl(is_statement_mode, ctx);
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

        // Create module registry and register stdlib modules and globals
        let mut registry = ModuleRegistry::new();
        qcl_stdlib::register_stdlib_globals(&mut registry);
        qcl_stdlib::register_stdlib_modules(&mut registry);

        // Create environment with stdlib modules
        let resolver = Arc::new(ModuleResolver::with_registry(registry));
        let mut env = stmt::Environment::with_resolver(resolver);

        program.execute_with_env(&ctx, &mut env)
    } else {
        let val = Expr::parse_cached(&input)?;
        val.eval(&ctx)
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
