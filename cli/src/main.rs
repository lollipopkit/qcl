use std::io::{BufRead, IsTerminal};
use std::{collections::HashMap, sync::Arc};

use qcl_core::{
    de, expr::Expr, import, module::ModuleRegistry, stmt, stmt_parser::StmtParser,
    token::Tokenizer, val::Val,
};

#[cfg(feature = "concurrency")]
use qcl_core::runtime;

fn read_file_content(path: &str) -> anyhow::Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))
}

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        let formats = ["json", "yaml", "toml"];

        let format_str = formats.join("|");
        let flag_str = formats
            .iter()
            .map(|f| format!("--{}", f))
            .collect::<Vec<_>>()
            .join("|");

        eprintln!(
            "Usage: cat <{}> | {} [{}] [--expr] <expr|program|file>",
            format_str, args[0], flag_str
        );
        eprintln!("  Format is auto-detected unless {} is specified", flag_str);
        eprintln!("  Default is statement mode, use --expr for expression mode");
        eprintln!("  If a single argument is a file path, it will be executed");
        std::process::exit(1);
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

    let mut arg_idx = 1;
    let mut format_override = None;
    let mut is_statement_mode = true;

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
            _ => break,
        }
    }

    if arg_idx >= args.len() {
        eprintln!("Error: No expression or program provided");
        std::process::exit(1);
    }

    let input_args = args[arg_idx..].to_vec();

    // Check if the first argument is a file path
    let input = if input_args.len() == 1 {
        let potential_file = &input_args[0];

        // Try to read as file first
        match read_file_content(potential_file) {
            Ok(content) => {
                // When reading from file, default to statement mode
                is_statement_mode = true;
                content
            }
            Err(_) => {
                // Not a file, treat as expression/program
                input_args.join(" ")
            }
        }
    } else {
        // Multiple arguments, treat as expression/program
        input_args.join(" ")
    };
    let ctx: Val = if raw.is_empty() {
        Val::Map(Arc::new(HashMap::new()))
    } else {
        de::parse_with_format(&raw, format_override)?
    };

    // Initialize runtime for concurrency if needed (for both statement and expression modes)
    #[cfg(feature = "concurrency")]
    {
        if let Err(e) = runtime::init_runtime() {
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
        let resolver = Arc::new(import::ModuleResolver::with_registry(registry));
        let mut env = stmt::Environment::with_resolver(resolver);

        program.execute_with_env(&ctx, &mut env)
    } else {
        let val = Expr::parse_cached(&input)?;
        val.eval(&ctx)
    };

    // Shutdown runtime after execution
    #[cfg(feature = "concurrency")]
    runtime::shutdown_runtime();

    match result {
        Ok(res) => {
            println!("{}", res);
            Ok(())
        }
        Err(e) => Err(e),
    }
}
