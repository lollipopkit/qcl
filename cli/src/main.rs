use std::{collections::HashMap, sync::Arc, path::Path};
use std::io::BufRead;
use sanitize_filename::{sanitize_with_options, Options};

use qcl_core::{de, expr::Expr, stmt_parser::StmtParser, token::Tokenizer, val::Val, module::ModuleRegistry, import, stmt};

fn is_safe_path(path: &str) -> bool {
    let path = Path::new(path);
    
    // Check for empty path
    if path.as_os_str().is_empty() {
        return false;
    }
    
    // Check for absolute paths (security measure)
    if path.is_absolute() {
        return false;
    }
    
    // Check for parent directory traversal attempts
    if path.components().any(|c| c == std::path::Component::ParentDir) {
        return false;
    }
    
    // Use sanitize-filename to validate path components
    let path_str = path.to_string_lossy();
    let options = Options {
        truncate: true,
        windows: true, // Enable Windows compatibility for cross-platform safety
        replacement: "",
    };
    
    // Check if sanitization would change the path
    let sanitized = sanitize_with_options(&path_str, options);
    if sanitized != path_str {
        return false;
    }
    
    // Additional check for suspicious characters
    let suspicious_chars = ['\0', '\n', '\r', '\t'];
    if path_str.chars().any(|c| suspicious_chars.contains(&c)) {
        return false;
    }
    
    true
}

fn read_file_content(path: &str) -> anyhow::Result<String> {
    if !is_safe_path(path) {
        return Err(anyhow::anyhow!("Unsafe file path: {}", path));
    }
    
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", path, e))
}

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        let mut formats = Vec::new();
        #[cfg(feature = "json")]
        formats.push("json");
        #[cfg(feature = "yaml")]
        formats.push("yaml");
        #[cfg(feature = "toml")]
        formats.push("toml");

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

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>();
    
    let raw = match raw {
        Ok(lines) => lines.join("\n"),
        Err(_) => String::new(),
    };

    let mut arg_idx = 1;
    let mut format_override = None;
    let mut is_statement_mode = true;

    while arg_idx < args.len() {
        match args[arg_idx].as_str() {
            #[cfg(feature = "json")]
            "--json" => {
                format_override = Some(de::Format::Json);
                arg_idx += 1;
            }
            #[cfg(feature = "yaml")]
            "--yaml" => {
                format_override = Some(de::Format::Yaml);
                arg_idx += 1;
            }
            #[cfg(feature = "toml")]
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
    let input: String;
    
    // Check if the first argument is a file path
    if input_args.len() == 1 {
        let potential_file = &input_args[0];
        
        // Try to read as file first
        input = match read_file_content(potential_file) {
            Ok(content) => {
                // When reading from file, default to statement mode
                is_statement_mode = true;
                content
            }
            Err(_) => {
                // Not a file, treat as expression/program
                input_args.join(" ")
            }
        };
    } else {
        // Multiple arguments, treat as expression/program
        input = input_args.join(" ");
    }
    let ctx: Val = if raw.is_empty() {
        Val::Map(Arc::new(HashMap::new()))
    } else {
        de::parse_with_format(&raw, format_override)?
    };

    if is_statement_mode {
        let tokens = Tokenizer::new(&input)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        
        // Create module registry and register stdlib modules
        let mut registry = ModuleRegistry::new();
        qcl_stdlib::register_stdlib_modules(&mut registry);
        
        // Create environment with stdlib modules
        let resolver = Arc::new(import::ModuleResolver::with_registry(registry));
        let mut env = stmt::Environment::with_resolver(resolver);
        
        let res = program.execute_with_env(&ctx, &mut env)?;
        println!("{}", res);
    } else {
        let val = Expr::parse_cached(&input)?;
        let res = val.eval(&ctx)?;
        println!("{}", res);
    }
    Ok(())
}
