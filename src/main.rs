use std::io::Read;

use qcl::{de, error, expr::Expr, val::Val};

fn main() -> error::Result<()> {
    let mut args: Vec<String> = std::env::args().collect();
    let bin = args.remove(0);

    if args.is_empty() {
        print_usage(&bin);
        std::process::exit(1);
    }

    // Parse flags
    let mut check_mode = false;
    let mut show_ast = false;
    let mut format_override = None;
    let mut positional = Vec::new();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--version" | "-V" => {
                println!("qcl {}", env!("CARGO_PKG_VERSION"));
                return Ok(());
            }
            "--help" | "-h" => {
                print_usage(&bin);
                return Ok(());
            }
            "--check" | "-c" => check_mode = true,
            "--ast" => show_ast = true,
            #[cfg(feature = "json")]
            "--json" => format_override = Some(de::Format::Json),
            #[cfg(feature = "yaml")]
            "--yaml" => format_override = Some(de::Format::Yaml),
            #[cfg(feature = "toml")]
            "--toml" => format_override = Some(de::Format::Toml),
            _ => positional.push(args[i].clone()),
        }
        i += 1;
    }

    let expr_str = positional.join(" ");
    if expr_str.is_empty() {
        eprintln!("Error: no expression provided");
        print_usage(&bin);
        std::process::exit(1);
    }

    let expr = Expr::parse_cached_arc(&expr_str)?;

    if show_ast {
        println!("{:?}", *expr);
        return Ok(());
    }

    let mut raw = String::new();
    std::io::stdin().read_to_string(&mut raw)?;

    let ctx: Val = de::parse_with_format(&raw, format_override)?;

    let res = expr.eval(&ctx)?;

    if check_mode {
        let code = match &res {
            Val::Bool(true) => 0,
            Val::Bool(false) | Val::Nil => 1,
            _ => 0,
        };
        std::process::exit(code);
    }

    println!("{res}");
    Ok(())
}

fn print_usage(bin: &str) {
    let formats = [
        #[cfg(feature = "json")]
        "json",
        #[cfg(feature = "yaml")]
        "yaml",
        #[cfg(feature = "toml")]
        "toml",
    ];

    let format_str = formats.join("|");
    let flag_str = formats.iter().map(|f| format!("--{f}")).collect::<Vec<_>>().join("|");

    eprintln!("Usage: cat <{format_str}> | {bin} [OPTIONS] <expr>");
    eprintln!();
    eprintln!("Options:");
    eprintln!(
        "  {flag_str}    Select input format (default: {:?})",
        de::default_format()
    );
    eprintln!("  --check, -c           Exit 0 if truthy, 1 if falsy");
    eprintln!("  --ast                 Print the parsed AST instead of evaluating");
    eprintln!("  --version, -V         Print version");
    eprintln!("  --help, -h            Print this help");
}
