use std::io::BufRead;

use qcl::{expr::Expr, val::Val, de};

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        eprintln!("Usage: cat <json|yaml|toml> | {} [--json|--yaml|--toml] <expr>", args[0]);
        eprintln!("  Format is auto-detected unless --json, --yaml, or --toml is specified");
        std::process::exit(1);
    }

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .join("\n");
    
    let (format_override, expr) = if args.len() > 2 {
        match args[1].as_str() {
            "--json" => (Some(de::Format::Json), args[2..].join(" ")),
            "--yaml" => (Some(de::Format::Yaml), args[2..].join(" ")),
            "--toml" => (Some(de::Format::Toml), args[2..].join(" ")),
            _ => (None, args[1..].join(" ")),
        }
    } else {
        (None, args[1..].join(" "))
    };

    let ctx: Val = de::parse_with_format(&raw, format_override)?;
    
    let val = Expr::parse_cached(&expr)?;
    let res = val.eval(&ctx)?;
    println!("{}", res);
    Ok(())
}
