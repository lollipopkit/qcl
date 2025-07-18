use std::io::BufRead;

use qcl::{expr::Expr, val::Val, de};

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
        let flag_str = formats.iter().map(|f| format!("--{}", f)).collect::<Vec<_>>().join("|");
        
        eprintln!("Usage: cat <{}> | {} [{}] <expr>", format_str, args[0], flag_str);
        eprintln!("  Format is auto-detected unless {} is specified", flag_str);
        std::process::exit(1);
    }

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .join("\n");
    
    let (format_override, expr) = if args.len() > 2 {
        match args[1].as_str() {
            #[cfg(feature = "json")]
            "--json" => (Some(de::Format::Json), args[2..].join(" ")),
            #[cfg(feature = "yaml")]
            "--yaml" => (Some(de::Format::Yaml), args[2..].join(" ")),
            #[cfg(feature = "toml")]
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
