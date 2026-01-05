use std::io::Read;

use qcl::{de, expr::Expr, val::Val};

#[allow(clippy::vec_init_then_push)]
fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        #[allow(clippy::vec_init_then_push)]
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

    let mut raw = String::new();
    std::io::stdin().read_to_string(&mut raw)?;

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

    let expr = Expr::parse_cached_arc(&expr)?;
    let res = expr.eval(&ctx)?;
    println!("{}", res);
    Ok(())
}
