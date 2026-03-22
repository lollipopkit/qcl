use std::io::Read;

use qcl::{de, expr::Expr, val::Val};

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args();
    let bin = args.next().unwrap_or_else(|| "qcl".to_string());
    let first = match args.next() {
        Some(arg) => arg,
        None => {
            print_usage(&bin);
            std::process::exit(1);
        }
    };

    let mut raw = String::new();
    std::io::stdin().read_to_string(&mut raw)?;

    let (format_override, expr) = match first.as_str() {
        #[cfg(feature = "json")]
        "--json" => (Some(de::Format::Json), join_args(args)),
        #[cfg(feature = "yaml")]
        "--yaml" => (Some(de::Format::Yaml), join_args(args)),
        #[cfg(feature = "toml")]
        "--toml" => (Some(de::Format::Toml), join_args(args)),
        _ => (None, join_first_and_rest(first, args)),
    };

    let ctx: Val = de::parse_with_format(&raw, format_override)?;

    let expr = Expr::parse_cached_arc(&expr)?;
    let res = expr.eval(&ctx)?;
    println!("{}", res);
    Ok(())
}

fn join_args(args: impl Iterator<Item = String>) -> String {
    args.collect::<Vec<_>>().join(" ")
}

fn join_first_and_rest(first: String, args: impl Iterator<Item = String>) -> String {
    let mut expr = first;
    for arg in args {
        expr.push(' ');
        expr.push_str(&arg);
    }
    expr
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
    let flag_str = formats.iter().map(|f| format!("--{}", f)).collect::<Vec<_>>().join("|");

    eprintln!("Usage: cat <{}> | {} [{}] <expr>", format_str, bin, flag_str);
    eprintln!(
        "  Default format is {:?}; use {} to select a different parser",
        de::default_format(),
        flag_str
    );
}
