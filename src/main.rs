use std::io::BufRead;

use qcl::{expr::Expr, val::Val, de};

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        eprintln!("Usage: cat <json> | {} <expr>", args[0]);
        std::process::exit(1);
    }

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .join("\n");
    let expr = args[1..].join(" ");

    let ctx: Val = de::from_json_str(&raw)?;
    let val = Expr::parse_cached(&expr)?;
    let res = val.eval(&ctx)?;
    println!("{}", res);
    Ok(())
}
