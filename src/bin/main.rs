use std::io::BufRead;

use qcl::{expr::Expr, val::Val};

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        eprintln!("Usage: cat a.json | {} <expr>", args[0]);
        std::process::exit(1);
    }

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .join("\n");
    let expr = args[1..].join(" ");

    let ctx = Val::from_json(serde_json::from_str(&raw)?);
    let val = Expr::try_from(expr.as_ref())?;
    let res = val.exec(&ctx)?;
    println!("{}", res);
    Ok(())
}