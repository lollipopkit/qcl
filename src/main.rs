use std::io::BufRead;

use qcl::{expr::Expr, val::Val, de, stmt_parser::StmtParser, token::Tokenizer};

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
        
        eprintln!("Usage: cat <{}> | {} [{}] [--stmt] <expr|program>", format_str, args[0], flag_str);
        eprintln!("  Format is auto-detected unless {} is specified", flag_str);
        eprintln!("  Use --stmt to execute statement programs instead of expressions");
        std::process::exit(1);
    }

    let raw = std::io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .join("\n");
    
    let mut arg_idx = 1;
    let mut format_override = None;
    let mut is_statement_mode = false;

    // 解析格式标志
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
            _ => break,
        }
    }

    if arg_idx >= args.len() {
        eprintln!("Error: No expression or program provided");
        std::process::exit(1);
    }

    let input = args[arg_idx..].join(" ");
    let ctx: Val = de::parse_with_format(&raw, format_override)?;
    
    if is_statement_mode {
        // 执行语句程序
        let tokens = Tokenizer::new(&input)?;
        let mut parser = StmtParser::new(&tokens);
        let program = parser.parse_program()?;
        let res = program.execute(&ctx)?;
        println!("{}", res);
    } else {
        // 执行表达式
        let val = Expr::parse_cached(&input)?;
        let res = val.eval(&ctx)?;
        println!("{}", res);
    }
    Ok(())
}
