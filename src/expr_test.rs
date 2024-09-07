#[cfg(test)]
mod test {
    use anyhow::{anyhow, Result};

    use crate::{expr::{BinOp, Expr}, val::Val};

    #[test]
    fn basic() -> Result<()> {
        let expr = Expr::Bin(
            Box::new(Expr::Int(1)),
            BinOp::Gt,
            Box::new(Expr::Float(0.1)),
        );
        match expr.exec(&Val::Nil) {
            Ok(v) => {
                match v {
                    Val::Bool(v) => match v {
                        true => {
                            println!("{:?}", v);
                            Ok(())
                        },
                        false => Err(anyhow!("Expected true, got {:?}", v)),
                    }
                    _ => Err(anyhow!("Invalid result {:?}", v),)
                }
            }
            Err(e) => Err(e),
        }
    }
}
