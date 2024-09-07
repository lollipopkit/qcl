use anyhow::{anyhow, Ok, Result};

use crate::{expr::Expr, val::Val};

pub struct Executor {
    expr: Expr,
}

impl Executor {
    pub fn new(raw: &str) -> Result<Self> {
        let executor = Self { expr: Expr::Bool(true) };
        Ok(executor)
    }

    pub fn eval(&self, ctx: &Val) -> Result<bool> {
        let val = self.expr.eval(ctx)?;
        match val {
            Val::Bool(b) => Ok(b),
            _ => Err(anyhow!("Invalid result {:?}", val)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::val::Val;

    use super::*;

    #[test]
    fn test_parse() {
        let rule = "@req.user.age >= 18";
    }
}
