use anyhow::{anyhow, Ok, Result};

use crate::{
    ast::Parser, expr::Expr, token::Tokenizer, val::Val
};

pub struct Executor {
    expr: Expr,
}

impl Executor {
    pub fn new(raw: &str) -> Result<Self> {
        let tokens = Tokenizer::new(raw).parse()?;
        let expr = Parser::new(tokens).parse()?;
        Ok(Self { expr })
    }

    pub fn exec(&self, ctx: &Val) -> Result<bool> {
        let val = self.expr.exec(ctx)?;
        match val {
            Val::Bool(b) => Ok(b),
            _ => Err(anyhow!("Invalid result {:?}", val)),
        }
    }
}
