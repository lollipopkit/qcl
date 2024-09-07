#[cfg(test)]
mod test {
    use crate::{exec::Executor, val::Val};
    use anyhow::{anyhow, Result};

    #[test]
    fn test_parse() -> Result<()> {
        let rule = "@req.user.age >= 18";
        let exec = Executor::new(rule)?;
        let ctx = Val::from_json(serde_json::json!({
            "req": {
                "user": {
                    "age": 18
                }
            }
        }));
        let res = exec.exec(&ctx)?;
        match res {
            true => Ok(()),
            _ => Err(anyhow!("Invalid result {:?}", res)),
        }
    }
}
