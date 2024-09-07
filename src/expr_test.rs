#[cfg(test)]
mod test {
    use anyhow::Result;
    use serde_json::json;

    use crate::{expr::{Expr, IntoExpr}, val::Val};

    #[test]
    fn simple() {
        expect_bool("true", true);
        expect_bool("false", false);
        expect_bool("1 >= 1", true);
        expect_bool("1 > 1", false);
        expect_bool("1 <= 1", true);
        expect_bool("1 < 1", false);
        expect_bool("1 == 1", true);
        expect_bool("1 != 1", false);
        expect_bool("1 != 2", true);
        expect_bool("1 + 1 == 2", true);

        expect_int("3 / 3", 1);
        expect_int("3 % 3", 0);

        expect_float("1.1 + 1.2", 2.3);
        expect_float("1.1 - 1.2", -0.09999999999999987); // IEEE 754
        expect_float("1.1 * 1.2", 1.32);

        expect_str(r#""str1""#, "str1");
        expect_str(" '1' + '2' ", "12");
    }

    #[test]
    fn map_list() {
        let ctx = json!({
            "req": {
                "user": {
                    "name": "lk",
                    "age": 18,
                }
            },
            "list": [1, 2, 3]
        });
        
        expect_ctx(r#"@req.user.name + 'pt'"#, ctx.clone(), Val::Str("lkpt".to_string()));
        expect_ctx("@req.user.age + @list.0 == 19", ctx.clone(), Val::Bool(true));
    }

    fn with_ctx(rule: &str, ctx: serde_json::Value) -> Result<Val> {
        let expr = Expr::try_from(rule)?;
        let ctx = Val::from(ctx.clone());
        expr.exec(&ctx)
    }

    fn expect_ctx(rule: &str, ctx: serde_json::Value, val: Val) {
        let res = with_ctx(rule, ctx);
        assert_eq!(res.unwrap(), val);
    }

    fn expect(rule: &str, val: Val) {
        let res = with_nil_ctx(rule);
        assert_eq!(res.unwrap(), val);
    }

    fn expect_bool(rule: &str, b: bool) {
        expect(rule, Val::Bool(b));
    }

    fn expect_int(rule: &str, i: i64) {
        expect(rule, Val::Int(i));
    }

    fn expect_float(rule: &str, f: f64) {
        expect(rule, Val::Float(f));
    }

    fn expect_str(rule: &str, s: &str) {
        expect(rule, Val::Str(s.to_string()));
    }

    fn with_nil_ctx(rule: &str) -> Result<Val> {
        let exec = rule.into_expr()?;
        let ctx = Val::Nil;
        exec.exec(&ctx)
    }
}
