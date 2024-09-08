#[cfg(test)]
mod test {
    use anyhow::Result;
    use serde_json::json;

    use crate::{expr::{Expr, IntoExpr}, val::{FromJson, Val}};

    #[test]
    fn simple() {
        expect("true", true);
        expect("false", false);
        expect("1 >= 1", true);
        expect("1 > 1", false);
        expect("1 <= 1", true);
        expect("1 < 1", false);
        expect("1 == 1", true);
        expect("1 != 1", false);
        expect("1 != 2", true);
        expect("1 + 1 == 2", true);

        expect("3 / 3", 1);
        expect("3 % 3", 0);

        expect("1.1 + 1.2", 2.3);
        expect("1.1 - 1.2", -0.09999999999999987); // IEEE 754
        expect("1.1 * 1.2", 1.32);

        expect(r#""str1""#, "str1");
        expect(" '1' + '2' ", "12");
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
        
        expect_ctx(r#"@req.user.name + 'pt'"#, ctx.clone(), "lkpt".into());
        expect_ctx("@req.user.age + @list.0 == 19", ctx.clone(), Val::Bool(true));
    }

    #[test]
    fn test_complex_expressions() {
        let ctx = json!({
            "user": {
                "age": 25,
                "name": "Alice",
                "scores": [80, 90, 85]
            },
            "settings": {
                "active": true,
                "threshold": 75.5
            }
        });

        let cases = vec![
            ("@user.age > 20 && @user.name == 'Alice'", true),
            ("@user.scores.1 >= 90 || @settings.active", true),
            ("@user.age < 18 || (@settings.threshold > 70 && @settings.active)", true),
            ("@user.scores.0 + @user.scores.2 > 160", true),
            ("@user.name in 'Alice'", true),
            ("!(@user.age < 20 || @user.age > 30)", true),
            ("(@user.age * 2) > (@settings.threshold + 25)", false),
        ];

        for (expr, expected) in cases {
            let result = with_ctx(expr, ctx.clone()).unwrap();
            assert_eq!(result, Val::Bool(expected), "Failed for expression: {}", expr);
        }
    }

    #[test]
    #[should_panic]
    fn test_error_cases() {
        let ctx = json!({
            "user": {
                "age": 25,
            }
        });

        let error_cases = vec![
            "@user.nonexistent",
            "@user.age > 'string'",
            "1 / 0",
            "@user.age in 25",
        ];

        for expr in error_cases {
            assert!(with_ctx(expr, ctx.clone()).is_err(), "Expected error for: {}", expr);
        }
    }

    fn with_ctx(rule: &str, ctx: serde_json::Value) -> Result<Val> {
        let expr = Expr::try_from(rule)?;
        let ctx = Val::from_json(ctx.clone());
        expr.exec(&ctx)
    }

    fn expect_ctx(rule: &str, ctx: serde_json::Value, val: Val) {
        let res = with_ctx(rule, ctx);
        assert_eq!(res.unwrap(), val);
    }

    fn expect<V: Into<Val>>(rule: &str, val: V) {
        let res = with_nil_ctx(rule);
        assert_eq!(res.unwrap(), val.into());
    }

    fn with_nil_ctx(rule: &str) -> Result<Val> {
        let exec = rule.into_expr()?;
        let ctx = Val::Nil;
        exec.exec(&ctx)
    }
}
