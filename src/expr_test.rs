#[cfg(test)]
mod test {
    use std::sync::OnceLock;

    use anyhow::Result;
    use serde_json::json;

    use crate::{expr::Expr, val::Val};

    #[test]
    fn basic() {
        sexpect("true || false", true);
        sexpect("false && true", false);
        sexpect("1 >= 1 && 1 <= 1 && 1 == 1", true);
        sexpect("1 > 1 || 1 < 1 || 1 != 1", false);

        sexpect("1 + 1 == 2", true);
        sexpect("3 / 3 + 3 % 3", 1);
        sexpect("1.1 - 1.2", -0.09999999999999987); // IEEE 754
        sexpect("1.1 * 1.2", 1.32);

        sexpect(r#""str1""#, "str1");
        sexpect(" '1' + '2' ", "12");

        spanic("@nonexist");
    }

    #[test]
    fn simple() {
        expect("@list + @list-2", vec![1, 2, 3, 1]);
        expect("@req.user.name + 'pt'", "lkpt");
        expect("@req.user.age + @list.0 == 19", true);

        panic("@req.user.name + @list");
        panic("@req.user.name + @list-2.0");
    }

    fn get_ctx() -> &'static Val {
        CTX.get_or_init(|| {
            json!({
                "req": {
                    "user": {
                        "name": "lk",
                        "age": 18,
                    }
                },
                "list": [1, 2, 3],
                "list-2": [1]
            })
            .into()
        })
    }

    static CTX: OnceLock<Val> = OnceLock::new();

    fn with_ctx(rule: &str, ctx: &Val) -> Result<Val> {
        let expr = Expr::try_from(rule)?;
        expr.eval(ctx.into())
    }

    fn expect<V: Into<Val>>(rule: &str, val: V) {
        let res = with_ctx(rule, get_ctx());
        assert_eq!(res.unwrap(), val.into());
    }

    fn panic(rule: &str) {
        let res = with_ctx(rule, get_ctx());
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{}", err);
    }

    fn sexpect<V: Into<Val>>(rule: &str, val: V) {
        let res = with_ctx(rule, &Val::Nil);
        assert_eq!(res.unwrap(), val.into());
    }

    fn spanic(rule: &str) {
        let res = with_ctx(rule, &Val::Nil);
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{}", err);
    }
}
