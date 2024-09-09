#[cfg(test)]
mod test {
    use anyhow::Result;
    use serde_json::json;

    use crate::{expr::Expr, val::Val};

    #[test]
    fn simple() {
        expect("@pub", true);
        expect("@user.name + 'pt'", "lkpt");
        expect("@user.age + @list.0 == 19", true);

        #[cfg(feature = "adv_arith")]
        expect("@user.name + @user.age", "lk18");

        #[cfg(feature = "adv_arith")]
        expect("@list + @list-2", vec![1, 2, 3, 2]);

        #[cfg(feature = "adv_arith")]
        expect("@list - @list-2", vec![1, 3]);

        #[cfg(feature = "sem_arith")]
        expect("@list.2 / 2", 1.5);

        #[cfg(not(feature = "sem_arith"))]
        expect("@list.2 / 2", 1);

        panic("@user.name + @list");

        #[cfg(not(feature = "adv_arith"))]
        panic("@user.name + @list-2.0");
    }

    fn with_ctx(rule: &str) -> Result<Val> {
        let ctx: Val = json!({
            "user": {"name": "lk", "age": 18},
            "list": [1, 2, 3],
            "list-2": [2],
            "pub": true,
        })
        .into();
        let expr = Expr::try_from(rule)?;
        expr.eval(&ctx)
    }

    fn expect<V: Into<Val>>(rule: &str, val: V) {
        let res = with_ctx(rule);
        assert_eq!(res.unwrap(), val.into());
    }

    fn panic(rule: &str) {
        let res = with_ctx(rule);
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{}", err);
    }
}
