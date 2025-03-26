#[cfg(test)]
mod test {
    use anyhow::Result;
    use serde_json::json;
    use std::collections::HashSet;

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

    #[test]
    fn complex_expressions() {
        // Nested arithmetic operations
        expect("(@user.age + 2) * 3", 60);

        // Parenthesized expressions
        expect("@user.age * (2 + 1)", 54);

        // Multiple operators with precedence
        expect("@user.age + 2 * 3", 24);

        // Comparison operators
        expect("@user.age > 17", true);
        expect("@user.age < 19", true);
        expect("@user.age >= 18", true);
        expect("@user.age <= 18", true);
        expect("@user.age == 18", true);
        expect("@user.age != 19", true);
    }

    #[test]
    fn logical_operators() {
        // AND operator
        expect("@pub && @user.age > 17", true);
        expect("@pub && @user.age > 20", false);

        // OR operator
        expect("@user.age > 20 || @pub", true);
        expect("@user.age > 20 || @user.name == 'john'", false);

        // Complex logical expressions
        expect("@pub && (@user.age > 17 || @user.name == 'john')", true);
        expect("@pub || (@user.age < 17 && @user.name == 'john')", true);

        // Short-circuit evaluation
        expect("false && @nonexistent.field", false);
        expect("true || @nonexistent.field", true);
    }

    #[test]
    fn unary_operations() {
        // Logical NOT
        expect("!@pub", false);
        expect("!false", true);

        // Double negation
        expect("!!@pub", true);

        // NOT with expressions
        expect("!(@user.age > 20)", true);
    }

    #[test]
    fn map_and_list_access() {
        // Nested map access
        expect("@nested.level1.level2", "value");

        // // List access with variable index
        // expect("@list.@index", 2);

        // // Access with expressions
        // expect("@list.(@index-1)", 1);
    }

    #[test]
    fn test_requested_ctx() {
        let expr = Expr::try_from("@user.name && @list.0 || @pub").unwrap();
        let names = expr.requested_ctx();

        let mut expected = HashSet::new();
        expected.insert("user".to_string());
        expected.insert("list".to_string());
        expected.insert("pub".to_string());

        assert_eq!(names, expected);
    }

    #[test]
    fn test_nil_handling() {
        expect("@nonexistent == nil", true);
        expect("@nonexistent.field == nil", true);
        expect("nil == nil", true);
    }

    fn with_ctx(rule: &str) -> Result<Val> {
        let ctx: Val = json!({
            "user": {"name": "lk", "age": 18},
            "list": [1, 2, 3],
            "list-2": [2],
            "pub": true,
            "index": 1,
            "nested": {
                "level1": {
                    "level2": "value"
                }
            }
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
