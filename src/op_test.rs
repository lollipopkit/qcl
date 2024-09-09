#[cfg(test)]
mod tests {
    use crate::{expr::Expr, op::BinOp, val::Val};

    use serde_json::json;

    macro_rules! test_op {
        ($name:ident, $op:tt, $l:expr, $r:expr, $res:expr) => {
            #[test]
            fn $name() {
                let ctx = json!({
                    "req": {"user": {"name": "lk", "age": 18}},
                    "list": [1, 2, 3],
                    "list-2": [1]
                });

                let l: Expr = $l.try_into().unwrap();
                let r: Expr = $r.try_into().unwrap();
                let res: Val = $res.into();
                assert_eq!(BinOp::$op.eval(&l, &r, &ctx.into()).unwrap(), res);
            }
        };
    }

    test_op!(add, Add, "@list.0", "1", 2);
    #[cfg(feature = "adv_arith")]
    test_op!(num_str_add, Add, "@list.0", "'str'", "1str");
    test_op!(sub, Sub, "@list.0", "1", 0);
    test_op!(mul, Mul, "@list.0", "2", 2);
    #[cfg(feature = "sem_arith")]
    test_op!(div, Div, "@list.2", "2", 1.5);
    #[cfg(not(feature = "sem_arith"))]
    test_op!(div, Div, "@list.2", "2", 1);
    #[cfg(feature = "adv_arith")]
    test_op!(list_add_val, Add, "@list", "4", vec![1, 2, 3, 4]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_add_list, Add, "@list", "@list-2", vec![1, 2, 3, 1]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_val, Sub, "@list", "2", vec![1, 3]);
    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_list, Sub, "@list", "@list-2", vec![2, 3]);
}
