#[cfg(test)]
mod tests {
    use crate::val::Val;

    macro_rules! test_op {
        ($name:ident, $op:tt, $l:expr, $r:expr, $res:expr) => {
            #[test]
            fn $name() {
                let l: Val = $l.into();
                let r: Val = $r.into();
                let res: Val = $res.into();
                assert_eq!((&l $op &r).unwrap(), res);
            }
        };
    }

    test_op!(add, +, 1, 2, 3);
    test_op!(sub, -, 1, 2, -1);
    test_op!(mul, *, 2, 3, 6);
    #[cfg(feature="sem_arith")]
    test_op!(div, /, 3, 2, 1.5);
    #[cfg(not(feature="sem_arith"))]
    test_op!(div, /, 3, 2, 1);
    #[cfg(feature="adv_arith")]
    test_op!(list_add_val, +, vec![1], 2, vec![1, 2]);
    #[cfg(feature="adv_arith")]
    test_op!(list_add_list, +, vec![1], vec![2], vec![1, 2]);
    #[cfg(feature="adv_arith")]
    test_op!(list_sub_val, -, vec![1, 2], 2, vec![1]);
    #[cfg(feature="adv_arith")]
    test_op!(list_sub_list, -, vec![1, 2], vec![2], vec![1]);
}
