#[cfg(test)]
mod tests {
    use crate::val::Val;
    use std::collections::HashMap;

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

    #[cfg(feature = "sem_arith")]
    test_op!(div, /, 3, 2, 1.5);

    #[cfg(not(feature = "sem_arith"))]
    test_op!(div, /, 3, 2, 1);

    #[cfg(feature = "adv_arith")]
    test_op!(list_add_val, +, vec![1], 2, vec![1, 2]);

    #[cfg(feature = "adv_arith")]
    test_op!(list_add_list, +, vec![1], vec![2], vec![1, 2]);

    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_val, -, vec![1, 2], 2, vec![1]);

    #[cfg(feature = "adv_arith")]
    test_op!(list_sub_list, -, vec![1, 2], vec![2], vec![1]);

    // Modulo tests
    test_op!(mod_int, %, 7, 3, 1);
    test_op!(mod_float, %, 7.5, 2.0, 1.5);
    test_op!(mod_mixed1, %, 7, 2.5, 2.0);
    test_op!(mod_mixed2, %, 7.5, 2, 1.5);

    #[cfg(feature = "adv_arith")]
    mod adv_arith_tests {
        use super::*;

        // String concatenation with numbers
        test_op!(str_add_int, +, "hello", 123, "hello123");
        test_op!(str_add_float, +, "hello", 12.34, "hello12.34");
        test_op!(int_add_str, +, 123, "hello", "123hello");
        test_op!(float_add_str, +, 12.34, "hello", "12.34hello");

        // Map operations
        #[test]
        fn map_add_map() {
            let mut map1 = HashMap::new();
            map1.insert("a", 1);
            map1.insert("b", 2);

            let mut map2 = HashMap::new();
            map2.insert("c", 3);
            map2.insert("a", 4); // This should override map1's "a"

            let mut expected = HashMap::new();
            expected.insert("a".to_string(), Val::Int(4));
            expected.insert("b".to_string(), Val::Int(2));
            expected.insert("c".to_string(), Val::Int(3));

            let l: Val = map1.into();
            let r: Val = map2.into();
            let result = (&l + &r).unwrap();

            assert_eq!(result, Val::Map(Box::new(expected)));
        }

        #[test]
        fn map_sub_keys() {
            let mut map1 = HashMap::new();
            map1.insert("a", 1);
            map1.insert("b", 2);
            map1.insert("c", 3);

            let mut map2 = HashMap::new();
            map2.insert("a", 10); // Value doesn't matter, only key is used in subtraction

            let mut expected = HashMap::new();
            expected.insert("b".to_string(), Val::Int(2));
            expected.insert("c".to_string(), Val::Int(3));

            let l: Val = map1.into();
            let r: Val = map2.into();
            let result = (&l - &r).unwrap();

            assert_eq!(result, Val::Map(Box::new(expected)));
        }

        #[test]
        fn map_sub_str_key() {
            let mut map1 = HashMap::new();
            map1.insert("a", 1);
            map1.insert("b", 2);

            let key = "a";

            let mut expected = HashMap::new();
            expected.insert("b".to_string(), Val::Int(2));

            let l: Val = map1.into();
            let r: Val = key.into();
            let result = (&l - &r).unwrap();

            assert_eq!(result, Val::Map(Box::new(expected)));
        }
    }

    // Access tests
    #[test]
    fn test_map_access() {
        let mut map = HashMap::new();
        map.insert("name", "alice".to_string());
        map.insert("age", 30.to_string());

        let val: Val = map.into();
        let field = Val::Str("name".to_string());

        assert_eq!(val.access(&field), Some(&Val::Str("alice".to_string())));
    }

    #[test]
    fn test_list_access() {
        let list = vec![10, 20, 30];
        let val: Val = list.into();
        let index = Val::Int(1);

        assert_eq!(val.access(&index), Some(&Val::Int(20)));
    }

    #[test]
    fn test_access_out_of_bounds() {
        let list = vec![10, 20, 30];
        let val: Val = list.into();
        let index = Val::Int(5);

        assert_eq!(val.access(&index), None);
    }

    #[test]
    fn test_access_negative_index() {
        let list = vec![10, 20, 30];
        let val: Val = list.into();
        let index = Val::Int(-1);

        assert_eq!(val.access(&index), None);
    }

    // Comparison tests
    #[test]
    fn test_partial_ord_integers() {
        let a = Val::Int(10);
        let b = Val::Int(20);

        assert!(a < b);
    }

    #[test]
    fn test_partial_ord_floats() {
        let a = Val::Float(10.5);
        let b = Val::Float(20.5);

        assert!(a < b);
    }

    #[test]
    fn test_partial_ord_mixed() {
        let a = Val::Int(10);
        let b = Val::Float(10.5);

        assert!(a < b);
    }

    #[test]
    fn test_partial_ord_strings() {
        let a = Val::Str("abc".to_string());
        let b = Val::Str("def".to_string());

        assert!(a < b);
    }

    #[test]
    fn test_incomparable_types() {
        let a = Val::Int(10);
        let b = Val::Str("abc".to_string());

        assert_eq!(a.partial_cmp(&b), None);
    }
}
