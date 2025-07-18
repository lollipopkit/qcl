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

            assert_eq!(result, Val::Map(expected.into()));
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

            assert_eq!(result, Val::Map(expected.into()));
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

            assert_eq!(result, Val::Map(expected.into()));
        }
    }

    // Access tests
    #[test]
    fn test_map_access() {
        let mut map = HashMap::new();
        map.insert("name", "alice".to_string());
        map.insert("age", 30.to_string());

        let val: Val = map.into();
        let field = Val::Str("name".into());

        assert_eq!(val.access(&field), Some(&Val::Str("alice".into())));
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

    // Literal creation tests
    #[test]
    fn test_literal_list_creation() {
        let list = vec![Val::Int(1), Val::Str("hello".into()), Val::Bool(true)];
        let val = Val::List(list.clone().into());

        // Test access
        assert_eq!(val.access(&Val::Int(0)), Some(&Val::Int(1)));
        assert_eq!(
            val.access(&Val::Int(1)),
            Some(&Val::Str("hello".into()))
        );
        assert_eq!(val.access(&Val::Int(2)), Some(&Val::Bool(true)));
        assert_eq!(val.access(&Val::Int(3)), None);
    }

    #[test]
    fn test_literal_map_creation() {
        let mut map = HashMap::new();
        map.insert("name".to_string(), Val::Str("Alice".into()));
        map.insert("age".to_string(), Val::Int(30));
        map.insert("active".to_string(), Val::Bool(true));

        let val = Val::Map(map.into());

        // Test access
        assert_eq!(
            val.access(&Val::Str("name".into())),
            Some(&Val::Str("Alice".into()))
        );
        assert_eq!(
            val.access(&Val::Str("age".into())),
            Some(&Val::Int(30))
        );
        assert_eq!(
            val.access(&Val::Str("active".into())),
            Some(&Val::Bool(true))
        );
        assert_eq!(val.access(&Val::Str("nonexistent".into())), None);
    }

    #[test]
    fn test_nested_literal_access() {
        // Create nested structure: {"users": [{"name": "Alice", "age": 30}]}
        let mut inner_map = HashMap::new();
        inner_map.insert("name".to_string(), Val::Str("Alice".into()));
        inner_map.insert("age".to_string(), Val::Int(30));

        let users_list = vec![Val::Map(inner_map.into())];

        let mut outer_map = HashMap::new();
        outer_map.insert("users".to_string(), Val::List(users_list.into()));

        let val = Val::Map(outer_map.into());

        // Test nested access
        let users = val.access(&Val::Str("users".into())).unwrap();
        let first_user = users.access(&Val::Int(0)).unwrap();
        let name = first_user.access(&Val::Str("name".into())).unwrap();

        assert_eq!(name, &Val::Str("Alice".into()));
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
        let a = Val::Str("abc".into());
        let b = Val::Str("def".into());

        assert!(a < b);
    }

    #[test]
    fn test_incomparable_types() {
        let a = Val::Int(10);
        let b = Val::Str("abc".into());

        assert_eq!(a.partial_cmp(&b), None);
    }

    #[test]
    fn test_literal_equality() {
        // Test list equality
        let list1 = Val::List(vec![Val::Int(1), Val::Int(2), Val::Int(3)].into());
        let list2 = Val::List(vec![Val::Int(1), Val::Int(2), Val::Int(3)].into());
        let list3 = Val::List(vec![Val::Int(1), Val::Int(2), Val::Int(4)].into());

        assert_eq!(list1, list2);
        assert_ne!(list1, list3);

        // Test map equality
        let mut map1 = HashMap::new();
        map1.insert("a".to_string(), Val::Int(1));
        map1.insert("b".to_string(), Val::Int(2));

        let mut map2 = HashMap::new();
        map2.insert("a".to_string(), Val::Int(1));
        map2.insert("b".to_string(), Val::Int(2));

        let mut map3 = HashMap::new();
        map3.insert("a".to_string(), Val::Int(1));
        map3.insert("b".to_string(), Val::Int(3));

        let val1 = Val::Map(map1.into());
        let val2 = Val::Map(map2.into());
        let val3 = Val::Map(map3.into());

        assert_eq!(val1, val2);
        assert_ne!(val1, val3);
    }

    #[test]
    fn test_display_formatting() {
        // Test list display
        let list = Val::List(vec![
            Val::Int(1),
            Val::Str("hello".into()),
            Val::Bool(true),
        ].into());
        let display = format!("{}", list);
        assert!(display.contains("1") && display.contains("hello") && display.contains("true"));

        // Test map display
        let mut map = HashMap::new();
        map.insert("name".to_string(), Val::Str("Alice".into()));
        map.insert("age".to_string(), Val::Int(30));
        let val = Val::Map(map.into());
        let display = format!("{}", val);
        assert!(
            display.contains("name")
                && display.contains("Alice")
                && display.contains("age")
                && display.contains("30")
        );
    }
}
