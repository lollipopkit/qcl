#[cfg(test)]
mod tests {
    use crate::ser::to_val;
    use crate::val::Val;
    use hashbrown::HashMap;
    use serde::Serialize;

    #[derive(Serialize)]
    struct User {
        role: String,
        age: i64,
        admin: bool,
    }

    #[derive(Serialize)]
    enum Shape {
        Circle(f64),
        Rect { w: f64, h: f64 },
        Point,
        Pair(i64, i64),
    }

    #[test]
    fn primitives() {
        assert_eq!(to_val(&true).unwrap(), Val::Bool(true));
        assert_eq!(to_val(&42i64).unwrap(), Val::Int(42));
        assert_eq!(to_val(&42u32).unwrap(), Val::Int(42));
        assert_eq!(to_val(&u64::MAX).unwrap(), Val::Float(u64::MAX as f64));
        assert_eq!(to_val(&3.14f64).unwrap(), Val::Float(3.14));
        assert_eq!(to_val(&"hello").unwrap(), Val::Str("hello".into()));
        assert_eq!(to_val(&'a').unwrap(), Val::Str("a".into()));
        assert_eq!(to_val(&None::<i64>).unwrap(), Val::Nil);
        assert_eq!(to_val(&Some(7i64)).unwrap(), Val::Int(7));
        assert_eq!(to_val(&()).unwrap(), Val::Nil);
    }

    #[test]
    fn list_and_map() {
        let v = vec![1i64, 2, 3];
        let val = to_val(&v).unwrap();
        assert!(matches!(val, Val::List(l) if l.len() == 3));

        let mut m = HashMap::new();
        m.insert("k".to_string(), 1i64);
        let val = to_val(&m).unwrap();
        assert!(matches!(val, Val::Map(m) if m.len() == 1));
    }

    #[test]
    fn map_with_int_key_normalizes_to_string() {
        let mut m = HashMap::new();
        m.insert(42i64, "answer");
        let val = to_val(&m).unwrap();
        match val {
            Val::Map(m) => {
                assert_eq!(m.len(), 1);
                assert_eq!(m.get("42"), Some(&Val::Str("answer".into())));
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn map_with_non_primitive_key_errors() {
        let mut m = HashMap::new();
        m.insert((1i64, 2i64), "v");
        let err = to_val(&m).unwrap_err();
        assert!(
            err.to_string().contains("map key must be a primitive"),
            "{}",
            err
        );
    }

    #[test]
    fn struct_to_map() {
        let u = User {
            role: "admin".into(),
            age: 18,
            admin: true,
        };
        let val = to_val(&u).unwrap();
        let m = match val {
            Val::Map(m) => m,
            _ => panic!("expected map"),
        };
        assert_eq!(m.len(), 3);
        assert_eq!(m.get("role"), Some(&Val::Str("admin".into())));
        assert_eq!(m.get("age"), Some(&Val::Int(18)));
        assert_eq!(m.get("admin"), Some(&Val::Bool(true)));
    }

    #[test]
    fn enum_variants() {
        assert_eq!(to_val(&Shape::Point).unwrap(), Val::Str("Point".into()));

        let newtype = to_val(&Shape::Circle(1.0)).unwrap();
        match newtype {
            Val::Map(m) => {
                assert_eq!(m.len(), 1);
                assert_eq!(m.get("Circle"), Some(&Val::Float(1.0)));
            }
            _ => panic!("expected map"),
        }

        let tuple = to_val(&Shape::Pair(1, 2)).unwrap();
        assert!(matches!(tuple, Val::List(l) if l.len() == 2));

        let st = to_val(&Shape::Rect { w: 2.0, h: 3.0 }).unwrap();
        assert!(matches!(st, Val::Map(m) if m.len() == 2));
    }

    #[test]
    fn nested_struct_in_vec() {
        let v = vec![
            User {
                role: "a".into(),
                age: 1,
                admin: false,
            },
            User {
                role: "b".into(),
                age: 2,
                admin: true,
            },
        ];
        let val = to_val(&v).unwrap();
        assert!(matches!(val, Val::List(l) if l.len() == 2));
    }

    #[cfg(feature = "json")]
    #[test]
    fn matches_json_roundtrip() {
        // to_val should produce the same Val as struct -> json text -> from_json_str,
        // proving the serializer mirrors the deserializer's representation.
        let u = User {
            role: "admin".into(),
            age: 18,
            admin: true,
        };
        let direct = to_val(&u).unwrap();
        let json = serde_json::to_string(&u).unwrap();
        let via_json = crate::de::from_json_str(&json).unwrap();
        assert_eq!(direct, via_json);
    }

    #[test]
    fn try_from_uses_serializer() {
        // Val::try_from now goes through ser::to_val (no serde_json::Value layer),
        // so it works even without the `json` feature.
        let u = User {
            role: "admin".into(),
            age: 18,
            admin: true,
        };
        let v: Val = Val::try_from(&u).unwrap();
        assert!(matches!(v, Val::Map(_)));
    }
}
