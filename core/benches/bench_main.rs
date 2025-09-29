use criterion::{Criterion, black_box, criterion_group, criterion_main};
use qcl_core::expr::Expr;
use qcl_core::val::Val;
use std::collections::HashMap;
use std::sync::Arc;

// Prepare context data for benchmarking (contains various types)
fn make_context() -> Val {
    let mut ctx_map = HashMap::new();
    // Basic values
    ctx_map.insert("val_small".to_string(), Val::Int(11));
    ctx_map.insert("val_large".to_string(), Val::Int(9999));
    ctx_map.insert("x".to_string(), Val::Int(1));
    // Lists (small and large)
    ctx_map.insert(
        "smalllist".to_string(),
        (1..=10).map(Val::Int).collect::<Vec<_>>().into(),
    );
    ctx_map.insert(
        "biglist".to_string(),
        (0..1000).map(Val::Int).collect::<Vec<_>>().into(),
    );
    // Map and keys
    ctx_map.insert("key".to_string(), Val::Str(Arc::from("key50")));
    let mut bigmap_inner = HashMap::new();
    for i in 0..100 {
        bigmap_inner.insert(format!("key{}", i), Val::Int(i));
    }
    ctx_map.insert("bigmap".to_string(), bigmap_inner.into());
    // Long string
    let big_string = "a".repeat(1000) + "z";
    ctx_map.insert(
        "bigstr".to_string(),
        Val::Str(Arc::from(big_string.as_str())),
    );
    ctx_map.into()
}

// Benchmark 1: Expression parsing performance (without cache vs with cache)
fn bench_parsing(c: &mut Criterion) {
    let expr_str =
        "(@user.age + 2) * (3 + 4) && @user.name == \"Alice\" || [1, 2, 3].1 in [0, 1, 2]";

    // Parsing without cache
    c.bench_function("parse_without_cache", |b| {
        b.iter(|| {
            let tokens = qcl_core::token::Tokenizer::tokenize(expr_str).unwrap();
            let expr = qcl_core::ast::Parser::new(&tokens).parse().unwrap();
            black_box(&expr);
        })
    });

    // Parsing with cache (warm up cache then repeatedly parse same expression)
    let _ = Expr::parse_cached_arc(expr_str).unwrap(); // Warm up cache
    c.bench_function("parse_with_cache", |b| {
        b.iter(|| {
            let expr = Expr::parse_cached_arc(expr_str).unwrap();
            black_box(&expr);
        })
    });
}

// Benchmark 2: Expression evaluation performance (constant folding vs no folding)
fn bench_evaluation(c: &mut Criterion) {
    let ctx = make_context();

    // Build long expression: pure constants and containing context variables
    let expr_const_str = concat!(
        "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + ",
        "11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + ",
        "21 + 22 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30 + ",
        "31 + 32 + 33 + 34 + 35 + 36 + 37 + 38 + 39 + 40 + ",
        "41 + 42 + 43 + 44 + 45 + 46 + 47 + 48 + 49 + 50 + ",
        "51 + 52 + 53 + 54 + 55 + 56 + 57 + 58 + 59 + 60 + ",
        "61 + 62 + 63 + 64 + 65 + 66 + 67 + 68 + 69 + 70 + ",
        "71 + 72 + 73 + 74 + 75 + 76 + 77 + 78 + 79 + 80 + ",
        "81 + 82 + 83 + 84 + 85 + 86 + 87 + 88 + 89 + 90 + ",
        "91 + 92 + 93 + 94 + 95 + 96 + 97 + 98 + 99 + 100"
    );
    let expr_nonconst_str = "@x + ".repeat(99) + "@x";

    // Parse expressions (constant folding will happen during parsing)
    let expr_constant = Expr::parse_cached_arc(expr_const_str).unwrap(); // Will fold to a constant
    let expr_nonconstant = Expr::parse_cached_arc(&expr_nonconst_str).unwrap(); // Keep chain of @x additions

    // Evaluate constant-folded expression
    c.bench_function("eval_constant_folded", |b| {
        b.iter(|| {
            black_box(expr_constant.eval(&ctx).unwrap());
        })
    });

    // Evaluate non-folded expression
    c.bench_function("eval_not_folded", |b| {
        b.iter(|| {
            black_box(expr_nonconstant.eval(&ctx).unwrap());
        })
    });
}

// Benchmark 3: 'in' operator performance comparison (small list vs large list vs Map vs string)
fn bench_in_operator(c: &mut Criterion) {
    let ctx = make_context();

    // Build test expressions containing 'in' (using predefined data in context)
    let expr_in_list_small = Expr::parse_cached_arc("@val_small in @smalllist").unwrap();
    let expr_in_list_large = Expr::parse_cached_arc("@val_large in @biglist").unwrap();
    let expr_in_map = Expr::parse_cached_arc("@key in @bigmap").unwrap();
    let expr_in_str = Expr::parse_cached_arc("\"z\" in @bigstr").unwrap();

    // Small list membership
    c.bench_function("in_list_small", |b| {
        b.iter(|| {
            black_box(expr_in_list_small.eval(&ctx).unwrap());
        })
    });

    // Large list membership
    c.bench_function("in_list_large", |b| {
        b.iter(|| {
            black_box(expr_in_list_large.eval(&ctx).unwrap());
        })
    });

    // Map key lookup
    c.bench_function("in_map_keys", |b| {
        b.iter(|| {
            black_box(expr_in_map.eval(&ctx).unwrap());
        })
    });

    // String substring lookup
    c.bench_function("in_string", |b| {
        b.iter(|| {
            black_box(expr_in_str.eval(&ctx).unwrap());
        })
    });
}

// Benchmark 4: Val cloning and arithmetic operations (large data structures)
fn bench_val_operations(c: &mut Criterion) {
    // Create large Map and List for testing
    let mut large_map = HashMap::new();
    for i in 0..1000 {
        large_map.insert(format!("key{}", i), Val::Int(i));
    }
    // Be explicit to avoid type inference issues in benches
    let val_map: Val = large_map.into();

    let large_list: Vec<Val> = (0..1000).map(Val::Int).collect();
    let val_list = Val::List(Arc::from(large_list));

    let mut small_map = HashMap::new();
    for i in 0..10 {
        small_map.insert(format!("key{}", i), Val::Int(i));
    }
    let val_map_small: Val = small_map.into();

    let small_list: Vec<Val> = (0..10).map(Val::Int).collect();
    let val_list_small = Val::List(Arc::from(small_list));

    // Benchmark Map + Map operations (merge with capacity optimization)
    c.bench_function("map_add_large", |b| {
        b.iter(|| {
            let result = (&val_map + &val_map_small).unwrap();
            black_box(result);
        })
    });

    // Benchmark List + List operations (with capacity optimization)
    c.bench_function("list_add_large", |b| {
        b.iter(|| {
            let result = (&val_list + &val_list_small).unwrap();
            black_box(result);
        })
    });

    // Benchmark List + Val operations (append single element)
    c.bench_function("list_add_element", |b| {
        b.iter(|| {
            let result = (&val_list_small + &Val::Int(999)).unwrap();
            black_box(result);
        })
    });

    // Benchmark List - List operations (set difference with filtering)
    c.bench_function("list_subtract_large", |b| {
        b.iter(|| {
            let result = (&val_list - &val_list_small).unwrap();
            black_box(result);
        })
    });

    // Benchmark Map - Map operations (key removal)
    c.bench_function("map_subtract_large", |b| {
        b.iter(|| {
            let result = (&val_map - &val_map_small).unwrap();
            black_box(result);
        })
    });

    // Benchmark Map - String operations (single key removal)
    c.bench_function("map_subtract_key", |b| {
        b.iter(|| {
            let result = (&val_map - &Val::Str(Arc::from("key50"))).unwrap();
            black_box(result);
        })
    });
}

// Benchmark 5: String concatenation performance
fn bench_string_operations(c: &mut Criterion) {
    let short_str = Val::Str(Arc::from("short"));
    let long_str = Val::Str(Arc::from("a".repeat(1000).as_str()));
    let empty_str = Val::Str(Arc::from(""));
    let number = Val::Int(12345);
    let float = Val::Float(123.456);

    // String + String concatenation (optimized with capacity)
    c.bench_function("string_concat_short", |b| {
        b.iter(|| {
            let result = (&short_str + &short_str).unwrap();
            black_box(result);
        })
    });

    c.bench_function("string_concat_long", |b| {
        b.iter(|| {
            let result = (&long_str + &short_str).unwrap();
            black_box(result);
        })
    });

    // String + empty optimization
    c.bench_function("string_concat_empty", |b| {
        b.iter(|| {
            let result = (&short_str + &empty_str).unwrap();
            black_box(result);
        })
    });

    // String + Number concatenation (with feature flag)
    #[cfg(feature = "adv_arith")]
    {
        c.bench_function("string_concat_number", |b| {
            b.iter(|| {
                let result = (&short_str + &number).unwrap();
                black_box(result);
            })
        });

        c.bench_function("string_concat_float", |b| {
            b.iter(|| {
                let result = (&short_str + &float).unwrap();
                black_box(result);
            })
        });
    }
}

// Benchmark 6: Memory allocation patterns (Vec/HashMap with_capacity vs default)
fn bench_memory_allocation(c: &mut Criterion) {
    let size = 1000;

    // Benchmark HashMap creation with capacity vs without
    c.bench_function("hashmap_with_capacity", |b| {
        b.iter(|| {
            let mut map = HashMap::with_capacity(size);
            for i in 0..size {
                map.insert(format!("key{}", i), Val::Int(i as i64));
            }
            black_box(map);
        })
    });

    c.bench_function("hashmap_default_capacity", |b| {
        b.iter(|| {
            let mut map = HashMap::new();
            for i in 0..size {
                map.insert(format!("key{}", i), Val::Int(i as i64));
            }
            black_box(map);
        })
    });

    // Benchmark Vec creation with capacity vs without
    c.bench_function("vec_with_capacity", |b| {
        b.iter(|| {
            let mut vec = Vec::with_capacity(size);
            for i in 0..size {
                vec.push(Val::Int(i as i64));
            }
            black_box(vec);
        })
    });

    c.bench_function("vec_default_capacity", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..size {
                vec.push(Val::Int(i as i64));
            }
            black_box(vec);
        })
    });
}

// Benchmark 7: Expression evaluation with complex arithmetic (measures overall cloning impact)
fn bench_complex_arithmetic(c: &mut Criterion) {
    let mut ctx_map = HashMap::new();

    // Create large lists for complex operations
    let list1: Vec<Val> = (0..100).map(Val::Int).collect();
    let list2: Vec<Val> = (50..150).map(Val::Int).collect();
    ctx_map.insert("list1".to_string(), Val::List(Arc::from(list1)));
    ctx_map.insert("list2".to_string(), Val::List(Arc::from(list2)));

    // Create large maps for merging operations
    let mut map1 = HashMap::new();
    let mut map2 = HashMap::new();
    for i in 0..50 {
        map1.insert(format!("key{}", i), Val::Int(i));
        map2.insert(format!("key{}", i + 25), Val::Int(i + 25));
    }
    ctx_map.insert("map1".to_string(), Val::from(map1));
    ctx_map.insert("map2".to_string(), Val::from(map2));

    let ctx = Val::from(ctx_map);

    // Complex arithmetic operations that trigger multiple clones
    let expr_list_ops = Expr::parse_cached_arc("@list1 + @list2 - [75, 76, 77]").unwrap();
    let expr_map_ops = Expr::parse_cached_arc("@map1 + @map2 - \"key25\"").unwrap();
    let expr_mixed = Expr::parse_cached_arc("(@list1 + [999]) + (@list2 - [100, 101])").unwrap();

    c.bench_function("complex_list_arithmetic", |b| {
        b.iter(|| {
            let result = expr_list_ops.eval(&ctx).unwrap();
            black_box(result);
        })
    });

    c.bench_function("complex_map_arithmetic", |b| {
        b.iter(|| {
            let result = expr_map_ops.eval(&ctx).unwrap();
            black_box(result);
        })
    });

    c.bench_function("complex_mixed_arithmetic", |b| {
        b.iter(|| {
            let result = expr_mixed.eval(&ctx).unwrap();
            black_box(result);
        })
    });
}

// Criterion benchmark group definition
criterion_group!(
    benches,
    bench_parsing,
    bench_evaluation,
    bench_in_operator,
    bench_val_operations,
    bench_string_operations,
    bench_memory_allocation,
    bench_complex_arithmetic
);
criterion_main!(benches);
