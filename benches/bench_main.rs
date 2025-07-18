use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qcl::expr::Expr;
use qcl::val::Val;
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
    ctx_map.insert("smalllist".to_string(), (1..=10).map(Val::Int).collect::<Vec<_>>().into());
    ctx_map.insert("biglist".to_string(), (0..1000).map(Val::Int).collect::<Vec<_>>().into());
    // Map and keys
    ctx_map.insert("key".to_string(), Val::Str(Arc::from("key50")));
    let mut bigmap_inner = HashMap::new();
    for i in 0..100 {
        bigmap_inner.insert(format!("key{}", i), Val::Int(i));
    }
    ctx_map.insert("bigmap".to_string(), bigmap_inner.into());
    // Long string
    let big_string = "a".repeat(1000) + "z";
    ctx_map.insert("bigstr".to_string(), Val::Str(Arc::from(big_string.as_str())));
    Val::Map(Arc::new(ctx_map))
}

// Benchmark 1: Expression parsing performance (without cache vs with cache)
fn bench_parsing(c: &mut Criterion) {
    let expr_str = "(@user.age + 2) * (3 + 4) && @user.name == \"Alice\" || [1, 2, 3].1 in [0, 1, 2]";
    
    // Parsing without cache
    c.bench_function("parse_without_cache", |b| {
        b.iter(|| {
            let tokens = qcl::token::Tokenizer::new(expr_str).unwrap();
            let expr = qcl::ast::Parser::new(&tokens).parse().unwrap();
            black_box(&expr);
        })
    });
    
    // Parsing with cache (warm up cache then repeatedly parse same expression)
    let _ = Expr::parse_cached(expr_str).unwrap();  // Warm up cache
    c.bench_function("parse_with_cache", |b| {
        b.iter(|| {
            let expr = Expr::parse_cached(expr_str).unwrap();
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
    let expr_constant = Expr::parse_cached(expr_const_str).unwrap();      // Will fold to a constant
    let expr_nonconstant = Expr::parse_cached(&expr_nonconst_str).unwrap();  // Keep chain of @x additions
    
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
    let expr_in_list_small = Expr::parse_cached("@val_small in @smalllist").unwrap();
    let expr_in_list_large = Expr::parse_cached("@val_large in @biglist").unwrap();
    let expr_in_map = Expr::parse_cached("@key in @bigmap").unwrap();
    let expr_in_str = Expr::parse_cached("\"z\" in @bigstr").unwrap();
    
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

// Criterion benchmark group definition
criterion_group!(benches, bench_parsing, bench_evaluation, bench_in_operator);
criterion_main!(benches);