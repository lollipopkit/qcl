use criterion::{Criterion, black_box, criterion_group, criterion_main};
use qcl_core::{expr::Expr, stmt::Stmt, val::Val};

fn make_for_range_function(n: i64, inclusive: bool) -> qcl_core::vm::Function {
    // Build: sum = 0; for i in 0..n { sum = sum + 1 } ; return sum
    let for_iter = Expr::Range {
        start: Some(Box::new(Expr::Val(Val::Int(0)))),
        end: Some(Box::new(Expr::Val(Val::Int(n)))),
        inclusive,
        step: None,
    };
    let program = Stmt::Block {
        statements: vec![
            Box::new(Stmt::Define {
                name: "sum".into(),
                value: Box::new(Expr::Val(Val::Int(0))),
            }),
            Box::new(Stmt::For {
                pattern: qcl_core::stmt::ForPattern::Ignore,
                iterable: Box::new(for_iter),
                body: Box::new(Stmt::Block { statements: vec![] }),
            }),
            Box::new(Stmt::Return {
                value: Some(Box::new(Expr::Var("sum".into()))),
            }),
        ],
    };
    qcl_core::vm::Compiler::new().compile_stmt(&program)
}

fn bc32_for_range_bench(c: &mut Criterion) {
    let fun = make_for_range_function(10_000, false);

    // Clone and force normal path by clearing code32
    #[cfg(feature = "bc32")]
    let mut f_normal = fun.clone();
    #[cfg(feature = "bc32")]
    {
        f_normal.code32 = None;
    }

    // Run bc32 packed path
    c.bench_function("bc32_for_range_packed", |b| {
        b.iter(|| {
            let mut vm = qcl_core::vm::Vm::new();
            let mut env = qcl_core::stmt::Environment::new();
            let out = vm.exec_with(&fun, Some(&mut env), None).unwrap();
            black_box(out);
        })
    });

    // Run enum (unpacked) path
    #[cfg(feature = "bc32")]
    c.bench_function("bc32_for_range_enum", |b| {
        b.iter(|| {
            let mut vm = qcl_core::vm::Vm::new();
            let mut env = qcl_core::stmt::Environment::new();
            let out = vm.exec_with(&f_normal, Some(&mut env), None).unwrap();
            black_box(out);
        })
    });
}

criterion_group!(benches, bc32_for_range_bench);
criterion_main!(benches);
