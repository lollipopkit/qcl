use criterion::{Criterion, black_box, criterion_group, criterion_main};
use lkr_core::{expr::Expr, stmt::Stmt, val::Val};

fn make_while_function(n: i64) -> lkr_core::vm::Function {
    // i = 0; while (i < n) { i = i + 1 }; return i
    let cond = Expr::parse_cached_arc(&format!("i < {}", n)).unwrap();
    let incr = Expr::parse_cached_arc("i + 1").unwrap();

    let program = Stmt::Block {
        statements: vec![
            Box::new(Stmt::Define {
                name: "i".into(),
                value: Box::new(Expr::Val(Val::Int(0))),
            }),
            Box::new(Stmt::While {
                condition: Box::new((*cond).clone()),
                body: Box::new(Stmt::Block {
                    statements: vec![Box::new(Stmt::Assign {
                        name: "i".into(),
                        value: Box::new((*incr).clone()),
                        span: None,
                    })],
                }),
            }),
            Box::new(Stmt::Return {
                value: Some(Box::new(Expr::Var("i".into()))),
            }),
        ],
    };
    lkr_core::vm::Compiler::new().compile_stmt(&program)
}

fn bc32_while_bench(c: &mut Criterion) {
    let f_bc32 = make_while_function(10_000);
    // Force unpacked path copy
    #[cfg(feature = "bc32")]
    let mut f_enum = f_bc32.clone();
    #[cfg(feature = "bc32")]
    {
        f_enum.code32 = None;
    }

    c.bench_function("bc32_while_packed", |b| {
        b.iter(|| {
            let mut vm = lkr_core::vm::Vm::new();
            let mut env = lkr_core::stmt::Environment::new();
            let out = vm.exec_with(&f_bc32, Some(&mut env), None).unwrap();
            black_box(out);
        })
    });

    #[cfg(feature = "bc32")]
    c.bench_function("bc32_while_enum", |b| {
        b.iter(|| {
            let mut vm = lkr_core::vm::Vm::new();
            let mut env = lkr_core::stmt::Environment::new();
            let out = vm.exec_with(&f_enum, Some(&mut env), None).unwrap();
            black_box(out);
        })
    });
}

criterion_group!(benches, bc32_while_bench);
criterion_main!(benches);
