use criterion::{Criterion, black_box, criterion_group, criterion_main};
use qcl_core::{expr::Expr, stmt::Stmt, val::Val};

fn make_packable_function() -> qcl_core::vm::Function {
    // Build expression by parsing: (a[1] + m["k"]) * s.len
    let expr = Expr::parse_cached_arc("(a[1] + m[\"k\"]) * s.len").unwrap();
    let body = Stmt::Expr(Box::new((*expr).clone()));
    let params = vec!["a".to_string(), "s".to_string(), "m".to_string()];
    qcl_core::vm::Compiler::new().compile_function(&params, &body)
}

fn bc32_bench(c: &mut Criterion) {
    // Prepare arguments: a=[10,20,30], s="hello", m={"k":7}
    let a = Val::List(vec![Val::Int(10), Val::Int(20), Val::Int(30)].into());
    let s = Val::Str("hello".into());
    let mut map = std::collections::HashMap::new();
    map.insert("k".to_string(), Val::Int(7));
    let m: Val = map.into();
    let args = [a, s, m];

    // Compile function (will attempt bc32 packing under feature)
    let f_bc32 = make_packable_function();

    // Clone and force normal path by clearing code32
    #[cfg(feature = "bc32")]
    let mut f_normal = f_bc32.clone();
    #[cfg(feature = "bc32")]
    {
        f_normal.code32 = None;
    }

    // Run with bc32 (direct packed dispatch) when available
    c.bench_function("vm_exec_bc32_packed", |b| {
        b.iter(|| {
            let mut vm = qcl_core::vm::Vm::new();
            let mut env = qcl_core::stmt::Environment::new();
            let out = vm.exec_with(&f_bc32, Some(&mut env), Some(&args)).unwrap();
            black_box(out);
        })
    });

    // Run with normal Op enum dispatch
    #[cfg(feature = "bc32")]
    c.bench_function("vm_exec_enum_unpacked", |b| {
        b.iter(|| {
            let mut vm = qcl_core::vm::Vm::new();
            let mut env = qcl_core::stmt::Environment::new();
            let out = vm.exec_with(&f_normal, Some(&mut env), Some(&args)).unwrap();
            black_box(out);
        })
    });
}

criterion_group!(benches, bc32_bench);
criterion_main!(benches);
