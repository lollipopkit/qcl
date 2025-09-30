#[cfg(test)]
mod tests {
    use crate::{expr::Expr, val::Val};

    #[test]
    fn test_vm_const_bool_and_or() {
        let compiler = crate::vm::Compiler::new();

        // true && false => false
        let expr = Expr::And(
            Box::new(Expr::Val(Val::Bool(true))),
            Box::new(Expr::Val(Val::Bool(false))),
        );
        let fun = compiler.compile_expr(&expr);
        let mut vm = crate::vm::Vm::new();
        let out = vm.exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(false));

        // true || false => true
        let expr = Expr::Or(
            Box::new(Expr::Val(Val::Bool(true))),
            Box::new(Expr::Val(Val::Bool(false))),
        );
        let fun = compiler.compile_expr(&expr);
        let out = vm.exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(true));
    }

    #[test]
    fn test_vm_int_add() {
        let compiler = crate::vm::Compiler::new();
        let expr = Expr::Bin(
            Box::new(Expr::Val(Val::Int(2))),
            crate::op::BinOp::Add,
            Box::new(Expr::Val(Val::Int(40))),
        );
        let fun = compiler.compile_expr(&expr);
        let mut vm = crate::vm::Vm::new();
        let out = vm.exec(&fun).unwrap();
        assert_eq!(out, Val::Int(42));
    }

    #[test]
    fn test_vm_stmt_block_if_while_return() {
        use crate::stmt::Stmt;
        use crate::op::BinOp;

        // {
        //   x = 0;
        //   i = 0;
        //   while (i < 3) {
        //     x = x + 2;
        //     i = i + 1;
        //   }
        //   if (x == 6) { return x; } else { return 0; }
        // }
        let block = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::While {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("i".into())),
                        BinOp::Lt,
                        Box::new(Expr::Val(Val::Int(3))),
                    )),
                    body: Box::new(Stmt::Block {
                        statements: vec![
                            Box::new(Stmt::Assign {
                                name: "x".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("x".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Val(Val::Int(2))),
                                )),
                                span: None,
                            }),
                            Box::new(Stmt::Assign {
                                name: "i".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("i".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Val(Val::Int(1))),
                                )),
                                span: None,
                            }),
                        ],
                    }),
                }),
                Box::new(Stmt::If {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("x".into())),
                        BinOp::Eq,
                        Box::new(Expr::Val(Val::Int(6))),
                    )),
                    then_stmt: Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
                    else_stmt: Some(Box::new(Stmt::Return { value: Some(Box::new(Expr::Val(Val::Int(0)))) })),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&block);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(6));
    }
}
