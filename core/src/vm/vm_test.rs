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
    fn test_bc32_for_range_ascending_exclusive() {
        use crate::stmt::{ForPattern, Stmt};
        // Program: x=0; for _ in 0..3 { x = x + 1 } return x
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range { start: Some(Box::new(Expr::Val(Val::Int(0)))), end: Some(Box::new(Expr::Val(Val::Int(3)))), inclusive: false, step: None }),
                    body: Box::new(Stmt::Block { statements: vec![ Box::new(Stmt::Assign { name: "x".into(), value: Box::new(Expr::Bin(Box::new(Expr::Var("x".into())), crate::op::BinOp::Add, Box::new(Expr::Val(Val::Int(1))))), span: None }) ] }),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        #[cfg(feature = "bc32")]
        {
            assert!(fun.code32.is_some(), "function should be bc32-packed");
            let code32 = fun.code32.as_ref().unwrap();
            let mut seen_prep = 0;
            let mut seen_guard = 0;
            let mut seen_step = 0;
            for &w in code32.iter() {
                let tag = crate::vm::tag_of(w);
                if tag == crate::vm::TAG_FOR_RANGE_PREP { seen_prep += 1; }
                if tag == crate::vm::TAG_FOR_RANGE_GUARD { seen_guard += 1; }
                if tag == crate::vm::TAG_FOR_RANGE_STEP { seen_step += 1; }
            }
            assert!(seen_prep >= 1 && seen_guard >= 1 && seen_step >= 1, "expected ForRange* tags present in bc32 stream");
        }
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_bc32_for_range_descending_inclusive_with_step() {
        use crate::stmt::{ForPattern, Stmt};
        // Program: x=0; for _ in 5..=1 step -2 { x = x + 1 } return x
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range { start: Some(Box::new(Expr::Val(Val::Int(5)))), end: Some(Box::new(Expr::Val(Val::Int(1)))), inclusive: true, step: Some(Box::new(Expr::Val(Val::Int(-2)))) }),
                    body: Box::new(Stmt::Block { statements: vec![ Box::new(Stmt::Assign { name: "x".into(), value: Box::new(Expr::Bin(Box::new(Expr::Var("x".into())), crate::op::BinOp::Add, Box::new(Expr::Val(Val::Int(1))))), span: None }) ] }),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        #[cfg(feature = "bc32")]
        assert!(fun.code32.is_some(), "function should be bc32-packed");
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        // 5,3,1 => 3 iterations
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_bc32_for_range_explicit_step_inclusive() {
        use crate::stmt::{ForPattern, Stmt};
        // Program: x=0; for _ in 0..=10 step 2 { x = x + 1 } return x
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range { start: Some(Box::new(Expr::Val(Val::Int(0)))), end: Some(Box::new(Expr::Val(Val::Int(10)))), inclusive: true, step: Some(Box::new(Expr::Val(Val::Int(2)))) }),
                    body: Box::new(Stmt::Block { statements: vec![ Box::new(Stmt::Assign { name: "x".into(), value: Box::new(Expr::Bin(Box::new(Expr::Var("x".into())), crate::op::BinOp::Add, Box::new(Expr::Val(Val::Int(1))))), span: None }) ] }),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        #[cfg(feature = "bc32")]
        assert!(fun.code32.is_some(), "function should be bc32-packed");
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(6));
    }

    #[test]
    fn test_bc32_long_or_uses_extended_true_set() {
        use crate::op::BinOp;
        use crate::vm::Compiler;
        // Build a long right-hand expression to force an i16 jump distance
        let mut rhs = Expr::Bin(
            Box::new(Expr::Var("x".into())),
            BinOp::Add,
            Box::new(Expr::Val(Val::Int(1))),
        );
        for _ in 0..100 {
            rhs = Expr::Bin(Box::new(rhs), BinOp::Add, Box::new(Expr::Val(Val::Int(1))));
        }
        let expr = Expr::Or(Box::new(Expr::Val(Val::Bool(true))), Box::new(rhs));
        let fun = Compiler::new().compile_expr(&expr);
        #[cfg(feature = "bc32")]
        {
            let code32 = fun.code32.as_ref().expect("function should be bc32-packed");
            let mut found_ext = false;
            for &w in code32.iter() {
                let tag = crate::vm::tag_of(w);
                if tag == crate::vm::TAG_JMP_TRUE_SET_X { found_ext = true; break; }
            }
            assert!(found_ext, "expected extended JmpTrueSetX to be present for long OR");
        }
        // Execution should short-circuit to true
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(true));
    }

    #[test]
    fn test_bc32_long_and_uses_extended_false_set() {
        use crate::op::BinOp;
        use crate::vm::Compiler;
        let mut rhs = Expr::Bin(
            Box::new(Expr::Var("y".into())),
            BinOp::Add,
            Box::new(Expr::Val(Val::Int(1))),
        );
        for _ in 0..100 {
            rhs = Expr::Bin(Box::new(rhs), BinOp::Add, Box::new(Expr::Val(Val::Int(1))));
        }
        let expr = Expr::And(Box::new(Expr::Val(Val::Bool(false))), Box::new(rhs));
        let fun = Compiler::new().compile_expr(&expr);
        #[cfg(feature = "bc32")]
        {
            let code32 = fun.code32.as_ref().expect("function should be bc32-packed");
            let mut found_ext = false;
            for &w in code32.iter() {
                let tag = crate::vm::tag_of(w);
                if tag == crate::vm::TAG_JMP_FALSE_SET_X { found_ext = true; break; }
            }
            assert!(found_ext, "expected extended JmpFalseSetX to be present for long AND");
        }
        // Execution should short-circuit to false
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(false));
    }

    #[test]
    fn test_bc32_long_nullish_uses_extended_pick() {
        use crate::op::BinOp;
        use crate::vm::Compiler;
        // left is a variable (undefined -> nil at runtime), forcing NullishPick emission
        // right is a long chain to push the jump distance beyond i8
        let mut rhs = Expr::Bin(
            Box::new(Expr::Var("x".into())),
            BinOp::Add,
            Box::new(Expr::Val(Val::Int(0))),
        );
        for _ in 0..100 {
            rhs = Expr::Bin(Box::new(rhs), BinOp::Add, Box::new(Expr::Val(Val::Int(0))));
        }
        let expr = Expr::NullishCoalescing(Box::new(Expr::Var("u".into())), Box::new(rhs));
        let fun = Compiler::new().compile_expr(&expr);
        #[cfg(feature = "bc32")]
        {
            let code32 = fun.code32.as_ref().expect("function should be bc32-packed");
            let mut found_ext = false;
            for &w in code32.iter() {
                let tag = crate::vm::tag_of(w);
                if tag == crate::vm::TAG_NULLISH_PICK_X { found_ext = true; break; }
            }
            assert!(found_ext, "expected extended NullishPickX to be present for long nullish coalescing");
        }
        // Execution: define x so rhs evaluates cleanly; u is undefined -> nil so rhs taken
        let mut env = crate::stmt::Environment::new();
        env.define("x".into(), Val::Int(42));
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        // Result should be 42 given repeated +0
        assert_eq!(out, Val::Int(42));
    }

    #[test]
    fn test_vm_global_ic_multiple_redefines() {
        use crate::stmt::Stmt;
        use crate::vm::Compiler;
        // Program:
        //   x = 1; _ = x; x = 2; _ = x; x = 3; return x
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(1))) }),
                Box::new(Stmt::Expr(Box::new(Expr::Var("x".into())))),
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(2))) }),
                Box::new(Stmt::Expr(Box::new(Expr::Var("x".into())))),
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(3))) }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
            ],
        };
        let fun = Compiler::new().compile_stmt(&program);
        let mut env = crate::stmt::Environment::new();
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(3));
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
        use crate::op::BinOp;
        use crate::stmt::Stmt;

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
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::Define {
                    name: "i".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
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
                    then_stmt: Box::new(Stmt::Return {
                        value: Some(Box::new(Expr::Var("x".into()))),
                    }),
                    else_stmt: Some(Box::new(Stmt::Return {
                        value: Some(Box::new(Expr::Val(Val::Int(0)))),
                    })),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&block);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(6));
    }

    #[test]
    fn test_vm_for_range_numeric() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // x = 0; for i in 0..3 { x = x + 1; } return x;
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Variable("i".into()),
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(3)))),
                        inclusive: false,
                        step: None,
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let mut vm = crate::vm::Vm::new();
        let mut env = crate::stmt::Environment::new();
        let out = vm.exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_vm_for_range_descending_exclusive() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // x = 0; for _ in 3..0 { x = x + 1; } return x;  // visits 3,2,1
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(3)))),
                        end: Some(Box::new(Expr::Val(Val::Int(0)))),
                        inclusive: false,
                        step: None,
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_vm_for_range_inclusive_and_edges() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // Inclusive ascending: 0..=3 -> 4 iterations
        let prog_inc = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(3)))),
                        inclusive: true,
                        step: None,
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_inc);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(4));

        // start == end exclusive -> 0 iterations
        let prog_zero = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(2)))),
                        end: Some(Box::new(Expr::Val(Val::Int(2)))),
                        inclusive: false,
                        step: None,
                    }),
                    body: Box::new(Stmt::Block { statements: vec![] }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_zero);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(0));

        // start == end inclusive -> 1 iteration
        let prog_one = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(2)))),
                        end: Some(Box::new(Expr::Val(Val::Int(2)))),
                        inclusive: true,
                        step: None,
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_one);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(1));
    }

    #[test]
    fn test_vm_for_range_with_explicit_step() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // Ascending exclusive with step 2: 0..10..2 -> 0,2,4,6,8 => 5 iterations
        let prog_step2 = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(10)))),
                        inclusive: false,
                        step: Some(Box::new(Expr::Val(Val::Int(2)))),
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_step2);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(5));

        // Ascending inclusive with step 2: 0..=10..2 -> includes 10 => 6 iterations
        let prog_step2_inc = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(10)))),
                        inclusive: true,
                        step: Some(Box::new(Expr::Val(Val::Int(2)))),
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_step2_inc);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(6));

        // Descending with negative step: 5..0..-2 -> 5,3,1 => 3 iterations
        let prog_desc = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(5)))),
                        end: Some(Box::new(Expr::Val(Val::Int(0)))),
                        inclusive: false,
                        step: Some(Box::new(Expr::Val(Val::Int(-2)))),
                    }),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "x".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("x".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&prog_desc);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_vm_for_list_tuple_pattern() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // sum = 0; for (a,b) in [[1,2],[3,4]] { sum = sum + a; sum = sum + b; } return sum;
        let iter = Expr::List(vec![
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(1))),
                Box::new(Expr::Val(Val::Int(2))),
            ])),
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(3))),
                Box::new(Expr::Val(Val::Int(4))),
            ])),
        ]);
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "sum".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Tuple(vec![
                        ForPattern::Variable("a".into()),
                        ForPattern::Variable("b".into()),
                    ]),
                    iterable: Box::new(iter),
                    body: Box::new(Stmt::Block {
                        statements: vec![
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Var("a".into())),
                                )),
                                span: None,
                            }),
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Var("b".into())),
                                )),
                                span: None,
                            }),
                        ],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("sum".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(10));
    }

    #[test]
    fn test_vm_for_list_array_rest_pattern() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // sum = 0; for [a, ..rest] in [[1,2,3],[4],[5,6]] { sum += a; sum += rest.len } return sum;
        let iter = Expr::List(vec![
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(1))),
                Box::new(Expr::Val(Val::Int(2))),
                Box::new(Expr::Val(Val::Int(3))),
            ])),
            Box::new(Expr::List(vec![Box::new(Expr::Val(Val::Int(4)))])),
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(5))),
                Box::new(Expr::Val(Val::Int(6))),
            ])),
        ]);
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "sum".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Array {
                        patterns: vec![ForPattern::Variable("a".into())],
                        rest: Some("rest".into()),
                    },
                    iterable: Box::new(iter),
                    body: Box::new(Stmt::Block {
                        statements: vec![
                            // sum = sum + a
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Var("a".into())),
                                )),
                                span: None,
                            }),
                            // sum = sum + rest.len
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Access(
                                        Box::new(Expr::Var("rest".into())),
                                        Box::new(Expr::Val(Val::Str("len".into()))),
                                    )),
                                )),
                                span: None,
                            }),
                        ],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("sum".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(13));
    }

    #[test]
    fn test_vm_optional_and_nullish_nested() {
        use crate::vm::Compiler;

        // Optional access: {"a": {}}?.a?.b -> nil
        let expr = Expr::OptionalAccess(
            Box::new(Expr::OptionalAccess(
                Box::new(Expr::Map(vec![(
                    Box::new(Expr::Val(Val::Str("a".into()))),
                    Box::new(Expr::Map(vec![])),
                )])),
                Box::new(Expr::Val(Val::Str("a".into()))),
            )),
            Box::new(Expr::Val(Val::Str("b".into()))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Nil);

        // Nullish coalescing with nesting: nil ?? (nil ?? 3) -> 3
        let expr = Expr::NullishCoalescing(
            Box::new(Expr::Val(Val::Nil)),
            Box::new(Expr::NullishCoalescing(
                Box::new(Expr::Val(Val::Nil)),
                Box::new(Expr::Val(Val::Int(3))),
            )),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(3));

        // Short-circuit: true || (expensive) -> true; false && (expensive) -> false
        let expr = Expr::Or(
            Box::new(Expr::Val(Val::Bool(true))),
            Box::new(Expr::Bin(
                Box::new(Expr::Val(Val::Int(1))),
                crate::op::BinOp::Div,
                Box::new(Expr::Val(Val::Int(0))),
            )),
        );
        let fun = Compiler::new().compile_expr(&expr);
        // Division by zero would normally be undefined, but we rely on short-circuit to avoid eval; expect true
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(true));

        let expr = Expr::And(
            Box::new(Expr::Val(Val::Bool(false))),
            Box::new(Expr::Bin(
                Box::new(Expr::Val(Val::Int(1))),
                crate::op::BinOp::Div,
                Box::new(Expr::Val(Val::Int(0))),
            )),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(false));
    }

    #[test]
    fn test_vm_nullish_non_nil_values() {
        use crate::vm::Compiler;

        // false ?? 5 -> false (do not coalesce)
        let expr = Expr::NullishCoalescing(
            Box::new(Expr::Val(Val::Bool(false))),
            Box::new(Expr::Val(Val::Int(5))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Bool(false));

        // 0 ?? 5 -> 0
        let expr = Expr::NullishCoalescing(
            Box::new(Expr::Val(Val::Int(0))),
            Box::new(Expr::Val(Val::Int(5))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(0));

        // "" ?? 7 -> ""
        let expr = Expr::NullishCoalescing(
            Box::new(Expr::Val(Val::Str("".into()))),
            Box::new(Expr::Val(Val::Int(7))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Str("".into()));
    }

    #[test]
    fn test_vm_nullish_short_circuit_right_not_evaluated() {
        use crate::vm::Compiler;

        // 1 ?? (1 / 0) -> 1, and must not evaluate right (division by zero)
        let expr = Expr::NullishCoalescing(
            Box::new(Expr::Val(Val::Int(1))),
            Box::new(Expr::Bin(
                Box::new(Expr::Val(Val::Int(1))),
                crate::op::BinOp::Div,
                Box::new(Expr::Val(Val::Int(0))),
            )),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(1));
    }

    #[test]
    fn test_vm_indexk_negative_and_overflow() {
        use crate::vm::Compiler;

        // [1,2][ -1 ] -> nil
        let expr = Expr::Access(
            Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(1))),
                Box::new(Expr::Val(Val::Int(2))),
            ])),
            Box::new(Expr::Val(Val::Int(-1))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Nil);

        // "ab"[ 5 ] -> nil
        let expr = Expr::Access(
            Box::new(Expr::Val(Val::Str("ab".into()))),
            Box::new(Expr::Val(Val::Int(5))),
        );
        let fun = Compiler::new().compile_expr(&expr);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Nil);
    }

    #[test]
    fn test_vm_index_dynamic_list_negative_overflow() {
        use crate::stmt::Stmt;
        // l = [1,2]; i = -1; return l[i] -> nil
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "l".into(), value: Box::new(Expr::List(vec![
                    Box::new(Expr::Val(Val::Int(1))), Box::new(Expr::Val(Val::Int(2)))
                ])) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(-1))) }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Access(
                    Box::new(Expr::Var("l".into())), Box::new(Expr::Var("i".into()))
                ))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Nil);

        // i = 5 (overflow) -> nil
        let program2 = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "l".into(), value: Box::new(Expr::List(vec![
                    Box::new(Expr::Val(Val::Int(1))), Box::new(Expr::Val(Val::Int(2)))
                ])) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(5))) }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Access(
                    Box::new(Expr::Var("l".into())), Box::new(Expr::Var("i".into()))
                ))) }),
            ],
        };
        let fun2 = crate::vm::Compiler::new().compile_stmt(&program2);
        let out2 = crate::vm::Vm::new().exec(&fun2).unwrap();
        assert_eq!(out2, Val::Nil);
    }

    #[test]
    fn test_vm_index_dynamic_string_negative_overflow() {
        use crate::stmt::Stmt;
        // s = "ab"; i = -1; return s[i] -> nil
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "s".into(), value: Box::new(Expr::Val(Val::Str("ab".into()))) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(-1))) }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Access(
                    Box::new(Expr::Var("s".into())), Box::new(Expr::Var("i".into()))
                ))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Nil);

        // i = 5 (overflow) -> nil
        let program2 = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "s".into(), value: Box::new(Expr::Val(Val::Str("ab".into()))) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(5))) }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Access(
                    Box::new(Expr::Var("s".into())), Box::new(Expr::Var("i".into()))
                ))) }),
            ],
        };
        let fun2 = crate::vm::Compiler::new().compile_stmt(&program2);
        let out2 = crate::vm::Vm::new().exec(&fun2).unwrap();
        assert_eq!(out2, Val::Nil);
    }

    #[test]
    fn test_vm_global_ic_invalidation_on_define() {
        use crate::stmt::{Environment, Stmt};
        use crate::vm::Compiler;

        // Program: warm LoadGlobal(x); Define x=2; return x
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Expr(Box::new(Expr::Var("x".into())))),
                Box::new(Stmt::Define {
                    name: "x".into(),
                    value: Box::new(Expr::Val(Val::Int(2))),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("x".into()))),
                }),
            ],
        };

        let fun = Compiler::new().compile_stmt(&program);
        let mut env = Environment::new();
        env.define("x".into(), Val::Int(1));

        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(2));
    }

    #[test]
    fn test_vm_access_ic_identity_change_miss() {
        use crate::stmt::{Environment, Stmt};
        use crate::vm::Compiler;

        // Program:
        //   m = {"a":1}; m.a; m = {"a":2}; return m.a
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "m".into(),
                    value: Box::new(Expr::Map(vec![(
                        Box::new(Expr::Val(Val::Str("a".into()))),
                        Box::new(Expr::Val(Val::Int(1))),
                    )])),
                }),
                // Warm IC
                Box::new(Stmt::Expr(Box::new(Expr::Access(
                    Box::new(Expr::Var("m".into())),
                    Box::new(Expr::Val(Val::Str("a".into()))),
                )))),
                // Replace m with a new map (different Arc identity)
                Box::new(Stmt::Define {
                    name: "m".into(),
                    value: Box::new(Expr::Map(vec![(
                        Box::new(Expr::Val(Val::Str("a".into()))),
                        Box::new(Expr::Val(Val::Int(2))),
                    )])),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Access(
                        Box::new(Expr::Var("m".into())),
                        Box::new(Expr::Val(Val::Str("a".into()))),
                    ))),
                }),
            ],
        };

        let fun = Compiler::new().compile_stmt(&program);
        let mut env = Environment::new();
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(2));
    }

    // Large CALL packing case covered elsewhere; skipped here to keep CI fast.

    #[test]
    fn test_vm_for_list_object_pattern() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // sum = 0; for {x:a, y:b} in [{"x":1,"y":2},{"x":3,"y":4}] { sum += a; sum += b } return sum;
        let iter = Expr::List(vec![
            Box::new(Expr::Map(vec![
                (
                    Box::new(Expr::Val(Val::Str("x".into()))),
                    Box::new(Expr::Val(Val::Int(1))),
                ),
                (
                    Box::new(Expr::Val(Val::Str("y".into()))),
                    Box::new(Expr::Val(Val::Int(2))),
                ),
            ])),
            Box::new(Expr::Map(vec![
                (
                    Box::new(Expr::Val(Val::Str("x".into()))),
                    Box::new(Expr::Val(Val::Int(3))),
                ),
                (
                    Box::new(Expr::Val(Val::Str("y".into()))),
                    Box::new(Expr::Val(Val::Int(4))),
                ),
            ])),
        ]);
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "sum".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Object(vec![
                        ("x".into(), ForPattern::Variable("a".into())),
                        ("y".into(), ForPattern::Variable("b".into())),
                    ]),
                    iterable: Box::new(iter),
                    body: Box::new(Stmt::Block {
                        statements: vec![
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Var("a".into())),
                                )),
                                span: None,
                            }),
                            Box::new(Stmt::Assign {
                                name: "sum".into(),
                                value: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("sum".into())),
                                    BinOp::Add,
                                    Box::new(Expr::Var("b".into())),
                                )),
                                span: None,
                            }),
                        ],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("sum".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(10));
    }

    #[test]
    fn test_vm_for_string_iter_count() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // n = 0; for ch in "abcd" { n = n + 1 } return n; => 4
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "n".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Variable("ch".into()),
                    iterable: Box::new(Expr::Val(Val::Str("abcd".into()))),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "n".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("n".into())),
                                BinOp::Add,
                                Box::new(Expr::Val(Val::Int(1))),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("n".into()))),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(4));
    }

    #[test]
    fn test_vm_for_map_pairs_sum_values() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};

        // sum = 0; for (k,v) in {"a":1, "b":2, "c":3} { sum += v } return sum; => 6
        let map = Expr::Map(vec![
            (
                Box::new(Expr::Val(Val::Str("a".into()))),
                Box::new(Expr::Val(Val::Int(1))),
            ),
            (
                Box::new(Expr::Val(Val::Str("b".into()))),
                Box::new(Expr::Val(Val::Int(2))),
            ),
            (
                Box::new(Expr::Val(Val::Str("c".into()))),
                Box::new(Expr::Val(Val::Int(3))),
            ),
        ]);
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "sum".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Tuple(vec![
                        ForPattern::Variable("k".into()),
                        ForPattern::Variable("v".into()),
                    ]),
                    iterable: Box::new(map),
                    body: Box::new(Stmt::Block {
                        statements: vec![Box::new(Stmt::Assign {
                            name: "sum".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("sum".into())),
                                BinOp::Add,
                                Box::new(Expr::Var("v".into())),
                            )),
                            span: None,
                        })],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("sum".into()))),
                }),
            ],
        };

        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(6));
    }

    #[test]
    fn test_vm_call_many_args_packing_255() {
        use crate::stmt::Environment;
        use crate::vm::Compiler;
        // Define a Rust function that returns argc
        fn argc(args: &[Val], _env: &Environment) -> anyhow::Result<Val> {
            Ok(Val::Int(args.len() as i64))
        }
        // Build expression: argc(0,1,2,...,254) => 255
        let mut args: Vec<Box<Expr>> = Vec::with_capacity(255);
        for i in 0..255 {
            args.push(Box::new(Expr::Val(Val::Int(i))));
        }
        let expr = Expr::Call("argc".into(), args);
        let fun = Compiler::new().compile_expr(&expr);

        // Prepare env with argc
        let mut env = Environment::new();
        env.define("argc".into(), Val::RustFunction(argc));

        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(255));
    }

    #[test]
    fn test_vm_map_iteration_order_stable() {
        use crate::stmt::{Environment, ForPattern, Stmt};
        use crate::vm::Compiler;
        // Define a Rust function update(hash:Int, key:String) -> Int accumulating key order as digits
        fn update(args: &[Val], _env: &Environment) -> anyhow::Result<Val> {
            use anyhow::anyhow;
            if args.len() != 2 {
                return Err(anyhow!("update expects 2 args"));
            }
            let h = match &args[0] {
                Val::Int(i) => *i,
                other => return Err(anyhow!("hash must be Int, got {:?}", other)),
            };
            let k = match &args[1] {
                Val::Str(s) => s.as_ref(),
                other => return Err(anyhow!("key must be String, got {:?}", other)),
            };
            let digit = match k {
                "a" => 1,
                "b" => 2,
                "c" => 3,
                _ => 9,
            };
            Ok(Val::Int(h * 10 + digit))
        }
        // Program: hash = 0; for (k, _v) in {b:2, a:1, c:3} { hash = update(hash, k) } return hash
        let map = Expr::Map(vec![
            (
                Box::new(Expr::Val(Val::Str("b".into()))),
                Box::new(Expr::Val(Val::Int(2))),
            ),
            (
                Box::new(Expr::Val(Val::Str("a".into()))),
                Box::new(Expr::Val(Val::Int(1))),
            ),
            (
                Box::new(Expr::Val(Val::Str("c".into()))),
                Box::new(Expr::Val(Val::Int(3))),
            ),
        ]);
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define {
                    name: "hash".into(),
                    value: Box::new(Expr::Val(Val::Int(0))),
                }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Tuple(vec![ForPattern::Variable("k".into()), ForPattern::Ignore]),
                    iterable: Box::new(map),
                    body: Box::new(Stmt::Block {
                        statements: vec![
                            // hash = update(hash, k)
                            Box::new(Stmt::Assign {
                                name: "hash".into(),
                                value: Box::new(Expr::CallExpr(
                                    Box::new(Expr::Var("update".into())),
                                    vec![Box::new(Expr::Var("hash".into())), Box::new(Expr::Var("k".into()))],
                                )),
                                span: None,
                            }),
                        ],
                    }),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Var("hash".into()))),
                }),
            ],
        };

        let fun = Compiler::new().compile_stmt(&program);
        let mut env = Environment::new();
        env.define("update".into(), Val::RustFunction(update));
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        // Keys should be iterated in sorted order: a,b,c -> hash 123
        assert_eq!(out, Val::Int(123));
    }

    #[test]
    fn test_bc32_for_range_step_zero_errors() {
        use crate::stmt::{ForPattern, Stmt};
        // Program: for _ in 0..10 step 0 { } -> should error
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(10)))),
                        inclusive: false,
                        step: Some(Box::new(Expr::Val(Val::Int(0)))),
                    }),
                    body: Box::new(Stmt::Block { statements: vec![] }),
                }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let mut vm = crate::vm::Vm::new();
        let mut env = crate::stmt::Environment::new();
        let res = vm.exec_with(&fun, Some(&mut env), None);
        assert!(res.is_err(), "expected error for zero step in for-range");
    }

    #[test]
    fn test_vm_index_ic_list_identity_replacement() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};
        // Program:
        //   sum = 0; lst = [];
        //   for i in 0..3 { lst = [i, 99]; sum = sum + lst[0]; }
        //   return sum
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "sum".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::Define { name: "lst".into(), value: Box::new(Expr::List(vec![])) }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Variable("i".into()),
                    iterable: Box::new(Expr::Range { start: Some(Box::new(Expr::Val(Val::Int(0)))), end: Some(Box::new(Expr::Val(Val::Int(3)))), inclusive: false, step: None }),
                    body: Box::new(Stmt::Block { statements: vec![
                        Box::new(Stmt::Assign { name: "lst".into(), value: Box::new(Expr::List(vec![
                            Box::new(Expr::Var("i".into())),
                            Box::new(Expr::Val(Val::Int(99)))
                        ])), span: None }),
                        Box::new(Stmt::Assign { name: "sum".into(), value: Box::new(Expr::Bin(
                            Box::new(Expr::Var("sum".into())), BinOp::Add,
                            Box::new(Expr::Access(Box::new(Expr::Var("lst".into())), Box::new(Expr::Val(Val::Int(0)))))
                        )), span: None }),
                    ] }),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("sum".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        // Expect 0 + 1 + 2 = 3 if cache invalidates on identity change correctly
        assert_eq!(out, Val::Int(3));
    }

    #[test]
    fn test_vm_recursive_function_factorial() {
        use crate::op::BinOp;
        use crate::stmt::Stmt;
        use crate::vm::Compiler;

        // function fact(n) { if (n <= 1) return 1; return n * fact(n-1); }
        // return fact(5)
        let fact_body = Stmt::Block {
            statements: vec![
                Box::new(Stmt::If {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("n".into())),
                        BinOp::Le,
                        Box::new(Expr::Val(Val::Int(1))),
                    )),
                    then_stmt: Box::new(Stmt::Return {
                        value: Some(Box::new(Expr::Val(Val::Int(1)))),
                    }),
                    else_stmt: None,
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Bin(
                        Box::new(Expr::Var("n".into())),
                        BinOp::Mul,
                        Box::new(Expr::Call(
                            "fact".into(),
                            vec![Box::new(Expr::Bin(
                                Box::new(Expr::Var("n".into())),
                                BinOp::Sub,
                                Box::new(Expr::Val(Val::Int(1))),
                            ))],
                        )),
                    ))),
                }),
            ],
        };

        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Function {
                    name: "fact".into(),
                    params: vec!["n".into()],
                    param_types: vec![None],
                    return_type: None,
                    body: Box::new(fact_body),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Call(
                        "fact".into(),
                        vec![Box::new(Expr::Val(Val::Int(5)))],
                    ))),
                }),
            ],
        };

        let fun = Compiler::new().compile_stmt(&program);
        let mut env = crate::stmt::Environment::new();
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Int(120));
    }

    #[test]
    fn test_vm_mutual_recursion_even_odd() {
        use crate::op::BinOp;
        use crate::stmt::Stmt;
        use crate::vm::Compiler;

        // is_even(n) { if (n == 0) return true; return is_odd(n-1); }
        // is_odd(n) { if (n == 0) return false; return is_even(n-1); }
        // return is_even(10) && !is_even(11)
        let is_even_body = Stmt::Block {
            statements: vec![
                Box::new(Stmt::If {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("n".into())),
                        BinOp::Eq,
                        Box::new(Expr::Val(Val::Int(0))),
                    )),
                    then_stmt: Box::new(Stmt::Return {
                        value: Some(Box::new(Expr::Val(Val::Bool(true)))),
                    }),
                    else_stmt: None,
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Call(
                        "is_odd".into(),
                        vec![Box::new(Expr::Bin(
                            Box::new(Expr::Var("n".into())),
                            BinOp::Sub,
                            Box::new(Expr::Val(Val::Int(1))),
                        ))],
                    ))),
                }),
            ],
        };
        let is_odd_body = Stmt::Block {
            statements: vec![
                Box::new(Stmt::If {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("n".into())),
                        BinOp::Eq,
                        Box::new(Expr::Val(Val::Int(0))),
                    )),
                    then_stmt: Box::new(Stmt::Return {
                        value: Some(Box::new(Expr::Val(Val::Bool(false)))),
                    }),
                    else_stmt: None,
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::Call(
                        "is_even".into(),
                        vec![Box::new(Expr::Bin(
                            Box::new(Expr::Var("n".into())),
                            BinOp::Sub,
                            Box::new(Expr::Val(Val::Int(1))),
                        ))],
                    ))),
                }),
            ],
        };

        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Function {
                    name: "is_even".into(),
                    params: vec!["n".into()],
                    param_types: vec![None],
                    return_type: None,
                    body: Box::new(is_even_body),
                }),
                Box::new(Stmt::Function {
                    name: "is_odd".into(),
                    params: vec!["n".into()],
                    param_types: vec![None],
                    return_type: None,
                    body: Box::new(is_odd_body),
                }),
                Box::new(Stmt::Return {
                    value: Some(Box::new(Expr::And(
                        Box::new(Expr::Call("is_even".into(), vec![Box::new(Expr::Val(Val::Int(10)))])),
                        Box::new(Expr::Unary(
                            crate::op::UnaryOp::Not,
                            Box::new(Expr::Call("is_even".into(), vec![Box::new(Expr::Val(Val::Int(11)))])),
                        )),
                    ))),
                }),
            ],
        };

        let fun = Compiler::new().compile_stmt(&program);
        let mut env = crate::stmt::Environment::new();
        let out = crate::vm::Vm::new().exec_with(&fun, Some(&mut env), None).unwrap();
        assert_eq!(out, Val::Bool(true));
    }

    #[test]
    fn test_vm_index_ic_str_identity_replacement_multibyte() {
        use crate::op::BinOp;
        use crate::stmt::Stmt;
        // Program:
        //   acc = "";
        //   for i in 0..3 {
        //     if i == 0 { s = "αβ" } else if i == 1 { s = "XY" } else { s = "Z!" }
        //     acc = acc + s[0]
        //   }
        //   return acc
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "acc".into(), value: Box::new(Expr::Val(Val::Str("".into()))) }),
                Box::new(Stmt::Define { name: "i".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::While {
                    condition: Box::new(Expr::Bin(
                        Box::new(Expr::Var("i".into())),
                        BinOp::Lt,
                        Box::new(Expr::Val(Val::Int(3)))
                    )),
                    body: Box::new(Stmt::Block { statements: vec![
                        // if (i == 0) { s = "αβ" } else if (i == 1) { s = "XY" } else { s = "Z!" }
                        Box::new(Stmt::If {
                            condition: Box::new(Expr::Bin(
                                Box::new(Expr::Var("i".into())), BinOp::Eq, Box::new(Expr::Val(Val::Int(0)))
                            )),
                            then_stmt: Box::new(Stmt::Define { name: "s".into(), value: Box::new(Expr::Val(Val::Str("αβ".into()))) }),
                            else_stmt: Some(Box::new(Stmt::If {
                                condition: Box::new(Expr::Bin(
                                    Box::new(Expr::Var("i".into())), BinOp::Eq, Box::new(Expr::Val(Val::Int(1)))
                                )),
                                then_stmt: Box::new(Stmt::Define { name: "s".into(), value: Box::new(Expr::Val(Val::Str("XY".into()))) }),
                                else_stmt: Some(Box::new(Stmt::Define { name: "s".into(), value: Box::new(Expr::Val(Val::Str("Z!".into()))) })),
                            })),
                        }),
                        // acc = acc + s[0]
                        Box::new(Stmt::Assign {
                            name: "acc".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("acc".into())),
                                BinOp::Add,
                                Box::new(Expr::Access(Box::new(Expr::Var("s".into())), Box::new(Expr::Val(Val::Int(0))))),
                            )),
                            span: None,
                        }),
                        // i = i + 1
                        Box::new(Stmt::Assign {
                            name: "i".into(),
                            value: Box::new(Expr::Bin(
                                Box::new(Expr::Var("i".into())), BinOp::Add, Box::new(Expr::Val(Val::Int(1)))
                            )),
                            span: None,
                        }),
                    ]}),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("acc".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        // Expect "αXZ" if caching invalidates on identity change and multibyte index works
        assert_eq!(out, Val::Str("αXZ".into()));
    }

    #[test]
    fn test_vm_for_range_explicit_negative_step_ascending_zero_iters() {
        use crate::op::BinOp;
        use crate::stmt::{ForPattern, Stmt};
        // x = 0; for _ in 0..5 step -1 { x += 1 } return x;  // zero iterations
        let program = Stmt::Block {
            statements: vec![
                Box::new(Stmt::Define { name: "x".into(), value: Box::new(Expr::Val(Val::Int(0))) }),
                Box::new(Stmt::For {
                    pattern: ForPattern::Ignore,
                    iterable: Box::new(Expr::Range {
                        start: Some(Box::new(Expr::Val(Val::Int(0)))),
                        end: Some(Box::new(Expr::Val(Val::Int(5)))),
                        inclusive: false,
                        step: Some(Box::new(Expr::Val(Val::Int(-1)))),
                    }),
                    body: Box::new(Stmt::Block { statements: vec![
                        Box::new(Stmt::Assign { name: "x".into(), value: Box::new(Expr::Bin(
                            Box::new(Expr::Var("x".into())), BinOp::Add, Box::new(Expr::Val(Val::Int(1)))
                        )), span: None }),
                    ]}),
                }),
                Box::new(Stmt::Return { value: Some(Box::new(Expr::Var("x".into()))) }),
            ],
        };
        let fun = crate::vm::Compiler::new().compile_stmt(&program);
        let out = crate::vm::Vm::new().exec(&fun).unwrap();
        assert_eq!(out, Val::Int(0));
    }
}
