use crate::{
    expr::Expr,
    stmt::Stmt,
    val::Val,
};

use super::bytecode::{Function, Op};

/// Very small placeholder compiler that turns a constant expression into bytecode.
/// This exists to allow end-to-end wiring while full compiler work is pending.
pub struct Compiler;

impl Compiler {
    pub fn new() -> Self { Self }

    /// Compile a single expression into a self-contained function.
    /// For now only supports literal constants; everything else falls back to Nil.
    pub fn compile_expr(&self, expr: &Expr) -> Function {
        match expr {
            Expr::Val(v) => Function {
                consts: vec![v.clone()],
                code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }],
                n_regs: 1,
            },
            Expr::And(l, r) => {
                match (&**l, &**r) {
                    (Expr::Val(Val::Bool(lb)), Expr::Val(Val::Bool(rb))) => {
                        // r0 = lb; if !r0 -> return r0; r1 = rb; return r1
                        Function {
                            consts: vec![Val::Bool(*lb), Val::Bool(*rb)],
                            code: vec![
                                Op::LoadK(0, 0),
                                Op::JmpFalse(0, 3), // if false -> jump to Ret r0
                                Op::LoadK(1, 1),
                                Op::Ret { base: 1, retc: 1 },
                                Op::Ret { base: 0, retc: 1 },
                            ],
                            n_regs: 2,
                        }
                    }
                    _ => Function { consts: vec![Val::Nil], code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }], n_regs: 1 },
                }
            }
            Expr::Or(l, r) => {
                match (&**l, &**r) {
                    (Expr::Val(Val::Bool(lb)), Expr::Val(Val::Bool(rb))) => {
                        // r0 = lb; if !r0 -> load r1=rb and return; else return r0
                        Function {
                            consts: vec![Val::Bool(*lb), Val::Bool(*rb)],
                            code: vec![
                                Op::LoadK(0, 0),
                                Op::JmpFalse(0, 2), // if false -> jump to load rhs
                                Op::Ret { base: 0, retc: 1 },
                                Op::LoadK(1, 1),
                                Op::Ret { base: 1, retc: 1 },
                            ],
                            n_regs: 2,
                        }
                    }
                    _ => Function { consts: vec![Val::Nil], code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }], n_regs: 1 },
                }
            }
            Expr::Bin(l, crate::op::BinOp::Add, r) => {
                match (&**l, &**r) {
                    (Expr::Val(Val::Int(li)), Expr::Val(Val::Int(ri))) => Function {
                        consts: vec![Val::Int(*li), Val::Int(*ri)],
                        code: vec![
                            Op::LoadK(0, 0),
                            Op::LoadK(1, 1),
                            Op::Add(2, 0, 1),
                            Op::Ret { base: 2, retc: 1 },
                        ],
                        n_regs: 3,
                    },
                    _ => Function {
                        consts: vec![Val::Nil],
                        code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }],
                        n_regs: 1,
                    },
                }
            }
            _ => Function {
                consts: vec![Val::Nil],
                code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }],
                n_regs: 1,
            },
        }
    }

    /// Compile a statement block; placeholder returns a Nil-returning function.
    pub fn compile_stmt(&self, _stmt: &Stmt) -> Function {
        Function { consts: vec![Val::Nil], code: vec![Op::LoadK(0, 0), Op::Ret { base: 0, retc: 1 }], n_regs: 1 }
    }
}
