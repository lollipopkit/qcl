use anyhow::Result;

use crate::val::Val;

use super::bytecode::{Function, Op};

/// Minimal VM loop that can execute the placeholder Function produced by the stub compiler.
pub struct Vm;

impl Vm {
    pub fn new() -> Self { Self }

    pub fn exec(&mut self, f: &Function) -> Result<Val> {
        let mut regs: Vec<Val> = vec![Val::Nil; f.n_regs as usize];
        let mut pc: usize = 0;
        while pc < f.code.len() {
            match &f.code[pc] {
                Op::LoadK(dst, k) => {
                    regs[*dst as usize] = f.consts[*k as usize].clone();
                    pc += 1;
                }
                Op::Move(dst, src) => {
                    regs[*dst as usize] = regs[*src as usize].clone();
                    pc += 1;
                }
                Op::Add(dst, a, b) => {
                    // Very minimal addition semantics for Int only (placeholder)
                    match (&regs[*a as usize], &regs[*b as usize]) {
                        (Val::Int(x), Val::Int(y)) => regs[*dst as usize] = Val::Int(x + y),
                        _ => regs[*dst as usize] = Val::Nil,
                    }
                    pc += 1;
                }
                Op::Jmp(ofs) => {
                    pc = ((pc as isize) + (*ofs as isize)) as usize;
                }
                Op::JmpFalse(r, ofs) => {
                    let cond_falsey = matches!(regs[*r as usize], Val::Nil | Val::Bool(false));
                    if cond_falsey {
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::Call { .. } => {
                    // Not implemented in scaffold
                    pc += 1;
                }
                Op::Ret { base, retc } => {
                    let ret = if *retc > 0 { regs[*base as usize].clone() } else { Val::Nil };
                    return Ok(ret);
                }
            }
        }
        Ok(Val::Nil)
    }
}

