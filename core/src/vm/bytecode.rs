use std::fmt;

use crate::val::Val;

/// Compact bytecode representation and constant pool.
/// This is a minimal scaffold to unblock incremental VM work.
#[derive(Debug, Clone)]
pub struct Function {
    pub consts: Vec<Val>,
    pub code: Vec<Op>,
    pub n_regs: u16,
}

#[derive(Clone)]
pub enum Op {
    LoadK(u16 /*dst*/, u16 /*kidx*/),
    Move(u16 /*dst*/, u16 /*src*/),
    Add(u16 /*dst*/, u16 /*a*/, u16 /*b*/),
    Jmp(i16 /*ofs*/),
    JmpFalse(u16 /*r*/, i16 /*ofs*/),
    Call { f: u16, base: u16, argc: u8, retc: u8 },
    Ret { base: u16, retc: u8 },
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::LoadK(d, k) => write!(f, "LoadK r{}, k{}", d, k),
            Op::Move(d, s) => write!(f, "Move r{}, r{}", d, s),
            Op::Add(d, a, b) => write!(f, "Add r{}, r{}, r{}", d, a, b),
            Op::Jmp(ofs) => write!(f, "Jmp {}", ofs),
            Op::JmpFalse(r, ofs) => write!(f, "JmpFalse r{}, {}", r, ofs),
            Op::Call { f: rf, base, argc, retc } => write!(
                f,
                "Call r{}, base={}, argc={}, retc={}",
                rf, base, argc, retc
            ),
            Op::Ret { base, retc } => write!(f, "Ret base={}, retc={}", base, retc),
        }
    }
}

