use std::fmt;

use crate::val::Val;

/// Compact bytecode representation and constant pool.
/// This is a minimal scaffold to unblock incremental VM work.
#[derive(Debug, Clone)]
pub struct Function {
    pub consts: Vec<Val>,
    pub code: Vec<Op>,
    pub n_regs: u16,
    pub protos: Vec<ClosureProto>,
}

#[derive(Debug, Clone)]
pub struct ClosureProto {
    pub params: Vec<String>,
    pub body: crate::stmt::Stmt,
}

#[derive(Clone)]
pub enum Op {
    LoadK(u16 /*dst*/, u16 /*kidx*/),
    Move(u16 /*dst*/, u16 /*src*/),
    // Arithmetic
    Add(u16 /*dst*/, u16 /*a*/, u16 /*b*/),
    Sub(u16, u16, u16),
    Mul(u16, u16, u16),
    Div(u16, u16, u16),
    Mod(u16, u16, u16),
    // Comparisons -> Bool
    CmpEq(u16 /*dst*/, u16 /*a*/, u16 /*b*/),
    CmpNe(u16, u16, u16),
    CmpLt(u16, u16, u16),
    CmpLe(u16, u16, u16),
    CmpGt(u16, u16, u16),
    CmpGe(u16, u16, u16),
    // Locals
    LoadLocal(u16 /*dst*/, u16 /*idx*/),
    StoreLocal(u16 /*idx*/, u16 /*src*/),
    // Globals and context
    LoadGlobal(u16 /*dst*/, u16 /*name_kidx*/),
    DefineGlobal(u16 /*name_kidx*/, u16 /*src*/),
    LoadCtx(u16 /*dst*/),
    // Access and constructors
    Access(u16 /*dst*/, u16 /*base*/, u16 /*field*/),
    BuildList {
        dst: u16,
        base: u16,
        len: u16,
    },
    BuildMap {
        dst: u16,
        base: u16,
        len: u16,
    }, // base..base+2*len-1 as k,v pairs
    MakeClosure {
        dst: u16,
        proto: u16,
    },
    Jmp(i16 /*ofs*/),
    JmpFalse(u16 /*r*/, i16 /*ofs*/),
    Call {
        f: u16,
        base: u16,
        argc: u8,
        retc: u8,
    },
    Ret {
        base: u16,
        retc: u8,
    },
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::LoadK(d, k) => write!(f, "LoadK r{}, k{}", d, k),
            Op::Move(d, s) => write!(f, "Move r{}, r{}", d, s),
            Op::Add(d, a, b) => write!(f, "Add r{}, r{}, r{}", d, a, b),
            Op::Sub(d, a, b) => write!(f, "Sub r{}, r{}, r{}", d, a, b),
            Op::Mul(d, a, b) => write!(f, "Mul r{}, r{}, r{}", d, a, b),
            Op::Div(d, a, b) => write!(f, "Div r{}, r{}, r{}", d, a, b),
            Op::Mod(d, a, b) => write!(f, "Mod r{}, r{}, r{}", d, a, b),
            Op::CmpEq(d, a, b) => write!(f, "CmpEq r{}, r{}, r{}", d, a, b),
            Op::CmpNe(d, a, b) => write!(f, "CmpNe r{}, r{}, r{}", d, a, b),
            Op::CmpLt(d, a, b) => write!(f, "CmpLt r{}, r{}, r{}", d, a, b),
            Op::CmpLe(d, a, b) => write!(f, "CmpLe r{}, r{}, r{}", d, a, b),
            Op::CmpGt(d, a, b) => write!(f, "CmpGt r{}, r{}, r{}", d, a, b),
            Op::CmpGe(d, a, b) => write!(f, "CmpGe r{}, r{}, r{}", d, a, b),
            Op::LoadLocal(d, i) => write!(f, "LoadLocal r{}, [{}]", d, i),
            Op::StoreLocal(i, s) => write!(f, "StoreLocal [{}], r{}", i, s),
            Op::LoadGlobal(d, k) => write!(f, "LoadGlobal r{}, k{}", d, k),
            Op::DefineGlobal(k, s) => write!(f, "DefineGlobal k{}, r{}", k, s),
            Op::LoadCtx(d) => write!(f, "LoadCtx r{}", d),
            Op::Access(d, b, fld) => write!(f, "Access r{}, r{}, r{}", d, b, fld),
            Op::BuildList { dst, base, len } => {
                write!(f, "BuildList r{}, base={}, len={}", dst, base, len)
            }
            Op::BuildMap { dst, base, len } => {
                write!(f, "BuildMap r{}, base={}, len={}", dst, base, len)
            }
            Op::MakeClosure { dst, proto } => write!(f, "MakeClosure r{}, p{}", dst, proto),
            Op::Jmp(ofs) => write!(f, "Jmp {}", ofs),
            Op::JmpFalse(r, ofs) => write!(f, "JmpFalse r{}, {}", r, ofs),
            Op::Call {
                f: rf,
                base,
                argc,
                retc,
            } => write!(
                f,
                "Call r{}, base={}, argc={}, retc={}",
                rf, base, argc, retc
            ),
            Op::Ret { base, retc } => write!(f, "Ret base={}, retc={}", base, retc),
        }
    }
}
