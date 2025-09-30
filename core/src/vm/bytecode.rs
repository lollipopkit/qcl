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
    // Register indices for parameters in the order declared by the closure/function.
    // Empty for expression/statement wrappers that are not functions.
    pub param_regs: Vec<u16>,
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
    // Globals
    LoadGlobal(u16 /*dst*/, u16 /*name_kidx*/),
    DefineGlobal(u16 /*name_kidx*/, u16 /*src*/),
    LoadCtx(u16 /*dst*/),
    // Access and constructors
    Access(u16 /*dst*/, u16 /*base*/, u16 /*field*/),
    // Length and index helpers
    Len {
        dst: u16,
        src: u16,
    },
    Index {
        dst: u16,
        base: u16,
        idx: u16,
    },
    // Normalize a value into an iterable for for-in loops.
    // - List, Str: passthrough
    // - Map: materialize a stable, sorted list of [key, value] pairs once
    ToIter {
        dst: u16,
        src: u16,
    },
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
    // List slicing helpers
    ListSlice {
        dst: u16,   // destination register for result list
        src: u16,   // source list register
        start: u16, // start index (inclusive) in register (must be Int)
    },
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
    // Numeric for-range (specialized fast path):
    // Usage pattern compiled as:
    //   ForRangePrep { idx, limit, step, inclusive, explicit }
    //   ForRangeGuard { idx, limit, step, inclusive, ofs: end } // jump to end when done
    //   ... body ... (optional: move idx into loop variable before body)
    //   ForRangeStep { idx, step, back_ofs: guard } // idx += step; jump back to guard
    ForRangePrep {
        idx: u16,
        limit: u16,
        step: u16,       // register holding step (+1 or -1)
        inclusive: bool, // ..= vs ..
        explicit: bool,  // if true, keep provided step as-is
    },
    ForRangeGuard {
        idx: u16,
        limit: u16,
        step: u16,
        inclusive: bool,
        ofs: i16, // jump to end when guard fails
    },
    ForRangeStep {
        idx: u16,
        step: u16,
        back_ofs: i16, // jump back to guard
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
            Op::Len { dst, src } => write!(f, "Len r{}, r{}", dst, src),
            Op::Index { dst, base, idx } => write!(f, "Index r{}, r{}, r{}", dst, base, idx),
            Op::ToIter { dst, src } => write!(f, "ToIter r{}, r{}", dst, src),
            Op::BuildList { dst, base, len } => {
                write!(f, "BuildList r{}, base={}, len={}", dst, base, len)
            }
            Op::BuildMap { dst, base, len } => {
                write!(f, "BuildMap r{}, base={}, len={}", dst, base, len)
            }
            Op::ListSlice { dst, src, start } => {
                write!(f, "ListSlice r{}, r{}, r{}", dst, src, start)
            }
            Op::MakeClosure { dst, proto } => write!(f, "MakeClosure r{}, p{}", dst, proto),
            Op::Jmp(ofs) => write!(f, "Jmp {}", ofs),
            Op::JmpFalse(r, ofs) => write!(f, "JmpFalse r{}, {}", r, ofs),
            Op::Call {
                f: rf,
                base,
                argc,
                retc,
            } => write!(f, "Call r{}, base={}, argc={}, retc={}", rf, base, argc, retc),
            Op::Ret { base, retc } => write!(f, "Ret base={}, retc={}", base, retc),
            Op::ForRangePrep {
                idx,
                limit,
                step,
                inclusive,
                explicit,
            } => write!(
                f,
                "ForRangePrep idx=r{}, limit=r{}, step=r{}, inclusive={}, explicit={}",
                idx, limit, step, inclusive, explicit
            ),
            Op::ForRangeGuard {
                idx,
                limit,
                step,
                inclusive,
                ofs,
            } => write!(
                f,
                "ForRangeGuard idx=r{}, limit=r{}, step=r{}, inclusive={}, ofs={}",
                idx, limit, step, inclusive, ofs
            ),
            Op::ForRangeStep { idx, step, back_ofs } => {
                write!(f, "ForRangeStep idx=r{}, step=r{}, back_ofs={}", idx, step, back_ofs)
            }
        }
    }
}
