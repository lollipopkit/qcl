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
    #[cfg(feature = "bc32")]
    pub code32: Option<Vec<u32>>, // Optional packed encoding for direct execution
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
    // Boolean/logic
    Not(u16 /*dst*/, u16 /*src*/),
    // Convert any value to boolean truthiness (only Nil/false are falsey)
    ToBool(u16 /*dst*/, u16 /*src*/),
    // Branch helpers for nil checks
    JmpIfNil(u16 /*r*/, i16 /*ofs*/),
    JmpIfNotNil(u16 /*r*/, i16 /*ofs*/),
    // Nullish coalescing fused branch: if l != nil { dst = l; jmp ofs } else fallthrough
    NullishPick {
        l: u16,
        dst: u16,
        ofs: i16,
    },
    // Boolean short-circuit helpers that also set a boolean result register
    // If r is falsey: set dst=false and jump by ofs; else fallthrough
    JmpFalseSet {
        r: u16,
        dst: u16,
        ofs: i16,
    },
    // If r is truthy: set dst=true and jump by ofs; else fallthrough
    JmpTrueSet {
        r: u16,
        dst: u16,
        ofs: i16,
    },
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
    // Membership test: dst = (a in b)
    In(u16 /*dst*/, u16 /*a*/, u16 /*b*/),
    // Locals
    LoadLocal(u16 /*dst*/, u16 /*idx*/),
    StoreLocal(u16 /*idx*/, u16 /*src*/),
    // Globals
    LoadGlobal(u16 /*dst*/, u16 /*name_kidx*/),
    DefineGlobal(u16 /*name_kidx*/, u16 /*src*/),
    LoadCtx(u16 /*dst*/),
    // Access and constructors
    Access(u16 /*dst*/, u16 /*base*/, u16 /*field*/),
    // Access with constant string field (avoids allocating/register for field expr)
    AccessK(u16 /*dst*/, u16 /*base*/, u16 /*kidx*/),
    // Index with constant integer (avoids temp registers)
    IndexK(u16 /*dst*/, u16 /*base*/, u16 /*kidx*/),
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
            Op::Not(d, s) => write!(f, "Not r{}, r{}", d, s),
            Op::ToBool(d, s) => write!(f, "ToBool r{}, r{}", d, s),
            Op::JmpIfNil(r, ofs) => write!(f, "JmpIfNil r{}, {}", r, ofs),
            Op::JmpIfNotNil(r, ofs) => write!(f, "JmpIfNotNil r{}, {}", r, ofs),
            Op::NullishPick { l, dst, ofs } => write!(f, "NullishPick l=r{}, dst=r{}, {}", l, dst, ofs),
            Op::JmpFalseSet { r, dst, ofs } => write!(f, "JmpFalseSet r{}, dst=r{}, {}", r, dst, ofs),
            Op::JmpTrueSet { r, dst, ofs } => write!(f, "JmpTrueSet r{}, dst=r{}, {}", r, dst, ofs),
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
            Op::In(d, a, b) => write!(f, "In r{}, r{}, r{}", d, a, b),
            Op::LoadLocal(d, i) => write!(f, "LoadLocal r{}, [{}]", d, i),
            Op::StoreLocal(i, s) => write!(f, "StoreLocal [{}], r{}", i, s),
            Op::LoadGlobal(d, k) => write!(f, "LoadGlobal r{}, k{}", d, k),
            Op::DefineGlobal(k, s) => write!(f, "DefineGlobal k{}, r{}", k, s),
            Op::LoadCtx(d) => write!(f, "LoadCtx r{}", d),
            Op::Access(d, b, fld) => write!(f, "Access r{}, r{}, r{}", d, b, fld),
            Op::AccessK(d, b, k) => write!(f, "AccessK r{}, r{}, k{}", d, b, k),
            Op::IndexK(d, b, k) => write!(f, "IndexK r{}, r{}, k{}", d, b, k),
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
