//! Optional 32-bit packed bytecode encoding scaffold (feature = "bc32").
//!
//! This module provides a compact encoding for a common subset of ops with a
//! simple encoder/decoder. It is intended as a building block for future i-cache
//! density work, not as a full replacement yet.

use super::bytecode::{ClosureProto, Function, Op};

/// Packed function using 32-bit instructions for a subset of ops.
#[derive(Debug, Clone)]
pub struct Bc32Function {
    pub consts: Vec<crate::val::Val>,
    pub code32: Vec<u32>,
    pub n_regs: u16,
    pub protos: Vec<ClosureProto>,
    pub param_regs: Vec<u16>,
}

impl Bc32Function {
    /// Decode back to the standard Function format for execution.
    pub fn decode(&self) -> Function {
        // Multi-word aware decode to reconstruct enum Ops, including extended forms.
        let mut code = Vec::with_capacity(self.code32.len());
        let mut pc = 0usize;
        while pc < self.code32.len() {
            let w = self.code32[pc];
            let tag = tag_of(w);
            if tag == TAG_FOR_RANGE_PREP {
                // First word carries registers; Ext carries flags (inclusive, explicit)
                let a = ((w >> 16) & 0xFF) as u16; // idx
                let b = ((w >> 8) & 0xFF) as u16; // limit
                let c = (w & 0xFF) as u16; // step
                let w2 = self.code32[pc + 1];
                let flags = ((w2 >> 16) & 0xFF) as u8;
                let inclusive = (flags & 1) != 0;
                let explicit = (flags & 2) != 0;
                code.push(Op::ForRangePrep {
                    idx: a,
                    limit: b,
                    step: c,
                    inclusive,
                    explicit,
                });
                pc += 2;
                continue;
            } else if tag == TAG_FOR_RANGE_GUARD {
                let a = ((w >> 16) & 0xFF) as u16; // idx
                let b = ((w >> 8) & 0xFF) as u16; // limit
                let c = (w & 0xFF) as u16; // step
                let w2 = self.code32[pc + 1];
                let inclusive = (((w2 >> 16) & 0xFF) as u8 & 1) != 0;
                let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                code.push(Op::ForRangeGuard {
                    idx: a,
                    limit: b,
                    step: c,
                    inclusive,
                    ofs,
                });
                pc += 2;
                continue;
            } else if tag == TAG_FOR_RANGE_STEP {
                let a = ((w >> 16) & 0xFF) as u16; // idx
                let b = ((w >> 8) & 0xFF) as u16; // step
                let w2 = self.code32[pc + 1];
                let back_ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                code.push(Op::ForRangeStep {
                    idx: a,
                    step: b,
                    back_ofs,
                });
                pc += 2;
                continue;
            } else if tag == TAG_JMP_FALSE_SET_X {
                let r = ((w >> 16) & 0xFF) as u16;
                let dst = ((w >> 8) & 0xFF) as u16;
                let w2 = self.code32[pc + 1];
                let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                code.push(Op::JmpFalseSet { r, dst, ofs });
                pc += 2;
                continue;
            } else if tag == TAG_JMP_TRUE_SET_X {
                let r = ((w >> 16) & 0xFF) as u16;
                let dst = ((w >> 8) & 0xFF) as u16;
                let w2 = self.code32[pc + 1];
                let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                code.push(Op::JmpTrueSet { r, dst, ofs });
                pc += 2;
                continue;
            } else if tag == TAG_NULLISH_PICK_X {
                let l = ((w >> 16) & 0xFF) as u16;
                let dst = ((w >> 8) & 0xFF) as u16;
                let w2 = self.code32[pc + 1];
                let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                code.push(Op::NullishPick { l, dst, ofs });
                pc += 2;
                continue;
            }
            code.push(decode_word(w));
            pc += 1;
        }
        Function {
            consts: self.consts.clone(),
            code,
            n_regs: self.n_regs,
            protos: self.protos.clone(),
            param_regs: self.param_regs.clone(),
            #[cfg(feature = "bc32")]
            code32: None,
        }
    }
}

// Common 8-bit tags for encodable ops. Layout: [tag:8 | a:8 | b:8 | c:8]
#[repr(u8)]
enum Tag {
    Move = 1,
    LoadK = 2,
    Add = 3,
    Sub = 4,
    Mul = 5,
    Div = 6,
    Mod = 7,
    Eq = 8,
    Ne = 9,
    Lt = 10,
    Le = 11,
    Gt = 12,
    Ge = 13,
    Jmp = 14,          // ofs: 24-bit signed
    JmpFalse = 15,     // r=a, ofs: i16 from (b|c)
    ToBool = 16,       // dst=a, src=b
    Not = 17,          // dst=a, src=b
    Len = 18,          // dst=a, src=b
    Index = 19,        // dst=a, base=b, idx=c
    JmpIfNil = 21,     // r=a, ofs i16
    JmpIfNotNil = 22,  // r=a, ofs i16
    NullishPick = 23,  // l=a, dst=b, ofs i8 (small jump); fallback when not encodable
    Ret = 24,          // base=a, retc=b
    LoadGlobal = 25,   // dst=a, k=b
    DefineGlobal = 26, // k=a, src=b
    Access = 27,       // dst=a, base=b, field=c
    AccessK = 28,      // dst=a, base=b, k=c
    IndexK = 29,       // dst=a, base=b, k=c
    LoadLocal = 30,    // dst=a, idx=b
    StoreLocal = 31,   // idx=a, src=b
    Call = 32,         // f=a, base=b, argc=c (retc implied=1)
    LoadCtx = 33,      // dst=a
    JmpFalseSet = 34,  // r=a, dst=b, ofs i8
    JmpTrueSet = 35,   // r=a, dst=b, ofs i8
    ListSlice = 36,    // dst=a, src=b, start=c
    // Two-word extended conditional set+jump (ofs i16 via Ext)
    JmpFalseSetX = 37, // r=a, dst=b, ofs i16 in Ext
    JmpTrueSetX = 38,  // r=a, dst=b, ofs i16 in Ext
    // Two-word extended nullish pick (ofs i16 via Ext)
    NullishPickX = 39, // l=a, dst=b, ofs i16 in Ext
    // Two-word range ops (first word + Ext)
    ForRangePrep = 40,
    ForRangeGuard = 41,
    ForRangeStep = 42,
    Ext = 43,
}

#[inline]
fn pack(tag: Tag, a: u8, b: u8, c: u8) -> u32 {
    ((tag as u32) << 24) | ((a as u32) << 16) | ((b as u32) << 8) | (c as u32)
}

#[inline]
fn encode_i16(x: i16) -> (u8, u8) {
    (((x as u16) >> 8) as u8, (x as u8))
}

fn encode_op(op: &Op) -> Option<u32> {
    match *op {
        Op::Move(d, s) if d < 256 && s < 256 => Some(pack(Tag::Move, d as u8, s as u8, 0)),
        Op::LoadK(d, k) if d < 256 && k < 256 => Some(pack(Tag::LoadK, d as u8, k as u8, 0)),
        Op::Add(d, a, b) | Op::Sub(d, a, b) | Op::Mul(d, a, b) | Op::Div(d, a, b) | Op::Mod(d, a, b)
            if d < 256 && a < 256 && b < 256 =>
        {
            let tag = match op {
                Op::Add(_, _, _) => Tag::Add,
                Op::Sub(_, _, _) => Tag::Sub,
                Op::Mul(_, _, _) => Tag::Mul,
                Op::Div(_, _, _) => Tag::Div,
                _ => Tag::Mod,
            };
            Some(pack(tag, d as u8, a as u8, b as u8))
        }
        Op::CmpEq(d, a, b)
        | Op::CmpNe(d, a, b)
        | Op::CmpLt(d, a, b)
        | Op::CmpLe(d, a, b)
        | Op::CmpGt(d, a, b)
        | Op::CmpGe(d, a, b)
            if d < 256 && a < 256 && b < 256 =>
        {
            let tag = match op {
                Op::CmpEq(_, _, _) => Tag::Eq,
                Op::CmpNe(_, _, _) => Tag::Ne,
                Op::CmpLt(_, _, _) => Tag::Lt,
                Op::CmpLe(_, _, _) => Tag::Le,
                Op::CmpGt(_, _, _) => Tag::Gt,
                _ => Tag::Ge,
            };
            Some(pack(tag, d as u8, a as u8, b as u8))
        }
        Op::Jmp(ofs) => Some(((Tag::Jmp as u32) << 24) | (ofs as i32 as u32 & 0x00FF_FFFF)),
        Op::JmpFalse(r, ofs) if r < 256 => {
            let (hi, lo) = encode_i16(ofs);
            Some(pack(Tag::JmpFalse, r as u8, hi, lo))
        }
        Op::ToBool(d, s) if d < 256 && s < 256 => Some(pack(Tag::ToBool, d as u8, s as u8, 0)),
        Op::Not(d, s) if d < 256 && s < 256 => Some(pack(Tag::Not, d as u8, s as u8, 0)),
        Op::Len { dst, src } if dst < 256 && src < 256 => Some(pack(Tag::Len, dst as u8, src as u8, 0)),
        Op::Index { dst, base, idx } if dst < 256 && base < 256 && idx < 256 => {
            Some(pack(Tag::Index, dst as u8, base as u8, idx as u8))
        }
        Op::JmpIfNil(r, ofs) if r < 256 => {
            let (hi, lo) = encode_i16(ofs);
            Some(pack(Tag::JmpIfNil, r as u8, hi, lo))
        }
        Op::JmpIfNotNil(r, ofs) if r < 256 => {
            let (hi, lo) = encode_i16(ofs);
            Some(pack(Tag::JmpIfNotNil, r as u8, hi, lo))
        }
        // Note: NullishPick with remapped offsets is handled in pass 2; this
        // simple encoder only supports 1-word i8 immediate when used directly.
        Op::NullishPick { l, dst, ofs } if l < 256 && dst < 256 && (-128..=127).contains(&ofs) => {
            Some(pack(Tag::NullishPick, l as u8, dst as u8, (ofs as i8) as u8))
        }
        Op::Ret { base, retc } if base < 256 => Some(pack(Tag::Ret, base as u8, retc, 0)),
        Op::LoadGlobal(dst, k) if dst < 256 && k < 256 => Some(pack(Tag::LoadGlobal, dst as u8, k as u8, 0)),
        Op::DefineGlobal(k, src) if k < 256 && src < 256 => Some(pack(Tag::DefineGlobal, k as u8, src as u8, 0)),
        Op::Access(d, b, f) if d < 256 && b < 256 && f < 256 => Some(pack(Tag::Access, d as u8, b as u8, f as u8)),
        Op::AccessK(d, b, k) if d < 256 && b < 256 && k < 256 => Some(pack(Tag::AccessK, d as u8, b as u8, k as u8)),
        Op::IndexK(d, b, k) if d < 256 && b < 256 && k < 256 => Some(pack(Tag::IndexK, d as u8, b as u8, k as u8)),
        Op::LoadLocal(d, i) if d < 256 && i < 256 => Some(pack(Tag::LoadLocal, d as u8, i as u8, 0)),
        Op::StoreLocal(i, s) if i < 256 && s < 256 => Some(pack(Tag::StoreLocal, i as u8, s as u8, 0)),
        Op::Call { f, base, argc, retc } if f < 256 && base < 256 && retc == 1 => {
            Some(pack(Tag::Call, f as u8, base as u8, argc))
        }
        Op::LoadCtx(dst) if dst < 256 => Some(pack(Tag::LoadCtx, dst as u8, 0, 0)),
        // Note: Jmp*Set encodings with remapped offsets and extended i16 are
        // handled in pass 2; this function only covers the compact i8 forms.
        Op::JmpFalseSet { r, dst, ofs } if r < 256 && dst < 256 && (-128..=127).contains(&ofs) => {
            Some(pack(Tag::JmpFalseSet, r as u8, dst as u8, (ofs as i8) as u8))
        }
        Op::JmpTrueSet { r, dst, ofs } if r < 256 && dst < 256 && (-128..=127).contains(&ofs) => {
            Some(pack(Tag::JmpTrueSet, r as u8, dst as u8, (ofs as i8) as u8))
        }
        Op::ListSlice { dst, src, start } if dst < 256 && src < 256 && start < 256 => {
            Some(pack(Tag::ListSlice, dst as u8, src as u8, start as u8))
        }
        _ => None,
    }
}

#[inline]
fn sign_extend_24(x: u32) -> i32 {
    ((x as i32) << 8) >> 8
}

pub(crate) fn decode_word(w: u32) -> Op {
    let tag = ((w >> 24) & 0xFF) as u8;
    let a = ((w >> 16) & 0xFF) as u16;
    let b = ((w >> 8) & 0xFF) as u16;
    let c = (w & 0xFF) as u16;
    match tag {
        x if x == Tag::Move as u8 => Op::Move(a, b),
        x if x == Tag::LoadK as u8 => Op::LoadK(a, b),
        x if x == Tag::Add as u8 => Op::Add(a, b, c),
        x if x == Tag::Sub as u8 => Op::Sub(a, b, c),
        x if x == Tag::Mul as u8 => Op::Mul(a, b, c),
        x if x == Tag::Div as u8 => Op::Div(a, b, c),
        x if x == Tag::Mod as u8 => Op::Mod(a, b, c),
        x if x == Tag::Eq as u8 => Op::CmpEq(a, b, c),
        x if x == Tag::Ne as u8 => Op::CmpNe(a, b, c),
        x if x == Tag::Lt as u8 => Op::CmpLt(a, b, c),
        x if x == Tag::Le as u8 => Op::CmpLe(a, b, c),
        x if x == Tag::Gt as u8 => Op::CmpGt(a, b, c),
        x if x == Tag::Ge as u8 => Op::CmpGe(a, b, c),
        x if x == Tag::Jmp as u8 => Op::Jmp(sign_extend_24(w) as i16),
        x if x == Tag::JmpFalse as u8 => Op::JmpFalse(a, ((b << 8) | c) as i16),
        x if x == Tag::ToBool as u8 => Op::ToBool(a, b),
        x if x == Tag::Not as u8 => Op::Not(a, b),
        x if x == Tag::Len as u8 => Op::Len { dst: a, src: b },
        x if x == Tag::Index as u8 => Op::Index {
            dst: a,
            base: b,
            idx: c,
        },
        x if x == Tag::JmpIfNil as u8 => Op::JmpIfNil(a, ((b << 8) | c) as i16),
        x if x == Tag::JmpIfNotNil as u8 => Op::JmpIfNotNil(a, ((b << 8) | c) as i16),
        x if x == Tag::NullishPick as u8 => Op::NullishPick {
            l: a,
            dst: b,
            ofs: (c as i8) as i16,
        },
        x if x == Tag::Ret as u8 => Op::Ret { base: a, retc: b as u8 },
        x if x == Tag::LoadGlobal as u8 => Op::LoadGlobal(a, b),
        x if x == Tag::DefineGlobal as u8 => Op::DefineGlobal(a, b),
        x if x == Tag::Access as u8 => Op::Access(a, b, c),
        x if x == Tag::AccessK as u8 => Op::AccessK(a, b, c),
        x if x == Tag::IndexK as u8 => Op::IndexK(a, b, c),
        x if x == Tag::LoadLocal as u8 => Op::LoadLocal(a, b),
        x if x == Tag::StoreLocal as u8 => Op::StoreLocal(a, b),
        x if x == Tag::Call as u8 => Op::Call {
            f: a,
            base: b,
            argc: c as u8,
            retc: 1,
        },
        x if x == Tag::LoadCtx as u8 => Op::LoadCtx(a),
        x if x == Tag::JmpFalseSet as u8 => Op::JmpFalseSet {
            r: a,
            dst: b,
            ofs: (c as i8) as i16,
        },
        x if x == Tag::JmpTrueSet as u8 => Op::JmpTrueSet {
            r: a,
            dst: b,
            ofs: (c as i8) as i16,
        },
        x if x == Tag::ListSlice as u8 => Op::ListSlice {
            dst: a,
            src: b,
            start: c,
        },
        _ => Op::Jmp(0),
    }
}

impl Bc32Function {
    /// Two-pass packing: computes word indices per Op to remap branch offsets and handle multi-word ops.
    pub fn try_from_function(f: &Function) -> Option<Self> {
        let n = f.code.len();
        if n == 0 {
            return Some(Self {
                consts: f.consts.clone(),
                code32: vec![],
                n_regs: f.n_regs,
                protos: f.protos.clone(),
                param_regs: f.param_regs.clone(),
            });
        }
        // Pass 1a: initial word size guess (ForRange* -> 2, others -> 1 if encodable)
        let mut words_per_op: Vec<usize> = vec![1; n];
        for (i, op) in f.code.iter().enumerate() {
            words_per_op[i] = match op {
                Op::ForRangePrep { .. } | Op::ForRangeGuard { .. } | Op::ForRangeStep { .. } => 2,
                // Optimistically 1 for Jmp*Set/NullishPick; we refine below.
                Op::JmpFalseSet { .. } | Op::JmpTrueSet { .. } | Op::NullishPick { .. } => 1,
                _ => encode_op(op).map(|_| 1)?,
            };
        }
        // Iteratively refine sizes for Jmp*Set to allow i16 extended forms when needed.
        loop {
            let mut changed = false;
            // Prefix sum to map op index -> word index
            let mut pref: Vec<usize> = vec![0; n + 1];
            for i in 0..n {
                pref[i + 1] = pref[i] + words_per_op[i];
            }
            for (i, op) in f.code.iter().enumerate() {
                match *op {
                    Op::JmpFalseSet { ofs, .. } | Op::JmpTrueSet { ofs, .. } => {
                        let j = (i as isize) + ofs as isize;
                        if j < 0 || j as usize >= n {
                            return None;
                        }
                        let j = j as usize;
                        let wofs = (pref[j] as isize - pref[i] as isize) as i32;
                        let need_two = !(-128..=127).contains(&wofs);
                        let old = words_per_op[i];
                        let new = if need_two { 2 } else { 1 };
                        if new != old {
                            words_per_op[i] = new;
                            changed = true;
                        }
                    }
                    _ => {}
                }
            }
            if !changed {
                break;
            }
        }
        // Build op->word index map after convergence
        let mut op_to_word: Vec<usize> = vec![0; n];
        let mut acc = 0usize;
        for (i, w) in words_per_op.iter().enumerate() {
            op_to_word[i] = acc;
            acc += *w;
        }
        let total_words = acc;
        // Pass 2: encode with remapped offsets using final mapping
        let mut out: Vec<u32> = Vec::with_capacity(total_words);
        for (i, op) in f.code.iter().enumerate() {
            match op {
                Op::Jmp(ofs) => {
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i32;
                    out.push(((Tag::Jmp as u32) << 24) | ((wofs as u32) & 0x00FF_FFFF));
                }
                Op::JmpFalse(r, ofs) => {
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i16;
                    let (hi, lo) = ((wofs >> 8) as u8, (wofs & 0xFF) as u8);
                    out.push(pack(Tag::JmpFalse, *r as u8, hi, lo));
                }
                Op::JmpIfNil(r, ofs) => {
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i16;
                    let (hi, lo) = ((wofs >> 8) as u8, (wofs & 0xFF) as u8);
                    out.push(pack(Tag::JmpIfNil, *r as u8, hi, lo));
                }
                Op::JmpIfNotNil(r, ofs) => {
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i16;
                    let (hi, lo) = ((wofs >> 8) as u8, (wofs & 0xFF) as u8);
                    out.push(pack(Tag::JmpIfNotNil, *r as u8, hi, lo));
                }
                Op::NullishPick { l, dst, ofs } => {
                    if *l >= 256 || *dst >= 256 {
                        return None;
                    }
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i32;
                    if (-128..=127).contains(&wofs) {
                        out.push(pack(Tag::NullishPick, *l as u8, *dst as u8, (wofs as i8) as u8));
                    } else {
                        let wofs16 = wofs as i16;
                        out.push(pack(Tag::NullishPickX, *l as u8, *dst as u8, 0));
                        out.push(pack(Tag::Ext, 0, (wofs16 >> 8) as u8, (wofs16 & 0xFF) as u8));
                    }
                }
                Op::JmpFalseSet { r, dst, ofs } => {
                    if *r >= 256 || *dst >= 256 {
                        return None;
                    }
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i32;
                    if (-128..=127).contains(&wofs) && words_per_op[i] == 1 {
                        out.push(pack(Tag::JmpFalseSet, *r as u8, *dst as u8, (wofs as i8) as u8));
                    } else {
                        let wofs16 = wofs as i16;
                        out.push(pack(Tag::JmpFalseSetX, *r as u8, *dst as u8, 0));
                        out.push(pack(Tag::Ext, 0, (wofs16 >> 8) as u8, (wofs16 & 0xFF) as u8));
                    }
                }
                Op::JmpTrueSet { r, dst, ofs } => {
                    if *r >= 256 || *dst >= 256 {
                        return None;
                    }
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i32;
                    if (-128..=127).contains(&wofs) && words_per_op[i] == 1 {
                        out.push(pack(Tag::JmpTrueSet, *r as u8, *dst as u8, (wofs as i8) as u8));
                    } else {
                        let wofs16 = wofs as i16;
                        out.push(pack(Tag::JmpTrueSetX, *r as u8, *dst as u8, 0));
                        out.push(pack(Tag::Ext, 0, (wofs16 >> 8) as u8, (wofs16 & 0xFF) as u8));
                    }
                }
                Op::ForRangePrep {
                    idx,
                    limit,
                    step,
                    inclusive,
                    explicit,
                } => {
                    let flags = (if *inclusive { 1 } else { 0 }) | (if *explicit { 2 } else { 0 });
                    out.push(pack(Tag::ForRangePrep, *idx as u8, *limit as u8, *step as u8));
                    out.push(pack(Tag::Ext, flags as u8, 0, 0));
                }
                Op::ForRangeGuard {
                    idx,
                    limit,
                    step,
                    inclusive,
                    ofs,
                } => {
                    let tgt = ((i as isize) + *ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i16;
                    let flags = if *inclusive { 1 } else { 0 };
                    out.push(pack(Tag::ForRangeGuard, *idx as u8, *limit as u8, *step as u8));
                    out.push(pack(Tag::Ext, flags as u8, (wofs >> 8) as u8, (wofs & 0xFF) as u8));
                }
                Op::ForRangeStep { idx, step, back_ofs } => {
                    let tgt = ((i as isize) + *back_ofs as isize) as usize;
                    let wofs = (op_to_word[tgt] as isize - op_to_word[i] as isize) as i16;
                    out.push(pack(Tag::ForRangeStep, *idx as u8, *step as u8, 0));
                    out.push(pack(Tag::Ext, 0, (wofs >> 8) as u8, (wofs & 0xFF) as u8));
                }
                _ => {
                    out.push(encode_op(op)?);
                }
            }
        }
        Some(Self {
            consts: f.consts.clone(),
            code32: out,
            n_regs: f.n_regs,
            protos: f.protos.clone(),
            param_regs: f.param_regs.clone(),
        })
    }
}

/// Utility: expose tag and constants for VM bc32 fast-path
pub(crate) fn tag_of(w: u32) -> u8 {
    ((w >> 24) & 0xFF) as u8
}
pub(crate) const TAG_FOR_RANGE_PREP: u8 = Tag::ForRangePrep as u8;
pub(crate) const TAG_FOR_RANGE_GUARD: u8 = Tag::ForRangeGuard as u8;
pub(crate) const TAG_FOR_RANGE_STEP: u8 = Tag::ForRangeStep as u8;
pub(crate) const TAG_JMP_FALSE_SET_X: u8 = Tag::JmpFalseSetX as u8;
pub(crate) const TAG_JMP_TRUE_SET_X: u8 = Tag::JmpTrueSetX as u8;
pub(crate) const TAG_NULLISH_PICK_X: u8 = Tag::NullishPickX as u8;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_bc32_roundtrip_simple() {
        let f = Function {
            consts: vec![crate::val::Val::Int(42)],
            code: vec![
                Op::LoadK(0, 0),
                Op::Move(1, 0),
                Op::ToBool(2, 1),
                Op::Jmp(1),
                Op::JmpFalse(2, -1),
            ],
            n_regs: 3,
            protos: vec![],
            param_regs: vec![],
            #[cfg(feature = "bc32")]
            code32: None,
        };
        let bc = Bc32Function::try_from_function(&f).expect("encodable");
        let f2 = bc.decode();
        assert_eq!(format!("{:?}", f.code), format!("{:?}", f2.code));
    }
}
