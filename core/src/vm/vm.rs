use std::sync::Arc;

use anyhow::{Result, anyhow};

use crate::util::fast_map::{FastHashMap, fast_hash_map_with_capacity};
use crate::val::Val;

use super::bytecode::{Function, Op};

// Small polymorphic inline caches (4-way) for property/index access per instruction site.
// This reduces churn at megamorphic sites while staying allocation-free.
#[derive(Clone)]
struct MapStrEntry {
    map_ptr: usize,
    key_ptr: usize,
    value: Val,
}
#[derive(Clone)]
struct ObjectStrEntry {
    obj_ptr: usize,
    key: String,
    value: Val,
}

#[derive(Clone)]
enum AccessIc {
    MapStr([Option<MapStrEntry>; 4]),
    ObjectStr([Option<ObjectStrEntry>; 4]),
}

// Per-op inline cache entries reused across VM executions (to avoid reallocation).
#[derive(Clone)]
struct ListEntry {
    base_ptr: usize,
    idx: i64,
    value: Val,
}
#[derive(Clone)]
struct StrEntry {
    base_ptr: usize,
    idx: i64,
    value: Val,
}

#[derive(Clone)]
enum IndexIc {
    List([Option<ListEntry>; 4]),
    Str([Option<StrEntry>; 4]),
}

#[derive(Clone)]
struct GlobalEntry(usize /*name_ptr*/, Val, u64 /*generation*/);

#[derive(Clone, Copy)]
enum CallIc {
    Rust(crate::val::RustFunction, u8 /*argc*/),
}

/// Minimal VM loop that can execute the placeholder Function produced by the stub compiler.
/// Reuses an internal register vector across executions to reduce allocations.
pub struct Vm {
    regs: Vec<Val>,
    // Reused instruction-site caches to minimize per-exec allocations
    access_ic: Vec<Option<AccessIc>>,
    index_ic: Vec<Option<IndexIc>>,
    global_ic: Vec<Option<GlobalEntry>>,
    call_ic: Vec<Option<CallIc>>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            regs: Vec::new(),
            access_ic: Vec::new(),
            index_ic: Vec::new(),
            global_ic: Vec::new(),
            call_ic: Vec::new(),
        }
    }

    pub fn exec(&mut self, f: &Function) -> Result<Val> {
        self.exec_with(f, None, None)
    }

    pub fn exec_with(
        &mut self,
        f: &Function,
        mut env: Option<&mut crate::stmt::Environment>,
        args: Option<&[Val]>,
    ) -> Result<Val> {
        // Aliases to reusable instruction-site caches
        let access_ic = &mut self.access_ic;
        let index_ic = &mut self.index_ic;
        let global_ic = &mut self.global_ic;
        let call_ic = &mut self.call_ic;
        // Ensure capacity and initialize registers to Nil without unnecessary drops/reallocs.
        let regs = &mut self.regs;
        let needed = f.n_regs as usize;
        if regs.len() >= needed {
            // Overwrite the first N slots with Nil and logically shrink if longer
            for slot in &mut regs[..needed] {
                *slot = Val::Nil;
            }
            regs.truncate(needed);
        } else {
            // Grow to needed size, filling with Nil
            regs.resize(needed, Val::Nil);
        }
        // Seed parameter registers directly from provided args, if any.
        if let Some(a) = args
            && !f.param_regs.is_empty()
        {
            // Defensive: only seed up to min(len)
            let n = a.len().min(f.param_regs.len());
            for (i, val) in a.iter().enumerate().take(n) {
                let r = f.param_regs[i] as usize;
                regs[r] = val.clone();
            }
        }
        // Locals share the same storage space as registers for this simple VM.
        // `LoadLocal/StoreLocal` simply index into this vector.
        let mut pc: usize = 0;

        // Fast path: execute packed 32-bit bytecode directly when available (feature = bc32)
        #[cfg(feature = "bc32")]
        if let Some(code32) = f.code32.as_ref() {
            // Persist instruction-site caches across executions; only grow when needed.
            if access_ic.len() < code32.len() {
                access_ic.resize(code32.len(), None);
            }
            if index_ic.len() < code32.len() {
                index_ic.resize(code32.len(), None);
            }
            if global_ic.len() < code32.len() {
                global_ic.resize(code32.len(), None);
            }
            if call_ic.len() < code32.len() {
                call_ic.resize(code32.len(), None);
            }
            while pc < code32.len() {
                // Handle multi-word ForRange* specially using tag peek
                let tag = crate::vm::bc32::tag_of(code32[pc]);
                if tag == crate::vm::bc32::TAG_FOR_RANGE_PREP {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // idx
                    let b = ((w >> 8) & 0xFF) as u16; // limit
                    let c = (w & 0xFF) as u16; // step
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for ForRangePrep"))?;
                    let flags = ((w2 >> 16) & 0xFF) as u8;
                    let inclusive = (flags & 1) != 0;
                    let explicit = (flags & 2) != 0;
                    // Execute ForRangePrep
                    let (i0, ilim) = match (&regs[a as usize], &regs[b as usize]) {
                        (Val::Int(a0), Val::Int(b0)) => (*a0, *b0),
                        _ => return Err(anyhow!("For-range requires integer bounds")),
                    };
                    if !explicit {
                        let step_val = if i0 <= ilim { 1 } else { -1 };
                        regs[c as usize] = Val::Int(step_val);
                    } else {
                        match &regs[c as usize] {
                            Val::Int(0) => return Err(anyhow!("For-range step cannot be zero")),
                            Val::Int(_) => {}
                            other => return Err(anyhow!("For-range step must be Int when explicit, got {:?}", other)),
                        }
                    }
                    let _ = inclusive; // carried to guard
                    pc += 2;
                    continue;
                } else if tag == crate::vm::bc32::TAG_FOR_RANGE_GUARD {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // idx
                    let b = ((w >> 8) & 0xFF) as u16; // limit
                    let c = (w & 0xFF) as u16; // step
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for ForRangeGuard"))?;
                    let flags = ((w2 >> 16) & 0xFF) as u8;
                    let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                    let inclusive = (flags & 1) != 0;
                    let (i, lim, st) = match (&regs[a as usize], &regs[b as usize], &regs[c as usize]) {
                        (Val::Int(i), Val::Int(l), Val::Int(s)) => (*i, *l, *s),
                        _ => return Err(anyhow!("For-range guard expects Int registers")),
                    };
                    let cont = if st > 0 {
                        if inclusive { i <= lim } else { i < lim }
                    } else if inclusive {
                        i >= lim
                    } else {
                        i > lim
                    };
                    if !cont {
                        pc = ((pc as isize) + (ofs as isize)) as usize;
                    } else {
                        pc += 2;
                    }
                    continue;
                } else if tag == crate::vm::bc32::TAG_FOR_RANGE_STEP {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // idx
                    let b = ((w >> 8) & 0xFF) as u16; // step
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for ForRangeStep"))?;
                    let back_ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                    let (i, st) = match (&regs[a as usize], &regs[b as usize]) {
                        (Val::Int(i), Val::Int(s)) => (*i, *s),
                        _ => return Err(anyhow!("For-range step expects Int registers")),
                    };
                    regs[a as usize] = Val::Int(i + st);
                    pc = ((pc as isize) + (back_ofs as isize)) as usize;
                    continue;
                } else if tag == crate::vm::bc32::TAG_JMP_FALSE_SET_X {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // r
                    let b = ((w >> 8) & 0xFF) as u16; // dst
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for JmpFalseSetX"))?;
                    let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                    let cond_falsey = matches!(regs[a as usize], Val::Nil | Val::Bool(false));
                    if cond_falsey {
                        regs[b as usize] = Val::Bool(false);
                        pc = ((pc as isize) + (ofs as isize)) as usize;
                    } else {
                        pc += 2;
                    }
                    continue;
                } else if tag == crate::vm::bc32::TAG_JMP_TRUE_SET_X {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // r
                    let b = ((w >> 8) & 0xFF) as u16; // dst
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for JmpTrueSetX"))?;
                    let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                    let cond_truthy = !matches!(regs[a as usize], Val::Nil | Val::Bool(false));
                    if cond_truthy {
                        regs[b as usize] = Val::Bool(true);
                        pc = ((pc as isize) + (ofs as isize)) as usize;
                    } else {
                        pc += 2;
                    }
                    continue;
                } else if tag == crate::vm::bc32::TAG_NULLISH_PICK_X {
                    let w = code32[pc];
                    let a = ((w >> 16) & 0xFF) as u16; // l
                    let b = ((w >> 8) & 0xFF) as u16; // dst
                    let w2 = code32
                        .get(pc + 1)
                        .ok_or_else(|| anyhow!("bc32: missing Ext for NullishPickX"))?;
                    let ofs = (((((w2 >> 8) & 0xFF) as u16) << 8) | ((w2 & 0xFF) as u16)) as i16;
                    if !matches!(regs[a as usize], Val::Nil) {
                        let v = regs[a as usize].clone();
                        regs[b as usize] = v;
                        pc = ((pc as isize) + (ofs as isize)) as usize;
                    } else {
                        pc += 2;
                    }
                    continue;
                }
                let op = crate::vm::bc32::decode_word(code32[pc]);
                match op {
                    Op::LoadK(dst, k) => {
                        regs[dst as usize] = f.consts[k as usize].clone();
                        pc += 1;
                    }
                    Op::Move(dst, src) => {
                        regs[dst as usize] = regs[src as usize].clone();
                        pc += 1;
                    }
                    Op::Add(dst, a, b) => {
                        if !Self::arith2_try_numeric(regs, dst, a, b, |x, y| x + y, |x, y| x + y) {
                            let out = crate::op::BinOp::Add.eval_vals(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = out;
                        }
                        pc += 1;
                    }
                    Op::Sub(dst, a, b) => {
                        if !Self::arith2_try_numeric(regs, dst, a, b, |x, y| x - y, |x, y| x - y) {
                            let out = crate::op::BinOp::Sub.eval_vals(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = out;
                        }
                        pc += 1;
                    }
                    Op::Mul(dst, a, b) => {
                        if !Self::arith2_try_numeric(regs, dst, a, b, |x, y| x * y, |x, y| x * y) {
                            let out = crate::op::BinOp::Mul.eval_vals(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = out;
                        }
                        pc += 1;
                    }
                    Op::Div(dst, a, b) => {
                        if !Self::arith2_try_numeric(regs, dst, a, b, |x, y| x / y, |x, y| x / y) {
                            let out = crate::op::BinOp::Div.eval_vals(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = out;
                        }
                        pc += 1;
                    }
                    Op::Mod(dst, a, b) => {
                        match (&regs[a as usize], &regs[b as usize]) {
                            (Val::Int(x), Val::Int(y)) => regs[dst as usize] = Val::Int(x % y),
                            _ => {
                                let out = crate::op::BinOp::Mod.eval_vals(&regs[a as usize], &regs[b as usize])?;
                                regs[dst as usize] = out;
                            }
                        }
                        pc += 1;
                    }
                    Op::CmpEq(dst, a, b) => {
                        regs[dst as usize] = Val::Bool(regs[a as usize] == regs[b as usize]);
                        pc += 1;
                    }
                    Op::CmpNe(dst, a, b) => {
                        regs[dst as usize] = Val::Bool(regs[a as usize] != regs[b as usize]);
                        pc += 1;
                    }
                    Op::CmpLt(dst, a, b) => {
                        if !Self::cmp2_try_numeric(regs, dst, a, b, |x, y| x < y, |x, y| x < y) {
                            let res = crate::op::BinOp::Lt.cmp(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = Val::Bool(res);
                        }
                        pc += 1;
                    }
                    Op::CmpLe(dst, a, b) => {
                        if !Self::cmp2_try_numeric(regs, dst, a, b, |x, y| x <= y, |x, y| x <= y) {
                            let res = crate::op::BinOp::Le.cmp(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = Val::Bool(res);
                        }
                        pc += 1;
                    }
                    Op::CmpGt(dst, a, b) => {
                        if !Self::cmp2_try_numeric(regs, dst, a, b, |x, y| x > y, |x, y| x > y) {
                            let res = crate::op::BinOp::Gt.cmp(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = Val::Bool(res);
                        }
                        pc += 1;
                    }
                    Op::CmpGe(dst, a, b) => {
                        if !Self::cmp2_try_numeric(regs, dst, a, b, |x, y| x >= y, |x, y| x >= y) {
                            let res = crate::op::BinOp::Ge.cmp(&regs[a as usize], &regs[b as usize])?;
                            regs[dst as usize] = Val::Bool(res);
                        }
                        pc += 1;
                    }
                    Op::Len { dst, src } => {
                        let v = &regs[src as usize];
                        let out = match v {
                            Val::List(l) => Val::Int(l.len() as i64),
                            Val::Str(s) => Val::Int(s.len() as i64),
                            Val::Map(m) => Val::Int(m.len() as i64),
                            _ => Val::Int(0),
                        };
                        regs[dst as usize] = out;
                        pc += 1;
                    }
                    Op::Index { dst, base, idx } => {
                        let res = match (&regs[base as usize], &regs[idx as usize]) {
                            (Val::List(l), Val::Int(i)) => {
                                if *i < 0 {
                                    Val::Nil
                                } else {
                                    let lptr = Arc::as_ptr(l) as *const Val as usize;
                                    let hit = if let Some(IndexIc::List(slots)) = &index_ic[pc] {
                                        let mut out: Option<Val> = None;
                                        for e in slots.iter().flatten() {
                                            if e.base_ptr == lptr && e.idx == *i {
                                                out = Some(e.value.clone());
                                                break;
                                            }
                                        }
                                        out
                                    } else {
                                        None
                                    };
                                    if let Some(v) = hit {
                                        v
                                    } else {
                                        let v = l.get(*i as usize).cloned().unwrap_or(Val::Nil);
                                        // update slots
                                        match index_ic[pc].as_mut() {
                                            Some(IndexIc::List(slots)) => {
                                                let newe = ListEntry {
                                                    base_ptr: lptr,
                                                    idx: *i,
                                                    value: v.clone(),
                                                };
                                                if slots[0].as_ref().is_some_and(|e| e.base_ptr == lptr && e.idx == *i)
                                                {
                                                    slots[0] = Some(newe);
                                                } else if slots[1]
                                                    .as_ref()
                                                    .is_some_and(|e| e.base_ptr == lptr && e.idx == *i)
                                                {
                                                    slots[1] = Some(newe);
                                                } else {
                                                    slots[3] = slots[2].clone();
                                                    slots[2] = slots[1].clone();
                                                    slots[1] = slots[0].clone();
                                                    slots[0] = Some(newe);
                                                }
                                            }
                                            _ => {
                                                index_ic[pc] = Some(IndexIc::List([
                                                    Some(ListEntry {
                                                        base_ptr: lptr,
                                                        idx: *i,
                                                        value: v.clone(),
                                                    }),
                                                    None,
                                                    None,
                                                    None,
                                                ]));
                                            }
                                        }
                                        v
                                    }
                                }
                            }
                            (Val::Str(s), Val::Int(i)) => {
                                if *i < 0 {
                                    Val::Nil
                                } else {
                                    let sptr = s.as_ref().as_ptr() as usize;
                                    let hit = if let Some(IndexIc::Str(slots)) = &index_ic[pc] {
                                        let mut out: Option<Val> = None;
                                        for e in slots.iter().flatten() {
                                            if e.base_ptr == sptr && e.idx == *i {
                                                out = Some(e.value.clone());
                                                break;
                                            }
                                        }
                                        out
                                    } else {
                                        None
                                    };
                                    if let Some(v) = hit {
                                        v
                                    } else {
                                        let v = if s.is_ascii() {
                                            let bi = *i as usize;
                                            let bs = s.as_bytes();
                                            if bi < bs.len() {
                                                let ch = bs[bi] as char;
                                                Val::Str(ch.to_string().into())
                                            } else {
                                                Val::Nil
                                            }
                                        } else {
                                            s.chars()
                                                .nth(*i as usize)
                                                .map(|c| Val::Str(c.to_string().into()))
                                                .unwrap_or(Val::Nil)
                                        };
                                        match index_ic[pc].as_mut() {
                                            Some(IndexIc::Str(slots)) => {
                                                let newe = StrEntry {
                                                    base_ptr: sptr,
                                                    idx: *i,
                                                    value: v.clone(),
                                                };
                                                if slots[0].as_ref().is_some_and(|e| e.base_ptr == sptr && e.idx == *i)
                                                {
                                                    slots[0] = Some(newe);
                                                } else if slots[1]
                                                    .as_ref()
                                                    .is_some_and(|e| e.base_ptr == sptr && e.idx == *i)
                                                {
                                                    slots[1] = Some(newe);
                                                } else {
                                                    slots[3] = slots[2].clone();
                                                    slots[2] = slots[1].clone();
                                                    slots[1] = slots[0].clone();
                                                    slots[0] = Some(newe);
                                                }
                                            }
                                            _ => {
                                                index_ic[pc] = Some(IndexIc::Str([
                                                    Some(StrEntry {
                                                        base_ptr: sptr,
                                                        idx: *i,
                                                        value: v.clone(),
                                                    }),
                                                    None,
                                                    None,
                                                    None,
                                                ]));
                                            }
                                        }
                                        v
                                    }
                                }
                            }
                            _ => Val::Nil,
                        };
                        regs[dst as usize] = res;
                        pc += 1;
                    }
                    Op::Jmp(ofs) => {
                        pc = ((pc as isize) + (ofs as isize)) as usize;
                    }
                    Op::JmpFalse(r, ofs) => {
                        let cond_falsey = matches!(regs[r as usize], Val::Nil | Val::Bool(false));
                        if cond_falsey {
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::JmpIfNil(r, ofs) => {
                        if matches!(regs[r as usize], Val::Nil) {
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::JmpIfNotNil(r, ofs) => {
                        if !matches!(regs[r as usize], Val::Nil) {
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::ToBool(dst, src) => {
                        let truthy = !matches!(regs[src as usize], Val::Nil | Val::Bool(false));
                        regs[dst as usize] = Val::Bool(truthy);
                        pc += 1;
                    }
                    Op::Not(dst, src) => {
                        match &regs[src as usize] {
                            Val::Bool(b) => regs[dst as usize] = Val::Bool(!b),
                            other => return Err(anyhow!("Invalid operand: !{:?}", other)),
                        }
                        pc += 1;
                    }
                    Op::NullishPick { l, dst, ofs } => {
                        if !matches!(regs[l as usize], Val::Nil) {
                            regs[dst as usize] = regs[l as usize].clone();
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::Ret { base, retc } => {
                        let ret = if retc > 0 {
                            regs[base as usize].clone()
                        } else {
                            Val::Nil
                        };
                        return Ok(ret);
                    }
                    Op::LoadGlobal(dst, name_k) => {
                        let name_val = &f.consts[name_k as usize];
                        let mut out = Val::Nil;
                        if let Val::Str(s) = name_val {
                            let key_ptr = s.as_ref().as_ptr() as usize;
                            let cur_gen = if let Some(e) = env.as_ref() { e.generation() } else { 0 };
                            if let Some(GlobalEntry(ptr, v, generation)) = &global_ic[pc]
                                && *ptr == key_ptr
                                && *generation == cur_gen
                            {
                                out = v.clone();
                            }
                            if matches!(out, Val::Nil) {
                                if let Some(e) = env.as_ref()
                                    && let Some(v) = e.get_value(s.as_ref())
                                {
                                    out = v.clone();
                                }
                                global_ic[pc] = Some(GlobalEntry(key_ptr, out.clone(), cur_gen));
                            }
                        } else if let Some(e) = env.as_ref()
                            && let Some(v) = e.get_value(&format!("{}", name_val))
                        {
                            out = v;
                        }
                        regs[dst as usize] = out;
                        pc += 1;
                    }
                    Op::DefineGlobal(name_k, src) => {
                        if let Some(e) = env.as_mut() {
                            let name_val = &f.consts[name_k as usize];
                            if let Val::Str(s) = name_val {
                                e.define_global(s.to_string(), regs[src as usize].clone());
                            }
                        }
                        pc += 1;
                    }
                    Op::Access(dst, base, field) => {
                        let hit_val = match (&regs[base as usize], &regs[field as usize]) {
                            (Val::Map(m), Val::Str(s)) => {
                                let mp = std::sync::Arc::as_ptr(m) as usize;
                                let kp = s.as_ref().as_ptr() as usize;
                                if let Some(AccessIc::MapStr(slots)) = &access_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.map_ptr == mp && e.key_ptr == kp {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    out
                                } else {
                                    None
                                }
                            }
                            (Val::Object { fields, .. }, Val::Str(s)) => {
                                let optr = std::sync::Arc::as_ptr(fields) as usize;
                                let kstr = s.as_ref();
                                if let Some(AccessIc::ObjectStr(slots)) = &access_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.obj_ptr == optr && e.key.as_str() == kstr {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    out
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        };
                        let res = if let Some(v) = hit_val {
                            v
                        } else {
                            let v = regs[base as usize].access(&regs[field as usize]).unwrap_or(Val::Nil);
                            match (&regs[base as usize], &regs[field as usize]) {
                                (Val::Map(m), Val::Str(s)) => {
                                    let mp = std::sync::Arc::as_ptr(m) as usize;
                                    let kp = s.as_ref().as_ptr() as usize;
                                    match access_ic[pc].as_mut() {
                                        Some(AccessIc::MapStr(slots)) => {
                                            let newe = MapStrEntry {
                                                map_ptr: mp,
                                                key_ptr: kp,
                                                value: v.clone(),
                                            };
                                            if slots[0].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp) {
                                                slots[0] = Some(newe);
                                            } else if slots[1]
                                                .as_ref()
                                                .is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp)
                                            {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            access_ic[pc] = Some(AccessIc::MapStr([
                                                Some(MapStrEntry {
                                                    map_ptr: mp,
                                                    key_ptr: kp,
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                }
                                (Val::Object { fields, .. }, Val::Str(s)) => {
                                    let optr = std::sync::Arc::as_ptr(fields) as usize;
                                    match access_ic[pc].as_mut() {
                                        Some(AccessIc::ObjectStr(slots)) => {
                                            let newe = ObjectStrEntry {
                                                obj_ptr: optr,
                                                key: s.as_ref().to_string(),
                                                value: v.clone(),
                                            };
                                            if slots[0]
                                                .as_ref()
                                                .is_some_and(|e| e.obj_ptr == optr && e.key.as_str() == s.as_ref())
                                            {
                                                slots[0] = Some(newe);
                                            } else if slots[1]
                                                .as_ref()
                                                .is_some_and(|e| e.obj_ptr == optr && e.key.as_str() == s.as_ref())
                                            {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            access_ic[pc] = Some(AccessIc::ObjectStr([
                                                Some(ObjectStrEntry {
                                                    obj_ptr: optr,
                                                    key: s.as_ref().to_string(),
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                }
                                _ => {}
                            }
                            v
                        };
                        regs[dst as usize] = res;
                        pc += 1;
                    }
                    Op::AccessK(dst, base, kidx) => {
                        let key = &f.consts[kidx as usize];
                        let res = if let Val::Str(s) = key {
                            let (hit_val, mp, kp, obj) = match &regs[base as usize] {
                                Val::Map(m) => {
                                    let mp = std::sync::Arc::as_ptr(m) as usize;
                                    let kp = s.as_ref().as_ptr() as usize;
                                    if let Some(AccessIc::MapStr(slots)) = &access_ic[pc] {
                                        let mut out: Option<Val> = None;
                                        for e in slots.iter().flatten() {
                                            if e.map_ptr == mp && e.key_ptr == kp {
                                                out = Some(e.value.clone());
                                                break;
                                            }
                                        }
                                        (out, Some(mp), Some(kp), false)
                                    } else {
                                        (None, Some(mp), Some(kp), false)
                                    }
                                }
                                Val::Object { fields, .. } => {
                                    let optr = std::sync::Arc::as_ptr(fields) as usize;
                                    if let Some(AccessIc::ObjectStr(slots)) = &access_ic[pc] {
                                        let mut out: Option<Val> = None;
                                        for e in slots.iter().flatten() {
                                            if e.obj_ptr == optr && e.key.as_str() == s.as_ref() {
                                                out = Some(e.value.clone());
                                                break;
                                            }
                                        }
                                        (out, None, None, true)
                                    } else {
                                        (None, None, None, true)
                                    }
                                }
                                _ => (None, None, None, false),
                            };
                            if let Some(v) = hit_val {
                                v
                            } else {
                                let v = regs[base as usize].access(key).unwrap_or(Val::Nil);
                                if let (Some(mp), Some(kp)) = (mp, kp) {
                                    match access_ic[pc].as_mut() {
                                        Some(AccessIc::MapStr(slots)) => {
                                            let newe = MapStrEntry {
                                                map_ptr: mp,
                                                key_ptr: kp,
                                                value: v.clone(),
                                            };
                                            if slots[0].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp) {
                                                slots[0] = Some(newe);
                                            } else if slots[1]
                                                .as_ref()
                                                .is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp)
                                            {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            access_ic[pc] = Some(AccessIc::MapStr([
                                                Some(MapStrEntry {
                                                    map_ptr: mp,
                                                    key_ptr: kp,
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                } else if obj {
                                    match access_ic[pc].as_mut() {
                                        Some(AccessIc::ObjectStr(slots)) => {
                                            let newe = ObjectStrEntry {
                                                obj_ptr: std::sync::Arc::as_ptr(match &regs[base as usize] {
                                                    Val::Object { fields, .. } => fields,
                                                    _ => unreachable!(),
                                                }) as usize,
                                                key: s.as_ref().to_string(),
                                                value: v.clone(),
                                            };
                                            if slots[0].as_ref().is_some_and(|e| {
                                                e.obj_ptr == newe.obj_ptr && e.key.as_str() == s.as_ref()
                                            }) {
                                                slots[0] = Some(newe);
                                            } else if slots[1].as_ref().is_some_and(|e| {
                                                e.obj_ptr == newe.obj_ptr && e.key.as_str() == s.as_ref()
                                            }) {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            let optr = std::sync::Arc::as_ptr(match &regs[base as usize] {
                                                Val::Object { fields, .. } => fields,
                                                _ => unreachable!(),
                                            }) as usize;
                                            access_ic[pc] = Some(AccessIc::ObjectStr([
                                                Some(ObjectStrEntry {
                                                    obj_ptr: optr,
                                                    key: s.as_ref().to_string(),
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                }
                                v
                            }
                        } else {
                            Val::Nil
                        };
                        regs[dst as usize] = res;
                        pc += 1;
                    }
                    Op::IndexK(dst, base, kidx) => {
                        let key = &f.consts[kidx as usize];
                        let res = if let Val::Int(i) = key {
                            match &regs[base as usize] {
                                Val::List(l) => {
                                    if *i < 0 {
                                        Val::Nil
                                    } else {
                                        l.get(*i as usize).cloned().unwrap_or(Val::Nil)
                                    }
                                }
                                Val::Str(s) => {
                                    if *i < 0 {
                                        Val::Nil
                                    } else if s.is_ascii() {
                                        let bi = *i as usize;
                                        let bs = s.as_bytes();
                                        if bi < bs.len() {
                                            let ch = bs[bi] as char;
                                            Val::Str(ch.to_string().into())
                                        } else {
                                            Val::Nil
                                        }
                                    } else {
                                        s.chars()
                                            .nth(*i as usize)
                                            .map(|c| Val::Str(c.to_string().into()))
                                            .unwrap_or(Val::Nil)
                                    }
                                }
                                _ => Val::Nil,
                            }
                        } else {
                            Val::Nil
                        };
                        regs[dst as usize] = res;
                        pc += 1;
                    }
                    Op::BuildList { dst, base, len } => {
                        let start = base as usize;
                        let n = len as usize;
                        let mut v = Vec::with_capacity(n);
                        for i in 0..n {
                            v.push(regs[start + i].clone());
                        }
                        regs[dst as usize] = Val::List(v.into());
                        pc += 1;
                    }
                    Op::BuildMap { dst, base, len } => {
                        let start = base as usize;
                        let n = len as usize;
                        let mut map: FastHashMap<Arc<str>, Val> = fast_hash_map_with_capacity(n);
                        for i in 0..n {
                            let k = &regs[start + 2 * i];
                            let v = regs[start + 2 * i + 1].clone();
                            let key_arc: Arc<str> = match k {
                                Val::Str(s) => s.clone(),
                                Val::Int(i) => Arc::from(i.to_string()),
                                Val::Float(f) => Arc::from(f.to_string()),
                                Val::Bool(b) => Arc::from(b.to_string()),
                                _ => {
                                    return Err(anyhow!("Map key must be a primitive type, got: {:?}", k));
                                }
                            };
                            map.insert(key_arc, v);
                        }
                        regs[dst as usize] = Val::Map(Arc::new(map));
                        pc += 1;
                    }
                    Op::MakeClosure { dst, proto } => {
                        let p = f
                            .protos
                            .get(proto as usize)
                            .ok_or_else(|| anyhow!("closure proto out of range"))?;
                        if let Some(e) = env.as_ref() {
                            let clo = Val::Closure {
                                params: std::sync::Arc::new(p.params.clone()),
                                body: std::sync::Arc::new(p.body.clone()),
                                env: std::sync::Arc::new((**e).clone()),
                                upvalues: std::sync::Arc::new(Vec::new()),
                                #[cfg(feature = "vm")]
                                code: std::sync::Arc::new(once_cell::sync::OnceCell::new()),
                                #[cfg(feature = "slots")]
                                layout: std::sync::Arc::new(once_cell::sync::OnceCell::new()),
                            };
                            regs[dst as usize] = clo;
                        } else {
                            regs[dst as usize] = Val::Nil;
                        }
                        pc += 1;
                    }
                    Op::LoadLocal(dst, idx) => {
                        regs[dst as usize] = regs[idx as usize].clone();
                        pc += 1;
                    }
                    Op::StoreLocal(idx, src) => {
                        let v = regs[src as usize].clone();
                        regs[idx as usize] = v;
                        pc += 1;
                    }
                    Op::Call {
                        f: rf,
                        base,
                        argc,
                        retc,
                    } => {
                        let func = regs[rf as usize].clone();
                        let start = base as usize;
                        let n = argc as usize;
                        let args_slice: &[Val] = &regs[start..start + n];
                        let result = if let Some(e) = env.as_ref() {
                            if let Some(CallIc::Rust(fp, cached_argc)) = call_ic[pc]
                                && argc == cached_argc
                                && matches!(func, Val::RustFunction(_))
                            {
                                fp(args_slice, e)
                            } else {
                                match &func {
                                    Val::RustFunction(fptr) => {
                                        call_ic[pc] = Some(CallIc::Rust(*fptr, argc));
                                        fptr(args_slice, e)
                                    }
                                    _ => func.call(args_slice, e),
                                }
                            }
                        } else {
                            Err(anyhow!("Function call requires environment"))
                        }?;
                        if retc > 0 {
                            regs[base as usize] = result;
                        }
                        pc += 1;
                    }
                    Op::LoadCtx(dst) => {
                        regs[dst as usize] = Val::Nil;
                        pc += 1;
                    }
                    Op::JmpFalseSet { r, dst, ofs } => {
                        let cond_falsey = matches!(regs[r as usize], Val::Nil | Val::Bool(false));
                        if cond_falsey {
                            regs[dst as usize] = Val::Bool(false);
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::JmpTrueSet { r, dst, ofs } => {
                        let cond_truthy = !matches!(regs[r as usize], Val::Nil | Val::Bool(false));
                        if cond_truthy {
                            regs[dst as usize] = Val::Bool(true);
                            pc = ((pc as isize) + (ofs as isize)) as usize;
                        } else {
                            pc += 1;
                        }
                    }
                    Op::ListSlice { dst, src, start } => {
                        let (list, start_idx) = match (&regs[src as usize], &regs[start as usize]) {
                            (Val::List(l), Val::Int(i)) => (l, *i),
                            (a, b) => return Err(anyhow!("ListSlice expects (List, Int), got ({:?}, {:?})", a, b)),
                        };
                        if start_idx <= 0 {
                            regs[dst as usize] = Val::List(list.clone());
                        } else {
                            let s = start_idx as usize;
                            if s >= list.len() {
                                regs[dst as usize] = Val::List(Vec::<Val>::new().into());
                            } else {
                                regs[dst as usize] = Val::List((list[s..]).to_vec().into());
                            }
                        }
                        pc += 1;
                    }
                    _ => {
                        // Unreachable for bc32-packed functions (subset only)
                        return Err(anyhow!("bc32: unsupported opcode in packed function"));
                    }
                }
            }
            return Ok(Val::Nil);
        }

        // Default path: execute Op enum bytecode
        // Persist instruction-site caches across executions; only grow when needed.
        if access_ic.len() < f.code.len() {
            access_ic.resize(f.code.len(), None);
        }
        if index_ic.len() < f.code.len() {
            index_ic.resize(f.code.len(), None);
        }
        if global_ic.len() < f.code.len() {
            global_ic.resize(f.code.len(), None);
        }
        if call_ic.len() < f.code.len() {
            call_ic.resize(f.code.len(), None);
        }
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
                    if !Self::arith2_try_numeric(regs, *dst, *a, *b, |x, y| x + y, |x, y| x + y) {
                        // Fallback to high-level semantics (strings, lists, maps under features)
                        let out = crate::op::BinOp::Add.eval_vals(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = out;
                    }
                    pc += 1;
                }
                Op::Sub(dst, a, b) => {
                    if !Self::arith2_try_numeric(regs, *dst, *a, *b, |x, y| x - y, |x, y| x - y) {
                        let out = crate::op::BinOp::Sub.eval_vals(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = out;
                    }
                    pc += 1;
                }
                Op::Mul(dst, a, b) => {
                    if !Self::arith2_try_numeric(regs, *dst, *a, *b, |x, y| x * y, |x, y| x * y) {
                        let out = crate::op::BinOp::Mul.eval_vals(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = out;
                    }
                    pc += 1;
                }
                Op::Div(dst, a, b) => {
                    if !Self::arith2_try_numeric(regs, *dst, *a, *b, |x, y| x / y, |x, y| x / y) {
                        let out = crate::op::BinOp::Div.eval_vals(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = out;
                    }
                    pc += 1;
                }
                Op::Mod(dst, a, b) => {
                    match (&regs[*a as usize], &regs[*b as usize]) {
                        (Val::Int(x), Val::Int(y)) => regs[*dst as usize] = Val::Int(x % y),
                        _ => {
                            let out = crate::op::BinOp::Mod.eval_vals(&regs[*a as usize], &regs[*b as usize])?;
                            regs[*dst as usize] = out;
                        }
                    }
                    pc += 1;
                }
                Op::CmpEq(dst, a, b) => {
                    let r = regs[*a as usize] == regs[*b as usize];
                    regs[*dst as usize] = Val::Bool(r);
                    pc += 1;
                }
                Op::CmpNe(dst, a, b) => {
                    let r = regs[*a as usize] != regs[*b as usize];
                    regs[*dst as usize] = Val::Bool(r);
                    pc += 1;
                }
                Op::CmpLt(dst, a, b) => {
                    if !Self::cmp2_try_numeric(regs, *dst, *a, *b, |x, y| x < y, |x, y| x < y) {
                        let res = crate::op::BinOp::Lt.cmp(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = Val::Bool(res);
                    }
                    pc += 1;
                }
                Op::CmpLe(dst, a, b) => {
                    if !Self::cmp2_try_numeric(regs, *dst, *a, *b, |x, y| x <= y, |x, y| x <= y) {
                        let res = crate::op::BinOp::Le.cmp(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = Val::Bool(res);
                    }
                    pc += 1;
                }
                Op::CmpGt(dst, a, b) => {
                    if !Self::cmp2_try_numeric(regs, *dst, *a, *b, |x, y| x > y, |x, y| x > y) {
                        let res = crate::op::BinOp::Gt.cmp(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = Val::Bool(res);
                    }
                    pc += 1;
                }
                Op::CmpGe(dst, a, b) => {
                    if !Self::cmp2_try_numeric(regs, *dst, *a, *b, |x, y| x >= y, |x, y| x >= y) {
                        let res = crate::op::BinOp::Ge.cmp(&regs[*a as usize], &regs[*b as usize])?;
                        regs[*dst as usize] = Val::Bool(res);
                    }
                    pc += 1;
                }
                Op::In(dst, a, b) => {
                    let res = crate::op::BinOp::In.cmp(&regs[*a as usize], &regs[*b as usize])?;
                    regs[*dst as usize] = Val::Bool(res);
                    pc += 1;
                }
                Op::LoadLocal(dst, idx) => {
                    regs[*dst as usize] = regs[*idx as usize].clone();
                    pc += 1;
                }
                Op::StoreLocal(idx, src) => {
                    let v = regs[*src as usize].clone();
                    regs[*idx as usize] = v;
                    pc += 1;
                }
                Op::LoadGlobal(dst, name_k) => {
                    let name_val = &f.consts[*name_k as usize];
                    let mut out = Val::Nil;
                    if let Val::Str(s) = name_val {
                        let key_ptr = s.as_ref().as_ptr() as usize;
                        let cur_gen = if let Some(e) = env.as_ref() { e.generation() } else { 0 };
                        if let Some(GlobalEntry(ptr, v, generation)) = &global_ic[pc]
                            && *ptr == key_ptr
                            && *generation == cur_gen
                        {
                            out = v.clone();
                        }
                        if matches!(out, Val::Nil) {
                            if let Some(e) = env.as_ref()
                                && let Some(v) = e.get_value(s.as_ref())
                            {
                                out = v.clone();
                            }
                            global_ic[pc] = Some(GlobalEntry(key_ptr, out.clone(), cur_gen));
                        }
                    } else if let Some(e) = env.as_ref() {
                        // Non-string globals are uncommon; fall back
                        if let Some(v) = e.get_value(&format!("{}", name_val)) {
                            out = v;
                        }
                    }
                    regs[*dst as usize] = out;
                    pc += 1;
                }
                Op::DefineGlobal(name_k, src) => {
                    if let Some(e) = env.as_mut() {
                        let name_val = &f.consts[*name_k as usize];
                        if let Val::Str(s) = name_val {
                            e.define_global(s.to_string(), regs[*src as usize].clone());
                        }
                    }
                    // Env::define_global bumps global generation
                    pc += 1;
                }
                Op::LoadCtx(dst) => {
                    // Context is removed; load Nil for backward compatibility
                    regs[*dst as usize] = Val::Nil;
                    pc += 1;
                }
                Op::Access(dst, base, field) => {
                    // Polymorphic 2-way IC for Map[String] and Object[String]
                    let hit_val = match (&regs[*base as usize], &regs[*field as usize]) {
                        (Val::Map(m), Val::Str(s)) => {
                            let mp = Arc::as_ptr(m) as usize;
                            let kp = s.as_ref().as_ptr() as usize;
                            if let Some(AccessIc::MapStr(slots)) = &access_ic[pc] {
                                let mut out: Option<Val> = None;
                                for e in slots.iter().flatten() {
                                    if e.map_ptr == mp && e.key_ptr == kp {
                                        out = Some(e.value.clone());
                                        break;
                                    }
                                }
                                out
                            } else {
                                None
                            }
                        }
                        (Val::Object { fields, .. }, Val::Str(s)) => {
                            let optr = Arc::as_ptr(fields) as usize;
                            if let Some(AccessIc::ObjectStr(slots)) = &access_ic[pc] {
                                let mut out: Option<Val> = None;
                                for e in slots.iter().flatten() {
                                    if e.obj_ptr == optr && e.key.as_str() == s.as_ref() {
                                        out = Some(e.value.clone());
                                        break;
                                    }
                                }
                                out
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    let res = if let Some(v) = hit_val {
                        v
                    } else {
                        let v = regs[*base as usize].access(&regs[*field as usize]).unwrap_or(Val::Nil);
                        match (&regs[*base as usize], &regs[*field as usize]) {
                            (Val::Map(m), Val::Str(s)) => {
                                let mp = Arc::as_ptr(m) as usize;
                                let kp = s.as_ref().as_ptr() as usize;
                                match access_ic[pc].as_mut() {
                                    Some(AccessIc::MapStr(slots)) => {
                                        let newe = MapStrEntry {
                                            map_ptr: mp,
                                            key_ptr: kp,
                                            value: v.clone(),
                                        };
                                        if slots[0].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp) {
                                            slots[0] = Some(newe);
                                        } else if slots[1].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp)
                                        {
                                            slots[1] = Some(newe);
                                        } else {
                                            slots[3] = slots[2].clone();
                                            slots[2] = slots[1].clone();
                                            slots[1] = slots[0].clone();
                                            slots[0] = Some(newe);
                                        }
                                    }
                                    _ => {
                                        access_ic[pc] = Some(AccessIc::MapStr([
                                            Some(MapStrEntry {
                                                map_ptr: mp,
                                                key_ptr: kp,
                                                value: v.clone(),
                                            }),
                                            None,
                                            None,
                                            None,
                                        ]));
                                    }
                                }
                            }
                            (Val::Object { fields, .. }, Val::Str(s)) => {
                                let optr = Arc::as_ptr(fields) as usize;
                                match access_ic[pc].as_mut() {
                                    Some(AccessIc::ObjectStr(slots)) => {
                                        let newe = ObjectStrEntry {
                                            obj_ptr: optr,
                                            key: s.as_ref().to_string(),
                                            value: v.clone(),
                                        };
                                        if slots[0]
                                            .as_ref()
                                            .is_some_and(|e| e.obj_ptr == optr && e.key.as_str() == s.as_ref())
                                        {
                                            slots[0] = Some(newe);
                                        } else if slots[1]
                                            .as_ref()
                                            .is_some_and(|e| e.obj_ptr == optr && e.key.as_str() == s.as_ref())
                                        {
                                            slots[1] = Some(newe);
                                        } else {
                                            slots[3] = slots[2].clone();
                                            slots[2] = slots[1].clone();
                                            slots[1] = slots[0].clone();
                                            slots[0] = Some(newe);
                                        }
                                    }
                                    _ => {
                                        access_ic[pc] = Some(AccessIc::ObjectStr([
                                            Some(ObjectStrEntry {
                                                obj_ptr: optr,
                                                key: s.as_ref().to_string(),
                                                value: v.clone(),
                                            }),
                                            None,
                                            None,
                                            None,
                                        ]));
                                    }
                                }
                            }
                            _ => {}
                        }
                        v
                    };
                    regs[*dst as usize] = res;
                    pc += 1;
                }
                Op::AccessK(dst, base, kidx) => {
                    let key = &f.consts[*kidx as usize];
                    // Only valid for string constants; otherwise yield Nil
                    let res = if let Val::Str(s) = key {
                        let (hit_val, mp, kp, obj) = match &regs[*base as usize] {
                            Val::Map(m) => {
                                let mp = Arc::as_ptr(m) as usize;
                                let kp = s.as_ref().as_ptr() as usize;
                                if let Some(AccessIc::MapStr(slots)) = &access_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.map_ptr == mp && e.key_ptr == kp {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    (out, Some(mp), Some(kp), false)
                                } else {
                                    (None, Some(mp), Some(kp), false)
                                }
                            }
                            Val::Object { fields, .. } => {
                                let optr = Arc::as_ptr(fields) as usize;
                                if let Some(AccessIc::ObjectStr(slots)) = &access_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.obj_ptr == optr && e.key.as_str() == s.as_ref() {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    (out, None, None, true)
                                } else {
                                    (None, None, None, true)
                                }
                            }
                            _ => (None, None, None, false),
                        };
                        if let Some(v) = hit_val {
                            v
                        } else {
                            let v = regs[*base as usize].access(key).unwrap_or(Val::Nil);
                            if let (Some(mp), Some(kp)) = (mp, kp) {
                                match access_ic[pc].as_mut() {
                                    Some(AccessIc::MapStr(slots)) => {
                                        let newe = MapStrEntry {
                                            map_ptr: mp,
                                            key_ptr: kp,
                                            value: v.clone(),
                                        };
                                        if slots[0].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp) {
                                            slots[0] = Some(newe);
                                        } else if slots[1].as_ref().is_some_and(|e| e.map_ptr == mp && e.key_ptr == kp)
                                        {
                                            slots[1] = Some(newe);
                                        } else {
                                            slots[3] = slots[2].clone();
                                            slots[2] = slots[1].clone();
                                            slots[1] = slots[0].clone();
                                            slots[0] = Some(newe);
                                        }
                                    }
                                    _ => {
                                        access_ic[pc] = Some(AccessIc::MapStr([
                                            Some(MapStrEntry {
                                                map_ptr: mp,
                                                key_ptr: kp,
                                                value: v.clone(),
                                            }),
                                            None,
                                            None,
                                            None,
                                        ]));
                                    }
                                }
                            } else if obj {
                                match access_ic[pc].as_mut() {
                                    Some(AccessIc::ObjectStr(slots)) => {
                                        let newe = ObjectStrEntry {
                                            obj_ptr: Arc::as_ptr(match &regs[*base as usize] {
                                                Val::Object { fields, .. } => fields,
                                                _ => unreachable!(),
                                            }) as usize,
                                            key: s.as_ref().to_string(),
                                            value: v.clone(),
                                        };
                                        if slots[0]
                                            .as_ref()
                                            .is_some_and(|e| e.obj_ptr == newe.obj_ptr && e.key.as_str() == s.as_ref())
                                        {
                                            slots[0] = Some(newe);
                                        } else if slots[1]
                                            .as_ref()
                                            .is_some_and(|e| e.obj_ptr == newe.obj_ptr && e.key.as_str() == s.as_ref())
                                        {
                                            slots[1] = Some(newe);
                                        } else {
                                            slots[3] = slots[2].clone();
                                            slots[2] = slots[1].clone();
                                            slots[1] = slots[0].clone();
                                            slots[0] = Some(newe);
                                        }
                                    }
                                    _ => {
                                        let optr = Arc::as_ptr(match &regs[*base as usize] {
                                            Val::Object { fields, .. } => fields,
                                            _ => unreachable!(),
                                        }) as usize;
                                        access_ic[pc] = Some(AccessIc::ObjectStr([
                                            Some(ObjectStrEntry {
                                                obj_ptr: optr,
                                                key: s.as_ref().to_string(),
                                                value: v.clone(),
                                            }),
                                            None,
                                            None,
                                            None,
                                        ]));
                                    }
                                }
                            }
                            v
                        }
                    } else {
                        Val::Nil
                    };
                    regs[*dst as usize] = res;
                    pc += 1;
                }
                Op::Len { dst, src } => {
                    let v = &regs[*src as usize];
                    let out = match v {
                        Val::List(l) => Val::Int(l.len() as i64),
                        Val::Str(s) => Val::Int(s.len() as i64),
                        Val::Map(m) => Val::Int(m.len() as i64),
                        _ => Val::Int(0),
                    };
                    regs[*dst as usize] = out;
                    pc += 1;
                }
                Op::Index { dst, base, idx } => {
                    let res = match (&regs[*base as usize], &regs[*idx as usize]) {
                        (Val::List(l), Val::Int(i)) => {
                            if *i < 0 {
                                Val::Nil
                            } else {
                                let lptr = Arc::as_ptr(l) as *const Val as usize;
                                let hit = if let Some(IndexIc::List(slots)) = &index_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.base_ptr == lptr && e.idx == *i {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    out
                                } else {
                                    None
                                };
                                if let Some(v) = hit {
                                    v
                                } else {
                                    let v = l.get(*i as usize).cloned().unwrap_or(Val::Nil);
                                    match index_ic[pc].as_mut() {
                                        Some(IndexIc::List(slots)) => {
                                            let newe = ListEntry {
                                                base_ptr: lptr,
                                                idx: *i,
                                                value: v.clone(),
                                            };
                                            if slots[0].as_ref().is_some_and(|e| e.base_ptr == lptr && e.idx == *i) {
                                                slots[0] = Some(newe);
                                            } else if slots[1]
                                                .as_ref()
                                                .is_some_and(|e| e.base_ptr == lptr && e.idx == *i)
                                            {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            index_ic[pc] = Some(IndexIc::List([
                                                Some(ListEntry {
                                                    base_ptr: lptr,
                                                    idx: *i,
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                    v
                                }
                            }
                        }
                        (Val::Str(s), Val::Int(i)) => {
                            if *i < 0 {
                                Val::Nil
                            } else {
                                let sptr = s.as_ref().as_ptr() as usize;
                                let hit = if let Some(IndexIc::Str(slots)) = &index_ic[pc] {
                                    let mut out: Option<Val> = None;
                                    for e in slots.iter().flatten() {
                                        if e.base_ptr == sptr && e.idx == *i {
                                            out = Some(e.value.clone());
                                            break;
                                        }
                                    }
                                    out
                                } else {
                                    None
                                };
                                if let Some(v) = hit {
                                    v
                                } else {
                                    let v = s
                                        .chars()
                                        .nth(*i as usize)
                                        .map(|c| Val::Str(c.to_string().into()))
                                        .unwrap_or(Val::Nil);
                                    match index_ic[pc].as_mut() {
                                        Some(IndexIc::Str(slots)) => {
                                            let newe = StrEntry {
                                                base_ptr: sptr,
                                                idx: *i,
                                                value: v.clone(),
                                            };
                                            if slots[0].as_ref().is_some_and(|e| e.base_ptr == sptr && e.idx == *i) {
                                                slots[0] = Some(newe);
                                            } else if slots[1]
                                                .as_ref()
                                                .is_some_and(|e| e.base_ptr == sptr && e.idx == *i)
                                            {
                                                slots[1] = Some(newe);
                                            } else {
                                                slots[3] = slots[2].clone();
                                                slots[2] = slots[1].clone();
                                                slots[1] = slots[0].clone();
                                                slots[0] = Some(newe);
                                            }
                                        }
                                        _ => {
                                            index_ic[pc] = Some(IndexIc::Str([
                                                Some(StrEntry {
                                                    base_ptr: sptr,
                                                    idx: *i,
                                                    value: v.clone(),
                                                }),
                                                None,
                                                None,
                                                None,
                                            ]));
                                        }
                                    }
                                    v
                                }
                            }
                        }
                        _ => Val::Nil,
                    };
                    regs[*dst as usize] = res;
                    pc += 1;
                }
                Op::IndexK(dst, base, kidx) => {
                    let key = &f.consts[*kidx as usize];
                    let res = if let Val::Int(i) = key {
                        match &regs[*base as usize] {
                            Val::List(l) => {
                                if *i < 0 {
                                    Val::Nil
                                } else {
                                    l.get(*i as usize).cloned().unwrap_or(Val::Nil)
                                }
                            }
                            Val::Str(s) => {
                                if *i < 0 {
                                    Val::Nil
                                } else if s.is_ascii() {
                                    let bi = *i as usize;
                                    let bs = s.as_bytes();
                                    if bi < bs.len() {
                                        let ch = bs[bi] as char;
                                        Val::Str(ch.to_string().into())
                                    } else {
                                        Val::Nil
                                    }
                                } else {
                                    s.chars()
                                        .nth(*i as usize)
                                        .map(|c| Val::Str(c.to_string().into()))
                                        .unwrap_or(Val::Nil)
                                }
                            }
                            _ => Val::Nil,
                        }
                    } else {
                        Val::Nil
                    };
                    regs[*dst as usize] = res;
                    pc += 1;
                }
                Op::ToIter { dst, src } => {
                    let out = match &regs[*src as usize] {
                        Val::List(_) | Val::Str(_) => regs[*src as usize].clone(),
                        Val::Map(m) => {
                            let mut keys: Vec<&str> = m.keys().map(|k| k.as_ref()).collect();
                            keys.sort();
                            let mut pairs = Vec::with_capacity(keys.len());
                            for k in keys {
                                if let Some(v) = m.get(k) {
                                    let pair = Val::List(vec![Val::Str(k.to_string().into()), v.clone()].into());
                                    pairs.push(pair);
                                }
                            }
                            Val::List(pairs.into())
                        }
                        _ => Val::List(Vec::<Val>::new().into()),
                    };
                    regs[*dst as usize] = out;
                    pc += 1;
                }
                Op::BuildList { dst, base, len } => {
                    let start = *base as usize;
                    let n = *len as usize;
                    let mut v = Vec::with_capacity(n);
                    for i in 0..n {
                        v.push(regs[start + i].clone());
                    }
                    regs[*dst as usize] = Val::List(v.into());
                    pc += 1;
                }
                Op::BuildMap { dst, base, len } => {
                    let start = *base as usize;
                    let n = *len as usize;
                    let mut map: FastHashMap<Arc<str>, Val> = fast_hash_map_with_capacity(n);
                    for i in 0..n {
                        let k = &regs[start + 2 * i];
                        let v = regs[start + 2 * i + 1].clone();
                        let key_arc: Arc<str> = match k {
                            Val::Str(s) => s.clone(),
                            Val::Int(i) => Arc::from(i.to_string()),
                            Val::Float(f) => Arc::from(f.to_string()),
                            Val::Bool(b) => Arc::from(b.to_string()),
                            _ => {
                                return Err(anyhow!("Map key must be a primitive type, got: {:?}", k));
                            }
                        };
                        map.insert(key_arc, v);
                    }
                    regs[*dst as usize] = Val::Map(Arc::new(map));
                    pc += 1;
                }
                Op::ListSlice { dst, src, start } => {
                    let (list, start_idx) = match (&regs[*src as usize], &regs[*start as usize]) {
                        (Val::List(l), Val::Int(i)) => (l, *i),
                        (a, b) => return Err(anyhow!("ListSlice expects (List, Int), got ({:?}, {:?})", a, b)),
                    };
                    if start_idx <= 0 {
                        regs[*dst as usize] = Val::List(list.clone());
                    } else {
                        let s = start_idx as usize;
                        if s >= list.len() {
                            regs[*dst as usize] = Val::List(Vec::<Val>::new().into());
                        } else {
                            regs[*dst as usize] = Val::List((list[s..]).to_vec().into());
                        }
                    }
                    pc += 1;
                }
                Op::ForRangePrep {
                    idx,
                    limit,
                    step,
                    inclusive: _,
                    explicit,
                } => {
                    // Determine step at runtime based on start and limit if not explicit; integers only.
                    let (i0, ilim) = match (&regs[*idx as usize], &regs[*limit as usize]) {
                        (Val::Int(a), Val::Int(b)) => (*a, *b),
                        _ => {
                            return Err(anyhow!(
                                "For-range requires integer bounds, got idx={:?}, limit={:?}",
                                regs[*idx as usize],
                                regs[*limit as usize]
                            ));
                        }
                    };
                    if !*explicit {
                        let step_val = if i0 <= ilim { 1 } else { -1 };
                        regs[*step as usize] = Val::Int(step_val);
                    } else {
                        // Validate explicit step is Int and non-zero
                        match &regs[*step as usize] {
                            Val::Int(0) => return Err(anyhow!("For-range step cannot be zero")),
                            Val::Int(_) => {}
                            other => return Err(anyhow!("For-range step must be Int when explicit, got {:?}", other)),
                        }
                        // leave provided step as-is
                    }
                    pc += 1;
                }
                Op::ForRangeGuard {
                    idx,
                    limit,
                    step,
                    inclusive,
                    ofs,
                } => {
                    // Guard: if not within range, jump to end
                    let (i, lim, st) = match (&regs[*idx as usize], &regs[*limit as usize], &regs[*step as usize]) {
                        (Val::Int(i), Val::Int(l), Val::Int(s)) => (*i, *l, *s),
                        _ => {
                            return Err(anyhow!(
                                "For-range guard expects Int registers, got idx={:?}, limit={:?}, step={:?}",
                                regs[*idx as usize],
                                regs[*limit as usize],
                                regs[*step as usize]
                            ));
                        }
                    };
                    let cont = if st > 0 {
                        if *inclusive { i <= lim } else { i < lim }
                    } else if *inclusive {
                        i >= lim
                    } else {
                        i > lim
                    };
                    if !cont {
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::ForRangeStep { idx, step, back_ofs } => {
                    let (i, st) = match (&regs[*idx as usize], &regs[*step as usize]) {
                        (Val::Int(i), Val::Int(s)) => (*i, *s),
                        _ => {
                            return Err(anyhow!(
                                "For-range step expects Int registers, got idx={:?}, step={:?}",
                                regs[*idx as usize],
                                regs[*step as usize]
                            ));
                        }
                    };
                    regs[*idx as usize] = Val::Int(i + st);
                    pc = ((pc as isize) + (*back_ofs as isize)) as usize;
                }
                Op::MakeClosure { dst, proto } => {
                    let p = f
                        .protos
                        .get(*proto as usize)
                        .ok_or_else(|| anyhow!("closure proto out of range"))?;
                    if let Some(e) = env.as_ref() {
                        let clo = Val::Closure {
                            params: Arc::new(p.params.clone()),
                            body: Arc::new(p.body.clone()),
                            env: Arc::new((*e).clone()),
                            upvalues: Arc::new(Vec::new()),
                            #[cfg(feature = "vm")]
                            code: Arc::new(once_cell::sync::OnceCell::new()),
                            #[cfg(feature = "slots")]
                            layout: Arc::new(once_cell::sync::OnceCell::new()),
                        };
                        regs[*dst as usize] = clo;
                    } else {
                        regs[*dst as usize] = Val::Nil;
                    }
                    pc += 1;
                }
                Op::Not(dst, src) => {
                    match &regs[*src as usize] {
                        Val::Bool(b) => regs[*dst as usize] = Val::Bool(!b),
                        other => return Err(anyhow!("Invalid operand: !{:?}", other)),
                    }
                    pc += 1;
                }
                Op::ToBool(dst, src) => {
                    let truthy = !matches!(regs[*src as usize], Val::Nil | Val::Bool(false));
                    regs[*dst as usize] = Val::Bool(truthy);
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
                Op::JmpFalseSet { r, dst, ofs } => {
                    let cond_falsey = matches!(regs[*r as usize], Val::Nil | Val::Bool(false));
                    if cond_falsey {
                        regs[*dst as usize] = Val::Bool(false);
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::JmpIfNil(r, ofs) => {
                    if matches!(regs[*r as usize], Val::Nil) {
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::JmpIfNotNil(r, ofs) => {
                    if !matches!(regs[*r as usize], Val::Nil) {
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::NullishPick { l, dst, ofs } => {
                    if !matches!(regs[*l as usize], Val::Nil) {
                        regs[*dst as usize] = regs[*l as usize].clone();
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::JmpTrueSet { r, dst, ofs } => {
                    let cond_truthy = !matches!(regs[*r as usize], Val::Nil | Val::Bool(false));
                    if cond_truthy {
                        regs[*dst as usize] = Val::Bool(true);
                        pc = ((pc as isize) + (*ofs as isize)) as usize;
                    } else {
                        pc += 1;
                    }
                }
                Op::Call {
                    f: rf,
                    base,
                    argc,
                    retc,
                } => {
                    let func = regs[*rf as usize].clone();
                    let start = *base as usize;
                    let n = *argc as usize;
                    // Pass a borrowed slice of registers as arguments to avoid cloning.
                    let args_slice: &[Val] = &regs[start..start + n];
                    let result = if let Some(e) = env.as_ref() {
                        // Check call-site IC for Rust function
                        if let Some(CallIc::Rust(fp, cached_argc)) = call_ic[pc]
                            && *argc == cached_argc
                            && matches!(func, Val::RustFunction(_))
                        {
                            fp(args_slice, e)
                        } else {
                            match &func {
                                Val::RustFunction(fptr) => {
                                    call_ic[pc] = Some(CallIc::Rust(*fptr, *argc));
                                    fptr(args_slice, e)
                                }
                                _ => func.call(args_slice, e),
                            }
                        }
                    } else {
                        Err(anyhow!("Function call requires environment"))
                    }?;
                    if *retc > 0 {
                        regs[*base as usize] = result;
                    }
                    pc += 1;
                }
                Op::Ret { base, retc } => {
                    let ret = if *retc > 0 {
                        regs[*base as usize].clone()
                    } else {
                        Val::Nil
                    };
                    return Ok(ret);
                }
            }
        }
        Ok(Val::Nil)
    }

    #[inline(always)]
    fn arith2_try_numeric(
        regs: &mut [Val],
        dst: u16,
        a: u16,
        b: u16,
        iop: impl FnOnce(i64, i64) -> i64,
        fop: impl FnOnce(f64, f64) -> f64,
    ) -> bool {
        match (&regs[a as usize], &regs[b as usize]) {
            (Val::Int(x), Val::Int(y)) => {
                regs[dst as usize] = Val::Int(iop(*x, *y));
                true
            }
            (Val::Float(x), Val::Float(y)) => {
                regs[dst as usize] = Val::Float(fop(*x, *y));
                true
            }
            // Mixed numeric: promote to Float
            (Val::Int(x), Val::Float(y)) => {
                regs[dst as usize] = Val::Float(fop(*x as f64, *y));
                true
            }
            (Val::Float(x), Val::Int(y)) => {
                regs[dst as usize] = Val::Float(fop(*x, *y as f64));
                true
            }
            _ => false,
        }
    }

    #[inline(always)]
    fn cmp2_try_numeric(
        regs: &mut [Val],
        dst: u16,
        a: u16,
        b: u16,
        iop: impl FnOnce(i64, i64) -> bool,
        fop: impl FnOnce(f64, f64) -> bool,
    ) -> bool {
        let res_opt = match (&regs[a as usize], &regs[b as usize]) {
            (Val::Int(x), Val::Int(y)) => Some(iop(*x, *y)),
            (Val::Float(x), Val::Float(y)) => Some(fop(*x, *y)),
            (Val::Int(x), Val::Float(y)) => Some(fop(*x as f64, *y)),
            (Val::Float(x), Val::Int(y)) => Some(fop(*x, *y as f64)),
            _ => None,
        };
        if let Some(res) = res_opt {
            regs[dst as usize] = Val::Bool(res);
            true
        } else {
            false
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
