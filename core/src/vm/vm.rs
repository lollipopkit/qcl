use anyhow::{Result, anyhow};

use crate::val::Val;

use super::bytecode::{Function, Op};

/// Minimal VM loop that can execute the placeholder Function produced by the stub compiler.
/// Reuses an internal register vector across executions to reduce allocations.
pub struct Vm {
    regs: Vec<Val>,
}

impl Vm {
    pub fn new() -> Self {
        Self { regs: Vec::new() }
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
        // Ensure capacity and initialize registers to Nil without reallocating where possible.
        let regs = &mut self.regs;
        regs.clear();
        regs.resize(f.n_regs as usize, Val::Nil);
        // Seed parameter registers directly from provided args, if any.
        if let Some(a) = args && !f.param_regs.is_empty() {
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
                    Self::arith2(regs, *dst, *a, *b, |x, y| x + y, |x, y| x + y);
                    pc += 1;
                }
                Op::Sub(dst, a, b) => {
                    Self::arith2(regs, *dst, *a, *b, |x, y| x - y, |x, y| x - y);
                    pc += 1;
                }
                Op::Mul(dst, a, b) => {
                    Self::arith2(regs, *dst, *a, *b, |x, y| x * y, |x, y| x * y);
                    pc += 1;
                }
                Op::Div(dst, a, b) => {
                    Self::arith2(regs, *dst, *a, *b, |x, y| x / y, |x, y| x / y);
                    pc += 1;
                }
                Op::Mod(dst, a, b) => {
                    match (&regs[*a as usize], &regs[*b as usize]) {
                        (Val::Int(x), Val::Int(y)) => regs[*dst as usize] = Val::Int(x % y),
                        _ => regs[*dst as usize] = Val::Nil,
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
                    Self::cmp2(regs, *dst, *a, *b, |x, y| x < y, |x, y| x < y);
                    pc += 1;
                }
                Op::CmpLe(dst, a, b) => {
                    Self::cmp2(regs, *dst, *a, *b, |x, y| x <= y, |x, y| x <= y);
                    pc += 1;
                }
                Op::CmpGt(dst, a, b) => {
                    Self::cmp2(regs, *dst, *a, *b, |x, y| x > y, |x, y| x > y);
                    pc += 1;
                }
                Op::CmpGe(dst, a, b) => {
                    Self::cmp2(regs, *dst, *a, *b, |x, y| x >= y, |x, y| x >= y);
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
                    if let Val::Str(s) = name_val
                        && let Some(e) = env.as_ref()
                        && let Some(v) = e.get_value(s.as_ref())
                    {
                        out = v;
                    }
                    regs[*dst as usize] = out;
                    pc += 1;
                }
                Op::DefineGlobal(name_k, src) => {
                    if let Some(e) = env.as_mut() {
                        let name_val = &f.consts[*name_k as usize];
                        if let Val::Str(s) = name_val {
                            e.define(s.to_string(), regs[*src as usize].clone());
                        }
                    }
                    pc += 1;
                }
                Op::LoadCtx(dst) => {
                    // Context is removed; load Nil for backward compatibility
                    regs[*dst as usize] = Val::Nil;
                    pc += 1;
                }
                Op::Access(dst, base, field) => {
                    let res = regs[*base as usize].access(&regs[*field as usize]).unwrap_or(Val::Nil);
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
                                None
                            } else {
                                l.get(*i as usize).cloned()
                            }
                        }
                        (Val::Str(s), Val::Int(i)) => {
                            if *i < 0 {
                                None
                            } else {
                                s.chars().nth(*i as usize).map(|c| Val::Str(c.to_string().into()))
                            }
                        }
                        _ => None,
                    }
                    .unwrap_or(Val::Nil);
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
                    let mut map: std::collections::HashMap<String, Val> = std::collections::HashMap::with_capacity(n);
                    for i in 0..n {
                        let k = &regs[start + 2 * i];
                        let v = regs[start + 2 * i + 1].clone();
                        let key_str = match k {
                            Val::Str(s) => s.as_ref().to_string(),
                            Val::Int(i) => i.to_string(),
                            Val::Float(f) => f.to_string(),
                            Val::Bool(b) => b.to_string(),
                            _ => {
                                return Err(anyhow!("Map key must be a primitive type, got: {:?}", k));
                            }
                        };
                        map.insert(key_str, v);
                    }
                    regs[*dst as usize] = Val::from(map);
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
                            params: std::sync::Arc::new(p.params.clone()),
                            body: std::sync::Arc::new(p.body.clone()),
                            env: std::sync::Arc::new((*e).clone()),
                            upvalues: std::sync::Arc::new(Vec::new()),
                            #[cfg(feature = "vm")]
                            code: std::sync::Arc::new(once_cell::sync::OnceCell::new()),
                            #[cfg(feature = "slots")]
                            layout: std::sync::Arc::new(once_cell::sync::OnceCell::new()),
                        };
                        regs[*dst as usize] = clo;
                    } else {
                        regs[*dst as usize] = Val::Nil;
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
                        match &func {
                            Val::RustFunction(f) => f(args_slice, e),
                            _ => func.call(args_slice, e),
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

    fn arith2(
        regs: &mut [Val],
        dst: u16,
        a: u16,
        b: u16,
        iop: impl FnOnce(i64, i64) -> i64,
        fop: impl FnOnce(f64, f64) -> f64,
    ) {
        match (&regs[a as usize], &regs[b as usize]) {
            (Val::Int(x), Val::Int(y)) => regs[dst as usize] = Val::Int(iop(*x, *y)),
            (Val::Float(x), Val::Float(y)) => regs[dst as usize] = Val::Float(fop(*x, *y)),
            // Mixed numeric: promote to Float
            (Val::Int(x), Val::Float(y)) => regs[dst as usize] = Val::Float(fop(*x as f64, *y)),
            (Val::Float(x), Val::Int(y)) => regs[dst as usize] = Val::Float(fop(*x, *y as f64)),
            _ => regs[dst as usize] = Val::Nil,
        }
    }

    fn cmp2(
        regs: &mut [Val],
        dst: u16,
        a: u16,
        b: u16,
        iop: impl FnOnce(i64, i64) -> bool,
        fop: impl FnOnce(f64, f64) -> bool,
    ) {
        let res = match (&regs[a as usize], &regs[b as usize]) {
            (Val::Int(x), Val::Int(y)) => iop(*x, *y),
            (Val::Float(x), Val::Float(y)) => fop(*x, *y),
            (Val::Int(x), Val::Float(y)) => fop(*x as f64, *y),
            (Val::Float(x), Val::Int(y)) => fop(*x, *y as f64),
            _ => false,
        };
        regs[dst as usize] = Val::Bool(res);
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
