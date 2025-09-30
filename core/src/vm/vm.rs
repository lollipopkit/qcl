use anyhow::{Result, anyhow};

use crate::val::Val;

use super::bytecode::{Function, Op};

/// Minimal VM loop that can execute the placeholder Function produced by the stub compiler.
pub struct Vm;

impl Vm {
    pub fn new() -> Self { Self }

    pub fn exec(&mut self, f: &Function) -> Result<Val> {
        self.exec_with(f, None, &Val::Nil)
    }

    pub fn exec_with(
        &mut self,
        f: &Function,
        mut env: Option<&mut crate::stmt::Environment>,
        ctx: &Val,
    ) -> Result<Val> {
        let mut regs: Vec<Val> = vec![Val::Nil; f.n_regs as usize];
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
                Op::Add(dst, a, b) => { Self::arith2(&mut regs, *dst, *a, *b, |x,y| x+y, |x,y| x+y); pc += 1; }
                Op::Sub(dst, a, b) => { Self::arith2(&mut regs, *dst, *a, *b, |x,y| x-y, |x,y| x-y); pc += 1; }
                Op::Mul(dst, a, b) => { Self::arith2(&mut regs, *dst, *a, *b, |x,y| x*y, |x,y| x*y); pc += 1; }
                Op::Div(dst, a, b) => { Self::arith2(&mut regs, *dst, *a, *b, |x,y| x/y, |x,y| x/y); pc += 1; }
                Op::Mod(dst, a, b) => {
                    match (&regs[*a as usize], &regs[*b as usize]) {
                        (Val::Int(x), Val::Int(y)) => regs[*dst as usize] = Val::Int(x % y),
                        _ => regs[*dst as usize] = Val::Nil,
                    }
                    pc += 1;
                }
                Op::CmpEq(dst, a, b) => { let r = (regs[*a as usize] == regs[*b as usize]); regs[*dst as usize] = Val::Bool(r); pc += 1; }
                Op::CmpNe(dst, a, b) => { let r = (regs[*a as usize] != regs[*b as usize]); regs[*dst as usize] = Val::Bool(r); pc += 1; }
                Op::CmpLt(dst, a, b) => { Self::cmp2(&mut regs, *dst, *a, *b, |x,y| x<y, |x,y| x<y); pc += 1; }
                Op::CmpLe(dst, a, b) => { Self::cmp2(&mut regs, *dst, *a, *b, |x,y| x<=y, |x,y| x<=y); pc += 1; }
                Op::CmpGt(dst, a, b) => { Self::cmp2(&mut regs, *dst, *a, *b, |x,y| x>y, |x,y| x>y); pc += 1; }
                Op::CmpGe(dst, a, b) => { Self::cmp2(&mut regs, *dst, *a, *b, |x,y| x>=y, |x,y| x>=y); pc += 1; }
                Op::LoadLocal(dst, idx) => { regs[*dst as usize] = regs[*idx as usize].clone(); pc += 1; }
                Op::StoreLocal(idx, src) => { let v = regs[*src as usize].clone(); regs[*idx as usize] = v; pc += 1; }
                Op::LoadGlobal(dst, name_k) => {
                    let name_val = &f.consts[*name_k as usize];
                    let mut out = Val::Nil;
                    if let Val::Str(s) = name_val {
                        if let Some(e) = env.as_ref() {
                            if let Some(v) = e.get_value(s.as_ref()) { out = v; }
                        }
                    }
                    regs[*dst as usize] = out;
                    pc += 1;
                }
                Op::DefineGlobal(name_k, src) => {
                    if let Some(e) = env.as_mut() {
                        let name_val = &f.consts[*name_k as usize];
                        if let Val::Str(s) = name_val { e.define(s.to_string(), regs[*src as usize].clone()); }
                    }
                    pc += 1;
                }
                Op::LoadCtx(dst) => { regs[*dst as usize] = ctx.clone(); pc += 1; }
                Op::Access(dst, base, field) => {
                    let res = regs[*base as usize].access(&regs[*field as usize]).unwrap_or(Val::Nil);
                    regs[*dst as usize] = res;
                    pc += 1;
                }
                Op::BuildList { dst, base, len } => {
                    let start = *base as usize; let n = *len as usize;
                    let mut v = Vec::with_capacity(n);
                    for i in 0..n { v.push(regs[start + i].clone()); }
                    regs[*dst as usize] = Val::List(v.into());
                    pc += 1;
                }
                Op::BuildMap { dst, base, len } => {
                    let start = *base as usize; let n = *len as usize;
                    let mut map: std::collections::HashMap<String, Val> = std::collections::HashMap::with_capacity(n);
                    for i in 0..n { let k = &regs[start + 2*i]; let v = regs[start + 2*i + 1].clone();
                        let key_str = match k {
                            Val::Str(s) => s.as_ref().to_string(),
                            Val::Int(i) => i.to_string(),
                            Val::Float(f) => f.to_string(),
                            Val::Bool(b) => b.to_string(),
                            _ => return Err(anyhow!("Map key must be a primitive type, got: {:?}", k)),
                        };
                        map.insert(key_str, v);
                    }
                    regs[*dst as usize] = Val::from(map);
                    pc += 1;
                }
                Op::MakeClosure { dst, proto } => {
                    let p = f.protos.get(*proto as usize).ok_or_else(|| anyhow!("closure proto out of range"))?;
                    if let Some(e) = env.as_ref() {
                        let clo = Val::Closure {
                            params: std::sync::Arc::new(p.params.clone()),
                            body: std::sync::Arc::new(p.body.clone()),
                            env: std::sync::Arc::new((*e).clone()),
                            upvalues: std::sync::Arc::new(Vec::new()),
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
                Op::Call { f: rf, base, argc, retc } => {
                    let func = regs[*rf as usize].clone();
                    let start = *base as usize; let n = *argc as usize;
                    let mut args: Vec<Val> = Vec::with_capacity(n);
                    for i in 0..n { args.push(regs[start + i].clone()); }
                    let result = if let Some(e) = env.as_ref() { func.call(&args, e, ctx) } else { Err(anyhow!("Function call requires environment")) }?;
                    if *retc > 0 { regs[*base as usize] = result; }
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

    fn arith2(
        regs: &mut [Val], dst: u16, a: u16, b: u16,
        iop: impl FnOnce(i64,i64)->i64,
        fop: impl FnOnce(f64,f64)->f64,
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
        regs: &mut [Val], dst: u16, a: u16, b: u16,
        iop: impl FnOnce(i64,i64)->bool,
        fop: impl FnOnce(f64,f64)->bool,
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
