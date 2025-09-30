use std::collections::HashMap;

use crate::{expr::Expr, stmt::Stmt, val::Val};

use super::bytecode::{Function, Op, ClosureProto};

/// A simple single-function compiler that lowers a subset of Stmt/Expr to register bytecode.
/// - Locals are assigned sequential indices at first definition and never deallocated.
/// - Control flow supports if/while/return and expression statements.
pub struct Compiler;

impl Compiler {
    pub fn new() -> Self { Self }

    /// Compile a single expression into a self-contained function.
    pub fn compile_expr(&self, expr: &Expr) -> Function {
        let mut b = FunctionBuilder::new();
        let dst = b.expr(expr);
        b.emit(Op::Ret { base: dst, retc: 1 });
        b.finish()
    }

    /// Compile a statement into a function. Returns from explicit `return`;
    /// if no return occurs, returns Nil.
    pub fn compile_stmt(&self, stmt: &Stmt) -> Function {
        let mut b = FunctionBuilder::new();
        b.stmt(stmt);
        let k = b.k(Val::Nil);
        let r0 = b.alloc();
        b.emit(Op::LoadK(r0, k));
        b.emit(Op::Ret { base: r0, retc: 1 });
        b.finish()
    }
}

struct FunctionBuilder {
    consts: Vec<Val>,
    code: Vec<Op>,
    n_regs: u16,
    vars: HashMap<String, u16>,
    protos: Vec<ClosureProto>,
}

impl FunctionBuilder {
    fn new() -> Self {
        Self { consts: Vec::new(), code: Vec::new(), n_regs: 0, vars: HashMap::new(), protos: Vec::new() }
    }
    fn finish(self) -> Function { Function { consts: self.consts, code: self.code, n_regs: self.n_regs, protos: self.protos } }
    fn emit(&mut self, op: Op) { self.code.push(op); }
    fn alloc(&mut self) -> u16 { let r = self.n_regs; self.n_regs = self.n_regs.saturating_add(1); r }
    fn k(&mut self, v: Val) -> u16 {
        if let Some((i, _)) = self.consts.iter().enumerate().find(|(_, x)| *x == &v) { i as u16 } else { self.consts.push(v); (self.consts.len() - 1) as u16 }
    }
    fn get_or_define(&mut self, name: &str) -> u16 {
        if let Some(&i) = self.vars.get(name) { i } else { let idx = self.alloc(); self.vars.insert(name.to_string(), idx); idx }
    }
    fn lookup(&self, name: &str) -> Option<u16> { self.vars.get(name).copied() }

    // Expression compilation returns the register containing the result
    fn expr(&mut self, e: &Expr) -> u16 {
        use crate::op::BinOp;
        match e {
            Expr::Val(v) => {
                let dst = self.alloc();
                let k = self.k(v.clone());
                self.emit(Op::LoadK(dst, k));
                dst
            }
            Expr::Var(name) => {
                let dst = self.alloc();
                if let Some(idx) = self.lookup(name) {
                    self.emit(Op::LoadLocal(dst, idx));
                } else {
                    // Try global lookup at runtime
                    let kname = self.k(Val::Str(name.clone().into()));
                    self.emit(Op::LoadGlobal(dst, kname));
                }
                dst
            }
            Expr::Paren(inner) => self.expr(inner),
            Expr::Unary(uop, inner) => {
                let r = self.expr(inner);
                // Lower some unary ops via constants and comparisons
                match uop {
                    crate::op::UnaryOp::Not => {
                        // !x  ->  (x == false) or (x is Nil/false)
                        // Implement as JmpFalse/LoadK/Move
                        let out = self.alloc();
                        let k_true = self.k(Val::Bool(true));
                        let k_false = self.k(Val::Bool(false));
                        // if !r -> out=true else out=false
                        let jslot = self.code.len();
                        self.emit(Op::JmpFalse(r, 0)); // patch later
                        self.emit(Op::LoadK(out, k_false));
                        let jend = self.code.len();
                        self.emit(Op::Jmp(0));
                        // falsey branch
                        let target = self.code.len();
                        // patch JmpFalse to jump here
                        if let Op::JmpFalse(_, ref mut ofs) = self.code[jslot] { *ofs = (target as isize - jslot as isize) as i16; }
                        self.emit(Op::LoadK(out, k_true));
                        // end label
                        let end = self.code.len();
                        if let Op::Jmp(ref mut ofs) = self.code[jend] { *ofs = (end as isize - jend as isize) as i16; }
                        out
                    }
                }
            }
            Expr::And(l, r) => {
                let out = self.alloc();
                let rl = self.expr(l);
                // if !rl -> out=false
                let jf = self.code.len();
                self.emit(Op::JmpFalse(rl, 0)); // patch
                let rr = self.expr(r);
                // out = rl && rr
                let k_true = self.k(Val::Bool(true));
                let k_false = self.k(Val::Bool(false));
                // if rr -> out=true else out=false
                let jf2 = self.code.len();
                self.emit(Op::JmpFalse(rr, 0)); // to false branch
                self.emit(Op::LoadK(out, k_true));
                let jend = self.code.len();
                self.emit(Op::Jmp(0));
                let f2 = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf2] { *ofs = (f2 as isize - jf2 as isize) as i16; }
                self.emit(Op::LoadK(out, k_false));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] { *ofs = (end as isize - jend as isize) as i16; }
                // patch first false to jump to set false
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] { *ofs = (f2 as isize - jf as isize) as i16; }
                out
            }
            Expr::Or(l, r) => {
                let out = self.alloc();
                let rl = self.expr(l);
                // if !rl -> evaluate r, else set true
                let jf = self.code.len();
                self.emit(Op::JmpFalse(rl, 0));
                let k_true = self.k(Val::Bool(true));
                let k_false = self.k(Val::Bool(false));
                self.emit(Op::LoadK(out, k_true));
                let jend = self.code.len();
                self.emit(Op::Jmp(0));
                let fall = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] { *ofs = (fall as isize - jf as isize) as i16; }
                let rr = self.expr(r);
                let jf2 = self.code.len();
                self.emit(Op::JmpFalse(rr, 0));
                self.emit(Op::LoadK(out, k_true));
                let jend2 = self.code.len();
                self.emit(Op::Jmp(0));
                let f2 = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf2] { *ofs = (f2 as isize - jf2 as isize) as i16; }
                self.emit(Op::LoadK(out, k_false));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] { *ofs = (end as isize - jend as isize) as i16; }
                if let Op::Jmp(ref mut ofs) = self.code[jend2] { *ofs = (end as isize - jend2 as isize) as i16; }
                out
            }
            // legacy '@' context access removed
            Expr::Access(base, field) => {
                let b = self.expr(base);
                let f = self.expr(field);
                let out = self.alloc();
                self.emit(Op::Access(out, b, f));
                out
            }
            Expr::OptionalAccess(base, field) => {
                let b = self.expr(base);
                // if b == nil -> out=nil else out = b[field]
                let out = self.alloc();
                let k_nil = self.k(Val::Nil);
                let rnil = self.alloc();
                self.emit(Op::LoadK(rnil, k_nil));
                let beq = self.alloc();
                self.emit(Op::CmpEq(beq, b, rnil));
                let j_not_nil = self.code.len();
                self.emit(Op::JmpFalse(beq, 0));
                // b is nil -> out=nil
                self.emit(Op::LoadK(out, k_nil));
                let jend = self.code.len();
                self.emit(Op::Jmp(0));
                // not nil path
                let not_nil = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[j_not_nil] { *ofs = (not_nil as isize - j_not_nil as isize) as i16; }
                let f = self.expr(field);
                self.emit(Op::Access(out, b, f));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] { *ofs = (end as isize - jend as isize) as i16; }
                out
            }
            Expr::NullishCoalescing(l, r) => {
                let out = self.alloc();
                let rl = self.expr(l);
                let k_nil = self.k(Val::Nil);
                let rnil = self.alloc();
                self.emit(Op::LoadK(rnil, k_nil));
                let r_is_nil = self.alloc();
                self.emit(Op::CmpEq(r_is_nil, rl, rnil));
                let j_not_nil = self.code.len();
                // if not (rl == nil) jump to use-left
                self.emit(Op::JmpFalse(r_is_nil, 0));
                // left is nil -> evaluate right
                let rr = self.expr(r);
                self.emit(Op::Move(out, rr));
                let jend = self.code.len();
                self.emit(Op::Jmp(0));
                let use_left = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[j_not_nil] { *ofs = (use_left as isize - j_not_nil as isize) as i16; }
                self.emit(Op::Move(out, rl));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] { *ofs = (end as isize - jend as isize) as i16; }
                out
            }
            Expr::Bin(l, op, r) => {
                let a = self.expr(l);
                let b = self.expr(r);
                let dst = self.alloc();
                match op {
                    BinOp::Add => self.emit(Op::Add(dst, a, b)),
                    BinOp::Sub => self.emit(Op::Sub(dst, a, b)),
                    BinOp::Mul => self.emit(Op::Mul(dst, a, b)),
                    BinOp::Div => self.emit(Op::Div(dst, a, b)),
                    BinOp::Mod => self.emit(Op::Mod(dst, a, b)),
                    BinOp::Eq => self.emit(Op::CmpEq(dst, a, b)),
                    BinOp::Ne => self.emit(Op::CmpNe(dst, a, b)),
                    BinOp::Lt => self.emit(Op::CmpLt(dst, a, b)),
                    BinOp::Le => self.emit(Op::CmpLe(dst, a, b)),
                    BinOp::Gt => self.emit(Op::CmpGt(dst, a, b)),
                    BinOp::Ge => self.emit(Op::CmpGe(dst, a, b)),
                    _ => {
                        // Fallback: unsupported binary op -> Nil
                        let k = self.k(Val::Nil);
                        self.emit(Op::LoadK(dst, k));
                    }
                }
                dst
            }
            Expr::List(items) => {
                let dst = self.alloc();
                let base = self.alloc(); // base is first temp slot; we'll reuse
                // Ensure contiguous temps: evaluate each to a fresh reg; collect base index
                let mut first: Option<u16> = None;
                let mut count = 0u16;
                for it in items {
                    let ri = self.expr(it);
                    if first.is_none() { first = Some(ri); }
                    count += 1;
                }
                let b = first.unwrap_or_else(|| { let z=self.alloc(); let k=self.k(Val::Nil); self.emit(Op::LoadK(z,k)); z});
                self.emit(Op::BuildList { dst, base: b, len: count });
                dst
            }
            Expr::Map(pairs) => {
                let dst = self.alloc();
                // Evaluate as k0,v0,k1,v1... in contiguous regs
                let mut first: Option<u16> = None;
                let mut n: u16 = 0;
                for (k, v) in pairs {
                    let rk = self.expr(k);
                    let rv = self.expr(v);
                    if first.is_none() { first = Some(rk); }
                    let _ = rv; // ensure evaluation order
                    n += 1;
                }
                let b = first.unwrap_or_else(|| { let z=self.alloc(); let k=self.k(Val::Nil); self.emit(Op::LoadK(z,k)); z});
                self.emit(Op::BuildMap { dst, base: b, len: n });
                dst
            }
            Expr::Call(name, args) => {
                // Load function from global env, eval args, then call
                let f = self.alloc();
                let kname = self.k(Val::Str(name.clone().into()));
                self.emit(Op::LoadGlobal(f, kname));
                // Evaluate args into contiguous regs starting at base = next available
                let base = self.n_regs;
                for arg in args {
                    let _ = self.expr(arg);
                }
                let argc = args.len() as u8;
                self.emit(Op::Call { f, base, argc, retc: 1 });
                base
            }
            Expr::CallExpr(callee, args) => {
                let f = self.expr(callee);
                let base = self.n_regs;
                for arg in args { let _ = self.expr(arg); }
                let argc = args.len() as u8;
                self.emit(Op::Call { f, base, argc, retc: 1 });
                base
            }
            // Minimal fallback for uncompiled nodes
            _ => {
                let dst = self.alloc();
                let k = self.k(Val::Nil);
                self.emit(Op::LoadK(dst, k));
                dst
            }
        }
    }

    fn stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Block { statements } => {
                for st in statements { self.stmt(st); }
            }
            Stmt::Define { name, value } => {
                // Define as local first, then mirror to global env as well for compatibility
                let idx = self.get_or_define(name);
                let rv = self.expr(value);
                self.emit(Op::StoreLocal(idx, rv));
                let kname = self.k(Val::Str(name.clone().into()));
                self.emit(Op::DefineGlobal(kname, idx));
            }
            Stmt::Let { pattern: crate::expr::Pattern::Variable(name), type_annotation: _, value, span: _ } => {
                let idx = self.get_or_define(name);
                let rv = self.expr(value);
                self.emit(Op::StoreLocal(idx, rv));
            }
            Stmt::Assign { name, value, span: _ } => {
                if let Some(idx) = self.lookup(name) { let rv = self.expr(value); self.emit(Op::StoreLocal(idx, rv)); }
            }
            Stmt::Expr(e) => { let _ = self.expr(e); }
            Stmt::If { condition, then_stmt, else_stmt } => {
                let rc = self.expr(condition);
                let jf = self.code.len();
                self.emit(Op::JmpFalse(rc, 0));
                self.stmt(then_stmt);
                let jend_pos = self.code.len();
                let need_else = else_stmt.is_some();
                if need_else { self.emit(Op::Jmp(0)); }
                let else_label = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] { *ofs = (else_label as isize - jf as isize) as i16; }
                if let Some(es) = else_stmt { self.stmt(es); }
                if need_else { if let Op::Jmp(ref mut ofs) = self.code[jend_pos] { *ofs = (self.code.len() as isize - jend_pos as isize) as i16; } }
            }
            Stmt::While { condition, body } => {
                let start = self.code.len();
                let rc = self.expr(condition);
                let jf = self.code.len();
                self.emit(Op::JmpFalse(rc, 0));
                self.stmt(body);
                let back = start as isize - self.code.len() as isize;
                self.emit(Op::Jmp(back as i16));
                let end = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] { *ofs = (end as isize - jf as isize) as i16; }
            }
            Stmt::Return { value } => {
                let base = if let Some(v) = value { self.expr(v) } else { let k = self.k(Val::Nil); let r = self.alloc(); self.emit(Op::LoadK(r, k)); r };
                self.emit(Op::Ret { base, retc: 1 });
            }
            Stmt::Function { name, params, param_types: _, return_type: _, body } => {
                // Create closure proto and emit MakeClosure, then store to local and define global
                let proto_idx = self.protos.len() as u16;
                self.protos.push(ClosureProto { params: params.clone(), body: (**body).clone() });
                let dst = self.alloc();
                self.emit(Op::MakeClosure { dst, proto: proto_idx });
                let idx = self.get_or_define(name);
                self.emit(Op::StoreLocal(idx, dst));
                let kname = self.k(Val::Str(name.clone().into()));
                self.emit(Op::DefineGlobal(kname, idx));
            }
            // Skip unsupported statements for now
            _ => {}
        }
    }
}

fn name_of_define(s: &Stmt) -> &str {
    match s {
        Stmt::Define { name, .. } => name,
        Stmt::Let { pattern: crate::expr::Pattern::Variable(name), .. } => name,
        // Fallback: anonymous temp
        _ => "_",
    }
}
