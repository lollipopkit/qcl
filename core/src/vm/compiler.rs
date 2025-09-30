use std::collections::HashMap;

use crate::{expr::Expr, stmt::Stmt, val::Val};

use super::bytecode::{ClosureProto, Function, Op};

/// A simple single-function compiler that lowers a subset of Stmt/Expr to register bytecode.
/// - Locals are assigned sequential indices at first definition and never deallocated.
/// - Control flow supports if/while/return and expression statements.
pub struct Compiler;

impl Compiler {
    pub fn new() -> Self {
        Self
    }

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

    /// Compile a function body with given parameter names.
    /// Parameters are treated as locals with preassigned registers. Previously
    /// a prologue loaded values by name; now the VM seeds param registers
    /// directly using `Function::param_regs`.
    pub fn compile_function(&self, params: &[String], body: &Stmt) -> Function {
        let mut b = FunctionBuilder::new();
        // Prebind params to local registers and record their register indices
        b.param_regs.reserve(params.len());
        for p in params {
            let idx = b.get_or_define(p);
            b.param_regs.push(idx);
        }
        // Body: closures with expression bodies should return the expression's value.
        match body {
            Stmt::Expr(e) => {
                let r = b.expr(e);
                b.emit(Op::Ret { base: r, retc: 1 });
            }
            other => {
                b.stmt(other);
                // Default return Nil if no explicit return executed
                let k = b.k(Val::Nil);
                let r0 = b.alloc();
                b.emit(Op::LoadK(r0, k));
                b.emit(Op::Ret { base: r0, retc: 1 });
            }
        }
        b.finish()
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

struct FunctionBuilder {
    consts: Vec<Val>,
    code: Vec<Op>,
    n_regs: u16,
    vars: HashMap<String, u16>,
    protos: Vec<ClosureProto>,
    param_regs: Vec<u16>,
}

impl FunctionBuilder {
    fn new() -> Self {
        Self {
            consts: Vec::new(),
            code: Vec::new(),
            n_regs: 0,
            vars: HashMap::new(),
            protos: Vec::new(),
            param_regs: Vec::new(),
        }
    }
    fn finish(self) -> Function {
        Function {
            consts: self.consts,
            code: self.code,
            n_regs: self.n_regs,
            protos: self.protos,
            param_regs: self.param_regs,
        }
    }
    fn emit(&mut self, op: Op) {
        self.code.push(op);
    }
    fn alloc(&mut self) -> u16 {
        let r = self.n_regs;
        self.n_regs = self.n_regs.saturating_add(1);
        r
    }
    fn k(&mut self, v: Val) -> u16 {
        if let Some((i, _)) = self.consts.iter().enumerate().find(|(_, x)| *x == &v) {
            i as u16
        } else {
            self.consts.push(v);
            (self.consts.len() - 1) as u16
        }
    }
    fn get_or_define(&mut self, name: &str) -> u16 {
        if let Some(&i) = self.vars.get(name) {
            i
        } else {
            let idx = self.alloc();
            self.vars.insert(name.to_string(), idx);
            idx
        }
    }
    fn lookup(&self, name: &str) -> Option<u16> {
        self.vars.get(name).copied()
    }

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
                        if let Op::JmpFalse(_, ref mut ofs) = self.code[jslot] {
                            *ofs = (target as isize - jslot as isize) as i16;
                        }
                        self.emit(Op::LoadK(out, k_true));
                        // end label
                        let end = self.code.len();
                        if let Op::Jmp(ref mut ofs) = self.code[jend] {
                            *ofs = (end as isize - jend as isize) as i16;
                        }
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
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf2] {
                    *ofs = (f2 as isize - jf2 as isize) as i16;
                }
                self.emit(Op::LoadK(out, k_false));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] {
                    *ofs = (end as isize - jend as isize) as i16;
                }
                // patch first false to jump to set false
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] {
                    *ofs = (f2 as isize - jf as isize) as i16;
                }
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
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] {
                    *ofs = (fall as isize - jf as isize) as i16;
                }
                let rr = self.expr(r);
                let jf2 = self.code.len();
                self.emit(Op::JmpFalse(rr, 0));
                self.emit(Op::LoadK(out, k_true));
                let jend2 = self.code.len();
                self.emit(Op::Jmp(0));
                let f2 = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf2] {
                    *ofs = (f2 as isize - jf2 as isize) as i16;
                }
                self.emit(Op::LoadK(out, k_false));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] {
                    *ofs = (end as isize - jend as isize) as i16;
                }
                if let Op::Jmp(ref mut ofs) = self.code[jend2] {
                    *ofs = (end as isize - jend2 as isize) as i16;
                }
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
                if let Op::JmpFalse(_, ref mut ofs) = self.code[j_not_nil] {
                    *ofs = (not_nil as isize - j_not_nil as isize) as i16;
                }
                let f = self.expr(field);
                self.emit(Op::Access(out, b, f));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] {
                    *ofs = (end as isize - jend as isize) as i16;
                }
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
                if let Op::JmpFalse(_, ref mut ofs) = self.code[j_not_nil] {
                    *ofs = (use_left as isize - j_not_nil as isize) as i16;
                }
                self.emit(Op::Move(out, rl));
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] {
                    *ofs = (end as isize - jend as isize) as i16;
                }
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
                // Reserve contiguous slots for items, evaluate each, and pack into [base..)
                let dst = self.alloc();
                let count = items.len() as u16;
                let base = self.n_regs;
                for _ in 0..items.len() {
                    let _ = self.alloc();
                }
                for (i, it) in items.iter().enumerate() {
                    let ri = self.expr(it);
                    let d = base + i as u16;
                    if ri != d {
                        self.emit(Op::Move(d, ri));
                    }
                }
                self.emit(Op::BuildList { dst, base, len: count });
                dst
            }
            Expr::Map(pairs) => {
                // Reserve 2 slots per pair (k, v), pack into [base..)
                let dst = self.alloc();
                let n = pairs.len() as u16;
                let base = self.n_regs;
                for _ in 0..(pairs.len() * 2) {
                    let _ = self.alloc();
                }
                for (i, (k, v)) in pairs.iter().enumerate() {
                    let rk = self.expr(k);
                    let rv = self.expr(v);
                    let dk = base + (2 * i) as u16;
                    let dv = dk + 1;
                    if rk != dk {
                        self.emit(Op::Move(dk, rk));
                    }
                    if rv != dv {
                        self.emit(Op::Move(dv, rv));
                    }
                }
                self.emit(Op::BuildMap { dst, base, len: n });
                dst
            }
            Expr::Call(name, args) => {
                // Load function from global env, reserve arg slots, eval+pack args, then call
                let f = self.alloc();
                let kname = self.k(Val::Str(name.clone().into()));
                self.emit(Op::LoadGlobal(f, kname));
                let argc = args.len() as u8;
                let base = self.n_regs;
                // Reserve contiguous arg slots [base .. base+argc)
                for _ in 0..args.len() {
                    let _ = self.alloc();
                }
                // Evaluate each arg and pack into reserved slots
                for (i, arg) in args.iter().enumerate() {
                    let ri = self.expr(arg);
                    let dst = base + i as u16;
                    if ri != dst {
                        self.emit(Op::Move(dst, ri));
                    }
                }
                self.emit(Op::Call { f, base, argc, retc: 1 });
                base
            }
            Expr::CallExpr(callee, args) => {
                let f = self.expr(callee);
                let argc = args.len() as u8;
                let base = self.n_regs;
                // Reserve arg slots
                for _ in 0..args.len() {
                    let _ = self.alloc();
                }
                // Evaluate and pack
                for (i, arg) in args.iter().enumerate() {
                    let ri = self.expr(arg);
                    let dst = base + i as u16;
                    if ri != dst {
                        self.emit(Op::Move(dst, ri));
                    }
                }
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
                for st in statements {
                    self.stmt(st);
                }
            }
            Stmt::For {
                pattern,
                iterable,
                body,
            } => {
                // Fast-path compilation for numeric range for-loops:
                // for x in a..b { body }  or for _ in a..=b { body }
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                    step,
                } = iterable.as_ref()
                {
                    // Evaluate start and end into registers
                    let r_idx = match start {
                        Some(e) => self.expr(e),
                        None => {
                            let r = self.alloc();
                            let k0 = self.k(Val::Int(0));
                            self.emit(Op::LoadK(r, k0));
                            r
                        }
                    };
                    let r_lim = match end {
                        Some(e) => self.expr(e),
                        None => {
                            // Open-ended ranges are not supported; fall back to no-op
                            // (Interpreter path already errors; VM compiler keeps behavior consistent)
                            return;
                        }
                    };
                    // Step: explicit if provided, else allocate and compute at runtime
                    let r_step = if let Some(st_expr) = step {
                        self.expr(st_expr)
                    } else {
                        self.alloc()
                    };
                    // Prepare loop: decide step (+1 or -1) at runtime unless explicit
                    self.emit(Op::ForRangePrep {
                        idx: r_idx,
                        limit: r_lim,
                        step: r_step,
                        inclusive: *inclusive,
                        explicit: step.is_some(),
                    });

                    // Guard (patched to jump to end when done)
                    let guard_pos = self.code.len();
                    self.emit(Op::ForRangeGuard {
                        idx: r_idx,
                        limit: r_lim,
                        step: r_step,
                        inclusive: *inclusive,
                        ofs: 0, // patch to end
                    });

                    // Bind pattern for this loop iteration from the loop index value
                    match pattern {
                        crate::stmt::ForPattern::Variable(name) => {
                            let idx = self.get_or_define(name);
                            self.emit(Op::StoreLocal(idx, r_idx));
                        }
                        crate::stmt::ForPattern::Ignore => {}
                        other => {
                            // For numeric ranges, bind the index into a temporary Val and destructure if needed
                            // Here, since r_idx is already the value, reuse binder for complex patterns if any.
                            // Note: For patterns like tuples/objects, this likely doesn't apply; keep variable/ignore primary path.
                            // Future: support richer patterns for numeric loops if semantics allow.
                            let _ = other; // silence warning in this branch
                        }
                    }

                    // Body
                    self.stmt(body);

                    // Step and jump back to guard
                    let step_pos = self.code.len();
                    self.emit(Op::ForRangeStep {
                        idx: r_idx,
                        step: r_step,
                        back_ofs: 0, // patch to guard
                    });

                    // Patch back_ofs to guard
                    let back = (guard_pos as isize - step_pos as isize) as i16;
                    if let Op::ForRangeStep { back_ofs, .. } = &mut self.code[step_pos] {
                        *back_ofs = back;
                    }
                    // Patch guard to jump to end
                    let end = self.code.len();
                    if let Op::ForRangeGuard { ofs, .. } = &mut self.code[guard_pos] {
                        *ofs = (end as isize - guard_pos as isize) as i16;
                    }
                    return;
                }
                // General for-in lowering for list-like values:
                // Evaluate iterable, normalize to an iteration-friendly value (List/Str passthrough;
                // Map -> precomputed list of [key, value] pairs), then iterate i from 0..len(it)
                let r_src = self.expr(iterable);
                let r_it = self.alloc();
                self.emit(Op::ToIter { dst: r_it, src: r_src });
                // r_len = len(it)
                let r_len = self.alloc();
                self.emit(Op::Len { dst: r_len, src: r_it });
                // i = 0
                let r_i = self.alloc();
                let k0 = self.k(Val::Int(0));
                self.emit(Op::LoadK(r_i, k0));
                // guard: if !(i < len) jmp end
                let r_cmp = self.alloc();
                let guard_pos = self.code.len();
                self.emit(Op::CmpLt(r_cmp, r_i, r_len));
                let jf_pos = self.code.len();
                self.emit(Op::JmpFalse(r_cmp, 0));

                // item = it[i]
                let r_item = self.alloc();
                self.emit(Op::Index {
                    dst: r_item,
                    base: r_it,
                    idx: r_i,
                });

                // Bind pattern into locals from item
                self.bind_for_pattern(pattern, r_item);

                // Body
                self.stmt(body);

                // i = i + 1
                let k1 = self.k(Val::Int(1));
                let r_one = self.alloc();
                self.emit(Op::LoadK(r_one, k1));
                self.emit(Op::Add(r_i, r_i, r_one));
                // jump back to guard
                let back = (guard_pos as isize - self.code.len() as isize) as i16;
                self.emit(Op::Jmp(back));
                // patch false jump to here
                let end = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf_pos] {
                    *ofs = (end as isize - jf_pos as isize) as i16;
                }
            }
            Stmt::Define { name, value } => {
                // Define as local first, then mirror to global env as well for compatibility
                let idx = self.get_or_define(name);
                let rv = self.expr(value);
                self.emit(Op::StoreLocal(idx, rv));
                let kname = self.k(Val::Str(name.clone().into()));
                self.emit(Op::DefineGlobal(kname, idx));
            }
            Stmt::Let {
                pattern: crate::expr::Pattern::Variable(name),
                type_annotation: _,
                value,
                span: _,
            } => {
                let idx = self.get_or_define(name);
                let rv = self.expr(value);
                self.emit(Op::StoreLocal(idx, rv));
            }
            Stmt::Assign { name, value, span: _ } => {
                if let Some(idx) = self.lookup(name) {
                    let rv = self.expr(value);
                    self.emit(Op::StoreLocal(idx, rv));
                }
            }
            Stmt::Expr(e) => {
                let _ = self.expr(e);
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let rc = self.expr(condition);
                let jf = self.code.len();
                self.emit(Op::JmpFalse(rc, 0));
                self.stmt(then_stmt);
                let jend_pos = self.code.len();
                let need_else = else_stmt.is_some();
                if need_else {
                    self.emit(Op::Jmp(0));
                }
                let else_label = self.code.len();
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] {
                    *ofs = (else_label as isize - jf as isize) as i16;
                }
                if let Some(es) = else_stmt {
                    self.stmt(es);
                }
                if need_else {
                    let cur_len = self.code.len();
                    if let Op::Jmp(ref mut ofs) = self.code[jend_pos] {
                        *ofs = (cur_len as isize - jend_pos as isize) as i16;
                    }
                }
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
                if let Op::JmpFalse(_, ref mut ofs) = self.code[jf] {
                    *ofs = (end as isize - jf as isize) as i16;
                }
            }
            Stmt::Return { value } => {
                let base = if let Some(v) = value {
                    self.expr(v)
                } else {
                    let k = self.k(Val::Nil);
                    let r = self.alloc();
                    self.emit(Op::LoadK(r, k));
                    r
                };
                self.emit(Op::Ret { base, retc: 1 });
            }
            Stmt::Function {
                name,
                params,
                param_types: _,
                return_type: _,
                body,
            } => {
                // Create closure proto and emit MakeClosure, then store to local and define global
                let proto_idx = self.protos.len() as u16;
                self.protos.push(ClosureProto {
                    params: params.clone(),
                    body: (**body).clone(),
                });
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

impl FunctionBuilder {
    fn bind_for_pattern(&mut self, pattern: &crate::stmt::ForPattern, src: u16) {
        use crate::stmt::ForPattern;
        match pattern {
            ForPattern::Variable(name) => {
                let idx = self.get_or_define(name);
                self.emit(Op::StoreLocal(idx, src));
            }
            ForPattern::Ignore => {}
            ForPattern::Tuple(patterns) => {
                for (i, sub) in patterns.iter().enumerate() {
                    let r_idx = self.alloc();
                    let k = self.k(Val::Int(i as i64));
                    self.emit(Op::LoadK(r_idx, k));
                    let r_field = self.alloc();
                    self.emit(Op::Access(r_field, src, r_idx));
                    self.bind_for_pattern(sub, r_field);
                }
            }
            ForPattern::Array { patterns, rest } => {
                // Bind fixed prefix elements
                for (i, sub) in patterns.iter().enumerate() {
                    let r_idx = self.alloc();
                    let k = self.k(Val::Int(i as i64));
                    self.emit(Op::LoadK(r_idx, k));
                    let r_field = self.alloc();
                    self.emit(Op::Access(r_field, src, r_idx));
                    self.bind_for_pattern(sub, r_field);
                }
                // Bind rest tail if requested: rest = src[prefix_len..]
                if let Some(name) = rest {
                    let start = patterns.len() as i64;
                    let r_start = self.alloc();
                    let k = self.k(Val::Int(start));
                    self.emit(Op::LoadK(r_start, k));
                    let r_tail = self.alloc();
                    self.emit(Op::ListSlice {
                        dst: r_tail,
                        src,
                        start: r_start,
                    });
                    let idx = self.get_or_define(name);
                    self.emit(Op::StoreLocal(idx, r_tail));
                }
            }
            ForPattern::Object(entries) => {
                for (key, sub) in entries.iter() {
                    let r_key = self.alloc();
                    let k = self.k(Val::Str(key.clone().into()));
                    self.emit(Op::LoadK(r_key, k));
                    let r_field = self.alloc();
                    self.emit(Op::Access(r_field, src, r_key));
                    self.bind_for_pattern(sub, r_field);
                }
            }
        }
    }
}
