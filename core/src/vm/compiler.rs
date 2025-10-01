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
        let mut f = Function {
            consts: self.consts,
            code: self.code,
            n_regs: self.n_regs,
            protos: self.protos,
            param_regs: self.param_regs,
            #[cfg(feature = "bc32")]
            code32: None,
        };
        #[cfg(feature = "bc32")]
        {
            if let Some(packed) = crate::vm::Bc32Function::try_from_function(&f) {
                f.code32 = Some(packed.code32);
            }
        }
        f
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

    // Constant folding helpers
    fn try_fold_unary(&mut self, uop: &crate::op::UnaryOp, inner: &crate::expr::Expr) -> Option<Val> {
        if let crate::expr::Expr::Val(v) = inner {
            match uop {
                crate::op::UnaryOp::Not => {
                    if let Val::Bool(b) = v {
                        return Some(Val::Bool(!b));
                    }
                }
            }
        }
        None
    }

    fn try_fold_bin(&mut self, op: &crate::op::BinOp, l: &crate::expr::Expr, r: &crate::expr::Expr) -> Option<Val> {
        use crate::expr::Expr;
        match (l, r) {
            (Expr::Val(lv), Expr::Val(rv)) => {
                let res = if op.is_arith() {
                    op.eval_vals(lv, rv)
                } else if op.is_cmp() {
                    op.cmp(lv, rv).map(Val::Bool)
                } else {
                    return None;
                };
                res.ok()
            }
            _ => None,
        }
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
                if let Some(v) = self.try_fold_unary(uop, inner) {
                    let dst = self.alloc();
                    let k = self.k(v);
                    self.emit(Op::LoadK(dst, k));
                    return dst;
                }
                let r = self.expr(inner);
                match uop {
                    crate::op::UnaryOp::Not => {
                        let out = self.alloc();
                        self.emit(Op::Not(out, r));
                        out
                    }
                }
            }
            Expr::And(l, r) => {
                // Short-circuiting AND producing a boolean result:
                // rl = l; if !rl { out=false; jmp end } ; rr = r; out = bool(rr)
                let out = self.alloc();
                let rl = self.expr(l);
                let jpos = self.code.len();
                self.emit(Op::JmpFalseSet { r: rl, dst: out, ofs: 0 });
                let rr = self.expr(r);
                self.emit(Op::ToBool(out, rr));
                let end = self.code.len();
                if let Op::JmpFalseSet { ofs, .. } = &mut self.code[jpos] {
                    *ofs = (end as isize - jpos as isize) as i16;
                }
                out
            }
            Expr::Or(l, r) => {
                // Short-circuiting OR producing a boolean result:
                // rl = l; if rl { out=true; jmp end } ; rr = r; out = bool(rr)
                let out = self.alloc();
                let rl = self.expr(l);
                let jpos = self.code.len();
                self.emit(Op::JmpTrueSet { r: rl, dst: out, ofs: 0 });
                let rr = self.expr(r);
                self.emit(Op::ToBool(out, rr));
                let end = self.code.len();
                if let Op::JmpTrueSet { ofs, .. } = &mut self.code[jpos] {
                    *ofs = (end as isize - jpos as isize) as i16;
                }
                out
            }
            // legacy '@' context access removed
            Expr::Access(base, field) => {
                // If both sides are constant, fold at compile time
                if let (crate::expr::Expr::Val(vb), crate::expr::Expr::Val(vf)) = (base.as_ref(), field.as_ref()) {
                    let folded = vb.access(vf).unwrap_or(Val::Nil);
                    let dst = self.alloc();
                    let k = self.k(folded);
                    self.emit(Op::LoadK(dst, k));
                    return dst;
                }
                let b = self.expr(base);
                let out = self.alloc();
                // If field is a constant string or int, use specialized opcodes
                if let Expr::Val(Val::Str(s)) = field.as_ref() {
                    let k = self.k(Val::Str(s.clone()));
                    self.emit(Op::AccessK(out, b, k));
                } else if let Expr::Val(Val::Int(i)) = field.as_ref() {
                    let k = self.k(Val::Int(*i));
                    self.emit(Op::IndexK(out, b, k));
                } else {
                    let f = self.expr(field);
                    self.emit(Op::Access(out, b, f));
                }
                out
            }
            Expr::OptionalAccess(base, field) => {
                let b = self.expr(base);
                // if b is nil -> out=nil else out = b[field]
                let out = self.alloc();
                let j_is_nil = self.code.len();
                self.emit(Op::JmpIfNil(b, 0));
                // not nil path
                if let Expr::Val(Val::Int(i)) = field.as_ref() {
                    let k = self.k(Val::Int(*i));
                    self.emit(Op::IndexK(out, b, k));
                } else if let Expr::Val(Val::Str(s)) = field.as_ref() {
                    let k = self.k(Val::Str(s.clone()));
                    self.emit(Op::AccessK(out, b, k));
                } else {
                    let f = self.expr(field);
                    self.emit(Op::Access(out, b, f));
                }
                let jend = self.code.len();
                self.emit(Op::Jmp(0));
                // nil path sets out=nil
                let nil_path = self.code.len();
                if let Op::JmpIfNil(_, ref mut ofs) = self.code[j_is_nil] {
                    *ofs = (nil_path as isize - j_is_nil as isize) as i16;
                }
                let k_nil = self.k(Val::Nil);
                self.emit(Op::LoadK(out, k_nil));
                // end
                let end = self.code.len();
                if let Op::Jmp(ref mut ofs) = self.code[jend] {
                    *ofs = (end as isize - jend as isize) as i16;
                }
                out
            }
            Expr::NullishCoalescing(l, r) => {
                // Compile-time reduction when left is a constant
                if let crate::expr::Expr::Val(vl) = l.as_ref() {
                    if *vl == Val::Nil {
                        return self.expr(r);
                    } else {
                        let dst = self.alloc();
                        let k = self.k(vl.clone());
                        self.emit(Op::LoadK(dst, k));
                        return dst;
                    }
                }
                // Fused: if rl != nil { out = rl; jmp end } ; rr = expr(r); out = rr
                let out = self.alloc();
                let rl = self.expr(l);
                let pick_pos = self.code.len();
                self.emit(Op::NullishPick { l: rl, dst: out, ofs: 0 });
                let rr = self.expr(r);
                self.emit(Op::Move(out, rr));
                let end = self.code.len();
                if let Op::NullishPick { ofs, .. } = &mut self.code[pick_pos] {
                    *ofs = (end as isize - pick_pos as isize) as i16;
                }
                out
            }
            Expr::Bin(l, op, r) => {
                if let Some(v) = self.try_fold_bin(op, l, r) {
                    let dst = self.alloc();
                    let k = self.k(v);
                    self.emit(Op::LoadK(dst, k));
                    return dst;
                }
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
                    BinOp::In => self.emit(Op::In(dst, a, b)),
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
