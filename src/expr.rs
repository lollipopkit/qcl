use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use core::fmt::{Debug, Display};

#[cfg(feature = "std")]
use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::{
        RwLock,
        atomic::{AtomicU64, Ordering},
    },
};

use crate::{
    ast::Parser,
    error::{Error, Result},
    op::{BinOp, UnaryOp, err_op},
    token::Tokenizer,
    val::Val,
};
use hashbrown::HashMap;
use hashbrown::HashSet as HbHashSet;
#[cfg(feature = "std")]
use once_cell::sync::Lazy;

#[cfg(feature = "std")]
const MAX_PARSE_CACHE_ENTRIES: usize = 4096;
#[cfg(feature = "std")]
const MAX_CACHED_EXPR_LEN: usize = 4096;
#[cfg(feature = "std")]
const PARSE_CACHE_SHARDS: usize = 16;

/// Grammar:
/// exp      ::= ternary
/// ternary  ::= coalesce {'?' expr ':' expr}
/// coalesce ::= or {'??' or}
/// or       ::= and {'||' and}
/// and      ::= cmp {'&&' cmp}
/// cmp      ::= addsub {('<' | '>' | '<=' | '>=' | '!=' | '==') addsub}
/// addsub   ::= muldiv {('+' | '-') muldiv}
/// muldiv   ::= unary {('*' | '/' | '%') unary}
/// unary    ::= {'!' | '-'} postfix
/// postfix  ::= primary {'.' field}
/// primary  ::= nil | false | true | int | float | string | at | list | map
/// at       ::= '@' field {'.' field}
/// field    ::= id | int
/// list     ::= '[' [expr {',' expr}] ']'
/// map      ::= '{' [expr ':' expr {',' expr ':' expr}] '}'
///
///
/// Details:
/// - @expr
///   + All accessible objects of `@` expr are maps actually.
///   + You can use `.` to access the fields of the map.
///   + @req: Request object, `@req.user` is the user object.
///   + @record: Record object, `@record` is the record object.
///   + All valid objects are defined in the [Context]
/// - int / float are considered as `i64 / f64`.
/// - bool can be `true` or `false`.
/// - String
///   + can be wrapped with `""` or `''`.
///   + max length is 64.
/// - nil
///   + ONLY [Option::None] and [Result::Err] are `nil`.
///   + zero value of all types are NOT `nil`.
/// - list literals: `[1, 2, "hello"]`
/// - map literals: `{"key": "value", "count": 42}`
/// - Access literals: `[1, 2, 3].1`, `{"name": "Alice"}.name`
///
/// Examples:
/// - `@req.user.age >= 18`
/// - `@req.user.name == "Alice" && @record.status == "active"`
/// - `[1, 2, 3]`
/// - `{"name": "John", "age": 30}`
/// - `[1, 2, 3].1`
/// - `{"name": "Alice"}.name`
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// expr == expr
    Bin(Box<Expr>, BinOp, Box<Expr>),
    /// !expr
    Unary(UnaryOp, Box<Expr>),
    /// expr && expr
    And(Box<Expr>, Box<Expr>),
    /// expr || expr
    Or(Box<Expr>, Box<Expr>),
    /// expr ?? expr (returns left if non-nil, else right)
    Coalesce(Box<Expr>, Box<Expr>),
    /// expr ? expr : expr (ternary conditional)
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    /// @field.field...
    /// field can be string or int
    At(Vec<Box<Expr>>),
    /// expr.field
    Access(Box<Expr>, Box<Expr>),
    // (expr)
    Paren(Box<Expr>),
    /// [expr, expr, ...]
    List(Vec<Box<Expr>>),
    /// {expr: expr, expr: expr, ...}
    Map(Vec<(Box<Expr>, Box<Expr>)>),
    Val(Val),
}

#[cfg(feature = "std")]
struct ParseCacheShard {
    state: RwLock<ParseCacheState>,
}

#[cfg(feature = "std")]
struct CacheEntry {
    expr: Arc<Expr>,
    last_used: AtomicU64,
}

#[cfg(feature = "std")]
struct ParseCacheState {
    entries: HashMap<String, CacheEntry>,
}

#[cfg(feature = "std")]
static CACHE_CLOCK: AtomicU64 = AtomicU64::new(0);

#[cfg(feature = "std")]
impl ParseCacheState {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Look up under a shared (read) lock. The LRU timestamp lives in an atomic
    /// so a cache hit — the hot path for repeated ACL checks — only needs a
    /// shared lock and never serializes readers within a shard.
    fn get(&self, expression: &str) -> Option<Arc<Expr>> {
        let entry = self.entries.get(expression)?;
        // fetch_max keeps last_used monotonic: two readers racing under the
        // shared lock cannot let an earlier ticket overwrite a later one's.
        entry
            .last_used
            .fetch_max(CACHE_CLOCK.fetch_add(1, Ordering::Relaxed), Ordering::Relaxed);
        Some(entry.expr.clone())
    }

    fn insert(&mut self, expression: String, expr: Arc<Expr>) {
        if self.entries.contains_key(expression.as_str()) {
            return;
        }

        if self.entries.len() >= parse_cache_entries_per_shard() {
            self.evict_oldest();
        }

        let ts = CACHE_CLOCK.fetch_add(1, Ordering::Relaxed);
        self.entries.insert(
            expression,
            CacheEntry {
                expr,
                last_used: AtomicU64::new(ts),
            },
        );
    }

    fn evict_oldest(&mut self) {
        if let Some(oldest_key) = self
            .entries
            .iter()
            .min_by_key(|(_, entry)| entry.last_used.load(Ordering::Relaxed))
            .map(|(k, _)| k.clone())
        {
            self.entries.remove(&oldest_key);
        }
    }
}

#[cfg(feature = "std")]
static PARSE_CACHE: Lazy<Box<[ParseCacheShard]>> = Lazy::new(|| {
    (0..PARSE_CACHE_SHARDS)
        .map(|_| ParseCacheShard {
            state: RwLock::new(ParseCacheState::new()),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
});

#[cfg(feature = "std")]
#[inline]
const fn parse_cache_entries_per_shard() -> usize {
    MAX_PARSE_CACHE_ENTRIES.div_ceil(PARSE_CACHE_SHARDS)
}

#[cfg(feature = "std")]
#[inline]
fn parse_cache_shard_idx(expression: &str) -> usize {
    let mut hasher = DefaultHasher::new();
    expression.hash(&mut hasher);
    (hasher.finish() as usize) % PARSE_CACHE_SHARDS
}

#[cfg(feature = "std")]
#[inline]
fn parse_cache_shard(expression: &str) -> &'static ParseCacheShard {
    &PARSE_CACHE[parse_cache_shard_idx(expression)]
}

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(l, op, r) => op.eval(l, r, ctx),
            Expr::Unary(op, expr) => op.eval(expr, ctx),
            Expr::And(e1, e2) => match e1.eval(ctx)? {
                Val::Bool(false) => Ok(Val::Bool(false)),
                Val::Bool(true) => match e2.eval(ctx)? {
                    Val::Bool(b) => Ok(Val::Bool(b)),
                    r => err_op(&Val::Bool(true), "&&", &r),
                },
                l => {
                    let r = e2.eval(ctx)?;
                    err_op(&l, "&&", &r)
                }
            },
            Expr::Or(e1, e2) => match e1.eval(ctx)? {
                Val::Bool(true) => Ok(Val::Bool(true)),
                Val::Bool(false) => match e2.eval(ctx)? {
                    Val::Bool(b) => Ok(Val::Bool(b)),
                    r => err_op(&Val::Bool(false), "||", &r),
                },
                l => {
                    let r = e2.eval(ctx)?;
                    err_op(&l, "||", &r)
                }
            },
            Expr::Coalesce(l, r) => {
                let left = l.eval(ctx)?;
                if matches!(left, Val::Nil) {
                    r.eval(ctx)
                } else {
                    Ok(left)
                }
            }
            Expr::Ternary(cond, t, f) => {
                let cond_val = cond.eval(ctx)?;
                match cond_val {
                    Val::Bool(true) => t.eval(ctx),
                    Val::Bool(false) => f.eval(ctx),
                    _ => Err(Error::Eval(format!("Ternary condition must be bool, got: {cond_val}"))),
                }
            }
            Expr::At(paths) => {
                if paths.is_empty() {
                    return Ok(Val::Nil);
                }

                let mut val = ctx;
                for path in paths {
                    let tmp;
                    let key = match &**path {
                        Expr::Val(v) => v,
                        _ => {
                            tmp = path.eval(ctx)?;
                            &tmp
                        }
                    };

                    val = match val.access(key) {
                        Some(v) => v,
                        None => return Ok(Val::Nil),
                    };
                }
                // Return a clone only at the end of evaluation to reduce allocations
                Ok(val.clone())
            }
            Expr::Access(expr, field) => {
                let val = expr.eval(ctx)?;
                let tmp;
                let field_val = match &**field {
                    Expr::Val(v) => v,
                    _ => {
                        tmp = field.eval(ctx)?;
                        &tmp
                    }
                };

                match val.access(field_val) {
                    Some(v) => Ok(v.clone()),
                    None => Ok(Val::Nil),
                }
            }
            Expr::List(exprs) => {
                let mut values = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values.push(match &**expr {
                        Expr::Val(v) => v.clone(),
                        _ => expr.eval(ctx)?,
                    });
                }
                Ok(Val::List(Arc::new(values)))
            }
            Expr::Map(pairs) => {
                let mut map = hashbrown::HashMap::with_capacity(pairs.len());
                for (key_expr, value_expr) in pairs {
                    let key_tmp;
                    let key_val = match &**key_expr {
                        Expr::Val(v) => v,
                        _ => {
                            key_tmp = key_expr.eval(ctx)?;
                            &key_tmp
                        }
                    };
                    let value_val = match &**value_expr {
                        Expr::Val(v) => v.clone(),
                        _ => value_expr.eval(ctx)?,
                    };

                    let Some(key_str) = primitive_map_key_to_string(key_val) else {
                        return Err(Error::Eval(format!(
                            "Map key must be a primitive type, got: {:?}",
                            key_val
                        )));
                    };

                    map.insert(key_str, value_val);
                }
                Ok(Val::Map(Arc::new(map)))
            }
            Expr::Paren(expr) => expr.eval(ctx),
            Expr::Val(val) => Ok(val.clone()), // Clone necessary as eval returns owned Val
        }
    }

    /// Get the requested context names from the expression.
    pub fn requested_ctx(&self) -> alloc::collections::BTreeSet<String> {
        let mut names = HbHashSet::new();
        self.collect_ctx_names(&mut names);
        names.into_iter().collect()
    }

    /// Returns true when the expression can be evaluated without consulting `ctx`.
    pub fn is_ctx_independent(&self) -> bool {
        match self {
            Expr::At(_) => false,
            Expr::Bin(l, _, r) | Expr::And(l, r) | Expr::Or(l, r) | Expr::Access(l, r) | Expr::Coalesce(l, r) => {
                l.is_ctx_independent() && r.is_ctx_independent()
            }
            Expr::Unary(_, expr) | Expr::Paren(expr) => expr.is_ctx_independent(),
            Expr::Ternary(cond, t, f) => cond.is_ctx_independent() && t.is_ctx_independent() && f.is_ctx_independent(),
            Expr::List(exprs) => exprs.iter().all(|expr| expr.is_ctx_independent()),
            Expr::Map(pairs) => pairs
                .iter()
                .all(|(key, value)| key.is_ctx_independent() && value.is_ctx_independent()),
            Expr::Val(_) => true,
        }
    }

    /// Helper method to collect context names recursively
    ///
    /// eg.: `@user.props.(@req.service).value && @list` => `["user", "req", "list"]`
    fn collect_ctx_names(&self, names: &mut HbHashSet<String>) {
        match self {
            Expr::At(paths) => {
                if !paths.is_empty() {
                    // The first path element is the context name
                    if let Expr::Val(Val::Str(name)) = &*paths[0] {
                        names.insert(name.as_ref().to_string());
                    } else {
                        // If the first element is a complex expression, process it
                        paths[0].collect_ctx_names(names);
                    }

                    // For other path elements, only process them if they might contain contexts
                    for path in &paths[1..] {
                        match &**path {
                            // Skip [Val]s that are just field names
                            Expr::Val(_) => {}
                            // Process other values normally
                            _ => path.collect_ctx_names(names),
                        }
                    }
                }
            }
            Expr::Access(expr, field) => {
                expr.collect_ctx_names(names);
                field.collect_ctx_names(names);
            }
            Expr::Bin(l, _, r) => {
                l.collect_ctx_names(names);
                r.collect_ctx_names(names);
            }
            Expr::Unary(_, expr) => {
                expr.collect_ctx_names(names);
            }
            Expr::And(l, r) | Expr::Or(l, r) | Expr::Coalesce(l, r) => {
                l.collect_ctx_names(names);
                r.collect_ctx_names(names);
            }
            Expr::Ternary(cond, t, f) => {
                cond.collect_ctx_names(names);
                t.collect_ctx_names(names);
                f.collect_ctx_names(names);
            }
            Expr::List(exprs) => {
                for expr in exprs {
                    expr.collect_ctx_names(names);
                }
            }
            Expr::Map(pairs) => {
                for (key, value) in pairs {
                    key.collect_ctx_names(names);
                    value.collect_ctx_names(names);
                }
            }
            Expr::Paren(expr) => {
                expr.collect_ctx_names(names);
            }
            // Only collect string values when they are actual context names, not field names
            Expr::Val(_) => {}
        }
    }

    /// Cached parsing: parse expression string to Expr with caching to avoid repeated parsing overhead
    #[cfg(feature = "std")]
    pub fn parse_cached(expression: &str) -> Result<Expr> {
        Ok((*Self::parse_cached_arc(expression)?).clone())
    }

    /// Cached parsing variant that avoids cloning the parsed AST on cache hits.
    #[cfg(feature = "std")]
    pub fn parse_cached_arc(expression: &str) -> Result<Arc<Expr>> {
        if expression.len() <= MAX_CACHED_EXPR_LEN {
            let shard = parse_cache_shard(expression);
            if let Some(cached) = shard.state.read().unwrap().get(expression) {
                return Ok(cached);
            }
        }

        let tokens = Tokenizer::new(expression)?;
        let expr = Parser::new(&tokens).parse()?; // Internal constant folding happens in parser
        let expr = Arc::new(expr);

        if expression.len() <= MAX_CACHED_EXPR_LEN {
            let shard = parse_cache_shard(expression);
            let mut state = shard.state.write().unwrap();

            if let Some(cached) = state.get(expression) {
                return Ok(cached);
            }

            state.insert(expression.to_string(), expr.clone());
        }

        Ok(expr)
    }

    /// Constant folding: calculate pure constant sub-expressions as Val constants
    pub(crate) fn fold_constants(self) -> Expr {
        match self {
            Expr::Val(_) => self, // Constant value, return directly
            Expr::Bin(l_box, op, r_box) => {
                // Recursively fold left and right sub-expressions
                let left = (*l_box).fold_constants();
                let right = (*r_box).fold_constants();
                // Try to calculate binary expression as constant
                if let (Expr::Val(lval), Expr::Val(rval)) = (&left, &right) {
                    if op.is_arith() {
                        // Arithmetic operation constant folding
                        let result = match op {
                            BinOp::Add => (lval as &Val) + (rval as &Val),
                            BinOp::Sub => (lval as &Val) - (rval as &Val),
                            BinOp::Mul => (lval as &Val) * (rval as &Val),
                            BinOp::Div => (lval as &Val) / (rval as &Val),
                            BinOp::Mod => (lval as &Val) % (rval as &Val),
                            _ => unreachable!(),
                        };
                        if let Ok(result_val) = result {
                            return Expr::Val(result_val);
                        }
                    } else if op.is_cmp() {
                        // Comparison/contains operation constant folding
                        if let Ok(res_bool) = op.cmp(lval, rval) {
                            return Expr::Val(Val::Bool(res_bool));
                        }
                    }
                    // Other cases (like type mismatch) don't fold, keep expression form
                }
                // Partial folding: left and right nodes already folded, but current node can't fold to constant
                Expr::Bin(Box::new(left), op, Box::new(right))
            }
            Expr::Unary(op, expr_box) => {
                let inner = (*expr_box).fold_constants();
                match (&op, &inner) {
                    (UnaryOp::Not, Expr::Val(Val::Bool(b))) => {
                        return Expr::Val(Val::Bool(!*b));
                    }
                    (UnaryOp::Neg, Expr::Val(Val::Int(i))) => {
                        if let Some(neg) = i.checked_neg() {
                            return Expr::Val(Val::Int(neg));
                        }
                    }
                    (UnaryOp::Neg, Expr::Val(Val::Float(f))) => {
                        return Expr::Val(Val::Float(-*f));
                    }
                    _ => {}
                }
                Expr::Unary(op, Box::new(inner))
            }
            Expr::And(e1_box, e2_box) => {
                let e1 = (*e1_box).fold_constants();
                // Short-circuit constant false: left side constant false, then entire AND is constant false
                if let Expr::Val(Val::Bool(false)) = e1 {
                    return Expr::Val(Val::Bool(false));
                }
                let e2 = (*e2_box).fold_constants();
                // Short-circuit constant true: left side constant true, then return right side expression result
                if let Expr::Val(Val::Bool(true)) = e1 {
                    return e2;
                }
                // Both folded, if both are boolean constants then can further fold
                if let (Expr::Val(Val::Bool(b1)), Expr::Val(Val::Bool(b2))) = (&e1, &e2) {
                    return Expr::Val(Val::Bool(*b1 && *b2));
                }
                Expr::And(Box::new(e1), Box::new(e2))
            }
            Expr::Or(e1_box, e2_box) => {
                let e1 = (*e1_box).fold_constants();
                if let Expr::Val(Val::Bool(true)) = e1 {
                    // Left side constant true, OR expression is constant true
                    return Expr::Val(Val::Bool(true));
                }
                let e2 = (*e2_box).fold_constants();
                if let Expr::Val(Val::Bool(false)) = e1 {
                    // Left side constant false, OR result depends on right side
                    return e2;
                }
                if let (Expr::Val(Val::Bool(b1)), Expr::Val(Val::Bool(b2))) = (&e1, &e2) {
                    return Expr::Val(Val::Bool(*b1 || *b2));
                }
                Expr::Or(Box::new(e1), Box::new(e2))
            }
            Expr::At(paths) => {
                // @path expressions depend on context, don't fold
                let folded_paths = paths.into_iter().map(|p| Box::new(p.fold_constants())).collect();
                Expr::At(folded_paths)
            }
            Expr::Access(base_box, field_box) => {
                let base = (*base_box).fold_constants();
                let field = (*field_box).fold_constants();
                if let (Expr::Val(base_val), Expr::Val(field_val)) = (&base, &field) {
                    // Direct access to constant structure, e.g. [1,2,3].1 or {"k":10}.k
                    if let Some(res_val) = base_val.access(field_val) {
                        return Expr::Val(res_val.clone());
                    } else {
                        return Expr::Val(Val::Nil);
                    }
                }
                Expr::Access(Box::new(base), Box::new(field))
            }
            Expr::List(exprs) => {
                // Fold once and keep the constant payloads as we go to avoid a second pass.
                let mut folded_elems = Vec::with_capacity(exprs.len());
                let mut const_vals = Vec::with_capacity(exprs.len());
                let mut all_const = true;

                for expr in exprs {
                    let folded = expr.fold_constants();
                    if let Expr::Val(v) = &folded {
                        if all_const {
                            const_vals.push(v.clone());
                        }
                    } else {
                        all_const = false;
                    }
                    folded_elems.push(folded);
                }

                if all_const {
                    return Expr::Val(Val::List(Arc::new(const_vals)));
                }
                Expr::List(folded_elems.into_iter().map(Box::new).collect())
            }
            Expr::Map(pairs) => {
                // Fold once and build the constant map incrementally when possible.
                let mut folded_pairs = Vec::with_capacity(pairs.len());
                let mut const_map = HashMap::with_capacity(pairs.len());
                let mut all_const = true;

                for (k, v) in pairs {
                    let key = k.fold_constants();
                    let value = v.fold_constants();

                    if let (Expr::Val(k_val), Expr::Val(v_val)) = (&key, &value) {
                        if all_const {
                            if let Some(key_str) = primitive_map_key_to_string(k_val) {
                                const_map.insert(key_str, v_val.clone());
                            } else {
                                all_const = false;
                            }
                        }
                    } else {
                        all_const = false;
                    }

                    folded_pairs.push((Box::new(key), Box::new(value)));
                }

                if all_const {
                    return Expr::Val(Val::Map(Arc::new(const_map)));
                }

                Expr::Map(folded_pairs)
            }
            Expr::Paren(expr_box) => {
                // Keep parentheses structure, but fold internal expression
                Expr::Paren(Box::new((*expr_box).fold_constants()))
            }
            Expr::Coalesce(l_box, r_box) => {
                let left = (*l_box).fold_constants();
                match &left {
                    Expr::Val(Val::Nil) => return (*r_box).fold_constants(),
                    Expr::Val(_) => return left,
                    _ => {}
                }
                let right = (*r_box).fold_constants();
                Expr::Coalesce(Box::new(left), Box::new(right))
            }
            Expr::Ternary(cond_box, t_box, f_box) => {
                let cond = (*cond_box).fold_constants();
                match &cond {
                    Expr::Val(Val::Bool(true)) => return (*t_box).fold_constants(),
                    Expr::Val(Val::Bool(false)) => return (*f_box).fold_constants(),
                    _ => {}
                }
                let t = (*t_box).fold_constants();
                let f = (*f_box).fold_constants();
                Expr::Ternary(Box::new(cond), Box::new(t), Box::new(f))
            }
        }
    }
}

impl TryInto<Val> for &Expr {
    type Error = crate::error::Error;

    fn try_into(self) -> Result<Val> {
        match self {
            Expr::Val(val) => Ok(val.clone()), // Clone necessary as eval returns owned Val
            _ => {
                let msg = format!("Can't convert Expr::{:?} to Val", self);
                Err(crate::error::Error::Eval(msg))
            }
        }
    }
}

fn primitive_map_key_to_string(value: &Val) -> Option<String> {
    match value {
        Val::Str(s) => Some(s.as_ref().to_string()),
        Val::Int(i) => Some(i.to_string()),
        Val::Float(f) => Some(f.to_string()),
        Val::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

fn into_expr<S: AsRef<str>>(s: S) -> Result<Expr> {
    let tokens = Tokenizer::new(s.as_ref())?;
    let expr = Parser::new(&tokens).parse()?;
    Ok(expr)
}

impl TryFrom<&str> for Expr {
    type Error = crate::error::Error;

    fn try_from(value: &str) -> core::result::Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl TryFrom<String> for Expr {
    type Error = crate::error::Error;

    fn try_from(value: String) -> core::result::Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Expr::Bin(left, op, right) => write!(f, "{left} {op:?} {right}"),
            Expr::Unary(op, expr) => write!(f, "{op:?}{expr}"),
            Expr::And(left, right) => write!(f, "{left} && {right}"),
            Expr::Or(left, right) => write!(f, "{left} || {right}"),
            Expr::Coalesce(left, right) => write!(f, "{left} ?? {right}"),
            Expr::Ternary(cond, t, fa) => write!(f, "{cond} ? {t} : {fa}"),
            Expr::At(paths) => {
                let paths: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                write!(f, "@{}", paths.join("."))
            }
            Expr::Access(expr, field) => write!(f, "{}.{}", expr, field),
            Expr::List(exprs) => {
                let exprs: Vec<String> = exprs.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", exprs.join(", "))
            }
            Expr::Map(pairs) => {
                let pairs: Vec<String> = pairs.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            }
            Expr::Paren(expr) => write!(f, "{expr}"),
            Expr::Val(val) => write!(f, "{}", val),
        }
    }
}

impl From<Val> for Expr {
    fn from(val: Val) -> Self {
        Expr::Val(val)
    }
}

#[cfg(test)]
pub(crate) fn reset_parse_cache_for_tests() {
    for shard in PARSE_CACHE.iter() {
        let mut state = shard.state.write().unwrap();
        state.entries.clear();
    }
}

#[cfg(test)]
pub(crate) fn parse_cache_shard_idx_for_tests(expression: &str) -> usize {
    parse_cache_shard_idx(expression)
}

#[cfg(test)]
pub(crate) fn parse_cache_entry_count_for_tests() -> usize {
    PARSE_CACHE
        .iter()
        .map(|shard| shard.state.read().unwrap().entries.len())
        .sum()
}

#[cfg(test)]
pub(crate) const fn parse_cache_entries_per_shard_for_tests() -> usize {
    parse_cache_entries_per_shard()
}

#[cfg(test)]
pub(crate) const fn parse_cache_shard_count_for_tests() -> usize {
    PARSE_CACHE_SHARDS
}
