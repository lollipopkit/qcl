use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    sync::Arc,
};

use anyhow::{Result, anyhow};

use crate::{
    ast::Parser,
    op::{BinOp, UnaryOp, err_op},
    token::Tokenizer,
    typ::TypeChecker,
    val::{Type, Val},
};
use once_cell::sync::Lazy;

/// Grammar (abridged):
/// exp     ::= paren
/// paren   ::= {'('} or {')'}
/// or      ::= and {'||' and}
/// and     ::= cmp {'&&' cmp}
/// cmp     ::= addsub {('<' | '>' | '<=' | '>=' | '!=' | '==') addsub}
/// addsub  ::= muldiv {('+' | '-') muldiv}
/// muldiv  ::= unary {('*' | '/' | '%') unary}
/// unary   ::= {'!'} postfix
/// postfix ::= primary { call | dot | opt_dot | opt_index | index }
/// primary ::= nil | false | true | int | float | string | template | list | map | var | paren
///            | closure | spawn | chan | send | recv | select | match
/// field   ::= id | int | string
/// list    ::= '[' [expr {',' expr}] ']'
/// map     ::= '{' [expr ':' expr {',' expr ':' expr}] '}'
///
/// Select case pattern for select statements
#[derive(Debug, Clone, PartialEq)]
pub enum SelectPattern {
    /// recv(channel) pattern with optional binding
    Recv {
        binding: Option<String>,
        channel: Box<Expr>,
    },
    /// send(channel, expr) pattern
    Send {
        channel: Box<Expr>,
        value: Box<Expr>,
    },
}

/// Select case: case pattern => expr
#[derive(Debug, Clone, PartialEq)]
pub struct SelectCase {
    pub pattern: SelectPattern,
    pub guard: Option<Box<Expr>>, // Optional guard expression
    pub body: Box<Expr>,
}

/// Template string part: either a literal string or an interpolated expression
#[derive(Debug, Clone, PartialEq)]
pub enum TemplateStringPart {
    /// String literal part
    Literal(String),
    /// Interpolated expression part (only ${expr} syntax)
    Expr(Box<Expr>),
}

impl Expr {
    // Enhanced formatting support has been removed. Only ${...} interpolation remains.

    // Default value-to-string conversion handled inline where needed.
}

/// Pattern matching pattern for match expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Literal pattern: matches exact values (1, "hello", true)
    Literal(Val),
    /// Variable pattern: binds any value to a variable (x)
    Variable(String),
    /// Wildcard pattern: matches anything, no binding (_)
    Wildcard,
    /// Array/List destructuring pattern: [first, second, ..rest]
    List {
        patterns: Vec<Pattern>,
        rest: Option<String>, // Variable to bind rest of list
    },
    /// Map/Object destructuring pattern: {"key": pattern, "other": var}
    Map {
        patterns: Vec<(String, Pattern)>,
        rest: Option<String>, // Variable to bind remaining fields
    },
    /// Multiple patterns with | (pattern1 | pattern2)
    Or(Vec<Pattern>),
    /// Pattern with guard condition (pattern if guard_expr)
    Guard {
        pattern: Box<Pattern>,
        guard: Box<Expr>,
    },
    /// Range pattern: 1..10, 'a'..='z'
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
}

/// Match arm: pattern => expression
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Literal(val) => write!(f, "{}", val),
            Pattern::Variable(name) => write!(f, "{}", name),
            Pattern::Wildcard => write!(f, "_"),
            Pattern::List { patterns, rest } => {
                write!(f, "[")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pattern)?;
                }
                if let Some(rest_name) = rest {
                    if !patterns.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "..{}", rest_name)?;
                }
                write!(f, "]")
            }
            Pattern::Map { patterns, rest } => {
                write!(f, "{{")?;
                for (i, (key, pattern)) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", key, pattern)?;
                }
                if let Some(rest_name) = rest {
                    if !patterns.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "..{}", rest_name)?;
                }
                write!(f, "}}")
            }
            Pattern::Or(patterns) => {
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", pattern)?;
                }
                Ok(())
            }
            Pattern::Guard { pattern, guard } => {
                write!(f, "{} if {}", pattern, guard)
            }
            Pattern::Range {
                start,
                end,
                inclusive,
            } => {
                let op = if *inclusive { "..=" } else { ".." };
                write!(f, "{}{}{}", start, op, end)
            }
        }
    }
}

impl Pattern {
    /// Check if this pattern matches a value, returning bindings if it matches
    /// Returns Ok(Some(bindings)) on match, Ok(None) on no match, Err on error
    pub fn matches(
        &self,
        value: &Val,
        ctx: &Val,
        env: Option<&crate::stmt::Environment>,
    ) -> Result<Option<Vec<(String, Val)>>> {
        let mut bindings = Vec::new();
        if self.matches_impl(value, &mut bindings, ctx, env)? {
            Ok(Some(bindings))
        } else {
            Ok(None)
        }
    }

    fn matches_impl(
        &self,
        value: &Val,
        bindings: &mut Vec<(String, Val)>,
        ctx: &Val,
        env: Option<&crate::stmt::Environment>,
    ) -> Result<bool> {
        match self {
            Pattern::Literal(pattern_val) => Ok(value == pattern_val),
            Pattern::Variable(name) => {
                bindings.push((name.clone(), value.clone()));
                Ok(true)
            }
            Pattern::Wildcard => Ok(true),
            Pattern::List { patterns, rest } => {
                let list_items: Vec<Val> = match value {
                    Val::List(list) => (*list).to_vec(),
                    Val::Str(s) => {
                        // Convert string to list of character strings for destructuring
                        s.chars()
                            .map(|c| Val::Str(c.to_string().into()))
                            .collect::<Vec<_>>()
                    }
                    _ => return Ok(false),
                };

                // Check if we have enough elements for non-rest patterns
                if patterns.len() > list_items.len() && rest.is_none() {
                    return Ok(false);
                }

                // Match each pattern against corresponding list element
                for (i, pattern) in patterns.iter().enumerate() {
                    if i >= list_items.len() {
                        return Ok(false);
                    }
                    if !pattern.matches_impl(&list_items[i], bindings, ctx, env)? {
                        return Ok(false);
                    }
                }

                // Bind rest elements if specified
                if let Some(rest_name) = rest {
                    let rest_items: Vec<Val> =
                        list_items.iter().skip(patterns.len()).cloned().collect();
                    bindings.push((rest_name.clone(), Val::List(Arc::from(rest_items))));
                } else if patterns.len() != list_items.len() {
                    // No rest pattern but lengths don't match
                    return Ok(false);
                }

                Ok(true)
            }
            Pattern::Map { patterns, rest } => {
                if let Val::Map(map) = value {
                    let map_ref = map.as_ref();

                    // Match each pattern against corresponding map field
                    for (key, pattern) in patterns {
                        if let Some(field_val) = map_ref.get(key.as_str()) {
                            if !pattern.matches_impl(field_val, bindings, ctx, env)? {
                                return Ok(false);
                            }
                        } else {
                            return Ok(false); // Required key not found
                        }
                    }

                    // Bind remaining fields if specified
                    if let Some(rest_name) = rest {
                        let matched_keys: std::collections::HashSet<&str> =
                            patterns.iter().map(|(k, _)| k.as_str()).collect();
                        let rest_map: std::collections::HashMap<String, Val> = map_ref
                            .iter()
                            .filter(|(k, _)| !matched_keys.contains(k.as_ref()))
                            .map(|(k, v)| (k.to_string(), v.clone()))
                            .collect();
                        bindings.push((rest_name.clone(), rest_map.into()));
                    }

                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pattern::Or(patterns) => {
                for pattern in patterns {
                    let mut temp_bindings = Vec::new();
                    if pattern.matches_impl(value, &mut temp_bindings, ctx, env)? {
                        bindings.extend(temp_bindings);
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Pattern::Guard { pattern, guard } => {
                let mut temp_bindings = Vec::new();
                if pattern.matches_impl(value, &mut temp_bindings, ctx, env)? {
                    // Create temporary environment with pattern bindings for guard evaluation
                    let guard_env = if let Some(env) = env {
                        let mut new_env = env.clone();
                        new_env.push_scope_with_capacity(temp_bindings.len());
                        for (name, val) in &temp_bindings {
                            new_env.define(name.clone(), val.clone());
                        }
                        Some(new_env)
                    } else if !temp_bindings.is_empty() {
                        return Err(anyhow!(
                            "Guard conditions with bindings require evaluation environment"
                        ));
                    } else {
                        None
                    };

                    let guard_result = guard.eval_with_env(ctx, guard_env.as_ref().or(env))?;
                    if let Val::Bool(true) = guard_result {
                        bindings.extend(temp_bindings);
                        Ok(true)
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }
            Pattern::Range {
                start,
                end,
                inclusive,
            } => {
                let start_val = start.eval_with_env(ctx, env)?;
                let end_val = end.eval_with_env(ctx, env)?;

                match (value, &start_val, &end_val) {
                    (Val::Int(v), Val::Int(s), Val::Int(e)) => {
                        if *inclusive {
                            Ok(*v >= *s && *v <= *e)
                        } else {
                            Ok(*v >= *s && *v < *e)
                        }
                    }
                    (Val::Float(v), Val::Float(s), Val::Float(e)) => {
                        if *inclusive {
                            Ok(*v >= *s && *v <= *e)
                        } else {
                            Ok(*v >= *s && *v < *e)
                        }
                    }
                    _ => Ok(false),
                }
            }
        }
    }
}

/// Details:
/// - No implicit context
///   + Identifiers must be defined in the lexical environment (e.g., via `let` in statements).
///   + There is no implicit runtime context lookup. Read with `io.read()` and parse with `json/yaml/toml` modules when needed.
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
/// - `{ "age": 20 }.age >= 18`
/// - `import json; let data = json.parse(io.read()); data.user.name == "Alice"` (in statements)
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
    /// cond ? then_expr : else_expr
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    /// expr && expr
    And(Box<Expr>, Box<Expr>),
    /// expr || expr
    Or(Box<Expr>, Box<Expr>),
    /// expr ?? expr (nullish coalescing)
    NullishCoalescing(Box<Expr>, Box<Expr>),
    /// expr.field
    Access(Box<Expr>, Box<Expr>),
    /// expr?.field (optional chaining)
    OptionalAccess(Box<Expr>, Box<Expr>),
    // (expr)
    Paren(Box<Expr>),
    /// [expr, expr, ...]
    List(Vec<Box<Expr>>),
    /// {expr: expr, expr: expr, ...}
    Map(Vec<(Box<Expr>, Box<Expr>)>),
    /// Variable identifier
    Var(String),
    /// Function call: func_name(arg1, arg2, ...)
    Call(String, Vec<Box<Expr>>),
    /// Function call on expression: expr(arg1, arg2, ...)
    CallExpr(Box<Expr>, Vec<Box<Expr>>),
    /// Range expression: start..end with optional step: start..end..step
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool, // .. vs ..=
        step: Option<Box<Expr>>, // optional explicit step (positive or negative, non-zero)
    },
    /// spawn(expr) - spawn a new task
    Spawn(Box<Expr>),
    /// chan(capacity, type) - create a channel
    ChanLiteral {
        capacity: Option<Box<Expr>>,
        type_expr: Option<Box<Expr>>,
    },
    /// send(channel, value) - send a value to a channel
    Send {
        channel: Box<Expr>,
        value: Box<Expr>,
    },
    /// recv(channel) - receive a value from a channel
    Recv(Box<Expr>),
    /// select { case pattern => expr; ...; default => expr }
    Select {
        cases: Vec<SelectCase>,
        default_case: Option<Box<Expr>>,
    },
    /// Template string: `Hello ${name}!`
    TemplateString(Vec<TemplateStringPart>),
    /// Closure: |param1, param2| expr
    Closure {
        params: Vec<String>,
        body: Box<Expr>,
    },
    /// Match expression: match value { pattern => expr, ... }
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Val(Val),
}

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        self.eval_with_env(ctx, None)
    }

    /// 支持变量环境的表达式求值
    pub fn eval_with_env(&self, ctx: &Val, env: Option<&crate::stmt::Environment>) -> Result<Val> {
        // Optional fast-path: if VM feature is enabled and the environment flag
        // `QCL_VM_LITE` is set, route trivially compilable expressions through the VM.
        // This is a minimal integration to validate the VM scaffold end-to-end.
        #[cfg(feature = "vm")]
        {
            if std::env::var("QCL_VM_LITE").is_ok() {
                if matches!(self, Expr::Val(_)) {
                    let c = crate::vm::Compiler::new();
                    let f = c.compile_expr(self);
                    let mut vm = crate::vm::Vm::new();
                    return vm.exec(&f);
                }
            }
        }
        match self {
            Expr::Bin(l, op, r) => {
                let left_val = l.eval_with_env(ctx, env)?;
                let right_val = r.eval_with_env(ctx, env)?;
                op.eval_vals(&left_val, &right_val)
            }
            Expr::Conditional(cond, then_expr, else_expr) => {
                let cv = cond.eval_with_env(ctx, env)?;
                match cv {
                    Val::Bool(true) => then_expr.eval_with_env(ctx, env),
                    Val::Bool(false) => else_expr.eval_with_env(ctx, env),
                    _ => Err(anyhow!("Ternary condition must be Bool, got: {:?}", cv)),
                }
            }
            Expr::Unary(op, expr) => {
                let val = expr.eval_with_env(ctx, env)?;
                op.eval_val(&val)
            }
            Expr::And(e1, e2) => {
                let l = e1.eval_with_env(ctx, env)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(false) = l {
                    return Ok(Val::Bool(false));
                }
                let r = e2.eval_with_env(ctx, env)?;
                match (&l, &r) {
                    (Val::Bool(true), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "&&", &r),
                }
            }
            Expr::Or(e1, e2) => {
                let l = e1.eval_with_env(ctx, env)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(true) = l {
                    return Ok(Val::Bool(true));
                }
                let r = e2.eval_with_env(ctx, env)?;
                match (&l, &r) {
                    (Val::Bool(_), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "||", &r),
                }
            }
            Expr::NullishCoalescing(e1, e2) => {
                let l = e1.eval_with_env(ctx, env)?;
                // If left side is nil, return right side; otherwise return left side
                if l == Val::Nil {
                    e2.eval_with_env(ctx, env)
                } else {
                    Ok(l)
                }
            }
            // legacy '@' context access removed
            Expr::Access(expr, field) => {
                let val = expr.eval_with_env(ctx, env)?;
                let field_val = field.eval_with_env(ctx, env)?;
                match val.access(&field_val) {
                    Some(v) => Ok(v),
                    None => Ok(Val::Nil),
                }
            }
            Expr::OptionalAccess(expr, field) => {
                let val = expr.eval_with_env(ctx, env)?;
                // Short-circuit if the left side is nil
                if val == Val::Nil {
                    return Ok(Val::Nil);
                }
                let field_val = field.eval_with_env(ctx, env)?;
                match val.access(&field_val) {
                    Some(v) => Ok(v),
                    None => Ok(Val::Nil),
                }
            }
            Expr::List(exprs) => {
                let mut values = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values.push(expr.eval_with_env(ctx, env)?);
                }
                Ok(Val::List(Arc::from(values)))
            }
            Expr::Map(pairs) => {
                let mut map = std::collections::HashMap::with_capacity(pairs.len());
                for (key_expr, value_expr) in pairs {
                    let key_val = key_expr.eval_with_env(ctx, env)?;
                    let value_val = value_expr.eval_with_env(ctx, env)?;

                    // Convert key to string for map indexing
                    let key_str = match key_val {
                        Val::Str(s) => s.as_ref().to_string(),
                        Val::Int(i) => i.to_string(),
                        Val::Float(f) => f.to_string(),
                        Val::Bool(b) => b.to_string(),
                        _ => {
                            return Err(anyhow!(
                                "Map key must be a primitive type, got: {:?}",
                                key_val
                            ));
                        }
                    };

                    map.insert(key_str, value_val);
                }
                Ok(Val::from(map))
            }
            Expr::Paren(expr) => expr.eval_with_env(ctx, env),
            Expr::Var(name) => {
                // Only resolve variables from the lexical environment. No implicit context lookup.
                if let Some(env) = env {
                    if let Some(val) = env.get_value(name) {
                        return Ok(val);
                    }
                }
                Err(anyhow!("Undefined variable: {}", name))
            }
            Expr::Call(func_name, args) => {
                if let Some(env) = env {
                    // Prefer slot-mapped fast path for function lookup
                    if let Some(func_val) = env.get_value(func_name) {
                        // Evaluate arguments (预分配容量)
                        let mut arg_values = Vec::with_capacity(args.len());
                        for arg in args {
                            arg_values.push(arg.eval_with_env(ctx, Some(env))?);
                        }
                        func_val.call(&arg_values, env, ctx)
                    } else {
                        Err(anyhow!("Undefined function: {}", func_name))
                    }
                } else {
                    Err(anyhow!("Function call {} requires environment", func_name))
                }
            }
            Expr::CallExpr(expr, args) => {
                // Special-case: method call sugar on access (obj.method(...))
                if let Expr::Access(obj_expr, field_expr) = expr.as_ref() {
                    // Evaluate receiver and field name first
                    let obj_val = obj_expr.eval_with_env(ctx, env)?;
                    let field_val = field_expr.eval_with_env(ctx, env)?;

                    if let Val::Str(method_name) = field_val {
                        let method_name_str = method_name.as_ref();

                        // First, try normal property-as-function if it's actually callable
                        if let Some(prop_val) = obj_val.access(&Val::Str(method_name.clone())) {
                            match prop_val {
                                Val::Closure { .. } | Val::RustFunction(_) => {
                                    // Evaluate arguments (预分配容量)
                                    let mut arg_values = Vec::with_capacity(args.len());
                                    for arg in args {
                                        arg_values.push(arg.eval_with_env(ctx, env)?);
                                    }
                                    if let Some(env) = env {
                                        return prop_val.call(&arg_values, env, ctx);
                                    } else {
                                        return Err(anyhow!("Function call requires environment"));
                                    }
                                }
                                _ => {
                                    // Property access with `()` — treat as value access
                                    return Ok(prop_val);
                                }
                            }
                        }

                        // Fall back to meta method registry
                        if let Some(func) =
                            crate::val::methods::find_method_for_val(&obj_val, method_name_str)
                        {
                            // Evaluate arguments and prepend receiver
                            let mut full_args = Vec::with_capacity(args.len() + 1);
                            full_args.push(obj_val.clone());
                            for arg in args {
                                full_args.push(arg.eval_with_env(ctx, env)?);
                            }
                            if let Some(env) = env {
                                // Call the meta method and return the result
                                let result = func(&full_args, env, ctx)?;
                                return Ok(result);
                            } else {
                                return Err(anyhow!("Function call requires environment"));
                            }
                        }

                        // No property or method found; produce a clearer error
                        return Err(anyhow!(
                            "{} has no method '{}'",
                            obj_val.type_name(),
                            method_name_str
                        ));
                    }
                }

                // Default: call the evaluated expression as a function
                let func_val = expr.eval_with_env(ctx, env)?;

                // Evaluate arguments (预分配容量)
                let mut arg_values = Vec::with_capacity(args.len());
                for arg in args {
                    arg_values.push(arg.eval_with_env(ctx, env)?);
                }

                if let Some(env) = env {
                    func_val.call(&arg_values, env, ctx)
                } else {
                    Err(anyhow!("Function call requires environment"))
                }
            }
            Expr::Range {
                start,
                end,
                inclusive,
                step,
            } => {
                let start_val = match start {
                    Some(expr) => expr.eval_with_env(ctx, env)?,
                    None => Val::Int(0),
                };
                let end_val = match end {
                    Some(expr) => expr.eval_with_env(ctx, env)?,
                    None => return Err(anyhow!("Open-ended ranges not supported in for loops")),
                };
                let step_val = match step {
                    Some(expr) => Some(expr.eval_with_env(ctx, env)?),
                    None => None,
                };

                // Generate range list
                match (start_val, end_val, step_val) {
                    (Val::Int(s), Val::Int(e), None) => {
                        let range: Vec<Val> = if *inclusive {
                            (s..=e).map(Val::Int).collect()
                        } else {
                            (s..e).map(Val::Int).collect()
                        };
                        Ok(Val::List(range.into()))
                    }
                    (Val::Int(mut i), Val::Int(e), Some(Val::Int(st))) => {
                        if st == 0 {
                            return Err(anyhow!("Range step cannot be zero"));
                        }
                        let mut out: Vec<Val> = Vec::new();
                        if st > 0 {
                            if *inclusive {
                                while i <= e {
                                    out.push(Val::Int(i));
                                    i += st;
                                }
                            } else {
                                while i < e {
                                    out.push(Val::Int(i));
                                    i += st;
                                }
                            }
                        } else {
                            if *inclusive {
                                while i >= e {
                                    out.push(Val::Int(i));
                                    i += st; // st is negative
                                }
                            } else {
                                while i > e {
                                    out.push(Val::Int(i));
                                    i += st;
                                }
                            }
                        }
                        Ok(Val::List(out.into()))
                    }
                    (_, _, Some(_)) => Err(anyhow!("Range step must be an integer")),
                    _ => Err(anyhow!("Range bounds must be integers")),
                }
            }
            Expr::Spawn(expr) => {
                #[cfg(feature = "concurrency")]
                {
                    // Clone the expression and context for the spawned task
                    let expr_clone = expr.clone();
                    let ctx_clone = ctx.clone();
                    let env_clone = env.cloned();

                    // Create a future that evaluates the expression
                    let future =
                        async move { expr_clone.eval_with_env(&ctx_clone, env_clone.as_ref()) };

                    // Spawn the task using the runtime
                    match crate::rt::with_runtime(|runtime| runtime.spawn(future)) {
                        Ok(task_id) => {
                            Ok(Val::Task {
                                id: task_id,
                                value: None, // Value will be set when task completes
                            })
                        }
                        Err(e) => Err(anyhow!("Failed to spawn task: {}", e)),
                    }
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    // Fallback: just evaluate the expression synchronously
                    let expr_val = expr.eval_with_env(ctx, env)?;
                    static mut TASK_ID_COUNTER: u64 = 0;
                    unsafe {
                        TASK_ID_COUNTER += 1;
                        Ok(Val::Task {
                            id: TASK_ID_COUNTER,
                            value: Some(Box::new(expr_val)),
                        })
                    }
                }
            }
            Expr::ChanLiteral {
                capacity,
                type_expr,
            } => {
                // Evaluate capacity if provided
                let capacity_num = match capacity {
                    Some(cap_expr) => {
                        let cap_val = cap_expr.eval_with_env(ctx, env)?;
                        match cap_val {
                            Val::Int(n) => n,
                            _ => return Err(anyhow!("Channel capacity must be an integer")),
                        }
                    }
                    None => 0, // Default unbuffered channel
                };

                // Parse the type expression
                let inner_type = match type_expr {
                    Some(type_expr) => {
                        let type_val = type_expr.eval_with_env(ctx, env)?;
                        match type_val {
                            Val::Str(type_str) => Type::parse(&type_str)
                                .ok_or_else(|| anyhow!("Invalid type: {}", type_str))?,
                            _ => return Err(anyhow!("Channel type must be a string")),
                        }
                    }
                    None => Type::Nil, // Default to Nil type
                };

                #[cfg(feature = "concurrency")]
                {
                    // Create channel using the runtime
                    let capacity_opt = if capacity_num == 0 {
                        None
                    } else {
                        Some(capacity_num as usize)
                    };
                    match crate::rt::with_runtime(|runtime| runtime.create_channel(capacity_opt)) {
                        Ok(channel_id) => Ok(Val::Channel {
                            id: channel_id,
                            capacity: Some(capacity_num),
                            inner_type: Box::new(inner_type),
                        }),
                        Err(e) => Err(anyhow!("Failed to create channel: {}", e)),
                    }
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    // Fallback implementation
                    static mut CHANNEL_ID_COUNTER: u64 = 0;
                    unsafe {
                        CHANNEL_ID_COUNTER += 1;
                        Ok(Val::Channel {
                            id: CHANNEL_ID_COUNTER,
                            capacity: Some(capacity_num),
                            inner_type: Box::new(inner_type),
                        })
                    }
                }
            }
            Expr::Send { channel, value } => {
                // Evaluate channel and value
                let channel_val = channel.eval_with_env(ctx, env)?;
                let value_val = value.eval_with_env(ctx, env)?;

                #[cfg(feature = "concurrency")]
                {
                    if let Val::Channel { id, .. } = channel_val {
                        match crate::rt::with_runtime(|runtime| {
                            runtime.block_on(runtime.send_async(id, value_val))
                        }) {
                            Ok(sent) => Ok(Val::Bool(sent)),
                            Err(e) => Err(anyhow!("Send operation failed: {}", e)),
                        }
                    } else {
                        Err(anyhow!("Send target is not a channel"))
                    }
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    // Fallback: just return success status
                    Ok(Val::Bool(true))
                }
            }
            Expr::Recv(channel) => {
                // Evaluate channel
                let channel_val = channel.eval_with_env(ctx, env)?;

                #[cfg(feature = "concurrency")]
                {
                    if let Val::Channel { id, .. } = channel_val {
                        match crate::rt::with_runtime(|runtime| {
                            runtime.block_on(runtime.recv_async(id))
                        }) {
                            Ok((ok, value)) => Ok(Val::List(vec![Val::Bool(ok), value].into())),
                            Err(e) => Err(anyhow!("Receive operation failed: {}", e)),
                        }
                    } else {
                        Err(anyhow!("Receive target is not a channel"))
                    }
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    // Fallback: return a tuple (ok: bool, value: T)
                    Ok(Val::List(vec![Val::Bool(false), Val::Nil].into()))
                }
            }
            Expr::Select {
                cases,
                default_case,
            } => {
                #[cfg(feature = "concurrency")]
                {
                    use crate::rt::SelectOperation;

                    let mut select_op = SelectOperation::new();
                    let mut bindings: Vec<Option<String>> = Vec::with_capacity(cases.len());

                    for (idx, case) in cases.iter().enumerate() {
                        match &case.pattern {
                            SelectPattern::Recv { binding, channel } => {
                                let channel_val = channel.eval_with_env(ctx, env)?;
                                let channel_id = if let Val::Channel { id, .. } = channel_val {
                                    id
                                } else {
                                    return Err(anyhow!("recv() target is not a channel"));
                                };
                                select_op.add_recv(idx, channel_id);
                                bindings.push(binding.clone());
                            }
                            SelectPattern::Send { channel, value } => {
                                let channel_val = channel.eval_with_env(ctx, env)?;
                                let value_val = value.eval_with_env(ctx, env)?;
                                let channel_id = if let Val::Channel { id, .. } = channel_val {
                                    id
                                } else {
                                    return Err(anyhow!("send() target is not a channel"));
                                };
                                select_op.add_send(idx, channel_id, value_val);
                                bindings.push(None);
                            }
                        }
                    }

                    let has_default = default_case.is_some();
                    if select_op.is_empty() && !has_default {
                        return Ok(Val::Nil);
                    }

                    let select_result = crate::rt::with_runtime(|runtime| {
                        runtime.block_on(select_op.execute(runtime, has_default))
                    })?;

                    if select_result.is_default {
                        if let Some(default_expr) = default_case {
                            return default_expr.eval_with_env(ctx, env);
                        }
                        return Ok(Val::Nil);
                    }

                    let case_index = select_result
                        .case_index
                        .ok_or_else(|| anyhow!("Select returned no case index"))?;

                    let selected_case = cases
                        .get(case_index)
                        .ok_or_else(|| anyhow!("Invalid select case index"))?;

                    let binding_name = bindings.get(case_index).cloned().flatten();

                    if binding_name.is_some() && env.is_none() {
                        return Err(anyhow!("Select binding requires evaluation environment"));
                    }

                    let binding_env: Option<crate::stmt::Environment>;
                    let env_for_case: Option<&crate::stmt::Environment> = if let Some(env_ref) = env
                    {
                        if let Some(name) = binding_name {
                            let mut new_env = env_ref.clone();
                            new_env.push_scope();
                            let tuple_val = select_result
                                .recv_payload
                                .clone()
                                .map(|(ok, value)| Val::List(vec![Val::Bool(ok), value].into()))
                                .unwrap_or_else(|| {
                                    Val::List(vec![Val::Bool(false), Val::Nil].into())
                                });
                            new_env.define(name, tuple_val);
                            binding_env = Some(new_env);
                            binding_env.as_ref()
                        } else {
                            Some(env_ref)
                        }
                    } else {
                        None
                    };

                    selected_case.body.eval_with_env(ctx, env_for_case)
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    // Fallback: evaluate default case if present, otherwise return nil
                    if let Some(default_expr) = default_case {
                        default_expr.eval_with_env(ctx, env)
                    } else {
                        Ok(Val::Nil)
                    }
                }
            }
            Expr::TemplateString(parts) => {
                let mut result = String::new();
                for part in parts {
                    match part {
                        TemplateStringPart::Literal(s) => {
                            result.push_str(s);
                        }
                        TemplateStringPart::Expr(expr) => {
                            let val = expr.eval_with_env(ctx, env)?;
                            let str_val = match val {
                                Val::Str(s) => s.as_ref().to_string(),
                                Val::Int(i) => i.to_string(),
                                Val::Float(f) => f.to_string(),
                                Val::Bool(b) => b.to_string(),
                                Val::Nil => "nil".to_string(),
                                Val::List(l) => format!("{:?}", l),
                                Val::Map(m) => format!("{:?}", m),
                                Val::Object { .. } => format!("{:?}", val),
                                #[cfg(feature = "concurrency")]
                                Val::Task { .. } => format!("{:?}", val),
                                #[cfg(feature = "concurrency")]
                                Val::Channel { .. } => format!("{:?}", val),
                                Val::Closure { .. } => "[Closure]".to_string(),
                                Val::RustFunction(_) => "[Function]".to_string(),
                            };
                            result.push_str(&str_val);
                        }
                    }
                }
                Ok(Val::Str(Arc::from(result)))
            }
            Expr::Match { value, arms } => {
                let match_val = value.eval_with_env(ctx, env)?;

                for arm in arms {
                    if let Some(bindings) = Pattern::matches(&arm.pattern, &match_val, ctx, env)? {
                        // Create new environment with pattern bindings
                        let new_env = if let Some(env) = env {
                            let mut new_env = env.clone();
                            new_env.push_scope_with_capacity(bindings.len());
                            for (name, val) in bindings {
                                new_env.define(name, val);
                            }
                            Some(new_env)
                        } else if !bindings.is_empty() {
                            // Need environment for bindings but none provided
                            return Err(anyhow!("Pattern bindings require evaluation environment"));
                        } else {
                            None
                        };

                        return arm.body.eval_with_env(ctx, new_env.as_ref().or(env));
                    }
                }

                Err(anyhow!("No pattern matched in match expression"))
            }
            // Remove the problematic string-to-variable resolution
            // String literals should always be treated as string literals
            Expr::Closure { params, body } => {
                // Create a closure value with the captured environment
                let env = env.cloned().unwrap_or_default();
                let stmt = crate::stmt::Stmt::Expr(body.clone());
                Ok(Val::Closure {
                    params: Arc::new(params.clone()),
                    body: Arc::new(stmt),
                    env: Arc::new(env),
                    upvalues: Arc::new(Vec::new()),
                    #[cfg(feature = "vm")]
                    code: Arc::new(once_cell::sync::OnceCell::new()),
                })
            }
            Expr::Val(val) => Ok(val.clone()), // Clone necessary as eval returns owned Val
        }
    }

    /// Get the requested context names from the expression.
    pub fn requested_ctx(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        self.collect_ctx_names(&mut names);
        names
    }

    /// Helper method to collect context names recursively
    ///
    /// eg.: `user.props.(req.service).value && list` => `["user", "req", "list"]`
    fn collect_ctx_names(&self, names: &mut HashSet<String>) {
        match self {
            Expr::Conditional(c, t, e) => {
                c.collect_ctx_names(names);
                t.collect_ctx_names(names);
                e.collect_ctx_names(names);
            }
            // legacy '@' context access removed
            Expr::Access(expr, field) => {
                expr.collect_ctx_names(names);
                field.collect_ctx_names(names);
            }
            Expr::OptionalAccess(expr, field) => {
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
            Expr::And(l, r) | Expr::Or(l, r) | Expr::NullishCoalescing(l, r) => {
                l.collect_ctx_names(names);
                r.collect_ctx_names(names);
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
            // Variables contribute potential context roots
            Expr::Var(name) => {
                names.insert(name.clone());
            }
            // Function calls - collect from arguments
            Expr::Call(_, args) => {
                for arg in args {
                    arg.collect_ctx_names(names);
                }
            }
            Expr::CallExpr(expr, args) => {
                expr.collect_ctx_names(names);
                for arg in args {
                    arg.collect_ctx_names(names);
                }
            }
            Expr::Range { start, end, step, .. } => {
                if let Some(s) = start {
                    s.collect_ctx_names(names);
                }
                if let Some(e) = end {
                    e.collect_ctx_names(names);
                }
                if let Some(st) = step {
                    st.collect_ctx_names(names);
                }
            }
            Expr::Spawn(expr) => {
                expr.collect_ctx_names(names);
            }
            Expr::ChanLiteral {
                capacity,
                type_expr,
            } => {
                if let Some(cap_expr) = capacity {
                    cap_expr.collect_ctx_names(names);
                }
                if let Some(type_expr) = type_expr {
                    type_expr.collect_ctx_names(names);
                }
            }
            Expr::Send { channel, value } => {
                channel.collect_ctx_names(names);
                value.collect_ctx_names(names);
            }
            Expr::Recv(channel) => {
                channel.collect_ctx_names(names);
            }
            Expr::Select {
                cases,
                default_case,
            } => {
                for case in cases {
                    match &case.pattern {
                        SelectPattern::Recv { channel, .. } => {
                            channel.collect_ctx_names(names);
                        }
                        SelectPattern::Send { channel, value } => {
                            channel.collect_ctx_names(names);
                            value.collect_ctx_names(names);
                        }
                    }
                    if let Some(guard) = &case.guard {
                        guard.collect_ctx_names(names);
                    }
                    case.body.collect_ctx_names(names);
                }
                if let Some(default_expr) = default_case {
                    default_expr.collect_ctx_names(names);
                }
            }
            Expr::TemplateString(parts) => {
                for part in parts {
                    match part {
                        TemplateStringPart::Literal(_) => {}
                        TemplateStringPart::Expr(expr) => {
                            expr.collect_ctx_names(names);
                        }
                    }
                }
            }
            Expr::Closure { params: _, body } => {
                body.collect_ctx_names(names);
            }
            Expr::Match { value, arms } => {
                value.collect_ctx_names(names);
                for arm in arms {
                    arm.body.collect_ctx_names(names);
                    // Collect from guard patterns if they contain context references
                    if let Pattern::Guard { guard, .. } = &arm.pattern {
                        guard.collect_ctx_names(names);
                    }
                }
            }
            // Only collect string values when they are actual context names, not field names
            Expr::Val(_) => {} // Receive operator: collect from inner expression
        }
    }

    /// Cached parsing: parse expression string and return a shared Arc<Expr>.
    /// Use `parse_cached` if you need an owned `Expr` value.
    pub fn parse_cached_arc(expression: &str) -> Result<Arc<Expr>> {
        use std::collections::hash_map::Entry;
        use std::sync::RwLock;

        // Global static cache: Key is expression string, Value is parsed Expr wrapped in Arc
        static PARSE_CACHE: Lazy<RwLock<HashMap<String, Arc<Expr>>>> =
            Lazy::new(|| RwLock::new(HashMap::new()));

        // Fast read path
        if let Some(found) = PARSE_CACHE
            .read()
            .ok()
            .and_then(|c| c.get(expression).cloned())
        {
            return Ok(found);
        }

        // Parse on miss, then insert with write lock
        let tokens = Tokenizer::tokenize(expression)?;
        let expr = Parser::new(&tokens).parse()?; // Constant folding happens in parser
        let expr_arc = Arc::new(expr);

        if let Ok(mut write) = PARSE_CACHE.write() {
            match write.entry(expression.to_string()) {
                Entry::Vacant(v) => {
                    v.insert(expr_arc.clone());
                    Ok(expr_arc)
                }
                Entry::Occupied(o) => Ok(o.get().clone()),
            }
        } else {
            // If poisoned, just return the fresh value without caching
            Ok(expr_arc)
        }
    }

    /// Backwards-compatible helper that returns an owned `Expr` by cloning
    /// the shared cached AST. Prefer `parse_cached_arc` for performance.
    pub fn parse_cached(expression: &str) -> Result<Expr> {
        Ok(Self::parse_cached_arc(expression)?.as_ref().clone())
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
            Expr::Conditional(c_box, t_box, e_box) => {
                let c = (*c_box).fold_constants();
                let t = (*t_box).fold_constants();
                let e = (*e_box).fold_constants();
                if let Expr::Val(Val::Bool(b)) = c {
                    return if b { t } else { e };
                }
                Expr::Conditional(Box::new(c), Box::new(t), Box::new(e))
            }
            Expr::Unary(op, expr_box) => {
                let inner = (*expr_box).fold_constants();
                // Constant folding: !expr, if expr is boolean constant then calculate result
                if let Expr::Val(Val::Bool(b)) = &inner {
                    return Expr::Val(Val::Bool(!*b));
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
            Expr::NullishCoalescing(e1_box, e2_box) => {
                let e1 = (*e1_box).fold_constants();
                // If left side is constant not nil, return it
                if let Expr::Val(v) = &e1
                    && *v != Val::Nil
                {
                    return e1;
                }
                let e2 = (*e2_box).fold_constants();
                // If left side is constant nil, return right side
                if let Expr::Val(Val::Nil) = e1 {
                    return e2;
                }
                Expr::NullishCoalescing(Box::new(e1), Box::new(e2))
            }
            // legacy '@' context access removed
            Expr::Access(base_box, field_box) => {
                let base = (*base_box).fold_constants();
                let field = (*field_box).fold_constants();
                if let (Expr::Val(base_val), Expr::Val(field_val)) = (&base, &field) {
                    // Important: preserve Access when the field is a string literal so that
                    // subsequent call syntax (e.g. foo.bar()) can be intercepted for meta-method dispatch.
                    // This avoids turning `foo.bar` into a concrete value (e.g. Int), which would
                    // later cause `foo.bar()` to attempt calling a non-function value.
                    if matches!(field_val, Val::Str(_)) {
                        return Expr::Access(Box::new(base.clone()), Box::new(field.clone()));
                    }

                    // For non-string fields (e.g. numeric indices), fold direct access where possible
                    if let Some(res_val) = base_val.access(field_val) {
                        return Expr::Val(res_val);
                    } else {
                        return Expr::Val(Val::Nil);
                    }
                }
                Expr::Access(Box::new(base), Box::new(field))
            }
            Expr::OptionalAccess(base_box, field_box) => {
                let base = (*base_box).fold_constants();
                let field = (*field_box).fold_constants();
                if let (Expr::Val(base_val), Expr::Val(field_val)) = (&base, &field) {
                    // Preserve OptionalAccess when field is a string literal to allow potential
                    // optional method-call sugar like `obj?.method()` to be handled later.
                    if matches!(field_val, Val::Str(_)) {
                        return Expr::OptionalAccess(
                            Box::new(base.clone()),
                            Box::new(field.clone()),
                        );
                    }

                    // Direct access to constant structure with optional chaining
                    if base_val == &Val::Nil {
                        return Expr::Val(Val::Nil);
                    }
                    if let Some(res_val) = base_val.access(field_val) {
                        return Expr::Val(res_val);
                    } else {
                        return Expr::Val(Val::Nil);
                    }
                }
                Expr::OptionalAccess(Box::new(base), Box::new(field))
            }
            Expr::List(exprs) => {
                // List constant folding: if all elements are constants then fold to one Val::List
                let folded_elems: Vec<Expr> =
                    exprs.into_iter().map(|e| e.fold_constants()).collect();
                if folded_elems.iter().all(|e| matches!(e, Expr::Val(_))) {
                    // Extract all constant values as new list elements
                    let const_vals: Vec<Val> = folded_elems
                        .into_iter()
                        .map(|e| {
                            if let Expr::Val(v) = e {
                                v
                            } else {
                                unreachable!()
                            }
                        })
                        .collect();
                    return Expr::Val(Val::List(Arc::from(const_vals)));
                }
                Expr::List(folded_elems.into_iter().map(Box::new).collect())
            }
            Expr::Map(pairs) => {
                // Map constant folding: if all keys and values are constants, then construct constant Map
                let folded_pairs: Vec<(Box<Expr>, Box<Expr>)> = pairs
                    .into_iter()
                    .map(|(k, v)| (Box::new(k.fold_constants()), Box::new(v.fold_constants())))
                    .collect();
                if folded_pairs
                    .iter()
                    .all(|(k, v)| matches!(&**k, Expr::Val(_)) && matches!(&**v, Expr::Val(_)))
                {
                    let mut const_map = HashMap::with_capacity(folded_pairs.len());
                    for (k_expr, v_expr) in &folded_pairs {
                        if let (Expr::Val(k_val), Expr::Val(v_val)) = (&**k_expr, &**v_expr) {
                            // Convert key to string (only allow basic type keys)
                            let key_str = match k_val {
                                Val::Str(s) => s.as_ref().to_string(),
                                Val::Int(i) => i.to_string(),
                                Val::Float(f) => f.to_string(),
                                Val::Bool(b) => b.to_string(),
                                _ => {
                                    // Map key must be basic type, if Nil/List/Map appears, don't fold entire Map
                                    return Expr::Map(folded_pairs);
                                }
                            };
                            const_map.insert(key_str, v_val.clone());
                        }
                    }
                    return Expr::Val(Val::from(const_map));
                }
                Expr::Map(folded_pairs)
            }
            Expr::Paren(expr_box) => {
                // Keep parentheses structure, but fold internal expression
                Expr::Paren(Box::new((*expr_box).fold_constants()))
            }
            Expr::Var(name) => {
                // Variables can't be folded without environment
                Expr::Var(name)
            }
            Expr::Call(name, args) => {
                // Function calls can't be folded at compile time, but fold arguments
                let folded_args = args
                    .into_iter()
                    .map(|a| Box::new(a.fold_constants()))
                    .collect();
                Expr::Call(name, folded_args)
            }
            Expr::CallExpr(expr, args) => {
                // Function calls can't be folded at compile time, but fold expression and arguments
                let folded_expr = Box::new(expr.fold_constants());
                let folded_args = args
                    .into_iter()
                    .map(|a| Box::new(a.fold_constants()))
                    .collect();
                Expr::CallExpr(folded_expr, folded_args)
            }
            Expr::Range {
                start,
                end,
                inclusive,
                step,
            } => {
                // Range expressions with constant bounds can be folded
                let folded_start = start.map(|s| Box::new(s.fold_constants()));
                let folded_end = end.map(|e| Box::new(e.fold_constants()));
                let folded_step = step.map(|st| Box::new(st.fold_constants()));
                Expr::Range {
                    start: folded_start,
                    end: folded_end,
                    inclusive,
                    step: folded_step,
                }
            }
            Expr::Spawn(expr) => Expr::Spawn(Box::new(expr.fold_constants())),
            Expr::ChanLiteral {
                capacity,
                type_expr,
            } => {
                let folded_capacity = capacity.map(|c| Box::new(c.fold_constants()));
                let folded_type_expr = type_expr.map(|t| Box::new(t.fold_constants()));
                Expr::ChanLiteral {
                    capacity: folded_capacity,
                    type_expr: folded_type_expr,
                }
            }
            Expr::Send { channel, value } => Expr::Send {
                channel: Box::new(channel.fold_constants()),
                value: Box::new(value.fold_constants()),
            },
            Expr::Recv(channel) => Expr::Recv(Box::new(channel.fold_constants())),
            Expr::Select {
                cases,
                default_case,
            } => {
                let folded_cases = cases
                    .into_iter()
                    .map(|case| SelectCase {
                        pattern: match case.pattern {
                            SelectPattern::Recv { binding, channel } => SelectPattern::Recv {
                                binding,
                                channel: Box::new(channel.fold_constants()),
                            },
                            SelectPattern::Send { channel, value } => SelectPattern::Send {
                                channel: Box::new(channel.fold_constants()),
                                value: Box::new(value.fold_constants()),
                            },
                        },
                        guard: case.guard.map(|g| Box::new(g.fold_constants())),
                        body: Box::new(case.body.fold_constants()),
                    })
                    .collect();
                let folded_default = default_case.map(|d| Box::new(d.fold_constants()));
                Expr::Select {
                    cases: folded_cases,
                    default_case: folded_default,
                }
            }
            Expr::TemplateString(parts) => {
                // Template string constant folding: if all interpolated expressions are constants, fold to constant string
                let folded_parts: Vec<TemplateStringPart> = parts
                    .into_iter()
                    .map(|part| match part {
                        TemplateStringPart::Literal(s) => TemplateStringPart::Literal(s),
                        TemplateStringPart::Expr(expr) => {
                            let folded_expr = expr.fold_constants();
                            if let Expr::Val(val) = folded_expr {
                                // Convert constant value to string
                                let str_val = match val {
                                    Val::Str(s) => s.as_ref().to_string(),
                                    Val::Int(i) => i.to_string(),
                                    Val::Float(f) => f.to_string(),
                                    Val::Bool(b) => b.to_string(),
                                    Val::Nil => "nil".to_string(),
                                    Val::List(l) => format!("{:?}", l),
                                    Val::Map(m) => format!("{:?}", m),
                                    Val::Object { .. } => format!("{:?}", val),
                                    #[cfg(feature = "concurrency")]
                                    Val::Task { .. } => format!("{:?}", val),
                                    #[cfg(feature = "concurrency")]
                                    Val::Channel { .. } => format!("{:?}", val),
                                    Val::Closure { .. } => "[Closure]".to_string(),
                                    Val::RustFunction(_) => "[Function]".to_string(),
                                };
                                TemplateStringPart::Literal(str_val)
                            } else {
                                TemplateStringPart::Expr(Box::new(folded_expr))
                            }
                        }
                    })
                    .collect();

                // If all parts are literals, fold to a single string constant
                if folded_parts
                    .iter()
                    .all(|part| matches!(part, TemplateStringPart::Literal(_)))
                {
                    let result = folded_parts
                        .into_iter()
                        .map(|part| {
                            if let TemplateStringPart::Literal(s) = part {
                                s
                            } else {
                                unreachable!()
                            }
                        })
                        .collect::<String>();
                    return Expr::Val(Val::Str(Arc::from(result)));
                }

                Expr::TemplateString(folded_parts)
            }
            Expr::Closure { params, body } => {
                // Closures cannot be folded at compile time due to environment capture
                Expr::Closure {
                    params: params.clone(),
                    body: Box::new(body.fold_constants()),
                }
            }
            Expr::Match { value, arms } => {
                // Match expressions cannot be fully folded without runtime evaluation
                // but we can fold the value and arm bodies
                let folded_value = Box::new(value.fold_constants());
                let folded_arms = arms
                    .into_iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern, // Patterns contain runtime values, don't fold
                        body: Box::new(arm.body.fold_constants()),
                    })
                    .collect();
                Expr::Match {
                    value: folded_value,
                    arms: folded_arms,
                }
            }
        }
    }
}

impl TryInto<Val> for &Expr {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Val> {
        match self {
            Expr::Val(val) => Ok(val.clone()), // Clone necessary as eval returns owned Val
            _ => {
                let msg = format!("Can't convert Expr::{:?} to Val", self);
                Err(anyhow!(msg))
            }
        }
    }
}

fn into_expr<S: AsRef<str>>(s: S) -> Result<Expr> {
    let tokens = Tokenizer::tokenize(s.as_ref())?;
    let expr = Parser::new(&tokens).parse()?;
    Ok(expr)
}

impl TryFrom<&str> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl TryFrom<String> for Expr {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        into_expr(value)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Bin(left, op, right) => write!(f, "{left} {op:?} {right}"),
            Expr::Unary(op, expr) => write!(f, "{op:?}{expr}"),
            Expr::Conditional(c, t, e) => write!(f, "{} ? {} : {}", c, t, e),
            Expr::And(left, right) => write!(f, "{left} && {right}"),
            Expr::Or(left, right) => write!(f, "{left} || {right}"),
            Expr::NullishCoalescing(left, right) => write!(f, "{left} ?? {right}"),
            // legacy '@' context access removed
            Expr::Access(expr, field) => write!(f, "{}.{}", expr, field),
            Expr::OptionalAccess(expr, field) => write!(f, "{}?.{}", expr, field),
            Expr::List(exprs) => {
                let exprs: Vec<String> = exprs.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", exprs.join(", "))
            }
            Expr::Map(pairs) => {
                let pairs: Vec<String> =
                    pairs.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            }
            Expr::Paren(expr) => write!(f, "{expr}"),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Call(name, args) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}({})", name, args_str.join(", "))
            }
            Expr::CallExpr(expr, args) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}({})", expr, args_str.join(", "))
            }
            Expr::Range {
                start,
                end,
                inclusive,
                step,
            } => {
                let start_str = match start {
                    Some(s) => s.to_string(),
                    None => "".to_string(),
                };
                let end_str = match end {
                    Some(e) => e.to_string(),
                    None => "".to_string(),
                };
                let op = if *inclusive { "..=" } else { ".." };
                if let Some(st) = step {
                    write!(f, "{}{}{}..{}", start_str, op, end_str, st)
                } else {
                    write!(f, "{}{}{}", start_str, op, end_str)
                }
            }
            Expr::Spawn(expr) => write!(f, "spawn({})", expr),
            Expr::ChanLiteral {
                capacity,
                type_expr,
            } => {
                write!(f, "chan(")?;
                if let Some(cap) = capacity {
                    write!(f, "{}", cap)?;
                    if type_expr.is_some() {
                        write!(f, ", ")?;
                    }
                }
                if let Some(ty) = type_expr {
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            Expr::Send { channel, value } => write!(f, "send({}, {})", channel, value),
            Expr::Recv(channel) => write!(f, "recv({})", channel),
            Expr::Select {
                cases,
                default_case,
            } => {
                write!(f, "select {{")?;
                for (i, case) in cases.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "case ")?;
                    match &case.pattern {
                        SelectPattern::Recv { binding, channel } => {
                            if let Some(name) = binding {
                                write!(f, "{} <- recv({})", name, channel)?;
                            } else {
                                write!(f, "recv({})", channel)?;
                            }
                        }
                        SelectPattern::Send { channel, value } => {
                            write!(f, "{} <= send({})", channel, value)?
                        }
                    }
                    write!(f, " => {}", case.body)?;
                }
                if let Some(default) = default_case {
                    if !cases.is_empty() {
                        write!(f, "; ")?;
                    }
                    write!(f, "default => {}", default)?;
                }
                write!(f, "}}")
            }
            Expr::TemplateString(parts) => {
                write!(f, "\"")?;
                for part in parts {
                    match part {
                        TemplateStringPart::Literal(s) => {
                            // Escape backslashes, quotes and dollar signs in literals
                            let escaped = s
                                .replace("\\", "\\\\")
                                .replace("\"", "\\\"")
                                .replace("$", "\\$")
                                .replace("{", "\\{")
                                .replace("}", "\\}");
                            write!(f, "{}", escaped)?;
                        }
                        TemplateStringPart::Expr(expr) => {
                            write!(f, "${{{}}}", expr)?;
                        }
                    }
                }
                write!(f, "\"")
            }
            Expr::Closure { params, body } => {
                let params_str = params.join(", ");
                write!(f, "|{}| {}", params_str, body)
            }
            Expr::Match { value, arms } => {
                write!(f, "match {} {{", value)?;
                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} => {}", arm.pattern, arm.body)?;
                }
                write!(f, "}}")
            }
            Expr::Val(val) => write!(f, "{}", val),
        }
    }
}

impl From<Val> for Expr {
    fn from(val: Val) -> Self {
        Expr::Val(val)
    }
}

impl Expr {
    /// 静态类型检查表达式
    pub fn type_check(&self, type_checker: &mut TypeChecker) -> Result<Type> {
        type_checker.check_expr(self)
    }
}
