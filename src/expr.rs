use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use anyhow::{Result, anyhow};

use crate::{
    ast::Parser,
    op::{BinOp, UnaryOp, err_op},
    token::Tokenizer,
    val::Val,
};

/// Grammar:
/// exp     ::= paren
/// paren   ::= {'('} or {')'}
/// or      ::= and {'||' and}
/// and     ::= cmp {'&&' cmp}
/// cmp     ::= addsub {('<' | '>' | '<=' | '>=' | '!=' | '==') addsub}
/// addsub  ::= muldiv {('+' | '-') muldiv}
/// muldiv  ::= unary {('*' | '/' | '%') unary}
/// unary   ::= {'!'} postfix
/// postfix ::= primary {'.' field}
/// primary ::= nil | false | true | int | float | string | at | list | map
/// at      ::= '@' field {'.' field}
/// field   ::= id | int
/// list    ::= '[' [expr {',' expr}] ']'
/// map     ::= '{' [expr ':' expr {',' expr ':' expr}] '}'
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

impl Expr {
    pub fn eval(&self, ctx: &Val) -> Result<Val> {
        match self {
            Expr::Bin(l, op, r) => op.eval(l, r, ctx),
            Expr::Unary(op, expr) => op.eval(expr, ctx),
            Expr::And(e1, e2) => {
                let l = e1.eval(ctx)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(false) = l {
                    return Ok(Val::Bool(false));
                }
                let r = e2.eval(ctx)?;
                match (&l, &r) {
                    (Val::Bool(true), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "&&", &r),
                }
            }
            Expr::Or(e1, e2) => {
                let l = e1.eval(ctx)?;
                // Short-circuit evaluation to improve performance
                if let Val::Bool(true) = l {
                    return Ok(Val::Bool(true));
                }
                let r = e2.eval(ctx)?;
                match (&l, &r) {
                    (Val::Bool(_), Val::Bool(true)) => Ok(Val::Bool(true)),
                    (Val::Bool(_), Val::Bool(_)) => Ok(Val::Bool(false)),
                    _ => err_op(&l, "||", &r),
                }
            }
            Expr::At(paths) => {
                if paths.is_empty() {
                    return Ok(Val::Nil);
                }

                let mut val = ctx;
                for path in paths {
                    val = match val.access(&path.eval(ctx)?) {
                        Some(v) => v,
                        None => return Ok(Val::Nil),
                    }
                }
                // Return a clone only at the end of evaluation to reduce allocations
                Ok(val.clone())
            }
            Expr::Access(expr, field) => {
                let val = expr.eval(ctx)?;
                let field_val = field.eval(ctx)?;
                match val.access(&field_val) {
                    Some(v) => Ok(v.clone()),
                    None => Ok(Val::Nil),
                }
            }
            Expr::List(exprs) => {
                let mut values = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values.push(expr.eval(ctx)?);
                }
                Ok(Val::List(Box::new(values)))
            }
            Expr::Map(pairs) => {
                let mut map = std::collections::HashMap::with_capacity(pairs.len());
                for (key_expr, value_expr) in pairs {
                    let key_val = key_expr.eval(ctx)?;
                    let value_val = value_expr.eval(ctx)?;

                    // Convert key to string for map indexing
                    let key_str = match key_val {
                        Val::Str(s) => s,
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
                Ok(Val::Map(Box::new(map)))
            }
            Expr::Paren(expr) => expr.eval(ctx),
            Expr::Val(val) => Ok(val.clone()), // TODO
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
    /// eg.: `@user.props.(@req.service).value && @list` => `["user", "req", "list"]`
    fn collect_ctx_names(&self, names: &mut HashSet<String>) {
        match self {
            Expr::At(paths) => {
                if !paths.is_empty() {
                    // The first path element is the context name
                    if let Expr::Val(Val::Str(name)) = &*paths[0] {
                        names.insert(name.clone());
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
            Expr::And(l, r) | Expr::Or(l, r) => {
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
            // Only collect string values when they are actual context names, not field names
            Expr::Val(_) => {}
        }
    }
}

impl TryInto<Val> for &Expr {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Val> {
        match self {
            Expr::Val(val) => Ok(val.clone()), // TODO
            _ => {
                let msg = format!("Can't convert Expr::{:?} to Val", self);
                Err(anyhow!(msg))
            }
        }
    }
}

fn into_expr<S: AsRef<str>>(s: S) -> Result<Expr> {
    let tokens = Tokenizer::new(s.as_ref())?;
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
            Expr::And(left, right) => write!(f, "{left} && {right}"),
            Expr::Or(left, right) => write!(f, "{left} || {right}"),
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
                let pairs: Vec<String> =
                    pairs.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
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
