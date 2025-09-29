use crate::{
    expr::Expr,
    op::BinOp,
    stmt::{ImportContext, ImportStmt, ModuleResolver},
    token::{ParseError, Span},
    typ::TypeChecker,
    val::{Type, Val},
};
use anyhow::{Result, anyhow};
use std::{collections::HashMap, fmt::Display, sync::Arc};

/// For 循环的模式匹配 (类似 Rust 的 Pattern)
#[derive(Debug, Clone, PartialEq)]
pub enum ForPattern {
    /// 简单变量绑定：for x in iter
    Variable(String),
    /// 忽略模式：for _ in iter
    Ignore,
    /// 元组解构：for (a, b, c) in iter
    Tuple(Vec<ForPattern>),
    /// 数组解构：for [a, b] in iter
    Array {
        patterns: Vec<ForPattern>,
        rest: Option<String>, // for [a, b, ..rest] or [a, b, ..]
    },
    /// 对象解构：for {"k1": v1, "k2": v2} in iter
    /// 仅支持字符串字面量作为键，值位置可以是变量或更深的模式（递归支持）
    Object(Vec<(String, ForPattern)>),
}

/// Statement AST 节点类型定义
///
/// 语法设计：
/// program  ::= statement*
/// statement ::= import_stmt | if_stmt | while_stmt | let_stmt | assign_stmt | break_stmt | continue_stmt | return_stmt | fn_stmt | expr_stmt | block_stmt
/// import_stmt ::= 'import' import_spec ';'
/// if_stmt  ::= 'if' '(' expr ')' statement ['else' statement]
/// while_stmt ::= 'while' '(' expr ')' statement
/// let_stmt ::= 'let' id [':' type] '=' expr ';'
/// assign_stmt ::= id '=' expr ';'
/// break_stmt ::= 'break' ';'
/// continue_stmt ::= 'continue' ';'
/// return_stmt ::= 'return' [expr] ';'
/// fn_stmt ::= 'fn' id '(' [id {',' id}] ')' block_stmt
/// expr_stmt ::= expr ';'
/// block_stmt ::= '{' statement* '}'
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// import statement
    Import(ImportStmt),
    /// if (condition) then_stmt [else else_stmt]
    If {
        condition: Box<Expr>,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    /// if let pattern = expression { then_stmt } [else else_stmt]
    IfLet {
        pattern: crate::expr::Pattern,
        value: Box<Expr>,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    /// while (condition) body
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    /// while let pattern = expression { body }
    WhileLet {
        pattern: crate::expr::Pattern,
        value: Box<Expr>,
        body: Box<Stmt>,
    },
    /// for pattern in iterable { body }
    For {
        pattern: ForPattern,
        iterable: Box<Expr>,
        body: Box<Stmt>,
    },
    /// let pattern [: type] = value; (supports both single variables and destructuring patterns)
    Let {
        pattern: crate::expr::Pattern,
        type_annotation: Option<Type>,
        value: Box<Expr>,
        span: Option<Span>,
    },
    /// name = value; (赋值语句)
    Assign {
        name: String,
        value: Box<Expr>,
        span: Option<Span>,
    },
    /// name op= value; (复合赋值语句, 如 x += 5)
    CompoundAssign {
        name: String,
        op: BinOp,
        value: Box<Expr>,
        span: Option<Span>,
    },
    /// name = value; (变量定义，类似 Go 的短声明)
    Define { name: String, value: Box<Expr> },
    /// break;
    Break,
    /// continue;
    Continue,
    /// return [expression];
    Return { value: Option<Box<Expr>> },
    /// fn name(param1[: type], ...) [-> type] { body }
    Function {
        name: String,
        params: Vec<String>,
        /// Parameter types aligned with params; None when unannotated
        param_types: Vec<Option<Type>>,
        /// Optional declared return type
        return_type: Option<Type>,
        body: Box<Stmt>,
    },
    /// expression;
    Expr(Box<Expr>),
    /// { statements }
    Block { statements: Vec<Box<Stmt>> },
    /// 空语句 (用于处理解析时的占位)
    Empty,
}

/// 控制流状态，用于 break/continue 的实现
#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    /// 正常执行
    None,
    /// break 语句
    Break,
    /// continue 语句  
    Continue,
    /// 函数返回 (预留给未来功能)
    Return(Val),
}

/// 变量作用域管理
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    /// 变量存储栈，每个作用域对应一个 HashMap
    scopes: Vec<HashMap<String, Val>>,
    /// Import context for managing imported symbols
    import_ctx: ImportContext,
    /// Module resolver (shared across all environments)
    resolver: Arc<ModuleResolver>,
    /// Type checker for static type analysis
    type_checker: Option<TypeChecker>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // 全局作用域
            import_ctx: ImportContext::new(),
            resolver: Arc::new(ModuleResolver::new()),
            type_checker: None,
        }
    }

    pub fn with_resolver(resolver: Arc<ModuleResolver>) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            import_ctx: ImportContext::new(),
            resolver,
            type_checker: None,
        }
    }

    pub fn with_type_checker(mut self, type_checker: TypeChecker) -> Self {
        self.type_checker = Some(type_checker);
        self
    }

    pub fn get_type_checker(&self) -> Option<&TypeChecker> {
        self.type_checker.as_ref()
    }

    pub fn get_type_checker_mut(&mut self) -> Option<&mut TypeChecker> {
        self.type_checker.as_mut()
    }

    /// 导出当前全局作用域的符号（用于模块导出）
    pub fn export_symbols(&self) -> HashMap<String, Val> {
        // Clone the top-level scope only; imported symbols are not re-exported by default
        self.scopes.first().cloned().unwrap_or_else(HashMap::new)
    }

    /// 进入新的作用域
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// 退出当前作用域
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// 定义变量 (在当前作用域中)
    pub fn define(&mut self, name: String, value: Val) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, value);
        }
    }

    /// 赋值变量 (在最近的包含该变量的作用域中)
    pub fn assign(&mut self, name: &str, value: Val) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }
        Err(anyhow!("Undefined variable: {}", name))
    }

    /// 获取变量值
    pub fn get(&self, name: &str) -> Option<&Val> {
        // Check local scopes first
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        // Check imported symbols
        if let Some(v) = self.import_ctx.get_symbol(name) {
            return Some(v);
        }

        // Check globally registered builtin functions in the resolver's registry
        self.resolver.get_builtin(name)
    }

    /// Execute import statement
    pub fn execute_import(&mut self, import: &ImportStmt) -> Result<()> {
        self.import_ctx.execute_import(import, &self.resolver)
    }
}

/// 语句执行引擎
impl Stmt {
    /// 执行语句，返回控制流状态
    pub fn execute(&self, env: &mut Environment, ctx: &Val) -> Result<ControlFlow> {
        match self {
            Stmt::Import(import_stmt) => {
                env.execute_import(import_stmt)?;
                Ok(ControlFlow::None)
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let cond_val = condition.eval_with_env(ctx, Some(env))?;
                let is_true = match cond_val {
                    Val::Bool(b) => b,
                    Val::Nil => false,
                    _ => true, // 非 nil 和 false 的值都视为真
                };

                if is_true {
                    then_stmt.execute(env, ctx)
                } else if let Some(else_stmt) = else_stmt {
                    else_stmt.execute(env, ctx)
                } else {
                    Ok(ControlFlow::None)
                }
            }
            Stmt::IfLet {
                pattern,
                value,
                then_stmt,
                else_stmt,
            } => {
                // 求值表达式
                let val = value.eval_with_env(ctx, Some(env))?;

                // 尝试模式匹配
                env.push_scope(); // 为模式变量绑定创建新作用域
                let match_result = pattern.matches(&val, ctx, Some(env))?;

                if let Some(bindings) = match_result {
                    // 绑定变量到环境
                    for (name, val) in bindings {
                        env.define(name, val);
                    }

                    // 执行then分支
                    let result = then_stmt.execute(env, ctx);
                    env.pop_scope();
                    result
                } else {
                    env.pop_scope(); // 清理未使用的作用域

                    // 执行else分支（如果有）
                    if let Some(else_stmt) = else_stmt {
                        else_stmt.execute(env, ctx)
                    } else {
                        Ok(ControlFlow::None)
                    }
                }
            }
            Stmt::While { condition, body } => {
                loop {
                    let cond_val = condition.eval_with_env(ctx, Some(env))?;
                    let is_true = match cond_val {
                        Val::Bool(b) => b,
                        Val::Nil => false,
                        _ => true,
                    };

                    if !is_true {
                        break;
                    }

                    match body.execute(env, ctx)? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::None => {}
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::WhileLet {
                pattern,
                value,
                body,
            } => {
                loop {
                    // 求值表达式
                    let val = value.eval_with_env(ctx, Some(env))?;

                    // 在 while let/if let 语境下，变量模式不匹配 nil
                    if let crate::expr::Pattern::Variable(_) = pattern
                        && val == Val::Nil
                    {
                        break;
                    }

                    // 对列表前缀匹配做放宽：允许 [a] 匹配 [a, b, ...]
                    // 同时记录是否为“放宽”匹配，以便在循环体执行后，
                    // 如果 value 是变量，则将其自动推进到 rest（与显式 [a, ..rest] 写法一致）。
                    let (pattern_for_match, prefix_relaxed) = match pattern {
                        crate::expr::Pattern::List { patterns, rest } if rest.is_none() => (
                            crate::expr::Pattern::List {
                                patterns: patterns.clone(),
                                rest: Some("__whilelet_rest".to_string()),
                            },
                            true,
                        ),
                        _ => (pattern.clone(), false),
                    };

                    // 如果 value 是形如 x[0] 的访问表达式，则在匹配失败时尝试“向前推进” x，
                    // 即将 x 赋值为其余切片 (从索引 1 开始)。这样可支持诸如
                    // `while let val if guard = x[0] { ... x = [x[1]]; }` 的按需过滤场景。
                    let scan_head_var: Option<String> = match value.as_ref() {
                        crate::expr::Expr::Access(obj, field) => {
                            match (obj.as_ref(), field.as_ref()) {
                                (
                                    crate::expr::Expr::Var(name),
                                    crate::expr::Expr::Val(Val::Int(i)),
                                ) if *i == 0 => Some(name.clone()),
                                _ => None,
                            }
                        }
                        _ => None,
                    };

                    // 尝试模式匹配
                    env.push_scope(); // 为模式变量绑定创建新作用域
                    let match_result = pattern_for_match.matches(&val, ctx, Some(env))?;

                    if let Some(bindings) = match_result {
                        // 模式匹配成功，绑定变量到环境
                        for (name, val) in bindings {
                            env.define(name, val);
                        }

                        // 如果是前缀放宽匹配，且 value 是变量，记录 rest 以便在循环体执行后推进该变量
                        let mut pending_prefix_advance: Option<(String, Val)> = None;
                        if prefix_relaxed
                            && let crate::expr::Expr::Var(var_name) = value.as_ref()
                            && let Some(rest_val) = env.get("__whilelet_rest").cloned()
                        {
                            pending_prefix_advance = Some((var_name.clone(), rest_val));
                        }

                        // 执行循环体
                        let exec_result = body.execute(env, ctx)?;

                        match exec_result {
                            ControlFlow::Break => {
                                env.pop_scope();
                                break;
                            }
                            ControlFlow::Continue => {
                                // 对于 continue，若存在需要的前缀推进，则在退出子作用域前记录，
                                // 并在退出后应用到外层环境。
                                let advance = pending_prefix_advance.clone();
                                env.pop_scope();
                                if let Some((name, rest_val)) = advance {
                                    let _ = env.assign(&name, rest_val); // 忽略错误：若变量已被覆盖则不影响
                                }
                                continue;
                            }
                            ControlFlow::Return(val) => {
                                env.pop_scope();
                                return Ok(ControlFlow::Return(val));
                            }
                            ControlFlow::None => {
                                // 正常执行结束，若存在需要的前缀推进，则在退出子作用域后应用
                                let advance = pending_prefix_advance.clone();
                                env.pop_scope();
                                if let Some((name, rest_val)) = advance {
                                    let _ = env.assign(&name, rest_val);
                                }
                            }
                        }
                    } else {
                        // 模式匹配失败，退出循环
                        env.pop_scope();

                        // 针对 x[0] 的场景：在不匹配时尝试推进 x 到其余切片后继续尝试
                        if let Some(var_name) = scan_head_var
                            && let Some(current) = env.get(&var_name).cloned()
                            && let Val::List(list) = current
                            && !list.is_empty()
                        {
                            let tail: Vec<Val> = list.iter().skip(1).cloned().collect();
                            let _ = env.assign(&var_name, Val::List(Arc::new(tail)));
                            // 不立即 break，继续下一轮尝试
                            continue;
                        }

                        break;
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::For {
                pattern,
                iterable,
                body,
            } => {
                // 求值可迭代表达式
                let iter_val = iterable.eval_with_env(ctx, Some(env))?;

                // 获取迭代器
                let iterator = create_iterator(&iter_val)?;

                // 执行循环
                for item in iterator {
                    // 进入新作用域用于模式绑定
                    env.push_scope();

                    // 根据模式绑定变量 (类似 Rust 的模式匹配)
                    if let Err(e) = bind_pattern(pattern, &item, env) {
                        env.pop_scope(); // 清理作用域
                        return Err(e);
                    }

                    // 执行循环体
                    let result = body.execute(env, ctx);
                    env.pop_scope(); // 清理循环变量作用域

                    match result? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::None => {}
                    }
                }

                Ok(ControlFlow::None)
            }
            Stmt::Let {
                pattern,
                type_annotation,
                value,
                span,
            } => {
                let val = value.eval_with_env(ctx, Some(env))?;

                // Validate type annotation if provided
                if let Some(expected_type) = type_annotation
                    && let Err(_e) = expected_type.validate(&val)
                {
                    let error_msg = format!(
                        "Type mismatch in pattern: expected {}, got {}",
                        expected_type.display(),
                        val.type_name()
                    );
                    return if let Some(span) = span {
                        Err(anyhow::anyhow!(ParseError::with_span(
                            error_msg,
                            span.clone()
                        )))
                    } else {
                        Err(anyhow::anyhow!(error_msg))
                    };
                }

                // Handle pattern matching and variable binding
                let bindings = match pattern.matches(&val, ctx, Some(env))? {
                    Some(bindings) => bindings,
                    None => {
                        let error_msg = format!(
                            "Pattern does not match value: {} does not match {}",
                            pattern, val
                        );
                        return if let Some(span) = span {
                            Err(anyhow::anyhow!(ParseError::with_span(
                                error_msg,
                                span.clone()
                            )))
                        } else {
                            Err(anyhow::anyhow!(error_msg))
                        };
                    }
                };

                // Bind all pattern variables
                for (name, bound_val) in bindings {
                    env.define(name, bound_val);
                }
                Ok(ControlFlow::None)
            }
            Stmt::Assign {
                name,
                value,
                span: _,
            } => {
                let val = value.eval_with_env(ctx, Some(env))?;
                env.assign(name, val)?;
                Ok(ControlFlow::None)
            }
            Stmt::CompoundAssign {
                name,
                op,
                value,
                span: _,
            } => {
                // Get current value of variable
                let current_val = env.get(name).cloned().ok_or_else(|| {
                    anyhow!("Undefined variable for compound assignment: {}", name)
                })?;

                // Evaluate the right-hand side
                let rhs_val = value.eval_with_env(ctx, Some(env))?;

                // Perform the operation
                let result_val = op.eval_vals(&current_val, &rhs_val)?;

                // Assign the result back
                env.assign(name, result_val)?;
                Ok(ControlFlow::None)
            }
            Stmt::Define { name, value } => {
                let val = value.eval_with_env(ctx, Some(env))?;
                env.define(name.clone(), val);
                Ok(ControlFlow::None)
            }
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
            Stmt::Return { value } => {
                let return_val = if let Some(expr) = value {
                    expr.eval_with_env(ctx, Some(env))?
                } else {
                    Val::Nil
                };
                Ok(ControlFlow::Return(return_val))
            }
            Stmt::Function {
                name,
                params,
                param_types: _,
                return_type: _,
                body,
            } => {
                let func_val = Val::Closure {
                    params: Arc::new(params.clone()),
                    body: Arc::new((**body).clone()),
                    env: Arc::new(env.clone()),
                };
                env.define(name.clone(), func_val);
                Ok(ControlFlow::None)
            }
            Stmt::Expr(expr) => {
                // 执行表达式
                let value = expr.eval_with_env(ctx, Some(env))?;

                // Heuristic: if this is a method call like `var.method(...)` and
                // the method is known to return an updated receiver (e.g. List.push),
                // assign the result back to the variable to simulate mutating methods.
                if let crate::expr::Expr::CallExpr(callee, _args) = expr.as_ref()
                    && let crate::expr::Expr::Access(obj_expr, field_expr) = callee.as_ref()
                    && let crate::expr::Expr::Var(var_name) = obj_expr.as_ref()
                    && let crate::expr::Expr::Val(Val::Str(method)) = field_expr.as_ref()
                    && method.as_ref() == "push"
                {
                    // For now, treat 'push' as mutating for Lists
                    let _ = env.assign(var_name, value.clone());
                }

                Ok(ControlFlow::None)
            }
            Stmt::Block { statements } => {
                env.push_scope();
                let mut result = ControlFlow::None;

                for stmt in statements {
                    match stmt.execute(env, ctx)? {
                        ControlFlow::None => {}
                        other => {
                            result = other;
                            break;
                        }
                    }
                }

                env.pop_scope();
                Ok(result)
            }
            Stmt::Empty => Ok(ControlFlow::None),
        }
    }

    /// 静态类型检查语句
    pub fn type_check(&self, type_checker: &mut TypeChecker) -> Result<()> {
        match self {
            Stmt::Let {
                pattern,
                type_annotation,
                value,
                span,
            } => {
                // 检查表达式的类型
                let expr_type = value.type_check(type_checker)?;

                // 如果有类型注解，验证类型匹配
                if let Some(expected_type) = type_annotation
                    && !expr_type.is_assignable_to(expected_type)
                {
                    let error_msg = format!(
                        "Type mismatch in let statement: pattern expected type {}, but expression has type {}",
                        expected_type.display(),
                        expr_type.display()
                    );
                    return if let Some(span) = span {
                        Err(anyhow::anyhow!(ParseError::with_span(
                            error_msg,
                            span.clone()
                        )))
                    } else {
                        Err(anyhow::anyhow!(error_msg))
                    };
                }

                // Extract variables from pattern and add their types to the type checker
                if let Some(pattern_vars) = extract_pattern_variables(pattern) {
                    let var_type = type_annotation.clone().unwrap_or(expr_type);
                    for var_name in pattern_vars {
                        type_checker.add_local_type(var_name, var_type.clone());
                    }
                }

                Ok(())
            }
            Stmt::Assign { name, value, span } => {
                // 检查表达式的类型
                let expr_type = value.type_check(type_checker)?;

                // 获取变量的已声明类型
                if let Some(var_type) = type_checker.get_local_type(name) {
                    if !expr_type.is_assignable_to(var_type) {
                        let error_msg = format!(
                            "Type mismatch in assignment: variable '{}' has type {}, but assigned expression has type {}",
                            name,
                            var_type.display(),
                            expr_type.display()
                        );
                        return if let Some(span) = span {
                            Err(anyhow::anyhow!(ParseError::with_span(
                                error_msg,
                                span.clone()
                            )))
                        } else {
                            Err(anyhow::anyhow!(error_msg))
                        };
                    }
                } else {
                    return Err(anyhow::anyhow!(
                        "Cannot assign to undefined variable '{}'",
                        name
                    ));
                }

                Ok(())
            }
            Stmt::CompoundAssign {
                name,
                op: _,
                value,
                span,
            } => {
                // 检查表达式的类型
                let expr_type = value.type_check(type_checker)?;

                // 获取变量的已声明类型
                if let Some(var_type) = type_checker.get_local_type(name) {
                    // 检查操作类型兼容性 (var_type op expr_type -> var_type)
                    // 简化：假设所有算术操作都是类型兼容的
                    if !expr_type.is_assignable_to(var_type)
                        && !var_type.is_assignable_to(&expr_type)
                    {
                        let error_msg = format!(
                            "Type mismatch in compound assignment: variable '{}' has type {}, but right-hand side has type {}",
                            name,
                            var_type.display(),
                            expr_type.display()
                        );
                        return if let Some(span) = span {
                            Err(anyhow::anyhow!(ParseError::with_span(
                                error_msg,
                                span.clone()
                            )))
                        } else {
                            Err(anyhow::anyhow!(error_msg))
                        };
                    }
                } else {
                    return Err(anyhow::anyhow!(
                        "Cannot compound assign to undefined variable '{}'",
                        name
                    ));
                }

                Ok(())
            }
            Stmt::Function {
                name,
                params,
                param_types,
                return_type,
                body,
            } => {
                // 为函数参数创建新的作用域
                type_checker.push_scope();

                // 将参数类型加入作用域（默认 Any；有注解用注解）
                for (i, param) in params.iter().enumerate() {
                    let ty = param_types.get(i).cloned().flatten().unwrap_or(Type::Any);
                    type_checker.add_local_type(param.clone(), ty);
                }

                // 检查函数体
                body.type_check(type_checker)?;

                // 弹出参数作用域
                type_checker.pop_scope();

                // 推断函数返回类型（若未显式注解）。策略：
                // - 收集所有显式 return 语句的表达式类型，合并为去重后的并集。
                // - 如果函数体内没有显式 return，则返回类型推断为 Nil（运行时默认返回）。
                fn collect_return_types(
                    stmt: &Stmt,
                    tc: &mut TypeChecker,
                    out: &mut Vec<Type>,
                ) -> anyhow::Result<()> {
                    match stmt {
                        Stmt::Return { value } => {
                            if let Some(expr) = value {
                                let ty = expr.type_check(tc)?;
                                out.push(ty);
                            } else {
                                out.push(Type::Nil);
                            }
                        }
                        Stmt::If {
                            condition,
                            then_stmt,
                            else_stmt,
                        } => {
                            // 先检查条件表达式，保证条件类型合法
                            let _ = condition.type_check(tc)?;
                            collect_return_types(then_stmt, tc, out)?;
                            if let Some(es) = else_stmt.as_deref() {
                                collect_return_types(es, tc, out)?;
                            }
                        }
                        Stmt::IfLet {
                            then_stmt,
                            else_stmt,
                            value: _,
                            pattern: _,
                        } => {
                            collect_return_types(then_stmt, tc, out)?;
                            if let Some(es) = else_stmt.as_deref() {
                                collect_return_types(es, tc, out)?;
                            }
                        }
                        Stmt::While { condition, body } => {
                            let _ = condition.type_check(tc)?;
                            collect_return_types(body, tc, out)?;
                        }
                        Stmt::WhileLet { body, .. } => {
                            collect_return_types(body, tc, out)?;
                        }
                        Stmt::For { body, .. } => {
                            collect_return_types(body, tc, out)?;
                        }
                        Stmt::Block { statements } => {
                            for s in statements {
                                collect_return_types(s, tc, out)?;
                            }
                        }
                        // 不应深入到嵌套函数内的 return
                        Stmt::Function { .. } => {}
                        // 其余语句不包含返回
                        _ => {}
                    }
                    Ok(())
                }

                fn normalize_union(mut tys: Vec<Type>) -> Type {
                    // 展开并去重（按 display 排序保证稳定）
                    let mut flat: Vec<Type> = Vec::new();
                    for t in tys.drain(..) {
                        match t {
                            Type::Union(inner) => flat.extend(inner),
                            other => flat.push(other),
                        }
                    }
                    use std::collections::BTreeMap;
                    let mut by_key: BTreeMap<String, Type> = BTreeMap::new();
                    for t in flat {
                        by_key.entry(t.display()).or_insert(t);
                    }
                    let mut uniq: Vec<Type> = by_key.into_values().collect();
                    if uniq.len() == 1 {
                        uniq.remove(0)
                    } else {
                        Type::Union(uniq)
                    }
                }

                let inferred_return: Type = if return_type.is_none() {
                    let mut rtys: Vec<Type> = Vec::new();
                    collect_return_types(body, type_checker, &mut rtys)?;
                    if rtys.is_empty() {
                        Type::Nil
                    } else {
                        normalize_union(rtys)
                    }
                } else {
                    // 已显式注解则直接使用
                    return_type.clone().unwrap()
                };

                // 将函数添加到当前作用域，类型为 Function
                let func_type = Type::Function {
                    params: params
                        .iter()
                        .enumerate()
                        .map(|(i, _)| param_types.get(i).cloned().flatten().unwrap_or(Type::Any))
                        .collect(),
                    return_type: Box::new(inferred_return),
                };
                type_checker.add_local_type(name.clone(), func_type);

                Ok(())
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                // 条件表达式必须是 Bool 类型
                let cond_type = condition.type_check(type_checker)?;
                if !cond_type.is_assignable_to(&Type::Bool) {
                    return Err(anyhow::anyhow!(
                        "If condition must be Bool, but got {}",
                        cond_type.display()
                    ));
                }

                // 检查 then 和 else 分支
                then_stmt.type_check(type_checker)?;
                if let Some(else_stmt) = else_stmt {
                    else_stmt.type_check(type_checker)?;
                }

                Ok(())
            }
            Stmt::IfLet {
                pattern: _,
                value,
                then_stmt,
                else_stmt,
            } => {
                // 检查值表达式的类型
                let _value_type = value.type_check(type_checker)?;

                // 为 then 分支创建新作用域，以便模式变量绑定
                type_checker.push_scope();

                // TODO: 根据模式添加变量类型，这需要更复杂的模式类型推导
                // 现在简化为检查 then 分支
                then_stmt.type_check(type_checker)?;

                // 弹出作用域
                type_checker.pop_scope();

                // 检查 else 分支（如果有）
                if let Some(else_stmt) = else_stmt {
                    else_stmt.type_check(type_checker)?;
                }

                Ok(())
            }
            Stmt::While { condition, body } => {
                // 条件表达式必须是 Bool 类型
                let cond_type = condition.type_check(type_checker)?;
                if !cond_type.is_assignable_to(&Type::Bool) {
                    return Err(anyhow::anyhow!(
                        "While condition must be Bool, but got {}",
                        cond_type.display()
                    ));
                }

                // 检查循环体
                body.type_check(type_checker)?;

                Ok(())
            }
            Stmt::WhileLet {
                pattern: _,
                value,
                body,
            } => {
                // 检查值表达式的类型
                let _value_type = value.type_check(type_checker)?;

                // 为循环体创建新作用域，以便模式变量绑定
                type_checker.push_scope();

                // TODO: 根据模式添加变量类型，这需要更复杂的模式类型推导
                // 现在简化为检查循环体
                body.type_check(type_checker)?;

                // 弹出作用域
                type_checker.pop_scope();

                Ok(())
            }
            Stmt::For {
                pattern,
                iterable,
                body,
            } => {
                // 检查可迭代表达式的类型
                let iter_type = iterable.type_check(type_checker)?;

                // 验证可迭代类型
                match iter_type {
                    Type::List(_) | Type::String | Type::Map(_, _) => {
                        // 这些类型都是可迭代的
                    }
                    _ => {
                        return Err(anyhow::anyhow!(
                            "For loop iterable must be List, String, or Map, but got {}",
                            iter_type.display()
                        ));
                    }
                }

                // 为模式匹配创建新的作用域
                type_checker.push_scope();

                // 根据模式添加变量类型
                Self::add_pattern_types(pattern, &iter_type, type_checker)?;

                // 检查循环体
                body.type_check(type_checker)?;

                // 弹出作用域
                type_checker.pop_scope();

                Ok(())
            }
            Stmt::Expr(expr) => {
                // 表达式语句，只检查类型，不使用结果
                expr.type_check(type_checker)?;
                Ok(())
            }
            Stmt::Block { statements } => {
                // 为块语句创建新的作用域
                type_checker.push_scope();

                // 检查块中的所有语句
                for stmt in statements {
                    stmt.type_check(type_checker)?;
                }

                // 弹出作用域
                type_checker.pop_scope();

                Ok(())
            }
            Stmt::Import(_) => {
                // Import 语句暂时不需要类型检查
                Ok(())
            }
            Stmt::Break | Stmt::Continue | Stmt::Return { .. } => {
                // 控制流语句暂时不需要类型检查
                Ok(())
            }
            Stmt::Define { .. } | Stmt::Empty => {
                // Define 语句和空语句暂时不需要类型检查
                Ok(())
            }
        }
    }

    /// 为 for 循环模式添加类型信息
    fn add_pattern_types(
        pattern: &ForPattern,
        iter_type: &Type,
        type_checker: &mut TypeChecker,
    ) -> Result<()> {
        match pattern {
            ForPattern::Variable(name) => {
                // 根据可迭代类型确定变量类型
                let var_type = match iter_type {
                    Type::List(inner) => (**inner).clone(),
                    Type::String => Type::String,
                    Type::Map(_, _) => {
                        // Map 迭代返回 [key, value] 对
                        Type::List(Box::new(Type::Union(vec![
                            Type::String, // key
                            Type::Any,    // value
                        ])))
                    }
                    _ => Type::Any,
                };
                type_checker.add_local_type(name.clone(), var_type);
            }
            ForPattern::Ignore => {
                // 忽略模式，不需要添加类型
            }
            ForPattern::Tuple(patterns) => {
                if let Type::List(inner_types) = iter_type {
                    // 这里简化处理，假设 inner_types 是一个包含所有元素类型的 List
                    // 实际上可能需要更复杂的类型推导
                    for pattern in patterns {
                        Self::add_pattern_types(pattern, inner_types, type_checker)?;
                    }
                }
            }
            ForPattern::Array { patterns, rest } => {
                if let Type::List(inner_types) = iter_type {
                    // 为固定模式的每个部分添加类型
                    for pattern in patterns {
                        Self::add_pattern_types(pattern, inner_types, type_checker)?;
                    }

                    // 为剩余模式添加类型
                    if let Some(rest_var) = rest {
                        type_checker.add_local_type(rest_var.clone(), (**inner_types).clone());
                    }
                }
            }
            ForPattern::Object(entries) => {
                // 目前仅支持元素为 Map<K, V> 的列表：List<Map<K,V>>
                // 将每个绑定变量加入作用域，类型为 V（未知则 Any）
                let value_ty = match iter_type {
                    Type::List(inner) => match &**inner {
                        Type::Map(_k, v) => Some((**v).clone()),
                        _ => None,
                    },
                    // 直接迭代 Map 时 create_iterator 产生 [key,value] 对，不适配对象解构
                    _ => None,
                }
                .unwrap_or(Type::Any);

                for (_key, subpat) in entries {
                    match subpat {
                        ForPattern::Variable(name) => {
                            type_checker.add_local_type(name.clone(), value_ty.clone());
                        }
                        ForPattern::Ignore => {}
                        // 对于嵌套模式，保守地继续使用相同的 value_ty
                        other => {
                            Self::add_pattern_types(other, &value_ty, type_checker)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

/// 程序结构 - 包含语句列表
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Box<Stmt>>,
}

impl Program {
    pub fn new(statements: Vec<Box<Stmt>>) -> Result<Self> {
        Ok(Program { statements })
    }

    /// 类型检查程序
    pub fn type_check(&self, type_checker: &mut TypeChecker) -> Result<()> {
        for stmt in &self.statements {
            stmt.type_check(type_checker)?;
        }
        Ok(())
    }

    /// 执行程序
    pub fn execute(&self, ctx: &Val) -> Result<Val> {
        let mut env = Environment::new();
        self.execute_with_env(ctx, &mut env)
    }

    /// 执行程序，使用指定的环境
    pub fn execute_with_env(&self, ctx: &Val, env: &mut Environment) -> Result<Val> {
        let mut pc = 0; // 程序计数器

        while pc < self.statements.len() {
            match self.statements[pc].execute(env, ctx)? {
                ControlFlow::None => {
                    pc += 1;
                }
                ControlFlow::Break => {
                    return Err(anyhow!(
                        "break statement outside of loop at statement {}",
                        pc
                    ));
                }
                ControlFlow::Continue => {
                    return Err(anyhow!(
                        "continue statement outside of loop at statement {}",
                        pc
                    ));
                }
                ControlFlow::Return(val) => {
                    return Ok(val);
                }
            }
        }

        // 程序正常结束，返回 nil
        Ok(Val::Nil)
    }
}

/// 从值创建迭代器
fn create_iterator(val: &Val) -> Result<Vec<Val>> {
    match val {
        Val::List(list) => Ok((**list).clone()),
        Val::Map(map) => {
            // 返回 [key, value] 对的迭代器
            let pairs: Vec<Val> = map
                .iter()
                .map(|(k, v)| Val::List(vec![Val::Str(k.clone().into()), v.clone()].into()))
                .collect();
            Ok(pairs)
        }
        Val::Str(s) => {
            // 按字符迭代
            let chars: Vec<Val> = s.chars().map(|c| Val::Str(c.to_string().into())).collect();
            Ok(chars)
        }
        _ => Err(anyhow!("Value is not iterable: {:?}", val)),
    }
}

/// 模式匹配绑定函数 (类似 Rust 的模式匹配语义)
fn bind_pattern(pattern: &ForPattern, value: &Val, env: &mut Environment) -> Result<()> {
    match pattern {
        ForPattern::Variable(name) => {
            env.define(name.clone(), value.clone());
            Ok(())
        }
        ForPattern::Ignore => {
            // _ 模式不绑定任何变量
            Ok(())
        }
        ForPattern::Tuple(patterns) => match value {
            Val::List(list) => {
                if patterns.len() != list.len() {
                    return Err(anyhow!(
                        "Tuple pattern length mismatch: expected {}, got {}",
                        patterns.len(),
                        list.len()
                    ));
                }
                for (pattern, val) in patterns.iter().zip(list.iter()) {
                    bind_pattern(pattern, val, env)?;
                }
                Ok(())
            }
            _ => Err(anyhow!(
                "Cannot match tuple pattern against non-list value: {:?}",
                value
            )),
        },
        ForPattern::Array { patterns, rest } => match value {
            Val::List(list) => {
                // 检查最小长度要求
                if list.len() < patterns.len() {
                    return Err(anyhow!(
                        "Array too short: expected at least {}, got {}",
                        patterns.len(),
                        list.len()
                    ));
                }

                // 绑定前面的固定模式
                for (i, pattern) in patterns.iter().enumerate() {
                    bind_pattern(pattern, &list[i], env)?;
                }

                // 处理剩余模式 (如果有)
                if let Some(rest_var) = rest {
                    let rest_values: Vec<Val> = list[patterns.len()..].to_vec();
                    env.define(rest_var.clone(), Val::List(rest_values.into()));
                }

                Ok(())
            }
            _ => Err(anyhow!(
                "Cannot match array pattern against non-list value: {:?}",
                value
            )),
        },
        ForPattern::Object(entries) => match value {
            Val::Map(map) => {
                for (key, subpat) in entries {
                    if let Some(v) = map.get(key) {
                        bind_pattern(subpat, v, env)?;
                    } else {
                        return Err(anyhow!("Missing key '{}' in object pattern", key));
                    }
                }
                Ok(())
            }
            _ => Err(anyhow!(
                "Cannot match object pattern against non-map value: {:?}",
                value
            )),
        },
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Import(import_stmt) => {
                write!(f, "{};", format_import_stmt(import_stmt))
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                if let Some(else_stmt) = else_stmt {
                    write!(f, "if ({}) {} else {}", condition, then_stmt, else_stmt)
                } else {
                    write!(f, "if ({}) {}", condition, then_stmt)
                }
            }
            Stmt::IfLet {
                pattern,
                value,
                then_stmt,
                else_stmt,
            } => {
                if let Some(else_stmt) = else_stmt {
                    write!(
                        f,
                        "if let {} = {} {} else {}",
                        pattern, value, then_stmt, else_stmt
                    )
                } else {
                    write!(f, "if let {} = {} {}", pattern, value, then_stmt)
                }
            }
            Stmt::While { condition, body } => {
                write!(f, "while ({}) {}", condition, body)
            }
            Stmt::WhileLet {
                pattern,
                value,
                body,
            } => {
                write!(f, "while let {} = {} {}", pattern, value, body)
            }
            Stmt::For {
                pattern,
                iterable,
                body,
            } => {
                write!(
                    f,
                    "for {} in {} {}",
                    format_pattern(pattern),
                    iterable,
                    body
                )
            }
            Stmt::Let {
                pattern,
                type_annotation,
                value,
                span: _,
            } => {
                if let Some(typ) = type_annotation {
                    write!(f, "let {}: {:?} = {};", pattern, typ, value)
                } else {
                    write!(f, "let {} = {};", pattern, value)
                }
            }
            Stmt::Assign {
                name,
                value,
                span: _,
            } => {
                write!(f, "{} = {};", name, value)
            }
            Stmt::CompoundAssign {
                name,
                op,
                value,
                span: _,
            } => {
                let op_str = match op {
                    BinOp::Add => "+=",
                    BinOp::Sub => "-=",
                    BinOp::Mul => "*=",
                    BinOp::Div => "/=",
                    BinOp::Mod => "%=",
                    _ => "?=", // Should not happen for compound assignment
                };
                write!(f, "{} {} {};", name, op_str, value)
            }
            Stmt::Define { name, value } => {
                write!(f, "{} = {};", name, value)
            }
            Stmt::Break => {
                write!(f, "break;")
            }
            Stmt::Continue => {
                write!(f, "continue;")
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    write!(f, "return {};", expr)
                } else {
                    write!(f, "return;")
                }
            }
            Stmt::Function {
                name,
                params,
                param_types,
                return_type,
                body,
            } => {
                // Format parameters with optional types
                let parts: Vec<String> = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| match param_types.get(i).and_then(|t| t.clone()) {
                        Some(ty) => format!("{}: {}", p, ty.display()),
                        None => p.clone(),
                    })
                    .collect();
                // Format return type and elide full body to avoid huge/recursive prints
                let body_summary = if let Stmt::Block { statements } = &**body {
                    format!("... ({} statements) ...", statements.len())
                } else {
                    "...".to_string()
                };
                if let Some(rt) = return_type {
                    write!(
                        f,
                        "fn {}({}) -> {} {{ {} }}",
                        name,
                        parts.join(", "),
                        rt.display(),
                        body_summary
                    )
                } else {
                    write!(
                        f,
                        "fn {}({}) {{ {} }}",
                        name,
                        parts.join(", "),
                        body_summary
                    )
                }
            }
            Stmt::Expr(expr) => {
                write!(f, "{};", expr)
            }
            Stmt::Block { statements } => {
                writeln!(f, "{{")?;
                for stmt in statements {
                    writeln!(f, "  {}", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Empty => {
                write!(f, ";")
            }
        }
    }
}

/// Helper function to format import statements for display
fn format_import_stmt(import: &ImportStmt) -> String {
    use crate::stmt::{ImportSource, ImportStmt};

    match import {
        ImportStmt::Module { module } => {
            format!("import {}", module)
        }
        ImportStmt::File { path } => {
            format!("import \"{}\"", path)
        }
        ImportStmt::Items { items, source } => {
            let items_str = items
                .iter()
                .map(|item| {
                    if let Some(alias) = &item.alias {
                        format!("{} as {}", item.name, alias)
                    } else {
                        item.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");

            let source_str = match source {
                ImportSource::Module(name) => name.clone(),
                ImportSource::File(path) => format!("\"{}\"", path),
            };

            format!("import {{ {} }} from {}", items_str, source_str)
        }
        ImportStmt::Namespace { alias, source } => {
            let source_str = match source {
                ImportSource::Module(name) => name.clone(),
                ImportSource::File(path) => format!("\"{}\"", path),
            };
            format!("import * as {} from {}", alias, source_str)
        }
        ImportStmt::ModuleAlias { module, alias } => {
            format!("import {} as {}", module, alias)
        }
    }
}

/// Helper method to extract variable names from a pattern for type checking
fn extract_pattern_variables(pattern: &crate::expr::Pattern) -> Option<Vec<String>> {
    let mut variables = Vec::new();

    fn collect_vars(pattern: &crate::expr::Pattern, vars: &mut Vec<String>) {
        match pattern {
            crate::expr::Pattern::Variable(name) => {
                vars.push(name.clone());
            }
            crate::expr::Pattern::List { patterns, rest } => {
                for pattern in patterns {
                    collect_vars(pattern, vars);
                }
                if let Some(rest_var) = rest {
                    vars.push(rest_var.clone());
                }
            }
            crate::expr::Pattern::Map { patterns, rest } => {
                for (_, pattern) in patterns {
                    collect_vars(pattern, vars);
                }
                if let Some(rest_var) = rest {
                    vars.push(rest_var.clone());
                }
            }
            crate::expr::Pattern::Or(patterns) => {
                for pattern in patterns {
                    collect_vars(pattern, vars);
                }
            }
            crate::expr::Pattern::Guard { pattern, .. } => {
                collect_vars(pattern, vars);
            }
            // Other pattern types don't bind variables
            crate::expr::Pattern::Literal(_)
            | crate::expr::Pattern::Wildcard
            | crate::expr::Pattern::Range { .. } => {}
        }
    }

    collect_vars(pattern, &mut variables);

    // Remove duplicates (can happen with OR patterns)
    variables.sort();
    variables.dedup();

    if variables.is_empty() {
        None
    } else {
        Some(variables)
    }
}

/// Helper function to format patterns for display
fn format_pattern(pattern: &ForPattern) -> String {
    match pattern {
        ForPattern::Variable(name) => name.clone(),
        ForPattern::Ignore => "_".to_string(),
        ForPattern::Tuple(patterns) => {
            let patterns_str = patterns
                .iter()
                .map(format_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", patterns_str)
        }
        ForPattern::Array { patterns, rest } => {
            let mut parts = patterns.iter().map(format_pattern).collect::<Vec<_>>();
            if let Some(rest_var) = rest {
                parts.push(format!("..{}", rest_var));
            } else if rest.is_some() {
                parts.push("..".to_string());
            }
            format!("[{}]", parts.join(", "))
        }
        ForPattern::Object(entries) => {
            let parts = entries
                .iter()
                .map(|(k, v)| format!("\"{}\": {}", k, format_pattern(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", parts)
        }
    }
}
