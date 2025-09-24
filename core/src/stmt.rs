use crate::{
    expr::Expr,
    import::{ImportContext, ImportStmt, ModuleResolver},
    val::{Type, Val},
};
use anyhow::{Result, anyhow};
use std::{collections::HashMap, fmt::Display, sync::Arc};

/// Statement AST 节点类型定义
///
/// 语法设计：
/// program  ::= statement*
/// statement ::= import_stmt | if_stmt | while_stmt | let_stmt | assign_stmt | goto_stmt | label_stmt | break_stmt | continue_stmt | return_stmt | fn_stmt | expr_stmt | block_stmt
/// import_stmt ::= 'import' import_spec ';'
/// if_stmt  ::= 'if' '(' expr ')' statement ['else' statement]
/// while_stmt ::= 'while' '(' expr ')' statement
/// let_stmt ::= 'let' id [':' type] '=' expr ';'
/// assign_stmt ::= id '=' expr ';'
/// goto_stmt ::= 'goto' id ';'
/// label_stmt ::= id ':'
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
    /// while (condition) body
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    /// let name [: type] = value;
    Let {
        name: String,
        type_annotation: Option<Type>,
        value: Box<Expr>,
    },
    /// name = value; (赋值语句)
    Assign { name: String, value: Box<Expr> },
    /// name = value; (变量定义，类似 Go 的短声明)
    Define { name: String, value: Box<Expr> },
    /// goto label;
    Goto { label: String },
    /// label:
    Label { name: String },
    /// break;
    Break,
    /// continue;
    Continue,
    /// return [expression];
    Return { value: Option<Box<Expr>> },
    /// fn name(param1, param2) { body }
    Function {
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
    },
    /// expression;
    Expr(Box<Expr>),
    /// { statements }
    Block { statements: Vec<Box<Stmt>> },
    /// 空语句 (用于处理解析时的占位)
    Empty,
}

/// 控制流状态，用于 break/continue/goto 的实现
#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    /// 正常执行
    None,
    /// break 语句
    Break,
    /// continue 语句  
    Continue,
    /// goto 语句
    Goto(String),
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
        }
    }

    pub fn with_resolver(resolver: Arc<ModuleResolver>) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            import_ctx: ImportContext::new(),
            resolver,
        }
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
                        ControlFlow::Goto(label) => return Ok(ControlFlow::Goto(label)),
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::None => {}
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                let val = value.eval_with_env(ctx, Some(env))?;

                // Validate type annotation if provided
                if let Some(expected_type) = type_annotation {
                    expected_type.validate(&val)?;
                }

                env.define(name.clone(), val);
                Ok(ControlFlow::None)
            }
            Stmt::Assign { name, value } => {
                let val = value.eval_with_env(ctx, Some(env))?;
                env.assign(name, val)?;
                Ok(ControlFlow::None)
            }
            Stmt::Define { name, value } => {
                let val = value.eval_with_env(ctx, Some(env))?;
                env.define(name.clone(), val);
                Ok(ControlFlow::None)
            }
            Stmt::Goto { label } => Ok(ControlFlow::Goto(label.clone())),
            Stmt::Label { .. } => {
                // 标签本身不执行任何操作
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
            Stmt::Function { name, params, body } => {
                let func_val = Val::Closure {
                    params: Arc::new(params.clone()),
                    body: Arc::new((**body).clone()),
                    env: Arc::new(env.clone()),
                };
                env.define(name.clone(), func_val);
                Ok(ControlFlow::None)
            }
            Stmt::Expr(expr) => {
                // 执行表达式，忽略返回值
                expr.eval_with_env(ctx, Some(env))?;
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
}

/// 程序结构 - 包含语句列表和标签映射
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Box<Stmt>>,
    pub labels: HashMap<String, usize>, // 标签名到语句索引的映射
}

impl Program {
    pub fn new(statements: Vec<Box<Stmt>>) -> Result<Self> {
        let mut labels = HashMap::new();

        // 构建标签映射
        for (index, stmt) in statements.iter().enumerate() {
            if let Stmt::Label { name } = stmt.as_ref() {
                if labels.contains_key(name) {
                    return Err(anyhow!("Duplicate label: {}", name));
                }
                labels.insert(name.clone(), index);
            }
        }

        Ok(Program { statements, labels })
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
                ControlFlow::Goto(label) => {
                    if let Some(&target_pc) = self.labels.get(&label) {
                        pc = target_pc;
                    } else {
                        return Err(anyhow!("Undefined label: {}", label));
                    }
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
            Stmt::While { condition, body } => {
                write!(f, "while ({}) {}", condition, body)
            }
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                if let Some(typ) = type_annotation {
                    write!(f, "let {}: {:?} = {};", name, typ, value)
                } else {
                    write!(f, "let {} = {};", name, value)
                }
            }
            Stmt::Assign { name, value } => {
                write!(f, "{} = {};", name, value)
            }
            Stmt::Define { name, value } => {
                write!(f, "{} = {};", name, value)
            }
            Stmt::Goto { label } => {
                write!(f, "goto {};", label)
            }
            Stmt::Label { name } => {
                write!(f, "{}:", name)
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
            Stmt::Function { name, params, body } => {
                write!(f, "fn {}({}) {}", name, params.join(", "), body)
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
    use crate::import::{ImportSource, ImportStmt};

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
