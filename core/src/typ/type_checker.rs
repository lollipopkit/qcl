use std::collections::HashMap;
use anyhow::Result;
use crate::{
    expr::Expr,
    val::{Type, Val},
    typ::type_system::{TypeRegistry, TypeInferenceEngine},
};

/// Type checking error with location information
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub expected: Option<Type>,
    pub actual: Option<Type>,
    pub expr: Option<Expr>,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type Error: {}", self.message)?;
        if let (Some(expected), Some(actual)) = (&self.expected, &self.actual) {
            write!(f, " (expected {}, got {})", expected.display(), actual.display())?;
        }
        Ok(())
    }
}

impl std::error::Error for TypeError {}

/// Type checker for QCL expressions
#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker {
    /// Type registry for custom types and traits
    registry: TypeRegistry,

    /// Type inference engine
    inference_engine: TypeInferenceEngine,

    /// Local variable types
    local_types: HashMap<String, Type>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    fn type_err(message: &str, expected: Option<Type>, actual: Option<Type>, expr: Option<Expr>) -> anyhow::Error {
        let te = TypeError {
            message: message.to_string(),
            expected,
            actual,
            expr,
        };
        anyhow::Error::new(te)
    }
    /// Create a new type checker
    pub fn new() -> Self {
        let registry = TypeRegistry::new();
        let inference_engine = TypeInferenceEngine::new(registry.clone());

        Self {
            registry,
            inference_engine,
            local_types: HashMap::new(),
        }
    }

    /// Create a type checker with existing registry
    pub fn with_registry(registry: TypeRegistry) -> Self {
        let inference_engine = TypeInferenceEngine::new(registry.clone());

        Self {
            registry,
            inference_engine,
            local_types: HashMap::new(),
        }
    }

    /// Type check an expression
    pub fn check_expr(&mut self, expr: &Expr) -> Result<Type> {
        match expr {
            // Literals (via Val enum)
            Expr::Val(val) => self.check_literal(val),

            // Variables and context access
            Expr::Var(name) => self.check_identifier(name),
            Expr::At(fields) => self.check_context_access(&fields.iter().map(|f| {
                match f.as_ref() {
                    Expr::Val(Val::Str(s)) => s.to_string(),
                    Expr::Val(Val::Int(i)) => i.to_string(),
                    _ => panic!("Field name must be string or int"),
                }
            }).collect::<Vec<_>>()),

            // Binary operations
            Expr::Bin(left, op, right) => self.check_binary_op(left, op, right),
            Expr::And(left, right) => self.check_logical_op(left, right, Type::Bool),
            Expr::Or(left, right) => self.check_logical_op(left, right, Type::Bool),

            // Unary operations
            Expr::Unary(op, expr) => self.check_unary_op(op, expr),

            // Collections
            Expr::List(items) => self.check_list(&items.iter().map(|i| i.as_ref().clone()).collect::<Vec<_>>()),
            Expr::Map(pairs) => self.check_map(&pairs.iter().map(|(k, v)| (k.as_ref().clone(), v.as_ref().clone())).collect::<Vec<_>>()),

            // Access operations
            Expr::Access(expr, field) => self.check_access(expr, field),
            Expr::NullishCoalescing(expr, default) => self.check_nullish_coalescing(expr, default),
            Expr::OptionalAccess(expr, field) => self.check_optional_chaining(expr, field),
            Expr::Conditional(cond, then_expr, else_expr) => {
                // condition must be Bool
                let cond_ty = self.check_expr(cond)?;
                if cond_ty != Type::Bool {
                    return Err(Self::type_err("Ternary condition must be Bool", Some(Type::Bool), Some(cond_ty), Some(*cond.clone())));
                }
                let then_ty = self.check_expr(then_expr)?;
                let else_ty = self.check_expr(else_expr)?;
                // unify then/else types; return the unified type (prefer then_ty)
                self.inference_engine.add_constraint(then_ty.clone(), else_ty.clone());
                Ok(then_ty)
            }
            // Functions - handle both Call (string name) and CallExpr (expression)
            Expr::Call(func, args) => {
                // For Call with string name, create a variable expression for the function
                let func_expr = Expr::Var(func.clone());
                self.check_function_call(&func_expr, &args.iter().map(|a| a.as_ref().clone()).collect::<Vec<_>>())
            }
            Expr::CallExpr(func_expr, args) => {
                self.check_function_call(func_expr, &args.iter().map(|a| a.as_ref().clone()).collect::<Vec<_>>())
            }

            // Concurrency
            Expr::Spawn(expr) => self.check_spawn(expr),
            Expr::Send { channel, value } => self.check_send(channel, value),
            Expr::Recv(channel) => self.check_recv(channel),

            // Complex expressions
            Expr::Select { cases, default_case: default } => self.check_select_expr(cases, default),
            Expr::TemplateString(parts) => self.check_template_string(parts),

            // Unhandled expressions for now
            Expr::Range { .. } => Ok(Type::Any),
            Expr::ChanLiteral { .. } => Ok(Type::Any),
            Expr::Closure { params, body } => {
                // Infer closure as a function type with param type variables and an inferred return
                let mut param_types = Vec::with_capacity(params.len());
                for _ in params {
                    param_types.push(self.inference_engine.fresh_type_var());
                }
                // Body type is inferred by checking the body expression
                let ret_type = self.check_expr(body)?;
                Ok(Type::Function { params: param_types, return_type: Box::new(ret_type) })
            }
            Expr::Match { value, arms } => {
                // Check the matched value type
                let _value_type = self.check_expr(value)?;

                if arms.is_empty() {
                    return Err(Self::type_err("Match expression must have at least one arm", None, None, Some(expr.clone())));
                }

                // Check all arms have compatible types
                let mut result_type: Option<Type> = None;
                for arm in arms {
                    // TODO: Add pattern type checking against value_type
                    let arm_type = self.check_expr(&arm.body)?;

                    if let Some(existing_type) = &result_type {
                        // Add constraint that all arms should return the same type
                        self.inference_engine.add_constraint(existing_type.clone(), arm_type.clone());
                    } else {
                        result_type = Some(arm_type);
                    }
                }

                result_type.ok_or_else(|| Self::type_err("Match expression has no arms", None, None, Some(expr.clone())))
            }
            Expr::Paren(expr) => self.check_expr(expr),
        }
    }

    /// Check identifier type
    fn check_identifier(&mut self, name: &str) -> Result<Type> {
        // Check local variables first
        if let Some(typ) = self.local_types.get(name) {
            return Ok(typ.clone());
        }

        // Check type registry for named types
        if let Some(typ) = self.registry.resolve_type(name) {
            return Ok(typ);
        }

        // Otherwise, assume it's a dynamic variable (type inference needed)
        let var_type = self.inference_engine.fresh_type_var();
        self.local_types.insert(name.to_string(), var_type.clone());
        Ok(var_type)
    }

    /// Check context access type (@req.user.name)
    fn check_context_access(&mut self, _fields: &[String]) -> Result<Type> {
        // Context access is dynamic, return Any for now
        // TODO: Could be enhanced with context schema information
        Ok(Type::Any)
    }

    /// Check binary operation types
    fn check_binary_op(&mut self, left: &Expr, op: &crate::op::BinOp, right: &Expr) -> Result<Type> {
        let left_type = self.check_expr(left)?;
        let right_type = self.check_expr(right)?;

        // Add type constraint
        self.inference_engine.add_constraint(left_type.clone(), right_type.clone());

        // Determine result type based on operator
        match op {
            // Arithmetic operators
            crate::op::BinOp::Add | crate::op::BinOp::Sub |
            crate::op::BinOp::Mul | crate::op::BinOp::Div | crate::op::BinOp::Mod => {
                // Numeric ops: if any side is Float -> Float, else Int.
                // Add constraints to steer inference towards numeric types.
                // If non-numeric types appear, leave to runtime ops or future strict checks.
                let result = match (&left_type, &right_type) {
                    (Type::Float, _) | (_, Type::Float) => Type::Float,
                    (Type::Int, Type::Int) => Type::Int,
                    (Type::Variable(_), Type::Int) | (Type::Int, Type::Variable(_)) => Type::Int,
                    (Type::Variable(_), Type::Variable(_)) => Type::Int,
                    // Fallback: Any (e.g., String + String handled elsewhere)
                    _ => Type::Any,
                };

                // Encourage numeric compatibility via constraints
                match &result {
                    Type::Float => {
                        // Allow Int -> Float promotion via assignability; add soft constraints
                        // T == Float or Int allowed; we don't have soft constraints, so tie both sides.
                        self.inference_engine.add_constraint(left_type.clone(), left_type.clone());
                        self.inference_engine.add_constraint(right_type.clone(), right_type.clone());
                    }
                    Type::Int => {
                        self.inference_engine.add_constraint(left_type.clone(), left_type.clone());
                        self.inference_engine.add_constraint(right_type.clone(), right_type.clone());
                    }
                    _ => {}
                }
                Ok(result)
            }

            // Comparison operators
            crate::op::BinOp::Eq | crate::op::BinOp::Ne => {
                // Equality comparisons allowed for any types
                Ok(Type::Bool)
            }
            crate::op::BinOp::Lt | crate::op::BinOp::Le |
            crate::op::BinOp::Gt | crate::op::BinOp::Ge => {
                // Enforce numeric operands for ordering comparisons
                let lhs_ok = matches!(left_type, Type::Int | Type::Float | Type::Variable(_));
                let rhs_ok = matches!(right_type, Type::Int | Type::Float | Type::Variable(_));
                if !lhs_ok {
                    return Err(Self::type_err(
                        "Ordering comparison requires numeric left operand",
                        None,
                        Some(left_type),
                        Some(Expr::Bin(Box::new(left.clone()), op.clone(), Box::new(right.clone())))
                    ));
                }
                if !rhs_ok {
                    return Err(Self::type_err(
                        "Ordering comparison requires numeric right operand",
                        None,
                        Some(right_type),
                        Some(Expr::Bin(Box::new(left.clone()), op.clone(), Box::new(right.clone())))
                    ));
                }
                Ok(Type::Bool)
            }

            // Special operators
            crate::op::BinOp::In => {
                // Check if right type is container
                match &right_type {
                    Type::List(_) | Type::Map(_, _) => Ok(Type::Bool),
                    _ => Err(Self::type_err(
                        "'in' operator requires container type",
                        Some(Type::List(Box::new(Type::Any))),
                        Some(right_type),
                        Some(Expr::Bin(Box::new(left.clone()), op.clone(), Box::new(right.clone()))),
                    )),
                }
            }
        }
    }

    /// Check logical operation types (&&, ||)
    fn check_logical_op(&mut self, left: &Expr, right: &Expr, result_type: Type) -> Result<Type> {
        let left_type = self.check_expr(left)?;
        let right_type = self.check_expr(right)?;

        // Both operands must be boolean
        if left_type != Type::Bool {
            return Err(Self::type_err("Expected boolean type for logical operation", Some(Type::Bool), Some(left_type), None));
        }
        if right_type != Type::Bool {
            return Err(Self::type_err("Expected boolean type for logical operation", Some(Type::Bool), Some(right_type), None));
        }

        Ok(result_type)
    }

    /// Check unary operation types
    fn check_unary_op(&mut self, op: &crate::op::UnaryOp, expr: &Expr) -> Result<Type> {
        let expr_type = self.check_expr(expr)?;

        match op {
            crate::op::UnaryOp::Not => {
                if expr_type != Type::Bool {
                    return Err(Self::type_err("Expected boolean type for '!' operator", Some(Type::Bool), Some(expr_type), None));
                }
                Ok(Type::Bool)
            }
        }
    }

    /// Check list literal type
    fn check_list(&mut self, items: &[Expr]) -> Result<Type> {
        if items.is_empty() {
            // Empty list, infer element type later
            let elem_type = self.inference_engine.fresh_type_var();
            return Ok(Type::List(Box::new(elem_type)));
        }

        // Check all items have compatible types
        let first_type = self.check_expr(&items[0])?;
        let elem_type = first_type.clone();

        for item in &items[1..] {
            let item_type = self.check_expr(item)?;
            self.inference_engine.add_constraint(elem_type.clone(), item_type);
        }

        Ok(Type::List(Box::new(elem_type)))
    }

    /// Check map literal type
    fn check_map(&mut self, pairs: &[(Expr, Expr)]) -> Result<Type> {
        if pairs.is_empty() {
            // Empty map, infer key/value types later
            let key_type = self.inference_engine.fresh_type_var();
            let value_type = self.inference_engine.fresh_type_var();
            return Ok(Type::Map(Box::new(key_type), Box::new(value_type)));
        }

        // Check all key/value pairs have compatible types
        let (first_key, first_value) = &pairs[0];
        let key_type = self.check_expr(first_key)?;
        let value_type = self.check_expr(first_value)?;

        for (key, value) in &pairs[1..] {
            let key_expr_type = self.check_expr(key)?;
            let value_expr_type = self.check_expr(value)?;

            self.inference_engine.add_constraint(key_type.clone(), key_expr_type);
            self.inference_engine.add_constraint(value_type.clone(), value_expr_type);
        }

        Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
    }

    /// Check access type (expr.field or expr[index])
    fn check_access(&mut self, expr: &Expr, field: &Expr) -> Result<Type> {
        let expr_type = self.check_expr(expr)?;
        let field_type = self.check_expr(field)?;

        match expr_type {
            Type::List(elem_type) => {
                // Field must be integer index
                if field_type != Type::Int {
                    return Err(Self::type_err("List index must be integer", Some(Type::Int), Some(field_type), None));
                }
                Ok((*elem_type).clone())
            }
            Type::Map(key_type, value_type) => {
                // Field must match key type
                self.inference_engine.add_constraint((*key_type).clone(), field_type);
                Ok((*value_type).clone())
            }
            _ => Err(Self::type_err("Cannot access field on type", None, Some(expr_type), None)),
        }
    }

    /// Check nullish coalescing type (expr ?? default)
    fn check_nullish_coalescing(&mut self, expr: &Expr, default: &Expr) -> Result<Type> {
        let expr_type = self.check_expr(expr)?;
        let default_type = self.check_expr(default)?;

        // Expression can be optional, default should be the base type
        match expr_type {
            Type::Optional(inner) => {
                self.inference_engine.add_constraint((*inner).clone(), default_type);
                Ok((*inner).clone())
            }
            Type::Nil => Ok(default_type),
            _ => {
                self.inference_engine.add_constraint(expr_type.clone(), default_type);
                Ok(expr_type)
            }
        }
    }

    /// Check optional chaining type (expr?.field)
    fn check_optional_chaining(&mut self, expr: &Expr, field: &Expr) -> Result<Type> {
        let expr_type = self.check_expr(expr)?;

        match expr_type {
            Type::Optional(inner) => {
                // Evaluate access on the inner type; result becomes optional
                match *inner {
                    Type::List(ref elem_type) => {
                        // index must be Int
                        let field_ty = self.check_expr(field)?;
                        if field_ty != Type::Int {
                            return Err(Self::type_err("List index must be integer", Some(Type::Int), Some(field_ty), None));
                        }
                        Ok(Type::Optional(elem_type.clone()))
                    }
                    Type::Map(ref key_type, ref value_type) => {
                        let field_ty = self.check_expr(field)?;
                        self.inference_engine.add_constraint((**key_type).clone(), field_ty);
                        Ok(Type::Optional(value_type.clone()))
                    }
                    _ => Err(Self::type_err("Cannot access field on type", None, Some(*inner), None)),
                }
            }
            Type::Nil => Ok(Type::Nil),
            _ => self.check_access(expr, field),
        }
    }

    /// Check function call type
    fn check_function_call(&mut self, func: &Expr, args: &[Expr]) -> Result<Type> {
        let func_type = self.check_expr(func)?;

        match func_type {
            Type::Function { params, return_type } => {
                if params.len() != args.len() {
                    return Err(Self::type_err(&format!("Function expects {} arguments", params.len()), None, None, None));
                }

                // Check each argument type
                for (param_type, arg) in params.iter().zip(args.iter()) {
                    let arg_type = self.check_expr(arg)?;
                    self.inference_engine.add_constraint(param_type.clone(), arg_type);
                }

                Ok(*return_type)
            }
            _ => Err(Self::type_err("Cannot call non-function type", None, Some(func_type), None)),
        }
    }


    /// Check select expression type
    fn check_select_expr(&mut self, cases: &[crate::expr::SelectCase], default: &Option<Box<Expr>>) -> Result<Type> {
        // For now, assume all cases return the same type
        let case_type = if let Some(first_case) = cases.first() {
            self.check_expr(&first_case.body)?
        } else if let Some(default_expr) = default {
            self.check_expr(default_expr)?
        } else {
            return Err(Self::type_err("Select expression must have at least one case or default", None, None, None));
        };

        // Check all cases and default have compatible types
        for case in cases {
            let case_result_type = self.check_expr(&case.body)?;
            self.inference_engine.add_constraint(case_type.clone(), case_result_type);
        }

        if let Some(default_expr) = default {
            let default_type = self.check_expr(default_expr)?;
            self.inference_engine.add_constraint(case_type.clone(), default_type);
        }

        Ok(case_type)
    }

    /// Check template string type
    fn check_template_string(&mut self, parts: &[crate::expr::TemplateStringPart]) -> Result<Type> {
        // All parts must be string-coercible
        for part in parts {
            match part {
                crate::expr::TemplateStringPart::Literal(_) => {
                    // String literals are fine
                }
                crate::expr::TemplateStringPart::Expr(expr) => {
                    let expr_type = self.check_expr(expr)?;
                    // Check if expression can be converted to string
                    if !expr_type.is_assignable_to(&Type::String) {
                        return Err(Self::type_err("Template string expression must be string-coercible", Some(Type::String), Some(expr_type), Some(*expr.clone())));
                    }
                }
                crate::expr::TemplateStringPart::Enhanced(spec) => {
                    // For enhanced parts, tokenize and check the expression
                    let tokens = match crate::token::Tokenizer::tokenize_enhanced(spec) {
                        Ok(tokens) => tokens,
                        Err(_) => return Err(Self::type_err("Invalid enhanced template specification", Some(Type::String), None, None)),
                    };

                    if !tokens.is_empty() {
                        let mut parser = crate::ast::Parser::new(&tokens);
                        let expr = match parser.parse() {
                            Ok(expr) => expr,
                            Err(_) => return Err(Self::type_err("Invalid enhanced template expression", Some(Type::String), None, None)),
                        };

                        let expr_type = self.check_expr(&expr)?;
                        if !expr_type.is_assignable_to(&Type::String) {
                            return Err(Self::type_err("Enhanced template expression must be string-coercible", Some(Type::String), Some(expr_type), Some(expr)));
                        }
                    }
                }
            }
        }

        Ok(Type::String)
    }

    /// Check spawn expression type
    fn check_spawn(&mut self, expr: &Expr) -> Result<Type> {
        let expr_type = self.check_expr(expr)?;
        Ok(Type::Task(Box::new(expr_type)))
    }

    /// Check send expression type
    fn check_send(&mut self, channel: &Expr, value: &Expr) -> Result<Type> {
        let channel_type = self.check_expr(channel)?;
        let value_type = self.check_expr(value)?;

        match channel_type {
            Type::Channel(inner) => {
                self.inference_engine.add_constraint(*inner, value_type);
                Ok(Type::Nil)
            }
            _ => Err(Self::type_err("Cannot send to non-channel type", None, Some(channel_type), Some(channel.clone()))),
        }
    }

    /// Check recv expression type
    fn check_recv(&mut self, channel: &Expr) -> Result<Type> {
        let channel_type = self.check_expr(channel)?;

        match channel_type {
            Type::Channel(inner) => Ok((*inner).clone()),
            _ => Err(Self::type_err("Cannot receive from non-channel type", None, Some(channel_type), Some(channel.clone()))),
        }
    }


    /// Solve type constraints and return final types
    pub fn solve_constraints(&mut self) -> Result<HashMap<String, Type>> {
        self.inference_engine.solve_constraints()
    }

    /// Check literal value type
    fn check_literal(&mut self, val: &Val) -> Result<Type> {
        match val {
            Val::Nil => Ok(Type::Nil),
            Val::Bool(_) => Ok(Type::Bool),
            Val::Int(_) => Ok(Type::Int),
            Val::Float(_) => Ok(Type::Float),
            Val::Str(_) => Ok(Type::String),
            Val::List(items) => {
                if items.is_empty() {
                    let elem_type = self.registry.fresh_type_var();
                    Ok(Type::List(Box::new(elem_type)))
                } else {
                    let first_type = self.infer_list_element_type(&items[0])?;
                    Ok(Type::List(Box::new(first_type)))
                }
            }
            Val::Map(map) => {
                if map.is_empty() {
                    let key_type = self.registry.fresh_type_var();
                    let value_type = self.registry.fresh_type_var();
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                } else {
                    let (_first_key, first_value) = map.iter().next().unwrap();
                    let key_type = Type::String; // Map keys are always strings
                    let value_type = self.infer_val_type(first_value)?;
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                }
            }
            // Other types return Any for now
            Val::Closure { .. } => Ok(Type::Any),
            Val::RustFunction(_) => Ok(Type::Any),
            Val::Task { .. } => Ok(Type::Any),
            Val::Channel { .. } => Ok(Type::Any),
        }
    }

    /// Infer type from a Val (for use in literal checking)
    fn infer_val_type(&mut self, val: &Val) -> Result<Type> {
        match val {
            Val::Nil => Ok(Type::Nil),
            Val::Bool(_) => Ok(Type::Bool),
            Val::Int(_) => Ok(Type::Int),
            Val::Float(_) => Ok(Type::Float),
            Val::Str(_) => Ok(Type::String),
            Val::List(items) => {
                if items.is_empty() {
                    let elem_type = self.registry.fresh_type_var();
                    Ok(Type::List(Box::new(elem_type)))
                } else {
                    let elem_type = self.infer_list_element_type(&items[0])?;
                    Ok(Type::List(Box::new(elem_type)))
                }
            }
            Val::Map(map) => {
                if map.is_empty() {
                    let key_type = self.registry.fresh_type_var();
                    let value_type = self.registry.fresh_type_var();
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                } else {
                    let (_first_key, first_value) = map.iter().next().unwrap();
                    let key_type = Type::String; // Map keys are always strings
                    let value_type = self.infer_val_type(first_value)?;
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                }
            }
            Val::Closure { .. } => Ok(Type::Any),
            Val::RustFunction(_) => Ok(Type::Any),
            Val::Task { .. } => Ok(Type::Any),
            Val::Channel { .. } => Ok(Type::Any),
        }
    }

    /// Infer list element type from a Val
    fn infer_list_element_type(&mut self, item: &Val) -> Result<Type> {
        self.infer_val_type(item)
    }

    /// Get the inferred type for a local variable
    pub fn get_local_type(&self, name: &str) -> Option<&Type> {
        self.local_types.get(name)
    }

    /// Add a type annotation for a local variable
    pub fn add_local_type(&mut self, name: String, typ: Type) {
        self.local_types.insert(name, typ);
    }

    /// Get the type registry
    pub fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    /// Get mutable access to the type registry
    pub fn registry_mut(&mut self) -> &mut TypeRegistry {
        &mut self.registry
    }

    /// Enter a new scope for local variables
    pub fn push_scope(&mut self) {
        // For now, we don't implement actual scope management
        // This is a placeholder for future scope management
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        // For now, we don't implement actual scope management
        // This is a placeholder for future scope management
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{expr::Expr, stmt::Stmt};

    #[test]
    fn test_literal_types() {
        let mut checker = TypeChecker::new();

        assert_eq!(checker.check_expr(&Expr::Val(Val::Nil)).unwrap(), Type::Nil);
        assert_eq!(checker.check_expr(&Expr::Val(Val::Bool(true))).unwrap(), Type::Bool);
        assert_eq!(checker.check_expr(&Expr::Val(Val::Int(42))).unwrap(), Type::Int);
        assert_eq!(checker.check_expr(&Expr::Val(Val::Float(3.14))).unwrap(), Type::Float);
        assert_eq!(checker.check_expr(&Expr::Val(Val::Str("hello".into()))).unwrap(), Type::String);
    }

    #[test]
    fn test_binary_operations() {
        let mut checker = TypeChecker::new();

        let add_expr = Expr::Bin(
            Box::new(Expr::Val(Val::Int(1))),
            crate::op::BinOp::Add,
            Box::new(Expr::Val(Val::Int(2))),
        );

        let result_type = checker.check_expr(&add_expr).unwrap();
        // Now numeric ops infer Int for Int+Int
        assert!(matches!(result_type, Type::Int));
    }

    #[test]
    fn test_list_types() {
        let mut checker = TypeChecker::new();

        let list_expr = Expr::List(vec![
            Box::new(Expr::Val(Val::Int(1))),
            Box::new(Expr::Val(Val::Int(2))),
            Box::new(Expr::Val(Val::Int(3))),
        ]);

        let result_type = checker.check_expr(&list_expr).unwrap();
        if let Type::List(elem_type) = result_type {
            assert_eq!(*elem_type, Type::Int);
        } else {
            panic!("Expected List<Int>");
        }
    }

    #[test]
    fn test_type_mismatch_error() {
        let mut checker = TypeChecker::new();

        let logical_expr = Expr::And(
            Box::new(Expr::Val(Val::Int(1))), // Should be Bool
            Box::new(Expr::Val(Val::Bool(true))),
        );

        let result = checker.check_expr(&logical_expr);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("Expected boolean type"));
    }

    #[test]
    fn test_let_statement_type_checking() {
        let mut checker = TypeChecker::new();

        // Test let statement with type annotation
        let let_stmt = Stmt::Let {
            pattern: crate::expr::Pattern::Variable("x".to_string()),
            type_annotation: Some(Type::Int),
            value: Box::new(Expr::Val(Val::Int(42))),
            span: None,
        };

        // Should pass type checking
        assert!(let_stmt.type_check(&mut checker).is_ok());

        // Test type mismatch
        let let_stmt_mismatch = Stmt::Let {
            pattern: crate::expr::Pattern::Variable("y".to_string()),
            type_annotation: Some(Type::String),
            value: Box::new(Expr::Val(Val::Int(42))), // Int assigned to String
            span: None,
        };

        let result = let_stmt_mismatch.type_check(&mut checker);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch in let statement"));
    }

    #[test]
    fn test_assignment_type_checking() {
        let mut checker = TypeChecker::new();

        // First declare a variable
        let let_stmt = Stmt::Let {
            pattern: crate::expr::Pattern::Variable("x".to_string()),
            type_annotation: Some(Type::Int),
            value: Box::new(Expr::Val(Val::Int(42))),
            span: None,
        };
        let_stmt.type_check(&mut checker).unwrap();

        // Test valid assignment
        let assign_stmt = Stmt::Assign {
            name: "x".to_string(),
            value: Box::new(Expr::Val(Val::Int(100))),
            span: None,
        };
        assert!(assign_stmt.type_check(&mut checker).is_ok());

        // Test invalid assignment
        let assign_stmt_invalid = Stmt::Assign {
            name: "x".to_string(),
            value: Box::new(Expr::Val(Val::Str("hello".into()))), // String assigned to Int
            span: None,
        };
        let result = assign_stmt_invalid.type_check(&mut checker);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Type mismatch in assignment"));
    }

    #[test]
    fn test_if_statement_type_checking() {
        let mut checker = TypeChecker::new();

        // Test if statement with boolean condition
        let if_stmt = Stmt::If {
            condition: Box::new(Expr::Val(Val::Bool(true))),
            then_stmt: Box::new(Stmt::Let {
                pattern: crate::expr::Pattern::Variable("x".to_string()),
                type_annotation: None,
                value: Box::new(Expr::Val(Val::Int(42))),
                span: None,
            }),
            else_stmt: None,
        };
        assert!(if_stmt.type_check(&mut checker).is_ok());

        // Test if statement with non-boolean condition
        let if_stmt_invalid = Stmt::If {
            condition: Box::new(Expr::Val(Val::Int(42))), // Int instead of Bool
            then_stmt: Box::new(Stmt::Let {
                pattern: crate::expr::Pattern::Variable("x".to_string()),
                type_annotation: None,
                value: Box::new(Expr::Val(Val::Int(42))),
                span: None,
            }),
            else_stmt: None,
        };
        let result = if_stmt_invalid.type_check(&mut checker);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("If condition must be Bool"));
    }

    #[test]
    fn test_while_statement_type_checking() {
        let mut checker = TypeChecker::new();

        // Test while statement with boolean condition
        let while_stmt = Stmt::While {
            condition: Box::new(Expr::Val(Val::Bool(true))),
            body: Box::new(Stmt::Expr(Box::new(Expr::Val(Val::Int(42))))),
        };
        assert!(while_stmt.type_check(&mut checker).is_ok());

        // Test while statement with non-boolean condition
        let while_stmt_invalid = Stmt::While {
            condition: Box::new(Expr::Val(Val::Int(42))), // Int instead of Bool
            body: Box::new(Stmt::Expr(Box::new(Expr::Val(Val::Int(42))))),
        };
        let result = while_stmt_invalid.type_check(&mut checker);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("While condition must be Bool"));
    }

    #[test]
    fn test_for_statement_type_checking() {
        let mut checker = TypeChecker::new();

        // Test for statement with list iterable
        let for_stmt = Stmt::For {
            pattern: crate::stmt::ForPattern::Variable("item".to_string()),
            iterable: Box::new(Expr::List(vec![
                Box::new(Expr::Val(Val::Int(1))),
                Box::new(Expr::Val(Val::Int(2))),
            ])),
            body: Box::new(Stmt::Expr(Box::new(Expr::Val(Val::Nil)))),
        };
        assert!(for_stmt.type_check(&mut checker).is_ok());

        // Test for statement with non-iterable
        let for_stmt_invalid = Stmt::For {
            pattern: crate::stmt::ForPattern::Variable("item".to_string()),
            iterable: Box::new(Expr::Val(Val::Int(42))), // Int is not iterable
            body: Box::new(Stmt::Expr(Box::new(Expr::Val(Val::Nil)))),
        };
        let result = for_stmt_invalid.type_check(&mut checker);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("For loop iterable must be List, String, or Map"));
    }
}
