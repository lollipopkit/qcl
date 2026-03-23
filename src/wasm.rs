use wasm_bindgen::prelude::*;

use crate::{de, expr::Expr, val::Val};

/// Evaluate a QCL expression against a JSON context string.
///
/// Returns the result as a JSON string.
#[wasm_bindgen]
pub fn eval_json(expression: &str, json_ctx: &str) -> Result<String, JsError> {
    let ctx: Val = de::from_json_str(json_ctx).map_err(|e| JsError::new(&e.to_string()))?;
    let expr = Expr::parse_cached(expression).map_err(|e| JsError::new(&e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| JsError::new(&e.to_string()))?;
    serde_json::to_string(&result).map_err(|e| JsError::new(&e.to_string()))
}

/// Evaluate a QCL expression against a JS object context (via serde).
///
/// The context is passed as a JsValue and converted to a Val.
/// Returns the result as a JsValue.
#[wasm_bindgen]
pub fn eval(expression: &str, ctx: JsValue) -> Result<JsValue, JsError> {
    let ctx: Val = serde_wasm_bindgen::from_value(ctx).map_err(|e| JsError::new(&e.to_string()))?;
    let expr = Expr::parse_cached(expression).map_err(|e| JsError::new(&e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| JsError::new(&e.to_string()))?;
    serde_wasm_bindgen::to_value(&result).map_err(|e| JsError::new(&e.to_string()))
}
