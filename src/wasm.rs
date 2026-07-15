use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicU32, Ordering};

use once_cell::sync::Lazy;
use wasm_bindgen::prelude::*;

use crate::{de, expr::Expr, val::Val};

/// Monotonic handle id for pre-parsed contexts (0 is reserved as "invalid").
static NEXT_HANDLE: AtomicU32 = AtomicU32::new(1);
/// Live pre-parsed contexts, keyed by handle id. Lets a JSON context be parsed
/// once and shared across many `eval_ctx` calls without re-parsing.
static CTX_TABLE: Lazy<Mutex<HashMap<u32, Arc<Val>>>> = Lazy::new(|| Mutex::new(HashMap::new()));

/// Evaluate a QCL expression against a JSON context string.
///
/// Returns the result as a JSON string.
#[wasm_bindgen]
pub fn eval_json(expression: &str, json_ctx: &str) -> Result<String, JsError> {
    let ctx: Val = de::from_json_str(json_ctx).map_err(|e| JsError::new(&e.to_string()))?;
    let expr = Expr::parse_cached_arc(expression).map_err(|e| JsError::new(&e.to_string()))?;
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
    let expr = Expr::parse_cached_arc(expression).map_err(|e| JsError::new(&e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| JsError::new(&e.to_string()))?;
    serde_wasm_bindgen::to_value(&result).map_err(|e| JsError::new(&e.to_string()))
}

/// Parse a JSON context string into an opaque handle for repeated evaluation.
/// Returns a non-zero handle id; pass it to `eval_ctx` any number of times and
/// release it with `free_ctx` when done.
#[wasm_bindgen]
pub fn parse_ctx(json_ctx: &str) -> Result<u32, JsError> {
    let val = de::from_json_str(json_ctx).map_err(|e| JsError::new(&e.to_string()))?;
    let arc = Arc::new(val);
    let mut table = CTX_TABLE.lock().unwrap();
    let id = alloc_handle(&table);
    table.insert(id, arc);
    Ok(id)
}

/// Allocate a handle id, skipping the reserved 0 and any id still live in the
/// table so that counter wraparound can never silently evict a live context.
fn alloc_handle(table: &HashMap<u32, Arc<Val>>) -> u32 {
    loop {
        let id = NEXT_HANDLE.fetch_add(1, Ordering::Relaxed);
        if id != 0 && !table.contains_key(&id) {
            return id;
        }
    }
}

/// Evaluate a QCL expression against a pre-parsed context handle.
#[wasm_bindgen]
pub fn eval_ctx(expression: &str, handle: u32) -> Result<JsValue, JsError> {
    let ctx = CTX_TABLE
        .lock()
        .unwrap()
        .get(&handle)
        .cloned()
        .ok_or_else(|| JsError::new("invalid or freed ctx handle"))?;
    let expr = Expr::parse_cached_arc(expression).map_err(|e| JsError::new(&e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| JsError::new(&e.to_string()))?;
    serde_wasm_bindgen::to_value(&result).map_err(|e| JsError::new(&e.to_string()))
}

/// Release a context handle returned by `parse_ctx`. After this the handle is
/// invalid and must not be used again.
#[wasm_bindgen]
pub fn free_ctx(handle: u32) {
    CTX_TABLE.lock().unwrap().remove(&handle);
}
