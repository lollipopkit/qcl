use std::collections::HashMap;
use std::sync::Mutex;

use once_cell::sync::Lazy;

use crate::val::{RustFunction, Val};

// Global registry: type_name -> method_name -> RustFunction
static METHOD_REGISTRY: Lazy<Mutex<HashMap<String, HashMap<String, RustFunction>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Register a method for a type name
pub fn register_method(type_name: &str, method: &str, func: RustFunction) {
    let mut reg = METHOD_REGISTRY.lock().unwrap();
    let entry = reg.entry(type_name.to_string()).or_default();
    entry.insert(method.to_string(), func);
}

/// Find a method function for a given receiver value and method name
pub fn find_method_for_val(receiver: &Val, method: &str) -> Option<RustFunction> {
    let reg = METHOD_REGISTRY.lock().unwrap();
    let tname = type_name_for_val(receiver);
    reg.get(&tname)
        .and_then(|m| m.get(method))
        .copied()
}

fn type_name_for_val(v: &Val) -> String {
    match v {
        Val::Str(_) => "String".to_string(),
        Val::Int(_) => "Int".to_string(),
        Val::Float(_) => "Float".to_string(),
        Val::Bool(_) => "Bool".to_string(),
        Val::List(_) => "List".to_string(),
        Val::Map(_) => "Map".to_string(),
        Val::Closure { .. } | Val::RustFunction(_) => "Function".to_string(),
        Val::Task { .. } => "Task".to_string(),
        Val::Channel { .. } => "Channel".to_string(),
        Val::Object { type_name, .. } => type_name.as_ref().to_string(),
        Val::Nil => "Nil".to_string(),
    }
}

