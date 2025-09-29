use std::collections::HashMap;
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::val::{RustFunction, Val};

// Global registry: type_name -> method_name -> RustFunction
static METHOD_REGISTRY: Lazy<RwLock<HashMap<String, HashMap<String, RustFunction>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Register a method for a type name
pub fn register_method(type_name: &str, method: &str, func: RustFunction) {
    if let Ok(mut reg) = METHOD_REGISTRY.write() {
        let entry = reg.entry(type_name.to_string()).or_default();
        entry.insert(method.to_string(), func);
    }
}

/// Find a method function for a given receiver value and method name
pub fn find_method_for_val(receiver: &Val, method: &str) -> Option<RustFunction> {
    // Avoid allocation by using static names or borrowed object name
    let tname: &str = match receiver {
        Val::Str(_) => "String",
        Val::Int(_) => "Int",
        Val::Float(_) => "Float",
        Val::Bool(_) => "Bool",
        Val::List(_) => "List",
        Val::Map(_) => "Map",
        Val::Closure { .. } | Val::RustFunction(_) => "Function",
        Val::Task { .. } => "Task",
        Val::Channel { .. } => "Channel",
        Val::Object { type_name, .. } => type_name.as_ref(),
        Val::Nil => "Nil",
    };
    METHOD_REGISTRY
        .read()
        .ok()
        .and_then(|reg| reg.get(tname).and_then(|m| m.get(method)).copied())
}
