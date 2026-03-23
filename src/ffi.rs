//! C FFI for QCL.
//!
//! Provides a C-compatible API for parsing and evaluating QCL expressions.
//! Strings are passed as null-terminated C strings; results are returned as
//! heap-allocated C strings that must be freed by the caller via `qcl_free`.

use alloc::{ffi::CString, format};
use core::ffi::{CStr, c_char, c_int};

use crate::{ast::Parser, token::Tokenizer, val::Val};

/// Parse and evaluate a QCL expression against a JSON context string.
///
/// # Safety
/// - `expression` and `json_ctx` must be valid, non-null, null-terminated C strings.
/// - The returned string must be freed by calling `qcl_free`.
/// - Returns null on error; call `qcl_last_error` to retrieve the message.
#[cfg(feature = "json")]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn qcl_eval_json(expression: *const c_char, json_ctx: *const c_char) -> *mut c_char {
    let expr_str = match unsafe { CStr::from_ptr(expression) }.to_str() {
        Ok(s) => s,
        Err(e) => return set_err(format!("invalid expression UTF-8: {e}")),
    };
    let ctx_str = match unsafe { CStr::from_ptr(json_ctx) }.to_str() {
        Ok(s) => s,
        Err(e) => return set_err(format!("invalid context UTF-8: {e}")),
    };

    let ctx: Val = match crate::de::from_json_str(ctx_str) {
        Ok(v) => v,
        Err(e) => return set_err(format!("{e}")),
    };

    eval_inner(expr_str, &ctx)
}

/// Parse and evaluate a QCL expression against a pre-parsed context.
///
/// This lower-level function takes a raw expression string and evaluates it.
/// Context must be built separately.
fn eval_inner(expression: &str, ctx: &Val) -> *mut c_char {
    let tokens = match Tokenizer::new(expression) {
        Ok(t) => t,
        Err(e) => return set_err(format!("{e}")),
    };
    let expr = match Parser::new(&tokens).parse() {
        Ok(e) => e,
        Err(e) => return set_err(format!("{e}")),
    };
    match expr.eval(ctx) {
        Ok(result) => to_c_string(format!("{result}")),
        Err(e) => set_err(format!("{e}")),
    }
}

/// Retrieve the last error message.
///
/// # Safety
/// The returned string must be freed by calling `qcl_free`.
/// Returns null if no error has occurred.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn qcl_last_error() -> *mut c_char {
    LAST_ERROR.with(|cell| {
        let err = cell.take();
        match err {
            Some(msg) => to_c_string(msg),
            None => core::ptr::null_mut(),
        }
    })
}

/// Free a string returned by QCL functions.
///
/// # Safety
/// `ptr` must be a pointer previously returned by a QCL function, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn qcl_free(ptr: *mut c_char) {
    if !ptr.is_null() {
        drop(unsafe { CString::from_raw(ptr) });
    }
}

/// Check if the result is truthy (non-nil, non-false).
///
/// Returns 1 for truthy, 0 for falsy, -1 on error.
///
/// # Safety
/// `expression` and `json_ctx` must be valid, non-null, null-terminated C strings.
#[cfg(feature = "json")]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn qcl_check_json(expression: *const c_char, json_ctx: *const c_char) -> c_int {
    let expr_str = match unsafe { CStr::from_ptr(expression) }.to_str() {
        Ok(s) => s,
        Err(e) => {
            set_err(format!("invalid expression UTF-8: {e}"));
            return -1;
        }
    };
    let ctx_str = match unsafe { CStr::from_ptr(json_ctx) }.to_str() {
        Ok(s) => s,
        Err(e) => {
            set_err(format!("invalid context UTF-8: {e}"));
            return -1;
        }
    };

    let ctx: Val = match crate::de::from_json_str(ctx_str) {
        Ok(v) => v,
        Err(e) => {
            set_err(format!("{e}"));
            return -1;
        }
    };

    let tokens = match Tokenizer::new(expr_str) {
        Ok(t) => t,
        Err(e) => {
            set_err(format!("{e}"));
            return -1;
        }
    };
    let expr = match Parser::new(&tokens).parse() {
        Ok(e) => e,
        Err(e) => {
            set_err(format!("{e}"));
            return -1;
        }
    };

    match expr.eval(&ctx) {
        Ok(Val::Bool(true)) => 1,
        Ok(Val::Bool(false) | Val::Nil) => 0,
        Ok(_) => 1,
        Err(e) => {
            set_err(format!("{e}"));
            -1
        }
    }
}

// Thread-local error storage
std::thread_local! {
    static LAST_ERROR: std::cell::Cell<Option<alloc::string::String>> = const { std::cell::Cell::new(None) };
}

fn set_err(msg: alloc::string::String) -> *mut c_char {
    LAST_ERROR.with(|cell| cell.set(Some(msg)));
    core::ptr::null_mut()
}

fn to_c_string(s: alloc::string::String) -> *mut c_char {
    match CString::new(s) {
        Ok(cs) => cs.into_raw(),
        Err(_) => core::ptr::null_mut(),
    }
}
