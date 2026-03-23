//! Python bindings for QCL via PyO3.

use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyDict, PyFloat, PyInt, PyList, PyString};

use alloc::{string::ToString, sync::Arc, vec::Vec};

use crate::{de, expr::Expr, val::Val};

/// Convert a Python object to a QCL Val.
fn py_to_val(obj: &Bound<'_, PyAny>) -> PyResult<Val> {
    if obj.is_none() {
        Ok(Val::Nil)
    } else if let Ok(b) = obj.downcast::<PyBool>() {
        Ok(Val::Bool(b.is_true()))
    } else if let Ok(i) = obj.downcast::<PyInt>() {
        Ok(Val::Int(i.extract::<i64>()?))
    } else if let Ok(f) = obj.downcast::<PyFloat>() {
        Ok(Val::Float(f.extract::<f64>()?))
    } else if let Ok(s) = obj.downcast::<PyString>() {
        Ok(Val::from(s.to_str()?))
    } else if let Ok(list) = obj.downcast::<PyList>() {
        let items: Vec<Val> = list.iter().map(|item| py_to_val(&item)).collect::<PyResult<_>>()?;
        Ok(Val::List(Arc::new(items)))
    } else if let Ok(dict) = obj.downcast::<PyDict>() {
        let mut map = hashbrown::HashMap::with_capacity(dict.len());
        for (k, v) in dict.iter() {
            let key = k
                .downcast::<PyString>()
                .map_err(|_| PyValueError::new_err("dict keys must be strings"))?
                .to_str()?
                .to_string();
            map.insert(key, py_to_val(&v)?);
        }
        Ok(Val::Map(Arc::new(map)))
    } else {
        Err(PyValueError::new_err(format!(
            "unsupported Python type: {}",
            obj.get_type().name()?
        )))
    }
}

/// Convert a QCL Val to a Python object.
fn val_to_py(py: Python<'_>, val: &Val) -> PyResult<PyObject> {
    match val {
        Val::Nil => Ok(py.None()),
        Val::Bool(b) => Ok(b.into_pyobject(py)?.to_owned().into_any().unbind()),
        Val::Int(i) => Ok(i.into_pyobject(py)?.into_any().unbind()),
        Val::Float(f) => Ok(f.into_pyobject(py)?.into_any().unbind()),
        Val::Str(s) => Ok(s.as_ref().into_pyobject(py)?.into_any().unbind()),
        Val::List(l) => {
            let items: Vec<PyObject> = l.iter().map(|v| val_to_py(py, v)).collect::<PyResult<_>>()?;
            Ok(PyList::new(py, items)?.into_any().unbind())
        }
        Val::Map(m) => {
            let dict = PyDict::new(py);
            for (k, v) in m.iter() {
                dict.set_item(k, val_to_py(py, v)?)?;
            }
            Ok(dict.into_any().unbind())
        }
    }
}

/// Evaluate a QCL expression against a Python dict context.
///
/// Args:
///     expression: QCL expression string
///     context: Python dict serving as the evaluation context
///
/// Returns:
///     The evaluation result as a Python object
#[pyfunction]
fn eval(py: Python<'_>, expression: &str, context: &Bound<'_, PyAny>) -> PyResult<PyObject> {
    let ctx = py_to_val(context)?;
    let expr = Expr::parse_cached_arc(expression).map_err(|e| PyValueError::new_err(e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| PyValueError::new_err(e.to_string()))?;;
    val_to_py(py, &result)
}

/// Evaluate a QCL expression against a JSON string context.
///
/// Args:
///     expression: QCL expression string
///     json_ctx: JSON string serving as the evaluation context
///
/// Returns:
///     The evaluation result as a Python object
#[pyfunction]
fn eval_json(py: Python<'_>, expression: &str, json_ctx: &str) -> PyResult<PyObject> {
    let ctx: Val = de::from_json_str(json_ctx).map_err(|e| PyValueError::new_err(e.to_string()))?;
    let expr = Expr::parse_cached_arc(expression).map_err(|e| PyValueError::new_err(e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| PyValueError::new_err(e.to_string()))?;;
    val_to_py(py, &result)
}

/// Check if a QCL expression evaluates to truthy.
///
/// Args:
///     expression: QCL expression string
///     context: Python dict serving as the evaluation context
///
/// Returns:
///     True if the result is truthy, False otherwise
#[pyfunction]
fn check(expression: &str, context: &Bound<'_, PyAny>) -> PyResult<bool> {
    let ctx = py_to_val(context)?;
    let expr = Expr::parse_cached_arc(expression).map_err(|e| PyValueError::new_err(e.to_string()))?;
    let result = expr.eval(&ctx).map_err(|e| PyValueError::new_err(e.to_string()))?;
    Ok(!matches!(result, Val::Bool(false) | Val::Nil))
}

/// QCL Python module.
#[pymodule]
fn qcl(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(eval, m)?)?;
    m.add_function(wrap_pyfunction!(eval_json, m)?)?;
    m.add_function(wrap_pyfunction!(check, m)?)?;
    Ok(())
}
