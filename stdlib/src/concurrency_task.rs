//! Concurrency module for QCL
//!
//! Provides Go-style concurrency primitives including tasks, channels, and synchronization.

use anyhow::{Result, anyhow};
use qcl_core::module::Module;
use qcl_core::val::Val;
use std::collections::HashMap;

/// Task module - provides task management functions
#[derive(Debug)]
pub struct TaskModule;

impl Default for TaskModule {
    fn default() -> Self {
        Self::new()
    }
}

impl Module for TaskModule {
    fn name(&self) -> &str {
        "task"
    }

    fn description(&self) -> &str {
        "Task management functions for concurrent operations"
    }

    fn enabled(&self) -> bool {
        #[cfg(feature = "concurrency")]
        {
            true
        }
        #[cfg(not(feature = "concurrency"))]
        {
            false
        }
    }

    fn register(&self, registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        let exports = self.exports();
        for (name, value) in exports {
            registry.register_builtin(&format!("{}::{}", self.name(), name), value);
        }
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        let mut functions = HashMap::new();

        functions.insert("await".to_string(), Val::RustFunction(task_await));
        functions.insert("try_await".to_string(), Val::RustFunction(task_try_await));
        functions.insert("join_all".to_string(), Val::RustFunction(task_join_all));
        functions.insert("sleep".to_string(), Val::RustFunction(task_sleep));
        functions.insert(
            "spawn_blocking".to_string(),
            Val::RustFunction(task_spawn_blocking),
        );

        functions
    }
}

impl TaskModule {
    pub fn new() -> Self {
        Self
    }
}

/// Await a task to complete and return its result
fn task_await(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("task::await() expects exactly 1 argument"));
    }

    match &args[0] {
        Val::Task { id, value: _ } => {
            #[cfg(feature = "concurrency")]
            {
                match qcl_core::runtime::with_runtime(|runtime| {
                    runtime.block_on(runtime.join_task(*id))
                }) {
                    Ok(result) => Ok(result),
                    Err(e) => Err(anyhow!("Failed to await task: {}", e)),
                }
            }
            #[cfg(not(feature = "concurrency"))]
            {
                // In non-concurrency mode, return the pre-computed value
                match value {
                    Some(val) => Ok(*val.clone()),
                    None => Err(anyhow!("Task not completed")),
                }
            }
        }
        _ => Err(anyhow!("task::await() expects a Task argument")),
    }
}

/// Try to await a task, returning None if not ready
fn task_try_await(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("task::try_await() expects exactly 1 argument"));
    }

    match &args[0] {
        Val::Task { value, .. } => {
            // In the current implementation, we don't have non-blocking await
            // For now, just check if we have a pre-computed value
            match value {
                Some(val) => Ok(Val::List(vec![Val::Bool(true), *val.clone()].into())),
                None => Ok(Val::List(vec![Val::Bool(false), Val::Nil].into())),
            }
        }
        _ => Err(anyhow!("task::try_await() expects a Task argument")),
    }
}

/// Join multiple tasks and return their results as a list
fn task_join_all(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.is_empty() {
        return Ok(Val::List(vec![].into()));
    }

    let mut results = Vec::new();

    for arg in args {
        match arg {
            Val::Task { id, value: _ } => {
                #[cfg(feature = "concurrency")]
                {
                    match qcl_core::runtime::with_runtime(|runtime| {
                        runtime.block_on(runtime.join_task(*id))
                    }) {
                        Ok(result) => results.push(result),
                        Err(e) => return Err(anyhow!("Failed to await task: {}", e)),
                    }
                }
                #[cfg(not(feature = "concurrency"))]
                {
                    match value {
                        Some(val) => results.push(*val.clone()),
                        None => return Err(anyhow!("Task not completed")),
                    }
                }
            }
            _ => return Err(anyhow!("task::join_all() expects Task arguments")),
        }
    }

    Ok(Val::List(results.into()))
}

/// Sleep for the specified duration in milliseconds
fn task_sleep(args: &[Val], _env: &qcl_core::stmt::Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("task::sleep() expects exactly 1 argument"));
    }

    let duration_ms = match &args[0] {
        Val::Int(ms) => *ms,
        Val::Float(ms) => *ms as i64,
        _ => return Err(anyhow!("task::sleep() expects a numeric argument")),
    };

    #[cfg(feature = "concurrency")]
    {
        match qcl_core::runtime::with_runtime(|runtime| {
            let duration = std::time::Duration::from_millis(duration_ms as u64);
            runtime.block_on(async {
                tokio::time::sleep(duration).await;
                Ok(Val::Nil)
            })
        }) {
            Ok(result) => Ok(result),
            Err(e) => Err(anyhow!("Failed to sleep: {}", e)),
        }
    }
    #[cfg(not(feature = "concurrency"))]
    {
        // In non-concurrency mode, just return nil
        Ok(Val::Nil)
    }
}

/// Spawn a blocking task (CPU-intensive work)
fn task_spawn_blocking(
    args: &[Val],
    _env: &qcl_core::stmt::Environment,
    _ctx: &Val,
) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("task::spawn_blocking() expects exactly 1 argument"));
    }

    // Extract the function from the argument
    let func = match &args[0] {
        Val::RustFunction(f) => *f,
        Val::Closure { .. } => {
            return Err(anyhow!(
                "task::spawn_blocking() does not support closures yet"
            ));
        }
        _ => {
            return Err(anyhow!(
                "task::spawn_blocking() expects a function argument"
            ));
        }
    };

    #[cfg(feature = "concurrency")]
    {
        // Note: This is a simplified implementation that doesn't capture env/ctx
        // In a full implementation, we'd need to handle the lifetime issues
        match qcl_core::runtime::with_runtime(|runtime| {
            let future = async move {
                // For now, execute with empty context
                func(&[], &qcl_core::stmt::Environment::new(), &Val::Nil)
            };
            runtime.spawn(future)
        }) {
            Ok(task_id) => Ok(Val::Task {
                id: task_id,
                value: None,
            }),
            Err(e) => Err(anyhow!("Failed to spawn blocking task: {}", e)),
        }
    }
    #[cfg(not(feature = "concurrency"))]
    {
        // In non-concurrency mode, just execute the function synchronously
        func(&[], _env, _ctx)
    }
}
