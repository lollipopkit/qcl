//! Register bytecode VM subsystem (feature = "vm")
//!
//! This module is optional and compiled only when the `vm` feature is enabled.
//! It contains a minimal scaffold for bytecode definitions, a compiler stub,
//! and the VM execution loop placeholder to enable incremental development.

mod bytecode;
mod compiler;
#[allow(clippy::module_inception)]
mod vm;

pub use bytecode::*;
pub use compiler::*;
pub use vm::*;

#[cfg(test)]
mod vm_test;
