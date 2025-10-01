//! Register bytecode VM subsystem (feature = "vm")
//!
//! This module is optional and compiled only when the `vm` feature is enabled.
//! It contains a minimal scaffold for bytecode definitions, a compiler stub,
//! and the VM execution loop placeholder to enable incremental development.

mod bytecode;
#[cfg(feature = "bc32")]
mod bc32;
mod compiler;
#[allow(clippy::module_inception)]
mod vm;

pub use bytecode::*;
#[cfg(feature = "bc32")]
pub use bc32::*;
pub use compiler::*;
pub use vm::*;

#[cfg(test)]
mod vm_test;
