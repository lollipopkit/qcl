//! Register bytecode VM subsystem (feature = "vm")
//!
//! This module is optional and compiled only when the `vm` feature is enabled.
//! It contains a minimal scaffold for bytecode definitions, a compiler stub,
//! and the VM execution loop placeholder to enable incremental development.

mod bytecode;
mod compiler;
mod vm;

pub use bytecode::*;
pub use compiler::*;
pub use vm::*;

