pub mod ast;
pub mod expr;
pub mod module;
mod op;
pub mod rt;
pub mod stmt;
pub mod token;
pub mod typ;
pub mod val;

// Optional subsystems, gated by features
// Register bytecode VM
#[cfg(feature = "vm")]
pub mod vm;

// Name resolution to slot indices
#[cfg(feature = "slots")]
pub mod resolve;
