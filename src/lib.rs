#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod ast;
#[cfg(feature = "std")]
pub mod de;
pub mod error;
pub mod expr;
#[cfg(feature = "ffi")]
pub mod ffi;
pub mod op;
#[cfg(feature = "python")]
pub mod python;
pub mod token;
pub mod val;
#[cfg(feature = "wasm")]
pub mod wasm;

mod ast_test;
#[cfg(feature = "std")]
mod de_test;
mod expr_test;
mod op_test;
mod token_test;
mod val_test;
