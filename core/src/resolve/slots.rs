//! Slot-based name resolution scaffold.
//!
//! This module defines minimal types for annotating variables with slot indices
//! and a placeholder resolver that will later walk AST and fill slots.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarSlot {
    pub depth: u16,
    pub index: u16,
}

#[derive(Debug, Default)]
pub struct SlotResolver;

impl SlotResolver {
    pub fn new() -> Self { Self }

    /// Placeholder: in the future, this will analyze scopes and assign slots.
    /// Currently it is a no-op to keep the integration minimal.
    pub fn resolve_program_slots(&mut self) {}
}

