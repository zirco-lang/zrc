//! Defines the `CgScope` struct and related utilities

use std::collections::HashMap;

use inkwell::values::PointerValue;

/// Represents the code generation scope, or the mapping from identifiers to
/// their LLVM [`PointerValue`]s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CgScope<'input, 'ctx> {
    /// The contained mappings from identifiers to [`PointerValue`]s
    pub identifiers: HashMap<&'input str, PointerValue<'ctx>>,
}
impl<'input, 'ctx> CgScope<'input, 'ctx> {
    /// Get the [`PointerValue`] of a particular identifier, if it exists
    pub fn get(&self, id: &'input str) -> Option<PointerValue<'ctx>> {
        self.identifiers.get(id).copied()
    }

    /// Insert a [`PointerValue`] of an identifier into the scope
    pub fn insert(&mut self, id: &'input str, value: PointerValue<'ctx>) {
        self.identifiers.insert(id, value);
    }

    /// Create a new [`CgScope`] with no values
    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
        }
    }
}

impl Default for CgScope<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}
