use std::collections::HashMap;

use inkwell::values::PointerValue;

pub mod expr;
pub mod stmt;
pub mod ty;

pub use stmt::cg_program;

#[derive(Debug, Clone)]
pub struct CgScope<'input, 'ctx> {
    identifiers: HashMap<&'input str, PointerValue<'ctx>>,
}
impl<'input, 'ctx> CgScope<'input, 'ctx> {
    pub fn get(&self, id: &'input str) -> Option<PointerValue<'ctx>> {
        self.identifiers.get(id).copied()
    }

    pub fn insert(&mut self, id: &'input str, value: PointerValue<'ctx>) {
        self.identifiers.insert(id, value);
    }

    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
        }
    }
}

impl<'input, 'ctx> Default for CgScope<'input, 'ctx> {
    fn default() -> Self {
        Self::new()
    }
}
