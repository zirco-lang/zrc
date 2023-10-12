//! Zirco programming language type checker

mod block;
mod expr;
mod ty;

use std::collections::HashMap;

pub use block::{
    process_declaration, type_block, BlockReturnAbility, BlockReturnActuality, BlockReturnType,
};
pub use expr::type_expr;
pub use ty::resolve_type;
use zrc_parser::ast::{stmt::Declaration as AstDeclaration, Spanned};

use crate::tast::{stmt::TypedDeclaration, ty::Type as TastType};

/// Contains scoping information during type checking.
///
/// Cloning it resembles the creation of a subscope.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Maps identifiers for values to their types.
    value_scope: HashMap<String, TastType>,
    /// Maps type names to their actual type representations.
    type_scope: HashMap<String, TastType>,
}
impl Scope {
    /// Creates a new Scope with no values or types.
    #[must_use]
    pub fn new() -> Self {
        Self {
            value_scope: HashMap::new(),
            type_scope: HashMap::new(),
        }
    }

    /// Creates a new Scope from two [`HashMap`]s.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn from_scopes(
        value_scope: HashMap<String, TastType>,
        type_scope: HashMap<String, TastType>,
    ) -> Self {
        Self {
            value_scope,
            type_scope,
        }
    }

    /// Gets a value-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_value(&self, identifier: &String) -> Option<&TastType> {
        self.value_scope.get(identifier)
    }

    /// Gets a type-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_type(&self, identifier: &String) -> Option<&TastType> {
        self.type_scope.get(identifier)
    }

    /// Sets a value-identifier's type in this [`Scope`]
    pub fn set_value(&mut self, identifier: String, ty: TastType) {
        self.value_scope.insert(identifier, ty);
    }

    /// Sets a type-identifier's type in this [`Scope`]
    pub fn set_type(&mut self, identifier: String, ty: TastType) {
        self.type_scope.insert(identifier, ty);
    }
}
impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

/// # Errors
/// Errors with type checker errors.
pub fn type_program(
    program: Vec<Spanned<AstDeclaration>>,
) -> anyhow::Result<Vec<TypedDeclaration>> {
    let mut scope = Scope::new();

    program
        .into_iter()
        .map(|declaration| process_declaration(&mut scope, declaration.1))
        .collect()
}
