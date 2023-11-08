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
use zrc_parser::ast::stmt::Declaration as AstDeclaration;
use zrc_utils::span::Spanned;

use crate::tast::{stmt::TypedDeclaration, ty::Type as TastType};

/// Contains scoping information during type checking.
///
/// Cloning it resembles the creation of a subscope.
#[derive(Debug, Clone)]
pub struct Scope<'input> {
    /// Maps identifiers for values to their types.
    value_scope: HashMap<&'input str, TastType<'input>>,
    /// Maps type names to their actual type representations.
    type_scope: HashMap<&'input str, TastType<'input>>,
}
impl<'input> Scope<'input> {
    /// Creates a new Scope from just the defaults
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new Scope without the primitives
    #[must_use]
    pub fn new_empty() -> Self {
        Self {
            value_scope: HashMap::new(),
            type_scope: HashMap::new(),
        }
    }

    /// Creates a new Scope from two [`HashMap`]s.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn from_scopes(
        value_scope: HashMap<&'input str, TastType<'input>>,
        type_scope: HashMap<&'input str, TastType<'input>>,
    ) -> Self {
        Self {
            value_scope,
            type_scope,
        }
    }

    /// Gets a value-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_value<'a>(&'a self, identifier: &'input str) -> Option<&'a TastType<'input>> {
        self.value_scope.get(identifier)
    }

    /// Gets a type-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_type<'a>(&'a self, identifier: &'input str) -> Option<&'a TastType<'input>> {
        self.type_scope.get(identifier)
    }

    /// Sets a value-identifier's type in this [`Scope`]
    pub fn set_value(&mut self, identifier: &'input str, ty: TastType<'input>) {
        self.value_scope.insert(identifier, ty);
    }

    /// Sets a type-identifier's type in this [`Scope`]
    pub fn set_type(&mut self, identifier: &'input str, ty: TastType<'input>) {
        self.type_scope.insert(identifier, ty);
    }
}

impl<'input> Default for Scope<'input> {
    fn default() -> Self {
        Self::from_scopes(
            HashMap::from([]),
            HashMap::from([
                ("i8", TastType::I8),
                ("u8", TastType::U8),
                ("i16", TastType::I16),
                ("u16", TastType::U16),
                ("i32", TastType::I32),
                ("u32", TastType::U32),
                ("i64", TastType::I64),
                ("u64", TastType::U64),
                ("bool", TastType::Bool),
                // void is not producible
            ]),
        )
    }
}

/// # Errors
/// Errors with type checker errors.
pub fn type_program(
    program: Vec<Spanned<AstDeclaration>>,
) -> Result<Vec<TypedDeclaration>, zrc_diagnostics::Diagnostic> {
    let mut scope = Scope::new();

    program
        .into_iter()
        .filter_map(|declaration| {
            process_declaration(&mut scope, declaration.into_value()).transpose()
        })
        .collect()
}
