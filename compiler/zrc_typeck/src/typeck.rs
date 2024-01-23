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

use crate::tast::{self, stmt::TypedDeclaration, ty::Type as TastType};

/// Represents a typing scope: a scope that contains the mapping from a type's
/// name to its internal [`TastType`] representation.
#[derive(Debug, Clone)]
pub struct TypeScope<'input> {
    /// Mappings from type name to its [TastType]
    mappings: HashMap<&'input str, TastType<'input>>,
}
impl<'input> TypeScope<'input> {
    /// Create a new [`TypeScope`] containing **NOTHING** -- not even
    /// primitives.
    #[must_use]
    pub fn new_empty() -> TypeScope<'input> {
        Self {
            mappings: HashMap::new(),
        }
    }

    /// Create a new [`TypeScope`] containing just the primitives
    #[must_use]
    pub fn new() -> TypeScope<'input> {
        Self::from([
            // all namable types
            ("i8", TastType::I8),
            ("u8", TastType::U8),
            ("i16", TastType::I16),
            ("u16", TastType::U16),
            ("i32", TastType::I32),
            ("u32", TastType::U32),
            ("i64", TastType::I64),
            ("u64", TastType::U64),
            ("bool", TastType::Bool),
            // void is not namable
        ])
    }

    /// Create a new [`TypeScope`] from a [`HashMap`] mapping identifier [str]s
    /// to [`TastType`]s
    #[must_use]
    pub const fn from_mappings(
        mappings: HashMap<&'input str, TastType<'input>>,
    ) -> TypeScope<'input> {
        Self { mappings }
    }

    /// Determine if a type exists by a given name
    #[must_use]
    pub fn has(&self, identifier: &'input str) -> bool {
        self.mappings.contains_key(identifier)
    }

    /// Resolve an identifier [str] (a name of a type) to its representative
    /// [`TastType`]
    #[must_use]
    pub fn resolve(&self, identifier: &'input str) -> Option<&TastType<'input>> {
        self.mappings.get(identifier)
    }

    /// Create a new type with a given name and resolved [`TastType`]
    pub fn insert(&mut self, identifier: &'input str, resolution: TastType<'input>) {
        self.mappings.insert(identifier, resolution);
    }
}
impl Default for TypeScope<'static> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'input, T> From<T> for TypeScope<'input>
where
    T: Into<HashMap<&'input str, TastType<'input>>>,
{
    fn from(value: T) -> Self {
        Self {
            mappings: value.into(),
        }
    }
}

/// Represents a value scope: a scope that contains the mapping from an
/// identifier to its contained data type.

#[derive(Debug, Clone)]
pub struct ValueScope<'input> {
    /// Mappings from identifier to its contained data [TastType]
    mappings: HashMap<&'input str, TastType<'input>>,
}
impl<'input> ValueScope<'input> {
    /// Create a new empty [`ValueScope`].
    // Does not impl [Default] because a default would be misleading: the "empty" scope is more
    // accurate.
    #[allow(clippy::new_without_default)]
    #[must_use]
    pub fn new() -> ValueScope<'input> {
        Self {
            mappings: HashMap::new(),
        }
    }

    /// Create a new [`ValueScope`] from a [`HashMap`] mapping identifier [str]s
    /// to data types
    #[must_use]
    pub const fn from_mappings(
        mappings: HashMap<&'input str, TastType<'input>>,
    ) -> ValueScope<'input> {
        Self { mappings }
    }

    /// Determine if a variable exists by a given name
    #[must_use]
    pub fn has(&self, identifier: &'input str) -> bool {
        self.mappings.contains_key(identifier)
    }

    /// Determine the data [`TastType`] of a variable
    #[must_use]
    pub fn resolve(&self, identifier: &'input str) -> Option<&TastType<'input>> {
        self.mappings.get(identifier)
    }

    /// Create a new variable with a given name and data [`TastType`]
    pub fn insert(&mut self, identifier: &'input str, resolution: TastType<'input>) {
        self.mappings.insert(identifier, resolution);
    }
}
impl<'input, T> From<T> for ValueScope<'input>
where
    T: Into<HashMap<&'input str, TastType<'input>>>,
{
    fn from(value: T) -> Self {
        Self {
            mappings: value.into(),
        }
    }
}

/// Auxillary data attached to a [`tast::ty::Fn`] in the [`GlobalScope`]
#[derive(Debug)]
pub struct FunctionDeclarationGlobalMetadata<'input> {
    /// The corresponding [`Fn`] we wrap
    fn_type: tast::ty::Fn<'input>,
    /// If a declaration exists to implement this function
    /// (Only one may exist)
    has_implementation: bool,
}

/// Represents the "global scope" of a single Zirco program.
///
/// The global scope contains all of the things that must be defined at the
/// top-level: information about function declarations, type aliases, etc.
/// Sub-[Scope]s can then be created off of a global scope to represent a
/// function or block scope, which can resolve (but not mutate) the global data.
// Cloning this would be an error, so it does not derive [Clone].
#[derive(Debug)]
pub struct GlobalScope<'input> {
    /// Maps every type name to its representation
    pub types: TypeScope<'input>,

    /// Maps every global value (static and function) to its data type
    pub global_values: ValueScope<'input>,

    /// Contains data about every global [`tast::ty::Fn`]
    pub declarations: HashMap<&'input str, FunctionDeclarationGlobalMetadata<'input>>,
}
impl<'input> GlobalScope<'input> {
    /// Create a new [`GlobalScope`] containing nothing -- not even primitives.
    /// This is most useful for testing.
    #[must_use]
    pub fn new_empty() -> GlobalScope<'input> {
        GlobalScope {
            types: TypeScope::new_empty(),
            global_values: ValueScope::new(),
            declarations: HashMap::new(),
        }
    }

    /// Create a new [`GlobalScope`] with just the primitive types. This is the
    /// normal initialization method.
    #[must_use]
    pub fn new() -> GlobalScope<'input> {
        GlobalScope {
            types: TypeScope::new(),
            global_values: ValueScope::new(),
            declarations: HashMap::new(),
        }
    }

    /// Create a [Subscope] from this [`GlobalScope`].
    #[must_use]
    pub fn create_subscope<'gs>(&'gs self) -> Scope<'input, 'gs> {
        Scope::from_global_scope(self)
    }
}
impl Default for GlobalScope<'static> {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a sub-scope within a Zirco program. Functions, blocks, etc. all
/// have their own sub-scopes.
///
/// Subscopes are special because they cannot declare types: they simply use the
/// [`TypeScope`] of the [`GlobalScope`] they were created within.
///
/// Subscopes should only be created with the [`GlobalScope::create_subscope`]
/// method.
// Cloning is proper behavior for creating a subscope off of another.
#[derive(Debug, Clone)]
pub struct Scope<'input, 'gs> {
    /// Maps every variable to its data type
    pub values: ValueScope<'input>,

    /// Maps every type name from the parent [`GlobalScope`] to its
    /// representation
    pub types: &'gs TypeScope<'input>,
}
impl<'input, 'gs> Scope<'input, 'gs> {
    /// Creates a new [`Scope`] from a parent [`GlobalScope`]
    fn from_global_scope(global_scope: &'gs GlobalScope<'input>) -> Scope<'input, 'gs> {
        Scope {
            values: global_scope.global_values.clone(),
            types: &global_scope.types,
        }
    }
}

/// # Errors
/// Errors with type checker errors.
pub fn type_program(
    program: Vec<Spanned<AstDeclaration>>,
) -> Result<Vec<Spanned<TypedDeclaration>>, zrc_diagnostics::Diagnostic> {
    let mut global_scope = GlobalScope::new();

    program
        .into_iter()
        .filter_map(|declaration| {
            declaration
                .map(|declaration| process_declaration(&mut global_scope, declaration).transpose())
                .transpose()
                .map(zrc_utils::span::Spanned::<Result<_, _>>::transpose)
        })
        // drop the redundant/erroneous error spans
        .map(|x| x.map_err(zrc_utils::span::Spanned::into_value))
        .collect()
}
