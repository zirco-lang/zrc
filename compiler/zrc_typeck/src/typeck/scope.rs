//! Scopes and other global typeck state

use std::collections::HashMap;

use indexmap::IndexMap;

use crate::tast::ty::{FunctionDeclarationGlobalMetadata, Type as TastType};

/// Represents a typing scope: a scope that contains the mapping from a type's
/// name to its internal [`TastType`] representation.
#[derive(Debug, Clone)]
pub struct TypeCtx<'input> {
    /// Mappings from type name to its [`TastType`]
    mappings: HashMap<&'input str, TastType<'input>>,
}
/// All types namable in the global scope
/// Returns all types namable in the global scope
fn all_namable_types<'input>() -> [(&'input str, TastType<'input>); 12] {
    [
        ("i8", TastType::I8),
        ("u8", TastType::U8),
        ("i16", TastType::I16),
        ("u16", TastType::U16),
        ("i32", TastType::I32),
        ("u32", TastType::U32),
        ("i64", TastType::I64),
        ("u64", TastType::U64),
        ("isize", TastType::Isize),
        ("usize", TastType::Usize),
        ("bool", TastType::Bool),
        ("void", TastType::Struct(IndexMap::new())),
    ]
}
impl<'input> TypeCtx<'input> {
    /// Create a new [`TypeScope`] containing **NOTHING** -- not even
    /// primitives.
    #[must_use]
    pub fn new_empty() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }

    /// Create a new [`TypeScope`] containing just the primitives
    #[must_use]
    pub fn new() -> Self {
        Self::from(all_namable_types())
    }

    /// Create a new [`TypeScope`] from a [`HashMap`] mapping identifier [str]s
    /// to [`TastType`]s
    #[must_use]
    pub const fn from_mappings(mappings: HashMap<&'input str, TastType<'input>>) -> Self {
        Self { mappings }
    }

    /// Create a new [`TypeScope`] from a [`HashMap`] mapping identifier [str]s
    /// to [`TastType`]s
    #[must_use]
    pub fn from_defaults_and_mappings(mappings: HashMap<&'input str, TastType<'input>>) -> Self {
        Self {
            mappings: all_namable_types()
                .iter()
                .cloned()
                .chain(mappings)
                .collect(),
        }
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
impl Default for TypeCtx<'static> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'input, T> From<T> for TypeCtx<'input>
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
pub struct ValueCtx<'input> {
    /// Mappings from identifier to its contained data [`TastType`]
    mappings: HashMap<&'input str, TastType<'input>>,
}
impl<'input> ValueCtx<'input> {
    /// Create a new empty [`ValueScope`].
    // Does not impl [Default] because a default would be misleading: the "empty" scope is more
    // accurate.
    #[allow(clippy::new_without_default)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }

    /// Create a new [`ValueScope`] from a [`HashMap`] mapping identifier [str]s
    /// to data types
    #[must_use]
    pub const fn from_mappings(mappings: HashMap<&'input str, TastType<'input>>) -> Self {
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
impl<'input, T> From<T> for ValueCtx<'input>
where
    T: Into<HashMap<&'input str, TastType<'input>>>,
{
    fn from(value: T) -> Self {
        Self {
            mappings: value.into(),
        }
    }
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
    pub types: TypeCtx<'input>,

    /// Maps every global value (static and function) to its data type
    pub global_values: ValueCtx<'input>,

    /// Contains data about every global [`tast::ty::Fn`]
    pub declarations: HashMap<&'input str, FunctionDeclarationGlobalMetadata<'input>>,
}
impl<'input> GlobalScope<'input> {
    /// Create a new [`GlobalScope`] containing nothing -- not even primitives.
    /// This is most useful for testing.
    #[must_use]
    pub fn new_empty() -> Self {
        GlobalScope {
            types: TypeCtx::new_empty(),
            global_values: ValueCtx::new(),
            declarations: HashMap::new(),
        }
    }

    /// Create a new [`GlobalScope`] with just the primitive types. This is the
    /// normal initialization method.
    #[must_use]
    pub fn new() -> Self {
        GlobalScope {
            types: TypeCtx::new(),
            global_values: ValueCtx::new(),
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
    pub values: ValueCtx<'input>,

    /// Maps every type name from the parent [`GlobalScope`] to its
    /// representation
    pub types: &'gs TypeCtx<'input>,
}
impl<'input, 'gs> Scope<'input, 'gs> {
    /// Creates a new [`Scope`] from a parent [`GlobalScope`]
    fn from_global_scope(global_scope: &'gs GlobalScope<'input>) -> Self {
        Scope {
            values: global_scope.global_values.clone(),
            types: &global_scope.types,
        }
    }
}
