//! Scopes and other global typeck state

use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::IntoIter},
    rc::Rc,
};

use indexmap::IndexMap;
use zrc_utils::span::Span;

use crate::tast::ty::{FunctionDeclarationGlobalMetadata, Type as TastType};

/// Represents a typing scope: a scope that contains the mapping from a type's
/// name to its internal [`TastType`] representation.
#[derive(Debug, Clone, PartialEq)]
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

/// Represents a singular value in a value context.
#[derive(Debug, Clone, PartialEq)]
pub struct ValueEntry<'input> {
    /// The data type of the value
    pub ty: TastType<'input>,
    /// List of span locations where this value has been referenced.
    /// This is used for unused variable warnings in zircop and for future use
    /// in a language server.
    pub referenced_spans: Vec<Span>,
    /// The source span where this value was declared, for diagnostic purposes
    pub declaration_span: Span,
}
impl<'input> ValueEntry<'input> {
    /// Create a used value entry with an initial reference span
    #[must_use]
    pub fn used(ty: TastType<'input>, declaration_span: Span, reference_span: Span) -> Self {
        Self {
            ty,
            referenced_spans: vec![reference_span],
            declaration_span,
        }
    }

    /// Create an unused value entry
    #[must_use]
    pub const fn unused(ty: TastType<'input>, declaration_span: Span) -> Self {
        Self {
            ty,
            referenced_spans: Vec::new(),
            declaration_span,
        }
    }
}

/// Represents a value scope: a scope that contains the mapping from an
/// identifier to its contained data type.
#[derive(Debug, Clone, PartialEq)]
pub struct ValueCtx<'input> {
    /// Mappings from identifier to its contained data [`TastType`].
    ///
    /// Each entry is stored behind an `Rc<RefCell<...>>` so that when a
    /// subscope clones the `ValueCtx` the entries remain shared and mutations
    /// (for example marking a variable as `used`) are visible to parent and
    /// sibling scopes.
    mappings: HashMap<&'input str, Rc<RefCell<ValueEntry<'input>>>>,
}
impl Default for ValueCtx<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'input> ValueCtx<'input> {
    /// Create a new empty [`ValueScope`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }

    /// Create a new [`ValueScope`] from a [`HashMap`] mapping identifier [str]s
    /// to data types
    #[must_use]
    pub fn from_mappings(mappings: HashMap<&'input str, ValueEntry<'input>>) -> Self {
        Self {
            mappings: mappings
                .into_iter()
                .map(|(k, v)| (k, Rc::new(RefCell::new(v))))
                .collect(),
        }
    }

    /// Create a new [`ValueScope`] from a [`HashMap`] mapping identifier [str]s
    /// to data types, assuming they are all unused.
    ///
    /// This generates the invalid span 0..0 for all entries, so it is for
    /// testing only.
    #[must_use]
    #[cfg(test)]
    pub fn from_unused_mappings(mappings: HashMap<&'input str, TastType<'input>>) -> Self {
        Self {
            mappings: mappings
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        Rc::new(RefCell::new(ValueEntry::unused(
                            v,
                            Span::from_positions_and_file(0, 0, "<test>"),
                        ))),
                    )
                })
                .collect(),
        }
    }

    /// Determine if a variable exists by a given name
    #[must_use]
    pub fn has(&self, identifier: &'input str) -> bool {
        self.mappings.contains_key(identifier)
    }

    #[must_use]
    /// Resolve an identifier to a shared `Rc<RefCell<ValueEntry>>`.
    ///
    /// Returns a cloned `Rc` so callers can borrow as needed.
    pub fn resolve(&self, identifier: &'input str) -> Option<Rc<RefCell<ValueEntry<'input>>>> {
        self.mappings.get(identifier).cloned()
    }

    /// Access the mutable [`ValueEntry`] for a given identifier
    /// Access the entry as a shared `Rc<RefCell<...>>` so mutation can occur
    /// through interior mutability even if the surrounding `GlobalScope` is
    /// immutably borrowed.
    #[expect(clippy::needless_pass_by_ref_mut)]
    pub fn resolve_mut(
        &mut self,
        identifier: &'input str,
    ) -> Option<Rc<RefCell<ValueEntry<'input>>>> {
        self.mappings.get(identifier).cloned()
    }

    /// Create a new variable with a given name and data [`TastType`]
    pub fn insert(&mut self, identifier: &'input str, resolution: ValueEntry<'input>) {
        self.mappings
            .insert(identifier, Rc::new(RefCell::new(resolution)));
    }

    /// Convert a [`HashMap`] of unused variable mappings into a value context
    ///
    /// This generates the invalid span 0..0 for all entries, so it is for
    /// testing only.
    #[must_use]
    #[cfg(test)]
    pub fn from_unused<T>(mappings: T) -> Self
    where
        T: Into<HashMap<&'input str, TastType<'input>>>,
    {
        Self::from_unused_mappings(mappings.into())
    }

    /// Iterate over the entries in this value context
    pub fn iter(&self) -> impl Iterator<Item = (&'input str, Rc<RefCell<ValueEntry<'input>>>)> {
        // Clone the Rc so the iterator yields owned `Rc<RefCell<...>>` values
        // that callers can borrow from without borrowing `self` for the whole
        // loop.
        self.mappings.iter().map(|(k, v)| (*k, v.clone()))
    }
}
impl<'input, T> From<T> for ValueCtx<'input>
where
    T: Into<HashMap<&'input str, ValueEntry<'input>>>,
{
    fn from(value: T) -> Self {
        Self::from_mappings(value.into())
    }
}
impl<'input> IntoIterator for ValueCtx<'input> {
    type Item = (&'input str, ValueEntry<'input>);
    type IntoIter = IntoIter<&'input str, ValueEntry<'input>>;

    fn into_iter(self) -> Self::IntoIter {
        // Convert the Rc<RefCell<ValueEntry>> back into owned ValueEntry
        // values for consumers of `into_iter`.
        let owned: HashMap<&'input str, ValueEntry<'input>> = self
            .mappings
            .into_iter()
            .map(|(k, v)| (k, v.borrow().clone()))
            .collect();
        owned.into_iter()
    }
}

/// Represents the "global scope" of a single Zirco program.
///
/// The global scope contains all of the things that must be defined at the
/// top-level: information about function declarations, type aliases, etc.
/// subscopes can then be created off of a global scope to represent a
/// function or block scope, which can resolve (but not mutate) the global data.
// Cloning this would be an error, so it does not derive [Clone].
#[derive(Debug)]
pub struct GlobalScope<'input> {
    /// Maps every type name to its representation
    pub types: TypeCtx<'input>,

    /// Maps every global value (static and function) to its data type
    pub global_values: ValueCtx<'input>,

    /// Contains data about every global [`crate::tast::ty::Fn`]
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

    /// Create a subscope from this [`GlobalScope`].
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
/// type context of the [`GlobalScope`] they were created within.
///
/// Subscopes should only be created with the [`GlobalScope::create_subscope`]
/// method.
// Cloning is proper behavior for creating a subscope off of another.
#[derive(Debug, Clone, PartialEq)]
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
