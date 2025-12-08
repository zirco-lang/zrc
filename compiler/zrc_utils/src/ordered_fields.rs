//! The declaration ordered fields of a struct or union type or instantiation

/// The declaration ordered fields of a struct or union type or instantiation
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct OrderedFields<'input, T> {
    /// The ordered fields
    pub fields: Vec<(&'input str, T)>,
}
impl<'input, T> OrderedFields<'input, T> {
    /// Creates a new, empty `OrderedFields`.
    #[must_use]
    pub const fn new() -> Self {
        Self { fields: Vec::new() }
    }

    /// Creates a new `OrderedFields` from the given fields.
    #[must_use]
    pub const fn from(fields: Vec<(&'input str, T)>) -> Self {
        Self { fields }
    }

    /// Gets the number of fields.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.fields.len()
    }

    /// Checks if there are no fields.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Returns `true` if the ordered fields contains the given key.
    #[must_use]
    pub fn contains_key(&self, key: &str) -> bool {
        self.fields.iter().any(|(k, _)| k == &key)
    }

    /// Inserts a new field, or updates an existing one.
    pub fn insert(&mut self, key: &'input str, value: T) {
        for (k, v) in &mut self.fields {
            if k == &key {
                *v = value;
                return;
            }
        }
        self.fields.push((key, value));
    }

    /// Gets a reference to a field by key.
    #[must_use]
    pub fn get(&self, key: &str) -> Option<&T> {
        for (k, v) in &self.fields {
            if k == &key {
                return Some(v);
            }
        }
        None
    }

    /// Iterate over the fields in order.
    pub fn iter(&self) -> impl Iterator<Item = (&'input str, &T)> {
        self.fields.iter().map(|(k, v)| (*k, v))
    }
}
impl<T> Default for OrderedFields<'_, T> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'input, T> IntoIterator for OrderedFields<'input, T> {
    type Item = (&'input str, T);
    type IntoIter = std::vec::IntoIter<(&'input str, T)>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}
impl<'input, T> FromIterator<(&'input str, T)> for OrderedFields<'input, T> {
    fn from_iter<I: IntoIterator<Item = (&'input str, T)>>(iter: I) -> Self {
        let mut of = OrderedFields::new();
        for (k, v) in iter {
            of.insert(k, v);
        }
        of
    }
}
