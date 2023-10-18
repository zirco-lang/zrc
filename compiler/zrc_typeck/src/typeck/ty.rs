//! for types

use indexmap::IndexMap;
use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};
use zrc_utils::span::Spanned;

use super::Scope;
use crate::tast::ty::Type as TastType;

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope.
pub fn resolve_type(
    scope: &Scope,
    ty: ParserType,
) -> Result<TastType, zrc_diagnostics::Diagnostic> {
    Ok(match ty.0.value() {
        ParserTypeKind::Identifier(x) => {
            if let Some(t) = scope.get_type(x) {
                t.clone()
            } else {
                return Err(Diagnostic(
                    zrc_diagnostics::Severity::Error,
                    ty.0.map(|x| DiagnosticKind::UnableToResolveType(x.to_string())),
                ));
            }
        }
        ParserTypeKind::Ptr(t) => TastType::Ptr(Box::new(resolve_type(scope, *t.clone())?)),
        ParserTypeKind::Struct(members) => TastType::Struct(resolve_struct_keys(members)?),
    })
}

/// Resolve the types within the [`IndexMap`]s used by
/// [`ParserTypeKind::Struct`] and ensure keys are unique, returning the value
/// to be passed to [`TastType::Struct`].
///
/// # Errors
/// Errors if a key is not unique or is unresolvable.
#[allow(clippy::type_complexity)]
pub fn resolve_struct_keys(
    members: &Spanned<IndexMap<String, Spanned<(Spanned<String>, ParserType)>>>,
) -> Result<IndexMap<String, TastType>, Diagnostic> {
    let mut map = IndexMap::new();
    for (k, v) in members.value().iter() {
        if map.contains_key(k) {
            return Err(Diagnostic(
                zrc_diagnostics::Severity::Error,
                v.as_ref()
                    .map(|x| DiagnosticKind::DuplicateStructMember(x.0.value().clone())),
            ));
        }
        map.insert(
            k.clone(),
            resolve_type(&Scope::default(), v.value().1.clone())?,
        );
    }
    Ok(map)
}
