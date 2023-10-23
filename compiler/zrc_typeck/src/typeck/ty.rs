//! for types

use indexmap::IndexMap;
use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};
use zrc_utils::span::{Spannable, Spanned};

use super::Scope;
use crate::tast::ty::Type as TastType;

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope.
pub fn resolve_type<'input>(
    scope: &Scope<'input>,
    ty: ParserType<'input>,
) -> Result<TastType<'input>, zrc_diagnostics::Diagnostic> {
    let span = ty.0.span();
    Ok(match ty.0.into_value() {
        ParserTypeKind::Identifier(x) => {
            if let Some(ty) = scope.get_type(x) {
                ty.clone()
            } else {
                return Err(Diagnostic(
                    zrc_diagnostics::Severity::Error,
                    DiagnosticKind::UnableToResolveType(x.to_string()).in_span(span),
                ));
            }
        }
        ParserTypeKind::Ptr(pointee_ty) => {
            TastType::Ptr(Box::new(resolve_type(scope, *pointee_ty)?))
        }
        ParserTypeKind::Struct(members) => TastType::Struct(resolve_struct_keys(
            scope,
            members.map(|x| x.into_iter().collect::<Vec<_>>()),
        )?),
    })
}

/// Resolve the types within the [`IndexMap`]s used by
/// [`ParserTypeKind::Struct`] and ensure keys are unique, returning the value
/// to be passed to [`TastType::Struct`].
///
/// # Errors
/// Errors if a key is not unique or is unresolvable.
#[allow(clippy::type_complexity)]
pub fn resolve_struct_keys<'input>(
    scope: &Scope<'input>,
    members: Spanned<Vec<Spanned<(Spanned<&'input str>, ParserType<'input>)>>>,
) -> Result<IndexMap<&'input str, TastType<'input>>, Diagnostic> {
    let mut map: IndexMap<&'input str, TastType> = IndexMap::new();
    for sp in members.into_value() {
        let (key, ast_type) = sp.value();
        if map.contains_key(key.value()) {
            return Err(Diagnostic(
                zrc_diagnostics::Severity::Error,
                sp.as_ref()
                    .map(|x| DiagnosticKind::DuplicateStructMember((*x.0.value()).to_string())),
            ));
        }
        map.insert(key.value(), resolve_type(scope, ast_type.clone())?);
    }
    Ok(map)
}
