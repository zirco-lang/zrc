//! for types

use std::collections::HashMap;

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};

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
        ParserTypeKind::Struct(members) => TastType::Struct(
            members
                .iter()
                .map(|(k, v)| Ok((k.clone(), resolve_type(scope, v.clone())?)))
                .collect::<Result<HashMap<String, TastType>, Diagnostic>>()?,
        ),
    })
}
