//! for types

use indexmap::IndexMap;
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};
use zrc_utils::span::{Spannable, Spanned};

use super::Scope;
use crate::tast::ty::Type as TastType;

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope or a key is
/// double-defined.
pub fn resolve_type<'input>(
    scope: &Scope<'input>,
    ty: ParserType<'input>,
) -> Result<TastType<'input>, Diagnostic> {
    let span = ty.0.span();
    Ok(match ty.0.into_value() {
        ParserTypeKind::Identifier(x) => {
            if let Some(ty) = scope.get_type(x) {
                ty.clone()
            } else {
                return Err(Diagnostic(
                    Severity::Error,
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use zrc_utils::spanned;

    use super::*;

    #[test]
    fn pointers_and_identifiers_resolve_as_expected() {
        assert_eq!(
            resolve_type(
                &Scope::from_scopes(HashMap::new(), HashMap::from([("i32", TastType::I32)])),
                ParserType(spanned!(
                    0,
                    ParserTypeKind::Ptr(Box::new(ParserType(spanned!(
                        1,
                        ParserTypeKind::Identifier("i32"),
                        4
                    )))),
                    4
                ))
            ),
            Ok(TastType::Ptr(Box::new(TastType::I32)))
        );
    }

    #[test]
    fn invalid_types_produce_error() {
        assert_eq!(
            resolve_type(
                &Scope::new(),
                ParserType(spanned!(0, ParserTypeKind::Identifier("x"), 1))
            ),
            Err(Diagnostic(
                Severity::Error,
                spanned!(0, DiagnosticKind::UnableToResolveType("x".to_string()), 1)
            ))
        );
    }

    #[test]
    fn structs_resolve_as_expected() {
        // struct { x: i32, y: i32 }
        assert_eq!(
            resolve_type(
                &Scope::default(),
                ParserType(spanned!(
                    0,
                    ParserTypeKind::Struct(spanned!(
                        7,
                        vec![
                            spanned!(
                                9,
                                (
                                    spanned!(9, "x", 10),
                                    ParserType(spanned!(12, ParserTypeKind::Identifier("i32"), 15))
                                ),
                                15
                            ),
                            spanned!(
                                17,
                                (
                                    spanned!(17, "y", 18),
                                    ParserType(spanned!(20, ParserTypeKind::Identifier("i32"), 23))
                                ),
                                23
                            )
                        ],
                        25
                    )),
                    25
                ))
            ),
            Ok(TastType::Struct(IndexMap::from([
                ("x", TastType::I32),
                ("y", TastType::I32)
            ])))
        );
    }
}
