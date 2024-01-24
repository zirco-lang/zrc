//! for types

use indexmap::IndexMap;
use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::ty::{KeyTypeMapping, Type as ParserType, TypeKind as ParserTypeKind};

use super::scope::TypeCtx;
use crate::tast::ty::Type as TastType;

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope or a key is
/// double-defined.
pub fn resolve_type<'input>(
    type_scope: &TypeCtx<'input>,
    ty: ParserType<'input>,
) -> Result<TastType<'input>, Diagnostic> {
    let span = ty.0.span();
    Ok(match ty.0.into_value() {
        ParserTypeKind::Identifier(x) => {
            if let Some(ty) = type_scope.resolve(x) {
                ty.clone()
            } else {
                return Err(DiagnosticKind::UnableToResolveType(x.to_string()).error_in(span));
            }
        }
        ParserTypeKind::Ptr(pointee_ty) => {
            TastType::Ptr(Box::new(resolve_type(type_scope, *pointee_ty)?))
        }
        ParserTypeKind::Struct(members) => {
            TastType::Struct(resolve_key_type_mapping(type_scope, members)?)
        }
        ParserTypeKind::Union(members) => {
            TastType::Union(resolve_key_type_mapping(type_scope, members)?)
        }
    })
}

/// Resolve the types within the [`IndexMap`]s used by
/// [`ParserTypeKind::Struct`] and ensure keys are unique, returning the value
/// to be passed to [`TastType::Struct`].
///
/// # Errors
/// Errors if a key is not unique or is unresolvable.
#[allow(clippy::type_complexity)]
pub(super) fn resolve_key_type_mapping<'input>(
    type_scope: &TypeCtx<'input>,
    members: KeyTypeMapping<'input>,
) -> Result<IndexMap<&'input str, TastType<'input>>, Diagnostic> {
    let mut map: IndexMap<&'input str, TastType> = IndexMap::new();
    for member in members.0.into_value() {
        let span = member.span();
        let (key, ast_type) = member.into_value();

        if map.contains_key(key.value()) {
            return Err(
                DiagnosticKind::DuplicateStructMember(key.into_value().to_string()).error_in(span),
            );
        }
        map.insert(key.value(), resolve_type(type_scope, ast_type)?);
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use zrc_diagnostics::Severity;
    use zrc_parser::ast::ty::KeyTypeMapping;
    use zrc_utils::{span::Span, spanned};

    use super::*;

    #[test]
    fn pointers_and_identifiers_resolve_as_expected() {
        assert_eq!(
            resolve_type(
                &TypeCtx::from([("i32", TastType::I32)]),
                ParserType::build_ptr(
                    Span::from_positions(0, 4),
                    ParserType::build_ident(spanned!(1, "i32", 4)),
                ),
            ),
            Ok(TastType::Ptr(Box::new(TastType::I32)))
        );
    }

    #[test]
    fn invalid_types_produce_error() {
        assert_eq!(
            resolve_type(
                &TypeCtx::new_empty(),
                ParserType::build_ident(spanned!(0, "x", 1))
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
                &TypeCtx::default(),
                ParserType(spanned!(
                    0,
                    ParserTypeKind::Struct(KeyTypeMapping(spanned!(
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
                    ))),
                    25
                ))
            ),
            Ok(TastType::Struct(IndexMap::from([
                ("x", TastType::I32),
                ("y", TastType::I32)
            ])))
        );
    }

    #[test]
    fn duplicate_keys_in_struct_causes_error() {
        // struct { x: i32, x: i32 }
        assert_eq!(
            resolve_type(
                &TypeCtx::default(),
                ParserType::build_struct_from_contents(
                    Span::from_positions(0, 25),
                    KeyTypeMapping(spanned!(
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
                                    spanned!(17, "x", 18),
                                    ParserType(spanned!(20, ParserTypeKind::Identifier("i32"), 23))
                                ),
                                23
                            )
                        ],
                        25
                    )),
                )
            ),
            Err(Diagnostic(
                Severity::Error,
                spanned!(
                    17,
                    DiagnosticKind::DuplicateStructMember("x".to_string()),
                    23
                )
            ))
        );
    }
}
