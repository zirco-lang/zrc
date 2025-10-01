//! for types

use indexmap::IndexMap;
use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::ty::{KeyTypeMapping, Type as ParserType, TypeKind as ParserTypeKind};
use zrc_utils::span::Span;

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
        ParserTypeKind::Enum(members) => {
            // Desugar an enum into its represented internal struct
            TastType::Struct(IndexMap::from([
                ("__discriminant__", TastType::Usize),
                (
                    "__value__",
                    (TastType::Union(resolve_key_type_mapping(type_scope, members)?)),
                ),
            ]))
        }
    })
}

/// Resolve an identifier to its corresponding [`tast::ty::Type`], allowing
/// opaque references to the type being defined.
///
/// This function is used during type resolution for type alias declarations
/// to allow self-referential types. When resolving a type like
/// `struct { value: i32, next: *Node }` where `Node` is the type being defined,
/// any reference to `Node` within the type will be replaced with
/// <code>[TastType::Opaque]("Node")</code> as a placeholder.
///
/// The opaque types are later validated (must be behind pointers only) and
/// replaced with concrete types by [`replace_opaque_with_concrete`].
///
/// # Parameters
/// - `type_scope`: The type context containing all known types
/// - `ty`: The parser type to resolve
/// - `opaque_name`: The name of the type being defined (allowed as opaque
///   reference)
///
/// # Errors
/// Errors if the identifier is not found in the type scope (and is not the
/// opaque name) or a key is double-defined in struct/union types.
///
/// # Examples
/// ```ignore
/// // Resolving: struct { value: i32, next: *Node }
/// // where opaque_name = "Node"
/// // Result: Struct { "value": I32, "next": Ptr(Opaque("Node")) }
/// ```
fn resolve_type_with_opaque<'input>(
    type_scope: &TypeCtx<'input>,
    ty: ParserType<'input>,
    opaque_name: &'input str,
) -> Result<TastType<'input>, Diagnostic> {
    let span = ty.0.span();
    Ok(match ty.0.into_value() {
        ParserTypeKind::Identifier(x) => {
            if x == opaque_name {
                TastType::Opaque(x)
            } else if let Some(ty) = type_scope.resolve(x) {
                ty.clone()
            } else {
                return Err(DiagnosticKind::UnableToResolveType(x.to_string()).error_in(span));
            }
        }
        ParserTypeKind::Ptr(pointee_ty) => TastType::Ptr(Box::new(resolve_type_with_opaque(
            type_scope,
            *pointee_ty,
            opaque_name,
        )?)),
        ParserTypeKind::Struct(members) => TastType::Struct(resolve_key_type_mapping_with_opaque(
            type_scope,
            members,
            opaque_name,
        )?),
        ParserTypeKind::Union(members) => TastType::Union(resolve_key_type_mapping_with_opaque(
            type_scope,
            members,
            opaque_name,
        )?),
    })
}

/// Check if a type contains an opaque reference not behind a pointer.
///
/// Returns the span of the first opaque type found that's not behind a pointer.
///
/// # Errors
/// Returns an error if an opaque type is found not behind a pointer.
#[allow(clippy::wildcard_enum_match_arm)]
fn check_opaque_behind_pointer<'input>(
    ty: &TastType<'input>,
    opaque_name: &'input str,
    ty_span: Span,
) -> Result<(), Diagnostic> {
    match ty {
        TastType::Opaque(name) if *name == opaque_name => Err(
            DiagnosticKind::SelfReferentialTypeNotBehindPointer((*name).to_string())
                .error_in(ty_span),
        ),
        TastType::Ptr(_) => {
            // Anything behind a pointer is OK, even opaque types
            Ok(())
        }
        TastType::Struct(members) | TastType::Union(members) => {
            for (_, member_ty) in members {
                check_opaque_behind_pointer(member_ty, opaque_name, ty_span)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Resolve a type definition that may contain self-references (only allowed
/// behind pointers).
///
/// # Errors
/// Errors if the type contains self-references not behind pointers or other
/// type resolution errors.
pub fn resolve_type_with_self_reference<'input>(
    type_scope: &TypeCtx<'input>,
    ty: ParserType<'input>,
    self_name: &'input str,
) -> Result<TastType<'input>, Diagnostic> {
    let resolved = resolve_type_with_opaque(type_scope, ty, self_name)?;
    // Note: validation happens during resolve_key_type_mapping_with_opaque for
    // struct/union members
    Ok(replace_opaque_with_concrete(resolved, self_name))
}

/// Replace all opaque type references with the concrete type.
/// For self-referential types behind pointers, replaces with an empty struct
/// as a placeholder since pointers don't need to know the full pointee type.
#[allow(clippy::wildcard_enum_match_arm)]
fn replace_opaque_with_concrete<'input>(
    ty: TastType<'input>,
    opaque_name: &'input str,
) -> TastType<'input> {
    match ty {
        TastType::Opaque(name) if name == opaque_name => {
            // This should never happen if check_opaque_behind_pointer succeeded
            // but we handle it gracefully by replacing with empty struct (unit type)
            TastType::unit()
        }
        TastType::Ptr(pointee) => {
            // For pointers to opaque types, we can safely replace the opaque
            // with an empty struct placeholder. The pointer doesn't need to know
            // the full layout of what it points to.
            match *pointee {
                TastType::Opaque(name) if name == opaque_name => {
                    // Replace *Opaque(name) with *struct{} (pointer to empty struct)
                    TastType::Ptr(Box::new(TastType::unit()))
                }
                other => TastType::Ptr(Box::new(replace_opaque_with_concrete(other, opaque_name))),
            }
        }
        TastType::Struct(members) => TastType::Struct(
            members
                .into_iter()
                .map(|(key, val)| (key, replace_opaque_with_concrete(val, opaque_name)))
                .collect(),
        ),
        TastType::Union(members) => TastType::Union(
            members
                .into_iter()
                .map(|(key, val)| (key, replace_opaque_with_concrete(val, opaque_name)))
                .collect(),
        ),
        other => other,
    }
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

/// Resolve the types within the [`IndexMap`]s used by
/// [`ParserTypeKind::Struct`] with opaque type support. Validates that any
/// opaque types only appear behind pointers.
///
/// # Errors
/// Errors if a key is not unique, is unresolvable, or contains a
/// self-referential type not behind a pointer.
#[allow(clippy::type_complexity)]
fn resolve_key_type_mapping_with_opaque<'input>(
    type_scope: &TypeCtx<'input>,
    members: KeyTypeMapping<'input>,
    opaque_name: &'input str,
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
        let resolved_type = resolve_type_with_opaque(type_scope, ast_type, opaque_name)?;
        // Check this specific field for invalid opaque references
        check_opaque_behind_pointer(&resolved_type, opaque_name, span)?;
        map.insert(key.value(), resolved_type);
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use zrc_diagnostics::Severity;
    use zrc_utils::{span::Span, spanned_test};

    use super::*;

    #[test]
    fn pointers_and_identifiers_resolve_as_expected() {
        assert_eq!(
            resolve_type(
                &TypeCtx::from([("i32", TastType::I32)]),
                ParserType::build_ptr(
                    Span::from_positions_and_file(0, 4, "<test>"),
                    ParserType::build_ident(spanned_test!(1, "i32", 4)),
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
                ParserType::build_ident(spanned_test!(0, "x", 1))
            ),
            Err(Diagnostic(
                Severity::Error,
                spanned_test!(0, DiagnosticKind::UnableToResolveType("x".to_string()), 1)
            ))
        );
    }

    #[test]
    fn structs_resolve_as_expected() {
        // struct { x: i32, y: i32 }
        assert_eq!(
            resolve_type(
                &TypeCtx::default(),
                ParserType(spanned_test!(
                    0,
                    ParserTypeKind::Struct(KeyTypeMapping(spanned_test!(
                        7,
                        vec![
                            spanned_test!(
                                9,
                                (
                                    spanned_test!(9, "x", 10),
                                    ParserType(spanned_test!(
                                        12,
                                        ParserTypeKind::Identifier("i32"),
                                        15
                                    ))
                                ),
                                15
                            ),
                            spanned_test!(
                                17,
                                (
                                    spanned_test!(17, "y", 18),
                                    ParserType(spanned_test!(
                                        20,
                                        ParserTypeKind::Identifier("i32"),
                                        23
                                    ))
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
    fn enums_resolve_as_expected() {
        // enum { Eight: i8, Sixteen: i16 }
        assert_eq!(
            resolve_type(
                &TypeCtx::default(),
                ParserType(spanned!(
                    0,
                    ParserTypeKind::Enum(KeyTypeMapping(spanned!(
                        1,
                        vec![
                            spanned!(
                                2,
                                (
                                    spanned!(3, "Eight", 4),
                                    ParserType(spanned!(5, ParserTypeKind::Identifier("i8"), 6))
                                ),
                                7
                            ),
                            spanned!(
                                8,
                                (
                                    spanned!(9, "Sixteen", 10),
                                    ParserType(spanned!(11, ParserTypeKind::Identifier("i16"), 12))
                                ),
                                13
                            )
                        ],
                        14
                    ))),
                    15
                ))
            ),
            Ok(TastType::Struct(IndexMap::from([
                ("__discriminant__", TastType::Usize),
                (
                    "__value__",
                    TastType::Union(IndexMap::from([
                        ("Eight", TastType::I8),
                        ("Sixteen", TastType::I16)
                    ]))
                )
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
                    Span::from_positions_and_file(0, 25, "<test>"),
                    KeyTypeMapping(spanned_test!(
                        7,
                        vec![
                            spanned_test!(
                                9,
                                (
                                    spanned_test!(9, "x", 10),
                                    ParserType(spanned_test!(
                                        12,
                                        ParserTypeKind::Identifier("i32"),
                                        15
                                    ))
                                ),
                                15
                            ),
                            spanned_test!(
                                17,
                                (
                                    spanned_test!(17, "x", 18),
                                    ParserType(spanned_test!(
                                        20,
                                        ParserTypeKind::Identifier("i32"),
                                        23
                                    ))
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
                spanned_test!(
                    17,
                    DiagnosticKind::DuplicateStructMember("x".to_string()),
                    23
                )
            ))
        );
    }

    #[test]
    fn self_referential_type_behind_pointer_resolves_correctly() {
        // struct { value: i32, next: *Node }
        // where Node is the name being defined
        let result = resolve_type_with_self_reference(
            &TypeCtx::default(),
            ParserType(spanned_test!(
                0,
                ParserTypeKind::Struct(KeyTypeMapping(spanned_test!(
                    7,
                    vec![
                        spanned_test!(
                            9,
                            (
                                spanned_test!(9, "value", 14),
                                ParserType(spanned_test!(
                                    16,
                                    ParserTypeKind::Identifier("i32"),
                                    19
                                ))
                            ),
                            19
                        ),
                        spanned_test!(
                            21,
                            (
                                spanned_test!(21, "next", 25),
                                ParserType(spanned_test!(
                                    27,
                                    ParserTypeKind::Ptr(Box::new(ParserType(spanned_test!(
                                        28,
                                        ParserTypeKind::Identifier("Node"),
                                        32
                                    )))),
                                    32
                                ))
                            ),
                            32
                        )
                    ],
                    34
                ))),
                34
            )),
            "Node",
        );

        assert!(result.is_ok());
        let resolved_ty = result.expect("should resolve successfully");
        if let TastType::Struct(fields) = resolved_ty {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields.get("value"), Some(&TastType::I32));
            // The pointer to self should be replaced with pointer to empty struct
            assert_eq!(
                fields.get("next"),
                Some(&TastType::Ptr(Box::new(TastType::unit())))
            );
        } else {
            panic!("Expected struct type");
        }
    }

    #[test]
    fn self_referential_type_not_behind_pointer_produces_error() {
        // struct { value: i32, next: Node }
        // where Node is the name being defined (ERROR: not behind pointer)
        let result = resolve_type_with_self_reference(
            &TypeCtx::default(),
            ParserType(spanned_test!(
                0,
                ParserTypeKind::Struct(KeyTypeMapping(spanned_test!(
                    7,
                    vec![
                        spanned_test!(
                            9,
                            (
                                spanned_test!(9, "value", 14),
                                ParserType(spanned_test!(
                                    16,
                                    ParserTypeKind::Identifier("i32"),
                                    19
                                ))
                            ),
                            19
                        ),
                        spanned_test!(
                            21,
                            (
                                spanned_test!(21, "next", 25),
                                ParserType(spanned_test!(
                                    27,
                                    ParserTypeKind::Identifier("Node"),
                                    31
                                ))
                            ),
                            31
                        )
                    ],
                    33
                ))),
                33
            )),
            "Node",
        );

        assert!(result.is_err());
        let err = result.expect_err("should produce error");
        assert_eq!(
            err,
            Diagnostic(
                Severity::Error,
                spanned_test!(
                    21,
                    DiagnosticKind::SelfReferentialTypeNotBehindPointer("Node".to_string()),
                    31
                )
            )
        );
    }

    #[test]
    fn nested_self_referential_type_works() {
        // struct { data: i32, children: *struct { item: *Node, next: *Node } }
        // where Node is the name being defined
        let result = resolve_type_with_self_reference(
            &TypeCtx::default(),
            ParserType(spanned_test!(
                0,
                ParserTypeKind::Struct(KeyTypeMapping(spanned_test!(
                    7,
                    vec![
                        spanned_test!(
                            9,
                            (
                                spanned_test!(9, "data", 13),
                                ParserType(spanned_test!(
                                    15,
                                    ParserTypeKind::Identifier("i32"),
                                    18
                                ))
                            ),
                            18
                        ),
                        spanned_test!(
                            20,
                            (
                                spanned_test!(20, "children", 28),
                                ParserType(spanned_test!(
                                    30,
                                    ParserTypeKind::Ptr(Box::new(ParserType(spanned_test!(
                                        31,
                                        ParserTypeKind::Struct(KeyTypeMapping(spanned_test!(
                                            38,
                                            vec![
                                                spanned_test!(
                                                    40,
                                                    (
                                                        spanned_test!(40, "item", 44),
                                                        ParserType(spanned_test!(
                                                            46,
                                                            ParserTypeKind::Ptr(Box::new(
                                                                ParserType(spanned_test!(
                                                                    47,
                                                                    ParserTypeKind::Identifier(
                                                                        "Node"
                                                                    ),
                                                                    51
                                                                ))
                                                            )),
                                                            51
                                                        ))
                                                    ),
                                                    51
                                                ),
                                                spanned_test!(
                                                    53,
                                                    (
                                                        spanned_test!(53, "next", 57),
                                                        ParserType(spanned_test!(
                                                            59,
                                                            ParserTypeKind::Ptr(Box::new(
                                                                ParserType(spanned_test!(
                                                                    60,
                                                                    ParserTypeKind::Identifier(
                                                                        "Node"
                                                                    ),
                                                                    64
                                                                ))
                                                            )),
                                                            64
                                                        ))
                                                    ),
                                                    64
                                                )
                                            ],
                                            66
                                        ))),
                                        66
                                    )))),
                                    66
                                ))
                            ),
                            66
                        )
                    ],
                    68
                ))),
                68
            )),
            "Node",
        );

        assert!(result.is_ok());
    }
}
