//! for expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpanExt};
use zrc_parser::ast::expr::{Arithmetic, Assignment, BinaryBitwise, Expr, ExprKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::scope::Scope;
use crate::{
    tast::{
        expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
        stmt::ArgumentDeclarationList,
        ty::{Fn, Type as TastType},
    },
    typeck::resolve_type,
};

/// Desugar an assignment like `x += y` to `x = x + y`.
fn desugar_assignment<'input>(
    mode: Assignment,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> (Expr<'input>, Expr<'input>) {
    match mode {
        Assignment::Standard => (lhs, rhs),
        // This makes the span of the generated 'a + b' in a += b the same span of 'b'. Do we want
        // this?
        Assignment::Arithmetic(op) => (
            lhs.clone(),
            Expr(
                rhs.0
                    .span()
                    .containing(ExprKind::Arithmetic(op, Box::new(lhs), Box::new(rhs))),
            ),
        ),
        Assignment::BinaryBitwise(op) => (
            lhs.clone(),
            Expr(rhs.0.span().containing(ExprKind::BinaryBitwise(
                op,
                Box::new(lhs),
                Box::new(rhs),
            ))),
        ),
    }
}

/// Validate an expr into a place
fn expr_to_place(span: Span, expr: TypedExpr) -> Result<Place, Diagnostic> {
    let kind_span = expr.kind.span();
    let stringified = expr.inferred_type.to_string();

    #[allow(clippy::wildcard_enum_match_arm)]
    Ok(match expr.kind.into_value() {
        TypedExprKind::UnaryDereference(x) => Place {
            inferred_type: expr.inferred_type,
            kind: PlaceKind::Deref(x).in_span(kind_span),
        },
        TypedExprKind::Identifier(x) => Place {
            inferred_type: expr.inferred_type,
            kind: PlaceKind::Variable(x).in_span(kind_span),
        },
        TypedExprKind::Index(x, y) => Place {
            inferred_type: expr.inferred_type,
            kind: PlaceKind::Index(x, y).in_span(kind_span),
        },
        TypedExprKind::Dot(x, y) => Place {
            inferred_type: expr.inferred_type,
            kind: PlaceKind::Dot(x, y).in_span(kind_span),
        },
        _ => return Err(span.error(DiagnosticKind::NotAnLvalue(stringified))),
    })
}

/// Assert two types are the same and produce a validation error otherwise
fn expect_identical_types<'a, 'input>(
    lhs: &'a TastType<'input>,
    rhs: &'a TastType<'input>,
    span: Span,
) -> Result<(), Diagnostic> {
    if lhs == rhs {
        Ok(())
    } else {
        Err(DiagnosticKind::ExpectedSameType(lhs.to_string(), rhs.to_string()).error_in(span))
    }
}

/// Assert a condition and produce a [`DiagnosticKind::ExpectedGot`] diagnostic
/// otherwise.
fn expect(
    condition: bool,
    expected_str: String,
    got_str: String,
    span: Span,
) -> Result<(), Diagnostic> {
    if condition {
        Ok(())
    } else {
        Err(DiagnosticKind::ExpectedGot {
            expected: expected_str,
            got: got_str,
        }
        .error_in(span))
    }
}

/// Assert that a type is an integer type
fn expect_is_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(ty.is_integer(), "integer".to_string(), ty.to_string(), span)
}

/// Assert that a type is a signed integer type
fn expect_is_signed_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(
        ty.is_signed_integer(),
        "signed integer".to_string(),
        ty.to_string(),
        span,
    )
}

/// Assert that a type is an unsigned integer type
fn expect_is_unsigned_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(
        ty.is_unsigned_integer(),
        "unsigned integer".to_string(),
        ty.to_string(),
        span,
    )
}

// FIXME: this NEEDS to be rewritten to use references almost everywhere and be
// no-clone. We stack overflow for deep expressions which is VERY VERY BAD.
/// Type check and infer an [AST expression](Expr) to a [TAST
/// expression](TypedExpr).
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(
    clippy::too_many_lines, // FIXME: make this fn shorter
    clippy::missing_panics_doc
)]
pub fn type_expr<'input>(
    scope: &Scope<'input, '_>,
    expr: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let expr_span = expr.0.span();
    Ok(match expr.0.into_value() {
        ExprKind::Comma(lhs, rhs) => {
            let at = type_expr(scope, *lhs)?;
            let bt = type_expr(scope, *rhs)?;
            TypedExpr {
                inferred_type: bt.inferred_type.clone(),
                kind: TypedExprKind::Comma(Box::new(at), Box::new(bt)).in_span(expr_span),
            }
        }
        ExprKind::Assignment(mode, place, value) => {
            // Desugar `x += y` to `x = x + y`.
            let (place, value) = desugar_assignment(mode, *place, *value);

            let place_t = expr_to_place(expr_span, type_expr(scope, place)?)?;
            let value_t = type_expr(scope, value)?;

            if place_t.inferred_type != value_t.inferred_type {
                return Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                    expected: place_t.inferred_type.to_string(),
                    got: value_t.inferred_type.to_string(),
                }
                .error_in(expr_span));
            }

            TypedExpr {
                inferred_type: place_t.inferred_type.clone(),
                kind: TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t))
                    .in_span(expr_span),
            }
        }

        ExprKind::UnaryNot(x) => {
            let x_ty = type_expr(scope, *x)?;

            expect(
                x_ty.inferred_type == TastType::Bool,
                "boolean".to_string(),
                x_ty.inferred_type.to_string(),
                expr_span,
            )?;

            TypedExpr {
                inferred_type: x_ty.inferred_type.clone(),
                kind: TypedExprKind::UnaryNot(Box::new(x_ty)).in_span(expr_span),
            }
        }
        ExprKind::UnaryBitwiseNot(x) => {
            let x_span = x.0.span();
            let x_ty = type_expr(scope, *x)?;

            expect_is_integer(&x_ty.inferred_type, x_span)?;

            TypedExpr {
                inferred_type: x_ty.inferred_type.clone(),
                kind: TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)).in_span(expr_span),
            }
        }
        ExprKind::UnaryMinus(x) => {
            let x_span = x.0.span();
            let x_ty = type_expr(scope, *x)?;

            expect_is_signed_integer(&x_ty.inferred_type, x_span)?;

            TypedExpr {
                inferred_type: x_ty.inferred_type.clone(),
                kind: TypedExprKind::UnaryMinus(Box::new(x_ty)).in_span(expr_span),
            }
        }
        ExprKind::UnaryAddressOf(x) => {
            let x_ty = type_expr(scope, *x)?;

            TypedExpr {
                inferred_type: TastType::Ptr(Box::new(x_ty.inferred_type.clone())),
                kind: TypedExprKind::UnaryAddressOf(Box::new(expr_to_place(expr_span, x_ty)?))
                    .in_span(expr_span),
            }
        }
        ExprKind::UnaryDereference(x) => {
            let x_ty = type_expr(scope, *x)?;
            if let TastType::Ptr(tt) = x_ty.inferred_type.clone() {
                TypedExpr {
                    inferred_type: *tt,
                    kind: TypedExprKind::UnaryDereference(Box::new(x_ty)).in_span(expr_span),
                }
            } else {
                return Err(DiagnosticKind::CannotDereferenceNonPointer(
                    x_ty.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }
        }

        ExprKind::Index(ptr, offset) => {
            let ptr_t = type_expr(scope, *ptr)?;
            let offset_t = type_expr(scope, *offset)?;

            expect(
                offset_t.inferred_type == TastType::Usize,
                "usize".to_string(),
                offset_t.inferred_type.to_string(),
                offset_t.kind.span(),
            )?;

            if let TastType::Ptr(points_to_ty) = ptr_t.inferred_type.clone() {
                TypedExpr {
                    inferred_type: *points_to_ty,
                    kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t))
                        .in_span(expr_span),
                }
            } else {
                return Err(DiagnosticKind::CannotIndexIntoNonPointer(
                    ptr_t.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }
        }

        ExprKind::Dot(obj, key) => {
            let obj_span = obj.0.span();
            let obj_t = type_expr(scope, *obj)?;

            if let TastType::Struct(fields) | TastType::Union(fields) = obj_t.inferred_type.clone()
            {
                if let Some(ty) = fields.get(key.value()) {
                    TypedExpr {
                        inferred_type: ty.clone(),
                        kind: TypedExprKind::Dot(Box::new(expr_to_place(obj_span, obj_t)?), key)
                            .in_span(expr_span),
                    }
                } else {
                    return Err(DiagnosticKind::StructOrUnionDoesNotHaveMember(
                        obj_t.inferred_type.to_string(),
                        key.into_value().to_string(),
                    )
                    .error_in(expr_span));
                }
            } else {
                return Err(DiagnosticKind::StructMemberAccessOnNonStruct(
                    obj_t.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }
        }
        ExprKind::Arrow(obj, key) => {
            let obj_t = type_expr(scope, *obj.clone())?;

            if let TastType::Ptr(_) = obj_t.inferred_type {
                type_expr(
                    scope,
                    Expr(Spanned::from_span_and_value(
                        Span::from_positions(obj.0.start(), key.end()),
                        ExprKind::Dot(
                            Box::new(Expr(
                                obj.0.span().containing(ExprKind::UnaryDereference(obj)),
                            )),
                            key,
                        ),
                    )),
                )?
            } else {
                return Err(DiagnosticKind::CannotDereferenceNonPointer(
                    obj_t.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }
        }
        ExprKind::Call(f, args) => {
            let f_span = (*f).0.span();
            let ft = type_expr(scope, *f)?;
            let args_t = args
                .value()
                .iter()
                .map(|x| type_expr(scope, x.clone()))
                .collect::<Result<Vec<TypedExpr>, Diagnostic>>()?;

            #[allow(clippy::wildcard_enum_match_arm)]
            match ft.inferred_type.clone() {
                TastType::Fn(Fn {
                    arguments: ArgumentDeclarationList::NonVariadic(arg_types),
                    returns: ret_type,
                }) => {
                    if arg_types.len() != args_t.len() {
                        return Err(DiagnosticKind::FunctionArgumentCountMismatch {
                            expected: arg_types.len().to_string(),
                            got: args_t.len().to_string(),
                        }
                        .error_in(expr_span));
                    }

                    for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                        if *arg_type.ty.value() != arg_t.inferred_type {
                            return Err(args.value()[i].0.span().error(
                                DiagnosticKind::FunctionArgumentTypeMismatch {
                                    n: i,
                                    expected: arg_type.ty.to_string(),
                                    got: arg_t.inferred_type.to_string(),
                                },
                            ));
                        }
                    }

                    TypedExpr {
                        inferred_type: *ret_type,
                        kind: TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_t)
                            .in_span(expr_span),
                    }
                }
                TastType::Fn(Fn {
                    arguments: ArgumentDeclarationList::Variadic(beginning_arg_types),
                    returns: ret_type,
                }) => {
                    if beginning_arg_types.len() > args_t.len() {
                        return Err(DiagnosticKind::FunctionArgumentCountMismatch {
                            expected: format!("at least {}", beginning_arg_types.len()),
                            got: args_t.len().to_string(),
                        }
                        .error_in(expr_span));
                    }

                    for (i, (arg_type, arg_t)) in
                        beginning_arg_types.iter().zip(args_t.iter()).enumerate()
                    {
                        if *arg_type.ty.value() != arg_t.inferred_type {
                            return Err(args.value()[i].0.span().error(
                                DiagnosticKind::FunctionArgumentTypeMismatch {
                                    n: i,
                                    expected: arg_type.ty.to_string(),
                                    got: arg_t.inferred_type.to_string(),
                                },
                            ));
                        }
                    }

                    // the rest may be any, so we don't need to check them
                    TypedExpr {
                        inferred_type: *ret_type,
                        kind: TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_t)
                            .in_span(expr_span),
                    }
                }
                _ => {
                    return Err(DiagnosticKind::CannotCallNonFunction(
                        ft.inferred_type.to_string(),
                    )
                    .error_in(expr_span));
                }
            }
        }

        ExprKind::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(scope, *cond.clone())?;
            let if_true_t = type_expr(scope, *if_true)?;
            let if_false_t = type_expr(scope, *if_false)?;

            expect(
                cond_t.inferred_type == TastType::Bool,
                "boolean".to_string(),
                cond_t.inferred_type.to_string(),
                cond.0.span(),
            )?;

            expect_identical_types(
                &if_true_t.inferred_type,
                &if_false_t.inferred_type,
                expr_span,
            )?;

            TypedExpr {
                inferred_type: if_true_t.inferred_type.clone(),
                kind: TypedExprKind::Ternary(
                    Box::new(cond_t),
                    Box::new(if_true_t),
                    Box::new(if_false_t),
                )
                .in_span(expr_span),
            }
        }

        ExprKind::Logical(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect(
                lhs_t.inferred_type == TastType::Bool,
                "bool".to_string(),
                lhs_t.inferred_type.to_string(),
                lhs_span,
            )?;
            expect(
                rhs_t.inferred_type == TastType::Bool,
                "bool".to_string(),
                rhs_t.inferred_type.to_string(),
                rhs_span,
            )?;

            TypedExpr {
                inferred_type: TastType::Bool,
                kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t))
                    .in_span(expr_span),
            }
        }
        ExprKind::Equality(op, lhs, rhs) => {
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_t = type_expr(scope, *rhs)?;

            if lhs_t.inferred_type.is_integer()
                && rhs_t.inferred_type.is_integer()
                && lhs_t.inferred_type == rhs_t.inferred_type
            {
                // int == int is valid
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) =
                (&lhs_t.inferred_type, &rhs_t.inferred_type)
            {
                // *T == *U is valid
            } else if lhs_t.inferred_type == TastType::Bool && rhs_t.inferred_type == TastType::Bool
            {
                // bool == bool is valid
            } else {
                return Err(DiagnosticKind::EqualityOperators(
                    lhs_t.inferred_type.to_string(),
                    rhs_t.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }

            TypedExpr {
                inferred_type: TastType::Bool,
                kind: TypedExprKind::Equality(op, Box::new(lhs_t), Box::new(rhs_t))
                    .in_span(expr_span),
            }
        }
        ExprKind::BinaryBitwise(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
            expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

            if matches!(op, BinaryBitwise::Shl | BinaryBitwise::Shr) {
                // we can only shift by an unsigned integer
                expect_is_unsigned_integer(&rhs_t.inferred_type, rhs_span)?;
            } else {
                // otherwise these must be the same type
                expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;
            }

            TypedExpr {
                inferred_type: lhs_t.inferred_type.clone(),
                kind: TypedExprKind::BinaryBitwise(op, Box::new(lhs_t), Box::new(rhs_t))
                    .in_span(expr_span),
            }
        }
        ExprKind::Comparison(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
            expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

            expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;

            TypedExpr {
                inferred_type: TastType::Bool,
                kind: TypedExprKind::Comparison(op, Box::new(lhs_t), Box::new(rhs_t))
                    .in_span(expr_span),
            }
        }
        ExprKind::Arithmetic(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            if let TastType::Ptr(_) = lhs_t.inferred_type {
                if matches!(
                    op,
                    Arithmetic::Division | Arithmetic::Multiplication | Arithmetic::Modulo
                ) {
                    return Err(
                        DiagnosticKind::InvalidPointerArithmeticOperation(op.to_string())
                            .error_in(lhs_span),
                    );
                }

                expect(
                    rhs_t.inferred_type == TastType::Usize,
                    "usize".to_string(),
                    rhs_t.inferred_type.to_string(),
                    rhs_t.kind.span(),
                )?;
            } else {
                expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
                expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

                expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;
            }

            TypedExpr {
                inferred_type: lhs_t.inferred_type.clone(),
                kind: TypedExprKind::Arithmetic(op, Box::new(lhs_t), Box::new(rhs_t))
                    .in_span(expr_span),
            }
        }

        ExprKind::Cast(x, ty) => {
            let x_t = type_expr(scope, *x)?;
            let ty_span = ty.0.span();
            let resolved_ty = resolve_type(scope.types, ty)?;

            if x_t.inferred_type.is_integer() && resolved_ty.is_integer() {
                // int -> int cast is valid
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (&x_t.inferred_type, &resolved_ty)
            {
                // *T -> *U cast is valid
            } else if let (TastType::Ptr(_), _) | (_, TastType::Ptr(_)) =
                (&x_t.inferred_type, &resolved_ty)
            {
                // ensure one is an int
                if x_t.inferred_type.is_integer() || resolved_ty.is_integer() {
                    // *T -> int or int -> *T cast is valid
                } else {
                    return Err(DiagnosticKind::InvalidCast(
                        x_t.inferred_type.to_string(),
                        resolved_ty.to_string(),
                    )
                    .error_in(expr_span));
                }
            } else if x_t.inferred_type == TastType::Bool && resolved_ty.is_integer() {
                // bool -> int cast is valid
            } else {
                return Err(DiagnosticKind::InvalidCast(
                    x_t.inferred_type.to_string(),
                    resolved_ty.to_string(),
                )
                .error_in(expr_span));
            }

            TypedExpr {
                inferred_type: resolved_ty.clone(),
                kind: TypedExprKind::Cast(Box::new(x_t), resolved_ty.in_span(ty_span))
                    .in_span(expr_span),
            }
        }

        ExprKind::SizeOfType(ty) => {
            let resolved_ty = resolve_type(scope.types, ty)?;
            TypedExpr {
                inferred_type: TastType::Usize,
                kind: TypedExprKind::SizeOf(resolved_ty).in_span(expr_span),
            }
        }
        // resolve `sizeof(expr)` by finding `typeof expr` and then basically becoming "sizeof
        // typeof expr"
        ExprKind::SizeOfExpr(x) => {
            let x_ty = type_expr(scope, *x)?;
            TypedExpr {
                inferred_type: TastType::Usize,
                kind: TypedExprKind::SizeOf(x_ty.inferred_type).in_span(expr_span),
            }
        }

        ExprKind::NumberLiteral(n, ty) => {
            let ty_resolved = ty
                .map(|ty| resolve_type(scope.types, ty))
                .transpose()?
                .unwrap_or(TastType::I32);

            if !ty_resolved.is_integer() {
                return Err(
                    DiagnosticKind::InvalidNumberLiteralType(ty_resolved.to_string())
                        .error_in(expr_span),
                );
            }

            // REVIEW: How can we make this work with integers like i128?
            // -4u8 parses as -(4u8) so we don't need to handle negative integers here
            // let parsed_integer = u64::from_str_radix(&n.text_content().replace('_', ""),
            // n.radix())     .expect("Number literal should have been valid");

            // TODO: Check the bounds of each number literal, instead
            // of just panicking or overflowing at runtime (what really happens?)
            //
            // This is difficult because we don't know the size of `usize`/`isize` here.

            TypedExpr {
                inferred_type: ty_resolved.clone(),
                kind: TypedExprKind::NumberLiteral(n, ty_resolved).in_span(expr_span),
            }
        }
        ExprKind::StringLiteral(str) => TypedExpr {
            inferred_type: TastType::Ptr(Box::new(TastType::U8)),
            kind: TypedExprKind::StringLiteral(str).in_span(expr_span),
        },
        ExprKind::CharLiteral(ch) => TypedExpr {
            inferred_type: TastType::U8,
            kind: TypedExprKind::CharLiteral(ch).in_span(expr_span),
        },
        ExprKind::Identifier(i) => {
            let ty = scope.values.resolve(i).ok_or_else(|| {
                DiagnosticKind::UnableToResolveIdentifier(i.to_string()).error_in(expr_span)
            })?;
            TypedExpr {
                inferred_type: ty.clone(),
                kind: TypedExprKind::Identifier(i).in_span(expr_span),
            }
        }
        ExprKind::BooleanLiteral(value) => TypedExpr {
            inferred_type: TastType::Bool,
            kind: TypedExprKind::BooleanLiteral(value).in_span(expr_span),
        },
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use indexmap::IndexMap;
    use zrc_diagnostics::Severity;
    use zrc_parser::lexer::NumberLiteral;
    use zrc_utils::spanned;

    use super::*;
    use crate::{
        tast::stmt::ArgumentDeclaration,
        typeck::scope::{GlobalScope, TypeCtx, ValueCtx},
    };

    #[test]
    fn expect_identical_types_produces_proper_diagnostic() {
        let sample_span = Span::from_positions(0, 5);
        assert_eq!(
            expect_identical_types(&TastType::I32, &TastType::I8, sample_span),
            Err(Diagnostic(
                Severity::Error,
                DiagnosticKind::ExpectedSameType("i32".to_string(), "i8".to_string())
                    .in_span(sample_span)
            ))
        );
    }

    #[test]
    fn expect_produces_proper_diagnostic() {
        let sample_span = Span::from_positions(0, 5);
        assert_eq!(
            expect(
                false,
                "expected".to_string(),
                "got".to_string(),
                sample_span
            ),
            Err(Diagnostic(
                Severity::Error,
                DiagnosticKind::ExpectedGot {
                    expected: "expected".to_string(),
                    got: "got".to_string()
                }
                .in_span(sample_span)
            ))
        );
    }

    mod desugar_assignment {
        use super::*;

        #[test]
        fn standard() {
            assert_eq!(
                // a = b
                desugar_assignment(
                    Assignment::Standard,
                    Expr::build_ident(spanned!(0, "a", 1)),
                    Expr::build_ident(spanned!(4, "b", 5)),
                ),
                (
                    Expr::build_ident(spanned!(0, "a", 1)),
                    Expr::build_ident(spanned!(4, "b", 5)),
                )
            );
        }

        #[test]
        fn arithmetic() {
            assert_eq!(
                // a += b
                desugar_assignment(
                    Assignment::Arithmetic(Arithmetic::Addition),
                    Expr::build_ident(spanned!(0, "a", 1)),
                    Expr::build_ident(spanned!(5, "b", 6)),
                ),
                (
                    Expr::build_ident(spanned!(0, "a", 1)),
                    // An exception to the normal spanning rules applies here
                    Expr(spanned!(
                        5,
                        ExprKind::Arithmetic(
                            Arithmetic::Addition,
                            Box::new(Expr::build_ident(spanned!(0, "a", 1))),
                            Box::new(Expr::build_ident(spanned!(5, "b", 6)))
                        ),
                        6
                    ))
                )
            );
        }

        #[test]
        fn bitwise() {
            assert_eq!(
                // a >>= b
                desugar_assignment(
                    Assignment::BinaryBitwise(BinaryBitwise::Shr),
                    Expr::build_ident(spanned!(0, "a", 1)),
                    Expr::build_ident(spanned!(6, "b", 7)),
                ),
                (
                    Expr::build_ident(spanned!(0, "a", 1)),
                    // An exception to the normal spanning rules applies here
                    Expr(spanned!(
                        6,
                        ExprKind::BinaryBitwise(
                            BinaryBitwise::Shr,
                            Box::new(Expr::build_ident(spanned!(0, "a", 1))),
                            Box::new(Expr::build_ident(spanned!(6, "b", 7)))
                        ),
                        7
                    ))
                )
            );
        }
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn various_expressions_infer_correctly() {
        let scope = GlobalScope {
            global_values: ValueCtx::from_mappings(HashMap::from([
                ("i8", TastType::I8),
                ("u8", TastType::U8),
                ("i32", TastType::I32),
                ("bool", TastType::Bool),
                (
                    "s",
                    TastType::Struct(IndexMap::from([("i8", TastType::I8)])),
                ),
                (
                    "get_bool",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::NonVariadic(vec![]),
                        returns: Box::new(TastType::Bool),
                    }),
                ),
                (
                    "id",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::NonVariadic(vec![
                            ArgumentDeclaration {
                                name: spanned!(0, "x", 1),
                                ty: spanned!(0, TastType::I32, 3),
                            },
                        ]),
                        returns: Box::new(TastType::I32),
                    }),
                ),
                (
                    "sink",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::Variadic(vec![ArgumentDeclaration {
                            name: spanned!(0, "i8", 3),
                            ty: spanned!(0, TastType::I8, 3),
                        }]),
                        returns: Box::new(TastType::unit()),
                    }),
                ),
            ])),
            types: TypeCtx::from_defaults_and_mappings(HashMap::from([(
                "NonIntegerType",
                TastType::Struct(IndexMap::from([])),
            )])),
            ..Default::default()
        };

        let tests = [
            ("i8, i32", Ok(TastType::I32)),
            ("i8 = i8", Ok(TastType::I8)),
            (
                "i8 = i32",
                Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                    expected: "i8".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            (
                "4 = i8",
                Err(DiagnosticKind::NotAnLvalue("i32".to_string())),
            ),
            ("!bool", Ok(TastType::Bool)),
            (
                "!i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "boolean".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            ("~i8", Ok(TastType::I8)),
            (
                "~bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            ("-i8", Ok(TastType::I8)),
            (
                "-bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "signed integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            ("&i8", Ok(TastType::Ptr(Box::new(TastType::I8)))),
            ("*&i8", Ok(TastType::I8)),
            (
                "*i8",
                Err(DiagnosticKind::CannotDereferenceNonPointer(
                    "i8".to_string(),
                )),
            ),
            ("(&i8)[4 as usize]", Ok(TastType::I8)),
            (
                "(&i8)['a']",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "usize".to_string(),
                    got: "u8".to_string(),
                }),
            ),
            (
                "i8[4 as usize]",
                Err(DiagnosticKind::CannotIndexIntoNonPointer("i8".to_string())),
            ),
            ("s.i8", Ok(TastType::I8)),
            (
                "s.fake",
                Err(DiagnosticKind::StructOrUnionDoesNotHaveMember(
                    "(struct { i8: i8 })".to_string(),
                    "fake".to_string(),
                )),
            ),
            (
                "i32.fake",
                Err(DiagnosticKind::StructMemberAccessOnNonStruct(
                    "i32".to_string(),
                )),
            ),
            ("(&s)->i8", Ok(TastType::I8)),
            ("get_bool()", Ok(TastType::Bool)),
            (
                "get_bool(i32)",
                Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: "0".to_string(),
                    got: "1".to_string(),
                }),
            ),
            (
                "id(i8)",
                Err(DiagnosticKind::FunctionArgumentTypeMismatch {
                    n: 0,
                    expected: "i32".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            ("sink(i8, i32, bool)", Ok(TastType::unit())),
            (
                "sink()",
                Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: "at least 1".to_string(),
                    got: "0".to_string(),
                }),
            ),
            (
                "sink(i32)",
                Err(DiagnosticKind::FunctionArgumentTypeMismatch {
                    n: 0,
                    expected: "i8".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            (
                "bool()",
                Err(DiagnosticKind::CannotCallNonFunction("bool".to_string())),
            ),
            ("bool ? i8 : i8", Ok(TastType::I8)),
            (
                "i8 ? i8 : i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "boolean".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            (
                "bool ? i8 : i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("bool && bool", Ok(TastType::Bool)),
            (
                "i32 && bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "bool".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            ("i8 == i8", Ok(TastType::Bool)),
            ("(&i8) == (&i8)", Ok(TastType::Bool)),
            (
                "(&i8) == i8",
                Err(DiagnosticKind::EqualityOperators(
                    "*(i8)".to_string(),
                    "i8".to_string(),
                )),
            ),
            ("bool == bool", Ok(TastType::Bool)),
            ("i8 >> u8", Ok(TastType::I8)),
            (
                "i8 >> i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "unsigned integer".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            (
                "i8 & i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("i8 & i8", Ok(TastType::I8)),
            ("i8 > i8", Ok(TastType::Bool)),
            (
                "bool > i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            (
                "i8 > i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("i8 + i8", Ok(TastType::I8)),
            (
                "i32 + i8",
                Err(DiagnosticKind::ExpectedSameType(
                    "i32".to_string(),
                    "i8".to_string(),
                )),
            ),
            (
                "(&i8) / 2",
                Err(DiagnosticKind::InvalidPointerArithmeticOperation(
                    "/".to_string(),
                )),
            ),
            (
                "(&i8) + 2",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "usize".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            (
                "(&i8) + (2 as usize)",
                Ok(TastType::Ptr(Box::new(TastType::I8))),
            ),
            ("i8 as i32", Ok(TastType::I32)),
            ("(&i8) as *i32", Ok(TastType::Ptr(Box::new(TastType::I32)))),
            ("(&i8) as usize", Ok(TastType::Usize)),
            ("0 as *i8", Ok(TastType::Ptr(Box::new(TastType::I8)))),
            ("true as i32", Ok(TastType::I32)),
            (
                "s as i8",
                Err(DiagnosticKind::InvalidCast(
                    "(struct { i8: i8 })".to_string(),
                    "i8".to_string(),
                )),
            ),
            ("sizeof(7)", Ok(TastType::Usize)),
            ("sizeof struct {}", Ok(TastType::Usize)),
            ("\"hello\"", Ok(TastType::Ptr(Box::new(TastType::U8)))),
            ("'a'", Ok(TastType::U8)),
            ("true", Ok(TastType::Bool)),
            ("4", Ok(TastType::I32)),
            ("4i8", Ok(TastType::I8)),
            ("-4i8", Ok(TastType::I8)),
            (
                "4 NonIntegerType",
                Err(DiagnosticKind::InvalidNumberLiteralType(
                    "(struct {  })".to_string(),
                )),
            ),
            (
                "bogus",
                Err(DiagnosticKind::UnableToResolveIdentifier(
                    "bogus".to_string(),
                )),
            ),
        ];

        for (input, expected_result) in tests {
            assert_eq!(
                type_expr(
                    &scope.create_subscope(),
                    zrc_parser::parser::parse_expr(input).expect("parsing should succeed")
                )
                .map(|result| result.inferred_type)
                .map_err(|diagnostic| diagnostic.1.into_value()),
                expected_result
            );
        }
    }

    mod expr {
        use super::*;

        #[test]
        fn sizeof_expr_works_as_expected() {
            assert_eq!(
                type_expr(
                    &GlobalScope::new().create_subscope(),
                    Expr::build_sizeof_expr(
                        Span::from_positions(0, 9),
                        Expr::build_number(spanned!(8, NumberLiteral::Decimal("1"), 9), None)
                    ),
                ),
                Ok(TypedExpr {
                    inferred_type: TastType::Usize,
                    kind: TypedExprKind::SizeOf(TastType::I32).in_span(Span::from_positions(0, 9)),
                })
            );
        }
    }
}
