//! for expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::expr::{Assignment, BinaryBitwise, Expr, ExprKind};
use zrc_utils::span::{Span, Spanned};

use super::Scope;
use crate::{
    tast::{
        expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
        stmt::ArgumentDeclarationList,
        ty::Type as TastType,
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
    #[allow(clippy::wildcard_enum_match_arm)]
    Ok(match expr.1 {
        TypedExprKind::UnaryDereference(x) => Place(expr.0, PlaceKind::Deref(x)),
        TypedExprKind::Identifier(x) => Place(expr.0, PlaceKind::Variable(x)),
        TypedExprKind::Index(x, y) => Place(expr.0, PlaceKind::Index(x, y)),
        TypedExprKind::Dot(x, y) => Place(expr.0, PlaceKind::Dot(x, y)),
        _ => {
            return Err(Diagnostic(
                Severity::Error,
                span.containing(DiagnosticKind::NotAnLvalue(expr.to_string())),
            ))
        }
    })
}

/// Assert two types are the same and produce a validation error otherwise
fn expect_identical_types<'a, 'input>(
    lhs: &'a TastType<'input>,
    rhs: &'a TastType<'input>,
    span: Span,
) -> Result<(), Diagnostic> {
    if lhs != rhs {
        Err(Diagnostic(
            Severity::Error,
            span.containing(DiagnosticKind::ExpectedSameType(
                lhs.to_string(),
                rhs.to_string(),
            )),
        ))
    } else {
        Ok(())
    }
}

/// Assert a condition and produce a [`DiagnosticKind::ExpectedGot`] diagnostic otherwise.
fn expect(
    condition: bool,
    expected_str: String,
    got_str: String,
    span: Span,
) -> Result<(), Diagnostic> {
    if !condition {
        Err(Diagnostic(
            Severity::Error,
            span.containing(DiagnosticKind::ExpectedGot {
                expected: expected_str,
                got: got_str,
            }),
        ))
    } else {
        Ok(())
    }
}

// FIXME: this NEEDS to be rewritten to use references almost everywhere and be
// no-clone. We stack overflow for deep expressions which is VERY VERY BAD.
/// Type check and infer an [AST expression](Expr) to a [TAST
/// expression](TypedExpr).
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines)] // FIXME: make this fn shorter
#[allow(clippy::module_name_repetitions)]
pub fn type_expr<'input>(
    scope: &Scope<'input>,
    expr: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let expr_span = expr.0.span();
    Ok(match expr.0.into_value() {
        ExprKind::Comma(lhs, rhs) => {
            let at = type_expr(scope, *lhs)?;
            let bt = type_expr(scope, *rhs)?;
            TypedExpr(
                bt.0.clone(),
                TypedExprKind::Comma(Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Assignment(mode, place, value) => {
            // Desugar `x += y` to `x = x + y`.
            let (place, value) = desugar_assignment(mode, *place, *value);

            let place_t = expr_to_place(expr_span, type_expr(scope, place)?)?;
            let value_t = type_expr(scope, value)?;

            if place_t.0 != value_t.0 {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::InvalidAssignmentRightHandSideType {
                        expected: place_t.0.to_string(),
                        got: value_t.0.to_string(),
                    }),
                ));
            }

            TypedExpr(
                place_t.0.clone(),
                TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t)),
            )
        }

        ExprKind::UnaryNot(x) => {
            let x_ty = type_expr(scope, *x)?;
            if x_ty.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span
                        .containing(DiagnosticKind::UnaryNotExpectedBoolean(x_ty.0.to_string())),
                ));
            }
            TypedExpr(x_ty.0.clone(), TypedExprKind::UnaryNot(Box::new(x_ty)))
        }
        ExprKind::UnaryBitwiseNot(x) => {
            let x_ty = type_expr(scope, *x)?;
            if !x_ty.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::UnaryBitwiseNotExpectedInteger(
                        x_ty.0.to_string(),
                    )),
                ));
            }
            TypedExpr(
                x_ty.0.clone(),
                TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)),
            )
        }
        ExprKind::UnaryMinus(x) => {
            let x_ty = type_expr(scope, *x)?;
            if !x_ty.0.is_signed_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::UnaryMinusExpectedSignedInteger(
                        x_ty.0.to_string(),
                    )),
                ));
            }
            TypedExpr(x_ty.0.clone(), TypedExprKind::UnaryMinus(Box::new(x_ty)))
        }
        ExprKind::UnaryAddressOf(x) => {
            let x_ty = type_expr(scope, *x)?;
            TypedExpr(
                TastType::Ptr(Box::new(x_ty.0.clone())),
                TypedExprKind::UnaryAddressOf(Box::new(expr_to_place(expr_span, x_ty)?)),
            )
        }
        ExprKind::UnaryDereference(x) => {
            let x_ty = type_expr(scope, *x)?;
            if let TastType::Ptr(tt) = x_ty.0.clone() {
                TypedExpr(*tt, TypedExprKind::UnaryDereference(Box::new(x_ty)))
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::CannotDereferenceNonPointer(
                        x_ty.0.to_string(),
                    )),
                ));
            }
        }

        ExprKind::Index(ptr, offset) => {
            let ptr_t = type_expr(scope, *ptr)?;
            let offset_t = type_expr(scope, *offset)?;

            if !offset_t.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::IndexOffsetMustBeInteger(
                        offset_t.0.to_string(),
                    )),
                ));
            }

            if let TastType::Ptr(points_to_ty) = ptr_t.0.clone() {
                TypedExpr(
                    *points_to_ty,
                    TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)),
                )
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::CannotIndexIntoNonPointer(
                        ptr_t.0.to_string(),
                    )),
                ));
            }
        }

        ExprKind::Dot(obj, key) => {
            let obj_span = obj.0.span();
            let obj_t = type_expr(scope, *obj)?;

            if let TastType::Struct(fields) | TastType::Union(fields) = obj_t.0.clone() {
                if let Some(ty) = fields.get(key.value()) {
                    TypedExpr(
                        ty.clone(),
                        TypedExprKind::Dot(Box::new(expr_to_place(obj_span, obj_t)?), key.value()),
                    )
                } else {
                    return Err(Diagnostic(
                        Severity::Error,
                        expr_span.containing(DiagnosticKind::StructOrUnionDoesNotHaveMember(
                            obj_t.to_string(),
                            key.into_value().to_string(),
                        )),
                    ));
                }
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::StructMemberAccessOnNonStruct(
                        obj_t.to_string(),
                    )),
                ));
            }
        }
        ExprKind::Arrow(obj, key) => {
            let obj_t = type_expr(scope, *obj.clone())?;

            if let TastType::Ptr(_) = obj_t.0 {
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
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::CannotDereferenceNonPointer(
                        obj_t.to_string(),
                    )),
                ));
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
            match ft.0.clone() {
                TastType::Fn(ArgumentDeclarationList::NonVariadic(arg_types), ret_type) => {
                    if arg_types.len() != args_t.len() {
                        return Err(Diagnostic(
                            Severity::Error,
                            expr_span.containing(DiagnosticKind::FunctionArgumentCountMismatch {
                                expected: arg_types.len().to_string(),
                                got: args_t.len().to_string(),
                            }),
                        ));
                    }

                    for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                        if arg_type.ty != arg_t.0 {
                            return Err(Diagnostic(
                                Severity::Error,
                                args.value()[i].0.span().containing(
                                    DiagnosticKind::FunctionArgumentTypeMismatch {
                                        n: i,
                                        expected: arg_type.to_string(),
                                        got: arg_t.0.to_string(),
                                    },
                                ),
                            ));
                        }
                    }

                    TypedExpr(
                        ret_type.into_option().unwrap_or(TastType::Void),
                        TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_t),
                    )
                }
                TastType::Fn(ArgumentDeclarationList::Variadic(beginning_arg_types), ret_type) => {
                    if beginning_arg_types.len() > args_t.len() {
                        return Err(Diagnostic(
                            Severity::Error,
                            expr_span.containing(DiagnosticKind::FunctionArgumentCountMismatch {
                                expected: format!("at least {}", beginning_arg_types.len()),
                                got: args_t.len().to_string(),
                            }),
                        ));
                    }

                    for (i, (arg_type, arg_t)) in
                        beginning_arg_types.iter().zip(args_t.iter()).enumerate()
                    {
                        if arg_type.ty != arg_t.0 {
                            return Err(Diagnostic(
                                Severity::Error,
                                args.value()[i].0.span().containing(
                                    DiagnosticKind::FunctionArgumentTypeMismatch {
                                        n: i,
                                        expected: arg_type.to_string(),
                                        got: arg_t.0.to_string(),
                                    },
                                ),
                            ));
                        }
                    }

                    // the rest may be any, so we don't need to check them
                    TypedExpr(
                        ret_type.into_option().unwrap_or(TastType::Void),
                        TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_t),
                    )
                }
                _ => {
                    return Err(Diagnostic(
                        Severity::Error,
                        expr_span.containing(DiagnosticKind::CannotCallNonFunction(ft.to_string())),
                    ));
                }
            }
        }

        ExprKind::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(scope, *cond.clone())?;
            let if_true_t = type_expr(scope, *if_true)?;
            let if_false_t = type_expr(scope, *if_false)?;

            if cond_t.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    cond.0
                        .span()
                        .containing(DiagnosticKind::TernaryConditionMustBeBoolean(
                            cond_t.to_string(),
                        )),
                ));
            }

            expect_identical_types(&if_true_t.0, &if_false_t.0, expr_span)?;

            TypedExpr(
                if_true_t.0.clone(),
                TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t)),
            )
        }

        ExprKind::Logical(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect(
                lhs_t.0 == TastType::Bool,
                "bool".to_string(),
                lhs_t.0.to_string(),
                lhs_span,
            )?;
            expect(
                rhs_t.0 == TastType::Bool,
                "bool".to_string(),
                rhs_t.0.to_string(),
                rhs_span,
            )?;

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t)),
            )
        }
        ExprKind::Equality(op, lhs, rhs) => {
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_t = type_expr(scope, *rhs)?;

            if lhs_t.0.is_integer() && rhs_t.0.is_integer() && lhs_t.0 == rhs_t.0 {
                // int == int is valid
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (&lhs_t.0, &rhs_t.0) {
                // *T == *U is valid
            } else if lhs_t.0 == TastType::Bool && rhs_t.0 == TastType::Bool {
                // bool == bool is valid
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::EqualityOperators(
                        lhs_t.0.to_string(),
                        rhs_t.0.to_string(),
                    )),
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Equality(op, Box::new(lhs_t), Box::new(rhs_t)),
            )
        }
        ExprKind::BinaryBitwise(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect(
                lhs_t.0.is_integer(),
                "integer".to_string(),
                lhs_t.0.to_string(),
                lhs_span,
            )?;
            expect(
                rhs_t.0.is_integer(),
                "integer".to_string(),
                rhs_t.0.to_string(),
                rhs_span,
            )?;

            if matches!(op, BinaryBitwise::Shl | BinaryBitwise::Shr) {
                expect(
                    lhs_t.0.is_signed_integer(),
                    "signed integer".to_string(),
                    lhs_t.0.to_string(),
                    lhs_span,
                )?;
            }

            expect_identical_types(&lhs_t.0, &rhs_t.0, expr_span)?;

            TypedExpr(
                lhs_t.0.clone(),
                TypedExprKind::BinaryBitwise(op, Box::new(lhs_t), Box::new(rhs_t)),
            )
        }
        ExprKind::Comparison(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect(
                lhs_t.0.is_integer(),
                "integer".to_string(),
                lhs_t.0.to_string(),
                lhs_span,
            )?;
            expect(
                rhs_t.0.is_integer(),
                "integer".to_string(),
                rhs_t.0.to_string(),
                rhs_span,
            )?;

            expect_identical_types(&lhs_t.0, &rhs_t.0, expr_span)?;

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Comparison(op, Box::new(lhs_t), Box::new(rhs_t)),
            )
        }
        ExprKind::Arithmetic(op, lhs, rhs) => {
            let lhs_span = lhs.0.span();
            let lhs_t = type_expr(scope, *lhs)?;
            let rhs_span = rhs.0.span();
            let rhs_t = type_expr(scope, *rhs)?;

            expect(
                lhs_t.0.is_integer(),
                "integer".to_string(),
                lhs_t.0.to_string(),
                lhs_span,
            )?;
            expect(
                rhs_t.0.is_integer(),
                "integer".to_string(),
                rhs_t.0.to_string(),
                rhs_span,
            )?;

            expect_identical_types(&lhs_t.0, &rhs_t.0, expr_span)?;

            TypedExpr(
                lhs_t.0.clone(),
                TypedExprKind::Arithmetic(op, Box::new(lhs_t), Box::new(rhs_t)),
            )
        }

        ExprKind::Cast(x, ty) => {
            let x_t = type_expr(scope, *x)?;
            let resolved_ty = resolve_type(scope, ty)?;

            if x_t.0.is_integer() && resolved_ty.is_integer() {
                // int -> int cast is valid
                TypedExpr(
                    resolved_ty.clone(),
                    TypedExprKind::Cast(Box::new(x_t), resolved_ty),
                )
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (&x_t.0, &resolved_ty) {
                // *T -> *U cast is valid
                TypedExpr(
                    resolved_ty.clone(),
                    TypedExprKind::Cast(Box::new(x_t), resolved_ty),
                )
            } else if let (TastType::Ptr(_), _) | (_, TastType::Ptr(_)) = (&x_t.0, &resolved_ty) {
                // ensure one is an int
                if x_t.0.is_integer() || resolved_ty.is_integer() {
                    // *T -> int or int -> *T cast is valid
                    TypedExpr(
                        resolved_ty.clone(),
                        TypedExprKind::Cast(Box::new(x_t), resolved_ty),
                    )
                } else {
                    return Err(Diagnostic(
                        Severity::Error,
                        expr_span.containing(DiagnosticKind::InvalidCast(
                            x_t.0.to_string(),
                            resolved_ty.to_string(),
                        )),
                    ));
                }
            } else if x_t.0 == TastType::Bool && resolved_ty.is_integer() {
                // bool -> int cast is valid
                TypedExpr(
                    resolved_ty.clone(),
                    TypedExprKind::Cast(Box::new(x_t), resolved_ty),
                )
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::InvalidCast(
                        x_t.0.to_string(),
                        resolved_ty.to_string(),
                    )),
                ));
            }
        }

        ExprKind::SizeOf(ty) => {
            let resolved_ty = resolve_type(scope, ty)?;
            TypedExpr(TastType::U64, TypedExprKind::SizeOf(resolved_ty))
        }

        ExprKind::NumberLiteral(n) => TypedExpr(TastType::I32, TypedExprKind::NumberLiteral(n)),
        ExprKind::StringLiteral(str) => TypedExpr(
            TastType::Ptr(Box::new(TastType::U8)),
            TypedExprKind::StringLiteral(str),
        ),
        ExprKind::CharLiteral(ch) => TypedExpr(TastType::U8, TypedExprKind::CharLiteral(ch)),
        ExprKind::Identifier(i) => {
            let ty = scope.get_value(i).ok_or_else(|| {
                Diagnostic(
                    Severity::Error,
                    expr_span.containing(DiagnosticKind::UnableToResolveIdentifier(i.to_string())),
                )
            })?;
            TypedExpr(ty.clone(), TypedExprKind::Identifier(i))
        }
        ExprKind::BooleanLiteral(value) => {
            TypedExpr(TastType::Bool, TypedExprKind::BooleanLiteral(value))
        }
    })
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;

    mod desugar_assignment {
        use zrc_parser::ast::expr::Arithmetic;

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
}
