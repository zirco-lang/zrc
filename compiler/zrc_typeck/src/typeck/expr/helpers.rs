//! helper tools used in the type-checking of expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpanExt};
use zrc_parser::ast::expr::{Assignment, Expr, ExprKind};
use zrc_utils::span::{Span, Spannable};

use crate::tast::{
    expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

use super::super::diagnostics::DiagnosticCollector;

/// Handle a type checking result by collecting any diagnostic and returning
/// a poison value if an error occurred, allowing type checking to continue.
///
/// This enables error recovery: instead of stopping at the first error, we
/// collect it and return a poison value that propagates through the type system.
pub fn handle_type_error<'input>(
    result: Result<TypedExpr<'input>, Diagnostic>,
    diagnostics: &DiagnosticCollector,
    span: Span,
) -> TypedExpr<'input> {
    match result {
        Ok(expr) => expr,
        Err(diagnostic) => {
            diagnostics.push(diagnostic);
            // Return a poison expression to continue type checking
            TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(span),
            }
        }
    }
}

/// Desugar an assignment like `x += y` to `x = x + y`.
pub fn desugar_assignment<'input>(
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
pub fn expr_to_place(span: Span, expr: TypedExpr) -> Result<Place, Diagnostic> {
    let kind_span = expr.kind.span();
    let stringified = expr.inferred_type.to_string();

    #[expect(clippy::wildcard_enum_match_arm)]
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
#[cfg(test)]
pub fn expect_identical_types<'a, 'input>(
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
/// otherwise. Returns Ok if the condition is true or if the got type is poison
/// (to prevent cascading errors).
pub fn expect(
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

/// Assert that a type is an integer type. Returns Ok if poison (to prevent
/// cascading errors).
pub fn expect_is_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    if ty.is_poison() {
        Ok(())
    } else {
        expect(ty.is_integer(), "integer".to_string(), ty.to_string(), span)
    }
}

/// Assert that a type is a signed integer type. Returns Ok if poison (to
/// prevent cascading errors).
pub fn expect_is_signed_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    if ty.is_poison() {
        Ok(())
    } else {
        expect(
            ty.is_signed_integer(),
            "signed integer".to_string(),
            ty.to_string(),
            span,
        )
    }
}

/// Assert that a type is an unsigned integer type. Returns Ok if poison (to
/// prevent cascading errors).
pub fn expect_is_unsigned_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    if ty.is_poison() {
        Ok(())
    } else {
        expect(
            ty.is_unsigned_integer(),
            "unsigned integer".to_string(),
            ty.to_string(),
            span,
        )
    }
}

/// Try to coerce an expression to a target type if possible.
/// If the expression type is `{int}`, it will be resolved to the target type.
/// Returns the coerced expression if successful, or the original if types
/// already match.
pub fn try_coerce_to<'input>(
    expr: TypedExpr<'input>,
    target_type: &TastType<'input>,
) -> TypedExpr<'input> {
    if expr.inferred_type == *target_type {
        expr
    } else if expr.inferred_type.can_implicitly_cast_to(target_type) {
        TypedExpr {
            inferred_type: target_type.clone(),
            kind: expr.kind,
        }
    } else {
        expr
    }
}

/// Resolve binary operands for operations that require matching types.
/// Returns a tuple of (`result_type`, lhs, rhs) where both operands have been
/// coerced to a compatible type. If both are `{int}`, they resolve to `i32`.
/// If either operand is poison, propagates poison type.
pub fn resolve_binary_int_operands<'input>(
    lhs: TypedExpr<'input>,
    rhs: TypedExpr<'input>,
) -> (TastType<'input>, TypedExpr<'input>, TypedExpr<'input>) {
    // If either side is poison, propagate poison
    if lhs.inferred_type.is_poison() || rhs.inferred_type.is_poison() {
        return (TastType::Poison, lhs, rhs);
    }

    if lhs.inferred_type == rhs.inferred_type {
        // Both have the same type
        if matches!(lhs.inferred_type, TastType::Int) {
            // Both are {int}, resolve to i32
            let lhs_resolved = TypedExpr {
                inferred_type: TastType::I32,
                kind: lhs.kind,
            };
            let rhs_resolved = TypedExpr {
                inferred_type: TastType::I32,
                kind: rhs.kind,
            };
            (TastType::I32, lhs_resolved, rhs_resolved)
        } else {
            (lhs.inferred_type.clone(), lhs, rhs)
        }
    } else if lhs.inferred_type.can_implicitly_cast_to(&rhs.inferred_type) {
        // lhs can coerce to rhs type (e.g., {int} -> i8)
        let lhs_coerced = try_coerce_to(lhs, &rhs.inferred_type);
        (rhs.inferred_type.clone(), lhs_coerced, rhs)
    } else if rhs.inferred_type.can_implicitly_cast_to(&lhs.inferred_type) {
        // rhs can coerce to lhs type (e.g., {int} -> i8)
        let rhs_coerced = try_coerce_to(rhs, &lhs.inferred_type);
        (lhs.inferred_type.clone(), lhs, rhs_coerced)
    } else {
        // No coercion possible, return as-is (caller will handle error)
        (lhs.inferred_type.clone(), lhs, rhs)
    }
}

#[cfg(test)]
mod tests {

    use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
    use zrc_parser::ast::expr::{Arithmetic, Assignment, BinaryBitwise, Expr, ExprKind};
    use zrc_utils::{
        span::{Span, Spannable},
        spanned_test,
    };

    use super::*;
    use crate::tast::ty::Type as TastType;

    #[test]
    fn expect_identical_types_produces_proper_diagnostic() {
        let sample_span = Span::from_positions_and_file(0, 5, "<test>");
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
        let sample_span = Span::from_positions_and_file(0, 5, "<test>");
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
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(4, "b", 5)),
                ),
                (
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(4, "b", 5)),
                )
            );
        }

        #[test]
        fn arithmetic() {
            assert_eq!(
                // a += b
                desugar_assignment(
                    Assignment::Arithmetic(Arithmetic::Addition),
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(5, "b", 6)),
                ),
                (
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    // An exception to the normal spanning rules applies here
                    Expr(spanned_test!(
                        5,
                        ExprKind::Arithmetic(
                            Arithmetic::Addition,
                            Box::new(Expr::build_ident(spanned_test!(0, "a", 1))),
                            Box::new(Expr::build_ident(spanned_test!(5, "b", 6)))
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
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(6, "b", 7)),
                ),
                (
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    // An exception to the normal spanning rules applies here
                    Expr(spanned_test!(
                        6,
                        ExprKind::BinaryBitwise(
                            BinaryBitwise::Shr,
                            Box::new(Expr::build_ident(spanned_test!(0, "a", 1))),
                            Box::new(Expr::build_ident(spanned_test!(6, "b", 7)))
                        ),
                        7
                    ))
                )
            );
        }
    }
}
