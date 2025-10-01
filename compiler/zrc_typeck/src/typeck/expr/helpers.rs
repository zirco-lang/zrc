//! helper tools used in the type-checking of expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpanExt};
use zrc_parser::ast::expr::{Assignment, Expr, ExprKind};
use zrc_utils::span::{Span, Spannable};

use crate::tast::{
    expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

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
/// otherwise.
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

/// Assert that a type is an integer type
pub fn expect_is_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(ty.is_integer(), "integer".to_string(), ty.to_string(), span)
}

/// Assert that a type is a signed integer type
pub fn expect_is_signed_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(
        ty.is_signed_integer(),
        "signed integer".to_string(),
        ty.to_string(),
        span,
    )
}

/// Assert that a type is an unsigned integer type
pub fn expect_is_unsigned_integer(ty: &TastType, span: Span) -> Result<(), Diagnostic> {
    expect(
        ty.is_unsigned_integer(),
        "unsigned integer".to_string(),
        ty.to_string(),
        span,
    )
}

#[cfg(test)]
mod tests {

    use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
    use zrc_parser::ast::expr::{Arithmetic, Assignment, BinaryBitwise, Expr, ExprKind};
    use zrc_utils::{
        span::{Span, Spannable},
        spanned,
    };

    use super::*;
    use crate::tast::ty::Type as TastType;

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
}
