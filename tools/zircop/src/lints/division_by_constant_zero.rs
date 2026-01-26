//! `division_by_constant_zero`: Lint that detects division by constant zero.
//!
//! This lint inspects arithmetic division operations in the typed AST
//! and raises a warning if it detects a division by a constant zero literal.
//! Dividing by zero is undefined behavior and should be avoided.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_parser::{ast::expr::Arithmetic, lexer::NumberLiteral};
use zrc_typeck::tast::{
    expr::{TypedExpr, TypedExpr as TcExpr, TypedExprKind, TypedExprKind as TcExprKind},
    stmt::TypedDeclaration,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintLabelKind, LintNoteKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `division_by_constant_zero`: Division by constant zero detected
///
/// This lint inspects arithmetic division operations in the typed AST
/// and raises a warning if it detects a division by a constant zero literal or
/// simple expressions that evaluate to zero.
pub struct DivisionByConstantZero;
impl DivisionByConstantZero {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}

impl Lint for DivisionByConstantZero {
    fn lint_tast(&self, program: Vec<Spanned<TypedDeclaration<'_, '_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: Vec::new(),
        };

        vis.visit_tc_program(&program);
        vis.diagnostics
    }
}

/// TAST visitor for the `division_by_constant_zero` lint
struct Visit {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,
}

impl<'input> SemanticVisit<'input, '_> for Visit {
    fn visit_tc_expr(&mut self, expr: &TcExpr<'input>) {
        SemanticVisit::walk_tc_expr(self, expr);

        if let TcExprKind::Arithmetic(op, _lhs, rhs) = expr.kind.value()
            && matches!(op, Arithmetic::Division)
            && is_literal_zero(rhs.as_ref())
        {
            let span = expr.kind.span();
            self.diagnostics.push(
                LintDiagnostic::warning(LintDiagnosticKind::DivisionByConstantZero.in_span(span))
                    .with_label(GenericLabel::warning(
                        LintLabelKind::DivisionByConstantZero.in_span(span),
                    ))
                    .with_note(LintNoteKind::DivisionByConstantZero),
            );
        }
    }
}

/// Returns `true` if the given expression is a syntactic numeric literal
/// representing zero.
///
/// Supported forms include:
/// - Decimal literals: `0`, `00`
/// - Binary literals: `0b0`
/// - Hexadecimal literals: `0x0`
/// - Unary minus: `-0`
///
/// Underscores in numeric literals are ignored.
/// This function does not perform full constant evaluation.
fn is_literal_zero(expr: &TypedExpr<'_>) -> bool {
    #[expect(clippy::wildcard_enum_match_arm)]
    match expr.kind.value() {
        TypedExprKind::NumberLiteral(num, _ty) => {
            let raw = match num {
                NumberLiteral::Binary(val)
                | NumberLiteral::Decimal(val)
                | NumberLiteral::Hexadecimal(val) => val,
            };
            let no_underscores: String = raw.chars().filter(|&ch| ch != '_').collect();

            !no_underscores.is_empty() && no_underscores.chars().all(|ch| ch == '0')
        }
        TypedExprKind::UnaryMinus(inner) => is_literal_zero(inner),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use zrc_utils::spanned_test;

    use super::*;
    use crate::zircop_lint_test;

    zircop_lint_test! {
        name: division_by_zero_decimal,
        source: indoc!{"
            fn main() -> i32 {
                let x: i32 = 10;
                return x / 0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    51,
                    LintDiagnosticKind::DivisionByConstantZero,
                    56
                )
            ),
        ]
    }

    zircop_lint_test! {
        name: division_by_zero_hex,
        source: indoc!{"
            fn main() -> i32 {
                let x: i32 = 10;
                return x / 0x0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    51,
                    LintDiagnosticKind::DivisionByConstantZero,
                    58
                )
            ),
        ]
    }

    zircop_lint_test! {
        name: division_by_zero_unary_minus,
        source: indoc!{"
            fn main() -> i32 {
                let x: i32 = 10;
                return x / -0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    51,
                    LintDiagnosticKind::DivisionByConstantZero,
                    57
                )
            ),
        ]
    }

    zircop_lint_test! {
        name: division_by_nonzero_ok,
        source: indoc!{"
            fn main() -> i32 {
                let x: i32 = 10;
                return x / 2;
            }
        "},
        diagnostics: vec![]
    }
}
