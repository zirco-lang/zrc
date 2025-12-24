use zrc_parser::{ast::expr::Arithmetic, lexer::NumberLiteral};
use zrc_typeck::tast::expr::{
    TypedExpr, TypedExpr as TcExpr, TypedExprKind, TypedExprKind as TcExprKind,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind},
    lint::Lint,
    visit::SemanticVisit,
};

pub struct DivisionByConstantZero;
impl DivisionByConstantZero {
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}

impl Lint for DivisionByConstantZero {
    fn lint_tast(
        &self,
        program: Vec<Spanned<zrc_typeck::tast::stmt::TypedDeclaration<'_, '_>>>,
    ) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: Vec::new(),
        };

        vis.visit_tc_program(&program);
        vis.diagnostics
    }
}

struct Visit {
    diagnostics: Vec<LintDiagnostic>,
}

impl<'input, 'gs> SemanticVisit<'input, 'gs> for Visit {
    fn visit_tc_expr(&mut self, expr: &TcExpr<'input>) {
        if let TcExprKind::Arithmetic(op, _lhs, rhs) = expr.kind.value() {
            if matches!(op, Arithmetic::Division) {
                if is_literal_zero(rhs.as_ref()) {
                    let span = expr.kind.span();
                    self.diagnostics.push(LintDiagnostic::new(
                        LintDiagnosticKind::DivisionByConstantZero.in_span(span),
                    ));
                }
            }
        }

        SemanticVisit::walk_tc_expr(self, expr);
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
    match expr.kind.value() {
        TypedExprKind::NumberLiteral(num, _ty) => {
            let raw = match num {
                NumberLiteral::Binary(s)
                | NumberLiteral::Decimal(s)
                | NumberLiteral::Hexadecimal(s) => s,
            };
            let no_underscores: String = raw.chars().filter(|&c| c != '_').collect();

            !no_underscores.is_empty() && no_underscores.chars().all(|c| c == '0')
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
            LintDiagnostic::new(
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
            LintDiagnostic::new(
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
            LintDiagnostic::new(
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
