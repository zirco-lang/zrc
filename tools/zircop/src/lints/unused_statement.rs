//! `unused_statement`: Unused statement detection
//!
//! This lint checks for expression statements that have no side effects.
//! These are statements like `1 + 2;` or `x;` that compute a value but
//! don't actually do anything with it. The lint warns about such statements
//! as they likely indicate a logical error or leftover debugging code.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_typeck::{tast::stmt::TypedDeclaration, typeck::BlockMetadata};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintLabelKind, LintNoteKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `unused_statement`: Unused statement detection
///
/// This lint walks through blocks and detects any expression statement
/// that has no side effects. Such statements compute a value but don't
/// use it, which is likely a mistake.
pub struct UnusedStatementLint;
impl UnusedStatementLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for UnusedStatementLint {
    fn lint_tast(&self, program: Vec<Spanned<TypedDeclaration<'_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
        };
        vis.visit_tc_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `unused_statement` lint
struct Visit {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,
}

impl<'input> SemanticVisit<'input, '_> for Visit {
    fn visit_tc_block(&mut self, block: &BlockMetadata<'input>) {
        use zrc_typeck::tast::stmt::TypedStmtKind;

        for stmt in &block.stmts {
            // Check if this is an expression statement
            if let TypedStmtKind::ExprStmt(expr) = stmt.kind.value() {
                // Check if the expression has no side effects
                if !has_side_effects(expr.kind.value()) {
                    let span = stmt.kind.span();
                    self.diagnostics.push(
                        LintDiagnostic::warning(
                            LintDiagnosticKind::UnusedStatement.in_span(span),
                        )
                        .with_label(GenericLabel::warning(
                            LintLabelKind::UnusedStatement.in_span(span),
                        ))
                        .with_note(LintNoteKind::UnusedStatement),
                    );
                }
            }
        }

        // Walk into nested blocks
        SemanticVisit::walk_tc_block(self, block);
    }
}

/// Check if an expression has side effects (recursively)
fn has_side_effects(expr: &zrc_typeck::tast::expr::TypedExprKind<'_>) -> bool {
    use zrc_typeck::tast::expr::TypedExprKind;

    match expr {
        // Expressions with direct side effects
        TypedExprKind::Assignment(_, _) => true,
        TypedExprKind::Call(_, _) => true,
        TypedExprKind::PrefixIncrement(_) => true,
        TypedExprKind::PrefixDecrement(_) => true,
        TypedExprKind::PostfixIncrement(_) => true,
        TypedExprKind::PostfixDecrement(_) => true,

        // Comma operator: check both sides
        TypedExprKind::Comma(left, right) => {
            has_side_effects(left.kind.value()) || has_side_effects(right.kind.value())
        }

        // Ternary: check all branches
        TypedExprKind::Ternary(cond, then_expr, else_expr) => {
            has_side_effects(cond.kind.value())
                || has_side_effects(then_expr.kind.value())
                || has_side_effects(else_expr.kind.value())
        }

        // Binary operations: check both operands
        TypedExprKind::BinaryBitwise(_, left, right)
        | TypedExprKind::Logical(_, left, right)
        | TypedExprKind::Equality(_, left, right)
        | TypedExprKind::Comparison(_, left, right)
        | TypedExprKind::Arithmetic(_, left, right) => {
            has_side_effects(left.kind.value()) || has_side_effects(right.kind.value())
        }

        // Unary operations: check the operand
        TypedExprKind::UnaryNot(expr)
        | TypedExprKind::UnaryBitwiseNot(expr)
        | TypedExprKind::UnaryMinus(expr)
        | TypedExprKind::UnaryDereference(expr) => has_side_effects(expr.kind.value()),

        // Address-of and dot operations don't have side effects on their own
        TypedExprKind::UnaryAddressOf(_) | TypedExprKind::Dot(_, _) => false,

        // Index: check both array and index expressions
        TypedExprKind::Index(array, index) => {
            has_side_effects(array.kind.value()) || has_side_effects(index.kind.value())
        }

        // Cast: check the expression being cast
        TypedExprKind::Cast(expr, _) => has_side_effects(expr.kind.value()),

        // SizeOf takes a Type, not an expression, so no side effects
        TypedExprKind::SizeOf(_) => false,

        // Array literals: check all elements
        TypedExprKind::ArrayLiteral(elements) => {
            elements.iter().any(|e| has_side_effects(e.kind.value()))
        }

        // Struct construction: check all field values
        TypedExprKind::StructConstruction(fields) => fields
            .fields
            .iter()
            .any(|(_, value)| has_side_effects(value.kind.value())),

        // Leaf expressions (no side effects)
        TypedExprKind::NumberLiteral(_, _)
        | TypedExprKind::StringLiteral(_)
        | TypedExprKind::CharLiteral(_)
        | TypedExprKind::BooleanLiteral(_)
        | TypedExprKind::Identifier(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use zrc_utils::spanned_test;

    use super::*;
    use crate::zircop_lint_test;

    zircop_lint_test! {
        name: unused_statement_arithmetic,
        source: indoc!{"
            fn f() -> void {
                1 + 2;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    21,
                    LintDiagnosticKind::UnusedStatement,
                    27
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        21,
                        LintLabelKind::UnusedStatement,
                        27
                    )
                )
            ).with_note(
                LintNoteKind::UnusedStatement
            ),
        ]
    }

    zircop_lint_test! {
        name: unused_statement_with_call_in_arithmetic,
        source: indoc!{"
            fn g() -> i32 { return 0; }
            fn f() -> void {
                g() + 2;
            }
        "},
        diagnostics: vec![]
    }

    zircop_lint_test! {
        name: unused_statement_variable,
        source: indoc!{"
            fn f() -> void {
                let x = 10;
                x;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    37,
                    LintDiagnosticKind::UnusedStatement,
                    39
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        37,
                        LintLabelKind::UnusedStatement,
                        39
                    )
                )
            ).with_note(
                LintNoteKind::UnusedStatement
            ),
        ]
    }

    zircop_lint_test! {
        name: no_warning_for_assignment,
        source: indoc!{"
            fn f() -> void {
                let x = 0;
                x = 5;
            }
        "},
        diagnostics: vec![]
    }

    zircop_lint_test! {
        name: no_warning_for_call,
        source: indoc!{"
            fn g() -> void {}
            fn f() -> void {
                g();
            }
        "},
        diagnostics: vec![]
    }

    zircop_lint_test! {
        name: no_warning_for_increment,
        source: indoc!{"
            fn f() -> void {
                let x = 0;
                x++;
                ++x;
            }
        "},
        diagnostics: vec![]
    }

    zircop_lint_test! {
        name: warning_for_comma_with_no_side_effects,
        source: indoc!{"
            fn f() -> void {
                1, 2;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    21,
                    LintDiagnosticKind::UnusedStatement,
                    26
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        21,
                        LintLabelKind::UnusedStatement,
                        26
                    )
                )
            ).with_note(
                LintNoteKind::UnusedStatement
            ),
        ]
    }

    zircop_lint_test! {
        name: no_warning_for_comma_with_side_effects,
        source: indoc!{"
            fn f() -> void {
                let x = 0;
                1, x = 2;
            }
        "},
        diagnostics: vec![]
    }
}
