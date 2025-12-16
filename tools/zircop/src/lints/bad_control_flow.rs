//! `bad_control_flow`: Bad control flow practices
//!
//! This lint detects the following bad control flow:
//! - Empty `else` blocks
//! - Empty `if` blocks with an `else` block present
//! - `while` loops with an empty body
//!
//! Each of these patterns can lead to confusing or misleading code and should
//! be avoided.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_parser::ast::stmt::{Declaration, Stmt, StmtKind};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintHelpKind, LintLabelKind},
    lint::Lint,
    visit::SyntacticVisit,
};

/// `bad_control_flow`: Bad control flow practices
///
/// This lint recursively scans the typed AST for bad control flow patterns
/// such as empty `else` blocks, empty `if` blocks with an `else` block present,
/// and `while` loops with empty bodies.
pub struct BadControlFlowLint;
impl BadControlFlowLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for BadControlFlowLint {
    fn lint_ast(&self, program: Vec<Spanned<Declaration<'_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
        };
        vis.visit_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `unused_variables` lint
struct Visit {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,
}
impl<'input> SyntacticVisit<'input> for Visit {
    fn visit_stmt(&mut self, stmt: &Stmt<'input>) {
        #[expect(clippy::else_if_without_else)]
        if let StmtKind::IfStmt(cond, then, then_else) = stmt.0.value() {
            let then_span = then.0.span();
            if let Some(else_block) = then_else {
                // 1. Empty if or else block

                if let StmtKind::BlockStmt(stmts) = then.0.value()
                    && stmts.is_empty()
                {
                    self.diagnostics.push(
                        LintDiagnostic::warning(
                            LintDiagnosticKind::SussyControlFlow.in_span(else_block.0.span()),
                        )
                        .with_label(GenericLabel::note(
                            LintLabelKind::EmptyIf.in_span(then_span),
                        ))
                        .with_label(GenericLabel::warning(
                            LintLabelKind::NonEmptyElse.in_span(else_block.0.span()),
                        ))
                        .with_help(LintHelpKind::InvertIfCondition(cond.to_string())),
                    );
                } else if let StmtKind::BlockStmt(stmts) = else_block.0.value()
                    && stmts.is_empty()
                {
                    self.diagnostics.push(
                        LintDiagnostic::warning(
                            LintDiagnosticKind::SussyControlFlow.in_span(else_block.0.span()),
                        )
                        .with_label(GenericLabel::warning(
                            LintLabelKind::EmptyElseBlock.in_span(else_block.0.span()),
                        ))
                        .with_help(LintHelpKind::RemoveElseBlock),
                    );
                }
            }
        } else if let StmtKind::WhileStmt(_, body) = stmt.0.value() {
            // 2. Empty while body
            if let StmtKind::BlockStmt(stmts) = body.0.value()
                && stmts.is_empty()
            {
                self.diagnostics.push(LintDiagnostic::warning(
                    LintDiagnosticKind::EmptyWhileBody.in_span(body.0.span()),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use zrc_utils::{span::Span, spanned_test};

    use super::*;
    use crate::zircop_lint_test;

    zircop_lint_test! {
        name: bad_control_flow_detects_empty_if,
        source: indoc!{"
            fn f(x: bool) -> i32 {
                if (x) {}
                else return 1;
                return 0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    46,
                    LintDiagnosticKind::SussyControlFlow,
                    55
                )
            ).with_label(
                GenericLabel::note(
                    LintLabelKind::EmptyIf.in_span(Span::from_positions_and_file(
                        34,
                        36,
                        "<test>",
                    )),
                )
            ).with_label(
                GenericLabel::warning(
                    LintLabelKind::NonEmptyElse.in_span(Span::from_positions_and_file(
                        46,
                        55,
                        "<test>",
                    )),
                )
            ).with_help(
                LintHelpKind::InvertIfCondition("x".to_string())
            ),
        ]
    }

    zircop_lint_test! {
        name: bad_control_flow_detects_empty_else,
        source: indoc!{"
            fn f(x: bool) -> i32 {
                if (x) return 1;
                else {}
                return 0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    53,
                    LintDiagnosticKind::SussyControlFlow,
                    55
                )
            ).with_label(
                GenericLabel::warning(
                    LintLabelKind::EmptyElseBlock.in_span(Span::from_positions_and_file(
                        53,
                        55,
                        "<test>",
                    )),
                )
            ).with_help(
                LintHelpKind::RemoveElseBlock
            ),
        ]
    }

    zircop_lint_test! {
        name: bad_control_flow_detects_empty_while,
        source: indoc!{"
            fn f() -> i32 {
                while (true) {}
                return 0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    33,
                    LintDiagnosticKind::EmptyWhileBody,
                    35
                )
            ),
        ]
    }
}
