//! `unreachable_code`: Unreachable code detection
//!
//! This lint checks for statements that appear after a statement
//! that always returns (e.g., `return`, `unreachable`). Such statements
//! can never be executed and indicate a potential logical error
//! in the program. The lint encourages developers to remove
//! unreachable statements to improve code clarity.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_typeck::{
    tast::stmt::TypedDeclaration,
    typeck::{BlockMetadata, BlockReturnActuality},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintLabelKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `unreachable_code`: Unreachable code detection
///
/// This lint walks through blocks and detects any statement that
/// follows a statement with [`BlockReturnActuality::AlwaysReturns`].
/// Such statements can never be executed.
pub struct UnreachableCodeLint;
impl UnreachableCodeLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for UnreachableCodeLint {
    fn lint_tast(&self, program: Vec<Spanned<TypedDeclaration<'_, '_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
        };
        vis.visit_tc_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `unreachable_code` lint
struct Visit {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,
}

impl<'input, 'gs> SemanticVisit<'input, 'gs> for Visit {
    fn visit_tc_block(&mut self, block: &BlockMetadata<'input, 'gs>) {
        let stmts = &block.stmts;

        // Track whether we've encountered a statement that always returns
        let mut found_always_returns_at = None::<Span>;

        for stmt in stmts {
            if let Some(return_span) = found_always_returns_at {
                // This statement is unreachable
                let span = stmt.kind.span();
                self.diagnostics.push(
                    LintDiagnostic::warning(LintDiagnosticKind::UnreachableCode.in_span(span))
                        .with_label(GenericLabel::warning(
                            LintLabelKind::UnreachableCode.in_span(span),
                        ))
                        .with_label(GenericLabel::note(
                            LintLabelKind::PriorControlFlow.in_span(return_span),
                        )),
                );
                // Continue checking to report all unreachable statements
            }

            if stmt.return_actuality == BlockReturnActuality::AlwaysReturns {
                found_always_returns_at = Some(stmt.kind.span());
            }
        }

        // Walk into nested blocks after checking this block
        SemanticVisit::walk_tc_block(self, block);
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use zrc_utils::spanned_test;

    use super::*;
    use crate::zircop_lint_test;

    zircop_lint_test! {
        name: unreachable_code_after_return,
        source: indoc!{"
            fn f() -> i32 {
                return 0;
                let _x = 1;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    34,
                    LintDiagnosticKind::UnreachableCode,
                    45
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        34,
                        LintLabelKind::UnreachableCode,
                        45
                    )
                )
            ).with_label(
                GenericLabel::note(
                    spanned_test!(
                        20,
                        LintLabelKind::PriorControlFlow,
                        29
                    )
                )
            ),
        ]
    }

    zircop_lint_test! {
        name: no_unreachable_code_when_proper,
        source: indoc!{"
            fn f() -> i32 {
                let x = 1;
                return x;
            }
        "},
        diagnostics: vec![]
    }
}
