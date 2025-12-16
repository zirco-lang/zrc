//! `unused_variables`: Unused variable declarations
//!
//! This lint checks for variable declarations that are never used
//! within Zirco programs. Unused variables can clutter the code,
//! reduce readability, and may indicate potential logical errors
//! in the program. The lint encourages developers to remove
//! unnecessary variable declarations to improve code clarity and
//! maintainability.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_typeck::{
    tast::{stmt::TypedDeclaration, ty::Type},
    typeck::BlockMetadata,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintHelpKind, LintLabelKind, LintNoteKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `unused_variables`: Unused variable declarations
///
/// This lint recursively searches every block's associated
/// [`BlockMetadata::scope`] for any variables with the
/// [`zrc_typeck::typeck::ValueEntry::referenced_spans`] parameter empty and a
/// name that does not start with an underscore.
pub struct UnusedVariablesLint;
impl UnusedVariablesLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for UnusedVariablesLint {
    fn lint_tast(&self, program: Vec<Spanned<TypedDeclaration<'_, '_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
            reported_vars: vec![],
        };
        vis.visit_tc_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `unused_variables` lint
struct Visit<'a> {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,

    /// The list of already reported variable names to avoid duplicates
    // Although some variables may be declared in different scopes with the same name,
    // it doesn't really matter for this lint since the user should still be
    // notified about each unused variable once.
    reported_vars: Vec<&'a str>,
}
impl<'input, 'gs> SemanticVisit<'input, 'gs> for Visit<'input> {
    fn visit_tc_block(&mut self, block: &BlockMetadata<'input, 'gs>) {
        // Call the default traversal first so other visits run as expected.
        SemanticVisit::walk_tc_block(self, block);

        // Check the current block's scope for unused variables that do not
        // start with an underscore. `ValueEntry`s are stored behind
        // `Rc<RefCell<...>>` so borrow the entry when inspecting it.
        for (var_name, var_entry_rc) in block.scope.values.iter() {
            let var_entry = var_entry_rc.borrow();
            if var_entry.referenced_spans.is_empty()
                && !var_name.starts_with('_')
                && !self.reported_vars.contains(&var_name)
                // Functions are also stored as variables in the scope, but
                // we don't want to report them as unused variables as they
                // may be extern
                && !matches!(var_entry.ty, Type::Fn(_))
            {
                let span = var_entry.declaration_span;
                self.diagnostics.push(
                    LintDiagnostic::warning(LintDiagnosticKind::UnusedVariable.in_span(span))
                        .with_label(GenericLabel::warning(
                            LintLabelKind::UnusedVariable.in_span(span),
                        ))
                        .with_help(LintHelpKind::RemoveVariableDeclaration)
                        .with_note(LintNoteKind::UnusedVariableSuppress(format!("_{var_name}"))),
                );
                self.reported_vars.push(var_name);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use zrc_utils::spanned_test;

    use super::*;
    use crate::zircop_lint_test;

    zircop_lint_test! {
        name: unused_variables_lints_properly,
        source: indoc!{"
            fn f() -> i32 {
                let unused = 10;
                let _with_underscore = 7;
                return 0;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    24,
                    LintDiagnosticKind::UnusedVariable,
                    35
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        24,
                        LintLabelKind::UnusedVariable,
                        35
                    )
                )
            ).with_help(
                LintHelpKind::RemoveVariableDeclaration
            ).with_note(
                LintNoteKind::UnusedVariableSuppress("_unused".to_string())
            ),
        ]
    }
}
