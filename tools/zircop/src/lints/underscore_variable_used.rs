//! `underscore_variable_used`: Variable starting with an underscore is used
//! in code
//!
//! This lint checks for instances where variables that start with an
//! underscore are used in the code. In Zirco, variables prefixed with an
//! underscore are conventionally meant to indicate that they are intentionally
//! unused. Using such variables can lead to confusion about their purpose and
//! may suggest that the developer intended to use them but forgot.
//!
//! The lint encourages developers to either remove the underscore prefix if
//! the variable is indeed used, or to rename the variable to avoid the
//! underscore if it is meant to be used.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_typeck::{
    tast::{stmt::TypedDeclaration, ty::Type},
    typeck::BlockMetadata,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintHelpKind, LintLabelKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `underscore_variable_used`: Variable starting with an underscore is used
/// in code
///
/// This lint recursively searches every block's associated
/// [`BlockMetadata::scope`] for any variables with the
/// [`zrc_typeck::typeck::ValueEntry::referenced_spans`] parameter non-empty and
/// a name starting with an underscore.
pub struct UnderscoreVariableUsedLint;
impl UnderscoreVariableUsedLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for UnderscoreVariableUsedLint {
    fn lint_tast(&self, program: Vec<Spanned<TypedDeclaration<'_, '_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
            reported_vars: vec![],
        };
        vis.visit_tc_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `underscore_variable_used` lint
struct Visit<'a> {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,

    /// The list of already reported variable names to avoid duplicates
    // Although some variables may be declared in different scopes with the same name,
    // it doesn't really matter for this lint since the user should still be
    // notified about each used underscore variable once.
    reported_vars: Vec<&'a str>,
}
impl<'input, 'gs> SemanticVisit<'input, 'gs> for Visit<'input> {
    fn visit_tc_block(&mut self, block: &BlockMetadata<'input, 'gs>) {
        // Call the default traversal first so other visits run as expected.
        SemanticVisit::walk_tc_block(self, block);

        // Check the current block's scope for underscore-used variables.
        for (var_name, var_entry_rc) in block.scope.values.iter() {
            let var_entry = var_entry_rc.borrow();
            if let Some(first_use) = var_entry.referenced_spans.first()
                && var_name.starts_with('_')
                && !self.reported_vars.contains(&var_name)
                // Functions are also stored as variables in the scope, but
                // we don't want to report them as underscore-prefixed functions
                // are commonly used as private/internal helpers where usage is intentional
                && !matches!(var_entry.ty, Type::Fn(_))
            {
                let span = var_entry.declaration_span;
                self.diagnostics.push(
                    LintDiagnostic::warning(
                        LintDiagnosticKind::UnderscoreVariableUsed.in_span(span),
                    )
                    .with_label(GenericLabel::warning(
                        LintLabelKind::UnderscoreVariableDeclaration.in_span(span),
                    ))
                    .with_label(GenericLabel::note(
                        LintLabelKind::UnderscoreVariableUsage.in_span(*first_use),
                    ))
                    .with_help(LintHelpKind::RenameVariable(
                        var_name
                            .to_string()
                            .strip_prefix("_")
                            .expect("variable name already determined to start with underscore")
                            .to_string(),
                    )),
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
        name: underscore_variable_used_lints_properly,
        source: indoc!{"
            fn f() -> i32 {
                let _used = 10;
                return _used;
            }
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(
                    24,
                    LintDiagnosticKind::UnderscoreVariableUsed,
                    34
                )
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(
                        24,
                        LintLabelKind::UnderscoreVariableDeclaration,
                        34
                    )
                )
            ).with_label(
                GenericLabel::note(
                    spanned_test!(
                        47,
                        LintLabelKind::UnderscoreVariableUsage,
                        52
                    )
                )
            ).with_help(
                LintHelpKind::RenameVariable("used".to_string())
            ),
        ]
    }
}
