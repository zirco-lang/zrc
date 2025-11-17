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

use zrc_typeck::{
    tast::{stmt::TypedDeclaration, ty::Type},
    typeck::BlockMetadata,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind},
    lint::Lint,
    visit::SemanticVisit,
};

/// `underscore_variable_used`: Variable starting with an underscore is used
/// in code
///
/// This lint recursively searches every block's associated
/// [`BlockMetadata::scope`] for any variables with the
/// [`zrc_typeck::typeck::ValueEntry::used`] parameter set to `true` and a name
/// starting with an underscore.
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
            if var_entry.used
                && var_name.starts_with('_')
                && !self.reported_vars.contains(&var_name)
                // Functions are also stored as variables in the scope, but
                // we don't want to report them as underscore-prefixed functions
                // are commonly used as private/internal helpers where usage is intentional
                && !matches!(var_entry.ty, Type::Fn(_))
            {
                let span = var_entry.declaration_span;
                self.diagnostics.push(LintDiagnostic::new(
                    LintDiagnosticKind::UnderscoreVariableUsed(var_name.to_string()).in_span(span),
                ));
                self.reported_vars.push(var_name);
            }
        }
    }
}
