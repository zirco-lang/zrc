//! `empty_struct_used`: Empty struct used where `void` would be more
//! appropriate
//!
//! This lint checks for instances where an empty struct is used in places
//! where a `void` type would be more suitable, such as function return types
//! or variable declarations. Using `void` in these contexts can improve code
//! clarity and intent.

use zrc_diagnostics::diagnostic::GenericLabel;
use zrc_parser::ast::{
    stmt::Declaration,
    ty::{Type, TypeKind},
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    diagnostic::{LintDiagnostic, LintDiagnosticKind, LintHelpKind, LintLabelKind},
    lint::Lint,
    visit::SyntacticVisit,
};

/// `empty_struct_used`: Empty struct used where `void` would be more
/// appropriate
pub struct EmptyStructUsedLint;
impl EmptyStructUsedLint {
    /// Initialize this lint
    pub fn init() -> Box<dyn Lint> {
        Box::new(Self)
    }
}
impl Lint for EmptyStructUsedLint {
    fn lint_ast(&self, program: Vec<Spanned<Declaration<'_>>>) -> Vec<LintDiagnostic> {
        let mut vis = Visit {
            diagnostics: vec![],
        };
        vis.visit_program(&program);

        vis.diagnostics
    }
}

/// AST visitor for the `empty_struct_used` lint
struct Visit {
    /// The collected diagnostics
    diagnostics: Vec<LintDiagnostic>,
}
impl<'input> SyntacticVisit<'input> for Visit {
    fn visit_type(&mut self, ty: &Type<'input>) {
        self.walk_type(ty);

        let sp = ty.0.span();

        if let TypeKind::Struct(struct_type) = ty.0.value()
            && struct_type.0.value().is_empty()
        {
            self.diagnostics.push(
                LintDiagnostic::warning(LintDiagnosticKind::EmptyStructUsed.in_span(sp))
                    .with_label(GenericLabel::warning(
                        LintLabelKind::EmptyStructType.in_span(sp),
                    ))
                    .with_help(LintHelpKind::UseVoidType),
            );
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
        name: empty_struct_declaration,
        source: indoc!{"
            struct EmptyStruct {}
        "},
        diagnostics: vec![
            LintDiagnostic::warning(
                spanned_test!(0, LintDiagnosticKind::EmptyStructUsed, 21)
            ).with_label(
                GenericLabel::warning(
                    spanned_test!(0, LintLabelKind::EmptyStructType, 21)
                )
            ).with_help(
                LintHelpKind::UseVoidType
            ),
        ]
    }
}
