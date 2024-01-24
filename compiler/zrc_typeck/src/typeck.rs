//! Zirco programming language type checker

mod block;
mod expr;
mod scope;
mod ty;

pub use block::{
    process_declaration, type_block, BlockReturnAbility, BlockReturnActuality, BlockReturnType,
};
pub use expr::type_expr;
pub use ty::resolve_type;
use zrc_parser::ast::stmt::Declaration as AstDeclaration;
use zrc_utils::span::Spanned;

use crate::tast::stmt::TypedDeclaration;

/// # Errors
/// Errors with type checker errors.
pub fn type_program(
    program: Vec<Spanned<AstDeclaration>>,
) -> Result<Vec<Spanned<TypedDeclaration>>, zrc_diagnostics::Diagnostic> {
    let mut global_scope = scope::GlobalScope::new();

    program
        .into_iter()
        .filter_map(|declaration| {
            declaration
                .map(|declaration| process_declaration(&mut global_scope, declaration).transpose())
                .transpose()
                .map(zrc_utils::span::Spanned::<Result<_, _>>::transpose)
        })
        // drop the redundant/erroneous error spans
        .map(|x| x.map_err(zrc_utils::span::Spanned::into_value))
        .collect()
}
