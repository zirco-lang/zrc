//! Zirco programming language type checker

mod block;
mod declaration;
mod diagnostics;
mod expr;
mod scope;
mod ty;

pub use block::{
    BlockReturnAbility, BlockReturnActuality, coerce_stmt_into_block, has_duplicates, type_block,
};
pub use declaration::process_declaration;
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
                .map(Spanned::<Result<_, _>>::transpose)
        })
        // drop the redundant/erroneous error spans
        .map(|x| x.map_err(Spanned::into_value))
        .collect()
}
