//! Zirco programming language type checker

mod block;
mod declaration;
mod expr;
mod scope;
mod ty;

pub use block::{BlockReturnAbility, BlockReturnActuality, type_block};
pub use declaration::process_declaration;
pub use expr::type_expr;
pub use ty::resolve_type;

use crate::tast;

/// # Errors
/// Errors with type checker errors.
pub fn type_program(
    program: zrc_parser::ast::Program<'_>,
) -> Result<tast::Program<'_>, zrc_diagnostics::Diagnostic> {
    let mut global_scope = scope::GlobalScope::new();

    let typed_decls = program
        .0
        .into_iter()
        .filter_map(|declaration| {
            declaration
                .map(|declaration| process_declaration(&mut global_scope, declaration).transpose())
                .transpose()
                .map(zrc_utils::span::Spanned::<Result<_, _>>::transpose)
        })
        // drop the redundant/erroneous error spans
        .map(|x| x.map_err(zrc_utils::span::Spanned::into_value))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(tast::Program(typed_decls))
}
