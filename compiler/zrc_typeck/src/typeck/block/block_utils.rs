//! Utilities for managing blocks

use zrc_parser::ast::stmt::{Stmt, StmtKind};
use zrc_utils::span::{Spannable, Spanned};

/// Convert a single [AST statement](Stmt) like `x;` to a block statement `{ x;
/// }` without converting `{ x; }` to `{ { x; } }`. This is preferred instead of
/// `vec![x]` as it prevents extra nesting layers.
#[must_use]
pub fn coerce_stmt_into_block(stmt: Stmt<'_>) -> Spanned<Vec<Stmt<'_>>> {
    let span = stmt.0.span();

    #[allow(clippy::wildcard_enum_match_arm)]
    stmt.0.map(|value| match value {
        StmtKind::BlockStmt(stmts) => stmts,
        stmt_kind => vec![Stmt(stmt_kind.in_span(span))],
    })
}

/// Returns whether there are duplicates in a slice.
pub fn has_duplicates<T>(slice: &[T]) -> bool
where
    T: PartialEq,
{
    (1..slice.len()).any(|i| slice[i..].contains(&slice[i - 1]))
}
