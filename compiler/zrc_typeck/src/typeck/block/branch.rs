//! Type checking for if statements.

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::{expr::Expr, stmt::Stmt};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::{scope::Scope, type_expr},
    block_utils::coerce_stmt_into_block,
    cfa::{BlockReturnAbility, BlockReturnActuality},
    type_block,
};
use crate::tast::{
    stmt::{TypedStmt, TypedStmtKind},
    ty::Type as TastType,
};

/// Type check an if statement.
#[expect(clippy::needless_pass_by_value)]
pub fn type_if<'input, 'gs>(
    scope: &mut Scope<'input, 'gs>,
    cond: Expr<'input>,
    then: Box<Stmt<'input>>,
    then_else: Option<Box<Stmt<'input>>>,
    can_use_break_continue: bool,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input, 'gs>, BlockReturnActuality)>, Diagnostic> {
    // TODO: if `cond` is always true at compile-time, we can prove the
    // if branch is always taken (hence
    // if it's WillReturn we can be WillReturn
    // instead of MayReturn)

    let cond_span = cond.0.span();
    let then_span = then.0.span();
    let te_span = then_else.as_ref().map(|x| x.0.span());

    let typed_cond = type_expr(scope, cond)?;

    if typed_cond.inferred_type != TastType::Bool {
        return Err(DiagnosticKind::ExpectedGot {
            expected: "bool".to_string(),
            got: typed_cond.inferred_type.to_string(),
        }
        .error_in(cond_span));
    }

    let typed_then = type_block(
        scope,
        coerce_stmt_into_block(*then),
        can_use_break_continue,
        return_ability.clone().demote(),
    )?;
    let then_act = typed_then.return_actuality;

    let typed_then_else = then_else
        .clone()
        .map(|then_else| {
            type_block(
                scope,
                coerce_stmt_into_block(*then_else),
                can_use_break_continue,
                return_ability.clone().demote(),
            )
        })
        .transpose()?;
    let te_act = typed_then_else
        .as_ref()
        .map_or(BlockReturnActuality::NeverReturns, |te| te.return_actuality);

    Ok(Some((
        TypedStmt(
            TypedStmtKind::IfStmt(
                typed_cond,
                typed_then.in_span(then_span),
                typed_then_else
                    .map(|x| x.in_span(te_span.expect("should have been unwrapped already"))),
            )
            .in_span(stmt_span),
        ),
        BlockReturnActuality::join(then_act, te_act),
    )))
}
