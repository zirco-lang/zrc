//! Type checking for if statements.

use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::{
    expr::Expr,
    stmt::{LetDeclaration, Stmt},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::{scope::Scope, type_expr},
    block_utils::coerce_stmt_into_block,
    cfa::{BlockReturnAbility, BlockReturnActuality},
    process_let_declaration, type_block,
};
use crate::tast::{
    stmt::{TypedStmt, TypedStmtKind},
    ty::Type as TastType,
};

/// Type check a for statement.
pub fn type_for<'input>(
    scope: &Scope<'input, '_>,
    init: Option<Box<Spanned<Vec<Spanned<LetDeclaration<'input>>>>>>,
    cond: Option<Expr<'input>>,
    post: Option<Expr<'input>>,
    body: Box<Stmt<'input>>,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input>, BlockReturnActuality)>, Diagnostic> {
    // TODO: same logic as the TODO comment on the while loop applies
    // here.

    // the declaration made in the for loop's init is scoped to *only*
    // the loop so we need to make a
    // subscope for it
    let mut loop_scope = scope.clone();

    // if present, evaluate the declaration
    let typed_init = init
        .map(|decl| process_let_declaration(&mut loop_scope, (*decl).into_value()))
        .transpose()?;

    let cond_span = cond.as_ref().map(|inner| inner.0.span());
    let typed_cond = cond.map(|cond| type_expr(&loop_scope, cond)).transpose()?;

    if let Some(inner_t_cond) = typed_cond.clone()
        && inner_t_cond.inferred_type != TastType::Bool
    {
        return Err(Diagnostic(
            Severity::Error,
            cond_span
                .expect("span should exist if we unwrapped it")
                .containing(DiagnosticKind::ExpectedGot {
                    expected: "bool".to_string(),
                    got: inner_t_cond.inferred_type.to_string(),
                }),
        ));
    }

    let typed_post = post.map(|post| type_expr(&loop_scope, post)).transpose()?;

    let body_as_block = coerce_stmt_into_block(*body);
    let body_as_block_span = body_as_block.span();
    let (typed_body, body_return_actuality) = type_block(
        &loop_scope,
        body_as_block,
        true,
        return_ability.clone().demote(),
    )?;

    Ok(Some((
        TypedStmt(
            TypedStmtKind::ForStmt {
                init: typed_init.map(Box::new),
                cond: typed_cond,
                post: typed_post,
                body: typed_body.in_span(body_as_block_span),
            }
            .in_span(stmt_span),
        ),
        body_return_actuality.demote(),
    )))
}

/// Type check a while statement.
pub fn type_while<'input>(
    scope: &Scope<'input, '_>,
    cond: Expr<'input>,
    body: Box<Stmt<'input>>,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input>, BlockReturnActuality)>, Diagnostic> {
    // TODO: we might be able to prove that the body runs at least once
    // or an infinite loop making this
    // won't/will return statically

    let cond_span = cond.0.span();
    let body_span = body.0.span();
    let typed_cond = type_expr(scope, cond)?;

    if typed_cond.inferred_type != TastType::Bool {
        return Err(DiagnosticKind::ExpectedGot {
            expected: "bool".to_string(),
            got: typed_cond.inferred_type.to_string(),
        }
        .error_in(cond_span));
    }

    let (typed_body, body_return_actuality) = type_block(
        scope,
        coerce_stmt_into_block(*body),
        true,
        return_ability.clone().demote(),
    )?;

    Ok(Some((
        TypedStmt(
            TypedStmtKind::WhileStmt(typed_cond, typed_body.in_span(body_span)).in_span(stmt_span),
        ),
        body_return_actuality.demote(),
    )))
}

/// Type check a do..while statement.
pub fn type_do_while<'input>(
    scope: &Scope<'input, '_>,
    body: Box<Stmt<'input>>,
    cond: Expr<'input>,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input>, BlockReturnActuality)>, Diagnostic> {
    let cond_span = cond.0.span();
    let body_span = body.0.span();
    let typed_cond = type_expr(scope, cond)?;

    if typed_cond.inferred_type != TastType::Bool {
        return Err(DiagnosticKind::ExpectedGot {
            expected: "bool".to_string(),
            got: typed_cond.inferred_type.to_string(),
        }
        .error_in(cond_span));
    }

    let (typed_body, body_return_actuality) = type_block(
        scope,
        coerce_stmt_into_block(*body),
        true,
        return_ability.clone().demote(),
    )?;

    Ok(Some((
        TypedStmt(
            TypedStmtKind::DoWhileStmt(typed_body.in_span(body_span), typed_cond)
                .in_span(stmt_span),
        ),
        // Unlike `while`, a `do..while` loop is guaranteed to run at
        // least once. For this reason, we do not need to `demote` it.
        body_return_actuality,
    )))
}
