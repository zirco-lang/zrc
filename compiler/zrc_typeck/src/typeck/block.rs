//! for blocks

mod block_return;
mod block_utils;

pub use block_return::{BlockReturnAbility, BlockReturnActuality};
pub use block_utils::{coerce_stmt_into_block, has_duplicates};
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::stmt::{Stmt, StmtKind, SwitchCase, SwitchTrigger};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{declaration::process_let_declaration, scope::Scope, type_expr};
use crate::tast::{
    expr::TypedExpr,
    stmt::{TypedStmt, TypedStmtKind},
    ty::Type as TastType,
};

/// Type check a block of [AST statement](Stmt)s and return a block of [TAST
/// statement](TypedStmt)s.
///
/// It performs a small desugaring where all statements become implicit blocks.
///
/// This function must be provided a block of statements, and a few bits of
/// information about the parent scope in form of booleans that toggle certain
/// statements like `break` and a [`BlockReturnAbility`].
///
/// # Behavior of block returns
/// In many cases, a block [MUST return](BlockReturnAbility::MustReturn). For
/// example, this is done in the main block of a function. When a function
/// contains sub-blocks, those blocks [*may*
/// return](BlockReturnAbility::MayReturn) but are not required to. However, at
/// least one of the blocks within must be guaranteed to return in order to
/// fulfill a MUST return, otherwise the function is not guaranteed to return.
/// So, if you pass this function a **may** return order, it will return a
/// [`BlockReturnActuality`] which can be used to determine if a MUST return is
/// fulfilled.
///
/// ```rs
/// { // This block must return.
///     { // This block MAY return.
///         if (x) return; // This MAY return.
///     } // This block WILL SOMETIMES return.
///     // Because the above block is not GUARANTEED to return, the "must
///     // return" is not yet satisfied.
/// }
/// ```
///
/// # Errors
/// Errors if a type checker error is encountered.
///
/// # Panics
/// Panics in some internal state failures.
// TODO: Maybe the TAST should attach the BlockReturnActuality in each BlockStmt itself and preserve
// it on sub-blocks in the TAST (this may be helpful in control flow analysis)
#[allow(clippy::too_many_lines)]
pub fn type_block<'input, 'gs>(
    parent_scope: &Scope<'input, 'gs>,
    input_block: Spanned<Vec<Stmt<'input>>>,
    can_use_break_continue: bool,
    return_ability: BlockReturnAbility,
) -> Result<(Vec<TypedStmt<'input>>, BlockReturnActuality), Diagnostic> {
    let mut scope: Scope<'input, 'gs> = parent_scope.clone();

    let input_block_span = input_block.span();

    // At first, the block does not return.
    let (mut tast_block, return_actualities): (Vec<_>, Vec<_>) = input_block
        .into_value()
        .into_iter()
        .filter_map(
            |stmt| -> Option<Result<(TypedStmt<'input>, BlockReturnActuality), Diagnostic>> {
                let stmt_span = stmt.0.span();
                let inner_closure =
                    || -> Result<Option<(TypedStmt<'_>, BlockReturnActuality)>, Diagnostic> {
                        match stmt.0.into_value() {
                            StmtKind::EmptyStmt => Ok(None),
                            StmtKind::BreakStmt if can_use_break_continue => Ok(Some((
                                TypedStmt(TypedStmtKind::BreakStmt.in_span_no_file(stmt_span)),
                                BlockReturnActuality::NeverReturns,
                            ))),
                            StmtKind::BreakStmt => {
                                Err(DiagnosticKind::CannotUseBreakOutsideOfLoop.error_in(stmt_span))
                            }

                            StmtKind::ContinueStmt if can_use_break_continue => Ok(Some((
                                TypedStmt(TypedStmtKind::ContinueStmt.in_span_no_file(stmt_span)),
                                BlockReturnActuality::NeverReturns,
                            ))),
                            StmtKind::ContinueStmt => {
                                Err(DiagnosticKind::CannotUseContinueOutsideOfLoop
                                    .error_in(stmt_span))
                            }

                            StmtKind::SwitchCase { scrutinee, cases } => {
                                let mut cases = cases.clone();
                                let scrutinee = type_expr(&scope, scrutinee)?;
                                let scrutinee_ty = scrutinee.inferred_type.clone();

                                // The last entry in over MUST be the default case
                                let maybe_default_case = cases.pop();
                                let Some(maybe_default_case) = maybe_default_case else {
                                    return Err(DiagnosticKind::SwitchCaseMissingTerminalDefault
                                        .error_in(stmt_span));
                                };

                                let SwitchCase(SwitchTrigger::Default, default_stmt) =
                                    maybe_default_case.value()
                                else {
                                    return Err(DiagnosticKind::SwitchCaseMissingTerminalDefault
                                        .error_in(stmt_span));
                                };

                                let (default_stmt, default_ra) = type_block(
                                    &scope,
                                    coerce_stmt_into_block(default_stmt.clone()),
                                    false,
                                    return_ability.clone().demote(),
                                )?;

                                if has_duplicates(
                                    &(cases
                                        .clone()
                                        .into_iter()
                                        .map(move |x| x.into_value().0)
                                        .collect::<Vec<_>>()),
                                ) {
                                    return Err(DiagnosticKind::MultipleCases.error_in(stmt_span));
                                }

                                let cases = cases
                                    .into_iter()
                                    .map(|case| {
                                        let SwitchCase(trigger, exec) = case.into_value();

                                        let trigger = type_expr(
                                            &scope,
                                            trigger
                                                .into_expr_value()
                                                .expect("default was already popped/de-duped"),
                                        )?;

                                        if trigger.inferred_type != scrutinee_ty {
                                            return Err(DiagnosticKind::ExpectedSameType(
                                                scrutinee_ty.to_string(),
                                                trigger.inferred_type.to_string(),
                                            )
                                            .error_in(trigger.kind.span()));
                                        }

                                        let (exec, return_status) = type_block(
                                            &scope,
                                            coerce_stmt_into_block(exec),
                                            false,
                                            return_ability.clone().demote(),
                                        )?;

                                        Ok::<
                                            (
                                                (TypedExpr<'input>, Vec<TypedStmt<'_>>),
                                                BlockReturnActuality,
                                            ),
                                            Diagnostic,
                                        >((
                                            (trigger, exec),
                                            return_status,
                                        ))
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;

                                let (cases, return_statuses): (Vec<_>, Vec<_>) =
                                    cases.into_iter().unzip();

                                Ok(Some((
                                    TypedStmt(
                                        (TypedStmtKind::SwitchCase {
                                            scrutinee,
                                            default: default_stmt,
                                            cases,
                                        })
                                        .in_span_no_file(stmt_span),
                                    ),
                                    BlockReturnActuality::join(
                                        BlockReturnActuality::join_iter(return_statuses),
                                        default_ra,
                                    ),
                                )))
                            }

                            StmtKind::DeclarationList(declarations) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::DeclarationList(process_let_declaration(
                                        &mut scope,
                                        declarations.clone().into_value(),
                                    )?)
                                    .in_span_no_file(stmt_span),
                                ),
                                BlockReturnActuality::NeverReturns, /* because expressions
                                                                     * can't
                                                                     * return */
                            ))),

                            StmtKind::IfStmt(cond, then, then_else) => {
                                // TODO: if `cond` is always true at compile-time, we can prove the
                                // if branch is always taken (hence
                                // if it's WillReturn we can be WillReturn
                                // instead of MayReturn)

                                let cond_span = cond.0.span();
                                let then_span = then.0.span();
                                let te_span = then_else.as_ref().map(|x| x.0.span());

                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.inferred_type.to_string(),
                                    }
                                    .error_in(cond_span));
                                }

                                let (typed_then, then_return_actuality) = type_block(
                                    &scope,
                                    coerce_stmt_into_block(*then),
                                    can_use_break_continue,
                                    return_ability.clone().demote(),
                                )?;

                                let (typed_then_else, then_else_return_actuality) = then_else
                                    .clone()
                                    .map(|then_else| {
                                        type_block(
                                            &scope,
                                            coerce_stmt_into_block(*then_else),
                                            can_use_break_continue,
                                            return_ability.clone().demote(),
                                        )
                                    })
                                    .transpose()?
                                    .unzip();

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::IfStmt(
                                            typed_cond,
                                            typed_then.in_span_no_file(then_span),
                                            typed_then_else.map(|x| {
                                                x.in_span_no_file(
                                                    te_span.expect(
                                                        "should have been unwrapped already",
                                                    ),
                                                )
                                            }),
                                        )
                                        .in_span_no_file(stmt_span),
                                    ),
                                    BlockReturnActuality::join(
                                        then_return_actuality,
                                        then_else_return_actuality
                                            .unwrap_or(BlockReturnActuality::NeverReturns),
                                    ),
                                )))
                            }
                            StmtKind::WhileStmt(cond, body) => {
                                // TODO: we might be able to prove that the body runs at least once
                                // or an infinite loop making this
                                // won't/will return statically

                                let cond_span = cond.0.span();
                                let body_span = body.0.span();
                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.inferred_type.to_string(),
                                    }
                                    .error_in(cond_span));
                                }

                                let (typed_body, body_return_actuality) = type_block(
                                    &scope,
                                    coerce_stmt_into_block(*body),
                                    true,
                                    return_ability.clone().demote(),
                                )?;

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::WhileStmt(
                                            typed_cond,
                                            typed_body.in_span_no_file(body_span),
                                        )
                                        .in_span_no_file(stmt_span),
                                    ),
                                    body_return_actuality.demote(),
                                )))
                            }
                            StmtKind::DoWhileStmt(body, cond) => {
                                let cond_span = cond.0.span();
                                let body_span = body.0.span();
                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.inferred_type.to_string(),
                                    }
                                    .error_in(cond_span));
                                }

                                let (typed_body, body_return_actuality) = type_block(
                                    &scope,
                                    coerce_stmt_into_block(*body),
                                    true,
                                    return_ability.clone().demote(),
                                )?;

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::DoWhileStmt(
                                            typed_body.in_span_no_file(body_span),
                                            typed_cond,
                                        )
                                        .in_span_no_file(stmt_span),
                                    ),
                                    // Unlike `while`, a `do..while` loop is guaranteed to run at
                                    // least once. For this reason, we do not need to `demote` it.
                                    body_return_actuality,
                                )))
                            }
                            StmtKind::ForStmt {
                                init,
                                cond,
                                post,
                                body,
                            } => {
                                // TODO: same logic as the TODO comment on the while loop applies
                                // here.

                                // the declaration made in the for loop's init is scoped to *only*
                                // the loop so we need to make a
                                // subscope for it
                                let mut loop_scope = scope.clone();

                                // if present, evaluate the declaration
                                let typed_init = init
                                    .map(|decl| {
                                        process_let_declaration(
                                            &mut loop_scope,
                                            (*decl).into_value(),
                                        )
                                    })
                                    .transpose()?;

                                let cond_span = cond.as_ref().map(|inner| inner.0.span());
                                let typed_cond =
                                    cond.map(|cond| type_expr(&loop_scope, cond)).transpose()?;

                                if let Some(inner_t_cond) = typed_cond.clone()
                                    && inner_t_cond.inferred_type != TastType::Bool
                                {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        cond_span
                                            .expect("span should exist if we unwrapped it")
                                            .containing_no_file(DiagnosticKind::ExpectedGot {
                                                expected: "bool".to_string(),
                                                got: inner_t_cond.inferred_type.to_string(),
                                            }),
                                    ));
                                }

                                let typed_post =
                                    post.map(|post| type_expr(&loop_scope, post)).transpose()?;

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
                                            body: typed_body.in_span_no_file(body_as_block_span),
                                        }
                                        .in_span_no_file(stmt_span),
                                    ),
                                    body_return_actuality.demote(),
                                )))
                            }

                            StmtKind::BlockStmt(body) => {
                                let (typed_body, return_actuality) = type_block(
                                    &scope,
                                    body.in_span_no_file(stmt_span),
                                    can_use_break_continue,
                                    return_ability.clone().demote(),
                                )?;
                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::BlockStmt(typed_body).in_span_no_file(stmt_span),
                                    ),
                                    return_actuality,
                                )))
                            }

                            StmtKind::ExprStmt(expr) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::ExprStmt(type_expr(&scope, expr)?)
                                        .in_span_no_file(stmt_span),
                                ),
                                BlockReturnActuality::NeverReturns,
                            ))),
                            StmtKind::ReturnStmt(value) => {
                                let resolved_value =
                                    value.map(|expr| type_expr(&scope, expr)).transpose()?;

                                let inferred_return_type = resolved_value
                                    .clone()
                                    .map_or_else(TastType::unit, |x| x.inferred_type);

                                match (resolved_value, return_ability.clone()) {
                                    // expects no return
                                    (_, BlockReturnAbility::MustNotReturn) => {
                                        Err(DiagnosticKind::CannotReturnHere.error_in(stmt_span))
                                    }

                                    // return x; in fn expecting to return x
                                    (
                                        return_value,
                                        BlockReturnAbility::MustReturn(return_ty)
                                        | BlockReturnAbility::MayReturn(return_ty),
                                    ) => {
                                        if inferred_return_type == return_ty {
                                            Ok(Some((
                                                TypedStmt(
                                                    TypedStmtKind::ReturnStmt(return_value)
                                                        .in_span_no_file(stmt_span),
                                                ),
                                                BlockReturnActuality::AlwaysReturns,
                                            )))
                                        } else {
                                            Err(Diagnostic(
                                                Severity::Error,
                                                stmt_span.containing_no_file(DiagnosticKind::ExpectedGot {
                                                    expected: return_ty.to_string(),
                                                    got: inferred_return_type.to_string(),
                                                }),
                                            ))
                                        }
                                    }
                                }
                            }
                        }
                    };

                inner_closure().transpose()
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()?
        .into_iter()
        .unzip();

    let might_return = return_actualities.iter().any(|x| {
        matches!(
            x,
            BlockReturnActuality::SometimesReturns | BlockReturnActuality::AlwaysReturns
        )
    });
    let will_return = return_actualities
        .iter()
        .any(|x| matches!(x, BlockReturnActuality::AlwaysReturns));

    let return_actuality = match (might_return, will_return) {
        (_, true) => BlockReturnActuality::AlwaysReturns,
        (true, false) => BlockReturnActuality::SometimesReturns,
        (false, false) => BlockReturnActuality::NeverReturns,
    };

    match (return_ability, return_actuality) {
        (
            BlockReturnAbility::MustNotReturn | BlockReturnAbility::MayReturn(_),
            BlockReturnActuality::NeverReturns,
        ) => Ok(BlockReturnActuality::NeverReturns),

        (BlockReturnAbility::MayReturn(_), BlockReturnActuality::SometimesReturns) => {
            Ok(BlockReturnActuality::SometimesReturns)
        }

        (
            BlockReturnAbility::MustReturn(_) | BlockReturnAbility::MayReturn(_),
            BlockReturnActuality::AlwaysReturns,
        ) => Ok(BlockReturnActuality::AlwaysReturns),

        // implicitly add a `return;`
        (
            BlockReturnAbility::MustReturn(return_ty),
            BlockReturnActuality::SometimesReturns | BlockReturnActuality::NeverReturns,
        ) if return_ty == TastType::unit() => {
            tast_block.push(TypedStmt(TypedStmtKind::ReturnStmt(None).in_span_no_file(
                Span::from_positions(input_block_span.end() - 1, input_block_span.end()),
            )));

            Ok(BlockReturnActuality::AlwaysReturns)
        }

        (
            BlockReturnAbility::MustReturn(_),
            BlockReturnActuality::SometimesReturns | BlockReturnActuality::NeverReturns,
        ) => Err(DiagnosticKind::ExpectedABlockToReturn.error_in(input_block_span)),

        (
            BlockReturnAbility::MustNotReturn,
            BlockReturnActuality::SometimesReturns | BlockReturnActuality::AlwaysReturns,
        ) => {
            panic!(concat!(
                "block must not return, but a sub-block may return",
                " -- this should have been caught when checking that block"
            ));
        }
    }
    .map(|actuality| (tast_block, actuality))
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;
    use crate::typeck::scope::GlobalScope;

    #[test]
    fn regression_297_switch_arm_types() {
        let gs = GlobalScope::default();

        let source = "switch (1 as i32) { 3 as i8 => {} default => {} }";

        let block_ast = zrc_parser::parser::parse_stmt_list(source, "test.zrc").expect("should parse");

        let tck_result = type_block(
            &gs.create_subscope(),
            block_ast,
            false,
            BlockReturnAbility::MustNotReturn,
        );

        let Err(diagnostic) = tck_result else {
            panic!("expected type checking to fail");
        };

        assert_eq!(
            diagnostic,
            Diagnostic(
                Severity::Error,
                spanned!(
                    20,
                    DiagnosticKind::ExpectedSameType("i32".to_string(), "i8".to_string()),
                    27
                )
            )
        );
    }
}
