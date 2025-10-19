//! for blocks

mod block_return;
mod block_utils;

pub use block_return::{BlockReturnAbility, BlockReturnActuality};
pub use block_utils::{coerce_stmt_into_block, has_duplicates};
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::{
    expr::{Expr, ExprKind},
    stmt::{LetDeclaration, Stmt, StmtKind, SwitchCase, SwitchTrigger},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{declaration::process_let_declaration, expr::try_coerce_to, scope::Scope, type_expr};
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
    return_ability: BlockReturnAbility<'input>,
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
                                TypedStmt(TypedStmtKind::BreakStmt.in_span(stmt_span)),
                                BlockReturnActuality::NeverReturns,
                            ))),
                            StmtKind::BreakStmt => {
                                Err(DiagnosticKind::CannotUseBreakOutsideOfLoop.error_in(stmt_span))
                            }

                            StmtKind::ContinueStmt if can_use_break_continue => Ok(Some((
                                TypedStmt(TypedStmtKind::ContinueStmt.in_span(stmt_span)),
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

                                        // Try to coerce trigger to scrutinee type if they don't
                                        // match
                                        let trigger = if trigger.inferred_type == scrutinee_ty {
                                            trigger
                                        } else if trigger
                                            .inferred_type
                                            .can_implicitly_cast_to(&scrutinee_ty)
                                        {
                                            try_coerce_to(trigger, &scrutinee_ty)
                                        } else if scrutinee_ty
                                            .can_implicitly_cast_to(&trigger.inferred_type)
                                        {
                                            // This shouldn't happen often, but handle it for
                                            // consistency
                                            trigger
                                        } else {
                                            return Err(DiagnosticKind::ExpectedSameType(
                                                scrutinee_ty.to_string(),
                                                trigger.inferred_type.to_string(),
                                            )
                                            .error_in(trigger.kind.span()));
                                        };

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
                                        .in_span(stmt_span),
                                    ),
                                    BlockReturnActuality::join(
                                        BlockReturnActuality::join_iter(return_statuses),
                                        default_ra,
                                    ),
                                )))
                            }

                            StmtKind::Match { scrutinee, cases } => {
                                // The following code:
                                // fn takes_i32(x: i32);
                                // fn takes_i64(x: i64);
                                // enum SpecialInt { I32: i32, I64: i64 }
                                // fn gives_si() -> SpecialInt;
                                //
                                // fn main() {
                                //     let si: SpecialInt = gives_si();
                                //     match si {
                                //         I32: x => takes_i32(x);
                                //         I64: x => takes_i64(x);
                                //     }
                                // }

                                // Should desugar to:
                                // fn takes_i32(x: i32);
                                // fn takes_i64(x: i64);
                                // struct SpecialInt {
                                //     __discriminant__: usize,
                                //     __value__: union { I32: i32, I64: i64 }
                                // }
                                // fn gives_si() -> SpecialInt;
                                //
                                // fn main() {
                                //     let si: SpecialInt = gives_si();
                                //     switch (si.__discriminant__) {
                                //         0 /* I32 */ => { let x =
                                // si.__value__.I32; takes_i32(x); }
                                //         1 /* I64 */ => { let x =
                                // si.__value__.I64; takes_i64(x); }
                                //     }
                                // }

                                // Semantic invariants:
                                // * The scrutinee must be of an enum type
                                // * There must be exactly one case per variant of the enum
                                // * Each case introduces a new variable into scope with the type of
                                //   the variant

                                // There is no TAST Enum type, it is simply represented as a Struct
                                // with a hidden discriminant field
                                // and a union of all the variants

                                let t_scrutinee = type_expr(&scope, scrutinee.clone())?;
                                let scrutinee_ty = t_scrutinee.inferred_type.clone();
                                // Clone scrutinee for AST construction (never move original)
                                let scrutinee_ast = scrutinee.clone();

                                // * The scrutinee must be of an enum type
                                // (Bonus points: Extracts the internal `union` declaration for
                                // later use)
                                let enum_as_union_def = if let TastType::Struct(ref struct_def) =
                                    scrutinee_ty
                                    && struct_def.get("__discriminant__").is_some()
                                    && struct_def.get("__value__").is_some()
                                {
                                    if let TastType::Union(union_def) = struct_def
                                        .get("__value__")
                                        .expect("value should exist")
                                        .clone()
                                    {
                                        union_def
                                    } else {
                                        return Err(DiagnosticKind::MatchOnNonEnum(
                                            scrutinee_ty.to_string(),
                                        )
                                        .error_in(t_scrutinee.kind.span()));
                                    }
                                } else {
                                    return Err(DiagnosticKind::MatchOnNonEnum(
                                        scrutinee_ty.to_string(),
                                    )
                                    .error_in(t_scrutinee.kind.span()));
                                };

                                // * There must be exactly one case per variant of the enum
                                if enum_as_union_def.len() != cases.len() {
                                    return Err(
                                        DiagnosticKind::MatchCaseCountMismatch.error_in(stmt_span)
                                    );
                                }

                                // Ensure each enum variant is covered by exactly one case
                                // We do this by sorting both lists and comparing them
                                #[allow(clippy::useless_asref)]
                                let mut sorted_enum_variants: Vec<
                                    (&str, &TastType<'_>),
                                > = enum_as_union_def
                                    .iter()
                                    .map(|(name, ty)| (name.as_ref(), ty))
                                    .collect::<Vec<_>>();

                                let mut sorted_case_variants = cases
                                    .iter()
                                    .map(|case| case.value().variant)
                                    .collect::<Vec<_>>();

                                sorted_enum_variants.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
                                sorted_case_variants.sort_unstable();

                                if sorted_enum_variants
                                    .iter()
                                    .map(|(name, _)| *name)
                                    .ne(sorted_case_variants.iter().copied())
                                {
                                    return Err(
                                        DiagnosticKind::NonExhaustiveMatchCases.error_in(stmt_span)
                                    );
                                }

                                // The index into sorted_enum_variants is the discriminant value
                                // We sorted both, so the indices line up

                                // Generate the literal AST for the desugared version

                                // Desugar match into switch on discriminant (AST only)

                                // Build the scrutinee for switch: <scrutinee>.__discriminant__ (AST
                                // Expr)
                                let discrim_access = Expr(Spanned::from_span_and_value(
                                    t_scrutinee.kind.span(),
                                    ExprKind::Dot(
                                        Box::new(scrutinee_ast.clone()),
                                        Spanned::from_span_and_value(
                                            t_scrutinee.kind.span(),
                                            "__discriminant__",
                                        ),
                                    ),
                                ));

                                // Build switch cases for each variant
                                let mut switch_cases = Vec::new();

                                for (idx, case) in cases.iter().enumerate() {
                                    let variant_name = case.value().variant;
                                    let var_binding = case.value().var;
                                    let body = case.value().body.clone();
                                    let case_span = case.span();

                                    // Build let binding: let <var_binding> =
                                    // <scrutinee>.__value__.<variant_name>;
                                    let value_access = Expr(Spanned::from_span_and_value(
                                        t_scrutinee.kind.span(),
                                        ExprKind::Dot(
                                            Box::new(Expr(Spanned::from_span_and_value(
                                                t_scrutinee.kind.span(),
                                                ExprKind::Dot(
                                                    Box::new(scrutinee_ast.clone()),
                                                    Spanned::from_span_and_value(
                                                        t_scrutinee.kind.span(),
                                                        "__value__",
                                                    ),
                                                ),
                                            ))),
                                            Spanned::from_span_and_value(
                                                t_scrutinee.kind.span(),
                                                variant_name,
                                            ),
                                        ),
                                    ));
                                    let let_decl = LetDeclaration {
                                        name: Spanned::from_span_and_value(case_span, var_binding),
                                        ty: None,
                                        value: Some(value_access),
                                    };
                                    let let_stmt = Stmt(Spanned::from_span_and_value(
                                        case_span,
                                        StmtKind::DeclarationList(Spanned::from_span_and_value(
                                            case_span,
                                            vec![Spanned::from_span_and_value(case_span, let_decl)],
                                        )),
                                    ));

                                    // Build block: { let <var_binding> = ...; <body> }
                                    let block_stmts = vec![let_stmt, body.clone()];
                                    let block = Stmt(Spanned::from_span_and_value(
                                        case_span,
                                        StmtKind::BlockStmt(block_stmts),
                                    ));

                                    // Switch trigger: discriminant value
                                    let trigger_expr = Expr(Spanned::from_span_and_value(
                                        case_span,
                                        ExprKind::NumberLiteral(
                                            // SAFETY: We leak this string because the AST
                                            // requires a &str for number literals and we need
                                            // it to live long enough
                                            zrc_parser::lexer::NumberLiteral::Decimal(Box::leak(
                                                Box::new(idx.to_string()),
                                            )),
                                            None,
                                        ),
                                    ));
                                    let trigger = SwitchTrigger::Expr(trigger_expr);

                                    switch_cases.push(Spanned::from_span_and_value(
                                        case_span,
                                        SwitchCase(trigger, block),
                                    ));
                                }

                                switch_cases.push(Spanned::from_span_and_value(
                                    stmt_span,
                                    SwitchCase(
                                        SwitchTrigger::Default,
                                        Stmt(StmtKind::UnreachableStmt.in_span(stmt_span)),
                                    ),
                                ));

                                // Build the switch statement AST
                                let switch_stmt = Stmt(Spanned::from_span_and_value(
                                    stmt_span,
                                    StmtKind::SwitchCase {
                                        scrutinee: discrim_access,
                                        cases: switch_cases,
                                    },
                                ));

                                // Recursively type check the desugared switch statement
                                let (typed_switch, switch_return_actuality) = type_block(
                                    &scope,
                                    Spanned::from_span_and_value(
                                        stmt_span,
                                        vec![switch_stmt.clone()],
                                    ),
                                    can_use_break_continue,
                                    return_ability.clone(),
                                )?;

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::BlockStmt(typed_switch).in_span(stmt_span),
                                    ),
                                    switch_return_actuality,
                                )))
                            }

                            StmtKind::UnreachableStmt => Ok(Some((
                                TypedStmt(TypedStmtKind::UnreachableStmt.in_span(stmt_span)),
                                // this may create some weird UB if used incorrectly, but it's on
                                // the user to ensure they don't do that
                                BlockReturnActuality::AlwaysReturns,
                            ))),

                            StmtKind::DeclarationList(declarations) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::DeclarationList(process_let_declaration(
                                        &mut scope,
                                        declarations.clone().into_value(),
                                    )?)
                                    .in_span(stmt_span),
                                ),
                                BlockReturnActuality::NeverReturns, /* because expressions
                                                                     * can't
                                                                     * return */
                            ))),

                            StmtKind::InlineAsm {
                                template,
                                outputs,
                                inputs,
                                clobbers,
                            } => {
                                use zrc_parser::ast::stmt::AsmOperand;

                                use crate::tast::stmt::TypedAsmOperand;

                                // Type check the template (must be a string literal)
                                let typed_template = type_expr(&scope, template)?;

                                // Type check output operands
                                let typed_outputs = outputs
                                    .into_iter()
                                    .map(|AsmOperand { constraint, expr }| {
                                        Ok(TypedAsmOperand {
                                            constraint: type_expr(&scope, constraint)?,
                                            expr: type_expr(&scope, expr)?,
                                        })
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;

                                // Type check input operands
                                let typed_inputs = inputs
                                    .into_iter()
                                    .map(|AsmOperand { constraint, expr }| {
                                        Ok(TypedAsmOperand {
                                            constraint: type_expr(&scope, constraint)?,
                                            expr: type_expr(&scope, expr)?,
                                        })
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;

                                // Type check clobbers (should be string literals)
                                let typed_clobbers = clobbers
                                    .into_iter()
                                    .map(|expr| type_expr(&scope, expr))
                                    .collect::<Result<Vec<_>, _>>()?;

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::InlineAsm {
                                            template: typed_template,
                                            outputs: typed_outputs,
                                            inputs: typed_inputs,
                                            clobbers: typed_clobbers,
                                        }
                                        .in_span(stmt_span),
                                    ),
                                    BlockReturnActuality::NeverReturns,
                                )))
                            }

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
                                            typed_then.in_span(then_span),
                                            typed_then_else.map(|x| {
                                                x.in_span(
                                                    te_span.expect(
                                                        "should have been unwrapped already",
                                                    ),
                                                )
                                            }),
                                        )
                                        .in_span(stmt_span),
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
                                            typed_body.in_span(body_span),
                                        )
                                        .in_span(stmt_span),
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
                                            typed_body.in_span(body_span),
                                            typed_cond,
                                        )
                                        .in_span(stmt_span),
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
                                            .containing(DiagnosticKind::ExpectedGot {
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
                                            body: typed_body.in_span(body_as_block_span),
                                        }
                                        .in_span(stmt_span),
                                    ),
                                    body_return_actuality.demote(),
                                )))
                            }

                            StmtKind::BlockStmt(body) => {
                                let (typed_body, return_actuality) = type_block(
                                    &scope,
                                    body.in_span(stmt_span),
                                    can_use_break_continue,
                                    return_ability.clone().demote(),
                                )?;
                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::BlockStmt(typed_body).in_span(stmt_span),
                                    ),
                                    return_actuality,
                                )))
                            }

                            StmtKind::ExprStmt(expr) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::ExprStmt(type_expr(&scope, expr)?)
                                        .in_span(stmt_span),
                                ),
                                BlockReturnActuality::NeverReturns,
                            ))),
                            StmtKind::ReturnStmt(value) => {
                                let resolved_value =
                                    value.map(|expr| type_expr(&scope, expr)).transpose()?;

                                let inferred_return_type = resolved_value
                                    .clone()
                                    .map_or_else(TastType::unit, |x| x.inferred_type);

                                match (resolved_value, &return_ability) {
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
                                        let coerced_value = if inferred_return_type == *return_ty {
                                            return_value
                                        } else if inferred_return_type
                                            .can_implicitly_cast_to(return_ty)
                                        {
                                            // Try to coerce the return value to the expected type
                                            return_value.map(|val| try_coerce_to(val, return_ty))
                                        } else {
                                            return Err(Diagnostic(
                                                Severity::Error,
                                                stmt_span.containing(DiagnosticKind::ExpectedGot {
                                                    expected: return_ty.to_string(),
                                                    got: inferred_return_type.to_string(),
                                                }),
                                            ));
                                        };

                                        Ok(Some((
                                            TypedStmt(
                                                TypedStmtKind::ReturnStmt(coerced_value)
                                                    .in_span(stmt_span),
                                            ),
                                            BlockReturnActuality::AlwaysReturns,
                                        )))
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
            tast_block.push(TypedStmt(TypedStmtKind::ReturnStmt(None).in_span(
                Span::from_positions_and_file(
                    input_block_span.end() - 1,
                    input_block_span.end(),
                    input_block_span.file_name(),
                ),
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
    use zrc_utils::spanned_test;

    use super::*;
    use crate::typeck::scope::GlobalScope;

    #[test]
    fn regression_297_switch_arm_types() {
        let gs = GlobalScope::default();

        let source = "switch (1 as i32) { 3 as i8 => {} default => {} }";

        let block_ast =
            zrc_parser::parser::parse_stmt_list(source, "<test>").expect("should parse");

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
                spanned_test!(
                    20,
                    DiagnosticKind::ExpectedSameType("i32".to_string(), "i8".to_string()),
                    27
                )
            )
        );
    }
}
