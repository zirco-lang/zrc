//! Type checking for switch case and match statements.

use std::collections::HashMap;

use zrc_diagnostics::{Diagnostic, DiagnosticKind, LabelKind, diagnostic::GenericLabel};
use zrc_parser::ast::{
    expr::{Expr, ExprKind},
    stmt::{LetDeclaration, MatchCase, Stmt, StmtKind, SwitchCase, SwitchTrigger},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::{expr::try_coerce_to, scope::Scope, type_expr},
    block_utils::{coerce_stmt_into_block, has_duplicates},
    cfa::{BlockReturnAbility, BlockReturnActuality},
    type_block,
};
use crate::{
    tast::{
        expr::TypedExpr,
        stmt::{TypedStmt, TypedStmtKind},
        ty::Type as TastType,
    },
    typeck::block::BlockMetadata,
};

/// Type check a switch case statement.
#[expect(clippy::ptr_arg)]
pub fn type_switch_case<'input>(
    scope: &mut Scope<'input>,
    scrutinee: Expr<'input>,
    cases: &Vec<Spanned<SwitchCase<'input>>>,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input>, BlockReturnActuality)>, Diagnostic> {
    let mut cases = cases.clone();
    let scrutinee = type_expr(scope, scrutinee)?;
    let scrutinee_ty = scrutinee.inferred_type.clone();

    // The last entry in over MUST be the default case
    let maybe_default_case = cases.pop();
    let Some(maybe_default_case) = maybe_default_case else {
        return Err(DiagnosticKind::SwitchCaseMissingTerminalDefault
            .error_in(stmt_span)
            .with_label(GenericLabel::error(
                LabelKind::SwitchCaseMissingTerminalDefault.in_span(stmt_span),
            )));
    };

    let SwitchCase(SwitchTrigger::Default, default_stmt) = maybe_default_case.value() else {
        return Err(DiagnosticKind::SwitchCaseMissingTerminalDefault
            .error_in(stmt_span)
            .with_label(GenericLabel::error(
                LabelKind::SwitchCaseMissingTerminalDefault.in_span(maybe_default_case.span()),
            )));
    };

    let default_block = type_block(
        scope,
        coerce_stmt_into_block(default_stmt.clone()),
        false,
        return_ability.clone().demote(),
    )?;

    let default_ra = default_block.return_actuality;

    if has_duplicates(
        &(cases
            .clone()
            .into_iter()
            .map(move |x| x.into_value().0)
            .collect::<Vec<_>>()),
    ) {
        return Err(DiagnosticKind::MultipleCases
            .error_in(stmt_span)
            .with_label(GenericLabel::error(
                LabelKind::MultipleCases.in_span(stmt_span),
            )));
    }

    let cases = cases
        .into_iter()
        .map(|case| {
            let SwitchCase(trigger, exec) = case.into_value();

            let trigger = type_expr(
                scope,
                trigger
                    .into_expr_value()
                    .expect("default was already popped/de-duped"),
            )?;

            // Try to coerce trigger to scrutinee type if they don't
            // match
            let trigger = if trigger.inferred_type == scrutinee_ty {
                trigger
            } else if trigger.inferred_type.can_implicitly_cast_to(&scrutinee_ty) {
                try_coerce_to(trigger, &scrutinee_ty)
            } else if scrutinee_ty.can_implicitly_cast_to(&trigger.inferred_type) {
                // This shouldn't happen often, but handle it for
                // consistency
                trigger
            } else {
                return Err(DiagnosticKind::ExpectedSameType(
                    scrutinee_ty.to_string(),
                    trigger.inferred_type.to_string(),
                )
                .error_in(trigger.kind.span())
                .with_label(GenericLabel::error(
                    LabelKind::ExpectedSameType(
                        scrutinee_ty.to_string(),
                        trigger.inferred_type.to_string(),
                    )
                    .in_span(trigger.kind.span()),
                )));
            };

            let exec_block = type_block(
                scope,
                coerce_stmt_into_block(exec),
                false,
                return_ability.clone().demote(),
            )?;
            let return_status = exec_block.return_actuality;

            Ok::<((TypedExpr<'input>, BlockMetadata<'_>), BlockReturnActuality), Diagnostic>((
                (trigger, exec_block),
                return_status,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let (cases, return_statuses): (Vec<_>, Vec<_>) = cases.into_iter().unzip();

    let return_actuality =
        BlockReturnActuality::join(BlockReturnActuality::join_iter(return_statuses), default_ra);

    Ok(Some((
        TypedStmt {
            kind: (TypedStmtKind::SwitchCase {
                scrutinee,
                default: default_block,
                cases,
            })
            .in_span(stmt_span),
            return_actuality,
        },
        return_actuality,
    )))
}

/// Desugar and type check a match statement.
#[expect(clippy::too_many_lines, clippy::needless_pass_by_value)]
pub fn type_match<'input>(
    scope: &mut Scope<'input>,
    scrutinee: Expr<'input>,
    cases: Vec<Spanned<MatchCase<'input>>>,
    can_use_break_continue: bool,
    return_ability: &BlockReturnAbility<'input>,
    stmt_span: Span,
) -> Result<Option<(TypedStmt<'input>, BlockReturnActuality)>, Diagnostic> {
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
    // * Each case introduces a new variable into scope with the type of the variant

    // There is no TAST Enum type, it is simply represented as a Struct
    // with a hidden discriminant field
    // and a union of all the variants

    let t_scrutinee = type_expr(scope, scrutinee.clone())?;
    let scrutinee_ty = t_scrutinee.inferred_type.clone();
    // Clone scrutinee for AST construction (never move original)
    let scrutinee_ast = scrutinee.clone();

    // * The scrutinee must be of an enum type
    // (Bonus points: Extracts the internal `union` declaration for
    // later use)
    let enum_as_union_def = if let TastType::Struct(ref struct_def) = scrutinee_ty
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
            return Err(DiagnosticKind::MatchOnNonEnum(scrutinee_ty.to_string())
                .error_in(t_scrutinee.kind.span())
                .with_label(GenericLabel::error(
                    LabelKind::MatchOnNonEnum(scrutinee_ty.to_string())
                        .in_span(t_scrutinee.kind.span()),
                )));
        }
    } else {
        return Err(DiagnosticKind::MatchOnNonEnum(scrutinee_ty.to_string())
            .error_in(t_scrutinee.kind.span())
            .with_label(GenericLabel::error(
                LabelKind::MatchOnNonEnum(scrutinee_ty.to_string())
                    .in_span(t_scrutinee.kind.span()),
            )));
    };

    // * There must be exactly one case per variant of the enum
    if enum_as_union_def.len() != cases.len() {
        return Err(DiagnosticKind::MatchCaseCountMismatch
            .error_in(stmt_span)
            .with_label(GenericLabel::error(
                LabelKind::MatchCaseCountMismatch.in_span(stmt_span),
            )));
    }

    // Ensure each enum variant is covered by exactly one case
    // We do this by sorting both lists and comparing them
    #[expect(clippy::useless_asref)]
    let mut sorted_enum_variants: Vec<(&str, &TastType<'_>)> = enum_as_union_def
        .iter()
        .map(|(name, ty)| (name.as_ref(), ty))
        .collect::<Vec<_>>();

    let mut sorted_case_variants = cases
        .iter()
        .map(|case| case.value().variant)
        .collect::<Vec<_>>();

    sorted_enum_variants.sort_unstable_by_key(|(a, _)| *a);
    sorted_case_variants.sort_unstable();

    if sorted_enum_variants
        .iter()
        .map(|(name, _)| *name)
        .ne(sorted_case_variants.iter().copied())
    {
        return Err(DiagnosticKind::NonExhaustiveMatchCases
            .error_in(stmt_span)
            .with_label(GenericLabel::error(
                LabelKind::NonExhaustiveMatchCases.in_span(stmt_span),
            )));
    }

    // Create discriminant mapping using ALPHABETICAL ORDER
    // Both enum construction and match must use the same alphabetically sorted
    // order
    let variant_to_discriminant: HashMap<&str, usize> = sorted_enum_variants
        .iter()
        .enumerate()
        .map(|(idx, (name, _))| (*name, idx))
        .collect();

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
            Spanned::from_span_and_value(t_scrutinee.kind.span(), "__discriminant__"),
        ),
    ));

    // Build switch cases for each variant
    let mut switch_cases = Vec::new();

    for case in &cases {
        let variant_name = case.value().variant;
        let var_binding = case.value().var;
        let body = case.value().body.clone();
        let case_span = case.span();

        let discriminant_idx = *variant_to_discriminant
            .get(&variant_name)
            .expect("variant should be present in discriminant map");

        // Build let binding: let <var_binding> =
        // <scrutinee>.__value__.<variant_name>;
        let value_access = Expr(Spanned::from_span_and_value(
            t_scrutinee.kind.span(),
            ExprKind::Dot(
                Box::new(Expr(Spanned::from_span_and_value(
                    t_scrutinee.kind.span(),
                    ExprKind::Dot(
                        Box::new(scrutinee_ast.clone()),
                        Spanned::from_span_and_value(t_scrutinee.kind.span(), "__value__"),
                    ),
                ))),
                Spanned::from_span_and_value(t_scrutinee.kind.span(), variant_name),
            ),
        ));
        let let_decl = LetDeclaration {
            name: Spanned::from_span_and_value(case_span, var_binding),
            ty: None,
            value: Some(value_access),
            is_constant: false,
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
                zrc_parser::lexer::NumberLiteral::Decimal(Box::leak(Box::new(
                    discriminant_idx.to_string(),
                ))),
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
    let typed_switch_block = type_block(
        scope,
        Spanned::from_span_and_value(stmt_span, vec![switch_stmt.clone()]),
        can_use_break_continue,
        return_ability.clone().demote(),
    )?;

    let switch_return_actuality = typed_switch_block.return_actuality;

    Ok(Some((
        TypedStmt {
            kind: TypedStmtKind::BlockStmt(typed_switch_block).in_span(stmt_span),
            return_actuality: switch_return_actuality,
        },
        switch_return_actuality,
    )))
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
            Diagnostic::error(spanned_test!(
                20,
                DiagnosticKind::ExpectedSameType("i32".to_string(), "i8".to_string()),
                27
            ))
            .with_label(GenericLabel::error(spanned_test!(
                20,
                LabelKind::ExpectedSameType("i32".to_string(), "i8".to_string()),
                27
            )))
        );
    }
}
