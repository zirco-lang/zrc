//! Code generation for switch statements

use inkwell::{basic_block::BasicBlock, debug_info::DILexicalBlock};
use zrc_typeck::{tast::expr::TypedExpr, typeck::BlockMetadata};
use zrc_utils::span::{Span, Spannable};

use crate::{
    bb::BasicBlockAnd,
    ctx::{BlockCtx, FunctionCtx},
    expr::cg_expr,
    scope::CgScope,
    stmt::{LoopBreakaway, cg_block},
    unpack,
};

/// Code generates a switch statement
#[expect(clippy::too_many_arguments, clippy::ref_option)]
pub fn cg_switch_stmt<'ctx, 'input, 'a>(
    cg: FunctionCtx<'ctx, 'a>,
    mut bb: BasicBlock<'ctx>,
    scope: &'a CgScope<'input, 'ctx>,
    lexical_block: Option<DILexicalBlock<'ctx>>,
    breakaway: &Option<LoopBreakaway<'ctx>>,
    stmt_span: Span,
    scrutinee: TypedExpr<'input>,
    default: BlockMetadata<'input>,
    cases: Vec<(TypedExpr<'input>, BlockMetadata<'input>)>,
) -> BasicBlock<'ctx> {
    let expr_cg = BlockCtx::new(cg, scope, lexical_block);

    let scrutinee = unpack!(bb = cg_expr(expr_cg, bb, scrutinee));

    let default_bb = cg.ctx.append_basic_block(cg.fn_value, "default");
    let return_bb = cg.ctx.append_basic_block(cg.fn_value, "post");

    let cases: Vec<_> = cases
        .into_iter()
        .map(|(trigger, stmt)| {
            (
                cg.ctx.append_basic_block(cg.fn_value, "case"),
                unpack!(bb = cg_expr(expr_cg, bb, trigger)),
                stmt,
            )
        })
        .collect();

    cg.builder
        .build_switch(
            scrutinee.into_int_value(),
            default_bb,
            &cases
                .iter()
                .map(|(bb, val, _)| (val.into_int_value(), *bb))
                .collect::<Vec<_>>(),
        )
        .expect("switch should generate successfully");

    cg.builder.position_at_end(default_bb);
    let default_bb = cg_block(
        cg,
        default_bb,
        scope,
        lexical_block,
        default.in_span(stmt_span),
        breakaway,
    );
    if default_bb.is_some() {
        cg.builder
            .build_unconditional_branch(return_bb)
            .expect("br should generate successfully");
    }

    for (case_bb, _, stmt) in cases {
        cg.builder.position_at_end(case_bb);
        let case_bb = cg_block(
            cg,
            case_bb,
            scope,
            lexical_block,
            stmt.in_span(stmt_span),
            breakaway,
        );

        if case_bb.is_some() {
            cg.builder
                .build_unconditional_branch(return_bb)
                .expect("br should generate successfully");
        }
    }

    cg.builder.position_at_end(return_bb);

    return_bb
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn enum_match_generates_as_expected() {
        cg_snapshot_test!(indoc! {"
            enum VarInt {
                I32: i32,
                I64: i64,
            }

            fn f() -> VarInt;
            fn fi32(x: i32);
            fn fi64(x: i64);

            fn main() -> i32 {
                let vi = f();

                match (vi) {
                    I32: x => fi32(x);
                    I64: y => fi64(y);
                }

                return 0;
            }
        "});
    }
}
