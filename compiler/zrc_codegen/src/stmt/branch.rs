//! Code generation for switch statements

use inkwell::{
    basic_block::BasicBlock,
    debug_info::{AsDIScope, DILexicalBlock},
};
use zrc_typeck::{
    tast::expr::TypedExpr,
    typeck::{BlockMetadata, BlockReturnActuality},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{
    ctx::{BlockCtx, FunctionCtx},
    expr::cg_expr,
    scope::CgScope,
    stmt::{LoopBreakaway, cg_block},
};

/// Code generates a switch statement
#[expect(clippy::too_many_arguments, clippy::ref_option)]
pub fn cg_if_stmt<'ctx, 'input, 'a>(
    cg: FunctionCtx<'ctx, 'a>,
    bb: BasicBlock<'ctx>,
    scope: &'a CgScope<'input, 'ctx>,
    lexical_block: Option<DILexicalBlock<'ctx>>,
    breakaway: &Option<LoopBreakaway<'ctx>>,
    cond: TypedExpr<'input>,
    then: Spanned<BlockMetadata<'input>>,
    then_else: Option<Spanned<BlockMetadata<'input>>>,
) -> Option<BasicBlock<'ctx>> {
    let expr_cg = BlockCtx::new(cg, scope, lexical_block);

    let then_else = then_else.unwrap_or_else(|| {
        // Create an empty `BlockMetadata` that uses the `then` block's scope
        // so it has a valid `Scope` reference for the lifetime `'gs`.
        let empty = BlockMetadata {
            stmts: vec![],
            scope: then.value().scope.clone(),
            return_actuality: BlockReturnActuality::NeverReturns,
        };

        empty.in_span(Span::from_positions_and_file(
            then.end(),
            then.end(),
            then.span().file_name(),
        ))
    });

    let then_end = then.end();
    let then_else_end = then_else.end();

    let cond = cg_expr(expr_cg, bb, cond).into_value();

    let then_bb = cg.ctx.append_basic_block(cg.fn_value, "then");
    let then_else_bb = cg.ctx.append_basic_block(cg.fn_value, "then_else");

    cg.builder
        .build_conditional_branch(cond.into_int_value(), then_bb, then_else_bb)
        .expect("conditional branch should generate successfully");

    cg.builder.position_at_end(then_bb);
    let maybe_then_bb = cg_block(cg, then_bb, scope, lexical_block, then, breakaway);

    cg.builder.position_at_end(then_else_bb);
    let maybe_then_else_bb = cg_block(cg, then_else_bb, scope, lexical_block, then_else, breakaway);

    match (maybe_then_bb, maybe_then_else_bb) {
        (None, None) => None,
        (Some(single_bb), None) | (None, Some(single_bb)) => {
            let end = cg.ctx.append_basic_block(cg.fn_value, "end");

            let then_end_line_col = cg.line_lookup.lookup_from_index(then_end);

            if let Some(dbg_builder) = &cg.dbg_builder {
                let terminating_debug_location = dbg_builder.create_debug_location(
                    cg.ctx,
                    then_end_line_col.line,
                    then_end_line_col.col,
                    lexical_block.expect("we have DI").as_debug_info_scope(),
                    None,
                );

                cg.builder
                    .set_current_debug_location(terminating_debug_location);
            }

            cg.builder.position_at_end(single_bb);
            cg.builder
                .build_unconditional_branch(end)
                .expect("branch should generate successfully");

            cg.builder.position_at_end(end);
            Some(end)
        }
        (Some(then_bb), Some(then_else_bb)) => {
            let end = cg.ctx.append_basic_block(cg.fn_value, "end");

            let then_end_line_col = cg.line_lookup.lookup_from_index(then_end);
            if let Some(dbg_builder) = &cg.dbg_builder {
                let then_terminating_debug_location = dbg_builder.create_debug_location(
                    cg.ctx,
                    then_end_line_col.line,
                    then_end_line_col.col,
                    lexical_block.expect("we have DI").as_debug_info_scope(),
                    None,
                );
                cg.builder
                    .set_current_debug_location(then_terminating_debug_location);
            }
            cg.builder.position_at_end(then_bb);
            cg.builder
                .build_unconditional_branch(end)
                .expect("branch should generate successfully");

            let then_else_end_line_col = cg.line_lookup.lookup_from_index(then_else_end);

            if let Some(dbg_builder) = &cg.dbg_builder {
                let then_else_terminating_debug_location = dbg_builder.create_debug_location(
                    cg.ctx,
                    then_else_end_line_col.line,
                    then_else_end_line_col.col,
                    lexical_block.expect("we have DI").as_debug_info_scope(),
                    None,
                );
                cg.builder
                    .set_current_debug_location(then_else_terminating_debug_location);
            }

            cg.builder.position_at_end(then_else_bb);
            cg.builder
                .build_unconditional_branch(end)
                .expect("branch should generate successfully");

            cg.builder.position_at_end(end);
            Some(end)
        }
    }
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn unreachable_statement_generates_properly() {
        cg_snapshot_test!(indoc! {"
            fn test(cond: bool) {
                let x = 7;
                if (x == 6) unreachable;
                else return;
            }
        "});
    }

    #[test]
    fn if_statements_generate_as_expected() {
        cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;
                    fn nop();

                    fn test() {
                        // TEST: properly produces a conditional break over the call result and
                        // both code paths join at the end
                        if (get_bool()) nop();

                        // TEST: code generation properly continues in the last block
                        nop();
                        return;
                    }
                "});
    }

    #[test]
    fn if_else_statements_generate_as_expected() {
        cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;
                    fn nop();

                    fn test() {
                        // TEST: properly produces a conditional break over the call result and
                        // both code baths call nop().
                        if (get_bool()) nop();
                        else {
                            nop();
                            // TEST: this path diverges
                            return;
                        }

                        // TEST: code generation properly continues from the if_true block
                        nop();
                    }
                "});
    }

    #[test]
    fn if_else_statements_where_both_blocks_terminate_do_not_continue_generating() {
        cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;

                    fn test() {
                        // TEST: properly produces a conditional break over the call result
                        // and both code paths return (diverge). there should be no %end bb.
                        if (get_bool()) return;
                        else return;
                    }
                "});
    }
}
