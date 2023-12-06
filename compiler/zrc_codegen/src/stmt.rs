//! Code generation for statements

use inkwell::{
    basic_block::BasicBlock,
    debug_info::{AsDIScope, DILexicalBlock},
};
use zrc_typeck::tast::{
    expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
    stmt::{LetDeclaration, TypedStmt, TypedStmtKind},
};
use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{expr::cg_expr, ty::llvm_basic_type, BasicBlockAnd, CgContext, CgScope};

/// Consists of the [`BasicBlock`]s to `br` to when encountering certain
/// instructions. It is passed to [`cg_block`] to allow it to properly handle
/// break and continue.
#[derive(PartialEq, Eq, Debug, Clone)]
#[allow(clippy::redundant_pub_crate)]
pub(crate) struct LoopBreakaway<'ctx> {
    /// Points to the exit basic block.
    on_break: BasicBlock<'ctx>,
    /// For `for` loops, points to the latch. For `while` loops, points to the
    /// header.
    on_continue: BasicBlock<'ctx>,
}

/// Generates the `alloca`tion, `store` instruction, and adds a new identifier
/// to the [`CgScope`].
///
/// # Panics
/// Panics if an internal code generation error is encountered.
#[allow(clippy::too_many_arguments)]
fn cg_let_declaration<'ctx, 'input, 'a>(
    cg: CgContext<'ctx, 'a>,
    mut bb: BasicBlock<'ctx>,
    scope: &'a mut CgScope<'input, 'ctx>,
    dbg_scope: DILexicalBlock<'ctx>,
    declarations: Vec<LetDeclaration<'input>>,
) -> BasicBlock<'ctx> {
    for let_declaration in declarations {
        // we create our own builder here because we need to insert the alloca
        // at the beginning of the entry block, and that is easier than trying to
        // somehow save our position.

        let entry_block_builder = cg.ctx.create_builder();
        let first_bb = cg
            .fn_value
            .get_first_basic_block()
            .expect("function should have at least one basic block");
        #[allow(clippy::option_if_let_else)]
        match first_bb.get_first_instruction() {
            Some(first_instruction) => {
                entry_block_builder.position_before(&first_instruction);
            }
            None => {
                entry_block_builder.position_at_end(first_bb);
            }
        }

        let ptr = entry_block_builder
            .build_alloca(
                llvm_basic_type(
                    cg.compilation_unit.get_file(),
                    cg.dbg_builder,
                    cg.ctx,
                    cg.target_machine,
                    &let_declaration.ty,
                )
                .0,
                &format!("let_{}", let_declaration.name.value()),
            )
            .expect("alloca should generate successfully");

        scope.insert(let_declaration.name.value(), ptr);

        if let Some(value) = let_declaration.value {
            bb = cg_expr(
                cg,
                bb,
                scope,
                dbg_scope,
                TypedExpr {
                    inferred_type: let_declaration.ty.clone(),
                    kind: value.kind.span().containing(TypedExprKind::Assignment(
                        Box::new(Place {
                            inferred_type: let_declaration.ty,
                            kind: PlaceKind::Variable(let_declaration.name.value())
                                .in_span(let_declaration.name.span()),
                        }),
                        Box::new(value),
                    )),
                },
            )
            .bb;
        }
    }

    bb
}

/// Process a vector of [`TypedStmt`]s (a block) and handle each statement.
///
/// # Panics
/// Panics if an internal code generation error is encountered.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    clippy::needless_pass_by_value,
    clippy::redundant_pub_crate
)]
pub(crate) fn cg_block<'ctx, 'input, 'a>(
    cg: CgContext<'ctx, 'a>,
    bb: BasicBlock<'ctx>,
    parent_scope: &'a CgScope<'input, 'ctx>,
    parent_lexical_block: DILexicalBlock<'ctx>,
    block: Spanned<Vec<TypedStmt<'input>>>,
    breakaway: &Option<LoopBreakaway<'ctx>>,
) -> Option<BasicBlock<'ctx>> {
    let mut scope = parent_scope.clone();
    let block_span = block.span();
    let block_line_col = cg.line_lookup.lookup_from_index(block_span.start());
    let lexical_block = cg.dbg_builder.create_lexical_block(
        parent_lexical_block.as_debug_info_scope(),
        cg.compilation_unit.get_file(),
        block_line_col.line,
        block_line_col.col,
    );

    block
        .into_value()
        .into_iter()
        .try_fold(bb, |bb, stmt| -> Option<BasicBlock> {
            let stmt_span = stmt.0.span();
            let stmt_line_col = cg.line_lookup.lookup_from_index(stmt_span.start());
            let debug_location = cg.dbg_builder.create_debug_location(
                cg.ctx,
                stmt_line_col.line,
                stmt_line_col.col,
                lexical_block.as_debug_info_scope(),
                None,
            );
            cg.builder.set_current_debug_location(debug_location);

            match stmt.0.into_value() {
                TypedStmtKind::ExprStmt(expr) => {
                    Some(cg_expr(cg, bb, &scope, lexical_block, expr).bb)
                }

                TypedStmtKind::IfStmt(cond, then, then_else) => {
                    let then_else = then_else.unwrap_or_else(|| {
                        vec![].in_span(Span::from_positions(then.end(), then.end()))
                    });

                    let BasicBlockAnd { value: cond, .. } =
                        cg_expr(cg, bb, &scope, lexical_block, cond);

                    let then_bb = cg.ctx.append_basic_block(cg.fn_value, "then");
                    let then_else_bb = cg.ctx.append_basic_block(cg.fn_value, "then_else");

                    cg.builder
                        .build_conditional_branch(cond.into_int_value(), then_bb, then_else_bb)
                        .expect("conditional branch should generate successfully");

                    cg.builder.position_at_end(then_bb);
                    let maybe_then_bb =
                        cg_block(cg, then_bb, &scope, lexical_block, then, breakaway);

                    cg.builder.position_at_end(then_else_bb);
                    let maybe_then_else_bb = cg_block(
                        cg,
                        then_else_bb,
                        &scope,
                        lexical_block,
                        then_else,
                        breakaway,
                    );

                    match (maybe_then_bb, maybe_then_else_bb) {
                        (None, None) => None,
                        (Some(single_bb), None) | (None, Some(single_bb)) => {
                            let end = cg.ctx.append_basic_block(cg.fn_value, "end");

                            cg.builder.position_at_end(single_bb);
                            cg.builder
                                .build_unconditional_branch(end)
                                .expect("branch should generate successfully");

                            cg.builder.position_at_end(end);
                            Some(end)
                        }
                        (Some(then_bb), Some(then_else_bb)) => {
                            let end = cg.ctx.append_basic_block(cg.fn_value, "end");

                            cg.builder.position_at_end(then_bb);
                            cg.builder
                                .build_unconditional_branch(end)
                                .expect("branch should generate successfully");

                            cg.builder.position_at_end(then_else_bb);
                            cg.builder
                                .build_unconditional_branch(end)
                                .expect("branch should generate successfully");

                            cg.builder.position_at_end(end);
                            Some(end)
                        }
                    }
                }

                TypedStmtKind::BlockStmt(block) => cg_block(
                    cg,
                    bb,
                    &scope,
                    lexical_block,
                    block.in_span(stmt_span),
                    breakaway,
                ),

                TypedStmtKind::ReturnStmt(Some(expr)) => {
                    let BasicBlockAnd { value: expr, .. } =
                        cg_expr(cg, bb, &scope, lexical_block, expr);

                    cg.builder
                        .build_return(Some(&expr))
                        .expect("return should generate successfully");

                    None
                }

                TypedStmtKind::ReturnStmt(None) => {
                    cg.builder
                        .build_return(None)
                        .expect("return should generate successfully");

                    None
                }

                TypedStmtKind::ContinueStmt => {
                    cg.builder
                        .build_unconditional_branch(
                            breakaway
                                .as_ref()
                                .expect("`breakaway` should exist all places `continue` is valid")
                                .on_continue,
                        )
                        .expect("branch should generate successfully");

                    None
                }

                TypedStmtKind::BreakStmt => {
                    cg.builder
                        .build_unconditional_branch(
                            breakaway
                                .as_ref()
                                .expect("`breakaway` should exist all places `break` is valid")
                                .on_break,
                        )
                        .expect("branch should generate successfully");

                    None
                }

                TypedStmtKind::DeclarationList(declarations) => Some(cg_let_declaration(
                    cg,
                    bb,
                    &mut scope,
                    lexical_block,
                    declarations,
                )),

                TypedStmtKind::ForStmt {
                    init,
                    cond,
                    post,
                    body,
                } => {
                    // For loops generate a somewhat more complicated CFG, with a few parts.
                    // The preheader, where `init` runs. Breaks to the header.
                    // The header, where `cond` is checked and breaks to either the exit or the
                    // body. The body, where most of the body runs. Breaks to
                    // the latch. `break` transfers to the exit by force and `continue` transfers to
                    // the latch by force. The latch, where `post` runs and
                    // breaks back to the header The exit, which is the basic
                    // block we return.

                    // loops lie in an implicit subscope
                    let mut scope = scope.clone();

                    // The block we are currently in will become the preheader. Generate the `init`
                    // code if there is any.
                    if let Some(init) = init {
                        cg_let_declaration(cg, bb, &mut scope, lexical_block, *init);
                    }

                    let header = cg.ctx.append_basic_block(cg.fn_value, "header");
                    let body_bb = cg.ctx.append_basic_block(cg.fn_value, "body");
                    let latch = cg.ctx.append_basic_block(cg.fn_value, "latch");
                    let exit = cg.ctx.append_basic_block(cg.fn_value, "exit");

                    // Branch to the header from the preheader.
                    cg.builder
                        .build_unconditional_branch(header)
                        .expect("branch should generate successfully");

                    // Generate the header.
                    cg.builder.position_at_end(header);
                    let header = cond.map_or_else(
                        || {
                            // If there is no condition, we always branch to the body.
                            cg.builder
                                .build_unconditional_branch(body_bb)
                                .expect("branch should generate successfully");

                            header
                        },
                        |cond| {
                            let BasicBlockAnd {
                                bb: header,
                                value: cond,
                            } = cg_expr(cg, header, &scope, lexical_block, cond);

                            cg.builder
                                .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                                .expect("branch should generate successfully");

                            header
                        },
                    );

                    // Generate the body.
                    cg.builder.position_at_end(body_bb);
                    let body_bb = cg_block(
                        cg,
                        body_bb,
                        &scope,
                        lexical_block,
                        body,
                        &Some(LoopBreakaway {
                            on_break: exit,
                            on_continue: latch,
                        }),
                    );

                    // The body breaks to latch
                    if body_bb.is_some() {
                        cg.builder
                            .build_unconditional_branch(latch)
                            .expect("branch should generate successfully");
                    }

                    // Latch runs post and then breaks right back to the header.
                    cg.builder.position_at_end(latch);
                    if let Some(post) = post {
                        cg_expr(cg, latch, &scope, lexical_block, post);
                    }

                    cg.builder
                        .build_unconditional_branch(header)
                        .expect("branch should generate successfully");

                    cg.builder.position_at_end(exit);

                    Some(exit)
                }

                TypedStmtKind::WhileStmt(cond, body) => {
                    // While loops are similar to for loops but much simpler.
                    // The preheader simply just breaks to the header.
                    // The header checks the condition and breaks to the exit or the body.
                    // The body simply breaks to the header.
                    // The exit is the continued code

                    // `break` => exit
                    // `continue` => header

                    let header = cg.ctx.append_basic_block(cg.fn_value, "header");

                    let body_bb = cg.ctx.append_basic_block(cg.fn_value, "body");

                    let exit = cg.ctx.append_basic_block(cg.fn_value, "exit");

                    cg.builder
                        .build_unconditional_branch(header)
                        .expect("branch should generate successfully");

                    cg.builder.position_at_end(header);

                    let BasicBlockAnd { value: cond, .. } =
                        cg_expr(cg, header, &scope, lexical_block, cond);

                    cg.builder
                        .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                        .expect("branch should generate successfully");

                    cg.builder.position_at_end(body_bb);

                    let body_bb = cg_block(
                        cg,
                        body_bb,
                        &scope,
                        lexical_block,
                        body,
                        &Some(LoopBreakaway {
                            on_break: exit,
                            on_continue: header,
                        }),
                    );

                    if body_bb.is_some() {
                        cg.builder
                            .build_unconditional_branch(header)
                            .expect("branch should generate successfully");
                    }

                    cg.builder.position_at_end(exit);

                    Some(exit)
                }
            }
        })
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn let_declarations_are_properly_generated() {
        cg_snapshot_test!(indoc! {"
            fn test() {
                // TEST: should allocate twice and assign to one.
                let a: i32;
                let b: i32 = 7;

                return;
            }
        "});
    }

    mod cg_block {
        use super::*;

        mod conditionals {
            use super::*;

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
                        return;
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

        mod loops {
            use super::*;

            #[test]
            fn while_loops_along_with_break_and_continue_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;

                    fn test() {
                        // TEST: the proper while loop structure is created
                        while (get_bool()) {
                            // TEST: break jumps to the `end` block
                            if (get_bool()) break;
                            else {
                                // TEST: continue jumps to the header block
                                if (get_bool()) continue;
                                // TEST: otherwise, we proceed
                                else {}
                            }

                            // TEST: the loop jumps back to the header block
                        }

                        // TEST: ...and code generation properly continues.
                        return;
                    }
                "});
            }

            #[test]
            fn for_loops_along_with_break_and_continue_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_int() -> i32;

                    fn test() {
                        // TEST: the proper while loop structure is created
                        for (let i = 0; i < get_int(); i += 1) {
                            // TEST: break jumps to the `end` block
                            if (i > get_int()) break;
                            else {
                                // TEST: continue jumps to the latch block
                                if (i < get_int()) continue;
                                else {}
                            }

                            // TEST: the loop jumps to the latch block which jumps back to the
                            // header
                        }

                        return;
                    }
                "});
            }
        }
    }
}
