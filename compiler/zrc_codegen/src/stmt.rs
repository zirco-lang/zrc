//! Code generation for statements
//!
//! This module contains functions to generate LLVM IR for
//! [`zrc_typeck::tast::stmt::TypedStmt`] statements, which
//! represent all statements in the Zirco language.
//!
//! The main function is [`cg_block`], which takes a vector of
//! `TypedStmt`s (a block) and generates the corresponding LLVM IR
//! to execute the statements in order.
//!
//! It also contains helper functions for generating specific
//! statement types, such as [`cg_let_declaration`]
//! which handles `let` declarations.
//!
//! The code generator maintains a [`CgScope`] to track variable
//! bindings and their corresponding LLVM values.

mod branch;
mod let_decl;
mod loops;
mod switch;

use inkwell::{
    basic_block::BasicBlock,
    debug_info::{AsDIScope, DILexicalBlock},
};
use zrc_typeck::{
    tast::{stmt::TypedStmtKind, ty::Type},
    typeck::BlockMetadata,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    ctx::{BlockCtx, FunctionCtx},
    expr::cg_expr,
    scope::CgScope,
    ty::llvm_basic_type,
};

/// Consists of the [`BasicBlock`]s to `br` to when encountering certain
/// instructions. It is passed to [`cg_block`] to allow it to properly handle
/// break and continue.
#[derive(PartialEq, Eq, Debug, Clone)]
#[expect(clippy::redundant_pub_crate)]
pub(crate) struct LoopBreakaway<'ctx> {
    /// Points to the exit basic block.
    on_break: BasicBlock<'ctx>,
    /// For `for` loops, points to the latch. For `while` loops, points to the
    /// header.
    on_continue: BasicBlock<'ctx>,
}

/// Process a vector of [`TypedStmt`]s (a block) and handle each statement.
///
/// # Panics
/// Panics if an internal code generation error is encountered.
#[expect(
    clippy::too_many_lines,
    clippy::redundant_pub_crate,
    clippy::ref_option,
    clippy::needless_pass_by_value
)]
pub(crate) fn cg_block<'ctx, 'input, 'a>(
    cg: FunctionCtx<'ctx, 'a>,
    bb: BasicBlock<'ctx>,
    parent_scope: &'a CgScope<'input, 'ctx>,
    parent_lexical_block: Option<DILexicalBlock<'ctx>>,
    block: Spanned<BlockMetadata<'input, '_>>,
    breakaway: &Option<LoopBreakaway<'ctx>>,
) -> Option<BasicBlock<'ctx>> {
    let mut scope = parent_scope.clone();
    let block_span = block.span();
    let block_line_col = cg.line_lookup.lookup_from_index(block_span.start());

    let lexical_block = cg.dbg_builder.as_ref().map(|dbg_builder| {
        dbg_builder.create_lexical_block(
            parent_lexical_block
                .expect("We have DI")
                .as_debug_info_scope(),
            cg.compilation_unit.expect("We have DI").get_file(),
            block_line_col.line,
            block_line_col.col,
        )
    });

    let stmts = block.value().stmts.as_slice();

    stmts.iter().try_fold(bb, |bb, stmt| -> Option<BasicBlock> {
        let stmt_span = stmt.kind.span();
        let stmt_line_col = cg.line_lookup.lookup_from_index(stmt_span.start());
        cg.dbg_builder.as_ref().map(|dbg_builder| {
            let debug_location = dbg_builder.create_debug_location(
                cg.ctx,
                stmt_line_col.line,
                stmt_line_col.col,
                lexical_block.expect("We have DI").as_debug_info_scope(),
                None,
            );
            cg.builder.set_current_debug_location(debug_location);
            debug_location
        });

        match stmt.kind.value() {
            TypedStmtKind::UnreachableStmt => {
                cg.builder
                    .build_unreachable()
                    .expect("unreachable should generate successfully");

                None
            }

            TypedStmtKind::SwitchCase {
                scrutinee,
                default,
                cases,
            } => Some(switch::cg_switch_stmt(
                cg,
                bb,
                &scope,
                lexical_block,
                breakaway,
                stmt_span,
                scrutinee.clone(),
                default.clone(),
                cases.clone(),
            )),

            TypedStmtKind::ExprStmt(expr) => {
                let expr_cg = BlockCtx::new(cg, &scope, lexical_block);

                Some(cg_expr(expr_cg, bb, expr.clone()).bb)
            }

            TypedStmtKind::IfStmt(cond, then, then_else) => branch::cg_if_stmt(
                cg,
                bb,
                &scope,
                lexical_block,
                breakaway,
                cond.clone(),
                then.clone(),
                then_else.clone(),
            ),

            TypedStmtKind::BlockStmt(block) => cg_block(
                cg,
                bb,
                &scope,
                lexical_block,
                block.clone().in_span(stmt_span),
                breakaway,
            ),

            TypedStmtKind::ReturnStmt(Some(expr)) => {
                let expr_cg = BlockCtx::new(cg, &scope, lexical_block);

                let expr = cg_expr(expr_cg, bb, expr.clone()).into_value();

                cg.builder
                    .build_return(Some(&expr))
                    .expect("return should generate successfully");

                None
            }

            TypedStmtKind::ReturnStmt(None) => {
                let unit_type = llvm_basic_type(&cg, &Type::unit());

                cg.builder
                    .build_return(Some(&unit_type.0.const_zero()))
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

            TypedStmtKind::DeclarationList(declarations) => Some(let_decl::cg_let_declaration(
                cg,
                bb,
                &mut scope,
                lexical_block,
                declarations.clone(),
            )),

            TypedStmtKind::ForStmt {
                init,
                cond,
                post,
                body,
            } => Some(loops::cg_for_stmt(
                cg,
                bb,
                &scope,
                lexical_block,
                init.clone(),
                cond.clone(),
                post.clone(),
                body.clone(),
            )),
            TypedStmtKind::FourStmt(body) => Some(loops::cg_four_stmt(
                cg,
                bb,
                &scope,
                lexical_block,
                body.clone(),
            )),

            TypedStmtKind::WhileStmt(cond, body) => Some(loops::cg_while_stmt(
                cg,
                &scope,
                lexical_block,
                cond.clone(),
                body.clone(),
            )),

            TypedStmtKind::DoWhileStmt(body, cond) => Some(loops::cg_do_while_stmt(
                cg,
                &scope,
                lexical_block,
                body.clone(),
                cond.clone(),
            )),
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
    fn unreachable_statement_generates_properly() {
        cg_snapshot_test!(indoc! {"
            fn test(cond: bool) {
                let x = 7;
                if (x == 6) unreachable;
                else return;
            }
        "});
    }
}
