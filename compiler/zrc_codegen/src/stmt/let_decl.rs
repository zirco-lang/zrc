//! Code generation for let declarations

use inkwell::{
    basic_block::BasicBlock,
    debug_info::{AsDIScope, DILexicalBlock},
};
use zrc_typeck::tast::{
    expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
    stmt::LetDeclaration,
};
use zrc_utils::span::{Spannable, Spanned};

use crate::{
    ctx::{BlockCtx, FunctionCtx},
    expr::cg_expr,
    scope::CgScope,
    ty::llvm_basic_type,
};

/// Generates the `alloca`tion, `store` instruction, and adds a new identifier
/// to the [`CgScope`].
///
/// # Panics
/// Panics if an internal code generation error is encountered.
pub fn cg_let_declaration<'ctx, 'input, 'a>(
    cg: FunctionCtx<'ctx, 'a>,
    mut bb: BasicBlock<'ctx>,
    scope: &'a mut CgScope<'input, 'ctx>,
    dbg_scope: Option<DILexicalBlock<'ctx>>,
    declarations: Vec<Spanned<LetDeclaration<'input>>>,
) -> BasicBlock<'ctx> {
    for spanned_let_declaration in declarations {
        let span = spanned_let_declaration.span();
        let let_declaration = spanned_let_declaration.into_value();

        let stmt_line_col = cg.line_lookup.lookup_from_index(span.start());

        if let Some(dbg_builder) = &cg.dbg_builder {
            let debug_location = dbg_builder.create_debug_location(
                cg.ctx,
                stmt_line_col.line,
                stmt_line_col.col,
                dbg_scope.expect("we have DI").as_debug_info_scope(),
                None,
            );
            cg.builder.set_current_debug_location(debug_location);
        }

        // we create our own builder here because we need to insert the alloca
        // at the beginning of the entry block, and that is easier than trying to
        // somehow save our position.

        let entry_block_builder = cg.ctx.create_builder();
        let first_bb = cg
            .fn_value
            .get_first_basic_block()
            .expect("function should have at least one basic block");

        match first_bb.get_first_instruction() {
            Some(first_instruction) => {
                entry_block_builder.position_before(&first_instruction);
            }
            None => {
                entry_block_builder.position_at_end(first_bb);
            }
        }

        let (ty, _dbg_ty) = llvm_basic_type(&cg, &let_declaration.ty);

        let ptr = entry_block_builder
            .build_alloca(ty, &format!("let_{}", let_declaration.name))
            .expect("alloca should generate successfully");

        scope.insert(let_declaration.name.value(), ptr);

        // let decl = cg.dbg_builder.create_auto_variable(
        //     dbg_scope.as_debug_info_scope(),
        //     let_declaration.name.value(),
        //     cg.compilation_unit.get_file(),
        //     cg.line_lookup.lookup_from_index(span.start()).line,
        //     dbg_ty,
        //     true,
        //     0,
        //     0,
        // );

        // FIXME: Re-enable this when Inkwell resolves TheDan64/inkwell#613
        // cg.dbg_builder
        //     .insert_declare_at_end(ptr, Some(decl), None, debug_location, first_bb);

        if let Some(value) = let_declaration.value {
            let expr_cg = BlockCtx::new(cg, scope, dbg_scope);

            bb = cg_expr(
                expr_cg,
                bb,
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
            }
        "});
    }

    #[test]
    fn variable_shadowing_generates_properly() {
        cg_snapshot_test!(indoc! {"
            fn test() {
                // TEST: shadowing should work, each variable gets its own allocation
                let x: i32 = 5;
                let y: i32 = x + 1;  // uses first x (5)
                let x: i32 = 10;     // shadows x
                let z: i32 = x + 1;  // uses second x (10)
            }
        "});
    }
}
