//! Code generation for Place types
//!
//! This module contains functions to generate LLVM IR for
//! [`zrc_typeck::tast::expr::Place`] expressions, which represent locations
//! that can be assigned to, such as variables, dereferences, and indexed
//! accesses.
//!
//! The main function is [`cg_place`], which takes a `Place` and generates
//! the corresponding LLVM IR to obtain a pointer to the location represented
//! by the `Place`.

use inkwell::{
    basic_block::BasicBlock,
    debug_info::AsDIScope,
    values::{BasicValue, PointerValue},
};
use zrc_typeck::tast::{
    expr::{Place, PlaceKind},
    ty::Type,
};

use super::cg_expr;
use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    ctx::BlockCtx,
    ty::llvm_basic_type,
    unpack,
};

/// Resolve a place to its LLVM [`PointerValue`]
pub fn cg_place<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    mut bb: BasicBlock<'ctx>,
    place: Place,
) -> BasicBlockAnd<'ctx, PointerValue<'ctx>> {
    let place_span = place.kind.span();
    let line_and_col = cg.line_lookup.lookup_from_index(place_span.start());
    let _debug_location = cg.dbg_builder.as_ref().map(|dbg_builder| {
        let dl = dbg_builder.create_debug_location(
            cg.ctx,
            line_and_col.line,
            line_and_col.col,
            cg.dbg_scope.expect("we have DI").as_debug_info_scope(),
            None,
        );
        cg.builder.set_current_debug_location(dl);
        dl
    });

    match place.kind.into_value() {
        PlaceKind::Variable(x) => {
            let reg = cg
                .scope
                .get(x)
                .expect("identifier that passed typeck should exist in the CgScope");

            bb.and(reg)
        }

        PlaceKind::Deref(x) => {
            let value = unpack!(bb = cg_expr(cg, bb, *x));

            bb.and(value.into_pointer_value())
        }

        PlaceKind::Index(ptr, idx) => {
            let ptr_val = unpack!(bb = cg_expr(cg, bb, *ptr));
            let idx = unpack!(bb = cg_expr(cg, bb, *idx));

            // If the left-hand side already yields a pointer, behave as
            // before. If it yields an aggregate (e.g., an array value), we
            // need to decay it to a pointer to its first element by
            // allocating a temporary on the stack, storing the aggregate
            // into it, and then bitcasting that allocation to a pointer to
            // the element type before performing GEP.
            let ptr_to_use = if ptr_val.is_pointer_value() {
                ptr_val.into_pointer_value()
            } else {
                // Allocate space for the aggregate value on the stack.
                // IMPORTANT: We must allocate at the entry block to avoid UB
                // when indexing happens inside loops or other control flow.
                let agg_ty = ptr_val.get_type();

                // Create a builder positioned at the entry block
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

                let agg_alloc = entry_block_builder
                    .build_alloca(agg_ty, "array_tmp")
                    .expect("alloca for aggregate temporary should succeed");

                // Store the aggregate into the temporary (using the current builder)
                cg.builder
                    .build_store(agg_alloc, ptr_val)
                    .expect("storing aggregate into temporary should succeed");

                agg_alloc
            };

            // SAFETY: This can segfault if indices are used incorrectly; the
            // tests ensure indices are well-formed. `place.inferred_type` is
            // the element type.
            let reg = unsafe {
                cg.builder.build_gep(
                    llvm_basic_type(&cg, &place.inferred_type).0,
                    ptr_to_use,
                    &[idx.into_int_value()],
                    "gep",
                )
            }
            .expect("building GEP instruction should succeed");

            bb.and(reg.as_basic_value_enum().into_pointer_value())
        }

        #[expect(clippy::wildcard_enum_match_arm)]
        PlaceKind::Dot(x, prop) => match &x.inferred_type {
            Type::Struct(contents) => {
                let x_ty = llvm_basic_type(&cg, &x.inferred_type).0;
                let prop_idx = contents
                    .iter()
                    .position(|(got_key, _)| *got_key == prop.into_value())
                    .expect("invalid struct field");

                let x = unpack!(bb = cg_place(cg, bb, *x));

                let reg = cg
                    .builder
                    .build_struct_gep(
                        x_ty,
                        x,
                        prop_idx
                            .try_into()
                            .expect("got more than u32::MAX as key index? HOW?"),
                        "gep",
                    )
                    .expect("building GEP instruction should succeed");

                bb.and(reg.as_basic_value_enum().into_pointer_value())
            }
            Type::Union(_) => {
                // All we need to do is cast the pointer, but there's no `bitcast` anymore,
                // so just return it and it'll take on the correct type

                let value = unpack!(bb = cg_place(cg, bb, *x));

                bb.and(value)
            }
            _ => panic!("cannot access property of non-struct"),
        },
    }
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    // Remember: In all of these tests, cg_place returns a *pointer* to the data in
    // the place.

    #[test]
    fn basic_identifiers_in_place_position() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let x = 6;

                    // TEST: we should simply be `store`ing to the %let_x we created
                    x = 7;
                }
            "});
    }

    #[test]
    fn identifier_deref_generates_as_expected() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let x: *i32;

                    // TEST: x is *i32, so %let_x is **i32 (ptr to the stack). we should load from
                    // %let_x to obtain the actual pointer. we should then store to that result
                    // value (we should never load it)
                    *x = 4;
                }
            "});
    }

    #[test]
    fn other_deref_generates_as_expected() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    // TEST: because cg_place returns a *pointer* to the represented value, handling
                    // *5 in a place context should return the address of *5, which is &*5 = 5.
                    // for this reason, we should literally be `store`ing to the hardcoded address
                    // 5, and never *loading* from it (because if we do load we may not be actually
                    // writing to that address)
                    // we use 5 not 0 because 0 is just 'ptr null'
                    *(5 as *i32) = 0;
                }
            "});
    }

    #[test]
    fn pointer_indexing_in_place_position() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let x: *i32;

                    // TEST: `x` is *i32, so %let_x is a **i32 (ptr to the stack).
                    // %let_x needs to be GEP'd into and then stored into, but we must not load
                    // from the address.
                    x[4 as usize] = 5;
                }
            "});
    }

    #[test]
    fn struct_property_access_in_place_position() {
        cg_snapshot_test!(indoc! {"
                struct S { x: i32, y: i32 }

                fn test() {
                    let x: S;

                    // TEST: the value must NOT be loaded! it must simply gep to obtain a pointer,
                    // then `store` into that pointer.
                    x.y = 4;
                }
            "});
    }

    #[test]
    fn array_value_indexing_in_loop() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let x: [3]i32 = [1i32, 2i32, 3i32];
                    let sum = 0;

                    // TEST: This tests that the temporary allocation for array value
                    // indexing happens in the entry block, not inside the loop.
                    // The alloca for array_tmp should appear at the top of the function.
                    for (let i = 0; i < 3; i++) {
                        sum = sum + x[i as usize];
                    }
                }
            "});
    }

    #[test]
    fn union_property_access_in_place_position() {
        cg_snapshot_test!(indoc! {"
                union U { x: i32, y: i8 }

                fn test() {
                    let x: U;

                    // TEST: the pointer is cast and then written to as an i32
                    x.x = 4;

                    // TEST: the pointer is cast and then written to as an i8
                    x.y = 5 as i8;
                }
            "});
    }
}
