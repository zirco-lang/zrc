//! code generation for prefix/postfix increment/decrement expressions

use inkwell::values::{BasicValue, BasicValueEnum};
use zrc_typeck::tast::expr::Place;

use super::place::cg_place;
use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::CgExprArgs,
    ty::{llvm_basic_type, llvm_int_type},
    unpack,
};

/// Code generate a prefix increment expression
pub fn cg_prefix_increment<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    place: Place<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let place_ptr = unpack!(bb = cg_place(cg, bb, place));

    // Load current value
    let current = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, place_ptr, "load")
        .expect("prefix increment load should have compiled successfully");

    // Add 1
    let one = llvm_int_type(&cg, &inferred_type).0.const_int(1, false);
    let new_value = cg
        .builder
        .build_int_add(current.into_int_value(), one, "inc")
        .expect("prefix increment add should have compiled successfully");

    // Store back
    cg.builder
        .build_store(place_ptr, new_value)
        .expect("prefix increment store should have compiled successfully");

    // Return new value
    bb.and(new_value.as_basic_value_enum())
}

/// Code generate a prefix decrement expression
pub fn cg_prefix_decrement<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    place: Place<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let place_ptr = unpack!(bb = cg_place(cg, bb, place));

    // Load current value
    let current = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, place_ptr, "load")
        .expect("prefix decrement load should have compiled successfully");

    // Subtract 1
    let one = llvm_int_type(&cg, &inferred_type).0.const_int(1, false);
    let new_value = cg
        .builder
        .build_int_sub(current.into_int_value(), one, "dec")
        .expect("prefix decrement sub should have compiled successfully");

    // Store back
    cg.builder
        .build_store(place_ptr, new_value)
        .expect("prefix decrement store should have compiled successfully");

    // Return new value
    bb.and(new_value.as_basic_value_enum())
}

/// Code generate a postfix increment expression
pub fn cg_postfix_increment<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    place: Place<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let place_ptr = unpack!(bb = cg_place(cg, bb, place));

    // Load current value
    let current = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, place_ptr, "load")
        .expect("postfix increment load should have compiled successfully");

    // Add 1
    let one = llvm_int_type(&cg, &inferred_type).0.const_int(1, false);
    let new_value = cg
        .builder
        .build_int_add(current.into_int_value(), one, "inc")
        .expect("postfix increment add should have compiled successfully");

    // Store back
    cg.builder
        .build_store(place_ptr, new_value)
        .expect("postfix increment store should have compiled successfully");

    // Return old value
    bb.and(current)
}

/// Code generate a postfix decrement expression
pub fn cg_postfix_decrement<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    place: Place<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let place_ptr = unpack!(bb = cg_place(cg, bb, place));

    // Load current value
    let current = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, place_ptr, "load")
        .expect("postfix decrement load should have compiled successfully");

    // Subtract 1
    let one = llvm_int_type(&cg, &inferred_type).0.const_int(1, false);
    let new_value = cg
        .builder
        .build_int_sub(current.into_int_value(), one, "dec")
        .expect("postfix decrement sub should have compiled successfully");

    // Store back
    cg.builder
        .build_store(place_ptr, new_value)
        .expect("postfix decrement store should have compiled successfully");

    // Return old value
    bb.and(current)
}
