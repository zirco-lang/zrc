//! Code generation for expressions

pub mod place;

use inkwell::{
    IntPredicate,
    basic_block::BasicBlock,
    builder::BuilderError,
    debug_info::AsDIScope,
    types::{BasicType, StringRadix},
    values::{BasicValue, BasicValueEnum, IntValue},
};
use place::cg_place;
use zrc_typeck::tast::{
    expr::{
        Arithmetic, BinaryBitwise, Comparison, Equality, Logical, NumberLiteral, Place, PlaceKind,
        TypedExpr, TypedExprKind,
    },
    ty::Type,
};
use zrc_utils::span::Spannable;

use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    ctx::BlockCtx,
    ty::{llvm_basic_type, llvm_int_type, llvm_type},
    unpack,
};

/// Get a [`NumberLiteral`]'s [`StringRadix`]
const fn number_literal_radix(n: &NumberLiteral) -> StringRadix {
    match n {
        NumberLiteral::Decimal(_) => StringRadix::Decimal,
        NumberLiteral::Binary(_) => StringRadix::Binary,
        NumberLiteral::Hexadecimal(_) => StringRadix::Hexadecimal,
    }
}
/// Get the [`IntPredicate`] for an [`Equality`] operation
const fn int_predicate_for_equality(op: &Equality) -> IntPredicate {
    match op {
        Equality::Eq => IntPredicate::EQ,
        Equality::Neq => IntPredicate::NE,
    }
}
/// Get the [`IntPredicate`] for a [`Comparison`] operation
const fn int_predicate_for_comparison(op: &Comparison, signed: bool) -> IntPredicate {
    match (op, signed) {
        (Comparison::Lt, true) => IntPredicate::SLT,
        (Comparison::Lt, false) => IntPredicate::ULT,
        (Comparison::Gt, true) => IntPredicate::SGT,
        (Comparison::Gt, false) => IntPredicate::UGT,
        (Comparison::Lte, true) => IntPredicate::SLE,
        (Comparison::Lte, false) => IntPredicate::ULE,
        (Comparison::Gte, true) => IntPredicate::SGE,
        (Comparison::Gte, false) => IntPredicate::UGE,
    }
}
/// Build the required instruction for a [`BinaryBitwise`] operation
pub fn build_binary_bitwise<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    op: &BinaryBitwise,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    result_is_signed: bool,
) -> Result<IntValue<'ctx>, BuilderError> {
    match op {
        BinaryBitwise::And => cg.builder.build_and(lhs, rhs, "and"),
        BinaryBitwise::Or => cg.builder.build_or(lhs, rhs, "or"),
        BinaryBitwise::Xor => cg.builder.build_xor(lhs, rhs, "xor"),
        BinaryBitwise::Shl => cg.builder.build_left_shift(lhs, rhs, "shl"),
        BinaryBitwise::Shr => cg
            .builder
            .build_right_shift(lhs, rhs, result_is_signed, "shr"),
    }
}
/// Build the required instruction for an [`Arithmetic`] operation
pub fn build_arithmetic<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    op: &Arithmetic,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    result_is_signed: bool,
) -> Result<IntValue<'ctx>, BuilderError> {
    match op {
        Arithmetic::Addition => cg.builder.build_int_add(lhs, rhs, "add"),
        Arithmetic::Subtraction => cg.builder.build_int_sub(lhs, rhs, "sub"),
        Arithmetic::Multiplication => cg.builder.build_int_mul(lhs, rhs, "mul"),

        Arithmetic::Division if result_is_signed => {
            cg.builder.build_int_signed_div(lhs, rhs, "div")
        }
        Arithmetic::Division => cg.builder.build_int_unsigned_div(lhs, rhs, "div"),

        Arithmetic::Modulo if result_is_signed => cg.builder.build_int_signed_rem(lhs, rhs, "rem"),
        Arithmetic::Modulo => cg.builder.build_int_unsigned_rem(lhs, rhs, "rem"),
    }
}

/// Generate an expression, yielding its result.
#[allow(
    clippy::redundant_pub_crate,
    clippy::too_many_lines,
    clippy::match_same_arms,
    clippy::too_many_arguments
)]
pub(crate) fn cg_expr<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    mut bb: BasicBlock<'ctx>,
    expr: TypedExpr,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let expr_span = expr.kind.span();
    let line_and_col = cg.line_lookup.lookup_from_index(expr_span.start());
    let debug_location = cg.dbg_builder.create_debug_location(
        cg.ctx,
        line_and_col.line,
        line_and_col.col,
        cg.dbg_scope.as_debug_info_scope(),
        None,
    );
    cg.builder.set_current_debug_location(debug_location);

    match expr.kind.into_value() {
        TypedExprKind::NumberLiteral(n, _) => {
            let no_underscores = n.text_content().replace('_', "");

            bb.and(
                llvm_int_type(&cg, &expr.inferred_type)
                    .0
                    .const_int_from_string(&no_underscores, number_literal_radix(&n))
                    .expect("number literal should have parsed correctly")
                    .as_basic_value_enum(),
            )
        }

        TypedExprKind::StringLiteral(str) => bb.and(
            cg.builder
                .build_global_string_ptr(&str.as_bytes(), "str")
                .expect("string should have built successfully")
                .as_basic_value_enum(),
        ),
        TypedExprKind::CharLiteral(ch) => bb.and(
            cg.ctx
                .i8_type()
                .const_int(ch.as_byte().into(), false)
                .as_basic_value_enum(),
        ),

        TypedExprKind::BooleanLiteral(value) => bb.and(
            cg.ctx
                .bool_type()
                .const_int(value.into(), false)
                .as_basic_value_enum(),
        ),

        TypedExprKind::Comma(lhs, rhs) => {
            let _ = unpack!(bb = cg_expr(cg, bb, *lhs));
            cg_expr(cg, bb, *rhs)
        }

        TypedExprKind::Identifier(id) => {
            let place = unpack!(
                bb = cg_place(
                    cg,
                    bb,
                    Place {
                        inferred_type: expr.inferred_type.clone(),
                        kind: PlaceKind::Variable(id).in_span(expr_span),
                    },
                )
            );

            let reg = cg
                .builder
                .build_load(llvm_basic_type(&cg, &expr.inferred_type).0, place, "load")
                .expect("ident load should have built successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::Assignment(place, value) => {
            let value = unpack!(bb = cg_expr(cg, bb, *value));
            let place = unpack!(bb = cg_place(cg, bb, *place));

            cg.builder
                .build_store(place, value)
                .expect("store instruction in assignment should have built successfully");

            bb.and(value)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
            let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

            let reg = build_binary_bitwise(
                cg,
                &op,
                lhs.into_int_value(),
                rhs.into_int_value(),
                expr.inferred_type.is_signed_integer(),
            )
            .expect("binary bitwise operation should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::Equality(op, lhs, rhs) => {
            let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
            let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

            let reg = cg
                .builder
                .build_int_compare(
                    int_predicate_for_equality(&op),
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "cmp",
                )
                .expect("equality comparison should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::Comparison(op, lhs, rhs) => {
            let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
            let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

            let reg = cg
                .builder
                .build_int_compare(
                    int_predicate_for_comparison(&op, expr.inferred_type.is_signed_integer()),
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "cmp",
                )
                .expect("comparison should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::Arithmetic(op, lhs, rhs) => {
            let lhs_ty = lhs.inferred_type.clone();
            let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
            let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

            if let Type::Ptr(pointee) = lhs_ty {
                // Most languages make incrementing a pointer increase the address by the size
                // of the pointee type, hence our use of `gep`.
                // REVIEW: Is this the approach we want to take?
                #[allow(clippy::wildcard_enum_match_arm)]
                let reg = match op {
                    // SAFETY: this can panic if indices are used incorrectly
                    // TODO: is this safe?
                    Arithmetic::Addition => unsafe {
                        cg.builder.build_gep(
                            llvm_basic_type(&cg, &pointee).0,
                            lhs.into_pointer_value(),
                            &[rhs.into_int_value()],
                            "ptr_add",
                        )
                    },
                    Arithmetic::Subtraction => {
                        let rhs = cg
                            .builder
                            .build_int_neg(rhs.into_int_value(), "neg")
                            .expect("negation should have compiled successfully")
                            .as_basic_value_enum();

                        // SAFETY: this can panic if indices are used incorrectly
                        // TODO: is this safe?
                        unsafe {
                            cg.builder.build_gep(
                                llvm_basic_type(&cg, &pointee).0,
                                lhs.into_pointer_value(),
                                &[rhs.into_int_value()],
                                "ptr_sub",
                            )
                        }
                    }
                    _ => panic!("invalid pointer arithmetic operation"),
                }
                .expect("pointer arithmetic should have compiled successfully");

                bb.and(reg.as_basic_value_enum())
            } else {
                let reg = build_arithmetic(
                    cg,
                    &op,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    expr.inferred_type.is_signed_integer(),
                )
                .expect("arithmetic operation should have compiled successfully");

                bb.and(reg.as_basic_value_enum())
            }
        }

        TypedExprKind::Logical(op, lhs, rhs) => {
            let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
            let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

            let reg = match op {
                Logical::And => {
                    cg.builder
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                }
                Logical::Or => {
                    cg.builder
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
                }
            }
            .expect("logical operation should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::UnaryNot(x) => {
            let value = unpack!(bb = cg_expr(cg, bb, *x));

            let reg = cg
                .builder
                .build_not(value.into_int_value(), "not")
                .expect("not should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let value = unpack!(bb = cg_expr(cg, bb, *x));

            let reg = cg
                .builder
                .build_not(value.into_int_value(), "not")
                .expect("not should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::UnaryMinus(x) => {
            let value = unpack!(bb = cg_expr(cg, bb, *x));

            let reg = cg
                .builder
                .build_int_neg(value.into_int_value(), "neg")
                .expect("negation should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let value = unpack!(bb = cg_place(cg, bb, *x));

            bb.and(value.as_basic_value_enum())
        }

        TypedExprKind::UnaryDereference(ptr) => {
            let ptr = unpack!(bb = cg_expr(cg, bb, *ptr));

            let reg = cg
                .builder
                .build_load(
                    llvm_basic_type(&cg, &expr.inferred_type).0,
                    ptr.into_pointer_value(),
                    "load",
                )
                .expect("dereference should have compiled successfully");

            bb.and(reg.as_basic_value_enum())
        }

        TypedExprKind::Index(ptr, idx) => {
            let ptr = unpack!(
                bb = cg_place(
                    cg,
                    bb,
                    Place {
                        inferred_type: expr.inferred_type.clone(),
                        kind: PlaceKind::Index(ptr, idx).in_span(expr_span),
                    },
                )
            );

            let loaded = cg
                .builder
                .build_load(llvm_basic_type(&cg, &expr.inferred_type).0, ptr, "load")
                .expect("index load should have compiled successfully");

            bb.and(loaded.as_basic_value_enum())
        }

        TypedExprKind::Dot(place, key) => {
            let ptr = unpack!(
                bb = cg_place(
                    cg,
                    bb,
                    Place {
                        inferred_type: expr.inferred_type.clone(),
                        kind: PlaceKind::Dot(place, key).in_span(expr_span),
                    },
                )
            );

            let loaded = cg
                .builder
                .build_load(llvm_basic_type(&cg, &expr.inferred_type).0, ptr, "load")
                .expect("dot load should have compiled successfully");

            bb.and(loaded.as_basic_value_enum())
        }

        TypedExprKind::Call(f, args) => {
            let llvm_f_type = llvm_type(&cg, &f.inferred_type).0.into_function_type();

            // will always be a function pointer
            let f_ptr = unpack!(bb = cg_place(cg, bb, *f));

            let mut bb = bb;
            let old_args = args;
            let mut args = vec![];
            for arg in old_args {
                let new_arg = unpack!(bb = cg_expr(cg, bb, arg));
                args.push(new_arg.into());
            }

            let ret = cg
                .builder
                .build_indirect_call(llvm_f_type, f_ptr, &args, "call")
                .expect("call should have compiled successfully");

            bb.and(if ret.try_as_basic_value().is_left() {
                ret.try_as_basic_value().unwrap_left()
            } else {
                cg.ctx.i8_type().get_undef().as_basic_value_enum()
            })
        }
        TypedExprKind::Ternary(cond, lhs, rhs) => {
            let cond = cg_expr(cg, bb, *cond).into_value();

            // If lhs and rhs are registers, the code generated will look like:
            //   entry:
            //       ...
            //       %cond = ...
            //       br i1 %cond, label %if_true, label %if_false
            //   if_true:
            //       ... yields %lhs
            //       br label %end
            //   if_false:
            //       ... yields %rhs
            //       br label %end
            //   end:
            //       %yield = phi TY [ %lhs, %if_true ], [ %rhs, %if_false ]
            //
            // If they are constants, the code generated will load as:
            //   entry:
            //       ...
            //       %cond = ...
            //       br i1 %cond, label %if_true, label %if_false
            //   if_true:
            //       br label %end
            //   if_false:
            //       br label %end
            //   end:
            //       %yield = phi TY [ LHS VALUE, %if_true ], [ RHS VALUE, %if_false ]
            //
            // This is just a weird way to `select` and will be inlined.

            let mut if_true_bb = cg.ctx.append_basic_block(cg.fn_value, "if_true");
            let mut if_false_bb = cg.ctx.append_basic_block(cg.fn_value, "if_false");
            let end_bb = cg.ctx.append_basic_block(cg.fn_value, "end");

            cg.builder
                .build_conditional_branch(cond.into_int_value(), if_true_bb, if_false_bb)
                .expect("conditional branch should have been created successfully");

            cg.builder.position_at_end(if_true_bb);
            let if_true = unpack!(if_true_bb = cg_expr(cg, if_true_bb, *lhs));
            cg.builder
                .build_unconditional_branch(end_bb)
                .expect("unconditional branch should have been created successfully");

            cg.builder.position_at_end(if_false_bb);
            let if_false = unpack!(if_false_bb = cg_expr(cg, if_false_bb, *rhs));
            cg.builder
                .build_unconditional_branch(end_bb)
                .expect("unconditional branch should have been created successfully");

            cg.builder.position_at_end(end_bb);
            let result_reg = cg
                .builder
                .build_phi(llvm_basic_type(&cg, &expr.inferred_type).0, "yield")
                .expect("phi node should have been created successfully");
            result_reg.add_incoming(&[(&if_true, if_true_bb), (&if_false, if_false_bb)]);

            end_bb.and(result_reg.as_basic_value())
        }
        TypedExprKind::Cast(x, ty) => {
            // signed -> signed = sext
            // signed -> unsigned = sext
            // unsigned -> signed = zext
            // unsigned -> unsigned = zext
            // ptr -> ptr = bitcast
            // T -> T = bitcast
            // int -> ptr = inttoptr
            // ptr -> int = ptrtoint
            // int -> fn = inttoptr
            // fn -> int = ptrtoint

            let x_ty_is_signed_integer = x.inferred_type.is_signed_integer();

            let x = unpack!(bb = cg_expr(cg, bb, *x));

            let reg = match (
                x.get_type().is_pointer_type(),
                matches!(ty.value(), Type::Ptr(_)),
            ) {
                (true, true) => cg
                    .builder
                    .build_bitcast(
                        x.into_pointer_value(),
                        llvm_basic_type(&cg, ty.value()).0,
                        "cast",
                    )
                    .expect("bitcast should have compiled successfully"),
                (true, false) => cg
                    .builder
                    .build_ptr_to_int(
                        x.into_pointer_value(),
                        llvm_int_type(&cg, ty.value()).0,
                        "cast",
                    )
                    .expect("ptrtoint should have compiled successfully")
                    .as_basic_value_enum(),
                (false, true) => cg
                    .builder
                    .build_int_to_ptr(
                        x.into_int_value(),
                        llvm_basic_type(&cg, ty.value()).0.into_pointer_type(),
                        "cast",
                    )
                    .expect("inttoptr should have compiled successfully")
                    .as_basic_value_enum(),
                (false, false) if x.get_type().is_int_type() && ty.value().is_integer() => {
                    // Cast between two integers
                    match (x_ty_is_signed_integer, ty.value().is_signed_integer()) {
                        // (x is signed, target is signed or unsigned)
                        (true, _) => cg
                            .builder
                            .build_int_s_extend(
                                x.into_int_value(),
                                llvm_basic_type(&cg, ty.value()).0.into_int_type(),
                                "cast",
                            )
                            .expect("sext should have compiled successfully")
                            .as_basic_value_enum(),

                        // (x is signed, target is signed or unsigned)
                        (false, _) => cg
                            .builder
                            .build_int_z_extend(
                                x.into_int_value(),
                                llvm_basic_type(&cg, ty.value()).0.into_int_type(),
                                "cast",
                            )
                            .expect("zext should have compiled successfully")
                            .as_basic_value_enum(),
                    }
                }
                (false, false) => {
                    // Other casts are just bitcasts
                    cg.builder
                        .build_bitcast(
                            x.into_int_value(),
                            llvm_basic_type(&cg, ty.value()).0,
                            "cast",
                        )
                        .expect("bitcast should have compiled successfully")
                        .as_basic_value_enum()
                }
            };

            bb.and(reg)
        }
        TypedExprKind::SizeOf(ty) => {
            let reg = llvm_basic_type(&cg, &ty)
                .0
                .size_of()
                .expect("size_of should have compiled successfully")
                .as_basic_value_enum();

            bb.and(reg)
        }

        TypedExprKind::Construction(ty, fields) => {
            // Get the LLVM struct type
            let struct_type = llvm_basic_type(&cg, &ty).0;

            // Get the field order from the type
            let field_order = match &ty {
                Type::Struct(type_fields) | Type::Union(type_fields) => type_fields
                    .iter()
                    .map(|(name, _)| *name)
                    .collect::<Vec<_>>(),
                Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::I64
                | Type::U64
                | Type::Usize
                | Type::Isize
                | Type::Bool
                | Type::Ptr(_)
                | Type::Fn(_) => unreachable!("Construction type should be struct or union"),
            };

            // Build the struct in the correct field order
            let mut current_value = struct_type.into_struct_type().get_undef();
            for (index, field_name) in field_order.iter().enumerate() {
                // Find the corresponding field in the provided fields
                let field_expr = fields
                    .iter()
                    .find(|(name, _)| name.value() == field_name)
                    .map(|(_, expr)| expr)
                    .expect("field should exist");

                let field_value = unpack!(bb = cg_expr(cg, bb, field_expr.clone()));
                #[allow(clippy::cast_possible_truncation)]
                #[allow(clippy::as_conversions)]
                let field_index = index as u32;
                current_value = cg
                    .builder
                    .build_insert_value(current_value, field_value, field_index, "struct_field")
                    .expect("insert_value should have built successfully")
                    .into_struct_value();
            }

            bb.and(current_value.as_basic_value_enum())
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
    fn typed_integers_generate_properly() {
        cg_snapshot_test!(indoc! {"
                fn test() -> i8 {
                    // TEST: returns `i8`
                    return 4i8;
                }
            "});
    }

    #[test]
    fn comma_yields_right_value() {
        cg_snapshot_test!(indoc! {"
                fn f();
                fn g() -> i32;

                fn test() -> i32 {
                    // TEST: f() is run, g() is run and used as the return value
                    return f(), g();
                }
            "});
    }

    #[test]
    fn pointer_indexing_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                fn take_int(x: i32);

                fn test() {
                    let x: *i32;

                    // TEST: `x` is *i32, so %let_x is a **i32 (ptr to the stack).
                    // %let_x needs to be GEP'd into and the value `i32` at idx 4 must be loaded.
                    take_int(x[4 as usize]);
                }
            "});
    }

    #[test]
    fn pointer_deref_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                fn test() -> i32 {
                    let x: *i32;

                    // TEST: should load twice, once to read the value of `x` and once to read what
                    // it points to
                    return *x;
                }
            "});
    }

    #[test]
    fn struct_property_access_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                struct S { x: i32, y: i32 }
                fn take_int(x: i32);

                fn test() {
                    let x: S;

                    // TEST: should GEP into `x` to get the second property (`y`) and then
                    // load that value and call take_int
                    take_int(x.y);
                }
            "});
    }

    #[test]
    fn union_property_access_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                union U { x: i32, y: i8 }
                fn take_i32(x: i32);
                fn take_i8(x: i8);

                fn test() {
                    let x: U;

                    // TEST: the pointer is cast and then read from as an i32
                    take_i32(x.x);

                    // TEST: the pointer is cast and then read from as an i8
                    take_i8(x.y);
                }
            "});
    }

    #[test]
    fn ternary_operations_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;
                fn get_int() -> i32;
                fn take_int(x: i32);
                fn test() {
                    // TEST: should produce a proper diamond-shaped cfg
                    let num = get_bool() ? get_int() : 3;
                    take_int(num);
                }
            "});
    }

    #[test]
    fn string_literal_escapes_generate() {
        cg_snapshot_test!(indoc! {r#"
                fn test() {
                    // TEST: should properly generate \xNN for each escape
                    let x = "\n\r\t\\\"\x41\0";
                }
            "#});
    }

    /// Tests to ensure non-decimal integer literals
    /// 1. don't panic
    /// 2. are valid.
    ///
    /// <https://github.com/zirco-lang/zrc/issues/119>
    #[test]
    fn regression_119_special_integer_literals() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let a = 0b10_10;
                    let b = 0x1F_A4;
                }
            "});
    }

    #[test]
    fn pointer_arithmetic_generates_proper_gep() {
        cg_snapshot_test!(indoc! {r#"
                fn test() {
                    let x: *i32;

                    // TEST: should create a GEP that is the same as "x[4]"
                    let y = x + 4 as usize;
                    // TEST: and the same, with -4:
                    let z = x - 4 as usize;
                }
            "#});
    }

    #[test]
    fn arithmetic_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_int() -> i32;
                fn get_uint() -> u32;

                fn test() {
                    let sx = get_int();
                    let sy = get_int();
                    let ux = get_uint();
                    let uy = get_uint();

                    // TEST: should create an `add i32` instruction
                    let s_add = sx + sy;
                    let u_add = ux + uy;

                    // TEST: should create a `sub i32` instruction
                    let s_sub = sx - sy;
                    let u_sub = ux - uy;

                    // TEST: should create a `mul i32` instruction
                    let s_mul = sx * sy;
                    let u_mul = ux * uy;

                    // TEST: should create a `sdiv i32` instruction
                    let s_div = sx / sy;
                    // TEST: should create a `udiv i32` instruction
                    let u_div = ux / uy;

                    // TEST: should create a `srem i32` instruction
                    let s_rem = sx % sy;
                    // TEST: should create a `urem i32` instruction
                    let u_rem = ux % uy;
                }
            "});
    }

    #[test]
    fn equality_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;

                fn test() {
                    let a = get_bool();
                    let b = get_bool();

                    // TEST: should create an `icmp eq` instruction
                    let eq = a == b;

                    // TEST: should create an `icmp ne` instruction
                    let ne = a != b;
                }
            "});
    }

    #[test]
    fn logical_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;

                fn test() {
                    let a = get_bool();
                    let b = get_bool();

                    // TEST: should create a bit AND
                    let and = a && b;

                    // TEST: should create a bit OR
                    let or = a || b;

                    // TEST: should create a bit NOT
                    let not = !a;
                }
            "});
    }

    #[test]
    fn bitwise_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_int() -> i32;
                fn get_uint() -> u32;

                fn test() {
                    let x = get_int();
                    let y = get_int();
                    let u = get_uint();

                    // TEST: should create a `not i32` instruction
                    let not = ~x;

                    // TEST: should create a `and i32` instruction
                    let and = x & y;

                    // TEST: should create a `or i32` instruction
                    let or = x | y;

                    // TEST: should create a `xor i32` instruction
                    let xor = x ^ y;

                    // TEST: should create a `shl i32` instruction
                    let shl = x << u;

                    // TEST: should create a `lshr i32` instruction
                    let lshr = u >> u;

                    // TEST: should create a `ashr i32` instruction (as the lhs is signed)
                    let ashr = x >> u;
                }
            "});
    }
}
