//! Code generation for expressions

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::{BasicType, StringRadix},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate,
};
use zrc_typeck::tast::{
    expr::{
        Arithmetic, BinaryBitwise, Comparison, Equality, Logical, Place, PlaceKind, StringTok,
        TypedExpr, TypedExprKind,
    },
    ty::Type,
};

use super::CgScope;
use crate::ty::{llvm_basic_type, llvm_int_type, llvm_type};

/// Resolve a place to its pointer
#[allow(clippy::trivially_copy_pass_by_ref, clippy::too_many_arguments)]
fn cg_place<'ctx, 'a>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    scope: &'a CgScope<'_, 'ctx>,
    place: Place,
) -> (PointerValue<'ctx>, BasicBlock<'ctx>) {
    match place.1 {
        PlaceKind::Variable(x) => {
            let reg = scope.get(x).unwrap();

            (reg, *bb)
        }

        PlaceKind::Deref(x) => {
            let (x, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );

            (x.into_pointer_value(), bb)
        }

        PlaceKind::Index(ptr, idx) => {
            let (ptr, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *ptr,
            );
            let (idx, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *idx,
            );

            // SAFETY: If indices are used incorrectly this may segfault
            // TODO: Is this actually safely used?
            let reg = unsafe {
                builder.build_gep(
                    llvm_basic_type(ctx, target_machine, &place.0),
                    ptr.into_pointer_value(),
                    &[idx.into_int_value()],
                    "gep",
                )
            };

            (reg.unwrap().as_basic_value_enum().into_pointer_value(), bb)
        }

        #[allow(clippy::wildcard_enum_match_arm)]
        PlaceKind::Dot(x, prop) => match &(*x).0 {
            Type::Struct(contents) => {
                let x_ty = llvm_basic_type(ctx, target_machine, &x.0);
                let prop_idx = contents
                    .iter()
                    .position(|(got_key, _)| *got_key == prop)
                    .expect("invalid struct field");

                let (x, bb) = cg_place(
                    ctx,
                    target_machine,
                    builder,
                    module,
                    function,
                    bb,
                    scope,
                    *x,
                );

                let reg = builder.build_struct_gep(
                    x_ty,
                    x,
                    prop_idx
                        .try_into()
                        .expect("got more than u32::MAX as key index? HOW?"),
                    "gep",
                );

                (reg.unwrap().as_basic_value_enum().into_pointer_value(), bb)
            }
            Type::Union(_) => {
                // All we need to do is cast the pointer, but there's no `bitcast` anymore,
                // so just return it and it'll take on the correct type
                let (x, bb) = cg_place(
                    ctx,
                    target_machine,
                    builder,
                    module,
                    function,
                    bb,
                    scope,
                    *x,
                );

                (x, bb)
            }
            _ => panic!("cannot access property of non-struct"),
        },
    }
}

/// Generate an expression, yielding its result.
#[allow(
    clippy::redundant_pub_crate,
    clippy::too_many_lines,
    clippy::match_same_arms,
    clippy::trivially_copy_pass_by_ref,
    clippy::too_many_arguments
)]
pub(crate) fn cg_expr<'ctx, 'a>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    scope: &'a CgScope<'_, 'ctx>,
    expr: TypedExpr,
) -> (BasicValueEnum<'ctx>, BasicBlock<'ctx>) {
    match expr.1 {
        TypedExprKind::NumberLiteral(n) => (
            llvm_int_type(ctx, &expr.0)
                .const_int_from_string(n, StringRadix::Decimal)
                .unwrap()
                .as_basic_value_enum(),
            *bb,
        ),

        TypedExprKind::StringLiteral(str) => {
            let formatted_contents = str
                .iter()
                .map(|x| match x {
                    StringTok::EscapedBackslash => "\\".to_string(),
                    StringTok::EscapedCr => "\r".to_string(),
                    StringTok::EscapedNewline => "\n".to_string(),
                    StringTok::EscapedHexByte(byte) => {
                        format!(
                            "{}",
                            char::from_u32(byte.parse::<u32>().expect("invalid byte"))
                                .expect("invalid char")
                        )
                    }
                    StringTok::EscapedDoubleQuote => "\"".to_string(),
                    StringTok::Text(text) => (*text).to_string(),
                })
                .collect::<String>();

            (
                builder
                    .build_global_string_ptr(&formatted_contents, "str")
                    .unwrap()
                    .as_basic_value_enum(),
                *bb,
            )
        }
        TypedExprKind::CharLiteral(str) => {
            let [ch] = str.as_slice() else {
                panic!("Char literal must be exactly one character")
            };

            #[allow(clippy::as_conversions)]
            (
                ctx.i8_type()
                    .const_int(
                        match ch {
                            StringTok::EscapedBackslash => '\\',
                            StringTok::EscapedCr => '\r',
                            StringTok::EscapedNewline => '\n',
                            StringTok::EscapedHexByte(byte) => {
                                char::from_u32(byte.parse::<u32>().expect("invalid byte"))
                                    .expect("invalid char")
                            }
                            StringTok::EscapedDoubleQuote => '"',
                            StringTok::Text(text) => {
                                assert!(
                                    text.len() == 1,
                                    "Char literal must be exactly one character"
                                );
                                text.chars().next().unwrap()
                            }
                        } as u64,
                        false,
                    )
                    .as_basic_value_enum(),
                *bb,
            )
        }

        TypedExprKind::BooleanLiteral(value) => (
            ctx.bool_type()
                .const_int(value.into(), false)
                .as_basic_value_enum(),
            *bb,
        ),

        TypedExprKind::Comma(lhs, rhs) => {
            let (_, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            )
        }

        TypedExprKind::Identifier(id) => {
            let place = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                Place(expr.0.clone(), PlaceKind::Variable(id)),
            );

            let reg = builder
                .build_load(
                    llvm_basic_type(ctx, target_machine, &expr.0),
                    place.0,
                    "load",
                )
                .unwrap();

            (reg.as_basic_value_enum(), place.1)
        }

        TypedExprKind::Assignment(place, value) => {
            let (value, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *value,
            );
            let place = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *place,
            );

            builder.build_store(place.0, value).unwrap();

            (value, bb)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            let (rhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            );

            let reg = match op {
                BinaryBitwise::And => {
                    builder.build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                }
                BinaryBitwise::Or => {
                    builder.build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
                }
                BinaryBitwise::Xor => {
                    builder.build_xor(lhs.into_int_value(), rhs.into_int_value(), "xor")
                }
                BinaryBitwise::Shl => {
                    builder.build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "shl")
                }
                BinaryBitwise::Shr => builder.build_right_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    expr.0.is_signed_integer(),
                    "shr",
                ),
            };

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Equality(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            let (rhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            );

            let op = match op {
                Equality::Eq => IntPredicate::EQ,
                Equality::Neq => IntPredicate::NE,
            };

            let reg = builder
                .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                .unwrap();

            (reg.as_basic_value_enum(), bb)
        }

        TypedExprKind::Comparison(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            let (rhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            );

            let op = match (op, expr.0.is_signed_integer()) {
                (Comparison::Lt, true) => IntPredicate::SLT,
                (Comparison::Lt, false) => IntPredicate::ULT,
                (Comparison::Gt, true) => IntPredicate::SGT,
                (Comparison::Gt, false) => IntPredicate::UGT,
                (Comparison::Lte, true) => IntPredicate::SLE,
                (Comparison::Lte, false) => IntPredicate::ULE,
                (Comparison::Gte, true) => IntPredicate::SGE,
                (Comparison::Gte, false) => IntPredicate::UGE,
            };

            let reg = builder
                .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                .unwrap();

            (reg.as_basic_value_enum(), bb)
        }

        TypedExprKind::Arithmetic(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            let (rhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            );

            let reg = match (op, expr.0.is_signed_integer()) {
                (Arithmetic::Addition, _) => {
                    builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add")
                }
                (Arithmetic::Subtraction, _) => {
                    builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub")
                }
                (Arithmetic::Multiplication, _) => {
                    builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul")
                }
                (Arithmetic::Division, true) => {
                    builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "div")
                }
                (Arithmetic::Division, false) => builder.build_int_unsigned_div(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "div",
                ),
                (Arithmetic::Modulo, true) => {
                    builder.build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "rem")
                }
                (Arithmetic::Modulo, false) => builder.build_int_unsigned_rem(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "rem",
                ),
            };

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Logical(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *lhs,
            );
            let (rhs, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                *rhs,
            );

            let reg = match op {
                Logical::And => {
                    builder.build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                }
                Logical::Or => builder.build_or(lhs.into_int_value(), rhs.into_int_value(), "or"),
            };

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryNot(x) => {
            let (x, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );

            let reg = builder.build_not(x.into_int_value(), "not");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let (x, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );

            let reg = builder.build_not(x.into_int_value(), "not");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryMinus(x) => {
            let (x, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );

            let reg = builder.build_int_neg(x.into_int_value(), "neg");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let (x, bb) = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );
            (x.as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryDereference(ptr) => {
            let (ptr, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *ptr,
            );

            let reg = builder.build_load(
                llvm_basic_type(ctx, target_machine, &expr.0),
                ptr.into_pointer_value(),
                "load",
            );

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Index(ptr, idx) => {
            let (ptr, bb) = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                Place(expr.0.clone(), PlaceKind::Index(ptr, idx)),
            );

            let loaded = builder
                .build_load(llvm_basic_type(ctx, target_machine, &expr.0), ptr, "load")
                .unwrap();

            (loaded.as_basic_value_enum(), bb)
        }

        TypedExprKind::Dot(place, key) => {
            let (ptr, bb) = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                Place(expr.0.clone(), PlaceKind::Dot(place, key)),
            );

            let loaded = builder
                .build_load(llvm_basic_type(ctx, target_machine, &expr.0), ptr, "load")
                .unwrap();

            (loaded.as_basic_value_enum(), bb)
        }

        TypedExprKind::Call(f, args) => {
            let llvm_f_type = llvm_type(ctx, target_machine, &f.0).into_function_type();

            // will always be a function pointer
            let (f_ptr, bb) = cg_place(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *f,
            );

            let mut bb = bb;
            let old_args = args;
            let mut args = vec![];
            for arg in old_args {
                let (new_arg, new_bb) = cg_expr(
                    ctx,
                    target_machine,
                    builder,
                    module,
                    function,
                    &bb,
                    scope,
                    arg,
                );
                bb = new_bb;
                args.push(new_arg.into());
            }

            let ret = builder
                .build_indirect_call(llvm_f_type, f_ptr, &args, "call")
                .unwrap();

            (
                if ret.try_as_basic_value().is_left() {
                    ret.try_as_basic_value().unwrap_left()
                } else {
                    ctx.i8_type().get_undef().as_basic_value_enum()
                },
                bb,
            )
        }
        TypedExprKind::Ternary(cond, lhs, rhs) => {
            let (cond, _) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *cond,
            );

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

            let if_true_bb = ctx.append_basic_block(*function, "if_true");
            let if_false_bb = ctx.append_basic_block(*function, "if_false");
            let end_bb = ctx.append_basic_block(*function, "end");

            builder
                .build_conditional_branch(cond.into_int_value(), if_true_bb, if_false_bb)
                .unwrap();

            builder.position_at_end(if_true_bb);
            let (if_true, if_true_bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &if_true_bb,
                scope,
                *lhs,
            );
            builder.build_unconditional_branch(end_bb).unwrap();

            builder.position_at_end(if_false_bb);
            let (if_false, if_false_bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &if_false_bb,
                scope,
                *rhs,
            );
            builder.build_unconditional_branch(end_bb).unwrap();

            builder.position_at_end(end_bb);
            let result_reg = builder
                .build_phi(llvm_basic_type(ctx, target_machine, &expr.0), "yield")
                .unwrap();
            result_reg.add_incoming(&[(&if_true, if_true_bb), (&if_false, if_false_bb)]);
            (result_reg.as_basic_value(), end_bb)
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

            let x_ty_is_signed_integer = (*x).0.is_signed_integer();

            let (x, bb) = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                bb,
                scope,
                *x,
            );

            let reg = match (x.get_type().is_pointer_type(), matches!(ty, Type::Ptr(_))) {
                (true, true) => builder
                    .build_bitcast(
                        x.into_pointer_value(),
                        llvm_basic_type(ctx, target_machine, &ty),
                        "cast",
                    )
                    .unwrap(),
                (true, false) => builder
                    .build_ptr_to_int(x.into_pointer_value(), llvm_int_type(ctx, &ty), "cast")
                    .unwrap()
                    .as_basic_value_enum(),
                (false, true) => builder
                    .build_int_to_ptr(
                        x.into_int_value(),
                        llvm_basic_type(ctx, target_machine, &ty).into_pointer_type(),
                        "cast",
                    )
                    .unwrap()
                    .as_basic_value_enum(),
                (false, false) if x.get_type().is_int_type() && ty.is_integer() => {
                    // Cast between two integers
                    match (x_ty_is_signed_integer, ty.is_signed_integer()) {
                        // (x is signed, target is signed or unsigned)
                        (true, _) => builder
                            .build_int_s_extend(
                                x.into_int_value(),
                                llvm_basic_type(ctx, target_machine, &ty).into_int_type(),
                                "cast",
                            )
                            .unwrap()
                            .as_basic_value_enum(),

                        // (x is signed, target is signed or unsigned)
                        (false, _) => builder
                            .build_int_z_extend(
                                x.into_int_value(),
                                llvm_basic_type(ctx, target_machine, &ty).into_int_type(),
                                "cast",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                    }
                }
                (false, false) => {
                    // Other casts are just bitcasts
                    builder
                        .build_bitcast(
                            x.into_int_value(),
                            llvm_basic_type(ctx, target_machine, &ty),
                            "cast",
                        )
                        .unwrap()
                        .as_basic_value_enum()
                }
            };

            (reg, bb)
        }
        TypedExprKind::SizeOf(ty) => {
            let reg = llvm_basic_type(ctx, target_machine, &ty)
                .size_of()
                .unwrap()
                .as_basic_value_enum();

            (reg, *bb)
        }
    }
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indexmap::IndexMap;
    use indoc::indoc;
    use inkwell::{
        context::Context, targets::TargetTriple, types::BasicType, values::AnyValue, AddressSpace,
    };
    use zrc_parser::parser::{parse_expr, parse_program};
    use zrc_typeck::{
        tast::stmt::ArgumentDeclarationList,
        typeck,
        typeck::{BlockReturnType, Scope},
    };

    use super::*;
    use crate::{
        cg_program_to_string, cg_snapshot_test, get_native_triple,
        test_utils::make_test_prelude_closure,
    };

    // Remember: In all of these tests, cg_place returns a *pointer* to the data in
    // the place.
    mod cg_place {
        use super::*;

        /// When generating an identifier, the pointer to their data is stored
        /// already within the [`CgScope`] instance. There is no need to
        /// do any extra work to generate the pointer other than return
        /// the allocation directly.
        // #[test]
        // TODO: rewrite to snaps
        fn identifier_registers_are_returned_as_is() {
            let ctx = Context::create();

            let generate_test_prelude = make_test_prelude_closure(
                |ctx, _target_machine, builder, _module, _fn_value, scope, bb| {
                    let x_stack_ptr = builder.build_alloca(ctx.i32_type(), "x").unwrap();
                    scope.insert("x", x_stack_ptr);

                    *bb
                },
            );

            let expected = {
                let (_target_machine, _builder, module, _fn_value, scope, _bb) =
                    generate_test_prelude(&ctx);

                (
                    module.print_to_string(),
                    scope
                        .get("x")
                        .unwrap()
                        .as_basic_value_enum()
                        .print_to_string(),
                )
            };

            let actual = {
                let (target_machine, builder, module, fn_value, scope, bb) =
                    generate_test_prelude(&ctx);

                let (ptr, _bb) = cg_place(
                    &ctx,
                    &target_machine,
                    &builder,
                    &module,
                    &fn_value,
                    &bb,
                    &scope,
                    Place(Type::I32, PlaceKind::Variable("x")),
                );

                (
                    module.print_to_string(),
                    ptr.as_basic_value_enum().print_to_string(),
                )
            };

            assert_eq!(actual, expected);
        }

        /// When dereferencing an identifier, the identifier itself represents a
        /// `*T`, and if we consider the fact that the value is stored
        /// on the stack, `%x` is of type `T**`. To get the underlying
        /// pointer to `T` (because cg place returns a pointer) `T*`, we need to
        /// `load` the identifier only.
        // #[test]
        // TODO: rewrite to snaps
        fn identifier_deref_generates_as_expected() {
            let ctx = Context::create();

            let generate_test_prelude = make_test_prelude_closure(
                |ctx, _target_machine, builder, _module, _fn_value, scope, bb| {
                    // generates %x = alloca i32* and scope mapping x -> %x
                    let x_stack_ptr = builder
                        .build_alloca(ctx.i32_type().ptr_type(AddressSpace::default()), "x")
                        .unwrap();
                    scope.insert("x", x_stack_ptr);

                    *bb
                },
            );

            let expected = {
                let (_target_machine, builder, module, _fn_value, scope, _bb) =
                    generate_test_prelude(&ctx);

                // Expect a single %yield = load i32*, i32** %x
                let yield_ptr = builder
                    .build_load(
                        ctx.i32_type().ptr_type(AddressSpace::default()),
                        scope.get("x").unwrap(),
                        "load",
                    )
                    .unwrap();

                (
                    module.print_to_string(),
                    // expected result of cg_place:
                    yield_ptr.print_to_string(),
                )
            };

            let actual = {
                let (target_machine, builder, module, fn_value, scope, bb) =
                    generate_test_prelude(&ctx);

                let (ptr, _bb) = cg_place(
                    &ctx,
                    &target_machine,
                    &builder,
                    &module,
                    &fn_value,
                    &bb,
                    &scope,
                    Place(
                        Type::I32,
                        PlaceKind::Deref(Box::new(TypedExpr(
                            Type::Ptr(Box::new(Type::I32)),
                            TypedExprKind::Identifier("x"),
                        ))),
                    ),
                );

                (
                    module.print_to_string(),
                    ptr.as_basic_value_enum().print_to_string(),
                )
            };

            assert_eq!(actual, expected);
        }

        /// When dereferencing a value that's not an identifier in place context
        /// e.g. `*0`, the address to this data is `&*0` which is just
        /// `0`. In this case, it gets cancelled out. We assert that the
        /// result is just `0 as *i32` when generating `*(0 as *i32)` in place
        /// context.
        // #[test]
        // TODO: rewrite to snaps
        fn other_deref_generates_as_expected() {
            let ctx = Context::create();

            let generate_test_prelude = make_test_prelude_closure(
                |_ctx, _target_machine, _builder, _module, _fn_value, _scope, bb| *bb,
            );

            let expected = {
                let (_target_machine, builder, module, _fn_value, _scope, _bb) =
                    generate_test_prelude(&ctx);

                // Generates `%cast = inttoptr 0 to i32*`
                let cast = builder
                    .build_int_to_ptr(
                        ctx.i32_type().const_zero(),
                        ctx.i32_type().ptr_type(AddressSpace::default()),
                        "cast",
                    )
                    .unwrap();

                // The yielded value is just %cast -- see the comment at the top
                (module.print_to_string(), cast.print_to_string())
            };

            let actual = {
                let (target_machine, builder, module, fn_value, scope, bb) =
                    generate_test_prelude(&ctx);

                let (ptr, _bb) = cg_place(
                    &ctx,
                    &target_machine,
                    &builder,
                    &module,
                    &fn_value,
                    &bb,
                    &scope,
                    Place(
                        Type::I32,
                        PlaceKind::Deref(Box::new(TypedExpr(
                            Type::Ptr(Box::new(Type::I32)),
                            TypedExprKind::Cast(
                                Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("0"))),
                                Type::Ptr(Box::new(Type::I32)),
                            ),
                        ))),
                    ),
                );

                (
                    module.print_to_string(),
                    ptr.as_basic_value_enum().print_to_string(),
                )
            };

            assert_eq!(actual, expected);
        }

        // #[test]
        // TODO: rewrite to snaps
        fn pointer_indexing_generates_proper_gep() {
            let ctx = Context::create();

            let generate_test_prelude = make_test_prelude_closure(
                |ctx, _target_machine, builder, _module, _fn_value, scope, bb| {
                    // generate `arr: *i32`
                    let arr_stack_ptr = builder
                        .build_alloca(ctx.i32_type().ptr_type(AddressSpace::default()), "arr")
                        .unwrap();
                    scope.insert("arr", arr_stack_ptr);

                    *bb
                },
            );

            let expected = {
                let (_target_machine, builder, module, _fn_value, scope, _bb) =
                    generate_test_prelude(&ctx);

                // First the `i32*` from `i32** %arr` is loaded, then a `gep` instruction gets
                // the pointer to that index in the array
                let loaded_arr = builder
                    .build_load(
                        ctx.i32_type().ptr_type(AddressSpace::default()),
                        scope.get("arr").unwrap(),
                        "load",
                    )
                    .unwrap();

                // SAFETY: If indices are used incorrectly this may segfault
                let indexed_ptr = unsafe {
                    builder.build_gep(
                        ctx.i32_type(),
                        loaded_arr.into_pointer_value(),
                        &[ctx.i32_type().const_int(3, false)],
                        "gep",
                    )
                }
                .unwrap();

                (module.print_to_string(), indexed_ptr.print_to_string())
            };

            let actual = {
                let (target_machine, builder, module, fn_value, scope, bb) =
                    generate_test_prelude(&ctx);

                let (ptr, _bb) = cg_place(
                    &ctx,
                    &target_machine,
                    &builder,
                    &module,
                    &fn_value,
                    &bb,
                    &scope,
                    Place(
                        Type::I32,
                        PlaceKind::Index(
                            Box::new(TypedExpr(
                                Type::Ptr(Box::new(Type::I32)),
                                TypedExprKind::Identifier("arr"),
                            )),
                            Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("3"))),
                        ),
                    ),
                );

                (module.print_to_string(), ptr.print_to_string())
            };

            assert_eq!(actual, expected);
        }

        // #[test]
        // TODO: rewrite to snaps
        fn struct_property_access_generates_proper_gep() {
            let ctx = Context::create();

            let generate_test_prelude = make_test_prelude_closure(
                |ctx, _target_machine, builder, _module, _fn_value, scope, bb| {
                    // generate: `x: { x: i32, y: i32 }`
                    let x_stack_ptr = builder
                        .build_alloca(
                            ctx.struct_type(
                                &[
                                    ctx.i32_type().as_basic_type_enum(),
                                    ctx.i32_type().as_basic_type_enum(),
                                ],
                                false,
                            ),
                            "x",
                        )
                        .unwrap();
                    scope.insert("x", x_stack_ptr);

                    *bb
                },
            );

            let expected = {
                let (_target_machine, builder, module, _fn_value, scope, _bb) =
                    generate_test_prelude(&ctx);

                // We do not load the struct, we GEP directly into it.
                let indexed_ptr = builder
                    .build_struct_gep(
                        ctx.struct_type(
                            &[
                                ctx.i32_type().as_basic_type_enum(),
                                ctx.i32_type().as_basic_type_enum(),
                            ],
                            false,
                        ),
                        scope.get("x").unwrap(),
                        1,
                        "gep",
                    )
                    .unwrap();

                (module.print_to_string(), indexed_ptr.print_to_string())
            };

            let actual = {
                let (target_machine, builder, module, fn_value, scope, bb) =
                    generate_test_prelude(&ctx);

                let (ptr, _bb) = cg_place(
                    &ctx,
                    &target_machine,
                    &builder,
                    &module,
                    &fn_value,
                    &bb,
                    &scope,
                    Place(
                        Type::I32,
                        PlaceKind::Dot(
                            Box::new(Place(
                                Type::Struct(IndexMap::from([("a", Type::I32), ("b", Type::I32)])),
                                PlaceKind::Variable("x"),
                            )),
                            "b",
                        ),
                    ),
                );

                (module.print_to_string(), ptr.print_to_string())
            };

            assert_eq!(actual, expected);
        }
    }
    mod cg_expr {
        use super::*;

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
                    take_int(x[4]);

                    return;
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

                    return;
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
                    return;
                }
            "});
        }
    }
}
