use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::StringRadix,
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

pub fn cg_place<'ctx, 'a>(
    ctx: &'ctx Context,
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
            let (x, bb) = cg_expr(ctx, builder, module, function, bb, scope, *x);

            (x.into_pointer_value(), bb)
        }

        PlaceKind::Index(ptr, idx) => {
            let (ptr, bb) = cg_expr(ctx, builder, module, function, bb, scope, *ptr);
            let (idx, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *idx);

            let reg = unsafe {
                builder.build_gep(
                    llvm_basic_type(ctx, place.0),
                    ptr.into_pointer_value(),
                    &[idx.into_int_value()],
                    "gep",
                )
            };

            (reg.unwrap().as_basic_value_enum().into_pointer_value(), bb)
        }

        PlaceKind::Dot(x, prop) => {
            let prop_idx = match &x.0 {
                Type::Struct(fields) => fields
                    .iter()
                    .position(|(k, _)| *k == prop)
                    .expect("invalid struct field"),
                _ => panic!("invalid struct field"),
            };

            let (x, bb) = cg_place(ctx, builder, module, function, bb, scope, *x);

            let reg =
                builder.build_struct_gep(llvm_basic_type(ctx, place.0), x, prop_idx as u32, "gep");

            (reg.unwrap().as_basic_value_enum().into_pointer_value(), bb)
        }
    }
}

pub fn cg_expr<'ctx, 'a>(
    ctx: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    scope: &'a CgScope<'_, 'ctx>,
    expr: TypedExpr,
) -> (BasicValueEnum<'ctx>, BasicBlock<'ctx>) {
    match expr.1 {
        TypedExprKind::NumberLiteral(n) => (
            llvm_int_type(ctx, expr.0)
                .const_int_from_string(n, StringRadix::Decimal)
                .unwrap()
                .as_basic_value_enum(),
            *bb,
        ),

        TypedExprKind::StringLiteral(str) => {
            let formatted_contents = str
                .iter()
                .map(|x| match x {
                    StringTok::EscapedBackslash => format!("\\"),
                    StringTok::EscapedCr => format!("\r"),
                    StringTok::EscapedNewline => format!("\n"),
                    StringTok::EscapedHexByte(byte) => {
                        format!(
                            "{}",
                            char::from_u32(byte.parse::<u32>().expect("invalid byte"))
                                .expect("invalid char")
                        )
                    }
                    StringTok::EscapedDoubleQuote => format!("\""),
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

        TypedExprKind::BooleanLiteral(b) => (
            ctx.bool_type()
                .const_int(b as u64, false)
                .as_basic_value_enum(),
            *bb,
        ),

        TypedExprKind::Comma(lhs, rhs) => {
            let (_, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            cg_expr(ctx, builder, module, function, &bb, scope, *rhs)
        }

        TypedExprKind::Identifier(id) => {
            let place = cg_place(
                ctx,
                builder,
                module,
                function,
                bb,
                scope,
                Place(expr.0.clone(), PlaceKind::Variable(id)),
            );

            let reg = builder
                .build_load(llvm_basic_type(ctx, expr.0), place.0, "load")
                .unwrap();

            (reg.as_basic_value_enum(), place.1)
        }

        TypedExprKind::Assignment(place, value) => {
            let (value, bb) = cg_expr(ctx, builder, module, function, bb, scope, *value);
            let place = cg_place(ctx, builder, module, function, &bb, scope, *place);

            builder.build_store(place.0, value).unwrap();

            (value, bb)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            let (rhs, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *rhs);

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
            let (lhs, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            let (rhs, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *rhs);

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
            let (lhs, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            let (rhs, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *rhs);

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
            let (lhs, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            let (rhs, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *rhs);

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
            let (lhs, bb) = cg_expr(ctx, builder, module, function, bb, scope, *lhs);
            let (rhs, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *rhs);

            let reg = match op {
                Logical::And => {
                    builder.build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                }
                Logical::Or => builder.build_or(lhs.into_int_value(), rhs.into_int_value(), "or"),
            };

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryNot(x) => {
            let (x, bb) = cg_expr(ctx, builder, module, function, bb, scope, *x);

            let reg = builder.build_not(x.into_int_value(), "not");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let (x, bb) = cg_expr(ctx, builder, module, function, bb, scope, *x);

            let reg = builder.build_not(x.into_int_value(), "not");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryMinus(x) => {
            let (x, bb) = cg_expr(ctx, builder, module, function, bb, scope, *x);

            let reg = builder.build_int_neg(x.into_int_value(), "neg");

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let (x, bb) = cg_place(ctx, builder, module, function, bb, scope, *x);
            (x.as_basic_value_enum(), bb)
        }

        TypedExprKind::UnaryDereference(ptr) => {
            let (ptr, bb) = cg_expr(ctx, builder, module, function, bb, scope, *ptr);

            let reg = builder.build_load(
                llvm_basic_type(ctx, expr.0),
                ptr.into_pointer_value(),
                "load",
            );

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Index(ptr, idx) => {
            let (ptr, bb) = cg_expr(ctx, builder, module, function, bb, scope, *ptr);
            let (idx, bb) = cg_expr(ctx, builder, module, function, &bb, scope, *idx);

            let reg = unsafe {
                builder.build_gep(
                    llvm_basic_type(ctx, expr.0),
                    ptr.into_pointer_value(),
                    &[idx.into_int_value()],
                    "gep",
                )
            };

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Dot(place, key) => {
            let key_idx = match &place.0 {
                Type::Struct(fields) => fields
                    .iter()
                    .position(|(k, _)| *k == key)
                    .expect("invalid struct field"),
                _ => panic!("invalid struct field"),
            };

            let (place, bb) = cg_place(ctx, builder, module, function, bb, scope, *place);

            let reg = builder.build_struct_gep(
                llvm_basic_type(ctx, expr.0),
                place,
                key_idx as u32,
                "gep",
            );

            (reg.unwrap().as_basic_value_enum(), bb)
        }

        TypedExprKind::Call(f, args) => {
            let old_f = f.clone();

            // will always be a function pointer
            let (f, bb) = cg_place(ctx, builder, module, function, bb, scope, *f);

            let mut bb = bb;
            let old_args = args;
            let mut args = vec![];
            for arg in old_args {
                let (new_arg, new_bb) = cg_expr(ctx, builder, module, function, &bb, scope, arg);
                bb = new_bb;
                args.push(new_arg.into());
            }

            let ret = builder
                .build_indirect_call(
                    llvm_type(ctx, old_f.0).into_function_type(),
                    f,
                    &args,
                    "call",
                )
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
            let (cond, _) = cg_expr(ctx, builder, module, function, bb, scope, *cond);

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

            builder
                .build_conditional_branch(cond.into_int_value(), if_true_bb, if_false_bb)
                .unwrap();

            let (if_true, if_true_bb) =
                cg_expr(ctx, builder, module, function, &if_true_bb, scope, *lhs);
            let (if_false, if_false_bb) =
                cg_expr(ctx, builder, module, function, &if_false_bb, scope, *rhs);

            let end_bb = ctx.append_basic_block(*function, "end");

            builder.position_at_end(if_true_bb);
            builder.build_unconditional_branch(end_bb).unwrap();
            builder.position_at_end(if_false_bb);
            builder.build_unconditional_branch(end_bb).unwrap();

            builder.position_at_end(end_bb);
            let result_reg = builder
                .build_phi(llvm_basic_type(ctx, expr.0), "yield")
                .unwrap();
            result_reg.add_incoming(&[(&if_true, if_true_bb), (&if_false, if_false_bb)]);
            (result_reg.as_basic_value(), end_bb)
        }
        TypedExprKind::Cast(x, t) => {
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

            let (x, bb) = cg_expr(ctx, builder, module, function, bb, scope, *x);

            let reg = match (x.get_type().is_pointer_type(), matches!(t, Type::Ptr(_))) {
                (true, true) => builder
                    .build_bitcast(x.into_pointer_value(), llvm_basic_type(ctx, t), "cast")
                    .unwrap(),
                (true, false) => builder
                    .build_ptr_to_int(x.into_pointer_value(), llvm_int_type(ctx, t), "cast")
                    .unwrap()
                    .as_basic_value_enum(),
                (false, true) => builder
                    .build_int_to_ptr(
                        x.into_int_value(),
                        llvm_basic_type(ctx, t).into_pointer_type(),
                        "cast",
                    )
                    .unwrap()
                    .as_basic_value_enum(),
                (false, false) => builder
                    .build_bitcast(x.into_int_value(), llvm_basic_type(ctx, t), "cast")
                    .unwrap()
                    .as_basic_value_enum(),
            };

            (reg, bb)
        }
    }
}
