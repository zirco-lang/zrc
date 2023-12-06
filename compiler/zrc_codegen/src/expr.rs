//! Code generation for expressions

use inkwell::{
    basic_block::BasicBlock,
    debug_info::{AsDIScope, DILexicalBlock},
    types::{BasicType, StringRadix},
    values::{BasicValue, BasicValueEnum, PointerValue},
    IntPredicate,
};
use zrc_typeck::tast::{
    expr::{
        Arithmetic, BinaryBitwise, Comparison, Equality, Logical, NumberLiteral, Place, PlaceKind,
        StringTok, TypedExpr, TypedExprKind,
    },
    ty::Type,
};
use zrc_utils::span::Spannable;

use super::CgScope;
use crate::{
    ty::{llvm_basic_type, llvm_int_type, llvm_type},
    BasicBlockAnd, CgContext,
};

/// Resolve a place to its pointer
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn cg_place<'ctx, 'a>(
    cg: CgContext<'ctx, 'a>,
    bb: BasicBlock<'ctx>,
    scope: &'a CgScope<'_, 'ctx>,
    dbg_scope: DILexicalBlock<'ctx>,
    place: Place,
) -> BasicBlockAnd<'ctx, PointerValue<'ctx>> {
    let place_span = place.kind.span();
    let line_and_col = cg.line_lookup.lookup_from_index(place_span.start());
    let debug_location = cg.dbg_builder.create_debug_location(
        cg.ctx,
        line_and_col.line,
        line_and_col.col,
        dbg_scope.as_debug_info_scope(),
        None,
    );
    cg.builder.set_current_debug_location(debug_location);

    match place.kind.into_value() {
        PlaceKind::Variable(x) => {
            let reg = scope
                .get(x)
                .expect("identifier that passed typeck should exist in the CgScope");

            BasicBlockAnd { bb, value: reg }
        }

        PlaceKind::Deref(x) => {
            let BasicBlockAnd { bb, value } = cg_expr(cg, bb, scope, dbg_scope, *x);

            BasicBlockAnd {
                bb,
                value: value.into_pointer_value(),
            }
        }

        PlaceKind::Index(ptr, idx) => {
            let BasicBlockAnd { bb, value: ptr } = cg_expr(cg, bb, scope, dbg_scope, *ptr);
            let BasicBlockAnd { bb, value: idx } = cg_expr(cg, bb, scope, dbg_scope, *idx);

            // SAFETY: If indices are used incorrectly this may segfault
            // TODO: Is this actually safely used?
            let reg = unsafe {
                cg.builder.build_gep(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &place.inferred_type,
                    )
                    .0,
                    ptr.into_pointer_value(),
                    &[idx.into_int_value()],
                    "gep",
                )
            }
            .expect("building GEP instruction should succeed");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum().into_pointer_value(),
            }
        }

        #[allow(clippy::wildcard_enum_match_arm)]
        PlaceKind::Dot(x, prop) => match &x.inferred_type {
            Type::Struct(contents) => {
                let x_ty = llvm_basic_type(
                    cg.compilation_unit.get_file(),
                    cg.dbg_builder,
                    cg.ctx,
                    cg.target_machine,
                    &x.inferred_type,
                )
                .0;
                let prop_idx = contents
                    .iter()
                    .position(|(got_key, _)| *got_key == prop.into_value())
                    .expect("invalid struct field");

                let BasicBlockAnd { bb, value: x } = cg_place(cg, bb, scope, dbg_scope, *x);

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

                BasicBlockAnd {
                    bb,
                    value: reg.as_basic_value_enum().into_pointer_value(),
                }
            }
            Type::Union(_) => {
                // All we need to do is cast the pointer, but there's no `bitcast` anymore,
                // so just return it and it'll take on the correct type
                let BasicBlockAnd { bb, value } = cg_place(cg, bb, scope, dbg_scope, *x);

                BasicBlockAnd { bb, value }
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
    clippy::too_many_arguments
)]
pub(crate) fn cg_expr<'ctx, 'a>(
    cg: CgContext<'ctx, 'a>,
    bb: BasicBlock<'ctx>,
    scope: &'a CgScope<'_, 'ctx>,
    dbg_scope: DILexicalBlock<'ctx>,
    expr: TypedExpr,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let expr_span = expr.kind.span();
    let line_and_col = cg.line_lookup.lookup_from_index(expr_span.start());
    let debug_location = cg.dbg_builder.create_debug_location(
        cg.ctx,
        line_and_col.line,
        line_and_col.col,
        dbg_scope.as_debug_info_scope(),
        None,
    );
    cg.builder.set_current_debug_location(debug_location);

    match expr.kind.into_value() {
        TypedExprKind::NumberLiteral(n) => {
            let no_underscores = match n {
                NumberLiteral::Decimal(x) => x.replace('_', ""),
                NumberLiteral::Binary(x) => x.replace('_', ""),
                NumberLiteral::Hexadecimal(x) => x.replace('_', ""),
            };

            BasicBlockAnd {
                bb,
                value: llvm_int_type(cg.dbg_builder, cg.ctx, &expr.inferred_type)
                    .0
                    .const_int_from_string(
                        &no_underscores,
                        match n {
                            NumberLiteral::Decimal(_) => StringRadix::Decimal,
                            NumberLiteral::Binary(_) => StringRadix::Binary,
                            NumberLiteral::Hexadecimal(_) => StringRadix::Hexadecimal,
                        },
                    )
                    .expect("number literal should have parsed correctly")
                    .as_basic_value_enum(),
            }
        }

        TypedExprKind::StringLiteral(str) => {
            let formatted_contents = str
                .iter()
                .map(|x| match x {
                    StringTok::EscapedBackslash => "\\".to_string(),
                    StringTok::EscapedCr => "\r".to_string(),
                    StringTok::EscapedNewline => "\n".to_string(),
                    StringTok::EscapedTab => "\t".to_string(),
                    StringTok::EscapedNull => "\0".to_string(),
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

            BasicBlockAnd {
                bb,
                value: cg
                    .builder
                    .build_global_string_ptr(&formatted_contents, "str")
                    .expect("string should have built successfully")
                    .as_basic_value_enum(),
            }
        }
        TypedExprKind::CharLiteral(str) => {
            let [ch] = str.as_slice() else {
                panic!("Char literal must be exactly one character")
            };

            #[allow(clippy::as_conversions)]
            BasicBlockAnd {
                bb,
                value: cg
                    .ctx
                    .i8_type()
                    .const_int(
                        match ch {
                            StringTok::EscapedBackslash => '\\',
                            StringTok::EscapedCr => '\r',
                            StringTok::EscapedNewline => '\n',
                            StringTok::EscapedTab => '\t',
                            StringTok::EscapedNull => '\0',
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
                                text.chars()
                                    .next()
                                    .expect("char literal should have a first character")
                            }
                        } as u64,
                        false,
                    )
                    .as_basic_value_enum(),
            }
        }

        TypedExprKind::BooleanLiteral(value) => BasicBlockAnd {
            bb,
            value: cg
                .ctx
                .bool_type()
                .const_int(value.into(), false)
                .as_basic_value_enum(),
        },

        TypedExprKind::Comma(lhs, rhs) => {
            let BasicBlockAnd { bb, .. } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            cg_expr(cg, bb, scope, dbg_scope, *rhs)
        }

        TypedExprKind::Identifier(id) => {
            let BasicBlockAnd { bb, value: place } = cg_place(
                cg,
                bb,
                scope,
                dbg_scope,
                Place {
                    inferred_type: expr.inferred_type.clone(),
                    kind: PlaceKind::Variable(id).in_span(expr_span),
                },
            );

            let reg = cg
                .builder
                .build_load(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &expr.inferred_type,
                    )
                    .0,
                    place,
                    "load",
                )
                .expect("ident load should have built successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::Assignment(place, value) => {
            let BasicBlockAnd { bb, value } = cg_expr(cg, bb, scope, dbg_scope, *value);
            let BasicBlockAnd { bb, value: place } = cg_place(cg, bb, scope, dbg_scope, *place);

            cg.builder
                .build_store(place, value)
                .expect("store instruction in assignment should have built successfully");

            BasicBlockAnd { bb, value }
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let BasicBlockAnd { bb, value: lhs } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            let BasicBlockAnd { bb, value: rhs } = cg_expr(cg, bb, scope, dbg_scope, *rhs);

            let reg = match op {
                BinaryBitwise::And => {
                    cg.builder
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                }
                BinaryBitwise::Or => {
                    cg.builder
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
                }
                BinaryBitwise::Xor => {
                    cg.builder
                        .build_xor(lhs.into_int_value(), rhs.into_int_value(), "xor")
                }
                BinaryBitwise::Shl => {
                    cg.builder
                        .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "shl")
                }
                BinaryBitwise::Shr => cg.builder.build_right_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    expr.inferred_type.is_signed_integer(),
                    "shr",
                ),
            }
            .expect("binary bitwise operation should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::Equality(op, lhs, rhs) => {
            let BasicBlockAnd { bb, value: lhs } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            let BasicBlockAnd { bb, value: rhs } = cg_expr(cg, bb, scope, dbg_scope, *rhs);

            let op = match op {
                Equality::Eq => IntPredicate::EQ,
                Equality::Neq => IntPredicate::NE,
            };

            let reg = cg
                .builder
                .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                .expect("equality comparison should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::Comparison(op, lhs, rhs) => {
            let BasicBlockAnd { bb, value: lhs } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            let BasicBlockAnd { bb, value: rhs } = cg_expr(cg, bb, scope, dbg_scope, *rhs);

            let op = match (op, expr.inferred_type.is_signed_integer()) {
                (Comparison::Lt, true) => IntPredicate::SLT,
                (Comparison::Lt, false) => IntPredicate::ULT,
                (Comparison::Gt, true) => IntPredicate::SGT,
                (Comparison::Gt, false) => IntPredicate::UGT,
                (Comparison::Lte, true) => IntPredicate::SLE,
                (Comparison::Lte, false) => IntPredicate::ULE,
                (Comparison::Gte, true) => IntPredicate::SGE,
                (Comparison::Gte, false) => IntPredicate::UGE,
            };

            let reg = cg
                .builder
                .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                .expect("comparison should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::Arithmetic(op, lhs, rhs) => {
            let lhs_ty = lhs.inferred_type.clone();
            let BasicBlockAnd { bb, value: lhs } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            let BasicBlockAnd { bb, value: rhs } = cg_expr(cg, bb, scope, dbg_scope, *rhs);

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
                            llvm_basic_type(
                                cg.compilation_unit.get_file(),
                                cg.dbg_builder,
                                cg.ctx,
                                cg.target_machine,
                                &pointee,
                            )
                            .0,
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
                                llvm_basic_type(
                                    cg.compilation_unit.get_file(),
                                    cg.dbg_builder,
                                    cg.ctx,
                                    cg.target_machine,
                                    &pointee,
                                )
                                .0,
                                lhs.into_pointer_value(),
                                &[rhs.into_int_value()],
                                "ptr_sub",
                            )
                        }
                    }
                    _ => panic!("invalid pointer arithmetic operation"),
                }
                .expect("pointer arithmetic should have compiled successfully");

                BasicBlockAnd {
                    bb,
                    value: reg.as_basic_value_enum(),
                }
            } else {
                let reg = match (op, expr.inferred_type.is_signed_integer()) {
                    (Arithmetic::Addition, _) => {
                        cg.builder
                            .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add")
                    }
                    (Arithmetic::Subtraction, _) => {
                        cg.builder
                            .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub")
                    }
                    (Arithmetic::Multiplication, _) => {
                        cg.builder
                            .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul")
                    }
                    (Arithmetic::Division, true) => cg.builder.build_int_signed_div(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "div",
                    ),
                    (Arithmetic::Division, false) => cg.builder.build_int_unsigned_div(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "div",
                    ),
                    (Arithmetic::Modulo, true) => cg.builder.build_int_signed_rem(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "rem",
                    ),
                    (Arithmetic::Modulo, false) => cg.builder.build_int_unsigned_rem(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "rem",
                    ),
                }
                .expect("arithmetic operation should have compiled successfully");

                BasicBlockAnd {
                    bb,
                    value: reg.as_basic_value_enum(),
                }
            }
        }

        TypedExprKind::Logical(op, lhs, rhs) => {
            let BasicBlockAnd { bb, value: lhs } = cg_expr(cg, bb, scope, dbg_scope, *lhs);
            let BasicBlockAnd { bb, value: rhs } = cg_expr(cg, bb, scope, dbg_scope, *rhs);

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

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::UnaryNot(x) => {
            let BasicBlockAnd { bb, value } = cg_expr(cg, bb, scope, dbg_scope, *x);

            let reg = cg
                .builder
                .build_not(value.into_int_value(), "not")
                .expect("not should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let BasicBlockAnd { bb, value } = cg_expr(cg, bb, scope, dbg_scope, *x);

            let reg = cg
                .builder
                .build_not(value.into_int_value(), "not")
                .expect("not should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::UnaryMinus(x) => {
            let BasicBlockAnd { bb, value } = cg_expr(cg, bb, scope, dbg_scope, *x);

            let reg = cg
                .builder
                .build_int_neg(value.into_int_value(), "neg")
                .expect("negation should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let BasicBlockAnd { bb, value } = cg_place(cg, bb, scope, dbg_scope, *x);

            BasicBlockAnd {
                bb,
                value: value.as_basic_value_enum(),
            }
        }

        TypedExprKind::UnaryDereference(ptr) => {
            let BasicBlockAnd { bb, value: ptr } = cg_expr(cg, bb, scope, dbg_scope, *ptr);

            let reg = cg
                .builder
                .build_load(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &expr.inferred_type,
                    )
                    .0,
                    ptr.into_pointer_value(),
                    "load",
                )
                .expect("dereference should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: reg.as_basic_value_enum(),
            }
        }

        TypedExprKind::Index(ptr, idx) => {
            let BasicBlockAnd { bb, value: ptr } = cg_place(
                cg,
                bb,
                scope,
                dbg_scope,
                Place {
                    inferred_type: expr.inferred_type.clone(),
                    kind: PlaceKind::Index(ptr, idx).in_span(expr_span),
                },
            );

            let loaded = cg
                .builder
                .build_load(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &expr.inferred_type,
                    )
                    .0,
                    ptr,
                    "load",
                )
                .expect("index load should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: loaded.as_basic_value_enum(),
            }
        }

        TypedExprKind::Dot(place, key) => {
            let BasicBlockAnd { bb, value: ptr } = cg_place(
                cg,
                bb,
                scope,
                dbg_scope,
                Place {
                    inferred_type: expr.inferred_type.clone(),
                    kind: PlaceKind::Dot(place, key).in_span(expr_span),
                },
            );

            let loaded = cg
                .builder
                .build_load(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &expr.inferred_type,
                    )
                    .0,
                    ptr,
                    "load",
                )
                .expect("dot load should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: loaded.as_basic_value_enum(),
            }
        }

        TypedExprKind::Call(f, args) => {
            let llvm_f_type = llvm_type(
                cg.compilation_unit.get_file(),
                cg.dbg_builder,
                cg.ctx,
                cg.target_machine,
                &f.inferred_type,
            )
            .0
            .into_function_type();

            // will always be a function pointer
            let BasicBlockAnd { bb, value: f_ptr } = cg_place(cg, bb, scope, dbg_scope, *f);

            let mut bb = bb;
            let old_args = args;
            let mut args = vec![];
            for arg in old_args {
                let BasicBlockAnd {
                    bb: new_bb,
                    value: new_arg,
                } = cg_expr(cg, bb, scope, dbg_scope, arg);
                bb = new_bb;
                args.push(new_arg.into());
            }

            let ret = cg
                .builder
                .build_indirect_call(llvm_f_type, f_ptr, &args, "call")
                .expect("call should have compiled successfully");

            BasicBlockAnd {
                bb,
                value: if ret.try_as_basic_value().is_left() {
                    ret.try_as_basic_value().unwrap_left()
                } else {
                    cg.ctx.i8_type().get_undef().as_basic_value_enum()
                },
            }
        }
        TypedExprKind::Ternary(cond, lhs, rhs) => {
            let BasicBlockAnd { value: cond, .. } = cg_expr(cg, bb, scope, dbg_scope, *cond);

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

            let if_true_bb = cg.ctx.append_basic_block(cg.fn_value, "if_true");
            let if_false_bb = cg.ctx.append_basic_block(cg.fn_value, "if_false");
            let end_bb = cg.ctx.append_basic_block(cg.fn_value, "end");

            cg.builder
                .build_conditional_branch(cond.into_int_value(), if_true_bb, if_false_bb)
                .expect("conditional branch should have been created successfully");

            cg.builder.position_at_end(if_true_bb);
            let BasicBlockAnd {
                bb: if_true_bb,
                value: if_true,
            } = cg_expr(cg, if_true_bb, scope, dbg_scope, *lhs);
            cg.builder
                .build_unconditional_branch(end_bb)
                .expect("unconditional branch should have been created successfully");

            cg.builder.position_at_end(if_false_bb);
            let BasicBlockAnd {
                bb: if_false_bb,
                value: if_false,
            } = cg_expr(cg, if_false_bb, scope, dbg_scope, *rhs);
            cg.builder
                .build_unconditional_branch(end_bb)
                .expect("unconditional branch should have been created successfully");

            cg.builder.position_at_end(end_bb);
            let result_reg = cg
                .builder
                .build_phi(
                    llvm_basic_type(
                        cg.compilation_unit.get_file(),
                        cg.dbg_builder,
                        cg.ctx,
                        cg.target_machine,
                        &expr.inferred_type,
                    )
                    .0,
                    "yield",
                )
                .expect("phi node should have been created successfully");
            result_reg.add_incoming(&[(&if_true, if_true_bb), (&if_false, if_false_bb)]);

            BasicBlockAnd {
                bb: end_bb,
                value: result_reg.as_basic_value(),
            }
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

            let BasicBlockAnd { bb, value: x } = cg_expr(cg, bb, scope, dbg_scope, *x);

            let reg = match (
                x.get_type().is_pointer_type(),
                matches!(ty.value(), Type::Ptr(_)),
            ) {
                (true, true) => cg
                    .builder
                    .build_bitcast(
                        x.into_pointer_value(),
                        llvm_basic_type(
                            cg.compilation_unit.get_file(),
                            cg.dbg_builder,
                            cg.ctx,
                            cg.target_machine,
                            ty.value(),
                        )
                        .0,
                        "cast",
                    )
                    .expect("bitcast should have compiled successfully"),
                (true, false) => cg
                    .builder
                    .build_ptr_to_int(
                        x.into_pointer_value(),
                        llvm_int_type(cg.dbg_builder, cg.ctx, ty.value()).0,
                        "cast",
                    )
                    .expect("ptrtoint should have compiled successfully")
                    .as_basic_value_enum(),
                (false, true) => cg
                    .builder
                    .build_int_to_ptr(
                        x.into_int_value(),
                        llvm_basic_type(
                            cg.compilation_unit.get_file(),
                            cg.dbg_builder,
                            cg.ctx,
                            cg.target_machine,
                            ty.value(),
                        )
                        .0
                        .into_pointer_type(),
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
                                llvm_basic_type(
                                    cg.compilation_unit.get_file(),
                                    cg.dbg_builder,
                                    cg.ctx,
                                    cg.target_machine,
                                    ty.value(),
                                )
                                .0
                                .into_int_type(),
                                "cast",
                            )
                            .expect("sext should have compiled successfully")
                            .as_basic_value_enum(),

                        // (x is signed, target is signed or unsigned)
                        (false, _) => cg
                            .builder
                            .build_int_z_extend(
                                x.into_int_value(),
                                llvm_basic_type(
                                    cg.compilation_unit.get_file(),
                                    cg.dbg_builder,
                                    cg.ctx,
                                    cg.target_machine,
                                    ty.value(),
                                )
                                .0
                                .into_int_type(),
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
                            llvm_basic_type(
                                cg.compilation_unit.get_file(),
                                cg.dbg_builder,
                                cg.ctx,
                                cg.target_machine,
                                ty.value(),
                            )
                            .0,
                            "cast",
                        )
                        .expect("bitcast should have compiled successfully")
                        .as_basic_value_enum()
                }
            };

            BasicBlockAnd { bb, value: reg }
        }
        TypedExprKind::SizeOf(ty) => {
            let reg = llvm_basic_type(
                cg.compilation_unit.get_file(),
                cg.dbg_builder,
                cg.ctx,
                cg.target_machine,
                &ty,
            )
            .0
            .size_of()
            .expect("size_of should have compiled successfully")
            .as_basic_value_enum();

            BasicBlockAnd { bb, value: reg }
        }
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
    mod cg_place {
        use super::*;

        #[test]
        fn basic_identifiers_in_place_position() {
            cg_snapshot_test!(indoc! {"
                fn test() {
                    let x = 6;

                    // TEST: we should simply be `store`ing to the %let_x we created
                    x = 7;

                    return;
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

                    return;
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

                    return;
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
                    x[4] = 5;

                    return;
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

                    return;
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

                    return;
                }
            "});
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

                    return;
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

        #[test]
        fn string_literal_escapes_generate() {
            cg_snapshot_test!(indoc! {r#"
                fn test() {
                    // TEST: should properly generate \xNN for each escape
                    let x = "\n\r\t\\\"\x41\0";

                    return;
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
                    return;
                }
            "});
        }

        #[test]
        fn pointer_arithmetic_generates_proper_gep() {
            cg_snapshot_test!(indoc! {r#"
                fn test() {
                    let x: *i32;

                    // TEST: should create a GEP that is the same as "x[4]"
                    let y = x + 4;
                    // TEST: and the same, with -4:
                    let z = x - 4;

                    return;
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

                    return;
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

                    return;
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

                    return;
                }
            "});
        }
    }
}
