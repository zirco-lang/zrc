use anyhow::{bail, Context as _};

use super::{cg_load, get_llvm_typename, BasicBlock, CgScope, FunctionCg, ModuleCg};

/// Returns a pointer to the place referred to along with the basic block
pub fn cg_place(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    place: zrc_typeck::tast::expr::Place,
) -> anyhow::Result<(String, BasicBlock)> {
    use zrc_typeck::tast::expr::PlaceKind;

    Ok(match place.1 {
        PlaceKind::Variable(x) => {
            let reg = scope
                .get(x)
                .with_context(|| format!("Identifier {} not found in scope", x))?
                .clone();
            (reg, *bb)
        }
        PlaceKind::Deref(x) => {
            let (x_ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;

            let x_reg = cg_load(cg, &bb, &get_llvm_typename(x.0), &x_ptr)?;

            (x_reg, bb)
        }
        PlaceKind::Index(x, index) => {
            let (x_ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;
            let (index_ptr, bb) = cg_expr(module, cg, &bb, scope, *index.clone())?;

            let x_typename = get_llvm_typename(x.clone().0);

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr)?;

            let index_typename = get_llvm_typename(index.0.clone());

            let index_reg = cg_load(cg, &bb, &index_typename, &index_ptr)?;

            let result_typename = match x.0 {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            #[allow(clippy::uninlined_format_args)] // for line length
            bb.add_instruction(
                cg,
                &format!(
                    "{} = getelementptr {}, {} {}, {} {}",
                    result_reg, result_typename, x_typename, x_reg, index_typename, index_reg
                ),
            )?;

            (result_reg, bb)
        }
        PlaceKind::Dot(x, key) => {
            let (x_ptr, bb) = cg_place(module, cg, bb, scope, *x.clone())?;

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    entries
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &key)
                        .with_context(|| format!("Struct {} has no field {}", x.0, key))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            #[allow(clippy::uninlined_format_args)] // for line length
            bb.add_instruction(
                cg,
                &format!(
                    "{} = getelementptr {}, ptr {}, i32 0, i32 {}",
                    result_reg,
                    get_llvm_typename(x.0.clone()),
                    // x_typename,
                    x_ptr,
                    key_idx
                ),
            )?;

            (result_reg, bb)
        }
    })
}

/// Returns either a literal or a register holding the result of the expression.
/// The old expression code generator used allocations for every expression, so 2 + 2 involved
/// allocating space for both constants and the result and relying on SROA to optimize it away.
/// However, expression results will never mutate, it's *assignments* / places that are mutated.
pub fn cg_expr(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    expr: zrc_typeck::tast::expr::TypedExpr,
) -> anyhow::Result<(String, BasicBlock)> {
    use zrc_typeck::tast::expr::TypedExprKind;

    Ok(match expr.1 {
        TypedExprKind::NumberLiteral(n) => (n.to_string(), *bb),
        TypedExprKind::BooleanLiteral(b) => (b.to_string(), *bb),

        TypedExprKind::Comma(lhs, rhs) => {
            let (_, bb) = cg_expr(module, cg, bb, scope, *lhs)?;
            cg_expr(module, cg, &bb, scope, *rhs)?
        }

        TypedExprKind::Ternary(cond, lhs, rhs) => {
            let (cond, bb) = cg_expr(module, cg, bb, scope, *cond)?;

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

            let result_typename = get_llvm_typename(expr.0.clone());

            let if_true_bb = cg.new_bb();
            let if_false_bb = cg.new_bb();

            bb.add_instruction(
                cg,
                &format!("br i1 {cond}, label {if_true_bb}, label {if_false_bb}"),
            )?;

            let (if_true, if_true_bb) = cg_expr(module, cg, &if_true_bb, scope, *lhs)?;
            let (if_false, if_false_bb) = cg_expr(module, cg, &if_false_bb, scope, *rhs)?;

            let end_bb = cg.new_bb();

            if_true_bb.add_instruction(cg, &format!("br label {end_bb}"))?;
            if_false_bb.add_instruction(cg, &format!("br label {end_bb}"))?;

            let result_reg = cg.new_reg();
            end_bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = phi {result_typename} [ {if_true}, {if_true_bb} ], [ {if_false}, {if_false_bb} ]",
                ),
            )?;

            (result_reg, end_bb)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs)?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let op = match op {
                zrc_typeck::tast::expr::BinaryBitwise::And => "and",
                zrc_typeck::tast::expr::BinaryBitwise::Or => "or",
                zrc_typeck::tast::expr::BinaryBitwise::Xor => "xor",
                zrc_typeck::tast::expr::BinaryBitwise::Shl => "shl",
                zrc_typeck::tast::expr::BinaryBitwise::Shr if expr.0.is_unsigned_integer() => {
                    "lshr"
                }
                zrc_typeck::tast::expr::BinaryBitwise::Shr if expr.0.is_signed_integer() => "ashr",
                zrc_typeck::tast::expr::BinaryBitwise::Shr => unreachable!(),
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_type} {lhs}, {rhs}"),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::Equality(op, lhs, rhs) => {
            let operand_typename = get_llvm_typename((*lhs).0.clone());
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs.clone())?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            let op = match op {
                zrc_typeck::tast::expr::Equality::Eq => "icmp eq",
                zrc_typeck::tast::expr::Equality::Neq => "icmp ne",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {operand_typename} {lhs}, {rhs}"),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::Comparison(op, lhs, rhs) => {
            let operand_typename = get_llvm_typename((*lhs).0.clone());
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs.clone())?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            // the operands are integers
            let op = match (op, expr.0.clone()) {
                (zrc_typeck::tast::expr::Comparison::Lt, ty) if ty.is_signed_integer() => {
                    "icmp slt"
                }
                (zrc_typeck::tast::expr::Comparison::Gt, ty) if ty.is_signed_integer() => {
                    "icmp sgt"
                }
                (zrc_typeck::tast::expr::Comparison::Lte, ty) if ty.is_signed_integer() => {
                    "icmp sle"
                }
                (zrc_typeck::tast::expr::Comparison::Gte, ty) if ty.is_signed_integer() => {
                    "icmp sge"
                }

                (zrc_typeck::tast::expr::Comparison::Lt, _) => "icmp ult",
                (zrc_typeck::tast::expr::Comparison::Gt, _) => "icmp ugt",
                (zrc_typeck::tast::expr::Comparison::Lte, _) => "icmp ule",
                (zrc_typeck::tast::expr::Comparison::Gte, _) => "icmp uge",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {operand_typename} {lhs}, {rhs}"),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::Arithmetic(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs)?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let op = match op {
                zrc_typeck::tast::expr::Arithmetic::Addition => "add",
                zrc_typeck::tast::expr::Arithmetic::Division if expr.0.is_signed_integer() => {
                    "sdiv"
                }
                zrc_typeck::tast::expr::Arithmetic::Division if expr.0.is_unsigned_integer() => {
                    "udiv"
                }
                zrc_typeck::tast::expr::Arithmetic::Division => unreachable!(),
                zrc_typeck::tast::expr::Arithmetic::Modulo if expr.0.is_signed_integer() => "srem",
                zrc_typeck::tast::expr::Arithmetic::Modulo if expr.0.is_unsigned_integer() => {
                    "urem"
                }
                zrc_typeck::tast::expr::Arithmetic::Modulo => unreachable!(),
                zrc_typeck::tast::expr::Arithmetic::Multiplication => "mul",
                zrc_typeck::tast::expr::Arithmetic::Subtraction => "sub",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_type} {lhs}, {rhs}"),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::Logical(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs)?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            let result_type = get_llvm_typename(expr.0.clone());

            // "and/or i1" works for bools
            let op = match op {
                zrc_typeck::tast::expr::Logical::And => "and",
                zrc_typeck::tast::expr::Logical::Or => "or",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_type} {lhs}, {rhs}"),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let (x, bb) = cg_expr(module, cg, bb, scope, *x)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            bb.add_instruction(cg, &format!("{result_reg} = xor {result_type} {x}, -1"))?;

            (result_reg, bb)
        }

        TypedExprKind::UnaryNot(x) => {
            // x is bool
            let (x, bb) = cg_expr(module, cg, bb, scope, *x)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            bb.add_instruction(cg, &format!("{result_reg} = xor {result_type} {x}, 1"))?;

            (result_reg, bb)
        }

        TypedExprKind::UnaryMinus(x) => {
            let (x, bb) = cg_expr(module, cg, bb, scope, *x)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            bb.add_instruction(cg, &format!("{result_reg} = sub {result_type} 0, {x}"))?;

            (result_reg, bb)
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let (x, bb) = cg_place(module, cg, bb, scope, *x)?;

            (x, bb)
        }

        TypedExprKind::UnaryDereference(x) => {
            let (x, bb) = cg_expr(module, cg, bb, scope, *x)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            bb.add_instruction(cg, &format!("{result_reg} = load {result_type}, {x}"))?;

            (result_reg, bb)
        }

        TypedExprKind::Assignment(place, value) => {
            let (new_value, bb) = cg_expr(module, cg, bb, scope, *value.clone())?;
            let (ptr, bb) = cg_place(module, cg, &bb, scope, *place)?;

            let value_type = get_llvm_typename((value).clone().0);

            bb.add_instruction(
                cg,
                &format!("store {} {}, ptr {}", value_type, new_value, ptr),
            )?;

            (new_value, bb)
        }

        TypedExprKind::Identifier(id) => {
            let reg = scope
                .get(id)
                .with_context(|| format!("Identifier {} not found in scope", id))?
                .clone();

            let value = cg_load(cg, bb, &get_llvm_typename(expr.0), &reg)?;

            (value, *bb)
        }

        TypedExprKind::Index(x, index) => {
            let (ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;

            let index_typename = get_llvm_typename(index.0.clone());

            let (index_reg, bb) = cg_expr(module, cg, &bb, scope, *index)?;

            let result_typename = match x.0 {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            #[allow(clippy::uninlined_format_args)] // for line length
            bb.add_instruction(
                cg,
                &format!(
                    "{} = getelementptr {}, {} {}, {} {}",
                    result_reg, result_typename, result_typename, ptr, index_typename, index_reg
                ),
            )?;

            let value = cg_load(cg, &bb, &result_typename, &result_reg)?;

            (value, bb)
        }

        TypedExprKind::Dot(x, prop) => {
            let (ptr, bb) = cg_place(module, cg, bb, scope, *x.clone())?;

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    entries
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &prop)
                        .with_context(|| format!("Struct {} has no field {}", x.0, prop))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            #[allow(clippy::uninlined_format_args)] // for line length
            bb.add_instruction(
                cg,
                &format!(
                    "{} = getelementptr {}, ptr {}, i32 0, i32 {}",
                    result_reg,
                    get_llvm_typename(x.0.clone()),
                    // x_typename,
                    ptr,
                    key_idx
                ),
            )?;

            let value = cg_load(cg, &bb, &get_llvm_typename(expr.0), &result_reg)?;

            (value, bb)
        }

        TypedExprKind::Call(f, args) => {
            let old_f = f.clone();
            let (f, bb) = cg_place(module, cg, bb, scope, *f)?;

            let result_reg = cg.new_reg();

            let mut bb = bb;
            let old_args = args;
            let mut args = vec![];
            for arg in old_args {
                let (new_arg, new_bb) = cg_expr(module, cg, &bb, scope, arg.clone())?;
                bb = new_bb;
                args.push((new_arg, arg.0));
            }

            let args = args
                .into_iter()
                .map(|arg| format!("{} {}", get_llvm_typename(arg.1), arg.0))
                .collect::<Vec<_>>()
                .join(", ");

            bb.add_instruction(
                cg,
                &format!(
                    "{} = call {} {}({})",
                    result_reg,
                    get_llvm_typename(old_f.0)
                        .strip_suffix('*')
                        .expect("fp type didn't end in *, could not trim it"),
                    f,
                    args
                ),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::Cast(x, t) => {
            use zrc_typeck::tast::ty::Type::*;

            // The legendary Cast Table. Contains the cast opcode used for T -> U
            let cast_opcode = match (x.0.clone(), t.clone()) {
                (x, y) if x == y => "bitcast",
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
                (Bool, I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "zext",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Bool) => "trunc",

                (I8, I16 | U16 | I32 | U32 | I64 | U64) => "sext",
                (U8, I16 | U16 | I32 | U32 | I64 | U64) => "zext",
                (I16, I32 | U32 | I64 | U64) => "sext",
                (U16, I32 | U32 | I64 | U64) => "zext",
                (I32, I64 | U64) => "sext",
                (U32, I64 | U64) => "zext",

                (I16 | U16 | I32 | U32 | I64 | U64, I8) => "trunc",
                (I16 | U16 | I32 | U32 | I64 | U64, U8) => "trunc",
                (I32 | U32 | I64 | U64, I16) => "trunc",
                (I32 | U32 | I64 | U64, U16) => "trunc",
                (I64 | U64, I32) => "trunc",
                (I64 | U64, U32) => "trunc",

                // also handle things like I32 => U32
                // is this even the correct way to do it?
                (I8, U8) | (U8, I8) => "bitcast",
                (I16, U16) | (U16, I16) => "bitcast",
                (I32, U32) | (U32, I32) => "bitcast",
                (I64, U64) | (U64, I64) => "bitcast",

                (Bool, x) if x.is_signed_integer() => "sext",
                (Bool, x) if x.is_unsigned_integer() => "zext",

                (Ptr(_), Ptr(_)) => "bitcast",
                (Ptr(_), I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "ptrtoint",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Ptr(_)) => "inttoptr",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Fn(_, _)) => "inttoptr",
                (Fn(_, _), I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "ptrtoint",
                _ => bail!("invalid cast from {} to {}", x.0, t),
            };
            let old_x = x.clone();

            let (x, bb) = cg_expr(module, cg, bb, scope, *x)?;

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!(
                    "{} = {} {} {} to {}",
                    result_reg,
                    cast_opcode,
                    get_llvm_typename(old_x.0),
                    x,
                    get_llvm_typename(t),
                ),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::StringLiteral(s) => {
            let id = module.global_constant_id.next();
            module.declarations.push(format!(
                "@.str{id} = private constant [{} x i8] c\"{}\\00\"",
                s.len() - 1, // subtract both quotes, add the null
                &s[1..s.len() - 1]
            ));

            (format!("@.str{id}"), *bb)
        }
    })
}
