use anyhow::{bail, Context as _};
use zrc_typeck::tast::{
    expr::{
        Arithmetic, BinaryBitwise, Comparison, Equality, Logical, Place, TypedExpr, TypedExprKind,
    },
    ty::Type,
};

use super::{cg_load, get_llvm_typename, BasicBlock, CgScope, FunctionCg, ModuleCg};

/// Returns a pointer to the place referred to along with the basic block
///
/// # Errors
/// Errors on an internal code generation error.
pub fn cg_place(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    place: Place,
) -> anyhow::Result<(String, BasicBlock)> {
    use zrc_typeck::tast::expr::PlaceKind;

    Ok(match place.1 {
        PlaceKind::Variable(x) => {
            let reg = scope
                .get(x)
                .with_context(|| format!("Identifier {x} not found in scope"))?
                .clone();
            (reg, *bb)
        }
        PlaceKind::Deref(x) => {
            // This is a bit confusing, even for me, but I'm leaving this note here for the
            // future. `x` in this case is an *expression* yielding type `*T`.
            // This expression is most likely an identifier, but it could be a
            // ternary, cast, etc. If it's an identifier, it is stored on the
            // stack and LLVM sees it as a `T**` (points to the stack). cg_place
            // is expected to return a *pointer* to the place, so we need to load the
            // `T*` from the stack or whatever else it represents. `cg_expr` luckily will
            // handle dereferencing an identifier for us, and will perform the
            // actual deref. For this reason, we have no need to generate a
            // `load` instruction here.
            //
            // For example, if we're generating the place `*0`, the pointer to this location
            // is just `0`. We do not need to actually `load` unless we are
            // pulling from the stack, which cg_expr will do for us.

            let (x_ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;

            (x_ptr, bb)
        }
        PlaceKind::Index(x, index) => {
            let old_index = index.clone();
            let (x_ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;
            let (index, bb) = cg_expr(module, cg, &bb, scope, *index.clone())?;

            let x_typename = get_llvm_typename(x.clone().0);

            let index_typename = get_llvm_typename(old_index.0.clone());

            #[allow(clippy::wildcard_enum_match_arm)]
            let result_typename = match x.0 {
                Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            #[allow(clippy::uninlined_format_args)] // for line length
            bb.add_instruction(
                cg,
                &format!(
                    "{} = getelementptr {}, {} {}, {} {}",
                    result_reg, result_typename, x_typename, x_ptr, index_typename, index
                ),
            )?;

            (result_reg, bb)
        }
        PlaceKind::Dot(x, key) => {
            let (x_ptr, bb) = cg_place(module, cg, bb, scope, *x.clone())?;

            let result_reg = cg.new_reg();

            #[allow(clippy::wildcard_enum_match_arm)]
            let key_idx = match x.0.clone() {
                Type::Struct(entries) => {
                    entries
                        .into_iter()
                        .enumerate()
                        .find(|(_, (found_k, _))| found_k == &key)
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
/// The old expression code generator used allocations for every expression, so
/// 2 + 2 involved allocating space for both constants and the result and
/// relying on SROA to optimize it away. However, expression results will never
/// mutate, it's *assignments* / places that are mutated.
///
/// # Errors
/// Errors on an internal code generation error.
#[allow(clippy::too_many_lines, clippy::module_name_repetitions)]
pub fn cg_expr(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    expr: TypedExpr,
) -> anyhow::Result<(String, BasicBlock)> {
    Ok(match expr.1 {
        TypedExprKind::NumberLiteral(n) => (n.to_string(), *bb),
        TypedExprKind::BooleanLiteral(value) => (value.to_string(), *bb),

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
            #[allow(clippy::uninlined_format_args)] // for line length
            end_bb.add_instruction(
                cg,
                &format!(
                    "{} = phi {} [ {}, {} ], [ {}, {} ]",
                    result_reg, result_typename, if_true, if_true_bb, if_false, if_false_bb
                ),
            )?;

            (result_reg, end_bb)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let (lhs, bb) = cg_expr(module, cg, bb, scope, *lhs)?;
            let (rhs, bb) = cg_expr(module, cg, &bb, scope, *rhs)?;

            let result_type = get_llvm_typename(expr.0.clone());

            let op = match op {
                BinaryBitwise::And => "and",
                BinaryBitwise::Or => "or",
                BinaryBitwise::Xor => "xor",
                BinaryBitwise::Shl => "shl",
                BinaryBitwise::Shr if expr.0.is_unsigned_integer() => "lshr",
                BinaryBitwise::Shr if expr.0.is_signed_integer() => "ashr",
                BinaryBitwise::Shr => unreachable!(),
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
                Equality::Eq => "icmp eq",
                Equality::Neq => "icmp ne",
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
                (Comparison::Lt, ty) if ty.is_signed_integer() => "icmp slt",
                (Comparison::Gt, ty) if ty.is_signed_integer() => "icmp sgt",
                (Comparison::Lte, ty) if ty.is_signed_integer() => "icmp sle",
                (Comparison::Gte, ty) if ty.is_signed_integer() => "icmp sge",

                (Comparison::Lt, _) => "icmp ult",
                (Comparison::Gt, _) => "icmp ugt",
                (Comparison::Lte, _) => "icmp ule",
                (Comparison::Gte, _) => "icmp uge",
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
                Arithmetic::Addition => "add",
                Arithmetic::Division if expr.0.is_signed_integer() => "sdiv",
                Arithmetic::Division if expr.0.is_unsigned_integer() => "udiv",
                Arithmetic::Modulo if expr.0.is_signed_integer() => "srem",
                Arithmetic::Modulo if expr.0.is_unsigned_integer() => "urem",
                // should always match one of the above
                Arithmetic::Modulo | Arithmetic::Division => unreachable!(),
                Arithmetic::Multiplication => "mul",
                Arithmetic::Subtraction => "sub",
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
                Logical::And => "and",
                Logical::Or => "or",
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

            bb.add_instruction(cg, &format!("store {value_type} {new_value}, ptr {ptr}"))?;

            (new_value, bb)
        }

        TypedExprKind::Identifier(id) => {
            let reg = scope
                .get(id)
                .with_context(|| format!("Identifier {id} not found in scope"))?
                .clone();

            let value = cg_load(cg, *bb, &get_llvm_typename(expr.0), &reg)?;

            (value, *bb)
        }

        TypedExprKind::Index(x, index) => {
            let (ptr, bb) = cg_expr(module, cg, bb, scope, *x.clone())?;

            let index_typename = get_llvm_typename(index.0.clone());

            let (index_reg, bb) = cg_expr(module, cg, &bb, scope, *index)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let result_typename = match x.0 {
                Type::Ptr(x) => get_llvm_typename(*x),
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

            let value = cg_load(cg, bb, &result_typename, &result_reg)?;

            (value, bb)
        }

        TypedExprKind::Dot(x, prop) => {
            let (ptr, bb) = cg_place(module, cg, bb, scope, *x.clone())?;

            let result_reg = cg.new_reg();

            #[allow(clippy::wildcard_enum_match_arm)]
            let key_idx = match x.0.clone() {
                Type::Struct(entries) => {
                    entries
                        .into_iter()
                        .enumerate()
                        .find(|(_, (got_k, _))| got_k == &prop)
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

            let value = cg_load(cg, bb, &get_llvm_typename(expr.0), &result_reg)?;

            (value, bb)
        }

        TypedExprKind::Call(f, args) => {
            let old_f = f.clone();
            let (f, bb) = cg_place(module, cg, bb, scope, *f)?;

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

            if expr.0 == Type::Void {
                bb.add_instruction(
                    cg,
                    &format!(
                        "call {} {}({})",
                        get_llvm_typename(old_f.0)
                            .strip_suffix('*')
                            .expect("fp type didn't end in *, could not trim it"),
                        f,
                        args
                    ),
                )?;

                ("undef".to_string(), bb)
            } else {
                let result_reg = cg.new_reg();

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
        }

        TypedExprKind::Cast(x, ty) => {
            #[allow(clippy::enum_glob_use)]
            use zrc_typeck::tast::ty::Type::*;

            // The legendary Cast Table. Contains the cast opcode used for T -> U
            let cast_opcode = match (x.0.clone(), ty.clone()) {
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
                (Bool, I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64)
                | (U8, I16 | U16 | I32 | U32 | I64 | U64)
                | (U16, I32 | U32 | I64 | U64)
                | (U32, I64 | U64) => "zext",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Bool)
                | (I16 | U16 | I32 | U32 | I64 | U64, I8 | U8)
                | (I64 | U64, I32 | U32)
                | (I32 | U32 | I64 | U64, I16 | U16) => "trunc",

                (I8, I16 | U16 | I32 | U32 | I64 | U64)
                | (I16, I32 | U32 | I64 | U64)
                | (I32, I64 | U64) => "sext",

                // also handle things like I32 => U32
                // is this even the correct way to do it?
                (I8, U8)
                | (U8, I8)
                | (I16, U16)
                | (U16, I16)
                | (I32, U32)
                | (U32, I32)
                | (I64, U64)
                | (U64, I64)
                | (Ptr(_), Ptr(_)) => "bitcast",

                (Bool, x) if x.is_signed_integer() => "sext",
                (Bool, x) if x.is_unsigned_integer() => "zext",

                (Ptr(_) | Fn(_, _), I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "ptrtoint",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Ptr(_) | Fn(_, _)) => "inttoptr",
                _ => bail!("invalid cast from {} to {}", x.0, ty),
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
                    get_llvm_typename(ty),
                ),
            )?;

            (result_reg, bb)
        }

        TypedExprKind::StringLiteral(str) => {
            let id = module.global_constant_id.next();
            module.declarations.push(format!(
                "@.str{id} = private constant [{} x i8] c\"{}\\00\"",
                str.len() - 1, // subtract both quotes, add the null
                &str[1..str.len() - 1]
            ));

            (format!("@.str{id}"), *bb)
        }
    })
}

#[cfg(test)]
mod tests {

    use zrc_typeck::{tast::expr::PlaceKind, typeck::BlockReturnType};

    use super::*;
    use crate::{init_single_function, BasicBlockData};

    mod cg_place {
        use super::*;

        /// The code generator should not need to do any extra work to handle
        /// identifiers, as they are already represented by a singular register.
        /// They should just return the pointer as-is.
        #[test]
        fn identifier_registers_are_returned_as_is() {
            let (mut module, mut cg, bb, mut scope) = init_single_function();

            scope.insert("x", "%x".to_string());

            let (reg, bb) = cg_place(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                Place(Type::I32, PlaceKind::Variable("x")),
            )
            .unwrap();

            // no basic blocks were produced
            assert_eq!(bb, BasicBlock { id: 0 });

            // no instructions were produced
            assert_eq!(cg.blocks[0].instructions, Vec::<String>::new());

            // the register was returned as-is
            assert_eq!(reg, "%x");
        }

        /// Dereferencing a pointer in place context should generate a single
        /// load instruction to retrieve the pointer itself off the
        /// stack.
        #[test]
        fn identifier_deref_generates_as_expected() {
            let (mut module, mut cg, bb, mut scope) = init_single_function();

            scope.insert("x", "%x".to_string());

            let (reg, bb) = cg_place(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                Place(
                    Type::I32,
                    PlaceKind::Deref(Box::new(TypedExpr(
                        Type::Ptr(Box::new(Type::I32)),
                        TypedExprKind::Identifier("x"),
                    ))),
                ),
            )
            .unwrap();

            // no new basic blocks were produced
            assert_eq!(bb, BasicBlock { id: 0 });

            // the `load` instruction was produced
            assert_eq!(
                cg.blocks[0].instructions,
                vec!["%l1 = load ptr, ptr %x".to_string()]
            );

            // the register was returned
            assert_eq!(reg, "%l1");
        }

        /// Dereferencing a value that is not an identifier should not involve
        /// any loading, because it is expected to return a pointer
        /// (e.g. the pointer to the value `*0` is just `0`)
        #[test]
        fn other_deref_generates_as_expected() {
            let (mut module, mut cg, bb, scope) = init_single_function();

            let (reg, bb) = cg_place(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                // in place context: *(0 as *i32)
                // should return `0` as an llvm ptr
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
            )
            .unwrap();

            // no new basic blocks were produced
            assert_eq!(bb, BasicBlock { id: 0 });

            // an inttoptr instruction is produced and the value is returned
            assert_eq!(
                cg.blocks[0].instructions,
                vec!["%l1 = inttoptr i32 0 to ptr".to_string()]
            );

            // the register was returned
            assert_eq!(reg, "%l1");
        }

        #[test]
        fn pointer_indexing_generates_proper_gep() {
            let (mut module, mut cg, bb, mut scope) = init_single_function();

            // of type *i32
            scope.insert("arr", "%arr".to_string());

            let (reg, bb) = cg_place(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                // in place context: arr[4]
                Place(
                    Type::I32,
                    PlaceKind::Index(
                        Box::new(TypedExpr(
                            Type::Ptr(Box::new(Type::I32)),
                            TypedExprKind::Identifier("arr"),
                        )),
                        Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("4"))),
                    ),
                ),
            )
            .unwrap();

            // no new basic blocks were produced
            assert_eq!(bb, BasicBlock { id: 0 });

            // `arr` is loaded from memory, then a gep instruction is produced
            assert_eq!(
                cg.blocks[0].instructions,
                vec![
                    "%l1 = load ptr, ptr %arr".to_string(),
                    "%l2 = getelementptr i32, ptr %l1, i32 4".to_string()
                ]
            );
            assert_eq!(reg, "%l2");
        }
    }

    mod cg_expr {
        use zrc_typeck::tast::stmt::ArgumentDeclarationList;

        use super::*;

        #[test]
        fn comma_yields_right_value() {
            let (mut module, mut cg, bb, scope) = init_single_function();

            let (reg, bb) = cg_expr(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                TypedExpr(
                    Type::I32,
                    TypedExprKind::Comma(
                        // This left-hand-side is to ensure that the left hand operand is still
                        // evaluated for side effects
                        Box::new(TypedExpr(
                            Type::I32,
                            TypedExprKind::Arithmetic(
                                Arithmetic::Addition,
                                Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("1"))),
                                Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("1"))),
                            ),
                        )),
                        Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("2"))),
                    ),
                ),
            )
            .unwrap();

            // no new basic blocks were produced
            assert_eq!(bb, BasicBlock { id: 0 });

            // the left hand side gets evaluated
            assert_eq!(
                cg.blocks[0].instructions,
                vec!["%l1 = add i32 1, 1".to_string()]
            );

            // the right hand side is what was yielded
            assert_eq!(reg, "2");
        }

        /// This is a much more complex test. It ensures a ternary operator
        /// properly resolves the left and right hand side expressions
        /// and generates the diamond-shaped control flow graph. This
        /// test is specifically constructed so that one side has side effects
        /// and cannot be inlined into the `phi` node, and the other can
        /// be.
        #[test]
        fn ternary_generates_proper_cfg() {
            let (mut module, mut cg, bb, mut scope) = init_single_function();

            // fn get_some_int() -> i32;
            scope.insert("get_some_int", "@get_some_int".to_string());

            let (reg, bb) = cg_expr(
                &mut module,
                &mut cg,
                &bb,
                &scope,
                TypedExpr(
                    Type::I32,
                    TypedExprKind::Ternary(
                        Box::new(TypedExpr(Type::Bool, TypedExprKind::BooleanLiteral(true))),
                        Box::new(TypedExpr(
                            Type::I32,
                            TypedExprKind::Call(
                                Box::new(Place(
                                    Type::Fn(
                                        ArgumentDeclarationList::NonVariadic(vec![]),
                                        Box::new(BlockReturnType::Return(Type::I32)),
                                    ),
                                    PlaceKind::Variable("get_some_int"),
                                )),
                                vec![],
                            ),
                        )),
                        Box::new(TypedExpr(Type::I32, TypedExprKind::NumberLiteral("2"))),
                    ),
                ),
            )
            .unwrap();

            // We expect the output to look like the following:
            // bb0:
            //     br i1 true, label %bb1, label %bb2
            // bb1: ; if_true
            //     %l1 = call i32 @get_some_int()
            //     br label %bb3
            // bb2: ; if false
            //     br label %bb3
            // bb3: ; end
            //     %l2 = phi i32 [ %l1, %bb1 ], [ 2, %bb2 ]
            //     ; yields %l2

            assert_eq!(bb, BasicBlock { id: 3 });

            assert_eq!(
                cg.blocks,
                vec![
                    BasicBlockData {
                        id: 0,
                        instructions: vec!["br i1 true, label %bb1, label %bb2".to_string()]
                    },
                    BasicBlockData {
                        id: 1,
                        instructions: vec![
                            "%l1 = call i32 () @get_some_int()".to_string(),
                            "br label %bb3".to_string()
                        ]
                    },
                    BasicBlockData {
                        id: 2,
                        instructions: vec!["br label %bb3".to_string()]
                    },
                    BasicBlockData {
                        id: 3,
                        instructions: vec!["%l2 = phi i32 [ %l1, %bb1 ], [ 2, %bb2 ]".to_string()]
                    },
                ]
            );

            assert_eq!(reg, "%l2");
        }
    }
}
