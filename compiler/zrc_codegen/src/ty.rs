//! Resolution from [`Type`] instances to LLVM types.
//!
//! This module contains functions to convert Zirco types
//! ([`zrc_typeck::tast::ty::Type`]) to their corresponding
//! LLVM types ([`inkwell::types::AnyTypeEnum`]).
//!
//! The main function is [`llvm_type`], which takes a Zirco type and
//! returns the corresponding LLVM type. There are also helper functions
//! for creating pointers and function types.

use inkwell::{
    AddressSpace,
    debug_info::{AsDIScope, DIBasicType, DISubroutineType, DIType},
    types::{
        AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        IntType,
    },
};
use zrc_typeck::tast::ty::{Fn, Type};

use crate::ctx::AsCompilationUnitCtx;

/// Create a function pointer from a prototype.
///
/// Returns a [`DIBasicType`] because for some reason [`DISubroutineType`] can't
/// be cast to a [`DIType`]. Use .1 when doing ``.create_function``, use .2 for
/// other uses
///
/// # Panics
/// Panics if `ty` is [`AnyTypeEnum::FunctionType`].
#[must_use]
pub fn create_fn<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: AnyTypeEnum<'ctx>,
    dbg_ty: Option<DIType<'ctx>>,
    args: &[BasicMetadataTypeEnum<'ctx>],
    dbg_args: Option<&[DIType<'ctx>]>,
    is_variadic: bool,
) -> (
    FunctionType<'ctx>,
    Option<DISubroutineType<'ctx>>,
    Option<DIBasicType<'ctx>>,
) {
    (
        match ty {
            AnyTypeEnum::ArrayType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::FloatType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::IntType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::PointerType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::StructType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::VectorType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::VoidType(x) => x.fn_type(args, is_variadic),
            AnyTypeEnum::FunctionType(_) => panic!("fn is not a valid return type for a function"),
            AnyTypeEnum::ScalableVectorType(x) => x.fn_type(args, is_variadic),
        },
        ctx.dbg_builder().map(|dbg_builder| {
            dbg_builder.create_subroutine_type(
                ctx.compilation_unit().expect("we have DI").get_file(),
                dbg_ty,
                dbg_args.expect("we have DI"),
                0,
            )
        }),
        ctx.dbg_builder().map(|dbg_builder| {
            dbg_builder
                .create_basic_type(&ty.to_string(), 0, 0, 0)
                .expect("basic type should be valid")
        }),
    )
}

/// Resolve a [`Type`] to a LLVM [`IntType`]
///
/// # Panics
/// Panics if `ty` is not an integer type
pub fn llvm_int_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (IntType<'ctx>, Option<DIBasicType<'ctx>>) {
    (
        match ty {
            Type::Bool => ctx.ctx().bool_type(),
            Type::I8 | Type::U8 => ctx.ctx().i8_type(),
            Type::I16 | Type::U16 => ctx.ctx().i16_type(),
            Type::I32 | Type::U32 => ctx.ctx().i32_type(),
            Type::I64 | Type::U64 => ctx.ctx().i64_type(),
            Type::Usize | Type::Isize => ctx.ctx().ptr_sized_int_type(
                &ctx.target_machine().get_target_data(),
                Some(AddressSpace::default()),
            ),
            Type::Int => {
                panic!("{{int}} type reached code generation, should be resolved in typeck")
            }
            _ => {
                panic!("not an integer type")
            }
        },
        ctx.dbg_builder().map(|dbg_builder| {
            dbg_builder
                .create_basic_type(&ty.to_string(), 0, 0, 0)
                .expect("basic type should be valid")
        }),
    )
}

/// Resolve a [`Type`] to a LLVM [`BasicTypeEnum`]
///
/// # Panics
/// Panics if `ty` is not a basic type
#[expect(clippy::too_many_lines)]
pub fn llvm_basic_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (BasicTypeEnum<'ctx>, Option<DIType<'ctx>>) {
    match ty {
        Type::Bool
        | Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::I32
        | Type::U32
        | Type::I64
        | Type::U64
        | Type::Usize
        | Type::Isize => {
            let (ty, dbg_ty) = llvm_int_type(ctx, ty);
            (ty.as_basic_type_enum(), dbg_ty.map(|x| x.as_type()))
        }
        Type::Int => {
            panic!("{{int}} type reached code generation, should be resolved in typeck")
        }
        // Since LLVM 18 pointer types are no longer distinct, just 'ptr's
        Type::Ptr(x) => (
            ctx.ctx()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            ctx.dbg_builder().map(|dbg_builder| {
                dbg_builder
                    .create_basic_type(&x.to_string(), 0, 0, 0)
                    .expect("basic type should be valid")
                    .as_type()
            }),
        ),
        Type::Array { size, element_type } => {
            let (elem_ty, elem_dbg_ty) = llvm_basic_type(ctx, element_type);
            (
                elem_ty.array_type(*size as u32).as_basic_type_enum(),
                ctx.dbg_builder().map(|dbg_builder| {
                    let elem_size_in_bits = elem_ty
                        .size_of()
                        .expect("element type should have size")
                        .get_zero_extended_constant()
                        .expect("size should be constant")
                        * 8;
                    #[expect(clippy::cast_possible_truncation, clippy::as_conversions)]
                    dbg_builder
                        .create_array_type(
                            elem_dbg_ty.expect("we have DI"),
                            *size,
                            elem_size_in_bits as u32,
                            &[],
                        )
                        .as_type()
                }),
            )
        }
        Type::Fn(_) => panic!("function is not a basic type"),
        Type::Opaque(name) => {
            panic!("opaque type '{name}' reached code generation, should be resolved in typeck")
        }
        Type::Struct(fields) => (
            ctx.ctx()
                .struct_type(
                    &fields
                        .into_iter()
                        .map(|(_, key_ty)| llvm_basic_type(ctx, key_ty).0)
                        .collect::<Vec<_>>(),
                    false,
                )
                .as_basic_type_enum(),
            ctx.dbg_builder().map(|dbg_builder| {
                dbg_builder
                    .create_struct_type(
                        ctx.compilation_unit()
                            .expect("we have DI")
                            .get_file()
                            .as_debug_info_scope(),
                        &ty.to_string(),
                        ctx.compilation_unit().expect("we have DI").get_file(),
                        0,
                        0,
                        0,
                        0,
                        None,
                        &fields
                            .into_iter()
                            .map(|(key, key_ty)| {
                                ctx.dbg_builder()
                                    .expect("we have DI")
                                    .create_member_type(
                                        ctx.compilation_unit()
                                            .expect("we have DI")
                                            .get_file()
                                            .as_debug_info_scope(),
                                        key,
                                        ctx.compilation_unit().expect("we have DI").get_file(),
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        llvm_basic_type(ctx, key_ty).1.expect("we have DI"),
                                    )
                                    .as_type()
                            })
                            .collect::<Vec<_>>(),
                        0,
                        None,
                        "",
                    )
                    .as_type()
            }),
        ),
        Type::Union(fields) => {
            // Determine which field has the largest size. This is what we will allocate.
            let largest_field = fields
                .into_iter()
                .map(|(_, ty)| {
                    let ty = llvm_basic_type(ctx, ty).0;
                    let size = ctx.target_machine().get_target_data().get_bit_size(&ty);
                    (ty, size)
                })
                .max_by_key(|(_, size)| *size)
                .unwrap_or_else(|| {
                    // this is basically `never`
                    let ty = ctx.ctx().struct_type(&[], false).as_basic_type_enum();
                    (ty, ctx.target_machine().get_target_data().get_bit_size(&ty))
                });

            (
                largest_field.0,
                ctx.dbg_builder().map(|dbg_builder| {
                    dbg_builder
                        .create_union_type(
                            ctx.compilation_unit()
                                .expect("we have DI")
                                .get_file()
                                .as_debug_info_scope(),
                            &ty.to_string(),
                            ctx.compilation_unit().expect("we have DI").get_file(),
                            0,
                            0,
                            0,
                            0,
                            &fields
                                .into_iter()
                                .map(|(_, ty)| llvm_basic_type(ctx, ty).1.expect("we have DI"))
                                .collect::<Vec<_>>(),
                            0,
                            "",
                        )
                        .as_type()
                }),
            )
        }
    }
}

/// Resolve a [`Type`] to a LLVM [`AnyTypeEnum`]
pub fn llvm_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (AnyTypeEnum<'ctx>, Option<DIType<'ctx>>) {
    match ty {
        Type::Bool
        | Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::I32
        | Type::U32
        | Type::I64
        | Type::U64
        | Type::Usize
        | Type::Isize
        | Type::Ptr(_)
        | Type::Struct(_)
        | Type::Array { .. }
        | Type::Union(_) => {
            let (ty, dbg_ty) = llvm_basic_type(ctx, ty);
            (ty.as_any_type_enum(), dbg_ty)
        }
        Type::Int => {
            panic!("{{int}} type reached code generation, should be resolved in typeck")
        }

        Type::Fn(Fn { arguments, returns }) => {
            let (ret, ret_dbg) = llvm_type(ctx, returns);
            let is_variadic = arguments.is_variadic();
            let argument_dbg_types = arguments
                .as_arguments()
                .iter()
                .all(|arg| llvm_type(ctx, arg.ty.value()).1.is_some())
                .then(|| {
                    arguments
                        .as_arguments()
                        .iter()
                        .map(|arg| llvm_type(ctx, arg.ty.value()).1.expect("we have DI"))
                        .collect::<Vec<_>>()
                });
            let (fn_ty, _, fn_dbg_ty) = create_fn(
                ctx,
                ret,
                ret_dbg,
                &arguments
                    .as_arguments()
                    .iter()
                    .map(|arg| llvm_basic_type(ctx, arg.ty.value()).0.into())
                    .collect::<Vec<_>>(),
                argument_dbg_types.as_deref(),
                is_variadic,
            );
            (fn_ty.as_any_type_enum(), fn_dbg_ty.map(|x| x.as_type()))
        }

        Type::Opaque(name) => {
            panic!("opaque type '{name}' reached code generation, should be resolved in typeck")
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
    fn tagged_unions_are_properly_typed() {
        cg_snapshot_test!(indoc! {"
            enum VariableInt {
                Eight: i8,
                Sixteen: i16,
            }

            fn test() -> VariableInt {
                // TEST: should generate a { usize, i16 (largest of values) }
                let x: VariableInt;

                // internal parameters
                let y = x.__discriminant__;
                let z = x.__value__;

                return x;
            }
        "});
    }

    #[test]
    fn self_referential_struct_generates_properly() {
        cg_snapshot_test!(indoc! {"
            // TEST: self-referential struct types should compile to LLVM IR
            // with pointers to empty structs as placeholders
            struct Node {
                value: i32,
                next: *Node
            }

            struct TreeNode {
                value: i32,
                left: *TreeNode,
                right: *TreeNode
            }

            fn create_node(val: i32) -> *Node {
                let node: *Node;
                return node;
            }

            fn main() -> i32 {
                let head: *Node;
                let tree: TreeNode;
                head = create_node(42);

                return 0;
            }
        "});
    }
}
