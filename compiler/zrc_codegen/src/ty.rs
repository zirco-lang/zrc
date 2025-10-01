//! Resolution from [`Type`] instances to LLVM types.

use inkwell::{
    AddressSpace,
    debug_info::{AsDIScope, DIBasicType, DIDerivedType, DISubroutineType, DIType},
    types::{
        AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        IntType, PointerType,
    },
};
use zrc_typeck::tast::ty::{Fn, Type};

use crate::ctx::AsCompilationUnitCtx;

/// Create a pointer to an [`AnyTypeEnum`] instance.
///
/// # Panics
/// Panics if `ty` is [`AnyTypeEnum::VoidType`].
fn create_ptr<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: AnyTypeEnum<'ctx>,
    dbg_ty: DIType<'ctx>,
) -> (PointerType<'ctx>, DIDerivedType<'ctx>) {
    (
        match ty {
            AnyTypeEnum::ArrayType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::FloatType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::IntType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::PointerType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::StructType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::VectorType(x) => x.ptr_type(AddressSpace::default()),
            AnyTypeEnum::VoidType(_) => panic!("cannot create a pointer to void"),
            AnyTypeEnum::FunctionType(x) => x.ptr_type(AddressSpace::default()),
        },
        ctx.dbg_builder().create_pointer_type(
            &ty.to_string(),
            dbg_ty,
            0,
            0,
            AddressSpace::default(),
        ),
    )
}
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
    dbg_ty: DIType<'ctx>,
    args: &[BasicMetadataTypeEnum<'ctx>],
    dbg_args: &[DIType<'ctx>],
    is_variadic: bool,
) -> (
    FunctionType<'ctx>,
    DISubroutineType<'ctx>,
    DIBasicType<'ctx>,
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
        },
        ctx.dbg_builder().create_subroutine_type(
            ctx.compilation_unit().get_file(),
            Some(dbg_ty),
            dbg_args,
            0,
        ),
        ctx.dbg_builder()
            .create_basic_type(&ty.to_string(), 0, 0, 0)
            .expect("basic type should be valid"),
    )
}

/// Resolve a [`Type`] to a LLVM [`IntType`]
///
/// # Panics
/// Panics if `ty` is not an integer type
#[allow(clippy::needless_pass_by_value)]
pub fn llvm_int_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (IntType<'ctx>, DIBasicType<'ctx>) {
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
            Type::Ptr(_) | Type::Fn(_) | Type::Struct(_) | Type::Union(_) | Type::Opaque(_) => {
                panic!("not an integer type")
            }
        },
        ctx.dbg_builder()
            .create_basic_type(&ty.to_string(), 0, 0, 0)
            .expect("basic type should be valid"),
    )
}

/// Resolve a [`Type`] to a LLVM [`BasicTypeEnum`]
///
/// # Panics
/// Panics if `ty` is not a basic type
#[allow(clippy::too_many_lines)]
pub fn llvm_basic_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (BasicTypeEnum<'ctx>, DIType<'ctx>) {
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
            (ty.as_basic_type_enum(), dbg_ty.as_type())
        }
        Type::Ptr(x) => {
            #[allow(clippy::wildcard_enum_match_arm)]
            let (pointee_ty, pointee_dbg_ty) = match **x {
                // Opaque types behind pointers are treated as opaque pointers
                // We use i8 as a placeholder since we don't know the actual size
                Type::Opaque(name) => (
                    ctx.ctx().i8_type().as_any_type_enum(),
                    ctx.dbg_builder()
                        .create_basic_type(name, 0, 0, 0)
                        .expect("basic type should be valid")
                        .as_type(),
                ),
                _ => llvm_type(ctx, x),
            };
            let (ty, dbg_ty) = create_ptr(ctx, pointee_ty, pointee_dbg_ty);
            (ty.as_basic_type_enum(), dbg_ty.as_type())
        }
        Type::Fn(_) => panic!("function is not a basic type"),
        Type::Opaque(_) => panic!(
            "opaque types should only appear behind pointers and should be resolved before codegen"
        ),
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
            ctx.dbg_builder()
                .create_struct_type(
                    ctx.compilation_unit().get_file().as_debug_info_scope(),
                    &ty.to_string(),
                    ctx.compilation_unit().get_file(),
                    0,
                    0,
                    0,
                    0,
                    None,
                    &fields
                        .into_iter()
                        .map(|(key, key_ty)| {
                            ctx.dbg_builder()
                                .create_member_type(
                                    ctx.compilation_unit().get_file().as_debug_info_scope(),
                                    key,
                                    ctx.compilation_unit().get_file(),
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    llvm_basic_type(ctx, key_ty).1,
                                )
                                .as_type()
                        })
                        .collect::<Vec<_>>(),
                    0,
                    None,
                    "",
                )
                .as_type(),
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
                ctx.dbg_builder()
                    .create_union_type(
                        ctx.compilation_unit().get_file().as_debug_info_scope(),
                        &ty.to_string(),
                        ctx.compilation_unit().get_file(),
                        0,
                        0,
                        0,
                        0,
                        &fields
                            .into_iter()
                            .map(|(_, ty)| llvm_basic_type(ctx, ty).1)
                            .collect::<Vec<_>>(),
                        0,
                        "",
                    )
                    .as_type(),
            )
        }
    }
}

/// Resolve a [`Type`] to a LLVM [`AnyTypeEnum`]
pub fn llvm_type<'ctx: 'a, 'a>(
    ctx: &impl AsCompilationUnitCtx<'ctx, 'a>,
    ty: &Type,
) -> (AnyTypeEnum<'ctx>, DIType<'ctx>) {
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
        | Type::Union(_) => {
            let (ty, dbg_ty) = llvm_basic_type(ctx, ty);
            (ty.as_any_type_enum(), dbg_ty)
        }

        Type::Opaque(name) => panic!("opaque type '{name}' should only appear behind pointers"),

        Type::Fn(Fn { arguments, returns }) => {
            let (ret, ret_dbg) = llvm_type(ctx, returns);
            let is_variadic = arguments.is_variadic();
            let (fn_ty, _, fn_dbg_ty) = create_fn(
                ctx,
                ret,
                ret_dbg,
                &arguments
                    .as_arguments()
                    .iter()
                    .map(|arg| llvm_basic_type(ctx, arg.ty.value()).0.into())
                    .collect::<Vec<_>>(),
                &arguments
                    .as_arguments()
                    .iter()
                    .map(|arg| llvm_basic_type(ctx, arg.ty.value()).1)
                    .collect::<Vec<_>>(),
                is_variadic,
            );
            (fn_ty.as_any_type_enum(), fn_dbg_ty.as_type())
        }
    }
}
