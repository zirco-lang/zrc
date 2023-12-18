//! Resolution from [`Type`] instances to LLVM types.

use inkwell::{
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DIDerivedType, DIFile, DISubroutineType, DIType, DebugInfoBuilder,
    },
    targets::TargetMachine,
    types::{
        AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        IntType, PointerType,
    },
    AddressSpace,
};
use zrc_typeck::{tast::ty::Type, typeck::BlockReturnType};

/// Create a pointer to an [`AnyTypeEnum`] instance.
///
/// # Panics
/// Panics if `ty` is [`AnyTypeEnum::VoidType`].
fn create_ptr<'ctx>(
    dbg_builder: &DebugInfoBuilder<'ctx>,
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
        dbg_builder.create_pointer_type(&ty.to_string(), dbg_ty, 0, 0, AddressSpace::default()),
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
pub fn create_fn<'ctx>(
    dbg_builder: &DebugInfoBuilder<'ctx>,
    file: DIFile<'ctx>,
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
        dbg_builder.create_subroutine_type(file, Some(dbg_ty), dbg_args, 0),
        dbg_builder
            .create_basic_type(&ty.to_string(), 0, 0, 0)
            .expect("basic type should be valid"),
    )
}

/// Resolve a [`Type`] to a LLVM [`IntType`]
///
/// # Panics
/// Panics if `ty` is not an integer type
#[allow(clippy::needless_pass_by_value)]
pub fn llvm_int_type<'ctx>(
    dbg_builder: &DebugInfoBuilder<'ctx>,
    ctx: &'ctx Context,
    ty: &Type,
) -> (IntType<'ctx>, DIBasicType<'ctx>) {
    (
        match ty {
            Type::Bool => ctx.bool_type(),
            Type::I8 | Type::U8 => ctx.i8_type(),
            Type::I16 | Type::U16 => ctx.i16_type(),
            Type::I32 | Type::U32 => ctx.i32_type(),
            Type::I64 | Type::U64 => ctx.i64_type(),
            Type::Void | Type::Ptr(_) | Type::Fn(_, _) | Type::Struct(_) | Type::Union(_) => {
                panic!("not an integer type")
            }
        },
        dbg_builder
            .create_basic_type(&ty.to_string(), 0, 0, 0)
            .expect("basic type should be valid"),
    )
}

/// Resolve a [`Type`] to a LLVM [`BasicTypeEnum`]
///
/// # Panics
/// Panics if `ty` is not a basic type
#[allow(clippy::too_many_lines)]
pub fn llvm_basic_type<'ctx>(
    file: DIFile<'ctx>,
    dbg_builder: &DebugInfoBuilder<'ctx>,
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
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
        | Type::U64 => {
            let (ty, dbg_ty) = llvm_int_type(dbg_builder, ctx, ty);
            (ty.as_basic_type_enum(), dbg_ty.as_type())
        }
        Type::Void => panic!("void is not a basic type"),
        Type::Ptr(x) => {
            let (pointee_ty, pointee_dbg_ty) = llvm_type(file, dbg_builder, ctx, target_machine, x);
            let (ty, dbg_ty) = create_ptr(dbg_builder, pointee_ty, pointee_dbg_ty);
            (ty.as_basic_type_enum(), dbg_ty.as_type())
        }
        Type::Fn(_, _) => panic!("function is not a basic type"),
        Type::Struct(fields) => (
            ctx.struct_type(
                &fields
                    .into_iter()
                    .map(|(_, key_ty)| {
                        llvm_basic_type(file, dbg_builder, ctx, target_machine, key_ty).0
                    })
                    .collect::<Vec<_>>(),
                false,
            )
            .as_basic_type_enum(),
            dbg_builder
                .create_struct_type(
                    file.as_debug_info_scope(),
                    &ty.to_string(),
                    file,
                    0,
                    0,
                    0,
                    0,
                    None,
                    &fields
                        .into_iter()
                        .map(|(key, key_ty)| {
                            dbg_builder
                                .create_member_type(
                                    file.as_debug_info_scope(),
                                    key,
                                    file,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    llvm_basic_type(file, dbg_builder, ctx, target_machine, key_ty)
                                        .1,
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
                    let ty = llvm_basic_type(file, dbg_builder, ctx, target_machine, ty).0;
                    let size = target_machine.get_target_data().get_bit_size(&ty);
                    (ty, size)
                })
                .max_by_key(|(_, size)| *size)
                .unwrap_or_else(|| {
                    // this is basically `never`
                    let ty = ctx.struct_type(&[], false).as_basic_type_enum();
                    (ty, target_machine.get_target_data().get_bit_size(&ty))
                });

            (
                largest_field.0,
                dbg_builder
                    .create_union_type(
                        file.as_debug_info_scope(),
                        &ty.to_string(),
                        file,
                        0,
                        0,
                        0,
                        0,
                        &fields
                            .into_iter()
                            .map(|(_, ty)| {
                                llvm_basic_type(file, dbg_builder, ctx, target_machine, ty).1
                            })
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
pub fn llvm_type<'ctx>(
    file: DIFile<'ctx>,
    dbg_builder: &DebugInfoBuilder<'ctx>,
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
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
        | Type::Ptr(_)
        | Type::Struct(_)
        | Type::Union(_) => {
            let (ty, dbg_ty) = llvm_basic_type(file, dbg_builder, ctx, target_machine, ty);
            (ty.as_any_type_enum(), dbg_ty)
        }

        Type::Void => (
            ctx.void_type().as_any_type_enum(),
            dbg_builder
                .create_basic_type("void", 0, 0, 0)
                .expect("basic type should be valid")
                .as_type(),
        ),

        Type::Fn(args, ret) => {
            let (ret, ret_dbg) = llvm_type(
                file,
                dbg_builder,
                ctx,
                target_machine,
                match &**ret {
                    BlockReturnType::Return(x) => x,
                    BlockReturnType::Void => &Type::Void,
                },
            );
            let is_variadic = args.is_variadic();
            let (fn_ty, _, fn_dbg_ty) = create_fn(
                dbg_builder,
                file,
                ret,
                ret_dbg,
                &args
                    .as_arguments()
                    .iter()
                    .map(|arg| {
                        llvm_basic_type(file, dbg_builder, ctx, target_machine, arg.ty.value())
                            .0
                            .into()
                    })
                    .collect::<Vec<_>>(),
                &args
                    .as_arguments()
                    .iter()
                    .map(|arg| {
                        llvm_basic_type(file, dbg_builder, ctx, target_machine, arg.ty.value()).1
                    })
                    .collect::<Vec<_>>(),
                is_variadic,
            );
            (fn_ty.as_any_type_enum(), fn_dbg_ty.as_type())
        }
    }
}
