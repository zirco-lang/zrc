//! Resolution from [`Type`] instances to LLVM types.

use inkwell::{
    context::Context,
    debug_info::{DIBasicType, DebugInfoBuilder},
    targets::TargetMachine,
    types::{
        AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        IntType, PointerType,
    },
    AddressSpace,
};
use zrc_typeck::tast::ty::Type;

/// Create the LLVM [`DIBasicType`] for a [`Type`] node.
// TODO: Properly create the composite types etc.
pub fn create_di_type<'ctx>(dbg_builder: &DebugInfoBuilder<'ctx>, ty: &Type) -> DIBasicType<'ctx> {
    dbg_builder
        .create_basic_type(&ty.to_string(), 0, 0, 0)
        .unwrap()
}

/// Create a pointer to an [`AnyTypeEnum`] instance.
///
/// # Panics
/// Panics if `ty` is [`AnyTypeEnum::VoidType`].
fn create_ptr(ty: AnyTypeEnum<'_>) -> PointerType<'_> {
    match ty {
        AnyTypeEnum::ArrayType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::FloatType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::IntType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::PointerType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::StructType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::VectorType(x) => x.ptr_type(AddressSpace::default()),
        AnyTypeEnum::VoidType(_) => panic!("cannot create a pointer to void"),
        AnyTypeEnum::FunctionType(x) => x.ptr_type(AddressSpace::default()),
    }
}
/// Create a function pointer from a prototype.
///
/// # Panics
/// Panics if `ty` is [`AnyTypeEnum::FunctionType`].
#[must_use]
pub fn create_fn<'ctx>(
    ty: AnyTypeEnum<'ctx>,
    args: &[BasicMetadataTypeEnum<'ctx>],
    is_variadic: bool,
) -> FunctionType<'ctx> {
    match ty {
        AnyTypeEnum::ArrayType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::FloatType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::IntType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::PointerType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::StructType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::VectorType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::VoidType(x) => x.fn_type(args, is_variadic),
        AnyTypeEnum::FunctionType(_) => panic!("fn is not a valid return type for a function"),
    }
}

/// Resolve a [`Type`] to a LLVM [`IntType`]
///
/// # Panics
/// Panics if `ty` is not an integer type
#[allow(clippy::needless_pass_by_value)]
pub fn llvm_int_type<'ctx>(ctx: &'ctx Context, ty: Type) -> IntType<'ctx> {
    match ty {
        Type::Bool => ctx.bool_type(),
        Type::I8 | Type::U8 => ctx.i8_type(),
        Type::I16 | Type::U16 => ctx.i16_type(),
        Type::I32 | Type::U32 => ctx.i32_type(),
        Type::I64 | Type::U64 => ctx.i64_type(),
        Type::Void | Type::Ptr(_) | Type::Fn(_, _) | Type::Struct(_) | Type::Union(_) => {
            panic!("not an integer type")
        }
    }
}

/// Resolve a [`Type`] to a LLVM [`BasicTypeEnum`]
///
/// # Panics
/// Panics if `ty` is not a basic type
pub fn llvm_basic_type<'ctx>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    ty: Type,
) -> BasicTypeEnum<'ctx> {
    match ty {
        Type::Bool
        | Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::I32
        | Type::U32
        | Type::I64
        | Type::U64 => llvm_int_type(ctx, ty).as_basic_type_enum(),
        Type::Void => panic!("void is not a basic type"),
        Type::Ptr(x) => create_ptr(llvm_type(ctx, target_machine, *x)).as_basic_type_enum(),
        Type::Fn(_, _) => panic!("function is not a basic type"),
        Type::Struct(fields) => ctx
            .struct_type(
                &fields
                    .into_iter()
                    .map(|(_, key_ty)| llvm_basic_type(ctx, target_machine, key_ty))
                    .collect::<Vec<_>>(),
                false,
            )
            .as_basic_type_enum(),
        Type::Union(fields) => {
            // Determine which field has the largest size. This is what we will allocate.
            let largest_field = fields
                .into_iter()
                .map(|(_, ty)| {
                    let ty = llvm_basic_type(ctx, target_machine, ty);
                    let size = target_machine.get_target_data().get_bit_size(&ty);
                    (ty, size)
                })
                .max_by_key(|(_, size)| *size)
                .unwrap_or_else(|| {
                    // this is basically `never`
                    let ty = ctx.struct_type(&[], false).as_basic_type_enum();
                    (ty, target_machine.get_target_data().get_bit_size(&ty))
                });

            largest_field.0
        }
    }
}

/// Resolve a [`Type`] to a LLVM [`AnyTypeEnum`]
pub fn llvm_type<'ctx>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    ty: Type,
) -> AnyTypeEnum<'ctx> {
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
        | Type::Union(_) => llvm_basic_type(ctx, target_machine, ty).as_any_type_enum(),

        Type::Void => ctx.void_type().as_any_type_enum(),

        Type::Fn(args, ret) => create_fn(
            llvm_type(ctx, target_machine, ret.into_tast_type()),
            &args
                .clone()
                .into_arguments()
                .into_iter()
                .map(|arg| llvm_basic_type(ctx, target_machine, arg.ty).into())
                .collect::<Vec<_>>(),
            args.is_variadic(),
        )
        .as_any_type_enum(),
    }
}
