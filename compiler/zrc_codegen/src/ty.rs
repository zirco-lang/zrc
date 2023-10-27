use inkwell::{
    context::Context,
    types::{
        AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        IntType, PointerType,
    },
    AddressSpace,
};
use zrc_typeck::tast::ty::Type;

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

pub fn llvm_int_type<'ctx>(ctx: &'ctx Context, ty: Type) -> IntType<'ctx> {
    match ty {
        Type::Bool => ctx.bool_type(),
        Type::I8 | Type::U8 => ctx.i8_type(),
        Type::I16 | Type::U16 => ctx.i16_type(),
        Type::I32 | Type::U32 => ctx.i32_type(),
        Type::I64 | Type::U64 => ctx.i64_type(),
        Type::Void | Type::Ptr(_) | Type::Fn(_, _) | Type::Struct(_) => {
            panic!("not an integer type")
        }
    }
}

pub fn llvm_basic_type<'ctx>(ctx: &'ctx Context, ty: Type) -> BasicTypeEnum<'ctx> {
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
        Type::Ptr(x) => create_ptr(llvm_type(ctx, *x)).as_basic_type_enum(),
        Type::Fn(_, _) => panic!("function is not a basic type"),
        Type::Struct(fields) => ctx
            .struct_type(
                &fields
                    .into_iter()
                    .map(|(_, key_ty)| llvm_basic_type(ctx, key_ty))
                    .collect::<Vec<_>>(),
                false,
            )
            .as_basic_type_enum(),
    }
}

pub fn llvm_type<'ctx>(ctx: &'ctx Context, ty: Type) -> AnyTypeEnum<'ctx> {
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
        | Type::Struct(_) => llvm_basic_type(ctx, ty).as_any_type_enum(),

        Type::Void => ctx.void_type().as_any_type_enum(),

        Type::Fn(args, ret) => create_fn(
            llvm_type(ctx, ret.into_tast_type()),
            &args
                .clone()
                .into_arguments()
                .into_iter()
                .map(|arg| llvm_basic_type(ctx, arg.ty).into())
                .collect::<Vec<_>>(),
            args.is_variadic(),
        )
        .as_any_type_enum(),
    }
}
