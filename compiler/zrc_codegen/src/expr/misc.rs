//! code generation for misc expressions

use indexmap::IndexMap;
use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum},
};
use zrc_typeck::tast::{expr::TypedExpr, ty::Type};
use zrc_utils::span::Spanned;

use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::{CgExprArgs, cg_expr},
    ty::{llvm_basic_type, llvm_int_type},
    unpack,
};

/// Generate LLVM IR for a cast expression
pub fn cg_cast<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    x: Box<TypedExpr<'input>>,
    ty: &Spanned<Type<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
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

    let x = unpack!(bb = cg_expr(cg, bb, *x));

    let reg = match (
        x.get_type().is_pointer_type(),
        matches!(ty.value(), Type::Ptr(_)),
    ) {
        (true, true) => cg
            .builder
            .build_bit_cast(
                x.into_pointer_value(),
                llvm_basic_type(&cg, ty.value()).0,
                "cast",
            )
            .expect("bitcast should have compiled successfully"),
        (true, false) => cg
            .builder
            .build_ptr_to_int(
                x.into_pointer_value(),
                llvm_int_type(&cg, ty.value()).0,
                "cast",
            )
            .expect("ptrtoint should have compiled successfully")
            .as_basic_value_enum(),
        (false, true) => cg
            .builder
            .build_int_to_ptr(
                x.into_int_value(),
                llvm_basic_type(&cg, ty.value()).0.into_pointer_type(),
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
                        llvm_basic_type(&cg, ty.value()).0.into_int_type(),
                        "cast",
                    )
                    .expect("sext should have compiled successfully")
                    .as_basic_value_enum(),

                // (x is signed, target is signed or unsigned)
                (false, _) => cg
                    .builder
                    .build_int_z_extend(
                        x.into_int_value(),
                        llvm_basic_type(&cg, ty.value()).0.into_int_type(),
                        "cast",
                    )
                    .expect("zext should have compiled successfully")
                    .as_basic_value_enum(),
            }
        }
        (false, false) => {
            // Other casts are just bitcasts
            cg.builder
                .build_bit_cast(
                    x.into_int_value(),
                    llvm_basic_type(&cg, ty.value()).0,
                    "cast",
                )
                .expect("bitcast should have compiled successfully")
                .as_basic_value_enum()
        }
    };

    bb.and(reg)
}

/// Generate LLVM IR for a sizeof expression
pub fn cg_size_of<'ctx, 'input>(
    CgExprArgs { cg, bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    ty: &Type<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let reg = llvm_basic_type(&cg, ty)
        .0
        .size_of()
        .expect("size_of should have compiled successfully")
        .as_basic_value_enum();

    bb.and(reg)
}

/// Generate LLVM IR for a struct or union construction expression
pub fn cg_struct_construction<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    fields: &IndexMap<&'input str, TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    match &inferred_type {
        Type::Struct(field_types) => {
            // Get the LLVM struct type
            let struct_type = llvm_basic_type(&cg, &inferred_type).0.into_struct_type();

            // Allocate space for the struct on the stack
            let struct_ptr = cg
                .builder
                .build_alloca(struct_type, "struct_tmp")
                .expect("struct allocation should have compiled successfully");

            // Initialize each field
            for (idx, (field_name, _field_ty)) in field_types.iter().enumerate() {
                if let Some(field_expr) = fields.get(field_name) {
                    // Evaluate the field value
                    let field_value = unpack!(bb = cg_expr(cg, bb, field_expr.clone()));

                    // Get pointer to this field in the struct
                    #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
                    let field_ptr = cg
                        .builder
                        .build_struct_gep(struct_type, struct_ptr, idx as u32, "field_ptr")
                        .expect("struct GEP should have compiled successfully");

                    // Store the value
                    cg.builder
                        .build_store(field_ptr, field_value)
                        .expect("store should have compiled successfully");
                }
            }

            // Load the complete struct value
            let reg = cg
                .builder
                .build_load(struct_type, struct_ptr, "struct_val")
                .expect("load should have compiled successfully");

            bb.and(reg)
        }
        Type::Union(field_types) => {
            // For unions, the LLVM type is the largest field type, not a struct
            let union_type = llvm_basic_type(&cg, &inferred_type).0;

            // Allocate space for the union on the stack
            let union_ptr = cg
                .builder
                .build_alloca(union_type, "union_tmp")
                .expect("union allocation should have compiled successfully");

            // Initialize the union with the provided field (if any)
            // In unions, all fields share the same memory space
            for (field_name, _field_ty) in field_types.iter() {
                if let Some(field_expr) = fields.get(field_name) {
                    // Evaluate the field value
                    let field_value = unpack!(bb = cg_expr(cg, bb, field_expr.clone()));

                    // For unions, we need to bitcast the pointer to the field's type
                    // and then store the value
                    let field_ptr = cg
                        .builder
                        .build_bit_cast(
                            union_ptr,
                            field_value.get_type().ptr_type(inkwell::AddressSpace::default()),
                            "union_field_ptr",
                        )
                        .expect("bitcast should have compiled successfully")
                        .into_pointer_value();

                    // Store the value
                    cg.builder
                        .build_store(field_ptr, field_value)
                        .expect("store should have compiled successfully");

                    // Only initialize one field for a union
                    break;
                }
            }

            // Load the complete union value
            let reg = cg
                .builder
                .build_load(union_type, union_ptr, "union_val")
                .expect("load should have compiled successfully");

            bb.and(reg)
        }
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::I32
        | Type::U32
        | Type::I64
        | Type::U64
        | Type::Usize
        | Type::Isize
        | Type::Bool
        | Type::Int
        | Type::Ptr(_)
        | Type::Fn(_)
        | Type::Opaque(_) => {
            unreachable!("struct construction should only be used with struct/union types")
        }
    }
}
