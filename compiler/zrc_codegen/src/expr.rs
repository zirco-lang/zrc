//! Code generation for expressions
//!
//! This module contains functions to generate LLVM IR for
//! [`zrc_typeck::tast::expr::TypedExpr`] expressions, which represent
//! all expressions in the Zirco language.
//!
//! The main function is [`cg_expr`], which takes a `TypedExpr` and generates
//! the corresponding LLVM IR to compute its value.

mod arithmetic;
mod control;
mod increment_decrement;
mod literals;
mod logic;
mod mem;
mod misc;
pub mod place;

use inkwell::{basic_block::BasicBlock, debug_info::AsDIScope, values::BasicValueEnum};
use zrc_typeck::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type,
};
use zrc_utils::span::Span;

use crate::{bb::BasicBlockAnd, ctx::BlockCtx};

/// Arguments passed to the various expression code generators
struct CgExprArgs<'ctx, 'input, 'a> {
    /// The code generator context
    pub cg: BlockCtx<'ctx, 'input, 'a>,
    /// The current basic block
    pub bb: BasicBlock<'ctx>,
    /// The span of the expression being generated
    pub expr_span: Span,
    /// The inferred type of the expression
    pub inferred_type: Type<'input>,
}

/// Generate LLVM IR for a [`TypedExpr`], returning a [`BasicBlockAnd`] with the
/// resulting basic block and the computed value
#[expect(clippy::redundant_pub_crate)]
pub(crate) fn cg_expr<'ctx, 'input, 'a>(
    cg: BlockCtx<'ctx, 'input, 'a>,
    bb: BasicBlock<'ctx>,
    expr: TypedExpr<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let expr_span = expr.kind.span();
    let line_and_col = cg.line_lookup.lookup_from_index(expr_span.start());
    let debug_location = cg.dbg_builder.create_debug_location(
        cg.ctx,
        line_and_col.line,
        line_and_col.col,
        cg.dbg_scope.as_debug_info_scope(),
        None,
    );
    cg.builder.set_current_debug_location(debug_location);

    let ce = CgExprArgs::<'ctx, 'input, 'a> {
        cg,
        bb,
        expr_span,
        inferred_type: expr.inferred_type.clone(),
    };

    match expr.kind.into_value() {
        TypedExprKind::NumberLiteral(n, _) => literals::cg_number_literal(ce, &n),
        TypedExprKind::StringLiteral(str) => literals::cg_string_literal(ce, &str),
        TypedExprKind::CharLiteral(ch) => literals::cg_char_literal(ce, &ch),
        TypedExprKind::BooleanLiteral(value) => literals::cg_boolean_literal(ce, value),
        TypedExprKind::Identifier(id) => literals::cg_identifier(ce, id),

        TypedExprKind::Comma(lhs, rhs) => control::cg_comma(ce, lhs, rhs),

        TypedExprKind::Assignment(place, value) => mem::cg_assignment(ce, *place, value),

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            arithmetic::cg_binary_bitwise(ce, op, lhs, rhs)
        }

        TypedExprKind::Equality(op, lhs, rhs) => logic::cg_equality(ce, op, lhs, rhs),

        TypedExprKind::Comparison(op, lhs, rhs) => logic::cg_comparison(ce, op, lhs, rhs),

        TypedExprKind::Arithmetic(op, lhs, rhs) => arithmetic::cg_arithmetic(ce, op, lhs, rhs),

        TypedExprKind::Logical(op, lhs, rhs) => logic::cg_logical(ce, op, lhs, rhs),

        TypedExprKind::UnaryNot(x) => logic::cg_unary_not(ce, x),

        TypedExprKind::UnaryBitwiseNot(x) => arithmetic::cg_unary_bitwise_not(ce, x),

        TypedExprKind::UnaryMinus(x) => arithmetic::cg_unary_minus(ce, x),

        TypedExprKind::UnaryAddressOf(x) => mem::cg_address_of(ce, *x),

        TypedExprKind::UnaryDereference(ptr) => mem::cg_deref(ce, ptr),

        TypedExprKind::PrefixIncrement(place) => {
            increment_decrement::cg_prefix_increment(ce, *place)
        }

        TypedExprKind::PrefixDecrement(place) => {
            increment_decrement::cg_prefix_decrement(ce, *place)
        }

        TypedExprKind::Index(ptr, idx) => mem::cg_index(ce, ptr, idx),

        TypedExprKind::Dot(place, key) => mem::cg_dot(ce, place, key),

        TypedExprKind::Call(f, args) => control::cg_call(ce, *f, args),

        TypedExprKind::PostfixIncrement(place) => {
            increment_decrement::cg_postfix_increment(ce, *place)
        }

        TypedExprKind::PostfixDecrement(place) => {
            increment_decrement::cg_postfix_decrement(ce, *place)
        }

        TypedExprKind::Ternary(cond, lhs, rhs) => control::cg_ternary(ce, cond, lhs, rhs),
        TypedExprKind::Cast(x, ty) => misc::cg_cast(ce, x, &ty),
        TypedExprKind::SizeOf(ty) => misc::cg_size_of(ce, &ty),
        TypedExprKind::StructConstruction(fields) => misc::cg_struct_construction(ce, &fields),
    }
}
