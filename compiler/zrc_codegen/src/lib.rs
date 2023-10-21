use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    values::{AnyValueEnum, BasicValue},
};
use zrc_typeck::tast::expr::{TypedExpr, TypedExprKind};

fn cg_expr<'ctx, 'a>(
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    expr: TypedExpr,
) -> (BasicBlock<'ctx>, AnyValueEnum<'ctx>) {
}
