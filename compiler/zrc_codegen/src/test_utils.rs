//! Extra utilities to help in testing the code generator
//!
//! Only available on crate feature `test`.

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, InstructionValue},
};

use crate::{stmt::cg_init_fn, CgScope};

/// Added extension methods to [`BasicBlock`] to help in testing
pub trait BasicBlockExt<'ctx> {
    /// Get a list of [`InstructionValue`]s from this [`BasicBlock`]
    ///
    /// This is helpful in tests to see what instructions are generated
    /// within a specific basic block. It is as if you called `get_first_instruction()` and then
    /// `get_next_instruction()` on each instruction until you reach the end of the block.
    fn get_instructions(&self) -> Vec<InstructionValue<'ctx>>;
}
impl<'ctx> BasicBlockExt<'ctx> for BasicBlock<'ctx> {
    fn get_instructions(&self) -> Vec<InstructionValue<'ctx>> {
        std::iter::successors(self.get_first_instruction(), |instr| {
            instr.get_next_instruction()
        })
        .collect()
    }
}

/// Initialize a new LLVM [`Builder`], [`Module`],
/// [`FunctionValue`], [`CgScope`], and [`BasicBlock`] within that function.
/// This allows you to generate code within a function without having to insert the same boilerplate each time.
///
/// This is the equivalent of generating:
/// ```zr
/// fn test() {
///     // << builder position
/// }
/// ```
/// in LLVM.
// Because creating builder/module/etc requires borrowing context, we must expect a context to be passed in and cannot create it as we would be returning a reference to a temporary
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn initialize_test_function(
    ctx: &Context,
) -> (
    Builder<'_>,
    Module<'_>,
    FunctionValue<'_>,
    CgScope<'static, '_>,
    BasicBlock<'_>,
) {
    let builder = ctx.create_builder();
    let module = ctx.create_module("main");

    let mut global_scope = CgScope::new();

    let fn_value = cg_init_fn(ctx, &module, "test", None, vec![], false);
    global_scope.insert("test", fn_value.as_global_value().as_pointer_value());
    // must come after the insert call so that recursion is valid
    let fn_scope = global_scope.clone();

    let bb = ctx.append_basic_block(fn_value, "entry");

    (builder, module, fn_value, fn_scope, bb)
}
