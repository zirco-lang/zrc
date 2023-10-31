//! Extra utilities to help in testing the code generator
//!
//! Only available on crate feature `test`.
//!
//! # Common patterns in tests
//! Because we use `inkwell` to generate LLVM IR, it is often not possible to
//! directly compare IRs with each other in tests. For this reason, there are a
//! few common structures of code generator tests throughout the repository used
//! to keep our tests clean and structured.
//!
//! First of all, because all LLVM values borrow from the [`Context`] instance,
//! the rule of thumb is **one test, one context.** Even though tests may
//! consist of multiple [`Module`]s (one `expected`, one `actual`), there is
//! only one [`Context`] they are both constructed from. Keep in mind the tests
//! may only compare stringified IR and IR parts because the internal pointers
//! would be different.
//!
//! Then, there is a concept of the "prelude." The prelude function, often
//! called `generate_test_prelude()` takes all of the values it needs and will
//! generate the "prelude" for the test. This may involve a call to
//! [`initialize_test_function`] to obtain a bare-bones test function, but is
//! then followed by appending any needed statements onto the IR. For example, a
//! test that requires a variable in scope may build an `alloca` instruction and
//! add it to the [`CgScope`] instance. This function then returns all of the IR
//! components.
//!
//! Then you will usually find two block statements that are being bound with
//! `let`. One of them is `expected` and the other is `actual`. Both of these
//! will start off with a call to the test's specific `generate_test_prelude`,
//! and they will both end with the same return statement which returns an extra
//! value to compare, usually something like the yielded value of `cg_expr`.
//!
//! Then, at the end, a `strip` pass is ran on both modules so that we are
//! comparing the IR, not the names of the values within it. Finally we compare
//! both result values and the stringified stripped modules.
//!
//! It has been decided this test architecture is cleanest for allowing us to
//! test the equality of IRs.
//!
//! tl;dr: `generate_test_prelude` initializes the module and appends some
//! test-specific IR such as allocations. Then, two IRs are generated (expected
//! and actual) and stringified, to compare their generated outputs.
//! For a simple test example, read
//! [`crate::expr::tests::cg_place::identifier_registers_are_returned_as_is`].

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
    /// within a specific basic block. It is as if you called
    /// `get_first_instruction()` and then `get_next_instruction()` on each
    /// instruction until you reach the end of the block.
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

/// Initialize a new LLVM [`Builder`], [`FunctionValue`], [`CgScope`], and
/// [`BasicBlock`] within that function. This allows you to generate code within
/// a function without having to insert the same boilerplate each time.
///
/// This is the equivalent of generating:
/// ```zr
/// fn test() {
///     // << builder position
/// }
/// ```
/// in LLVM.
// Because creating builder/module/etc requires borrowing context, we must expect a context to be
// passed in and cannot create it as we would be returning a reference to a temporary
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn initialize_test_function(
    ctx: &Context,
) -> (
    Builder,
    Module,
    FunctionValue,
    CgScope<'static, '_>,
    BasicBlock,
) {
    let module = ctx.create_module("test");

    let builder = ctx.create_builder();

    let mut global_scope = CgScope::new();

    let fn_value = cg_init_fn(ctx, &module, "test", None, vec![], false);
    global_scope.insert("test", fn_value.as_global_value().as_pointer_value());
    // must come after the insert call so that recursion is valid
    let fn_scope = global_scope.clone();

    let bb = ctx.append_basic_block(fn_value, "entry");
    builder.position_at_end(bb);

    (builder, module, fn_value, fn_scope, bb)
}
