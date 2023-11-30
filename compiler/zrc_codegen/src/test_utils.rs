//! Extra utilities to help in testing the code generator
//!
//! Only available on crate feature `test`.
//!
//! # Common patterns in tests
//! Because we use `inkwell` to generate LLVM IR, it is often not possible to
//! directly compare IRs with each other in tests. For this reason, testing the
//! code generator can be quite difficult and there are a few common patterns we
//! use to make these tests simpler and more consistent with each other. If a
//! test does not follow these paradigms, you will usually find a comment
//! explaining why.
//!
//! First of all, because all LLVM structures borrow from the [`Context`]
//! instance, every function will begin with the initialization of a
//! [`Context`]. This one context belongs to a single test, and there should
//! almost never exist two contexts within one test. This one context will
//! generate two [`Module`]s, both named `test`, which we will explore in more
//! detail in a few paragraphs.
//!
//! Every test also contains a "prelude." The prelude is a closure returned from
//! [`make_test_prelude_closure`] which takes the [`Context`], calls
//! [`initialize_test_function`] to obtain the required structures, and then
//! calls a closure passed to its argument that will allow you to perform any
//! needed test setup.
//!
//! This function may be used to generate the state of code generation before
//! your test, such as any needed allocations. It can also mutate the scope and
//! do any other needed setup.
//!
//! This will then be followed by two blocks statements bound to `expected` and
//! `actual`. These will both call the prelude function to obtain the initial
//! state, and then perform some mutations. These blocks can then return
//! whatever values are to be compared, usually as a tuple with the first
//! element being `module.print_to_string()` and the second being something like
//! the return value of the function we are testing.
//!
//! **Keep in mind that the items being compared should NEVER contain LLVM
//! structures.** They should at the most consist of
//! [`inkwell::support::LLVMString`]s, because all LLVM structures borrow from
//! the [`Module`] along with the [`Context`] and will segfault if the
//! [`Module`] is dropped and then you run a comparison on them. You must print
//! them to strings.
//!
//! It has been decided this test architecture is cleanest for allowing us to
//! test the equality of IRs.
//!
//! tl;dr: Context is created, prelude will initialize both expected/actual IRs,
//! and then we compare the stringified value of both.
//!
//! For a simple test example, read
//! [`crate::expr::tests::cg_place::identifier_registers_are_returned_as_is`].

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    values::FunctionValue,
    OptimizationLevel,
};
use zrc_typeck::{
    tast::{stmt::ArgumentDeclarationList, ty::Type},
    typeck::BlockReturnType,
};

use crate::{stmt::cg_init_fn, CgScope};

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
    TargetMachine,
    Builder,
    Module,
    FunctionValue,
    CgScope<'static, '_>,
    BasicBlock,
) {
    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();

    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "native",
            "",
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let module = ctx.create_module("test");

    let builder = ctx.create_builder();

    let mut global_scope = CgScope::new();

    let fn_value = cg_init_fn(ctx, &module, &target_machine, "test", None, &[], false);
    global_scope.insert("test", fn_value.as_global_value().as_pointer_value());
    // must come after the insert call so that recursion is valid
    let fn_scope = global_scope.clone();

    let bb = ctx.append_basic_block(fn_value, "entry");
    builder.position_at_end(bb);

    (target_machine, builder, module, fn_value, fn_scope, bb)
}

/// Generate a closure that calls [`initialize_test_function`] and then calls
/// your provided initialization code with the generated structures, returning
/// these. Used for test prelude generation.
///
/// For example, this is used in
/// [`crate::expr::tests::cg_expr::comma_yields_right_value`]
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn make_test_prelude_closure<'ctx>(
    prelude_generator: impl Fn(
        &'ctx Context,
        &TargetMachine,
        &Builder<'ctx>,
        &Module<'ctx>,
        &FunctionValue<'ctx>,
        &mut CgScope<'static, 'ctx>,
        &BasicBlock<'ctx>,
    ) -> BasicBlock<'ctx>,
) -> impl Fn(
    &'ctx Context,
) -> (
    TargetMachine,
    Builder<'ctx>,
    Module<'ctx>,
    FunctionValue<'ctx>,
    CgScope<'static, 'ctx>,
    BasicBlock<'ctx>,
) {
    move |ctx| {
        let (target_machine, builder, module, fn_value, mut scope, bb) =
            initialize_test_function(ctx);

        let bb = prelude_generator(
            ctx,
            &target_machine,
            &builder,
            &module,
            &fn_value,
            &mut scope,
            &bb,
        );

        (target_machine, builder, module, fn_value, scope, bb)
    }
}

/// Creates an extern function with a desired name that acts as a no-op.
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn generate_nop_fn<'name, 'ctx>(
    name: &'name str,
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    tck_scope: &mut zrc_typeck::typeck::Scope<'name>,
    cg_scope: &mut CgScope<'name, 'ctx>,
) {
    let fn_val = module.add_function(name, ctx.void_type().fn_type(&[], false), None);

    cg_scope.insert(name, fn_val.as_global_value().as_pointer_value());

    tck_scope.set_value(
        name,
        Type::Fn(
            ArgumentDeclarationList::NonVariadic(vec![]),
            Box::new(BlockReturnType::Void),
        ),
    );
}

/// Creates an extern function with a desired name that returns a boolean.
// TODO: Merge with the above? Make generic for any type?
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn generate_boolean_yielding_fn<'name, 'ctx>(
    name: &'name str,
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    tck_scope: &mut zrc_typeck::typeck::Scope<'name>,
    cg_scope: &mut CgScope<'name, 'ctx>,
) {
    let fn_val = module.add_function(name, ctx.bool_type().fn_type(&[], false), None);

    cg_scope.insert(name, fn_val.as_global_value().as_pointer_value());

    tck_scope.set_value(
        name,
        Type::Fn(
            ArgumentDeclarationList::NonVariadic(vec![]),
            Box::new(BlockReturnType::Return(Type::Bool)),
        ),
    );
}

/// Creates an extern function with a desired name that returns an i32.
// TODO: Merge with the above? Make generic for any type?
#[allow(clippy::redundant_pub_crate)]
pub(crate) fn generate_i32_yielding_fn<'name, 'ctx>(
    name: &'name str,
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    tck_scope: &mut zrc_typeck::typeck::Scope<'name>,
    cg_scope: &mut CgScope<'name, 'ctx>,
) {
    let fn_val = module.add_function(name, ctx.i32_type().fn_type(&[], false), None);

    cg_scope.insert(name, fn_val.as_global_value().as_pointer_value());

    tck_scope.set_value(
        name,
        Type::Fn(
            ArgumentDeclarationList::NonVariadic(vec![]),
            Box::new(BlockReturnType::Return(Type::I32)),
        ),
    );
}
