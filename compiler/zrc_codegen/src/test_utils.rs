//! Extra utilities to help in testing the code generator
//!
//! Only available on crate feature `test`.
//!
//! # Common patterns in tests
//! The old form of unit tests for `zrc_codegen` involved directly invoking the functions we are
//! testing, which meant we needed to prepare all of the needed scope for inkwell and this led to
//! very complicated and long tests (with most averaging at least 100 lines). This deterred me from
//! testing as often as I wanted, and many bugs were not found because of this.
//!
//! Instead, we now make our unit tests entire *programs* that we compile through the same process
//! that the frontend uses, and we snapshot test the result. However, this almost turns out to be
//! an integration test, and not a unit test! The reason I still consider these to be unit tests
//! is because the code is short and meant to invoke a very specific block of code in whatever
//! function we are testing, and not a full program. We will have full integration tests in the
//! future.
//!
//! Read any test for an example of how they flow. We recommend `cargo-insta` be installed so you
//! can `cargo insta review` any changed snapshots.

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

/// Creates a snapshot test given a valid input program
#[macro_export]
macro_rules! cg_snapshot_test {
    ($source:expr) => {
        let resulting_ir = cg_program_to_string(
            "test",
            typeck::type_program(::zrc_parser::parser::parse_program($source).unwrap()).unwrap(),
            ::inkwell::OptimizationLevel::None,
            &$crate::get_native_triple(),
            "",
        );

        insta::with_settings!({
            description => $source,
        }, {
            insta::assert_snapshot!(resulting_ir);
        });
    }
}
