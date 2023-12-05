//! Code generation for entire Zirco programs

use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::AnyType,
    values::{BasicValueEnum, FunctionValue},
    OptimizationLevel,
};
use zrc_typeck::tast::{
    stmt::{ArgumentDeclaration, TypedDeclaration},
    ty::Type,
};

use super::stmt::cg_block;
use crate::{
    ty::{create_fn, llvm_basic_type, llvm_type},
    CgContext, CgScope,
};

/// Initialize the LLVM [`FunctionValue`] for a given function prototype
pub fn cg_init_fn<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    target_machine: &TargetMachine,
    name: &str,
    ret: Option<Type>,
    args: &[&Type],
    is_variadic: bool,
) -> FunctionValue<'ctx> {
    let ret_type = llvm_type(ctx, target_machine, &ret.unwrap_or(Type::Void));
    let arg_types = args
        .iter()
        .map(|ty| llvm_basic_type(ctx, target_machine, ty).into())
        .collect::<Vec<_>>();

    let fn_type = create_fn(
        ret_type.as_any_type_enum(),
        arg_types.as_slice(),
        is_variadic,
    );
    let fn_val = module.add_function(name, fn_type, None);

    fn_val
}

/// Run optimizations on the given program.
fn optimize_module(module: &Module<'_>, optimization_level: OptimizationLevel) {
    let pmb = PassManagerBuilder::create();
    pmb.set_optimization_level(optimization_level);
    let pm = PassManager::<Module>::create(());
    pmb.populate_module_pass_manager(&pm);
    pm.run_on(module);
}

/// Code generate and verify a program given a [`Context`] and return the final
/// LLVM [`Module`] as a result, fully optimized and ready for printing as an IR
/// or compiling to assembly/native code.
///
/// # Panics
/// Panics if code generation fails. This can be caused by an invalid TAST being
/// passed, so make sure to type check it so invariants are upheld.
#[must_use]
fn cg_program<'ctx>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    optimization_level: OptimizationLevel,
    module_name: &str,
    program: Vec<TypedDeclaration<'_>>,
) -> Module<'ctx> {
    let builder = ctx.create_builder();
    let module = ctx.create_module(module_name);

    let mut global_scope = CgScope::new();

    for declaration in program {
        match declaration {
            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                let fn_value = cg_init_fn(
                    ctx,
                    &module,
                    target_machine,
                    name.value(),
                    return_type.map(zrc_utils::span::Spanned::into_value),
                    parameters
                        .value()
                        .as_arguments()
                        .iter()
                        .map(|ArgumentDeclaration { ty, .. }| ty.value())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    parameters.value().is_variadic(),
                );
                global_scope.insert(name.value(), fn_value.as_global_value().as_pointer_value());
                // must come after the insert call so that recursion is valid
                let mut fn_scope = global_scope.clone();

                if let Some(body) = body {
                    let entry = ctx.append_basic_block(fn_value, "entry");
                    builder.position_at_end(entry);

                    for (n, ArgumentDeclaration { name, ty }) in
                        parameters.value().as_arguments().iter().enumerate()
                    {
                        if entry.get_first_instruction().is_some() {
                            builder.position_before(&entry.get_first_instruction().expect(
                                ".gfi.is_some() should only return true if there is an instruction",
                            ));
                        } else {
                            builder.position_at_end(entry);
                        }

                        let alloc = builder
                            .build_alloca(
                                llvm_basic_type(ctx, target_machine, ty.value()),
                                &format!("arg_{}", name.value()),
                            )
                            .expect("alloca should generate successfully");

                        builder.position_at_end(entry);

                        builder
                            .build_store::<BasicValueEnum>(
                                alloc,
                                fn_value
                                    .get_nth_param(
                                        n.try_into()
                                            .expect("over u32::MAX parameters in a function? HOW?"),
                                    )
                                    .expect("nth parameter from fn type should exist in fn value"),
                            )
                            .expect("store should generate successfully");

                        fn_scope.insert(name.value(), alloc);
                    }

                    cg_block(
                        CgContext {
                            ctx,
                            target_machine,
                            builder: &builder,
                            module: &module,
                            fn_value,
                        },
                        entry,
                        &fn_scope,
                        body.into_value(),
                        &None,
                    );
                }
            }
        }
    }

    match module.verify() {
        Ok(()) => {}

        Err(error_as_llvm_string) => {
            panic!(
                "code generation failure:\n{}\nGenerated IR:\n{}",
                error_as_llvm_string.to_string(),
                module.print_to_string().to_string()
            );
        }
    }

    optimize_module(&module, optimization_level);

    module
}

/// Code generate a LLVM program to a string.
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
pub fn cg_program_to_string(
    module_name: &str,
    program: Vec<TypedDeclaration>,
    optimization_level: OptimizationLevel,
    triple: &TargetTriple,
    cpu: &str,
) -> String {
    let ctx = Context::create();

    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(triple).expect("target should be ready and exist");

    let target_machine = target
        .create_target_machine(
            triple,
            cpu,
            "",
            // FIXME: Does this potentially run the optimizer twice (as we run it ourselves later)?
            // That may be inefficient.
            optimization_level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("target machine should be created successfully");

    let module = cg_program(
        &ctx,
        &target_machine,
        optimization_level,
        module_name,
        program,
    );

    module.print_to_string().to_string()
}

/// Code generate a LLVM program to a [`MemoryBuffer`] based on the given
/// [`FileType`].
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
pub fn cg_program_to_buffer(
    module_name: &str,
    program: Vec<TypedDeclaration>,
    file_type: FileType,
    optimization_level: OptimizationLevel,
    triple: &TargetTriple,
    cpu: &str,
) -> MemoryBuffer {
    let ctx = Context::create();

    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(triple).expect("target should be ready and exist");

    let target_machine = target
        .create_target_machine(
            triple,
            cpu,
            "",
            // FIXME: Does this potentially run the optimizer twice (as we run it ourselves later)?
            // That may be inefficient.
            optimization_level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("target machine should be created successfully");

    let module = cg_program(
        &ctx,
        &target_machine,
        optimization_level,
        module_name,
        program,
    );

    target_machine
        .write_to_memory_buffer(&module, file_type)
        .expect("writing to memory buffer should succeed")
}
