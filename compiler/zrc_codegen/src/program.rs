//! Code generation for entire Zirco programs

use inkwell::{
    context::Context,
    debug_info::{
        AsDIScope, DICompileUnit, DISubprogram, DWARFEmissionKind, DWARFSourceLanguage,
        DebugInfoBuilder,
    },
    memory_buffer::MemoryBuffer,
    module::{FlagBehavior, Module},
    passes::{PassManager, PassManagerBuilder},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{AnyType, BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue},
    OptimizationLevel,
};
use zrc_typeck::tast::{
    stmt::{ArgumentDeclaration, TypedDeclaration},
    ty::Type,
};
use zrc_utils::span::Spanned;

use super::stmt::cg_block;
use crate::{
    ty::{create_fn, llvm_basic_type, llvm_type},
    CgContext, CgLineLookup, CgScope,
};

/// Initialize the LLVM [`FunctionValue`] for a given function prototype
/// This should only be used when generating **extern** declarations, as
/// it does not produce the needed [`DISubprogram`] for debugging.
/// Use [`cg_init_fn`] instead for definitions.
/// We do not attach debugging info to extern functions, to follow with clang's
/// (probably correct) behavior.
#[allow(clippy::too_many_arguments)]
pub fn cg_init_extern_fn<'ctx>(
    ctx: &'ctx Context,
    dbg_builder: &DebugInfoBuilder<'ctx>,
    compilation_unit: &DICompileUnit<'ctx>,
    module: &Module<'ctx>,
    target_machine: &TargetMachine,
    name: &str,
    ret: Option<Type>,
    args: &[&Type],
    is_variadic: bool,
) -> FunctionValue<'ctx> {
    let (ret_type, ret_dbg_type) = llvm_type(
        compilation_unit.get_file(),
        dbg_builder,
        ctx,
        target_machine,
        &ret.unwrap_or(Type::Void),
    );
    let (arg_types, arg_dbg_types): (Vec<_>, Vec<_>) = args
        .iter()
        .map(|ty| {
            let (ty, dbg_ty) = llvm_basic_type(
                compilation_unit.get_file(),
                dbg_builder,
                ctx,
                target_machine,
                ty,
            );
            (
                <BasicTypeEnum as Into<BasicMetadataTypeEnum>>::into(ty),
                dbg_ty,
            )
        })
        .unzip();

    let (fn_type, _, _) = create_fn(
        dbg_builder,
        compilation_unit.get_file(),
        ret_type.as_any_type_enum(),
        ret_dbg_type,
        arg_types.as_slice(),
        arg_dbg_types.as_slice(),
        is_variadic,
    );

    let fn_val = module.add_function(name, fn_type, None);

    fn_val
}

/// Same as [`cg_init_extern_fn`] but properly initializes function
/// *definitions* with their debugging information.
#[allow(clippy::too_many_arguments)]
pub fn cg_init_fn<'ctx>(
    ctx: &'ctx Context,
    dbg_builder: &DebugInfoBuilder<'ctx>,
    compilation_unit: &DICompileUnit<'ctx>,
    module: &Module<'ctx>,
    target_machine: &TargetMachine,
    name: &str,
    line_no: u32,
    ret: Option<Type>,
    args: &[&Type],
    is_variadic: bool,
) -> (FunctionValue<'ctx>, DISubprogram<'ctx>) {
    let (ret_type, ret_dbg_type) = llvm_type(
        compilation_unit.get_file(),
        dbg_builder,
        ctx,
        target_machine,
        &ret.unwrap_or(Type::Void),
    );
    let (arg_types, arg_dbg_types): (Vec<_>, Vec<_>) = args
        .iter()
        .map(|ty| {
            let (ty, dbg_ty) = llvm_basic_type(
                compilation_unit.get_file(),
                dbg_builder,
                ctx,
                target_machine,
                ty,
            );
            (
                <BasicTypeEnum as Into<BasicMetadataTypeEnum>>::into(ty),
                dbg_ty,
            )
        })
        .unzip();

    let (fn_type, fn_dbg_subroutine, _fn_dbg_type) = create_fn(
        dbg_builder,
        compilation_unit.get_file(),
        ret_type.as_any_type_enum(),
        ret_dbg_type,
        arg_types.as_slice(),
        arg_dbg_types.as_slice(),
        is_variadic,
    );

    let fn_subprogram = dbg_builder.create_function(
        compilation_unit.as_debug_info_scope(),
        name,
        None,
        compilation_unit.get_file(),
        line_no,
        fn_dbg_subroutine,
        // REVIEW: Are these values correct? They're the only values that seem to prevent invalid
        // codegen
        // This is not local to our unit -- it is exported.
        false,
        // This is, in fact, a definition.
        true,
        line_no,
        0,
        false,
    );

    let fn_val = module
        .get_function(name)
        .unwrap_or_else(|| module.add_function(name, fn_type, None));
    fn_val.set_subprogram(fn_subprogram);

    (fn_val, fn_subprogram)
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
#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
fn cg_program<'ctx>(
    frontend_version_string: &str,
    cli_args: &str,
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    optimization_level: OptimizationLevel,
    debug_level: DWARFEmissionKind,
    parent_directory: &str,
    file_name: &str,
    line_lookup: &CgLineLookup,
    program: Vec<Spanned<TypedDeclaration<'_>>>,
) -> Module<'ctx> {
    let builder = ctx.create_builder();
    let module = ctx.create_module(file_name);

    let debug_metadata_version = ctx.i32_type().const_int(3, false);
    module.add_basic_value_flag(
        "Debug Info Version",
        FlagBehavior::Warning,
        debug_metadata_version,
    );

    let (dbg_builder, compilation_unit) = module.create_debug_info_builder(
        true,
        // closest equivalent to Zirco
        DWARFSourceLanguage::C,
        file_name,
        parent_directory,
        frontend_version_string,
        false,
        // We do not directly obtain the args here because the test executables have a path in them
        // and that would change and mess up snapshotflagting
        cli_args,
        0,
        "",
        debug_level,
        0,
        false,
        false,
        "",
        "",
    );

    let mut global_scope = CgScope::new();

    for declaration in program {
        let span = declaration.span();

        match declaration.into_value() {
            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: Some(body),
            } => {
                let body_span = body.span();

                let (fn_value, fn_subprogram) = cg_init_fn(
                    ctx,
                    &dbg_builder,
                    &compilation_unit,
                    &module,
                    target_machine,
                    name.value(),
                    line_lookup.lookup_from_index(span.start()).line,
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

                let entry = ctx.append_basic_block(fn_value, "entry");
                builder.position_at_end(entry);

                let line_and_col = line_lookup.lookup_from_index(body_span.start());

                let lexical_block = dbg_builder.create_lexical_block(
                    fn_subprogram.as_debug_info_scope(),
                    compilation_unit.get_file(),
                    line_and_col.line,
                    line_and_col.col,
                );

                let debug_location = dbg_builder.create_debug_location(
                    ctx,
                    line_and_col.line,
                    line_and_col.col,
                    lexical_block.as_debug_info_scope(),
                    None,
                );
                builder.set_current_debug_location(debug_location);

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

                    let (ty, dbg_ty) = llvm_basic_type(
                        compilation_unit.get_file(),
                        &dbg_builder,
                        ctx,
                        target_machine,
                        ty.value(),
                    );

                    let alloc = builder
                        .build_alloca(ty, &format!("arg_{name}"))
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

                    let ident_line_col = line_lookup.lookup_from_index(name.start());

                    let id_debug_location = dbg_builder.create_debug_location(
                        ctx,
                        ident_line_col.line,
                        ident_line_col.col,
                        lexical_block.as_debug_info_scope(),
                        None,
                    );

                    let decl = dbg_builder.create_parameter_variable(
                        fn_subprogram.as_debug_info_scope(),
                        name.value(),
                        u32::try_from(n)
                            .expect("should not be more than u32::MAX args in a function"),
                        compilation_unit.get_file(),
                        ident_line_col.line,
                        dbg_ty,
                        true,
                        0,
                    );

                    dbg_builder.insert_declare_at_end(
                        alloc,
                        Some(decl),
                        None,
                        id_debug_location,
                        entry,
                    );

                    fn_scope.insert(name.value(), alloc);
                }

                cg_block(
                    CgContext {
                        ctx,
                        target_machine,
                        builder: &builder,
                        dbg_builder: &dbg_builder,
                        compilation_unit: &compilation_unit,
                        module: &module,
                        fn_value,
                        line_lookup,
                    },
                    entry,
                    &fn_scope,
                    lexical_block,
                    body,
                    &None,
                );
            }
            // We do not attach debugging information to extern functions, this is clang's behavior
            // so I assume it's correct.
            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: None,
            } => {
                let fn_value = cg_init_extern_fn(
                    ctx,
                    &dbg_builder,
                    &compilation_unit,
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
            }
        }
    }

    dbg_builder.finalize();

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
#[allow(clippy::too_many_arguments)]
pub fn cg_program_to_string(
    frontend_version_string: &str,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    source: &str,
    program: Vec<Spanned<TypedDeclaration>>,
    optimization_level: OptimizationLevel,
    debug_level: DWARFEmissionKind,
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
        frontend_version_string,
        cli_args,
        &ctx,
        &target_machine,
        optimization_level,
        debug_level,
        parent_directory,
        file_name,
        &CgLineLookup::new(source),
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
#[allow(clippy::too_many_arguments)]
pub fn cg_program_to_buffer(
    frontend_version_string: &str,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    source: &str,
    program: Vec<Spanned<TypedDeclaration>>,
    file_type: FileType,
    optimization_level: OptimizationLevel,
    debug_level: DWARFEmissionKind,
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
        frontend_version_string,
        cli_args,
        &ctx,
        &target_machine,
        optimization_level,
        debug_level,
        parent_directory,
        file_name,
        &CgLineLookup::new(source),
        program,
    );

    target_machine
        .write_to_memory_buffer(&module, file_type)
        .expect("writing to memory buffer should succeed")
}
