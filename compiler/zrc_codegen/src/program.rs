//! Code generation for entire Zirco programs
//!
//! This module contains the LLVM code generator for the Zirco compiler. It
//! translates the type-checked abstract syntax tree (TAST) into LLVM
//! intermediate representation (IR), which can then be optimized and compiled
//! to machine code.

use inkwell::{
    OptimizationLevel,
    context::Context,
    debug_info::{AsDIScope, DISubprogram, DWARFEmissionKind, DWARFSourceLanguage},
    memory_buffer::MemoryBuffer,
    module::{FlagBehavior, Module},
    passes::PassBuilderOptions,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{AnyType, BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue},
};
use zrc_typeck::tast::{
    stmt::{ArgumentDeclaration, TypedDeclaration},
    ty::Type,
};
use zrc_utils::{line_finder::LineLookup, span::Spanned};

use super::stmt::cg_block;
use crate::{
    ctx::{CompilationUnitCtx, FunctionCtx},
    scope::CgScope,
    ty::{create_fn, llvm_basic_type, llvm_type},
};

/// Evaluate a constant expression to an LLVM constant value.
/// This is used for global variable initializers.
///
/// # Panics
///
/// Panics if the expression is not a valid constant expression.
#[expect(clippy::wildcard_enum_match_arm)]
fn eval_const_expr<'ctx>(
    unit: &CompilationUnitCtx<'ctx, '_>,
    expr: &zrc_typeck::tast::expr::TypedExpr,
    ty: &Type,
) -> BasicValueEnum<'ctx> {
    use inkwell::types::StringRadix;
    use zrc_typeck::tast::expr::{NumberLiteral, TypedExprKind};

    match expr.kind.value() {
        TypedExprKind::NumberLiteral(n, _) => {
            let no_underscores = match n {
                NumberLiteral::Decimal(string)
                | NumberLiteral::Binary(string)
                | NumberLiteral::Hexadecimal(string) => string.replace('_', ""),
            };

            let radix = match n {
                NumberLiteral::Decimal(_) => StringRadix::Decimal,
                NumberLiteral::Binary(_) => StringRadix::Binary,
                NumberLiteral::Hexadecimal(_) => StringRadix::Hexadecimal,
            };

            let (llvm_ty, _) = llvm_basic_type(unit, ty);
            llvm_ty
                .into_int_type()
                .const_int_from_string(&no_underscores, radix)
                .expect("number literal should have parsed correctly")
                .as_basic_value_enum()
        }
        TypedExprKind::BooleanLiteral(value) => unit
            .ctx
            .bool_type()
            .const_int((*value).into(), false)
            .as_basic_value_enum(),
        TypedExprKind::StringLiteral(string) => {
            // Create a global constant string and return a pointer to it
            let bytes = string.as_bytes();
            let string_value = unit.ctx.const_string(bytes.as_bytes(), false);
            let global = unit
                .module
                .add_global(string_value.get_type(), None, ".str");
            global.set_initializer(&string_value);
            global.set_constant(true);
            global.as_pointer_value().as_basic_value_enum()
        }
        TypedExprKind::CharLiteral(ch) => unit
            .ctx
            .i8_type()
            .const_int(ch.as_byte().into(), false)
            .as_basic_value_enum(),
        _ => {
            // This should never happen as the type checker validates constant expressions
            panic!(
                "internal compiler error: non-constant expression in global initializer: {:?}",
                expr.kind.value()
            )
        }
    }
}

/// Initialize the LLVM [`FunctionValue`] for a given function prototype
/// This should only be used when generating **extern** declarations, as
/// it does not produce the needed [`DISubprogram`] for debugging.
/// Use [`cg_init_fn`] instead for definitions.
/// We do not attach debugging info to extern functions, to follow with clang's
/// (probably correct) behavior.
pub fn cg_init_extern_fn<'ctx>(
    unit: &CompilationUnitCtx<'ctx, '_>,
    name: &str,
    ret: &Type,
    args: &[&Type],
    is_variadic: bool,
) -> FunctionValue<'ctx> {
    let (ret_type, ret_dbg_type) = llvm_type(unit, ret);
    let (arg_types, arg_dbg_types): (Vec<_>, Vec<_>) = args
        .iter()
        .map(|ty| {
            let (ty, dbg_ty) = llvm_basic_type(unit, ty);
            (
                <BasicTypeEnum as Into<BasicMetadataTypeEnum>>::into(ty),
                dbg_ty,
            )
        })
        .unzip();

    let arg_dbg_types: Option<Vec<_>> = arg_dbg_types.iter().all(Option::is_some).then(|| {
        arg_dbg_types
            .into_iter()
            .map(|x| x.expect("we have DI"))
            .collect()
    });

    let (fn_type, _, _) = create_fn(
        unit,
        ret_type.as_any_type_enum(),
        ret_dbg_type,
        arg_types.as_slice(),
        arg_dbg_types.as_deref(),
        is_variadic,
    );

    unit.module.add_function(name, fn_type, None)
}

/// Same as [`cg_init_extern_fn`] but properly initializes function
/// *definitions* with their debugging information.
pub fn cg_init_fn<'ctx>(
    unit: &CompilationUnitCtx<'ctx, '_>,
    name: &str,
    line_no: u32,
    ret: &Type,
    args: &[&Type],
    is_variadic: bool,
) -> (FunctionValue<'ctx>, Option<DISubprogram<'ctx>>) {
    let (ret_type, ret_dbg_type) = llvm_type(unit, ret);
    let (arg_types, arg_dbg_types): (Vec<_>, Vec<_>) = args
        .iter()
        .map(|ty| {
            let (ty, dbg_ty) = llvm_basic_type(unit, ty);
            (
                <BasicTypeEnum as Into<BasicMetadataTypeEnum>>::into(ty),
                dbg_ty,
            )
        })
        .unzip();

    let arg_dbg_types: Option<Vec<_>> = arg_dbg_types.iter().all(Option::is_some).then(|| {
        arg_dbg_types
            .into_iter()
            .map(|x| x.expect("we have DI"))
            .collect()
    });

    let (fn_type, fn_dbg_subroutine, _fn_dbg_type) = create_fn(
        unit,
        ret_type.as_any_type_enum(),
        ret_dbg_type,
        arg_types.as_slice(),
        arg_dbg_types.as_deref(),
        is_variadic,
    );

    let fn_subprogram = unit.dbg_builder.as_ref().map(|dbg_builder| {
        dbg_builder.create_function(
            unit.compilation_unit
                .expect("We have a builder so must have a CU")
                .as_debug_info_scope(),
            name,
            None,
            unit.compilation_unit
                .expect("We have a builder so must have a CU")
                .get_file(),
            line_no,
            fn_dbg_subroutine.expect("we have DI"),
            // REVIEW: Are these values correct? They're the only values that seem to prevent
            // invalid codegen
            // This is not local to our unit -- it is exported.
            false,
            // This is, in fact, a definition.
            true,
            line_no,
            0,
            false,
        )
    });

    let fn_val = unit
        .module
        .get_function(name)
        .unwrap_or_else(|| unit.module.add_function(name, fn_type, None));

    if let Some(fn_subprogram) = fn_subprogram {
        fn_val.set_subprogram(fn_subprogram);
    }

    (fn_val, fn_subprogram)
}

/// Run optimizations on the given program.
fn optimize_module(module: &Module<'_>, tm: &TargetMachine, optimization_level: OptimizationLevel) {
    module
        .run_passes(
            match optimization_level {
                OptimizationLevel::None => "default<O0>",
                OptimizationLevel::Less => "default<O1>",
                OptimizationLevel::Default => "default<O2>",
                OptimizationLevel::Aggressive => "default<O3>",
            },
            tm,
            PassBuilderOptions::create(),
        )
        .expect("optimizing module should succeed");
}

/// Code generate and verify a program given a [`Context`] and return the final
/// LLVM [`Module`] as a result, sans optimization.
///
/// # Panics
/// Panics if code generation fails. This can be caused by an invalid TAST being
/// passed, so make sure to type check it so invariants are upheld.
#[must_use]
#[expect(clippy::too_many_lines, clippy::too_many_arguments)]
fn cg_program_without_optimization<'ctx>(
    frontend_version_string: &str,
    cli_args: &str,
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    debug_level: DWARFEmissionKind,
    parent_directory: &str,
    file_name: &str,
    line_lookup: &LineLookup,
    program: Vec<Spanned<TypedDeclaration<'_, '_>>>,
) -> Module<'ctx> {
    let builder = ctx.create_builder();
    let module = ctx.create_module(file_name);

    let debug_metadata_version = ctx.i32_type().const_int(3, false);

    if debug_level != DWARFEmissionKind::None {
        module.add_basic_value_flag(
            "Debug Info Version",
            FlagBehavior::Warning,
            debug_metadata_version,
        );
    }

    let (dbg_builder, compilation_unit) = match debug_level {
        DWARFEmissionKind::Full => {
            let (dbg, cu) = module.create_debug_info_builder(
                true,
                // closest equivalent to Zirco
                DWARFSourceLanguage::C,
                file_name,
                parent_directory,
                frontend_version_string,
                false,
                // We do not directly obtain the args here because the test executables have a path
                // in them and that would change and mess up snapshotting
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
            (Some(dbg), Some(cu))
        }
        DWARFEmissionKind::LineTablesOnly => {
            let (dbg, cu) = module.create_debug_info_builder(
                true,
                DWARFSourceLanguage::C,
                file_name,
                parent_directory,
                frontend_version_string,
                false,
                cli_args,
                0,
                "",
                DWARFEmissionKind::LineTablesOnly,
                0,
                false,
                false,
                "",
                "",
            );
            (Some(dbg), Some(cu))
        }
        DWARFEmissionKind::None => (None, None),
    };

    let unit = CompilationUnitCtx {
        builder: &builder,
        compilation_unit: compilation_unit.as_ref(),
        ctx,
        dbg_builder: dbg_builder.as_ref(),
        line_lookup,
        module: &module,
        target_machine,
    };

    let mut global_scope = CgScope::new();

    // =========================================================================
    // PASS 1: PROTOTYPES & GLOBALS
    // We scan the file to register all Function Names and Global Variables first.
    // This ensures that 'main' knows 'f' exists, even if 'f' is defined later.
    // =========================================================================
    for declaration in &program {
        let span = declaration.span();

        match declaration.value() {
            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                // Collect argument types for the function signature
                let arg_types: Vec<_> = parameters
                    .value()
                    .as_arguments()
                    .iter()
                    .map(|ArgumentDeclaration { ty, .. }| ty.value())
                    .collect();

                let is_variadic = parameters.value().is_variadic();

                // If it has a body, it's a normal function. If None, it's extern.
                match body {
                    Some(_) => {
                        let (fn_value, _fn_subprogram) = cg_init_fn(
                            &unit,
                            name.value(),
                            line_lookup.lookup_from_index(span.start()).line,
                            return_type.value(),
                            &arg_types,
                            is_variadic,
                        );
                        global_scope
                            .insert(name.value(), fn_value.as_global_value().as_pointer_value());
                    }
                    None => {
                        let fn_value = cg_init_extern_fn(
                            &unit,
                            name.value(),
                            return_type.value(),
                            &arg_types,
                            is_variadic,
                        );
                        global_scope
                            .insert(name.value(), fn_value.as_global_value().as_pointer_value());
                    }
                }
            }
            TypedDeclaration::GlobalLetDeclaration(declarations) => {
                // Generate global variables immediately (they don't have bodies)
                for let_decl in declarations {
                    let let_declaration = let_decl.value();
                    let (llvm_ty, _) = llvm_basic_type(&unit, &let_declaration.ty);

                    let global = module.add_global(llvm_ty, None, let_declaration.name.value());

                    let initializer = let_declaration.value.as_ref().map_or_else(
                        || llvm_ty.const_zero(),
                        |value| eval_const_expr(&unit, value, &let_declaration.ty),
                    );
                    global.set_initializer(&initializer);

                    global_scope.insert(let_declaration.name.value(), global.as_pointer_value());
                }
            }
        }
    }

    // PASS 2: FUNCTION BODIES
    // Now that all names are known, we go back and generate the code inside the functions.
    for declaration in program {
        // We only care about Functions with Bodies here.
        // Globals and Externs were finished in Pass 1.
        if let TypedDeclaration::FunctionDeclaration {
            name,
            parameters,
            body: Some(body),
            ..
        } = declaration.into_value()
        {
            let body_span = body.span();

            // Retrieve the function we created in Pass 1
            // We use the module to get it because it's cleaner than casting raw pointers
            let fn_value = module
                .get_function(name.value())
                .expect("Function should have been declared in Pass 1");

            //  RE-SETUP SCOPE FOR BODY GENERATION
            // We need to set up the arguments and debug info again for the builder context

            // must come after the insert call so that recursion is valid
            let mut fn_scope = global_scope.clone();

            // We need the subprogram for debug info. Since we don't have it easily from Pass 1,
            // we retrieve it from the LLVM function if debug info is enabled.
            let fn_subprogram = if debug_level != DWARFEmissionKind::None {
                fn_value.get_subprogram()
            } else {
                None
            };

            let entry = ctx.append_basic_block(fn_value, "entry");
            builder.position_at_end(entry);

            let line_and_col = line_lookup.lookup_from_index(body_span.start());

            let (lexical_block, _debug_location) = dbg_builder
                .as_ref()
                .map(|dbg_builder| {
                    let lexical_block = dbg_builder.create_lexical_block(
                        fn_subprogram.expect("We have DI").as_debug_info_scope(),
                        compilation_unit
                            .expect("We have a builder so we must have a unit")
                            .get_file(),
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
                    (lexical_block, debug_location)
                })
                .unzip();

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

                let (ty, _dbg_ty) = llvm_basic_type(&unit, ty.value());

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

                // note: The commented out debug code from original file is omitted for cleanliness,
                // but the logic here handles the argument storage

                fn_scope.insert(name.value(), alloc);
            }

            cg_block(
                FunctionCtx::from_unit_and_fn(unit, fn_value),
                entry,
                &fn_scope,
                lexical_block,
                body,
                &None,
            );
        }
    }

    if let Some(dbg_builder) = dbg_builder {
        dbg_builder.finalize();
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

    module
}

/// Code generate and verify a program given a [`Context`] and return the final
/// LLVM [`Module`] as a result, fully optimized and ready for printing as an IR
/// or compiling to assembly/native code.
///
/// # Panics
/// Panics if code generation fails. This can be caused by an invalid TAST being
/// passed, so make sure to type check it so invariants are upheld.
#[must_use]
#[expect(clippy::too_many_arguments)]
fn cg_program<'ctx>(
    frontend_version_string: &str,
    cli_args: &str,
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    optimization_level: OptimizationLevel,
    debug_level: DWARFEmissionKind,
    parent_directory: &str,
    file_name: &str,
    line_lookup: &LineLookup,
    program: Vec<Spanned<TypedDeclaration<'_, '_>>>,
) -> Module<'ctx> {
    let module = cg_program_without_optimization(
        frontend_version_string,
        cli_args,
        ctx,
        target_machine,
        debug_level,
        parent_directory,
        file_name,
        line_lookup,
        program,
    );

    optimize_module(&module, target_machine, optimization_level);

    module
}

/// Code generate a LLVM program to a string.
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
#[expect(clippy::too_many_arguments)]
pub fn cg_program_to_string(
    frontend_version_string: &str,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    source: &str,
    program: Vec<Spanned<TypedDeclaration<'_, '_>>>,
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
        &LineLookup::new(source),
        program,
    );

    module.print_to_string().to_string()
}

/// Code generate a LLVM program to a string, sans any optimization passes.
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
#[expect(clippy::too_many_arguments)]
// this function is currently only used in tests because the LLVM optimizer
// doesn't handle well when multithreaded.
#[cfg(test)]
pub fn cg_program_to_string_without_optimization(
    frontend_version_string: &str,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    source: &str,
    program: Vec<Spanned<TypedDeclaration<'_, '_>>>,
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
            // FIXME: Technically this function doesn't run optimizations, but we
            // do run them later in the cg_program function. Does this mean we're making
            // an invalid target?
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("target machine should be created successfully");

    let module = cg_program_without_optimization(
        frontend_version_string,
        cli_args,
        &ctx,
        &target_machine,
        debug_level,
        parent_directory,
        file_name,
        &LineLookup::new(source),
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
#[expect(clippy::too_many_arguments)]
pub fn cg_program_to_buffer(
    frontend_version_string: &str,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    source: &str,
    program: Vec<Spanned<TypedDeclaration<'_, '_>>>,
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
        &LineLookup::new(source),
        program,
    );

    target_machine
        .write_to_memory_buffer(&module, file_type)
        .expect("writing to memory buffer should succeed")
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn function_parameters_are_properly_generated() {
        cg_snapshot_test!(indoc! {"
                fn id(x: i32) -> i32 {
                    return x;
                }
            "});
    }

    /// Regression test for <https://github.com/zirco-lang/zrc/issues/441>
    /// Global string variables should compile without ICE
    #[test]
    fn global_string_variable_compiles() {
        cg_snapshot_test!(indoc! {r#"
            let x = "hello";
            let y = "world";

            fn main() -> i32 {
                return 0;
            }
        "#});
    }
}
