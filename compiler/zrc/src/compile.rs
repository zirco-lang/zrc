//! Zirco compiler driver
//!
//! This module contains the main driver function for the Zirco compiler,
//! which orchestrates the parsing, type checking, and code generation phases.

use std::{path::Path, time::Instant};

use zrc_codegen::{DebugLevel, OptimizationLevel};

use crate::OutputFormat;

/// Drive the compilation process.
///
/// This function takes the source code as input and processes it through
/// the various stages of compilation: parsing, type checking, and code
/// generation. Depending on the specified output format, it can return the AST,
/// TAST, LLVM IR, assembly, or object code.
///
/// # Arguments
///
/// * `frontend_version_string` - A string representing the version of the
///   frontend, obtained from [`crate::build_info::version()`].
/// * `emit` - The desired output format.
/// * `parent_directory` - The parent directory of the source file.
/// * `file_name` - The name of the source file.
/// * `cli_args` - The command line arguments passed to the compiler.
/// * `content` - The source code content to be compiled.
/// * `optimization_level` - The optimization level for code generation.
/// * `debug_mode` - The debug level for code generation.
/// * `triple` - The target triple for code generation.
/// * `cpu` - The target CPU for code generation.
/// * `show_timings` - Whether to display timing information.
#[expect(
    clippy::too_many_arguments,
    clippy::wildcard_enum_match_arm,
    clippy::result_large_err,
    clippy::too_many_lines
)]
pub fn compile(
    frontend_version_string: &str,
    include_paths: Vec<&'static Path>,
    emit: &OutputFormat,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    content: &str,
    optimization_level: OptimizationLevel,
    debug_mode: DebugLevel,
    triple: &zrc_codegen::TargetTriple,
    cpu: &str,
    show_timings: bool,
) -> Result<Box<[u8]>, zrc_diagnostics::Diagnostic> {
    let compile_start = Instant::now();

    // === PREPROCESSOR ===
    let preprocess_start = Instant::now();
    let chunks = zrc_preprocessor::preprocess(
        Path::new(parent_directory),
        include_paths,
        file_name,
        content,
    )?;
    let preprocess_duration = preprocess_start.elapsed();

    // === PARSER ===
    let parse_start = Instant::now();
    let mut ast = Vec::new();
    for chunk in &chunks {
        let chunk_decls = zrc_parser::parser::parse_source_chunk(chunk)?;
        ast.extend(chunk_decls);
    }
    let parse_duration = parse_start.elapsed();

    // display the AST if the user wants it
    if matches!(
        emit,
        OutputFormat::Ast | OutputFormat::AstDebug | OutputFormat::AstDebugPretty,
    ) {
        return Ok(match *emit {
            OutputFormat::Ast => ast
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            OutputFormat::AstDebug => format!("{ast:?}"),
            OutputFormat::AstDebugPretty => format!("{ast:#?}"),

            // unreachable because we test above
            _ => unreachable!(),
        }
        .as_bytes()
        .into());
    }

    // otherwise, move on:
    // === TYPE CHECKER ===
    let typecheck_start = Instant::now();
    let mut global_scope = zrc_typeck::typeck::GlobalScope::new();
    let typed_ast = zrc_typeck::typeck::type_program(&mut global_scope, ast)?;
    let typecheck_duration = typecheck_start.elapsed();

    // display the TAST if the user wants it
    if matches!(
        emit,
        OutputFormat::TastDebug | OutputFormat::TastDebugPretty | OutputFormat::Tast,
    ) {
        return Ok(match *emit {
            OutputFormat::TastDebug => format!("{typed_ast:?}"),
            OutputFormat::TastDebugPretty => format!("{typed_ast:#?}"),
            OutputFormat::Tast => typed_ast
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),

            // unreachable because we test above
            _ => unreachable!(),
        }
        .as_bytes()
        .into());
    }

    // otherwise, move on:
    // === CODE GENERATOR ===

    if show_timings {
        // Use timing-aware versions of codegen functions
        let (output, codegen_timings) = match *emit {
            OutputFormat::Asm => {
                let (buffer, timings) = zrc_codegen::cg_program_to_buffer_with_timings(
                    frontend_version_string,
                    parent_directory,
                    file_name,
                    cli_args,
                    content,
                    typed_ast,
                    zrc_codegen::FileType::Assembly,
                    optimization_level,
                    debug_mode,
                    triple,
                    cpu,
                );
                (buffer.as_slice().into(), timings)
            }
            OutputFormat::Object => {
                let (buffer, timings) = zrc_codegen::cg_program_to_buffer_with_timings(
                    frontend_version_string,
                    parent_directory,
                    file_name,
                    cli_args,
                    content,
                    typed_ast,
                    zrc_codegen::FileType::Object,
                    optimization_level,
                    debug_mode,
                    triple,
                    cpu,
                );
                (buffer.as_slice().into(), timings)
            }
            OutputFormat::Llvm => {
                let (string, timings) = zrc_codegen::cg_program_to_string_with_timings(
                    frontend_version_string,
                    parent_directory,
                    file_name,
                    cli_args,
                    content,
                    typed_ast,
                    optimization_level,
                    debug_mode,
                    triple,
                    cpu,
                );
                (string.as_bytes().into(), timings)
            }
            // unreachable because we return in the above cases
            _ => unreachable!(),
        };

        let total_duration = compile_start.elapsed();
        eprintln!("Compilation timings:");
        eprintln!(
            "  Preprocessing: {:.3}ms",
            preprocess_duration.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  Parsing:       {:.3}ms",
            parse_duration.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  Type checking: {:.3}ms",
            typecheck_duration.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  IR generation: {:.3}ms",
            codegen_timings.ir_generation.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  Optimization:  {:.3}ms",
            codegen_timings.optimization.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  Total:         {:.3}ms",
            total_duration.as_secs_f64() * 1000.0
        );

        Ok(output)
    } else {
        // Use non-timing versions for better performance
        match *emit {
            OutputFormat::Asm => Ok(zrc_codegen::cg_program_to_buffer(
                frontend_version_string,
                parent_directory,
                file_name,
                cli_args,
                content,
                typed_ast,
                zrc_codegen::FileType::Assembly,
                optimization_level,
                debug_mode,
                triple,
                cpu,
            )
            .as_slice()
            .into()),
            OutputFormat::Object => Ok(zrc_codegen::cg_program_to_buffer(
                frontend_version_string,
                parent_directory,
                file_name,
                cli_args,
                content,
                typed_ast,
                zrc_codegen::FileType::Object,
                optimization_level,
                debug_mode,
                triple,
                cpu,
            )
            .as_slice()
            .into()),

            OutputFormat::Llvm => Ok(zrc_codegen::cg_program_to_string(
                frontend_version_string,
                parent_directory,
                file_name,
                cli_args,
                content,
                typed_ast,
                optimization_level,
                debug_mode,
                triple,
                cpu,
            )
            .as_bytes()
            .into()),

            // unreachable because we return in the above cases
            _ => unreachable!(),
        }
    }
}
