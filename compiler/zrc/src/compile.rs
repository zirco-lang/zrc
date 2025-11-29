//! Zirco compiler driver
//!
//! This module contains the main driver function for the Zirco compiler,
//! which orchestrates the parsing, type checking, and code generation phases.

use std::path::Path;

use zrc_codegen::{DebugLevel, OptimizationLevel};

use crate::cli::OutputFormat;

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
#[expect(
    clippy::too_many_arguments,
    clippy::wildcard_enum_match_arm,
    clippy::result_large_err,
    clippy::missing_errors_doc
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
) -> Result<Box<[u8]>, zrc_diagnostics::Diagnostic> {
    // === PREPROCESSOR ===
    let chunks = zrc_preprocessor::preprocess(
        Path::new(parent_directory),
        include_paths,
        file_name,
        content,
    )?;

    // === PARSER ===
    let mut ast = Vec::new();
    for chunk in &chunks {
        let chunk_decls = zrc_parser::parser::parse_source_chunk(chunk)?;
        ast.extend(chunk_decls);
    }

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
    let mut global_scope = zrc_typeck::typeck::GlobalScope::new();
    let typed_ast = zrc_typeck::typeck::type_program(&mut global_scope, ast)?;

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
