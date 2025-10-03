//! Zirco compiler driver

use zrc_codegen::{DebugLevel, OptimizationLevel};

use crate::OutputFormat;

/// Drive the compilation process.
#[allow(clippy::too_many_arguments, clippy::wildcard_enum_match_arm)]
pub fn compile(
    frontend_version_string: &str,
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
    // === PARSER ===
    // Leak the file_name to get a &'static str for Spanned
    let static_file_name: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    let ast = zrc_parser::parser::parse_program(content, static_file_name)?;

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
    let typed_ast = zrc_typeck::typeck::type_program(ast)?;

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
