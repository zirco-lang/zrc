//! A dedicated function so non-`zrc` code can trivially create JIT modules.

use tracing::instrument;
use zrc_diagnostics::Diagnostic;
use zrc_jit::module::JitModule;
use zrc_parser::parser;
use zrc_typeck::typeck;

use crate::compile::CompileInputs;

/// JIT compile and link a Zirco program into the given [`JitModule`].
///
/// # Errors
///
/// Errors if a diagnostic is emitted.
#[instrument(skip_all)]
#[expect(clippy::needless_pass_by_value, clippy::result_large_err)]
pub fn jit_compile_and_link(
	module: &JitModule<'_>,
	inputs @ CompileInputs { path, content, .. }: CompileInputs,
) -> Result<(), Diagnostic> {
	let chunks = zrc_preprocessor::preprocess((&inputs).into())?;

	let mut ast = Vec::new();
	for chunk in &chunks {
		let chunk_decls = parser::parse_source_chunk(chunk)?;
		ast.extend(chunk_decls);
	}

	let mut gs = typeck::GlobalScope::new();
	let typed_ast = typeck::type_program(&mut gs, ast)?;

	module.cg_program_and_link(path, content, typed_ast);

	Ok(())
}
