//! An individual compilation unit within the JIT.

use std::{ffi::CString, os::raw::c_char, path::Path};

use inkwell::{
	OptimizationLevel,
	execution_engine::{ExecutionEngine, FunctionLookupError, JitFunction, UnsafeFunctionPointer},
	module::Module,
};
use tracing::debug;
use zrc_codegen::cg_program;
use zrc_typeck::typeck::TastRoot;
use zrc_utils::line_finder::LineLookup;

use crate::engine::JitEngine;

/// A [`JitModule`] represents a single compilation unit within the JIT. It is
/// responsible for managing the LLVM [`Module`] and state for JIT execution.
///
/// Unlike `zrc_codegen`, the Zirco JIT supports multiple files in one JIT
/// module, so no linking is required.
#[derive(Debug)]
pub struct JitModule<'ctx> {
	/// The overarching [`JitEngine`] for this module.
	engine: &'ctx JitEngine,
	/// This module's LLVM [`Module`].
	module: Module<'ctx>,
	/// This module's LLVM [`ExecutionEngine`].
	ee: ExecutionEngine<'ctx>,
}

impl<'ctx> JitModule<'ctx> {
	/// Create a new [`JitModule`] with the given name and context.
	///
	/// # Arguments
	///
	/// * `engine` - The [`JitEngine`] to use for this module.
	/// * `name` - The name of the module.
	pub(crate) fn new(engine: &'ctx JitEngine, name: &str) -> Self {
		let module = engine.ctx.create_module(name);
		let ee = module
			.create_jit_execution_engine(OptimizationLevel::Default)
			.expect("creating execution engine should succeed");

		Self { engine, module, ee }
	}

	/// Code generate and link a Zirco TAST into this JIT module.
	///
	/// # Panics
	///
	/// Panics during internal LLVM failures.
	pub fn cg_program_and_link(&self, path: &Path, source_content: &str, typed_ast: TastRoot<'_>) {
		let file_module = cg_program(
			self.engine.frontend_version_string,
			self.engine.cli_args,
			&self.engine.ctx,
			&self.engine.target_machine,
			OptimizationLevel::Default,
			zrc_codegen::DebugLevel::None,
			path,
			&LineLookup::new(source_content),
			typed_ast,
		);
		debug!("linking module into JIT");
		self.module
			.link_in_module(file_module)
			.expect("linking module into JIT should succeed");
	}

	/// Set a global symbol in this JIT module to a given pointer.
	///
	/// # Safety
	///
	/// The caller must ensure that the pointer is valid, lives for the lifetime
	/// of the JIT module, and is of the correct type for the symbol.
	///
	/// # Panics
	///
	/// Can panic if the symbol does not exist in the module.
	pub fn set_global_symbol(&self, symbol_name: &str, ptr: *const ()) {
		debug!(symbol = symbol_name, "setting global symbol");
		self.ee.add_global_mapping(
			&self
				.module
				.get_global(symbol_name)
				.expect("global should exist"),
			ptr.addr(),
		);
	}

	/// Try to load a function from the JIT.
	///
	/// # Errors
	///
	/// Can fail if the function is not found.
	///
	/// # Safety
	///
	/// It is the caller's responsibility to ensure that the function signature
	/// matches the expected signature.
	pub unsafe fn get_function<F>(
		&self,
		name: &str,
	) -> Result<JitFunction<'ctx, F>, FunctionLookupError>
	where
		F: UnsafeFunctionPointer,
	{
		// SAFETY: All invariants upheld by the caller
		unsafe { self.ee.get_function(name) }
	}

	/// Try to load and execute a function `main` from the JIT.
	///
	/// # Errors
	///
	/// Can fail if the function is not found.
	///
	/// # Panics
	///
	/// Can panic in an internal LLVM error.
	pub fn run_main(&self, args: Vec<String>) -> Result<i32, FunctionLookupError> {
		let c_strings: Vec<CString> = args
			.into_iter()
			.map(|arg| CString::new(arg).expect("program argument contained null byte"))
			.collect();

		let c_ptrs: Vec<*const c_char> = c_strings.iter().map(|cstr| cstr.as_ptr()).collect();

		// SAFETY: The Zirco type checker ensures any function named "main" has the
		// correct signature
		let main = unsafe {
			self.get_function::<unsafe extern "C" fn(usize, *const *const c_char) -> i32>("main")?
		};

		debug!("executing main function");
		// SAFETY: The Zirco type checker ensures any function named "main" has
		// the correct signature
		let result = unsafe { main.call(c_ptrs.len(), c_ptrs.as_ptr()) };
		debug!("main function returned {}", result);
		Ok(result)
	}
}
