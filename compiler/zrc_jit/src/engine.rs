//! [`JitEngine`] go vroom

use std::path::PathBuf;

use inkwell::{
	OptimizationLevel,
	context::Context,
	targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
};
use tracing::debug;
use zrc_codegen::get_native_triple;

use crate::{module::JitModule, utils::split_paths};

/// The Zirco JIT engine. There must only be one per thread.
#[derive(Debug)]
pub struct JitEngine {
	/// The LLVM [`Context`] for this JIT engine.
	pub(crate) ctx: Context,
	/// The target machine for this JIT engine.
	pub(crate) target_machine: TargetMachine,
	/// The frontend version string.
	pub(crate) frontend_version_string: &'static str,
	/// The CLI arguments.
	pub(crate) cli_args: &'static str,
	/// The paths libraries can be found in.
	pub(crate) lib_paths: Vec<PathBuf>,
}

impl JitEngine {
	/// Initialize the Zirco JIT for a thread, returning a [`JitEngine`].
	///
	/// # Panics
	///
	/// Can panic if LLVM fails to initialize, or if the native target machine
	/// cannot be created.
	#[must_use]
	pub fn init(
		frontend_version_string: &'static str,
		cli_args: &'static str,
		lib_paths: Vec<PathBuf>,
	) -> Self {
		let ctx = Context::create();
		Target::initialize_native(&InitializationConfig::default())
			.expect("LLVM should be initialized successfully");
		let triple = get_native_triple();
		let target =
			Target::from_triple(&triple).expect("native target should be created successfully");

		let target_machine = target
			.create_target_machine(
				&triple,
				"",
				"",
				OptimizationLevel::Default,
				RelocMode::Default,
				CodeModel::JITDefault,
			)
			.expect("native target machine should be created successfully");

		let mut library_paths = split_paths("LD_LIBRARY_PATH");
		library_paths.extend(split_paths("DYLD_LIBRARY_PATH"));
		library_paths.extend(lib_paths);

		debug!("JIT engine initialized for native target {}", triple);

		Self {
			ctx,
			target_machine,
			frontend_version_string,
			cli_args,
			lib_paths: library_paths,
		}
	}

	/// Create a new [`JitModule`].
	#[must_use]
	pub fn create_module(&self) -> JitModule<'_> {
		JitModule::new(self, "zrc_jit_module")
	}
}
