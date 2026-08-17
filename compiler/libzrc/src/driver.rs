//! C API for the `zrc` compilation driver.

use std::{
	ffi::{CStr, c_char, c_void},
	mem::forget,
	panic::catch_unwind,
	path::PathBuf,
	process, ptr, slice,
};

use zrc::{OutputFormat, codegen, compile::CompileInputs};

use crate::diagnostics::ZrcDiagnostic;

/// The level of optimization to apply during code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ZrcOptimizationLevel {
	/// -O0
	ZRC_OPTIMIZE_NONE = 0,
	/// -O1
	ZRC_OPTIMIZE_LESS = 1,
	/// -O2
	ZRC_OPTIMIZE_DEFAULT = 2,
	/// -O3
	ZRC_OPTIMIZE_AGGRESSIVE = 3,
}
impl From<ZrcOptimizationLevel> for codegen::OptimizationLevel {
	fn from(level: ZrcOptimizationLevel) -> Self {
		match level {
			ZrcOptimizationLevel::ZRC_OPTIMIZE_NONE => Self::None,
			ZrcOptimizationLevel::ZRC_OPTIMIZE_LESS => Self::Less,
			ZrcOptimizationLevel::ZRC_OPTIMIZE_DEFAULT => Self::Default,
			ZrcOptimizationLevel::ZRC_OPTIMIZE_AGGRESSIVE => Self::Aggressive,
		}
	}
}

/// The level of debug information to include during code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ZrcDebugLevel {
	/// No debug information.
	ZRC_DI_NONE = 0,
	/// Line tables only.
	ZRC_DI_BASIC = 1,
	/// Full debug information.
	ZRC_DI_FULL = 2,
}
impl From<ZrcDebugLevel> for codegen::DebugLevel {
	fn from(level: ZrcDebugLevel) -> Self {
		match level {
			ZrcDebugLevel::ZRC_DI_NONE => Self::None,
			ZrcDebugLevel::ZRC_DI_BASIC => Self::LineTablesOnly,
			ZrcDebugLevel::ZRC_DI_FULL => Self::Full,
		}
	}
}

/// The list of possible output formats `zrc` can produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ZrcOutputFormat {
	/// LLVM IR
	ZRC_OUTPUT_LLVM = 0,
	/// Assembly
	ZRC_OUTPUT_ASM = 1,
	/// Object code
	ZRC_OUTPUT_OBJ = 2,
}
impl From<ZrcOutputFormat> for OutputFormat {
	fn from(format: ZrcOutputFormat) -> Self {
		match format {
			ZrcOutputFormat::ZRC_OUTPUT_LLVM => Self::Llvm,
			ZrcOutputFormat::ZRC_OUTPUT_ASM => Self::Asm,
			ZrcOutputFormat::ZRC_OUTPUT_OBJ => Self::Object,
		}
	}
}

/// The results of a compilation attempt, including either the output data or
/// the diagnostic.
#[repr(C)]
#[derive(Debug)]
pub struct ZrcCompileResult {
	/// Whether compilation succeeded.
	pub success: bool,
	/// If `success` is true, the compiled output data as raw bytes. The caller
	/// is responsible for freeing this with `zrc_free_buffer` when no longer
	/// needed.
	pub data: *mut c_void,
	/// If `success` is true, the size of the compiled output data in bytes.
	pub size: usize,
	/// If `success` is false, a pointer to a `ZrcDiagnostic` describing the
	/// error.
	pub diagnostic: *mut ZrcDiagnostic,
}

/// Inputs for the compilation driver.
#[repr(C)]
#[derive(Debug)]
pub struct ZrcCompileInputs {
	/// The version string of the frontend.
	pub frontend_version_string: *const c_char,
	/// The list of include paths.
	pub include_paths: *const *const c_char,
	/// The number of include paths.
	pub include_paths_len: usize,
	/// The desired output format.
	pub emit: ZrcOutputFormat,
	/// The path of the source file.
	pub path: *const c_char,
	/// The command line arguments passed to the compiler.
	pub cli_args: *const c_char,
	/// The source code content to be compiled.
	pub content: *const c_char,
	/// The optimization level for code generation.
	pub optimization_level: ZrcOptimizationLevel,
	/// The debug level for code generation.
	pub debug_mode: ZrcDebugLevel,
	/// The target triple for code generation.
	pub triple: *const c_char,
	/// The target CPU for code generation.
	pub cpu: *const c_char,
	/// Whether to restrict includes to search paths only.
	pub forbid_unlisted_includes: bool,
}

/// Drive the compilation process.
///
/// # Errors
///
/// If compilation fails, a `ZrcDiagnostic` will be returned describing the
/// error.
///
/// # Safety
///
/// The caller must guarantee that all C strings are valid and that the pointers
/// passed to this function are valid for the duration of the call.
#[unsafe(no_mangle)]
#[expect(clippy::undocumented_unsafe_blocks, clippy::result_large_err)]
pub unsafe extern "C" fn zrc_compile(inputs: ZrcCompileInputs) -> ZrcCompileResult {
	let ZrcCompileInputs {
		frontend_version_string,
		include_paths,
		include_paths_len,
		emit,
		path,
		cli_args,
		content,
		optimization_level,
		debug_mode,
		triple,
		cpu,
		forbid_unlisted_includes,
	} = inputs;

	// SAFETY: the caller guarantees that all C strings are valid
	let frontend_version_string = unsafe { CStr::from_ptr(frontend_version_string) }
		.to_string_lossy()
		.into_owned();
	let include_paths = unsafe { slice::from_raw_parts(include_paths, include_paths_len) }
		.iter()
		.map(|&ptr| {
			Into::<PathBuf>::into(
				unsafe { CStr::from_ptr(ptr) }
					.to_string_lossy()
					.into_owned(),
			)
		})
		.collect::<Vec<PathBuf>>();
	let path = unsafe { PathBuf::from(CStr::from_ptr(path).to_string_lossy().into_owned()) };
	let cli_args = unsafe { CStr::from_ptr(cli_args) }
		.to_string_lossy()
		.into_owned();
	let content = unsafe { CStr::from_ptr(content) }
		.to_string_lossy()
		.into_owned();
	let triple = unsafe { CStr::from_ptr(triple) }
		.to_string_lossy()
		.into_owned();
	let cpu = unsafe { CStr::from_ptr(cpu) }
		.to_string_lossy()
		.into_owned();

	let compile_inputs = CompileInputs {
		frontend_version_string: &frontend_version_string,
		cli_args: &cli_args,
		include_paths: &include_paths,
		emit: emit.into(),
		path: &path,
		optimization_level: optimization_level.into(),
		debug_mode: debug_mode.into(),
		triple: &codegen::TargetTriple::create(&triple),
		cpu: &cpu,
		forbid_unlisted_includes,
		content: &content,
	};

	let result = catch_unwind(|| zrc::compile(compile_inputs));

	match result {
		Ok(Ok(output)) => {
			let mut output = output.into_vec();
			let size = output.len();
			let data = output.as_mut_ptr().cast::<c_void>();
			forget(output);
			ZrcCompileResult {
				success: true,
				data,
				size,
				diagnostic: ptr::null_mut(),
			}
		}
		Ok(Err(diag)) => {
			let diag = Box::new(diag);
			ZrcCompileResult {
				success: false,
				data: ptr::null_mut(),
				size: 0,
				diagnostic: Box::into_raw(diag).cast::<ZrcDiagnostic>(),
			}
		}
		Err(_) => {
			eprintln!("internal compiler error: compilation panicked");
			process::abort();
		}
	}
}
