//! A JIT API for zrc.

use std::{
	ffi::{CStr, c_char},
	os::raw::c_void,
	panic::{AssertUnwindSafe, catch_unwind},
	path::PathBuf,
	process, ptr, slice,
};

use zrc::{
	codegen,
	compile::CompileInputs,
	jit::{engine::JitEngine, module::JitModule},
	jit_driver::jit_compile_and_link,
};

use crate::{ZrcCompileInputs, ZrcCompileResult, ZrcDiagnostic};

/// Opaque struct representing the Zirco JIT engine.
#[derive(Debug)]
pub struct ZrcJitEngine;

/// Opaque struct representing a Zirco JIT module.
#[derive(Debug)]
pub struct ZrcJitModule;

/// Initialize the Zirco JIT for the current thread.
///
/// This function must only be called once per thread.
///
/// # Arguments
/// * `frontend_version_string` - The frontend version string.
/// * `cli_args` - The command line arguments passed to the compiler.
/// * `lib_paths` - The paths to search for libraries.
///
/// # Safety
/// The caller must guarantee that all C strings are valid and that the pointers
/// passed to this function are valid for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_init(
	frontend_version_string: *const c_char,
	cli_args: *const c_char,
	lib_paths: *const *const c_char,
	lib_paths_len: usize,
) -> *const ZrcJitEngine {
	// SAFETY: the caller guarantees that all C strings are valid
	let frontend_version_string = Box::leak(Box::new(
		unsafe { CStr::from_ptr(frontend_version_string) }
			.to_string_lossy()
			.into_owned(),
	));

	// SAFETY: the caller guarantees that all C strings are valid
	let cli_args = Box::leak(Box::new(
		unsafe { CStr::from_ptr(cli_args) }
			.to_string_lossy()
			.into_owned(),
	));

	// SAFETY: the caller guarantees that the length is correct
	let lib_paths = unsafe { slice::from_raw_parts(lib_paths, lib_paths_len) }
		.iter()
		.map(|&ptr| {
			Into::<PathBuf>::into(
				// SAFETY: the caller guarantees that all C strings are valid
				unsafe { CStr::from_ptr(ptr) }
					.to_string_lossy()
					.into_owned(),
			)
		})
		.collect::<Vec<PathBuf>>();

	let engine = JitEngine::init(frontend_version_string, cli_args, lib_paths);

	Box::into_raw(Box::new(engine)).cast::<ZrcJitEngine>()
}

/// Load a library by name into the given JIT engine.
///
/// Returns `true` if the library was loaded successfully, or `false` if the
/// load failed.
///
/// # Safety
///
/// The caller must guarantee that the engine pointer is valid, and that the
/// library name is a valid C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_load_library(
	engine: *const ZrcJitEngine,
	lib_name: *const c_char,
) -> bool {
	// SAFETY: the caller guarantees that the engine pointer is valid
	let engine = unsafe { &*engine.cast::<JitEngine>() };

	// SAFETY: the caller guarantees that the library name is a valid C string
	let lib_name = unsafe { CStr::from_ptr(lib_name) }
		.to_string_lossy()
		.into_owned();

	catch_unwind(AssertUnwindSafe(|| engine.load_library(&lib_name).is_ok())).is_ok()
}

/// Load all symbols visible to the current process into the given JIT engine.
///
/// # Safety
///
/// The caller must guarantee that the engine pointer is valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_load_visible_symbols(engine: *const ZrcJitEngine) {
	// SAFETY: the caller guarantees that the engine pointer is valid
	let engine = unsafe { &*engine.cast::<JitEngine>() };
	engine.load_visible_symbols();
}

/// Create a new JIT module within the given engine.
///
/// # Safety
///
/// The caller must guarantee that the engine pointer is valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_create_module(engine: *const ZrcJitEngine) -> *const ZrcJitModule {
	// SAFETY: the caller guarantees that the engine pointer is valid
	let engine = unsafe { &*engine.cast::<JitEngine>() };
	let module = engine.create_module();
	Box::into_raw(Box::new(module)).cast::<ZrcJitModule>()
}

/// Set a global symbol in the given JIT module to a given pointer.
///
/// Returns `true` if the symbol was set successfully, or `false` if the set
/// failed.
///
/// # Safety
///
/// The caller must guarantee that the module pointer is valid, and that the
/// pointer is valid, lives for the lifetime of the JIT module, and is of the
/// correct type for the symbol.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_set_global_symbol(
	module: *const ZrcJitModule,
	symbol_name: *const c_char,
	ptr: *const c_void,
) -> bool {
	// SAFETY: the caller guarantees that the module pointer is valid
	let module = unsafe { &*module.cast::<JitModule>() };

	// SAFETY: the caller guarantees that the symbol name is a valid C string
	let symbol_name = unsafe { CStr::from_ptr(symbol_name) }
		.to_string_lossy()
		.into_owned();

	catch_unwind(AssertUnwindSafe(|| {
		module.set_global_symbol(&symbol_name, ptr.cast::<()>());
	}))
	.is_ok()
}

/// Try to load a function from the given JIT module.
///
/// Returns NULL if the function is not found, or a pointer to the function if
/// it is found.
///
/// # Safety
///
/// The caller must guarantee that the module pointer is valid, and that the
/// function signature matches the expected signature. The caller must also
/// ensure that the module, and hence the JIT engine, is kept alive for the
/// lifetime of the function pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_get_function(
	module: *const ZrcJitModule,
	name: *const c_char,
) -> *const c_void {
	// SAFETY: the caller guarantees that the module pointer is valid
	let module = unsafe { &*module.cast::<JitModule>() };

	// SAFETY: the caller guarantees that the function name is a valid C string
	let name = unsafe { CStr::from_ptr(name) }
		.to_string_lossy()
		.into_owned();

	// SAFETY: the caller guarantees that the function signature matches the
	// expected signature
	#[expect(clippy::as_conversions, clippy::min_ident_chars)]
	unsafe { module.get_function::<unsafe extern "C" fn() -> ()>(&name) }
		// SAFETY: the caller is responsible for keeping this module, and hence self.ee, alive
		.map_or(ptr::null(), |f| unsafe { f.as_raw() } as *const c_void)
}

/// Destroy the given JIT module.
///
/// # Safety
///
/// The caller must guarantee that the module pointer is valid and that the
/// module is no longer in use.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_jit_destroy_module(module: *const ZrcJitModule) {
	// SAFETY: the caller guarantees that the module pointer is valid
	unsafe {
		drop(Box::from_raw(module.cast_mut().cast::<JitModule>()));
	}
}

/// Compile and link a Zirco program into the given JIT module.
///
/// # Safety
///
/// The caller must guarantee that the module pointer is valid and that the
/// values within `inputs` are valid for the duration of the call.
#[unsafe(no_mangle)]
#[expect(clippy::undocumented_unsafe_blocks, clippy::result_large_err)]
pub unsafe extern "C" fn zrc_jit_compile_and_link(
	module: *const ZrcJitModule,
	inputs: ZrcCompileInputs,
) -> ZrcCompileResult {
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

	// SAFETY: the caller guarantees that the module pointer is valid
	let module = unsafe { &*module.cast::<JitModule>() };

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

	let result = catch_unwind(AssertUnwindSafe(|| {
		jit_compile_and_link(module, compile_inputs)
	}));

	match result {
		Ok(Ok(())) => ZrcCompileResult {
			success: true,
			data: ptr::null_mut(),
			size: 0,
			diagnostic: ptr::null_mut(),
		},
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
