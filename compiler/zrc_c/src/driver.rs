//! C API for the `zrc` compilation driver.

use std::{
    ffi::{CStr, c_char},
    panic::{AssertUnwindSafe, catch_unwind},
    path::PathBuf,
    process, ptr, slice,
};

use zrc::{
    codegen,
    compile::{OutputFormat, compile},
};

use crate::diagnostics::ZrcDiagnostic;

/// The level of optimization to apply during code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OptimizationLevel {
    /// No optimizations. This is the default.
    ZRC_OPTIMIZE_NONE = 0,
    /// Some optimizations that do not significantly increase compile time or
    /// code size.
    ZRC_OPTIMIZE_LESS = 1,
    /// The default level of optimizations.
    ZRC_OPTIMIZE_DEFAULT = 2,
    /// Aggressive optimizations that may significantly increase compile time or
    /// code size.
    ZRC_OPTIMIZE_AGGRESSIVE = 3,
}
impl From<OptimizationLevel> for codegen::OptimizationLevel {
    fn from(val: OptimizationLevel) -> Self {
        match val {
            OptimizationLevel::ZRC_OPTIMIZE_NONE => Self::None,
            OptimizationLevel::ZRC_OPTIMIZE_LESS => Self::Less,
            OptimizationLevel::ZRC_OPTIMIZE_DEFAULT => Self::Default,
            OptimizationLevel::ZRC_OPTIMIZE_AGGRESSIVE => Self::Aggressive,
        }
    }
}

/// The level of debug information to include during code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DebugLevel {
    /// No debug information. This is the default.
    ZRC_DI_NONE = 0,
    /// Include basic debug information.
    ZRC_DI_BASIC = 1,
    /// Include full debug information.
    ZRC_DI_FULL = 2,
}
impl From<DebugLevel> for codegen::DebugLevel {
    fn from(val: DebugLevel) -> Self {
        match val {
            DebugLevel::ZRC_DI_NONE => Self::None,
            DebugLevel::ZRC_DI_BASIC => Self::LineTablesOnly,
            DebugLevel::ZRC_DI_FULL => Self::Full,
        }
    }
}

/// The list of possible outputs `zrc` can emit in
///
/// Usually you will want to use `llvm`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum ZrcOutputFormat {
    /// LLVM IR
    ZRC_OUTPUT_LLVM = 0,
    /// Assembly
    ZRC_OUTPUT_ASM = 1,
    /// Object file
    ZRC_OUTPUT_OBJ = 2,
}
impl From<ZrcOutputFormat> for OutputFormat {
    fn from(val: ZrcOutputFormat) -> Self {
        match val {
            ZrcOutputFormat::ZRC_OUTPUT_LLVM => Self::Llvm,
            ZrcOutputFormat::ZRC_OUTPUT_ASM => Self::Asm,
            ZrcOutputFormat::ZRC_OUTPUT_OBJ => Self::Object,
        }
    }
}

/// The results of a compilation attempt, including either the output data or
/// the diagnostics.
#[repr(C)]
#[derive(Debug)]
pub struct ZrcCompileResult {
    /// Whether compilation succeeded or failed.
    pub success: bool,
    /// The output data, if compilation succeeded.
    pub data: *mut u8,
    /// The size of the output data, if compilation succeeded.
    pub size: usize,
    /// The diagnostic, if compilation failed.
    pub diagnostics: *mut ZrcDiagnostic,
}

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
///   frontend.
/// * `include_paths` - The list of directories to search for includes.
/// * `emit` - The desired output format.
/// * `parent_directory` - The parent directory of the source file.
/// * `file_name` - The name of the source file.
/// * `cli_args` - The command line arguments passed to the compiler.
/// * `content` - The source code content to be compiled.
/// * `optimization_level` - The optimization level for code generation.
/// * `debug_mode` - The debug level for code generation.
/// * `triple` - The target triple for code generation.
/// * `cpu` - The target CPU for code generation.
/// * `forbid_unlisted_includes` - Whether to restrict includes to search paths
///   only.
///
/// # Errors
///
/// If compilation fails, the provided `ZrcDiagnostic**` will
/// be set to point to a newly allocated `ZrcDiagnostic` containing the error
/// information. The caller is responsible for freeing this diagnostic when it
/// is no longer needed. If compilation succeeds, the provided `uint8_t**` and
/// `size_t*` will be set to point to a newly allocated buffer containing the
/// output data and its size, respectively. The caller is responsible for
/// freeing this buffer when it is no longer needed.
///
/// # Safety
///
/// The caller must guarantee that all C strings passed to this function are
/// valid null-terminated strings, and that the pointers provided for the output
/// data and diagnostics are valid for writing. The caller is also responsible
/// for freeing any allocated memory returned by this function when it is no
/// longer needed.
#[unsafe(no_mangle)]
#[expect(clippy::undocumented_unsafe_blocks, clippy::result_large_err)]
pub unsafe extern "C" fn zrc_compile(
    frontend_version_string: *const c_char,
    include_paths: *const *const c_char,
    include_paths_len: usize,
    emit: ZrcOutputFormat,
    parent_directory: *const c_char,
    file_name: *const c_char,
    cli_args: *const c_char,
    content: *const c_char,
    optimization_level: OptimizationLevel,
    debug_mode: DebugLevel,
    triple: *const c_char,
    cpu: *const c_char,
    forbid_unlisted_includes: bool,
) -> ZrcCompileResult {
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
    let parent_directory = unsafe { CStr::from_ptr(parent_directory) }
        .to_string_lossy()
        .into_owned();
    let file_name = unsafe { CStr::from_ptr(file_name) }
        .to_string_lossy()
        .into_owned();
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

    match catch_unwind(AssertUnwindSafe(|| {
        compile(
            &frontend_version_string,
            &Box::leak(Box::new(include_paths))
                .iter()
                .map(PathBuf::as_path)
                .collect::<Vec<_>>(),
            &emit.into(),
            &parent_directory,
            &file_name,
            &cli_args,
            &content,
            optimization_level.into(),
            debug_mode.into(),
            &codegen::TargetTriple::create(&triple),
            &cpu,
            forbid_unlisted_includes,
        )
    })) {
        Ok(Ok(output)) => {
            let size = output.len();
            let data = Box::into_raw(output).cast::<u8>();
            ZrcCompileResult {
                success: true,
                data,
                size,
                diagnostics: ptr::null_mut(),
            }
        }
        Ok(Err(diag)) => {
            let diag = Box::new(diag);
            ZrcCompileResult {
                success: false,
                data: ptr::null_mut(),
                size: 0,
                diagnostics: Box::into_raw(diag).cast::<ZrcDiagnostic>(),
            }
        }
        Err(_) => {
            eprintln!("internal compiler error: compilation panicked");
            process::abort();
        }
    }
}
