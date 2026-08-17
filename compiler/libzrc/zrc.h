#ifndef ZRC_H
#define ZRC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Levels of diagnostic severity in the C API.
 */
enum ZrcDiagnosticSeverity
#if __STDC_VERSION__ >= 202311L
  : uint8_t
#endif // __STDC_VERSION__ >= 202311L
 {
  /**
   * An error.
   */
  ZRC_DIAG_ERROR = 0,
  /**
   * A warning.
   */
  ZRC_DIAG_WARNING = 1,
};
#if __STDC_VERSION__ >= 202311L
typedef enum ZrcDiagnosticSeverity ZrcDiagnosticSeverity;
#else
typedef uint8_t ZrcDiagnosticSeverity;
#endif // __STDC_VERSION__ >= 202311L

/**
 * The list of possible output formats `zrc` can produce.
 */
enum ZrcOutputFormat
#if __STDC_VERSION__ >= 202311L
  : uint8_t
#endif // __STDC_VERSION__ >= 202311L
 {
  /**
   * LLVM IR
   */
  ZRC_OUTPUT_LLVM = 0,
  /**
   * Assembly
   */
  ZRC_OUTPUT_ASM = 1,
  /**
   * Object code
   */
  ZRC_OUTPUT_OBJ = 2,
};
#if __STDC_VERSION__ >= 202311L
typedef enum ZrcOutputFormat ZrcOutputFormat;
#else
typedef uint8_t ZrcOutputFormat;
#endif // __STDC_VERSION__ >= 202311L

/**
 * The level of optimization to apply during code generation.
 */
enum ZrcOptimizationLevel
#if __STDC_VERSION__ >= 202311L
  : uint8_t
#endif // __STDC_VERSION__ >= 202311L
 {
  /**
   * -O0
   */
  ZRC_OPTIMIZE_NONE = 0,
  /**
   * -O1
   */
  ZRC_OPTIMIZE_LESS = 1,
  /**
   * -O2
   */
  ZRC_OPTIMIZE_DEFAULT = 2,
  /**
   * -O3
   */
  ZRC_OPTIMIZE_AGGRESSIVE = 3,
};
#if __STDC_VERSION__ >= 202311L
typedef enum ZrcOptimizationLevel ZrcOptimizationLevel;
#else
typedef uint8_t ZrcOptimizationLevel;
#endif // __STDC_VERSION__ >= 202311L

/**
 * The level of debug information to include during code generation.
 */
enum ZrcDebugLevel
#if __STDC_VERSION__ >= 202311L
  : uint8_t
#endif // __STDC_VERSION__ >= 202311L
 {
  /**
   * No debug information.
   */
  ZRC_DI_NONE = 0,
  /**
   * Line tables only.
   */
  ZRC_DI_BASIC = 1,
  /**
   * Full debug information.
   */
  ZRC_DI_FULL = 2,
};
#if __STDC_VERSION__ >= 202311L
typedef enum ZrcDebugLevel ZrcDebugLevel;
#else
typedef uint8_t ZrcDebugLevel;
#endif // __STDC_VERSION__ >= 202311L

/**
 * Opaque struct representing a diagnostic in the C API. It is only a handle to
 * pass diagnostics between Rust and C.
 */
typedef struct ZrcDiagnostic ZrcDiagnostic;

/**
 * Opaque struct representing the Zirco JIT engine.
 */
typedef struct ZrcJitEngine ZrcJitEngine;

/**
 * Opaque struct representing a Zirco JIT module.
 */
typedef struct ZrcJitModule ZrcJitModule;

/**
 * The results of a compilation attempt, including either the output data or
 * the diagnostic.
 */
typedef struct ZrcCompileResult {
  /**
   * Whether compilation succeeded.
   */
  bool success;
  /**
   * If `success` is true, the compiled output data as raw bytes. The caller
   * is responsible for freeing this with `zrc_free_buffer` when no longer
   * needed.
   */
  void *data;
  /**
   * If `success` is true, the size of the compiled output data in bytes.
   */
  size_t size;
  /**
   * If `success` is false, a pointer to a `ZrcDiagnostic` describing the
   * error.
   */
  struct ZrcDiagnostic *diagnostic;
} ZrcCompileResult;

/**
 * Inputs for the compilation driver.
 */
typedef struct ZrcCompileInputs {
  /**
   * The version string of the frontend.
   */
  const char *frontend_version_string;
  /**
   * The list of include paths.
   */
  const char *const *include_paths;
  /**
   * The number of include paths.
   */
  size_t include_paths_len;
  /**
   * The desired output format.
   */
  ZrcOutputFormat emit;
  /**
   * The path of the source file.
   */
  const char *path;
  /**
   * The command line arguments passed to the compiler.
   */
  const char *cli_args;
  /**
   * The source code content to be compiled.
   */
  const char *content;
  /**
   * The optimization level for code generation.
   */
  ZrcOptimizationLevel optimization_level;
  /**
   * The debug level for code generation.
   */
  ZrcDebugLevel debug_mode;
  /**
   * The target triple for code generation.
   */
  const char *triple;
  /**
   * The target CPU for code generation.
   */
  const char *cpu;
  /**
   * Whether to restrict includes to search paths only.
   */
  bool forbid_unlisted_includes;
} ZrcCompileInputs;

/**
 * Free a string returned by the zrc C API.
 *
 * # Safety
 * The caller must guarantee that `s` is a valid pointer to a null-terminated C
 * string that was returned by a function in the zrc C API, and that it has not
 * already been freed.
 */
void zrc_free_string(char *str);

/**
 * Free a raw byte buffer returned by the zrc C API.
 *
 * # Safety
 * The caller must guarantee that `data` is a valid pointer to a buffer that
 * was returned by a function in the zrc C API, that `size` matches the size
 * reported for that buffer, and that the buffer has not already been freed.
 */
void zrc_free_buffer(void *data, size_t size);

/**
 * Get the severity of a [`ZrcDiagnostic`]
 *
 * # Safety
 * The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`.
 */
ZrcDiagnosticSeverity zrc_diag_severity(const struct ZrcDiagnostic *diag);

/**
 * Obtain the "primary line" of a [`ZrcDiagnostic`] as a C string. This is the
 * main message of the diagnostic: `error[E1234]: message`
 *
 * # Safety
 * The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
 * and that the returned string is freed with `zrc_free_string` when no longer
 * needed.
 */
char *zrc_diag_fmt_primary_line(const struct ZrcDiagnostic *diag);

/**
 * Obtain the string representation of a full [`ZrcDiagnostic`], including all
 * related context, as a C string.
 *
 * # Safety
 * The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
 * and that `source` is either null or a valid pointer to a null-terminated C
 * string containing the source code buffer being compiled, and that the
 * returned string is freed with `zrc_free_string` when no longer needed.
 */
char *zrc_diag_fmt(const struct ZrcDiagnostic *diag, const char *source);

/**
 * Print a diagnostic as JSON.
 *
 * # Safety
 * The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
 * and that the returned string is freed with `zrc_free_string` when no longer
 * needed.
 */
char *zrc_diag_fmt_json(const struct ZrcDiagnostic *diag);

/**
 * Free a diagnostic returned by the zrc C API.
 *
 * # Safety
 * The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`
 * that was returned by a function in the zrc C API, and that it has not
 * already been freed.
 */
void zrc_diag_free(struct ZrcDiagnostic *diag);

/**
 * Drive the compilation process.
 *
 * # Errors
 *
 * If compilation fails, a `ZrcDiagnostic` will be returned describing the
 * error.
 *
 * # Safety
 *
 * The caller must guarantee that all C strings are valid and that the pointers
 * passed to this function are valid for the duration of the call.
 */
struct ZrcCompileResult zrc_compile(struct ZrcCompileInputs inputs);

/**
 * Initialize the Zirco JIT for the current thread.
 *
 * This function must only be called once per thread.
 *
 * # Arguments
 * * `frontend_version_string` - The frontend version string.
 * * `cli_args` - The command line arguments passed to the compiler.
 * * `lib_paths` - The paths to search for libraries.
 *
 * # Safety
 * The caller must guarantee that all C strings are valid and that the pointers
 * passed to this function are valid for the duration of the call.
 */
const struct ZrcJitEngine *zrc_jit_init(const char *frontend_version_string,
                                        const char *cli_args,
                                        const char *const *lib_paths,
                                        size_t lib_paths_len);

/**
 * Load a library by name into the given JIT engine.
 *
 * Returns `true` if the library was loaded successfully, or `false` if the
 * load failed.
 *
 * # Safety
 *
 * The caller must guarantee that the engine pointer is valid, and that the
 * library name is a valid C string.
 */
bool zrc_jit_load_library(const struct ZrcJitEngine *engine, const char *lib_name);

/**
 * Load all symbols visible to the current process into the given JIT engine.
 *
 * # Safety
 *
 * The caller must guarantee that the engine pointer is valid.
 */
void zrc_jit_load_visible_symbols(const struct ZrcJitEngine *engine);

/**
 * Create a new JIT module within the given engine.
 *
 * # Safety
 *
 * The caller must guarantee that the engine pointer is valid.
 */
const struct ZrcJitModule *zrc_jit_create_module(const struct ZrcJitEngine *engine);

/**
 * Set a global symbol in the given JIT module to a given pointer.
 *
 * Returns `true` if the symbol was set successfully, or `false` if the set
 * failed.
 *
 * # Safety
 *
 * The caller must guarantee that the module pointer is valid, and that the
 * pointer is valid, lives for the lifetime of the JIT module, and is of the
 * correct type for the symbol.
 */
bool zrc_jit_set_global_symbol(const struct ZrcJitModule *module,
                               const char *symbol_name,
                               const void *ptr);

/**
 * Try to load a function from the given JIT module.
 *
 * Returns NULL if the function is not found, or a pointer to the function if
 * it is found.
 *
 * # Safety
 *
 * The caller must guarantee that the module pointer is valid, and that the
 * function signature matches the expected signature. The caller must also
 * ensure that the module, and hence the JIT engine, is kept alive for the
 * lifetime of the function pointer.
 */
const void *zrc_jit_get_function(const struct ZrcJitModule *module, const char *name);

/**
 * Destroy the given JIT module.
 *
 * # Safety
 *
 * The caller must guarantee that the module pointer is valid and that the
 * module is no longer in use.
 */
void zrc_jit_destroy_module(const struct ZrcJitModule *module);

/**
 * Compile and link a Zirco program into the given JIT module.
 *
 * # Safety
 *
 * The caller must guarantee that the module pointer is valid and that the
 * values within `inputs` are valid for the duration of the call.
 */
struct ZrcCompileResult zrc_jit_compile_and_link(const struct ZrcJitModule *module,
                                                 struct ZrcCompileInputs inputs);

#endif  /* ZRC_H */
