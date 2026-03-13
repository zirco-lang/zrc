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
enum ZrcDiagnosticSeverity {
  /**
   * An error.
   */
  ZRC_DIAG_ERROR = 0,
  /**
   * A warning.
   */
  ZRC_DIAG_WARNING = 1,
};
typedef uint8_t ZrcDiagnosticSeverity;

/**
 * The list of possible outputs `zrc` can emit in
 *
 * Usually you will want to use `llvm`.
 */
enum ZrcOutputFormat {
  /**
   * LLVM IR
   */
  ZRC_OUTPUT_LLVM = 0,
  /**
   * Assembly
   */
  ZRC_OUTPUT_ASM = 1,
  /**
   * Object file
   */
  ZRC_OUTPUT_OBJ = 2,
};
typedef uint8_t ZrcOutputFormat;

/**
 * The level of optimization to apply during code generation.
 */
enum OptimizationLevel {
  /**
   * No optimizations. This is the default.
   */
  ZRC_OPTIMIZE_NONE = 0,
  /**
   * Some optimizations that do not significantly increase compile time or
   * code size.
   */
  ZRC_OPTIMIZE_LESS = 1,
  /**
   * The default level of optimizations.
   */
  ZRC_OPTIMIZE_DEFAULT = 2,
  /**
   * Aggressive optimizations that may significantly increase compile time or
   * code size.
   */
  ZRC_OPTIMIZE_AGGRESSIVE = 3,
};
typedef uint8_t OptimizationLevel;

/**
 * The level of debug information to include during code generation.
 */
enum DebugLevel {
  /**
   * No debug information. This is the default.
   */
  ZRC_DI_NONE = 0,
  /**
   * Include basic debug information.
   */
  ZRC_DI_BASIC = 1,
  /**
   * Include full debug information.
   */
  ZRC_DI_FULL = 2,
};
typedef uint8_t DebugLevel;

/**
 * Opaque struct representing a diagnostic in the C API. The actual contents of
 * this struct are not exposed to C code, and it is only used as a handle to
 * pass diagnostics between Rust and C.
 */
typedef struct ZrcDiagnostic {
  /**
   * opaque
   */
  uint8_t _private[0];
} ZrcDiagnostic;

/**
 * The results of a compilation attempt, including either the output data or
 * the diagnostics.
 */
typedef struct ZrcCompileResult {
  /**
   * Whether compilation succeeded or failed.
   */
  bool success;
  /**
   * The output data, if compilation succeeded.
   */
  uint8_t *data;
  /**
   * The size of the output data, if compilation succeeded.
   */
  size_t size;
  /**
   * The diagnostic, if compilation failed.
   */
  struct ZrcDiagnostic *diagnostics;
} ZrcCompileResult;

/**
 * Get the severity of a diagnostic as a [`ZrcDiagnosticSeverity`].
 *
 * # Safety
 * The caller must guarantee that `diag` is a valid pointer to a
 * `ZrcDiagnostic`.
 */
ZrcDiagnosticSeverity zrc_diag_severity(const struct ZrcDiagnostic *diag);

/**
 * Obtain the "primary line" of a diagnostic.
 * This is simply the line with "error[E1234]: message"
 *
 * # Safety
 * The caller must guarantee that `diag` is a valid pointer to a
 * `ZrcDiagnostic`.
 */
char *zrc_diag_fmt_primary(const struct ZrcDiagnostic *diag);

/**
 * Print a full diagnostic message. The source code for the original file
 * compiled should be passed in as `source` (or null, if it was read from a
 * file) to allow diagnostics that reference `/dev/stdin`.
 *
 * # Safety
 * The caller must guarantee that `diag` is a valid pointer to a
 * `ZrcDiagnostic`, and that `source` is either null or a valid pointer to a
 * null-terminated C string containing the source code of the file being
 * compiled.
 *
 * The caller is responsible for freeing the returned string when it is no
 * longer needed.
 */
char *zrc_diag_fmt(const struct ZrcDiagnostic *diag, const char *source);

/**
 * Print a diagnostic as JSON.
 *
 * # Safety
 * The caller must guarantee that `diag` is a valid pointer to a
 * `ZrcDiagnostic`. The caller is responsible for freeing the returned string
 * when it is no longer needed.
 */
char *zrc_diag_fmt_json(const struct ZrcDiagnostic *diag);

/**
 * Drive the compilation process.
 *
 * This function takes the source code as input and processes it through
 * the various stages of compilation: parsing, type checking, and code
 * generation. Depending on the specified output format, it can return the AST,
 * TAST, LLVM IR, assembly, or object code.
 *
 * # Arguments
 *
 * * `frontend_version_string` - A string representing the version of the
 *   frontend.
 * * `include_paths` - The list of directories to search for includes.
 * * `emit` - The desired output format.
 * * `parent_directory` - The parent directory of the source file.
 * * `file_name` - The name of the source file.
 * * `cli_args` - The command line arguments passed to the compiler.
 * * `content` - The source code content to be compiled.
 * * `optimization_level` - The optimization level for code generation.
 * * `debug_mode` - The debug level for code generation.
 * * `triple` - The target triple for code generation.
 * * `cpu` - The target CPU for code generation.
 * * `forbid_unlisted_includes` - Whether to restrict includes to search paths
 *   only.
 *
 * # Errors
 *
 * If compilation fails, the provided `ZrcDiagnostic**` will
 * be set to point to a newly allocated `ZrcDiagnostic` containing the error
 * information. The caller is responsible for freeing this diagnostic when it
 * is no longer needed. If compilation succeeds, the provided `uint8_t**` and
 * `size_t*` will be set to point to a newly allocated buffer containing the
 * output data and its size, respectively. The caller is responsible for
 * freeing this buffer when it is no longer needed.
 *
 * # Safety
 *
 * The caller must guarantee that all C strings passed to this function are
 * valid null-terminated strings, and that the pointers provided for the output
 * data and diagnostics are valid for writing. The caller is also responsible
 * for freeing any allocated memory returned by this function when it is no
 * longer needed.
 */
struct ZrcCompileResult zrc_compile(const char *frontend_version_string,
                                    const char *const *include_paths,
                                    size_t include_paths_len,
                                    ZrcOutputFormat emit,
                                    const char *parent_directory,
                                    const char *file_name,
                                    const char *cli_args,
                                    const char *content,
                                    OptimizationLevel optimization_level,
                                    DebugLevel debug_mode,
                                    const char *triple,
                                    const char *cpu,
                                    bool forbid_unlisted_includes);

#endif  /* ZRC_H */
