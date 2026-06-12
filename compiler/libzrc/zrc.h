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
typedef struct ZrcDiagnostic {
  /**
   * opaque
   */
  uint8_t _private[0];
} ZrcDiagnostic;

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
   * If `success` is true, the compiled output data. The caller is
   * responsible for freeing this with `zrc_free_string` when no longer
   * needed.
   */
  char *data;
  /**
   * If `success` is true, the size of the compiled output data in bytes.
   */
  size_t size;
  /**
   * If `success` is false, a pointer to a `ZrcDiagnostic` describing the
   * error.
   */
  const struct ZrcDiagnostic *diagnostic;
} ZrcCompileResult;

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
 * If compilation fails, the provided `ZrcDiagnostic*` will
 * be set to point to a newly allocated `ZrcDiagnostic` containing the error
 * information. The caller is responsible for freeing this diagnostic when it
 * is no longer needed. If compilation succeeds, the provided `uint8_t*` and
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
                                    ZrcOptimizationLevel optimization_level,
                                    ZrcDebugLevel debug_mode,
                                    const char *triple,
                                    const char *cpu,
                                    bool forbid_unlisted_includes);

#endif  /* ZRC_H */
