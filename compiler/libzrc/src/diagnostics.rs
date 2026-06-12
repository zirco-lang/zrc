//! C API for diagnostics emitted by the Zirco compiler

use std::{
    ffi::{CStr, CString, c_char},
    panic::catch_unwind,
    ptr,
};

use zrc::diagnostics::{Diagnostic, Severity};

/// Opaque struct representing a diagnostic in the C API. It is only a handle to
/// pass diagnostics between Rust and C.
// the actual backing value is a type-punned heap Diagnostic
#[repr(C)]
#[derive(Debug)]
pub struct ZrcDiagnostic {
    /// opaque
    _private: [u8; 0],
}

/// Levels of diagnostic severity in the C API.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZrcDiagnosticSeverity {
    /// An error.
    ZRC_DIAG_ERROR = 0,
    /// A warning.
    ZRC_DIAG_WARNING = 1,
}

/// Get the severity of a [`ZrcDiagnostic`]
///
/// # Safety
/// The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`.
#[unsafe(no_mangle)]
pub const unsafe extern "C" fn zrc_diag_severity(
    diag: *const ZrcDiagnostic,
) -> ZrcDiagnosticSeverity {
    // SAFETY: the caller guarantees that `diag` is valid
    let diag = unsafe { &*diag.cast::<Diagnostic>() };
    match diag.severity {
        Severity::Error => ZrcDiagnosticSeverity::ZRC_DIAG_ERROR,
        Severity::Warning => ZrcDiagnosticSeverity::ZRC_DIAG_WARNING,
    }
}

/// Obtain the "primary line" of a [`ZrcDiagnostic`] as a C string. This is the
/// main message of the diagnostic: `error[E1234]: message`
///
/// # Safety
/// The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
/// and that the returned string is freed with `zrc_free_string` when no longer
/// needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt_primary_line(diag: *const ZrcDiagnostic) -> *mut c_char {
    let result = catch_unwind(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };
        CString::new(diag.to_string()).ok()
    });

    match result {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}

/// Obtain the string representation of a full [`ZrcDiagnostic`], including all
/// related context, as a C string.
///
/// # Safety
/// The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
/// and that `source` is either null or a valid pointer to a null-terminated C
/// string containing the source code buffer being compiled, and that the
/// returned string is freed with `zrc_free_string` when no longer needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt(
    diag: *const ZrcDiagnostic,
    source: *const c_char,
) -> *mut c_char {
    let result = catch_unwind(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };
        let source = if source.is_null() {
            None
        } else {
            // SAFETY: the caller guarantees that `source` is a valid C string
            unsafe { CStr::from_ptr(source) }.to_str().ok()
        };
        CString::new(diag.print(source)).ok()
    });

    match result {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}

/// Print a diagnostic as JSON.
///
/// # Safety
/// The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`,
/// and that the returned string is freed with `zrc_free_string` when no longer
/// needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt_json(diag: *const ZrcDiagnostic) -> *mut c_char {
    let result = catch_unwind(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };
        CString::new(diag.print_json()).ok()
    });

    match result {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}

/// Free a diagnostic returned by the zrc C API.
///
/// # Safety
/// The caller must ensure that `diag` is a valid pointer to a `ZrcDiagnostic`
/// that was returned by a function in the zrc C API, and that it has not
/// already been freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_free(diag: *mut ZrcDiagnostic) {
    if !diag.is_null() {
        // SAFETY: the caller guarantees that `diag` is a valid pointer to a diagnostic
        // we've allocated
        unsafe {
            drop(Box::from_raw(diag.cast::<Diagnostic>()));
        }
    }
}
