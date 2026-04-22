//! C API for diagnostics emitted by the zrc compiler.

use std::{
    ffi::{CStr, CString, c_char},
    panic::{AssertUnwindSafe, catch_unwind},
    ptr,
};

use zrc::diagnostics::{Diagnostic, Severity};

/// Opaque struct representing a diagnostic in the C API. The actual contents of
/// this struct are not exposed to C code, and it is only used as a handle to
/// pass diagnostics between Rust and C.
// The actual backing value is a heap Diagnostic.
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

/// Get the severity of a diagnostic as a [`ZrcDiagnosticSeverity`].
///
/// # Safety
/// The caller must guarantee that `diag` is a valid pointer to a
/// `ZrcDiagnostic`.
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

/// Obtain the "primary line" of a diagnostic.
/// This is simply the line with "error[E1234]: message"
///
/// # Safety
/// The caller must guarantee that `diag` is a valid pointer to a
/// `ZrcDiagnostic`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt_primary(diag: *const ZrcDiagnostic) -> *mut c_char {
    match catch_unwind(AssertUnwindSafe(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };
        CString::new(diag.to_string()).ok()
    })) {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}

/// Print a full diagnostic message. The source code for the original file
/// compiled should be passed in as `source` (or null, if it was read from a
/// file) to allow diagnostics that reference `/dev/stdin`.
///
/// # Safety
/// The caller must guarantee that `diag` is a valid pointer to a
/// `ZrcDiagnostic`, and that `source` is either null or a valid pointer to a
/// null-terminated C string containing the source code of the file being
/// compiled.
///
/// The caller is responsible for freeing the returned string when it is no
/// longer needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt(
    diag: *const ZrcDiagnostic,
    source: *const c_char,
) -> *mut c_char {
    match catch_unwind(AssertUnwindSafe(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };
        let source = if source.is_null() {
            None
        } else {
            // SAFETY: the caller guarantees that `source` is a valid C string
            Some(
                unsafe { CStr::from_ptr(source) }
                    .to_string_lossy()
                    .into_owned(),
            )
        };
        CString::new(diag.print(source.as_deref())).ok()
    })) {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}

/// Print a diagnostic as JSON.
///
/// # Safety
/// The caller must guarantee that `diag` is a valid pointer to a
/// `ZrcDiagnostic`. The caller is responsible for freeing the returned string
/// when it is no longer needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zrc_diag_fmt_json(diag: *const ZrcDiagnostic) -> *mut c_char {
    match catch_unwind(AssertUnwindSafe(|| {
        // SAFETY: the caller guarantees that `diag` is valid
        let diag = unsafe { &*diag.cast::<Diagnostic>() };

        CString::new(diag.print_json()).ok()
    })) {
        Ok(Some(cstr)) => cstr.into_raw(),
        _ => ptr::null_mut(),
    }
}
