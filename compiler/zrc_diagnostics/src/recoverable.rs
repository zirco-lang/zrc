//! Types for error recovery during type checking
//!
//! This module provides types that allow the type checker to recover from
//! errors and continue checking the rest of the program, enabling better
//! diagnostic reporting by finding multiple errors in a single pass.

/// Represents a value in type checking that may be recoverable or fatal
///
/// This type is used in the type checker to distinguish between errors that
/// allow recovery (where a "poison" value can be substituted to continue
/// checking) and errors that are truly fatal and cannot be recovered from.
///
/// # Examples
///
/// ```
/// use zrc_diagnostics::{MaybeRecoverable, Diagnostic, DiagnosticKind, Severity};
/// use zrc_utils::{span::Spanned, spanned_test};
///
/// // A recoverable error with a fallback value
/// let recoverable: Result<i32, MaybeRecoverable<i32, Diagnostic>> = 
///     Err(MaybeRecoverable::Recoverable(
///         42, // fallback value
///         Diagnostic(
///             Severity::Error,
///             spanned_test!(0, DiagnosticKind::InvalidToken, 5)
///         )
///     ));
///
/// // A fatal error with no fallback
/// let fatal: Result<i32, MaybeRecoverable<i32, Diagnostic>> = 
///     Err(MaybeRecoverable::Fatal(
///         Diagnostic(
///             Severity::Error,
///             spanned_test!(0, DiagnosticKind::InvalidToken, 5)
///         )
///     ));
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum MaybeRecoverable<T, E> {
    /// An unrecoverable fatal error
    Fatal(E),
    /// A recoverable value, although it may be poisoned
    Recoverable(T, E),
}

impl<T, E> From<E> for MaybeRecoverable<T, E> {
    fn from(err: E) -> Self {
        Self::Fatal(err)
    }
}

/// Shorthand for a result with a [`MaybeRecoverable`]
///
/// This type alias makes it easier to work with results that can either
/// succeed, fail recoverably (with a fallback value), or fail fatally.
///
/// # Examples
///
/// ```
/// use zrc_diagnostics::{RecoverableResult, MaybeRecoverable, Diagnostic};
///
/// fn type_check_expr() -> RecoverableResult<String, Diagnostic> {
///     // On success
///     Ok("i32".to_string())
///     
///     // Or on recoverable error
///     // Err(MaybeRecoverable::Recoverable("poison".to_string(), diagnostic))
///     
///     // Or on fatal error
///     // Err(MaybeRecoverable::Fatal(diagnostic))
/// }
/// ```
pub type RecoverableResult<T, E> = Result<T, MaybeRecoverable<T, E>>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maybe_recoverable_from_error() {
        let error = "test error";
        let recoverable: MaybeRecoverable<i32, &str> = MaybeRecoverable::from(error);
        assert_eq!(recoverable, MaybeRecoverable::Fatal("test error"));
    }

    #[test]
    fn maybe_recoverable_variants() {
        let fatal: MaybeRecoverable<i32, &str> = MaybeRecoverable::Fatal("error");
        let recoverable: MaybeRecoverable<i32, &str> = MaybeRecoverable::Recoverable(42, "error");

        match fatal {
            MaybeRecoverable::Fatal(e) => assert_eq!(e, "error"),
            MaybeRecoverable::Recoverable(_, _) => panic!("Expected Fatal variant"),
        }

        match recoverable {
            MaybeRecoverable::Fatal(_) => panic!("Expected Recoverable variant"),
            MaybeRecoverable::Recoverable(v, e) => {
                assert_eq!(v, 42);
                assert_eq!(e, "error");
            }
        }
    }

    #[test]
    fn recoverable_result_is_result() {
        let ok_result: RecoverableResult<i32, &str> = Ok(42);
        let err_result: RecoverableResult<i32, &str> =
            Err(MaybeRecoverable::Fatal("error"));

        assert!(ok_result.is_ok());
        assert!(err_result.is_err());
    }
}
