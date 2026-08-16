//! Extra utilities to aid in testing Zircop
//!
//! Only available on crate feature `test`.
//!
//! # Common patterns in tests
//! The tests for Zircop often involve executing the Zircop runner on a small
//! sample program and checking if a particular diagnostic is produced or not.

/// Creates a Zircop test that runs the given source code and checks diagnostics
/// against a list
#[macro_export]
macro_rules! zircop_lint_test {
	(
        name: $name:ident,
        source: $source:expr,
        diagnostics: $diagnostics:expr
    ) => {
		#[test]
		fn $name() {
			let include_paths = vec![];
			let path = ::std::path::PathBuf::from("<test>");

			let lint_result =
				$crate::runner::run_with_default_passes(&include_paths, &path, $source, false)
					.expect("Compilation should succeed");

			assert_eq!(
				lint_result, $diagnostics,
				"Diagnostics did not match expected"
			);
		}
	};
}
