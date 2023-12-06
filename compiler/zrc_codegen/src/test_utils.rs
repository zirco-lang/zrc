//! Extra utilities to help in testing the code generator
//!
//! Only available on crate feature `test`.
//!
//! # Common patterns in tests
//! The old form of unit tests for `zrc_codegen` involved directly invoking the
//! functions we are testing, which meant we needed to prepare all of the needed
//! scope for inkwell and this led to very complicated and long tests (with most
//! averaging at least 100 lines). This deterred me from testing as often as I
//! wanted, and many bugs were not found because of this.
//!
//! Instead, we now make our unit tests entire *programs* that we compile
//! through the same process that the frontend uses, and we snapshot test the
//! result. However, this almost turns out to be an integration test, and not a
//! unit test! The reason I still consider these to be unit tests is because the
//! code is short and meant to invoke a very specific block of code in whatever
//! function we are testing, and not a full program. We will have full
//! integration tests in the future.
//!
//! Read any test for an example of how they flow. We recommend `cargo-insta` be
//! installed so you can `cargo insta review` any changed snapshots.

/// Creates a snapshot test given a valid input program
#[macro_export]
macro_rules! cg_snapshot_test {
    ($source:expr) => {
        let resulting_ir = $crate::cg_program_to_string(
            "zrc test runner",
            "/fake/path",
            "test.zr",
            // do not use real args because the text executables have a hash in their name and
            // this would mess up snapshots
            "zrc --fake-args",
            $source,
            ::zrc_typeck::typeck::type_program(
                ::zrc_parser::parser::parse_program($source).expect("parsing should succeed")
            ).expect("typeck should succeed"),
            ::inkwell::OptimizationLevel::None,
            ::inkwell::debug_info::DWARFEmissionKind::Full,
            &$crate::get_native_triple(),
            "",
        );

        insta::with_settings!({
            description => $source,
        }, {
            insta::assert_snapshot!(resulting_ir);
        });
    }
}
