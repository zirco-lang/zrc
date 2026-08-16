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
        let mut __zrc_codegen_test_gs = ::zrc_typeck::typeck::GlobalScope::new();
        let __zrc_codegen_typed = ::zrc_typeck::typeck::type_program(
            &mut __zrc_codegen_test_gs,
            ::zrc_parser::parser::parse_program($source, "<test>")
                .expect("parsing should succeed"),
        )
        .expect("typeck should succeed");

        let __zrc_codegen_inputs = $crate::program::CgProgramInputs {
            frontend_version_string: "zrc test runner",
            cli_args: "zrc --fake-args",
            path: &::std::path::PathBuf::from("/fake/path/test.zr"),
            source: $source,
            optimization_level: $crate::OptimizationLevel::None,
            debug_level: ::inkwell::debug_info::DWARFEmissionKind::Full,
            triple: &$crate::get_native_triple(),
            cpu: "",
            file_type: ::inkwell::targets::FileType::Assembly,
        };

        let resulting_ir = $crate::program::cg_program_to_string_without_optimization(
            __zrc_codegen_inputs,
            __zrc_codegen_typed,
        );

        insta::with_settings!({
            description => $source,
        }, {
            insta::assert_snapshot!(resulting_ir);
        });
    };
}
