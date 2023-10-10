//! The Zirco compiler

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

fn main() {
    println!(
        "{}",
        zrc_codegen::cg_program(
            zrc_typeck::type_program(
                zrc_parser::parser::parse_program(
                    r#"
                        fn puts(str: *u8);
                        fn main(argc: i32, argv: **u8) -> i32 {
                            puts("Hello, world!");
                            return 0;
                        }
                    "#
                )
                .unwrap()
            )
            .unwrap()
        )
        .unwrap()
    );
}
