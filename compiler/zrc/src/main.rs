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
    let content = std::fs::read_to_string(std::env::args().nth(1).expect("no input file provided"))
        .expect("failed to read input file");

    println!(
        "{}",
        zrc_codegen::cg_program(
            zrc_typeck::type_program(zrc_parser::parser::parse_program(&content).unwrap()).unwrap()
        )
        .unwrap()
    );
}
