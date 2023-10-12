//! The Zirco compiler

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use anyhow::Context as _;

fn main() -> anyhow::Result<()> {
    let content =
        std::fs::read_to_string(std::env::args().nth(1).context("no input file provided")?)
            .context("failed to read input file")?;

    println!(
        "{}",
        zrc_codegen::cg_program(
            zrc_typeck::type_program(
                zrc_parser::parser::parse_program(&content).context("parser error")?
            )
            .context("type checker error")?
        )
        .expect("code generation failed -- this is a compiler bug")
    );

    Ok(())
}
