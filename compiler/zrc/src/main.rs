//! The Zirco compiler

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

fn main(
) -> Result<(), zrc_parser::parser::ZircoParserError<Vec<zrc_parser::ast::stmt::Declaration>>> {
    let (mut cg, bb) = zrc_codegen::FunctionCg::new();
    let (out, _) = zrc_codegen::cg_expr(
        &mut cg,
        &bb,
        &zrc_codegen::CgScope::new(),
        zrc_typeck::type_expr(
            &zrc_typeck::Scope::new(),
            zrc_parser::parser::parse_expr("2 + 2 == 4 ? 2 + 2 : 4").unwrap(),
        )
        .unwrap(),
    )
    .unwrap();

    println!("{cg}");
    println!("Yields {out}");

    Ok(())
}
