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
    println!(
        "{}",
        zrc_typeck::type_block(
            &zrc_typeck::Scope::new(),
            zrc_parser::parser::parse_program(
                "fn main(argc: u8, argv: *u8) -> u8 {
                    struct S {
                        x: i32,
                        y: i32,
                    }
                    let a: S;
                    a.x = 7;
                    a.y = 2;
                    if (a.x + (&a)->y == 9) return 1 as u8;
                    return 0 as u8;
                }"
            )?
            .into_iter()
            .map(zrc_parser::ast::stmt::Stmt::Declaration)
            .collect(),
            false,
            zrc_typeck::BlockReturnAbility::MustNotReturn
        )
        .unwrap()
        .0
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
    );

    Ok(())
}
