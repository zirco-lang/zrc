#![doc=include_str!("../README.md")]
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
    let default_panic_hook = std::panic::take_hook();

    // ICE (internal compiler error) / panic message
    std::panic::set_hook(Box::new(move |panic_info| {
        eprintln!("error: internal compiler error encountered: thread panicked");
        eprintln!("note: this is not your fault! this is ALWAYS a compiler bug.");
        eprintln!(
            "note: compiler bugs threaten the Zirco ecosystem -- we would appreciate a bug report:"
        );
        eprintln!("note: bug reporting link: https://github.com/zirco-lang/zrc/issues/new?template=bug.yml");
        eprintln!();
        default_panic_hook(panic_info);
        eprintln!();
        eprintln!("error: end internal compiler error. compilation failed.");
    }));

    let content =
        std::fs::read_to_string(std::env::args().nth(1).context("no input file provided")?)
            .context("failed to read input file")?;

    let result = compile(&content);
    match result {
        Err(diagnostic) => eprintln!("{}", diagnostic.print(&content)),
        Ok(x) => println!("{x}"),
    }

    Ok(())
}

/// Drive the compilation process.
fn compile(content: &str) -> Result<String, zrc_diagnostics::Diagnostic> {
    Ok(zrc_codegen::cg_program(zrc_typeck::typeck::type_program(
        zrc_parser::parser::parse_program(content)?,
    )?)
    .expect("code generation should not fail"))
}
