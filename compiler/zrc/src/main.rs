#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions)]

use zrc_parser::parser;

fn main() {
    println!("{:?}", parser::parse_program("fn main() {}"));
}
