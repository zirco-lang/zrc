use zrc_parser::parser;

fn main() {
    println!(
        "{:?}",
        parser::parse_program("fn main() -> struct{x:i32} { let x: struct{} = 5; }")
    );
}
