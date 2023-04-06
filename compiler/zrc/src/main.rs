use zrc_parser::parser;

fn main() {
    println!("{:?}", parser::parse_program("fn main() {}"));
}
