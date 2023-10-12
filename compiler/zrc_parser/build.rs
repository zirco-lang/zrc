extern crate lalrpop;

fn main() {
    // Generate the LR parser from our grammar
    lalrpop::process_root().unwrap();
}
