fn main() {
    match cbindgen::generate(".") {
        Ok(bindings) => {
            bindings.write_to_file("zrc.h");
        }
        Err(_) => {
            // no big deal
        }
    };
}
