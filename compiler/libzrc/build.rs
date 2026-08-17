fn main() {
	println!("cargo:rerun-if-changed=src");
	let bindings = cbindgen::generate(".").expect("failed to generate zrc.h");
	bindings.write_to_file("zrc.h");
}
