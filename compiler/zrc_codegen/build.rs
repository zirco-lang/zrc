fn main() {
    let llvm_sys_root = std::env::var("LLVM_SYS_201_PREFIX")
        .expect("LLVM_SYS_201_PREFIX was not found in the environment. Is LLVM 20 installed?");

    let mut build = cc::Build::new();
    build.cpp(true);

    build.file("cpp/optimize_module.cpp");

    build.include(format!("{}/include", llvm_sys_root));
    build.flag_if_supported("-std=c++17");
    build.flag_if_supported("-w");

    build.compile("zrc_codegen_optimize_module");

    println!("cargo:rerun-if-changed=cpp/optimize_module.cpp");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_201_PREFIX");
    println!("cargo:rustc-link-search=native={}/lib", llvm_sys_root);
    println!("cargo:rustc-link-lib=dylib=LLVM-20");
}
