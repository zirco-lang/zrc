fn main() {
    // to keep git tainting info up-to-date
    println!("cargo:rerun-if-changed=../..");
    shadow_rs::ShadowBuilder::builder()
        .build()
        .expect("shadow_rs should have included build info successfully");
}
