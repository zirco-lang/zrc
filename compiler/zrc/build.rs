fn main() -> shadow_rs::SdResult<()> {
    // to keep git tainting info up-to-date
    println!("cargo:rerun-if-changed=../..");
    shadow_rs::new()
}
