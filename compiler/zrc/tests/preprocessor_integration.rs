use std::fs;
use std::path::PathBuf;

#[test]
fn preprocessor_includes_files() {
    let temp_dir = std::env::temp_dir();
    let helper_file = temp_dir.join("test_helper.zr");
    let main_file = temp_dir.join("test_main.zr");

    // Create helper file
    fs::write(
        &helper_file,
        "fn helper() -> i32 {\n    return 42;\n}",
    )
    .expect("Failed to write helper file");

    // Create main file with include
    let main_content = "#include \"test_helper.zr\"\n\nfn main() -> i32 {\n    return helper();\n}";
    fs::write(&main_file, main_content).expect("Failed to write main file");

    // Read the main file
    let content = fs::read_to_string(&main_file).expect("Failed to read main file");

    // Preprocess
    let preprocessed = zrc_preprocessor::preprocess(&content, &main_file)
        .expect("Preprocessing should succeed");

    // Verify the preprocessed output contains both functions
    assert!(preprocessed.contains("fn helper()"));
    assert!(preprocessed.contains("return 42;"));
    assert!(preprocessed.contains("fn main()"));
    assert!(preprocessed.contains("return helper();"));

    // Cleanup
    drop(fs::remove_file(&helper_file));
    drop(fs::remove_file(&main_file));
}
