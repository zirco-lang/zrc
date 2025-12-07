use std::{
    env::consts::{ARCH, OS},
    fs,
    process::Command,
};

#[cfg(debug_assertions)]
const BUILD_TYPE: &str = "debug";
#[cfg(not(debug_assertions))]
const BUILD_TYPE: &str = "release";

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let version_path = std::path::Path::new(&out_dir).join("version");

    let version_string = format!(
        "{} {} ({}{}, {} build, {} [{}])",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        get_git_string(),
        if is_working_tree_clean() {
            ""
        } else {
            " (dirty)"
        },
        BUILD_TYPE,
        OS,
        ARCH
    );

    fs::write(version_path, version_string).unwrap();
}

fn get_git_string() -> String {
    let commit = Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--pretty=format:%h")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).to_string());

    let branch = Command::new("git")
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("HEAD")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).to_string());

    match (commit, branch) {
        (Some(c), Some(b)) => format!("{b}@{c}"),
        (Some(c), None) => format!("unknown@{}", c),
        (None, Some(b)) => format!("{}@unknown", b),
        (None, None) => "commit unknown".to_string(),
    }
}

fn is_working_tree_clean() -> bool {
    let status = Command::new("git")
        .arg("diff")
        .arg("--quiet")
        .arg("--exit-code")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .status()
        .unwrap_or_default();

    status.code().unwrap_or(1) == 0
}
