//! Build information for the Zirco compiler.
//!
//! This module is responsible for generating and providing access to build
//! information such as the version number, commit hash, build time, and
//! whether the build was made from a clean git state.
//!
//! This information is generated at compile time and embedded into the
//! binary, allowing the compiler to report its own version and build details.
//! This is useful for debugging, user support, and ensuring compatibility
//! between different versions of the compiler and its components.
//!
//! The build information is generated using a build script (`build.rs`)
//! that runs before the main compilation process. The script gathers the
//! necessary data from the environment and git repository, then writes it
//! to a Rust source file that is included in the compilation.

use std::fmt::Write as FmtWrite;

#[doc(hidden)]
#[allow(
    clippy::all,
    clippy::nursery,
    clippy::pedantic,
    missing_docs,
    clippy::missing_docs_in_private_items,
    clippy::restriction
)]
/// Internal module containing build information constants.
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/shadow.rs"));
}

/// Returns the string representation of the Zirco compiler's version and build
/// information.
///
/// This includes the version number, commit hash, build target, build time,
/// Rust version, and whether the build was made from a clean git state.
/// If the build was made from a tainted state (i.e., there were uncommitted
/// changes), it also lists the modified files.
pub fn version() -> String {
    format!(
        concat!(
            "{zrc} version {version} ({commit}, {taint_string}) built for {target} on {time}",
            " ({mode} mode)",
            "\n{rust_version} ({rust_channel} on {build_os})\n",
            "{cargo_version}{taint_extra}"
        ),
        zrc = build::PROJECT_NAME,
        version = build::PKG_VERSION,
        commit = build::COMMIT_HASH,
        taint_string = if build::GIT_CLEAN {
            "not tainted"
        } else {
            "tainted!"
        },
        target = build::BUILD_TARGET,
        time = build::BUILD_TIME_3339,
        mode = build::BUILD_RUST_CHANNEL,
        rust_version = build::RUST_VERSION,
        rust_channel = build::RUST_CHANNEL,
        build_os = build::BUILD_OS,
        cargo_version = build::CARGO_VERSION,
        taint_extra = if build::GIT_CLEAN {
            String::new()
        } else {
            // git is tainted
            format!(
                "\ntainted files:{}",
                build::GIT_STATUS_FILE
                    .lines()
                    .fold(String::new(), |mut output, x| {
                        write!(output, "\n{}", {
                            x.strip_suffix(" (dirty)")
                                .or_else(|| x.strip_suffix(" (staged)"))
                                .unwrap_or(x)
                        })
                        .expect("writing to a string should succeed");
                        output
                    })
            )
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_string_contains_expected_components() {
        let version = version();

        assert!(version.contains("version"));
        assert!(version.contains(build::PROJECT_NAME));
        assert!(version.contains(build::PKG_VERSION));
    }

    #[test]
    fn version_string_contains_build_info() {
        let version = version();

        assert!(version.contains(build::BUILD_TARGET));
        assert!(version.contains(build::RUST_VERSION));
        assert!(version.contains(build::CARGO_VERSION));
    }
}
