//! Build information module

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
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/shadow.rs"));
}

/// Returns the string which represents the current Zirco version
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
