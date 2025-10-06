//! Internal compiler error handling (ICE handling) for the Zirco compiler
//!
//! This module sets up a custom panic hook to handle internal compiler errors
//! (ICEs).
//!
//! When a panic occurs, it provides a user-friendly message and encourages
//! users to report the issue, along with relevant issue context such as the
//! compiler version and command line arguments.

use std::panic::PanicHookInfo;

/// Handles panics and generates the ICE screen
///
/// # Arguments
///
/// * `default_panic_hook` - The default system panic hook to call after
///   printing the ICE message.
/// * `panic_info` - Information about the panic that occurred.
fn ice_hook(
    default_panic_hook: &'static (dyn Fn(&PanicHookInfo<'_>) + Send + Sync),
    panic_info: &PanicHookInfo,
) {
    eprintln!("error: internal compiler error encountered: thread panicked");
    eprintln!("note: this is not your fault! this is ALWAYS a compiler bug.");
    eprintln!(
        "note: compiler bugs threaten the Zirco ecosystem -- we would appreciate a bug report:"
    );
    eprintln!(concat!(
        "note: bug reporting link:",
        " https://github.com/zirco-lang/zrc/issues/new?template=ice.yml"
    ));
    eprintln!();
    eprintln!(
        "{}",
        super::build_info::version()
            .lines()
            .map(|line| format!("note: {line}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    eprintln!();
    eprintln!(
        "note: command line arguments: {}",
        std::env::args().collect::<Vec<_>>().join(" ")
    );
    eprintln!();
    default_panic_hook(panic_info);
    eprintln!();
    eprintln!("error: end internal compiler error. compilation failed.");
}

/// Configures the global panic (ICE) hook
///
/// This function sets up a custom panic hook that provides a user-friendly
/// message when an internal compiler error (ICE) occurs. It also ensures
/// that the `RUST_BACKTRACE` environment variable is set to `1` if it
/// is not already set to `full`, enabling backtraces for debugging purposes.
///
/// # Safety
///
/// This function intentionally leaks memory to obtain a `'static` reference
/// to the default panic hook, as required by the panic hook API. The memory
/// usage is negligible and the process will terminate after compilation, so
/// this leak is acceptable in this context.
pub fn setup_panic_hook() {
    // SAFETY: We intentionally leak the previous panic hook to obtain a 'static
    // reference, as required by the panic hook API. The memory usage is
    // negligible and the process will terminate after compilation, so this leak
    // is acceptable in this context.
    let default_panic_hook: &'static _ = Box::leak(std::panic::take_hook());

    // ICE (internal compiler error) / panic message
    std::panic::set_hook(Box::new(|panic_info| {
        ice_hook(default_panic_hook, panic_info);
    }));

    // Force RUST_BACKTRACE=1 if the user did not set RUST_BACKTRACE=full
    if std::env::var("RUST_BACKTRACE").ok().as_deref() != Some("full") {
        // SAFETY: We're setting an environment variable that only affects this process
        // and we're doing it before any threads are spawned
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }
}
