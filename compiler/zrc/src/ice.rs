//! Internal compiler error handling (ICE handling) for the Zirco compiler

use std::panic::PanicHookInfo;

/// Handles panics and generates the ICE screen
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
        std::env::set_var("RUST_BACKTRACE", "1");
    }
}
