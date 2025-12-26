/// CLI argument parsing for zrcat
use clap::Parser;

/// zrcat - Zirco C Auto Translator
///
/// Translates C header files (after preprocessing) to Zirco .zh equivalents.
#[derive(Parser, Debug)]
#[command(name = "zrcat")]
#[command(about = "Zirco C Auto Translator", long_about = None)]
pub struct Cli {
    /// Input C header file path
    pub input: Option<String>,

    /// Output file path (default: stdout)
    #[arg(short, long)]
    pub output: Option<String>,

    /// Read from stdin instead of a file
    #[arg(long)]
    pub stdin: bool,

    /// Run C preprocessor (cpp) on input first
    #[arg(short, long)]
    pub preprocess: bool,

    /// Show version information
    #[arg(short = 'V', long)]
    pub version: bool,
}
