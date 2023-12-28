#![warn(
    clippy::perf,
    clippy::complexity,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::needless_lifetimes
)]

use color_eyre::Result;

fn main() -> Result<()> {
    qcalc::run()
}
