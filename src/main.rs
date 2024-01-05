#![warn(
    clippy::perf,
    clippy::complexity,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::needless_lifetimes
)]

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    qcalc::run()
}
