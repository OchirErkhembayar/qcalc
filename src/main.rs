#![warn(
    clippy::perf,
    clippy::complexity,
    clippy::print_stderr,
    clippy::needless_lifetimes
)]

use std::error::Error;

use qcalc::eval;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args();
    args.next();
    let inputs = args.collect::<Vec<_>>();
    if inputs.is_empty() {
        qcalc::tui()
    } else {
        let now = std::time::Instant::now();
        for input in inputs.iter() {
            let res = eval(input)?;
            // println!("{}", res);
        }
        let elapsed = now.elapsed();
        println!("Time: {}", elapsed.as_millis());
        Ok(())
    }
}
