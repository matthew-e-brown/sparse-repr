mod args;

use std::error::Error;
use std::io;
use std::process::ExitCode;

use crate::args::{ArgError, Args};

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            // Try and convert error to an ArgError.
            match err.downcast_ref::<ArgError>() {
                // If it's not an ArgError, or if it was an IO error, print it as-is without the usage string
                None | Some(ArgError::IOError(_, _)) => {
                    eprintln!("error: {err}");
                    ExitCode::FAILURE
                },
                // If they were asking for help, print help and return success
                Some(ArgError::DisplayHelp) => {
                    Args::print_usage_full(io::stdout()).expect("failed to print help message to stdout");
                    ExitCode::SUCCESS
                },
                // Otherwise, print both the error and the usage string
                Some(_) => {
                    eprintln!("error: {err}");
                    Args::print_usage_short(io::stderr()).expect("failed to print help message to stderr");
                    ExitCode::FAILURE
                },
            }
        },
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let args = Args::get()?;

    println!("Got arguments: {args:#?}");
    // let Args { src, dst, fmt } = args;

    Ok(())
}
