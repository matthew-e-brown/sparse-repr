mod cli;

use std::error::Error;
use std::io;
use std::process::ExitCode;

use crate::cli::{ArgError, Args};

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
                Some(ArgError::DisplayHelpShort) => {
                    cli::write_help_short(io::stdout()).expect("failed to print help message to stdout");
                    ExitCode::SUCCESS
                },
                Some(ArgError::DisplayHelpLong) => {
                    cli::write_help_long(io::stdout()).expect("failed to print help message to stdout");
                    ExitCode::SUCCESS
                },
                // Otherwise, print both the error and the usage string
                Some(_arg_error) => {
                    eprintln!("error: {err}");
                    cli::write_usage(io::stderr()).expect("failed to print help message to stderr");
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
