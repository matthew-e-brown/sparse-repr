mod cli;
mod graph;

use std::error::Error;
use std::io::{self, Read};
use std::process::ExitCode;

use self::cli::{ArgError, Args};
use self::graph::{GraphBuilder, Vertex};

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
                Some(_arg_err) => {
                    cli::write_usage(io::stderr()).expect("failed to print help message to stderr");
                    eprintln!("\nerror: {err}");
                    ExitCode::FAILURE
                },
            }
        },
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let args = Args::get()?;

    // No matter what we do, we can't stream output before input is finished; we need to fully process input before we
    // can start writing output (since we need to know of *all* possible edges out of a vertex before we start writing
    // its data, and the input could be completely unordered). So, as opposed to iterating lines all as allocated
    // strings, we can simply read the entire thing into one huge String. This allows all our types to work with
    // cheap-to-copy `&str` references into this large buffer instead. On average, most labels should be single-letter
    // labels or hopefully small numbers. For numbers ≤8 digits long, it is actually smaller to keep them stored as
    // UTF-8 text anyways (as opposed to mapping them into numbers as we stream).
    let input = {
        let mut buf = String::new();
        let mut src = args.input;
        src.read_to_string(&mut buf)?;
        buf
    };

    let mut graph = GraphBuilder::new()
        .directed(!args.undirected)
        .multiple(args.multiple)
        .include_zero(!args.no_empty)
        .build();

    // There's not yet any way to create a `&dyn Pattern`, since `Pattern` is still unstable... so we'll have to make
    // the entire splitting routine dynamic.
    let splitter: &dyn Fn(&str) -> (Vertex, Option<Vertex>) = match args.delimiter {
        Some(delim) => &move |line| {
            let mut iter = line.split(&delim[..]).map(Vertex::from_str);
            (iter.next().unwrap(), iter.next())
        },
        None => &|line| {
            let mut iter = line.split(char::is_whitespace).map(Vertex::from_str);
            (iter.next().unwrap(), iter.next())
        },
    };

    for line in input.lines() {
        if line.trim().len() == 0 {
            continue;
        }

        match splitter(line) {
            (v1, Some(v2)) => graph.add_edge(v1, v2),
            (v1, None) => graph.add_vertex(v1),
        }
    }

    // - If `--no-preserve-indices` is set, simply loop through the indices and output them in that order.
    // - Otherwise, sort the vertices based on asc(numeric), asc(string). If all values were numeric and had no gaps,
    //   this will result in them being kept in the correct order.
    // - Problem: in both cases, vertices may be re-numbered. Need to ensure that re-ordered vertices are also
    //   re-ordered in their outputs (I think this is handled?).
    if !args.no_preserve_indices {
        graph.sort_vertices();
    }

    eprintln!("Found {} total edges between {} vertices", graph.num_edges(), graph.num_verts());

    if args.print_mapping {
        eprintln!("MAPPINGS:");
        for (vertex, index) in graph.mappings() {
            eprintln!("  {vertex} -> {index}");
        }
    }

    let (n, f) = graph.generate_csr();
    println!("N: {n:?}");
    println!("F: {f:?}");
    Ok(())
}
