use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, IsTerminal, Read, StdinLock, StdoutLock, Write};

use indoc::writedoc;

/// Command line arguments.
#[derive(Debug)]
pub struct Args {
    pub input: Input,
    pub output: Output,
    pub fmt: Format,
    pub undirected: bool,
    pub multiple: bool,
    pub no_preserve_indices: bool,
    pub no_empty: bool,
    pub print_mapping: bool,
}

/// Describes the possible output formats.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Text,
    Binary,
}

impl Default for Format {
    fn default() -> Self {
        Format::Text
    }
}

impl Args {
    pub fn get() -> Result<Args, ArgError> {
        let mut pos1 = None;

        // Option values
        let mut output = None;
        let mut force = None;
        let mut fmt = None;
        let mut undirected = None;
        let mut multiple = None;
        let mut no_preserve_indices = None;
        let mut no_empty = None;
        let mut print_mapping = None;

        let mut args = std::env::args().skip(1).peekable();
        let mut consumed = false; // Did the previous option consume this one as its argument?
        while let Some(raw_arg) = args.next() {
            // By default, `arg` is the whole argument, and `val` is the next argument (though `val` may be ignored
            // depending on what `arg` is, hence the peekable).
            let mut arg = raw_arg.as_str();
            let mut val = args.peek().map(String::as_str);

            // Check for -h/--help before we do *anything* else. If we check for this down in our `match arg` statement,
            // then it is subject to being consumed by previous flags. i.e., `<exe> <input> -f --help` should display
            // the help message instead of erroring on "'--help' is not a valid format".
            if arg == "-h" || val == Some("-h") {
                return Err(ArgError::DisplayHelpShort);
            } else if arg == "--help" || val == Some("--help") {
                return Err(ArgError::DisplayHelpLong);
            }

            // If this argument has already been consumed by the previous option, skip it.
            if consumed {
                consumed = false;
                continue;
            }

            let mut is_positional = true; // Is the argument we're about to process positional?
            let mut val_from_peek = true; // Did we get its value from args.peek()? i.e., do we need to advance args?

            if raw_arg.starts_with("--") && raw_arg != "--" {
                is_positional = false;
                if let Some(i) = raw_arg.find("=") {
                    arg = &raw_arg[..i];
                    val = Some(&raw_arg[i + 1..]); // skip past '='
                    val_from_peek = false;
                }
            } else if raw_arg.starts_with("-") && raw_arg != "-" {
                is_positional = false;
                // Is the option more than "-X"? The value is attached to the option.
                if raw_arg.len() > 2 && raw_arg.is_char_boundary(2) {
                    arg = &raw_arg[..2];
                    val = Some(&raw_arg[2..]);
                    val_from_peek = false;
                }
            }

            /// Sets one of the `Option<T>` variables above to `Some(X)`, *unless* it has already been set; then return
            /// an [`ArgError::DuplicateOpt`] instead.
            macro_rules! set_option {
                // Set an option to `true` if it hasn't been set yet.
                ($opt_name:expr, $option:ident) => {{
                    set_option!($opt_name, $option, true);
                }};
                // Set an option to a given value if it hasn't been set yet, and also consume the next argument.
                ($opt_name:expr, $option:ident, [take] $value:expr) => {{
                    consumed = val_from_peek; // Only skip if the value actually came from the next argument
                    set_option!($opt_name, $option, $value);
                }};
                // Set an option to a given value without if it hasn't been set yet.
                ($opt_name:expr, $option:ident, $value:expr) => {{
                    match $option {
                        None => $option = Some($value),
                        Some(_) => return Err(ArgError::DuplicateOpt($opt_name)),
                    }
                }};
            }

            if is_positional {
                if pos1.is_none() {
                    pos1 = Some(raw_arg);
                } else {
                    return Err(ArgError::UnexpectedArg(raw_arg));
                }
            } else {
                match arg {
                    "-o" | "--output" => match val {
                        Some(value) => set_option!(arg.to_string(), output, [take] value.to_string()),
                        None => return Err(ArgError::MissingArg("<output>")),
                    },
                    "-F" | "--force" => set_option!(arg.to_string(), force),
                    "-f" | "--format" => match val {
                        Some("t" | "txt" | "text") => set_option!(arg.to_string(), fmt, [take] Format::Text),
                        Some("b" | "bin" | "binary") => set_option!(arg.to_string(), fmt, [take] Format::Binary),
                        Some(other) => return Err(ArgError::InvalidArg("<fmt>", other.to_string())),
                        None => return Err(ArgError::MissingArg("<fmt>")),
                    },
                    "-u" | "--undirected" => set_option!(arg.to_string(), undirected),
                    "-m" | "--multiple" => set_option!(arg.to_string(), multiple),
                    "-I" | "--no-preserve-indices" => set_option!(arg.to_string(), no_preserve_indices),
                    "-Z" | "--no-empty" => set_option!(arg.to_string(), no_empty),
                    "-p" | "--print-mapping" => set_option!(arg.to_string(), print_mapping),
                    other => return Err(ArgError::UnknownOpt(other.to_string())),
                }
            }
        }

        let force = force.unwrap_or(false);
        let fmt = fmt.unwrap_or(Format::Text);

        let input: Input = {
            let stdin = io::stdin();
            match pos1.as_deref() {
                None | Some("-") => stdin.lock().into(),
                Some(path) => {
                    if !stdin.is_terminal() {
                        eprintln!(
                            "warning: detected input on stdin, but got <input> argument: ignoring stdin (use '-' to force stdin)"
                        );
                    }

                    File::options()
                        .write(false)
                        .read(true)
                        .open(path)
                        .map_err(|err| ArgError::IOError("input", err))?
                        .into()
                },
            }
        };

        let output: Output = match output.as_deref() {
            None if fmt == Format::Binary && io::stdout().is_terminal() => {
                return Err(ArgError::ImplicitBinaryToStdout);
            },
            None | Some("-") => io::stdout().lock().into(),
            Some(path) => File::options()
                .read(false)
                .write(true)
                .truncate(true)
                .create(true)
                .create_new(force)
                .open(path)
                .map_err(|err| ArgError::IOError("output", err))?
                .into(),
        };

        Ok(Args {
            input,
            output,
            fmt,
            undirected: undirected.unwrap_or(false),
            multiple: multiple.unwrap_or(false),
            no_preserve_indices: no_preserve_indices.unwrap_or(false),
            no_empty: no_empty.unwrap_or(false),
            print_mapping: print_mapping.unwrap_or(false),
        })
    }
}

#[derive(Debug)]
pub enum ArgError {
    /// The '-h' flag was passed; help should be displayed.
    DisplayHelpShort,

    /// The '--help' flag was passed; help should be displayed.
    DisplayHelpLong,

    /// An option with an unknown flag was found.
    UnknownOpt(String),

    /// More options than were expected appeared on the command line.
    UnexpectedArg(String),

    /// An option was specified more times than expected.
    DuplicateOpt(String), // String to be able to output exactly what they typed

    /// A required argument to either the program or an option was missing.
    MissingArg(&'static str),

    /// An argument to either the program or an option is in the wrong format.
    InvalidArg(&'static str, String),

    /// Failed to open a file for input or output.
    IOError(&'static str, io::Error),

    /// Binary format selected, but <output> was set to stdout automatically (don't want to mess up their terminal).
    ImplicitBinaryToStdout,
}

impl Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgError::UnknownOpt(got) => f.write_fmt(format_args!("Unknown option '{got}'.")),
            ArgError::UnexpectedArg(got) => f.write_fmt(format_args!("Unexpected argument '{got}'.")),
            ArgError::DuplicateOpt(opt) => f.write_fmt(format_args!("Duplicate option '{opt}'.")),
            ArgError::MissingArg(opt) => f.write_fmt(format_args!("Missing required argument for '{opt}'.")),
            ArgError::InvalidArg(opt, got) => f.write_fmt(format_args!("Invalid {opt} value '{got}'.")),
            ArgError::IOError(opt, err) => f.write_fmt(format_args!("Could not open {opt} file: {err}")),
            // Actual displaying of help messages is handled in `main`, this variant is not normally displayed.
            ArgError::DisplayHelpShort => f.write_str("Encountered -h in arguments."),
            ArgError::DisplayHelpLong => f.write_str("Encountered --help in arguments."),
            ArgError::ImplicitBinaryToStdout => f.write_str(
                "Detected terminal <output> with binary format selected. \
                Are you sure you want to write raw bytes to the terminal? \
                Set <output> to '-' to write to stdout explicitly.",
            ),
        }
    }
}

impl Error for ArgError {}

/// A readable source of input, either a file or stdin.
#[derive(Debug)]
pub enum Input {
    File(BufReader<File>),
    Stdin(StdinLock<'static>),
}

/// A writable destination for output, either a file or stdout.
#[derive(Debug)]
pub enum Output {
    File(BufWriter<File>),
    Stdout(StdoutLock<'static>),
}

impl Read for Input {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Input::File(reader) => <BufReader<File> as Read>::read(reader, buf),
            Input::Stdin(stdin) => <StdinLock<'static> as Read>::read(stdin, buf),
        }
    }
}

impl BufRead for Input {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        match self {
            Input::File(reader) => <BufReader<File> as BufRead>::fill_buf(reader),
            Input::Stdin(stdin) => <StdinLock<'static> as BufRead>::fill_buf(stdin),
        }
    }

    fn consume(&mut self, amount: usize) {
        match self {
            Input::File(reader) => <BufReader<File> as BufRead>::consume(reader, amount),
            Input::Stdin(stdin) => <StdinLock<'static> as BufRead>::consume(stdin, amount),
        }
    }
}

impl From<File> for Input {
    fn from(value: File) -> Self {
        Self::File(BufReader::new(value))
    }
}

impl From<StdinLock<'static>> for Input {
    fn from(value: StdinLock<'static>) -> Self {
        Self::Stdin(value)
    }
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Output::File(writer) => <BufWriter<File> as Write>::write(writer, buf),
            Output::Stdout(stdout) => <StdoutLock<'static> as Write>::write(stdout, buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Output::File(writer) => <BufWriter<File> as Write>::flush(writer),
            Output::Stdout(stdout) => <StdoutLock<'static> as Write>::flush(stdout),
        }
    }
}

impl From<File> for Output {
    fn from(value: File) -> Self {
        Self::File(BufWriter::new(value))
    }
}

impl From<StdoutLock<'static>> for Output {
    fn from(value: StdoutLock<'static>) -> Self {
        Self::Stdout(value)
    }
}

pub fn write_usage<W: Write>(mut w: W) -> std::io::Result<()> {
    writeln!(
        w,
        "usage: {} <input> [-o <output>] [-f <fmt>] [...OPTIONS] (see -h/--help for other options)",
        env!("CARGO_PKG_NAME")
    )?;
    w.flush()?;
    Ok(())
}

pub fn write_help_short<W: Write>(mut w: W) -> std::io::Result<()> {
    writedoc!(
        w,
        r#"
            {name} {ver}

            {desc}

            USAGE:
                {name} <input> [-f <fmt>]
                {name} <input> [-o <output>] [-f <fmt>]
                cat <input> | {name} [-o <output>] [-f <fmt>]

            ARGUMENTS:
                <input>                    Filepath to read input from [default: stdin].

            OPTIONS:
                -h                         Display this message and exit.
                --help                     Display a longer-form help message and exit.
                -o, --output=<output>      Filepath to write output to [default: stdout].
                -F, --force                Overwrite <output> if it already exists.
                -f, --format=<fmt>         Which to write output as [default: text].
                -u, --undirected           Treats <input> as containing undirected edges rather than directed edges.
                -m, --multiple             Allows for the presence of multiple edges between the same two vertices.
                -I, --no-preserve-indices  Re-number output vertices' indices based solely on the order in which they
                                           appear in <input>.
                -Z, --no-empty             Exclude the initial empty entry from the CSR format.
                -p, --print-mapping        Print a mapping of how vertex labels were re-numbered to stderr.
        "#,
        name = env!("CARGO_PKG_NAME"),
        ver = env!("CARGO_PKG_VERSION"),
        desc = env!("CARGO_PKG_DESCRIPTION"),
    )?;
    w.flush()?;
    Ok(())
}

pub fn write_help_long<W: Write>(mut w: W) -> std::io::Result<()> {
    writedoc!(
        w,
        r#"
            {name} {ver}

            {desc}

            The CSR format used is specifically as described in:
            [1] T. Kelly, "Programming Workbench: Compressed Sparse Row Format for Representing Graphics," ;login:,
                vol. 45, no. 4, pp. 76-82, 2020.
                https://www.usenix.org/system/files/login/articles/login_winter20_16_kelly.pdf

            USAGE:
                {name} <input> [-f <fmt>]
                {name} <input> [-o <output>] [-f <fmt>]
                cat <input> | {name} [-o <output>] [-f <fmt>]

            ARGUMENTS:
                <input>                    Filepath to read input from [default: stdin].

                                           Input is read from stdin by default. This argument can be set to '-' to
                                           explicitly read from stdin.

                                           See the INPUT FORMAT section for details on expected format.

            OPTIONS:
                -h                         Display a shorter-form help message and exit.

                --help                     Display this message and exit.

                -o, --output=<output>      Filepath to write output to [default: stdout].

                                           Output is written to stdout by default. This argument can be set to '-' to
                                           explicitly write to stdout.

                                           See the OUTPUT FORMAT section for details about what the output will be.

                -F, --force                Overwrite <output> if it already exists.

                -f, --format=<fmt>         Which to write output as [default: text].

                                           <fmt> is either 'text' (t/txt/text) or 'binary' (b/bin/binary). See OUTPUT
                                           FORMATS for details.

                -u, --undirected           Treats <input> as containing undirected edges rather than directed edges.

                                           In this mode, all edges u→v are encoded in the output as two directed edges
                                           (u→v and v→u). This is true even if both u→v and v→u appear in the input
                                           (see '-m/--multiple' flag).

                -m, --multiple             Allows for the presence of multiple edges between the same two vertices.

                                           When disabled, duplicated edges are simply skipped/ignored.

                                           INTERACTION WITH -u/--undirected:
                                           Usually, two edges u→v and v→u are two distinct edges. When 'undirected' is
                                           set, both are treated as the same edge; when 'multiple' and 'undirected'
                                           are used together, the edge will be counted as appearing twice. This means
                                           that four edges will be written to output: two from u to v, and two from v
                                           to u.

                -I, --no-preserve-indices  Re-number output vertices' indices based solely on the order in which they
                                           appear in <input>.

                                           By default, when all labels in <input> are numeric, their exact values are
                                           attempted to be preserved (see OUTPUT FORMATS for details). When this flag is
                                           set, all attempts to preserve numeric labels' values are disabled.

                -Z, --no-empty             Exclude the initial empty entry from the CSR format.

                                           According to the paper (see [1] above), the N array in the output should (by
                                           default) start with an "empty" entry, and the first vertex is indexed as 1
                                           (so F[N[1]] to F[N[2]] gives its out-bound adjacency list). This is done to
                                           allow for deleting edges: since the adjacency lists in F reference vertices
                                           by pointing back into N, and no vertices will have index 0, setting an entry
                                           in F to 0 can be interpreted as an edge deletion.

                                           This flag disables this behaviour: with it enabled, the first vertex will be
                                           stored at N[0].

                -p, --print-mapping        Print a mapping of how vertex labels were re-numbered to stderr.

            INPUT FORMAT:
              The input file should contain multiple lines representing an edge-list of a graph. Each line represents a
              single edge (directed by default; see -u/--undirected in OPTIONS). Each line/edge should contain one or
              two vertex labels, separated by whitespace or commas. A line with only one vertex label is treated as a
              disconnected vertex: this will create a vertex with degree zero in the output (unless said label has
              already appeared attached to an edge, then it will do nothing).

              Vertex labels may be any sequence of UTF-8-encoded text, excluding commas or whitespace (since those are
              used to separate labels on a line).

            OUTPUT ORDERING:
              Unless `-I/--no-preserve-indices` is set, vertex labels are sorted in ascending order. Any non-integer
              labels are sorted alphabetically (sort of: they are sorted by code-point) after integers. A consequence of
              this is that, if all vertices in the input are integers starting from 1, and there are no gaps in their
              sequence, their values will be exactly preserved in the final output. NOTE: see the `-Z/--no-empty` flag
              if your vertex labels start at 0 instead of 1.

            OUTPUT FORMATS:
              This section documents file structure of the output formats.

              TEXT FORMAT:
                TODO

              BINARY FORMAT:
                TODO
        "#,
        name = env!("CARGO_PKG_NAME"),
        ver = env!("CARGO_PKG_VERSION"),
        desc = env!("CARGO_PKG_DESCRIPTION"),
    )?;
    w.flush()?;
    Ok(())
}
