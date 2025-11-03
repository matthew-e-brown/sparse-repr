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
    pub bits: BitWidth,
    pub endian: Endianness,
    pub delimiter: Option<String>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
pub enum BitWidth {
    u8,
    u16,
    u32,
    u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
}

impl Args {
    pub fn get() -> Result<Args, ArgError> {
        let mut pos1 = None;

        // Option values
        let mut output = None;
        let mut force = None;
        let mut fmt = None;
        let mut bits = None;
        let mut endian = None;
        let mut delimiter = None;
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
                    "-b" | "--bits" => match val {
                        Some("8") => set_option!(arg.to_string(), bits, [take] BitWidth::u8),
                        Some("16") => set_option!(arg.to_string(), bits, [take] BitWidth::u16),
                        Some("32") => set_option!(arg.to_string(), bits, [take] BitWidth::u32),
                        Some("64") => set_option!(arg.to_string(), bits, [take] BitWidth::u64),
                        Some(other) => return Err(ArgError::InvalidArg("<width>", other.to_string())),
                        None => return Err(ArgError::MissingArg("<width>")),
                    },
                    // -E, --endian=<endianness>  Specify the indianness to use for binary output [default: {endian}].
                    "-E" | "--endian" => match val {
                        Some("big" | "b" | "B") => set_option!(arg.to_string(), endian, [take] Endianness::Big),
                        Some("little" | "l" | "L") => set_option!(arg.to_string(), endian, [take] Endianness::Little),
                        Some(other) => return Err(ArgError::InvalidArg("<endianness>", other.to_string())),
                        None => return Err(ArgError::MissingArg("<endianness>")),
                    },
                    "-d" | "--delimiter" => match val {
                        Some(value) => set_option!(arg.to_string(), delimiter, [take] value.to_string()),
                        None => return Err(ArgError::MissingArg("<delim>")),
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
            delimiter,
            bits: bits.unwrap_or(default::BIT_WIDTH),
            endian: endian.unwrap_or(default::ENDIANNESS),
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
                "Detected terminal on stdout with binary format selected. \
                Are you sure you want to write raw bytes to the terminal? \
                Set -o/--output to '-' to write to stdout explicitly.",
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


const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

#[rustfmt::skip]
mod target {
    #[cfg(target_endian = "big")] pub const ENDIANNESS: &str = "big";
    #[cfg(target_endian = "little")] pub const ENDIANNESS: &str = "little";

    #[cfg(target_pointer_width = "16")] pub const BIT_WIDTH: &str = "16";
    #[cfg(target_pointer_width = "32")] pub const BIT_WIDTH: &str = "32";
    #[cfg(target_pointer_width = "64")] pub const BIT_WIDTH: &str = "64";
}

#[rustfmt::skip]
mod default {
    use super::{Endianness, BitWidth};

    #[cfg(target_endian = "big")] pub const ENDIANNESS: Endianness = Endianness::Big;
    #[cfg(target_endian = "little")] pub const ENDIANNESS: Endianness = Endianness::Little;

    #[cfg(target_pointer_width = "16")] pub const BIT_WIDTH: BitWidth = BitWidth::u16;
    #[cfg(target_pointer_width = "32")] pub const BIT_WIDTH: BitWidth = BitWidth::u32;
    #[cfg(target_pointer_width = "64")] pub const BIT_WIDTH: BitWidth = BitWidth::u64;
}


pub fn write_usage<W: Write>(mut w: W) -> std::io::Result<()> {
    writeln!(w, "usage: {NAME} <input> [-o <output>] [-f <fmt>] [...OPTIONS] (see -h/--help for other options)")?;
    w.flush()?;
    Ok(())
}

pub fn write_help_short<W: Write>(mut w: W) -> std::io::Result<()> {
    writedoc!(
        w,
        r#"
            {NAME} {VERSION}

            {DESCRIPTION}

            USAGE:
                {NAME} <input> [-f <fmt>]
                {NAME} <input> [-o <output>] [-f <fmt>]
                cat <input> | {NAME} [-o <output>] [-f <fmt>]

            ARGUMENTS:
                <input>                    Filepath to read input from [default: stdin].

            OPTIONS:
                -h                         Display this message and exit.
                --help                     Display a longer-form help message and exit.
                -o, --output=<output>      Filepath to write output to [default: stdout].
                -d, --delimiter=<delim>    String used to separate vertex labels in <input> [default: whitespace].
                -F, --force                Overwrite <output> if it already exists.
                -f, --format=<fmt>         Which to write output as [default: text].
                -b, --bits=<width>         Specify the size of integer used in binary output, in bits [default: {width}].
                -E, --endian=<endianness>  Specify the indianness to use for binary output [default: {endian}].
                -u, --undirected           Treats <input> as containing undirected edges rather than directed edges.
                -m, --multiple             Allows for the presence of multiple edges between the same two vertices.
                -I, --no-preserve-indices  Re-number output vertices' indices based solely on the order in which they
                                           appear in <input>.
                -Z, --no-empty             Exclude the initial empty entry from the CSR format.
                -p, --print-mapping        Print a mapping of how vertex labels were re-numbered to stderr.
        "#,
        endian = target::ENDIANNESS,
        width = target::BIT_WIDTH,
    )?;
    w.flush()?;
    Ok(())
}

pub fn write_help_long<W: Write>(mut w: W) -> std::io::Result<()> {
    writedoc!(
        w,
        r#"
            {NAME} {VERSION}

            {DESCRIPTION}

            The CSR format used is specifically as described in:
            [1] T. Kelly, "Programming Workbench: Compressed Sparse Row Format for Representing Graphics," ;login:,
                vol. 45, no. 4, pp. 76-82, 2020.
                https://www.usenix.org/system/files/login/articles/login_winter20_16_kelly.pdf

            USAGE:
                {NAME} <input> [-f <fmt>]
                {NAME} <input> [-o <output>] [-f <fmt>]
                cat <input> | {NAME} [-o <output>] [-f <fmt>]

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

                -d, --delimiter=<delim>    String used to separate vertex labels in <input> [default: whitespace].

                                           Note that this disables splitting on whitespace: only the provided <delim>
                                           will be used to split lines.

                -F, --force                Overwrite <output> if it already exists.

                -f, --format=<fmt>         Which to write output as [default: text].

                                           <fmt> is either 'text'/'txt'/'t' or 'binary'/'bin'/'b'. See OUTPUT FORMATS
                                           for details on what exactly gets written.

                -b, --bits=<width>         Specify the size of integer used in binary output, in bits [default: {width}].

                                           <width> is one of '8', '16', '32', or '64'. The default, {width}, is the
                                           native pointer-width (size_t) for this platform. Note that the array lengths
                                           output at the start of each array in binary format will always be {width}
                                           bits, regardless of what <width> is set to.

                                           Does nothing when <fmt> is 'text'.

                -E, --endian=<endianness>  Specify the indianness to use for binary output [default: {endian}].

                                           <endianness> is either 'big'/'b'/'B' or 'little'/'l'/'L'. The default,
                                           '{endian}', is the native endianness for this platform.

                                           Does nothing when <fmt> is 'text'.

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
              <input> should consist of a series of lines describing the format of a graph. Each line is comprised of
              one or two vertex "labels". Any string of UTF-8-encoded text is a valid vertex label. Vertex labels are
              separated by whitespace by default, which means they cannot contain whitespace. -d/--delimiter can be used
              to set an explicit delimiter and allow for whitespace in labels.

              Lines with two vertex labels represent an edge (directed by default; see -u/--undirected in OPTIONS).
              Lines with one vertex label represent a disconnected vertex, and will result in a vertex with degree zero
              appearing in the output (unless their label has already been accounted for by another edge, in which case
              they will do nothing).

            OUTPUT ORDERING:
              Unless -I/--no-preserve-indices is set, vertex labels are sorted in ascending order. Any non-integer
              labels are sorted alphabetically (sort of: they are sorted by code-point) after integers. A consequence of
              this is that, if all vertices in the input are integers starting from 1, and there are no gaps in their
              sequence, their values will be exactly preserved in the final output. NOTE: see the -Z/--no-empty flag if
              your vertex labels start at 0 instead of 1.

            OUTPUT FORMATS:
              This section documents file structure of the output formats.

              TEXT FORMAT:
                The first line of <output> will contain the lengths of N and F, separated by a space. Lines two and
                three will contain the values of N and F respectively, also space-separated.

              BINARY FORMAT:
                The binary format starts with a small four-byte header of encoding information:

                - Byte #0:         Either 16 (0x10), 32 (0x20), or 64 (0x40). Gives the size, in bits, of `usize`
                                   (`size_t`) that the file was written with. This size is used to encode the lengths of
                                   N and F. This will always be {width} for files written by this particular executable.
                - Byte #1:         Either 8 (0x08), 16 (0x10), 32 (0x20), or 64 (0x40). Gives the size, in bits, of the
                                   unsigned integer-type used to encode the values of N and F.
                - Bytes #2 and 3:  Endian marker for the entire rest of the file. Will be 0xBB 0xAA for little-endian
                                   and 0xAA 0xBB for big-endian.

                Following bytes 0-3, the lengths of N and F are given back-to-back, in the bit-width described by byte
                #0. The N and F arrays, encoded with bit-width given in byte #1, follow immediately.

              EXAMPLES:
                Given the following input:

                  2 1
                  2 6
                  2 8
                  3 1
                  3 6
                  3 7
                  7 2
                  7 4
                  8 4
                  5
                  9

                Text output looks like this:
                  10 9
                  0 0 0 3 6 6 6 6 8 9
                  1 6 8 1 6 7 2 4 4

                An example of binary output using '--bits 8':
                  +--------+-------------------------+
                  |00000000| 40 08 bb aa 0a 00 00 00 |
                  |00000008| 00 00 00 00 09 00 00 00 |
                  |00000010| 00 00 00 00 00 00 00 03 |
                  |00000018| 06 06 06 06 08 09 01 06 |
                  |00000020| 08 01 06 07 02 04 04    |
                  +--------+-------------------------+

                Examples using '--bits 16' with:
                  '--endianness big':                       '--endianness little':
                  +--------+-------------------------+      +--------+-------------------------+
                  |00000000| 40 10 aa bb 00 00 00 00 |      |00000000| 40 10 bb aa 0a 00 00 00 |
                  |00000008| 00 00 00 0a 00 00 00 00 |      |00000008| 00 00 00 00 09 00 00 00 |
                  |00000010| 00 00 00 09 00 00 00 00 |      |00000010| 00 00 00 00 00 00 00 00 |
                  |00000018| 00 00 00 03 00 06 00 06 |      |00000018| 00 00 03 00 06 00 06 00 |
                  |00000020| 00 06 00 06 00 08 00 09 |      |00000020| 06 00 06 00 08 00 09 00 |
                  |00000028| 00 01 00 06 00 08 00 01 |      |00000028| 01 00 06 00 08 00 01 00 |
                  |00000030| 00 06 00 07 00 02 00 04 |      |00000030| 06 00 07 00 02 00 04 00 |
                  |00000038| 00 04                   |      |00000038| 04 00                   |
                  +--------+-------------------------+      +--------+-------------------------+
        "#,
        endian = target::ENDIANNESS,
        width = target::BIT_WIDTH,
    )?;
    w.flush()?;
    Ok(())
}
