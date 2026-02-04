use figue::{self as args, FigueBuiltins};
use facet::Facet;

#[derive(Facet, Debug)]
struct Args {
    /// Your actual arguments
    #[facet(args::positional)]
    filename: String,

    /// Provides standard CLI options like --help, --version and --completions
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

fn main() {
    // The builtins are automatically available. They will print their help, version or completion
    // script and then exit.
    let args: Args = figue::from_std_args().unwrap();
    println!("A regular argument was provided: {}", args.filename);
}

