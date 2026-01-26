#![warn(missing_docs)]
#![deny(unsafe_code)]
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]
// Allow deprecated during transition to new driver-based API
#![allow(deprecated)]
#![doc = include_str!("../README.md")]

extern crate self as figue;

#[macro_use]
mod macros;

// pub(crate) mod arg;
pub(crate) mod builder;
pub(crate) mod completions;
pub(crate) mod config_format;
pub(crate) mod config_value;
pub(crate) mod config_value_parser;
pub(crate) mod diagnostics;
pub mod driver;
pub(crate) mod dump;
pub(crate) mod error;
pub(crate) mod help;
pub(crate) mod layers;
pub(crate) mod merge;
pub(crate) mod missing;
pub(crate) mod path;
pub(crate) mod provenance;
pub(crate) mod reflection;
pub(crate) mod schema;
pub(crate) mod span;
pub(crate) mod span_registry;

use facet_core::Facet;

// ==========================================
// PUBLIC INTERFACE
// ==========================================

pub use builder::builder;
pub use completions::{Shell, generate_completions, generate_completions_for_shape};
pub use error::{ArgsErrorKind, ArgsErrorWithInput};
pub use help::{HelpConfig, generate_help, generate_help_for_shape};

/// Parse command-line arguments from `std::env::args()`.
///
/// This is a convenience function for CLI-only parsing (no env vars, no config files).
/// For layered configuration, use `builder()` instead.
pub fn from_std_args<T: Facet<'static>>() -> Result<T, driver::DriverError> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    from_slice(&args_ref)
}

/// Parse command-line arguments from a slice.
///
/// This is a convenience function for CLI-only parsing (no env vars, no config files).
/// For layered configuration, use `builder()` instead.
pub fn from_slice<T: Facet<'static>>(args: &[&str]) -> Result<T, driver::DriverError> {
    use crate::driver::{Driver, DriverError};

    let config = builder::<T>()
        .map_err(|e| DriverError::Builder { error: e })?
        .cli(|cli| cli.args(args.iter().map(|s| s.to_string())))
        .build();

    let driver = Driver::new(config);
    driver.run().map(|output| output.value)
}

/// Standard CLI builtins that can be flattened into your Args struct.
///
/// This provides the standard `--help`, `--version`, and `--completions` flags
/// that most CLI applications need. Flatten it into your Args struct:
///
/// ```rust,ignore
/// use figue::{self as args, FigueBuiltins};
/// use facet::Facet;
///
/// #[derive(Facet)]
/// struct Args {
///     /// Your actual arguments
///     #[facet(args::positional)]
///     input: String,
///
///     /// Standard CLI options
///     #[facet(flatten)]
///     builtins: FigueBuiltins,
/// }
/// ```
///
/// The driver automatically handles these fields:
/// - `--help` / `-h`: Shows help and exits with code 0
/// - `--version` / `-V`: Shows version and exits with code 0
/// - `--completions <SHELL>`: Generates shell completions and exits with code 0
#[derive(facet::Facet, Default, Debug)]
pub struct FigueBuiltins {
    /// Show help message and exit.
    #[facet(crate::named, crate::short = 'h', crate::help, default)]
    pub help: bool,

    /// Show version and exit.
    #[facet(crate::named, crate::short = 'V', crate::version, default)]
    pub version: bool,

    /// Generate shell completions.
    #[facet(crate::named, crate::completions, default)]
    pub completions: Option<Shell>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::help::generate_help;
    use crate::schema::Schema;

    #[derive(facet::Facet)]
    struct ArgsWithBuiltins {
        /// Input file
        #[facet(crate::positional)]
        input: String,

        /// Standard options
        #[facet(flatten)]
        builtins: FigueBuiltins,
    }

    #[test]
    fn test_figue_builtins_flatten_in_schema() {
        let schema = Schema::from_shape(ArgsWithBuiltins::SHAPE);
        assert!(schema.is_ok(), "Schema should build: {:?}", schema.err());
    }

    #[test]
    fn test_figue_builtins_in_help() {
        let help = generate_help::<ArgsWithBuiltins>(&crate::help::HelpConfig::default());
        assert!(help.contains("--help"), "help should contain --help");
        assert!(help.contains("-h"), "help should contain -h");
        assert!(help.contains("--version"), "help should contain --version");
        assert!(help.contains("-V"), "help should contain -V");
        assert!(
            help.contains("--completions"),
            "help should contain --completions"
        );
    }

    #[test]
    fn test_figue_builtins_special_fields_detected() {
        let schema = Schema::from_shape(ArgsWithBuiltins::SHAPE).unwrap();
        let special = schema.special();

        // With flatten, fields appear at top level - path is just ["help"]
        assert!(special.help.is_some(), "help should be detected");
        assert_eq!(special.help.as_ref().unwrap(), &vec!["help".to_string()]);

        // Version at top level
        assert!(special.version.is_some(), "version should be detected");
        assert_eq!(
            special.version.as_ref().unwrap(),
            &vec!["version".to_string()]
        );

        // Completions at top level
        assert!(
            special.completions.is_some(),
            "completions should be detected"
        );
        assert_eq!(
            special.completions.as_ref().unwrap(),
            &vec!["completions".to_string()]
        );
    }

    // ========================================================================
    // Tests: Special fields with custom names and nesting
    // ========================================================================

    /// Special fields can be renamed - detection works via attribute, not field name
    #[derive(facet::Facet)]
    struct ArgsWithRenamedHelp {
        /// Print documentation and exit
        #[facet(crate::named, crate::help, rename = "print-docs")]
        show_help: bool,

        /// Show program version
        #[facet(crate::named, crate::version, rename = "show-version")]
        show_ver: bool,
    }

    #[test]
    fn test_special_fields_renamed() {
        let schema = Schema::from_shape(ArgsWithRenamedHelp::SHAPE).unwrap();
        let special = schema.special();

        // Detection is by ATTRIBUTE (crate::help), not field name.
        // The path uses the EFFECTIVE name (after rename).
        assert!(
            special.help.is_some(),
            "help should be detected via attribute"
        );
        assert_eq!(
            special.help.as_ref().unwrap(),
            &vec!["print-docs".to_string()],
            "path should use effective name"
        );

        assert!(
            special.version.is_some(),
            "version should be detected via attribute"
        );
        assert_eq!(
            special.version.as_ref().unwrap(),
            &vec!["show-version".to_string()],
            "path should use effective name"
        );
    }

    /// Deeply nested special fields (flatten inside flatten)
    #[derive(facet::Facet)]
    struct DeepInner {
        #[facet(crate::named, crate::help, default)]
        help: bool,
    }

    #[derive(facet::Facet)]
    struct DeepMiddle {
        #[facet(flatten)]
        inner: DeepInner,
    }

    #[derive(facet::Facet)]
    struct ArgsWithDeepFlatten {
        #[facet(crate::positional)]
        input: String,

        #[facet(flatten)]
        middle: DeepMiddle,
    }

    #[test]
    fn test_special_fields_deeply_flattened() {
        let schema = Schema::from_shape(ArgsWithDeepFlatten::SHAPE).unwrap();
        let special = schema.special();

        // With flatten, all fields bubble up to top level - path is just ["help"]
        assert!(
            special.help.is_some(),
            "help should be detected in deeply flattened struct"
        );
        assert_eq!(
            special.help.as_ref().unwrap(),
            &vec!["help".to_string()],
            "flattened fields appear at top level"
        );
    }
}

// Args extension attributes for use with #[facet(args::attr)] syntax.
//
// After importing `use figue as args;`, users can write:
//   #[facet(args::positional)]
//   #[facet(args::short = 'v')]
//   #[facet(args::named)]

// Generate args attribute grammar using the grammar DSL.
// This generates:
// - `Attr` enum with all args attribute variants
// - `__attr!` macro that dispatches to attribute handlers and returns ExtensionAttr
// - `__parse_attr!` macro for parsing (internal use)
facet::define_attr_grammar! {
    ns "args";
    crate_path ::figue;

    /// Args attribute types for field configuration.
    pub enum Attr {
        /// Marks a field as a positional argument.
        ///
        /// Usage: `#[facet(args::positional)]`
        Positional,
        /// Marks a field as a named argument.
        ///
        /// Usage: `#[facet(args::named)]`
        Named,
        /// Specifies a short flag character for the field.
        ///
        /// Usage: `#[facet(args::short = 'v')]` or just `#[facet(args::short)]`
        Short(Option<char>),
        /// Marks a field as a subcommand.
        ///
        /// The field type must be an enum where each variant represents a subcommand.
        /// Variant names are converted to kebab-case for matching.
        ///
        /// Usage: `#[facet(args::subcommand)]`
        Subcommand,
        /// Marks a field as a counted flag.
        ///
        /// Each occurrence of the flag increments the count. Works with both short
        /// flags (`-vvv` or `-v -v -v`) and long flags (`--verbose --verbose`).
        /// The field type must be an integer type (u8, u16, u32, u64, usize, i8, i16, i32, i64, isize).
        /// Uses saturating arithmetic to avoid overflow.
        ///
        /// Usage: `#[facet(args::named, args::short = 'v', args::counted)]`
        Counted,
        /// Marks a field as a layered configuration field.
        ///
        /// The field will be populated from merged configuration sources (CLI overrides,
        /// environment variables, config files) in priority order: CLI > env > file > default.
        ///
        /// This automatically generates:
        /// - `--{field_name} <PATH>` flag to specify config file path
        /// - `--{field_name}.foo.bar <VALUE>` style CLI overrides
        /// - Environment variable parsing
        /// - Config file loading with multiple format support
        ///
        /// Usage: `#[facet(args::config)]`
        Config,
        /// Specifies the environment variable prefix for a config field.
        ///
        /// Must be used together with `#[facet(args::config)]`.
        ///
        /// Usage: `#[facet(args::env_prefix = "MYAPP")]`
        ///
        /// Example: `env_prefix = "MYAPP"` results in `MYAPP__FIELD__NAME` env vars.
        EnvPrefix(Option<&'static str>),
        /// Marks a field as the help flag.
        ///
        /// When this flag is set, the driver shows help and exits with code 0.
        /// The field should be a `bool`.
        ///
        /// Usage: `#[facet(figue::help)]`
        Help,
        /// Marks a field as the version flag.
        ///
        /// When this flag is set, the driver shows version and exits with code 0.
        /// The field should be a `bool`.
        ///
        /// Usage: `#[facet(figue::version)]`
        Version,
        /// Marks a field as the completions flag.
        ///
        /// When this flag is set, the driver generates shell completions and exits with code 0.
        /// The field should be `Option<Shell>`.
        ///
        /// Usage: `#[facet(figue::completions)]`
        Completions,
    }
}
