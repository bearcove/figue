//! Driver API for orchestrating layered configuration parsing, validation, and diagnostics.
//!
//! # Phases
//! 1. **Parse layers**: CLI, env, file (defaults filled during deserialization)
//! 2. **Check special fields**: If help/version/completions was requested, short-circuit
//! 3. **Merge** layers by priority (CLI > env > file > defaults)
//! 4. **Deserialize** merged ConfigValue into the target Facet type
//!
//! # TODO
//! - [x] Wire override tracking from merge result into DriverReport
//! - [x] Define DriverError enum (Failed, Help, Completions, Version)
//! - [x] Implement expect_value() on DriverResult
//! - [x] Add figue::help, figue::completions, figue::version attribute detection
//! - [x] Handle special fields in Driver::run() before deserialization
//! - [ ] Collect unused keys from layer parsers into LayerOutput
//! - [ ] Add facet-validate pass after deserialization
//! - [ ] Improve render_pretty() with Ariadne integration
//! - [ ] Migrate build_traced tests to driver API
#![allow(dead_code)]
#![allow(clippy::result_large_err)]

use std::marker::PhantomData;
use std::string::String;
use std::vec::Vec;

use crate::builder::Config;
use crate::completions::{Shell, generate_completions_for_shape};
use crate::config_value::ConfigValue;
use crate::config_value_parser::from_config_value;
use crate::help::generate_help_for_shape;
use crate::layers::{cli::parse_cli, env::parse_env, file::parse_file};
use crate::merge::merge_layers;
use crate::path::Path;
use crate::provenance::{FileResolution, Override, Provenance};
use crate::span::Span;
use facet_core::Facet;

/// Diagnostics for a single layer.
#[derive(Debug, Default)]
pub struct LayerOutput {
    /// Parsed value for this layer (if any).
    pub value: Option<ConfigValue>,
    /// Keys provided by this layer but unused by the schema.
    pub unused_keys: Vec<UnusedKey>,
    /// Layer-specific diagnostics collected while parsing.
    pub diagnostics: Vec<Diagnostic>,
}

/// A key that was unused by the schema, with provenance.
#[derive(Debug)]
pub struct UnusedKey {
    /// The unused key path.
    pub key: Path,
    /// Provenance for where it came from (CLI/env/file/default).
    pub provenance: Provenance,
}

/// Layered config values from CLI/env/file/defaults, with diagnostics.
#[derive(Debug, Default)]
pub struct ConfigLayers {
    /// Default layer (lowest priority).
    pub defaults: LayerOutput,
    /// File layer.
    pub file: LayerOutput,
    /// Environment layer.
    pub env: LayerOutput,
    /// CLI layer (highest priority).
    pub cli: LayerOutput,
}

/// Primary driver type that orchestrates parsing and validation.
///
/// This is generic over `T`, with a non-generic core for future optimization.
pub struct Driver<T> {
    config: Config<T>,
    core: DriverCore,
    _phantom: PhantomData<T>,
}

/// Non-generic driver core (placeholder for future monomorphization reduction).
#[derive(Debug, Default)]
pub struct DriverCore;

impl DriverCore {
    fn new() -> Self {
        Self
    }
}

impl<T: Facet<'static>> Driver<T> {
    /// Create a driver from a fully built config.
    pub fn new(config: Config<T>) -> Self {
        Self {
            config,
            core: DriverCore::new(),
            _phantom: PhantomData,
        }
    }

    /// Execute the driver and return a fully-typed value plus a report.
    pub fn run(self) -> Result<DriverOutput<T>, DriverError> {
        let _ = self.core;

        let mut layers = ConfigLayers::default();
        let mut all_diagnostics = Vec::new();
        let mut file_resolution = None;

        // Phase 1: Parse each layer
        // Priority order (lowest to highest): defaults < file < env < cli

        // 1a. Defaults layer (TODO: extract defaults from schema)
        // For now, defaults is empty - this will be filled in when we implement
        // default value extraction from the schema

        // 1b. File layer
        if let Some(ref file_config) = self.config.file_config {
            let result = parse_file(&self.config.schema, file_config);
            layers.file = result.output;
            file_resolution = Some(result.resolution);
            all_diagnostics.extend(layers.file.diagnostics.iter().cloned());
        }

        // 1c. Environment layer
        if let Some(ref env_config) = self.config.env_config {
            layers.env = parse_env(
                &self.config.schema,
                env_config,
                self.config.env_source.as_ref(),
            );
            all_diagnostics.extend(layers.env.diagnostics.iter().cloned());
        }

        // 1d. CLI layer
        if let Some(ref cli_config) = self.config.cli_config {
            layers.cli = parse_cli(&self.config.schema, cli_config);
            all_diagnostics.extend(layers.cli.diagnostics.iter().cloned());
        }

        // Phase 1.5: Check special fields (help/version/completions)
        // These short-circuit before merge/deserialization
        if let Some(cli_value) = &layers.cli.value {
            let special = self.config.schema.special();

            // Check for --help
            if let Some(ref help_path) = special.help {
                if let Some(ConfigValue::Bool(b)) = cli_value.get_by_path(help_path) {
                    if b.value {
                        let help_config = self
                            .config
                            .help_config
                            .as_ref()
                            .cloned()
                            .unwrap_or_default();
                        let text = generate_help_for_shape(T::SHAPE, &help_config);
                        return Err(DriverError::Help { text });
                    }
                }
            }

            // Check for --version
            if let Some(ref version_path) = special.version {
                if let Some(ConfigValue::Bool(b)) = cli_value.get_by_path(version_path) {
                    if b.value {
                        let version = self
                            .config
                            .help_config
                            .as_ref()
                            .and_then(|h| h.version.clone())
                            .unwrap_or_else(|| "unknown".to_string());
                        let program_name = self
                            .config
                            .help_config
                            .as_ref()
                            .and_then(|h| h.program_name.clone())
                            .or_else(|| std::env::args().next())
                            .unwrap_or_else(|| "program".to_string());
                        let text = format!("{} {}", program_name, version);
                        return Err(DriverError::Version { text });
                    }
                }
            }

            // Check for --completions <shell>
            if let Some(ref completions_path) = special.completions {
                if let Some(value) = cli_value.get_by_path(completions_path) {
                    // The value should be a string representing the shell name
                    if let Some(shell) = extract_shell_from_value(value) {
                        let program_name = self
                            .config
                            .help_config
                            .as_ref()
                            .and_then(|h| h.program_name.clone())
                            .or_else(|| std::env::args().next())
                            .unwrap_or_else(|| "program".to_string());
                        let script = generate_completions_for_shape(T::SHAPE, shell, &program_name);
                        return Err(DriverError::Completions { script });
                    }
                }
            }
        }

        // Check for errors before proceeding
        let has_errors = all_diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error);
        if has_errors {
            return Err(DriverError::Failed {
                report: DriverReport {
                    diagnostics: all_diagnostics,
                    layers,
                    file_resolution,
                    overrides: Vec::new(),
                },
            });
        }

        // Phase 2: Merge layers by priority
        let values_to_merge: Vec<ConfigValue> = [
            layers.defaults.value.clone(),
            layers.file.value.clone(),
            layers.env.value.clone(),
            layers.cli.value.clone(),
        ]
        .into_iter()
        .flatten()
        .collect();

        let merged = merge_layers(values_to_merge);
        let overrides = merged.overrides;

        // Phase 3: Deserialize into T
        let value: T = match from_config_value(&merged.value) {
            Ok(v) => v,
            Err(e) => {
                return Err(DriverError::Failed {
                    report: DriverReport {
                        diagnostics: vec![Diagnostic {
                            message: format!("Deserialization failed: {}", e),
                            path: None,
                            span: None,
                            severity: Severity::Error,
                        }],
                        layers,
                        file_resolution,
                        overrides,
                    },
                });
            }
        };

        Ok(DriverOutput {
            value,
            report: DriverReport {
                diagnostics: all_diagnostics,
                layers,
                file_resolution,
                overrides,
            },
        })
    }
}

/// Result type for driver operations.
pub type DriverResult<T> = Result<DriverOutput<T>, DriverError>;

/// Successful driver output: a typed value plus an execution report.
#[derive(Debug)]
pub struct DriverOutput<T> {
    /// The fully-typed value produced by deserialization.
    pub value: T,
    /// Diagnostics and metadata produced by the driver.
    pub report: DriverReport,
}

impl<T> DriverOutput<T> {
    /// Get the value, printing any warnings to stderr.
    pub fn get(self) -> T {
        self.print_warnings();
        self.value
    }

    /// Get the value silently (no warning output).
    pub fn get_silent(self) -> T {
        self.value
    }

    /// Get value and report separately.
    pub fn into_parts(self) -> (T, DriverReport) {
        (self.value, self.report)
    }

    /// Print any warnings to stderr.
    pub fn print_warnings(&self) {
        for diagnostic in &self.report.diagnostics {
            if diagnostic.severity == Severity::Warning {
                eprintln!("{}: {}", diagnostic.severity.as_str(), diagnostic.message);
            }
        }
    }
}

/// Extension trait for `DriverResult` to handle common exit patterns.
pub trait DriverResultExt<T> {
    /// Get the value, or print output and exit.
    ///
    /// - On success: prints warnings to stderr, returns value
    /// - On help/completions/version: prints to stdout, exits 0
    /// - On error: prints diagnostics to stderr, exits 1
    fn expect_value(self) -> T;
}

impl<T> DriverResultExt<T> for DriverResult<T> {
    fn expect_value(self) -> T {
        match self {
            Ok(output) => output.get(),
            Err(DriverError::Help { text }) => {
                println!("{}", text);
                std::process::exit(0);
            }
            Err(DriverError::Completions { script }) => {
                println!("{}", script);
                std::process::exit(0);
            }
            Err(DriverError::Version { text }) => {
                println!("{}", text);
                std::process::exit(0);
            }
            Err(DriverError::Failed { report }) => {
                eprintln!("{}", report.render_pretty());
                std::process::exit(1);
            }
        }
    }
}

/// Full report of the driver execution.
///
/// The report should be pretty-renderable and capture all diagnostics,
/// plus optional supporting metadata (merge overrides, spans, etc).
#[derive(Debug, Default)]
pub struct DriverReport {
    /// Diagnostics emitted by the driver.
    pub diagnostics: Vec<Diagnostic>,
    /// Per-layer outputs, including unused keys and layer diagnostics.
    pub layers: ConfigLayers,
    /// File resolution metadata (paths tried, picked, etc).
    pub file_resolution: Option<FileResolution>,
    /// Records of values that were overridden during merge.
    pub overrides: Vec<Override>,
}

impl DriverReport {
    /// Render the report for user-facing output.
    pub fn render_pretty(&self) -> String {
        let mut out = String::new();
        for diagnostic in &self.diagnostics {
            let _ = core::fmt::write(
                &mut out,
                format_args!("{}: {}\n", diagnostic.severity.as_str(), diagnostic.message),
            );
        }
        out
    }
}

/// A diagnostic message produced by the driver.
///
/// This is intentionally minimal and will grow as we integrate facet-pretty
/// spans and Ariadne rendering.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Human-readable message.
    pub message: String,
    /// Optional path within the schema or config.
    pub path: Option<Path>,
    /// Optional byte span within a formatted shape or source file.
    pub span: Option<Span>,
    /// Diagnostic severity.
    pub severity: Severity,
}

/// Severity for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Error that prevents producing a value.
    Error,
    /// Warning that allows a value to be produced.
    Warning,
    /// Informational note.
    Note,
}

impl Severity {
    fn as_str(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    }
}

/// Extract a Shell value from a ConfigValue.
///
/// The completions field is `Option<Shell>`, so after CLI parsing we get
/// either nothing (None) or a string like "bash", "zsh", "fish".
fn extract_shell_from_value(value: &ConfigValue) -> Option<Shell> {
    match value {
        ConfigValue::String(s) => match s.value.to_lowercase().as_str() {
            "bash" => Some(Shell::Bash),
            "zsh" => Some(Shell::Zsh),
            "fish" => Some(Shell::Fish),
            _ => None,
        },
        // Could also be an enum variant name directly
        ConfigValue::Enum(e) => match e.value.variant.to_lowercase().as_str() {
            "bash" => Some(Shell::Bash),
            "zsh" => Some(Shell::Zsh),
            "fish" => Some(Shell::Fish),
            _ => None,
        },
        _ => None,
    }
}

/// Error returned by the driver.
///
/// Not all variants are "errors" in the traditional sense - Help, Completions,
/// and Version are successful operations that just don't produce a config value.
pub enum DriverError {
    /// Parsing or validation failed - exit code 1
    Failed {
        /// Report containing all diagnostics
        report: DriverReport,
    },

    /// Help was requested (via `#[facet(figue::help)]` field) - exit code 0
    Help {
        /// Formatted help text
        text: String,
    },

    /// Shell completions were requested (via `#[facet(figue::completions)]` field) - exit code 0
    Completions {
        /// Generated completion script
        script: String,
    },

    /// Version was requested (via `#[facet(figue::version)]` field) - exit code 0
    Version {
        /// Version string
        text: String,
    },
}

impl DriverError {
    /// Returns the appropriate exit code for this error.
    pub fn exit_code(&self) -> i32 {
        match self {
            DriverError::Failed { .. } => 1,
            DriverError::Help { .. } => 0,
            DriverError::Completions { .. } => 0,
            DriverError::Version { .. } => 0,
        }
    }

    /// Returns true if this is a "success" error (help, completions, version).
    pub fn is_success(&self) -> bool {
        self.exit_code() == 0
    }
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DriverError::Failed { report } => write!(f, "{}", report.render_pretty()),
            DriverError::Help { text } => write!(f, "{}", text),
            DriverError::Completions { script } => write!(f, "{}", script),
            DriverError::Version { text } => write!(f, "{}", text),
        }
    }
}

impl std::fmt::Debug for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for DriverError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as figue;
    use crate::FigueBuiltins;
    use crate::builder::builder;
    use facet::Facet;

    /// Args struct with FigueBuiltins flattened in
    #[derive(Facet, Debug)]
    struct ArgsWithBuiltins {
        /// Input file
        #[facet(figue::positional)]
        input: Option<String>,

        /// Standard CLI options
        #[facet(flatten)]
        builtins: FigueBuiltins,
    }

    #[test]
    fn test_driver_help_flag() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["--help"]))
            .help(|h| h.program_name("test-app").version("1.0.0"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Help { text }) => {
                assert!(
                    text.contains("test-app"),
                    "help should contain program name"
                );
                assert!(text.contains("--help"), "help should mention --help flag");
            }
            other => panic!("expected DriverError::Help, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_help_short_flag() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["-h"]))
            .help(|h| h.program_name("test-app"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        assert!(
            matches!(result, Err(DriverError::Help { .. })),
            "expected DriverError::Help"
        );
    }

    #[test]
    fn test_driver_version_flag() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["--version"]))
            .help(|h| h.program_name("test-app").version("2.0.0"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Version { text }) => {
                assert!(
                    text.contains("test-app"),
                    "version should contain program name"
                );
                assert!(
                    text.contains("2.0.0"),
                    "version should contain version number"
                );
            }
            other => panic!("expected DriverError::Version, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_version_short_flag() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["-V"]))
            .help(|h| h.program_name("test-app").version("3.0.0"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Version { text }) => {
                assert!(
                    text.contains("3.0.0"),
                    "version should contain version number"
                );
            }
            other => panic!("expected DriverError::Version, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_completions_bash() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["--completions", "bash"]))
            .help(|h| h.program_name("test-app"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Completions { script }) => {
                assert!(
                    script.contains("_test-app"),
                    "bash completions should contain function name"
                );
                assert!(
                    script.contains("complete"),
                    "bash completions should contain 'complete'"
                );
            }
            other => panic!("expected DriverError::Completions, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_completions_zsh() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["--completions", "zsh"]))
            .help(|h| h.program_name("myapp"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Completions { script }) => {
                assert!(
                    script.contains("#compdef myapp"),
                    "zsh completions should contain #compdef"
                );
            }
            other => panic!("expected DriverError::Completions, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_completions_fish() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["--completions", "fish"]))
            .help(|h| h.program_name("myapp"))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Err(DriverError::Completions { script }) => {
                assert!(
                    script.contains("complete -c myapp"),
                    "fish completions should contain 'complete -c myapp'"
                );
            }
            other => panic!("expected DriverError::Completions, got {:?}", other),
        }
    }

    #[test]
    fn test_driver_normal_execution() {
        let config = builder::<ArgsWithBuiltins>()
            .unwrap()
            .cli(|cli| cli.args(["myfile.txt"]))
            .build();

        let driver = Driver::new(config);
        let result = driver.run();

        match result {
            Ok(output) => {
                assert_eq!(output.value.input, Some("myfile.txt".to_string()));
                assert!(!output.value.builtins.help);
                assert!(!output.value.builtins.version);
                assert!(output.value.builtins.completions.is_none());
            }
            Err(e) => panic!("expected success, got error: {:?}", e),
        }
    }

    #[test]
    fn test_driver_error_exit_codes() {
        let help_err = DriverError::Help {
            text: "help".to_string(),
        };
        let version_err = DriverError::Version {
            text: "1.0".to_string(),
        };
        let completions_err = DriverError::Completions {
            script: "script".to_string(),
        };
        let failed_err = DriverError::Failed {
            report: DriverReport::default(),
        };

        assert_eq!(help_err.exit_code(), 0);
        assert_eq!(version_err.exit_code(), 0);
        assert_eq!(completions_err.exit_code(), 0);
        assert_eq!(failed_err.exit_code(), 1);

        assert!(help_err.is_success());
        assert!(version_err.is_success());
        assert!(completions_err.is_success());
        assert!(!failed_err.is_success());
    }
}
