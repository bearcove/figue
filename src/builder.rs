//! Builder API for layered configuration.
//!
//! This module is under active development.
#![allow(dead_code)]
#![allow(deprecated)]
#![allow(private_interfaces)]

use std::marker::PhantomData;

use std::string::String;
use std::vec::Vec;

use camino::Utf8PathBuf;
use facet::Facet;
use facet_reflect::{Partial, ReflectError};

use crate::{
    config_format::{ConfigFormat, ConfigFormatError},
    config_value::ConfigValue,
    help::HelpConfig,
    layers::{
        cli::{CliConfig, CliConfigBuilder},
        env::{EnvConfig, EnvConfigBuilder},
        file::FileConfig,
    },
    provenance::{ConfigResult, FilePathStatus, FileResolution, Provenance},
    schema::{Schema, error::SchemaError},
};

/// Start configuring an args/config parser for a given type.
///
/// At this stage all you need to pass in is your `Args` type, which must implement [`Facet`].
///
/// This call is fallible because it makes sure that the struct that you pass in, is actually a struct
/// and not an enum, and that all its fields and subfields are actually properly annotated with
/// `#[facet(args::positional)]`, `#[facet(args::named)]`, etc. — see [`crate::Attr`]
///
/// This function also already allocates the destination shape, to avoid unsafe code later on.
/// If this allocation fails, then another error is returned.
pub fn builder<T>() -> Result<ConfigBuilder<T>, BuilderError>
where
    T: Facet<'static>,
{
    let schema = Schema::from_shape(T::SHAPE)?;
    let destination = Partial::alloc::<T>().map_err(BuilderError::Alloc)?;
    Ok(ConfigBuilder {
        _phantom: PhantomData,
        partial: destination,
        schema,
        cli_config: None,
        help_config: None,
        env_config: None,
        file_config: None,
    })
}

/// Builder for layered configuration parsing.
pub struct ConfigBuilder<T> {
    _phantom: PhantomData<T>,
    /// The partially allocated destination structure where parsed values land.
    partial: Partial<'static>,
    /// Parsed schema for the target type.
    schema: Schema,
    /// CLI parsing settings, if the user configured that layer.
    cli_config: Option<CliConfig>,
    /// Help text settings, if provided.
    help_config: Option<HelpConfig>,
    /// Environment parsing settings, if provided.
    env_config: Option<EnvConfig>,
    /// File parsing settings for the file layer.
    file_config: Option<FileConfig>,
}

/// Fully built configuration (schema + sources) for the driver.
pub struct Config<T> {
    /// Parsed schema for the target type.
    pub schema: Schema,
    /// CLI parsing settings, if the user configured that layer.
    pub cli_config: Option<CliConfig>,
    /// Help text settings, if provided.
    pub help_config: Option<HelpConfig>,
    /// Environment parsing settings, if provided.
    pub env_config: Option<EnvConfig>,
    /// File parsing settings for the file layer.
    pub file_config: Option<FileConfig>,
    /// Type marker.
    _phantom: PhantomData<T>,
}

impl<T> ConfigBuilder<T> {
    /// Configure CLI argument parsing.
    pub fn cli<F>(mut self, f: F) -> Self
    where
        F: FnOnce(CliConfigBuilder) -> CliConfigBuilder,
    {
        self.cli_config = Some(f(CliConfigBuilder::new()).build());
        self
    }

    /// Configure help text generation.
    pub fn help<F>(mut self, f: F) -> Self
    where
        F: FnOnce(HelpConfigBuilder) -> HelpConfigBuilder,
    {
        self.help_config = Some(f(HelpConfigBuilder::new()).build());
        self
    }

    /// Configure environment variable parsing.
    pub fn env<F>(mut self, f: F) -> Self
    where
        F: FnOnce(EnvConfigBuilder) -> EnvConfigBuilder,
    {
        self.env_config = Some(f(EnvConfigBuilder::new()).build());
        self
    }

    /// Configure config file parsing.
    pub fn file<F>(mut self, f: F) -> Self
    where
        F: FnOnce(FileConfigBuilder) -> FileConfigBuilder,
    {
        self.file_config = Some(f(FileConfigBuilder::new()).build());
        self
    }

    /// Finalize the builder and return a Config for use with the Driver.
    ///
    /// After calling this, create a `Driver` and call `run()`:
    /// ```ignore
    /// let config = builder::<MyArgs>()?.cli(...).env(...).build();
    /// let output = Driver::new(config).run().unwrap();
    /// ```
    pub fn build(self) -> Config<T> {
        Config {
            schema: self.schema,
            cli_config: self.cli_config,
            help_config: self.help_config,
            env_config: self.env_config,
            file_config: self.file_config,
            _phantom: PhantomData,
        }
    }

    /// Build the layered configuration, returning just the merged ConfigValue.
    ///
    /// This parses all configured layers and merges them in priority order:
    /// defaults < file < env < cli
    pub fn build_value(self) -> Result<ConfigValue, BuilderError> {
        panic!("build_value is being moved to the driver API")
    }

    /// Build the layered configuration with full provenance tracking.
    ///
    /// Returns a [`ConfigResult`] containing the merged value, provenance map,
    /// and override records.
    pub fn build_traced(self) -> Result<ConfigResult<ConfigValue>, BuilderError> {
        panic!("build_traced is being moved to the driver API")
    }

    /// Load and parse the config file if specified.
    fn load_config_file(
        file_config: &FileConfig,
    ) -> Result<(Option<ConfigValue>, FileResolution), BuilderError> {
        let mut resolution = FileResolution::new();

        // Check if explicit path was provided
        if let Some(ref explicit) = file_config.explicit_path {
            let exists = std::path::Path::new(explicit.as_str()).exists();
            resolution.add_explicit(explicit.clone(), exists);

            if !exists {
                return Err(BuilderError::FileNotFound {
                    path: explicit.clone(),
                });
            }

            // Mark default paths as not tried
            resolution.mark_defaults_not_tried(&file_config.default_paths);

            // Read and parse the explicit file
            let contents = std::fs::read_to_string(explicit.as_str())
                .map_err(|e| BuilderError::FileRead(explicit.clone(), e.to_string()))?;

            let value = file_config
                .registry
                .parse_file(explicit, &contents)
                .map_err(|e| BuilderError::FileParse(explicit.clone(), e))?;

            return Ok((Some(value), resolution));
        }

        // No explicit path, try defaults in order
        let mut found_path: Option<Utf8PathBuf> = None;

        for path in &file_config.default_paths {
            let exists = std::path::Path::new(path.as_str()).exists();

            if exists && found_path.is_none() {
                // This is the first one that exists - pick it
                resolution.add_default(path.clone(), FilePathStatus::Picked);
                found_path = Some(path.clone());
            } else {
                // Either doesn't exist, or we already found one
                let status = if exists {
                    FilePathStatus::NotTried // Exists but we picked an earlier one
                } else {
                    FilePathStatus::Absent
                };
                resolution.add_default(path.clone(), status);
            }
        }

        let Some(path) = found_path else {
            return Ok((None, resolution));
        };

        // Read and parse the picked file
        let contents = std::fs::read_to_string(path.as_str())
            .map_err(|e| BuilderError::FileRead(path.clone(), e.to_string()))?;

        let value = file_config
            .registry
            .parse_file(&path, &contents)
            .map_err(|e| BuilderError::FileParse(path, e))?;

        Ok((Some(value), resolution))
    }

    /// Parse CLI arguments into a ConfigValue tree.
    ///
    /// Handles:
    /// - Long flags: `--version` (bool true), `--name value`
    /// - Short flags: `-v` (bool true), `-n value`
    /// - Dotted paths: `--config.server.port 8080`
    /// - Boolean flags: `--flag` sets to true
    #[deprecated(note = "this has nothing to do in builder and it's a duplicate parser")]
    fn parse_cli_overrides(cli_config: &CliConfig) -> Result<Option<ConfigValue>, BuilderError> {
        use crate::config_value::Sourced;
        use heck::ToSnakeCase;
        use indexmap::IndexMap;

        let mut root = IndexMap::default();
        let mut i = 0;

        let args = cli_config.args();
        while i < args.len() {
            let arg = &args[i];

            if let Some(flag) = arg.strip_prefix("--") {
                if flag.is_empty() {
                    // "--" separator, skip rest
                    break;
                }

                // Check for dotted path (e.g., --settings.server.port)
                if flag.contains('.') {
                    let parts: Vec<&str> = flag.split('.').collect();
                    // Get the value from the next argument
                    i += 1;
                    if i >= args.len() {
                        return Err(BuilderError::CliParse(format!(
                            "Missing value for --{}",
                            flag
                        )));
                    }
                    let value_str = &args[i];
                    let arg_name = format!("--{}", flag);
                    let value = parse_cli_value(value_str, &arg_name);
                    insert_nested_value(&mut root, &parts, value);
                } else {
                    // Simple flag like --version or --name value
                    let key = flag.to_snake_case();

                    // Check if next arg looks like a value (not another flag)
                    let has_value = i + 1 < args.len() && !args[i + 1].starts_with('-');

                    if has_value {
                        i += 1;
                        let arg_name = format!("--{}", flag);
                        let value = parse_cli_value(&args[i], &arg_name);
                        root.insert(key, value);
                    } else {
                        // Boolean flag, set to true
                        let arg_name = format!("--{}", flag);
                        root.insert(
                            key,
                            ConfigValue::Bool(Sourced {
                                value: true,
                                span: None,
                                provenance: Some(Provenance::cli(arg_name, "true")),
                            }),
                        );
                    }
                }
            } else if let Some(flag) = arg.strip_prefix('-') {
                if flag.is_empty() {
                    // Bare "-" (stdin), treat as positional, skip for now
                    i += 1;
                    continue;
                }

                // Short flags like -v or -n value
                // For now, treat single char as boolean flag
                // TODO: Handle -vvv counting, -abc chaining
                for ch in flag.chars() {
                    let key = ch.to_string();
                    let arg_name = format!("-{}", ch);
                    root.insert(
                        key,
                        ConfigValue::Bool(Sourced {
                            value: true,
                            span: None,
                            provenance: Some(Provenance::cli(arg_name, "true")),
                        }),
                    );
                }
            } else {
                // Positional argument - skip for now
                // TODO: Handle positional args
            }

            i += 1;
        }

        if root.is_empty() {
            Ok(None)
        } else {
            Ok(Some(ConfigValue::Object(Sourced::new(root))))
        }
    }
}

use crate::config_value::{insert_nested_value, parse_cli_value};

// ============================================================================
// Help Configuration
// ============================================================================

/// Builder for help configuration.
#[derive(Debug, Default)]
pub struct HelpConfigBuilder {
    config: HelpConfig,
}

impl HelpConfigBuilder {
    /// Create a new help config builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the program name shown in help.
    pub fn program_name(mut self, name: impl Into<String>) -> Self {
        self.config.program_name = Some(name.into());
        self
    }

    /// Set the program version shown by `--version`.
    ///
    /// Use `env!("CARGO_PKG_VERSION")` to capture your crate's version:
    ///
    /// ```rust,ignore
    /// .help(|h| h.version(env!("CARGO_PKG_VERSION")))
    /// ```
    ///
    /// If not set, `--version` will display "unknown".
    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.config.version = Some(version.into());
        self
    }

    /// Set an additional description shown after the auto-generated one.
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.config.description = Some(description.into());
        self
    }

    /// Set the text wrapping width (0 = no wrapping).
    pub fn width(mut self, width: usize) -> Self {
        self.config.width = width;
        self
    }

    /// Build the help configuration.
    fn build(self) -> HelpConfig {
        self.config
    }
}

// ============================================================================
// File Configuration Builder
// ============================================================================

/// Builder for file configuration.
#[derive(Default)]
pub struct FileConfigBuilder {
    config: FileConfig,
}

impl FileConfigBuilder {
    /// Create a new file config builder.
    pub fn new() -> Self {
        Self {
            config: FileConfig::default(),
        }
    }

    /// Set an explicit config file path.
    pub fn path(mut self, path: impl Into<Utf8PathBuf>) -> Self {
        self.config.explicit_path = Some(path.into());
        self
    }

    /// Set default paths to check for config files.
    ///
    /// These are checked in order; the first existing file is used.
    pub fn default_paths<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Utf8PathBuf>,
    {
        self.config.default_paths = paths.into_iter().map(|p| p.into()).collect();
        self
    }

    /// Register an additional config file format.
    pub fn format<F: ConfigFormat + 'static>(mut self, format: F) -> Self {
        self.config.registry.register(format);
        self
    }

    /// Enable strict mode - error on unknown keys in config file.
    pub fn strict(mut self) -> Self {
        self.config.strict = true;
        self
    }

    /// Set inline content for testing (avoids disk I/O).
    ///
    /// The filename is used for format detection (e.g., "config.toml" or "settings.json").
    pub fn content(mut self, content: impl Into<String>, filename: impl Into<String>) -> Self {
        self.config.inline_content = Some((content.into(), filename.into()));
        self
    }

    /// Build the file configuration.
    fn build(self) -> FileConfig {
        self.config
    }
}

// ============================================================================
// Errors
// ============================================================================

#[derive(Facet)]
#[repr(u8)]
pub enum BuilderError {
    SchemaError(#[facet(opaque)] SchemaError),
    Alloc(#[facet(opaque)] ReflectError),
    FileNotFound {
        path: Utf8PathBuf,
    },
    FileRead(Utf8PathBuf, String),
    FileParse(Utf8PathBuf, ConfigFormatError),
    CliParse(String),
    UnknownKey {
        key: String,
        source: &'static str,
        suggestion: Option<String>,
    },
    MissingRequired(String),
}

impl std::fmt::Display for BuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuilderError::SchemaError(e) => write!(f, "{e}"),
            BuilderError::Alloc(e) => write!(f, "allocation failed: {e}"),
            BuilderError::FileNotFound { path } => {
                write!(f, "config file not found: {path}")
            }
            BuilderError::FileRead(path, msg) => {
                write!(f, "error reading {path}: {msg}")
            }
            BuilderError::FileParse(path, e) => {
                write!(f, "error parsing {path}: {e}")
            }
            BuilderError::CliParse(msg) => write!(f, "{msg}"),
            BuilderError::UnknownKey {
                key,
                source,
                suggestion,
            } => {
                write!(f, "unknown configuration key '{key}' from {source}")?;
                if let Some(suggestion) = suggestion {
                    write!(f, " (did you mean '{suggestion}'?)")?;
                }
                Ok(())
            }
            BuilderError::MissingRequired(field) => {
                write!(f, "missing required configuration: {field}")
            }
        }
    }
}

impl std::fmt::Debug for BuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for BuilderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BuilderError::SchemaError(e) => Some(e),
            BuilderError::Alloc(e) => Some(e),
            BuilderError::FileParse(_, e) => Some(e),
            _ => None,
        }
    }
}

impl From<SchemaError> for BuilderError {
    fn from(e: SchemaError) -> Self {
        BuilderError::SchemaError(e)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate as args;
    use facet::Facet;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[derive(Facet)]
    struct TestConfig {
        #[facet(args::config)]
        config: TestConfigLayer,
    }

    #[derive(Facet)]
    struct TestConfigLayer {
        #[facet(args::named)]
        port: u16,
        #[facet(args::named)]
        host: String,
    }

    #[test]
    #[ignore = "build_traced is being moved to the driver API"]
    fn test_builder_env_only() {
        use crate::layers::env::MockEnv;

        let env = MockEnv::from_pairs([
            ("TEST_BUILDER__PORT", "8080"),
            ("TEST_BUILDER__HOST", "localhost"),
        ]);

        let result = builder::<TestConfig>()
            .unwrap()
            .env(|e| e.prefix("TEST_BUILDER").source(env))
            .build_traced()
            .expect("should build");

        if let ConfigValue::Object(obj) = &result.value {
            assert!(obj.value.contains_key("port"));
            assert!(obj.value.contains_key("host"));

            if let Some(ConfigValue::String(port)) = obj.value.get("port") {
                assert_eq!(port.value, "8080");
            }
        } else {
            panic!("expected object");
        }

        // Check provenance was collected
        assert!(result.provenance.contains_key("port"));
        assert!(result.provenance.contains_key("host"));
    }

    #[test]
    #[ignore = "build_traced is being moved to the driver API"]
    fn test_builder_file_only() {
        // Create a temp config file
        let mut file = NamedTempFile::with_suffix(".json").unwrap();
        writeln!(file, r#"{{"port": 9000, "host": "filehost"}}"#).unwrap();
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let result = builder::<TestConfig>()
            .unwrap()
            .file(|f| f.path(path))
            .build_traced()
            .expect("should build");

        if let ConfigValue::Object(obj) = &result.value {
            if let Some(ConfigValue::Integer(port)) = obj.value.get("port") {
                assert_eq!(port.value, 9000);
            }
            if let Some(ConfigValue::String(host)) = obj.value.get("host") {
                assert_eq!(host.value, "filehost");
            }
        } else {
            panic!("expected object");
        }
    }

    #[test]
    fn test_cli_config_builder() {
        let config = CliConfigBuilder::new()
            .args(["--port", "8080"])
            .strict()
            .build();

        assert_eq!(config.args(), &["--port", "8080"]);
        assert!(config.strict());
    }

    #[test]
    fn test_env_config_builder() {
        let config = EnvConfigBuilder::new().prefix("MYAPP").strict().build();

        assert_eq!(config.prefix, "MYAPP");
        assert!(config.strict);
    }

    #[test]
    fn test_file_config_builder() {
        let config = FileConfigBuilder::new()
            .path("config.json")
            .default_paths(["./config.json", "~/.config/app.json"])
            .strict()
            .build();

        assert_eq!(config.explicit_path, Some(Utf8PathBuf::from("config.json")));
        assert_eq!(config.default_paths.len(), 2);
        assert!(config.strict);
    }
}
