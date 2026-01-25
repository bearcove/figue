//! Schema-driven config file parser that outputs ConfigValue with provenance.
//!
//! This module is under active development and not yet wired into the main API.
#![allow(dead_code)]
//!
//! This parser:
//! - Uses the pre-built Schema to validate config structure
//! - Outputs LayerOutput (ConfigValue + diagnostics), not a Partial
//! - Supports multiple file formats via FormatRegistry
//! - Reports unused keys (keys in file that don't match schema)
//! - Tracks provenance for all values
//!
//! # Example
//!
//! ```rust,ignore
//! use figue::layers::file::{parse_file, FileConfig, FormatRegistry};
//! use figue::schema::Schema;
//!
//! let schema = Schema::from_shape(MyConfig::SHAPE)?;
//! let config = FileConfig::new()
//!     .path("config.json")
//!     .registry(FormatRegistry::with_defaults());
//!
//! let output = parse_file(&schema, &config)?;
//! ```

use std::boxed::Box;
use std::string::{String, ToString};
use std::sync::Arc;
use std::vec::Vec;

use camino::{Utf8Path, Utf8PathBuf};

use crate::config_format::{ConfigFormat, ConfigFormatError, JsonFormat};
use crate::config_value::ConfigValue;
use crate::driver::{Diagnostic, LayerOutput, Severity, UnusedKey};
use crate::provenance::{ConfigFile, FilePathStatus, FileResolution, Provenance};
use crate::schema::{ConfigStructSchema, ConfigValueSchema, Schema};

// ============================================================================
// Format Registry
// ============================================================================

/// A registry of config file formats.
///
/// This allows registering multiple formats and selecting the appropriate
/// one based on file extension.
#[derive(Default)]
pub struct FormatRegistry {
    formats: Vec<Box<dyn ConfigFormat>>,
}

impl FormatRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            formats: Vec::new(),
        }
    }

    /// Create a registry with the default JSON format.
    pub fn with_defaults() -> Self {
        let mut registry = Self::new();
        registry.register(JsonFormat);
        registry
    }

    /// Register a new format.
    pub fn register<F: ConfigFormat + 'static>(&mut self, format: F) {
        self.formats.push(Box::new(format));
    }

    /// Find a format that handles the given file extension.
    ///
    /// The extension should not include the leading dot.
    pub fn find_by_extension(&self, extension: &str) -> Option<&dyn ConfigFormat> {
        let ext_lower = extension.to_lowercase();
        self.formats
            .iter()
            .find(|f| {
                f.extensions()
                    .iter()
                    .any(|e| e.eq_ignore_ascii_case(&ext_lower))
            })
            .map(|f| f.as_ref())
    }

    /// Parse a config file, automatically selecting the format based on extension.
    pub fn parse(&self, contents: &str, extension: &str) -> Result<ConfigValue, ConfigFormatError> {
        let format = self.find_by_extension(extension).ok_or_else(|| {
            ConfigFormatError::new(format!("unsupported file extension: .{extension}"))
        })?;
        format.parse(contents)
    }

    /// Parse a config file and set provenance on all values.
    ///
    /// This is the preferred method for loading config files, as it ensures
    /// all values have proper provenance tracking for error messages.
    pub fn parse_file(
        &self,
        path: &Utf8Path,
        contents: &str,
    ) -> Result<ConfigValue, ConfigFormatError> {
        let extension = path.extension().unwrap_or("");
        let mut value = self.parse(contents, extension)?;

        // Create config file and set provenance recursively
        let file = Arc::new(ConfigFile::new(path, contents));
        value.set_file_provenance_recursive(&file, "");

        Ok(value)
    }

    /// Get all registered extensions.
    pub fn extensions(&self) -> Vec<&str> {
        self.formats
            .iter()
            .flat_map(|f| f.extensions().iter().copied())
            .collect()
    }
}

// ============================================================================
// File Configuration
// ============================================================================

/// Configuration for config file parsing.
pub struct FileConfig {
    /// Explicit path provided via CLI (e.g., --config path.json).
    pub explicit_path: Option<Utf8PathBuf>,

    /// Default paths to check if no explicit path is provided.
    pub default_paths: Vec<Utf8PathBuf>,

    /// Format registry for parsing different file types.
    pub registry: FormatRegistry,

    /// Whether to error on unknown keys in the config file.
    pub strict: bool,
}

impl Default for FileConfig {
    fn default() -> Self {
        Self {
            explicit_path: None,
            default_paths: Vec::new(),
            registry: FormatRegistry::with_defaults(),
            strict: false,
        }
    }
}

impl FileConfig {
    /// Create a new file config with defaults.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set an explicit config file path.
    pub fn path(mut self, path: impl Into<Utf8PathBuf>) -> Self {
        self.explicit_path = Some(path.into());
        self
    }

    /// Set default paths to check for config files.
    pub fn default_paths<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Utf8PathBuf>,
    {
        self.default_paths = paths.into_iter().map(|p| p.into()).collect();
        self
    }

    /// Set the format registry.
    pub fn registry(mut self, registry: FormatRegistry) -> Self {
        self.registry = registry;
        self
    }

    /// Enable strict mode - error on unknown keys.
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }
}

// ============================================================================
// File Parsing Result
// ============================================================================

/// Result of file parsing, including resolution info.
pub struct FileParseResult {
    /// The layer output with parsed values and diagnostics.
    pub output: LayerOutput,
    /// Information about which files were tried.
    pub resolution: FileResolution,
}

// ============================================================================
// Main Parse Function
// ============================================================================

/// Parse a config file using the schema, returning a LayerOutput.
///
/// This resolves the file path, reads the file, parses it using the appropriate
/// format, validates against the schema, and tracks provenance.
pub fn parse_file(schema: &Schema, config: &FileConfig) -> FileParseResult {
    let mut ctx = FileParseContext::new(schema, config);
    ctx.parse();
    ctx.into_result()
}

/// Extract provenance from a ConfigValue.
fn get_provenance(value: &ConfigValue) -> Option<&Provenance> {
    match value {
        ConfigValue::Null(s) => s.provenance.as_ref(),
        ConfigValue::Bool(s) => s.provenance.as_ref(),
        ConfigValue::Integer(s) => s.provenance.as_ref(),
        ConfigValue::Float(s) => s.provenance.as_ref(),
        ConfigValue::String(s) => s.provenance.as_ref(),
        ConfigValue::Array(s) => s.provenance.as_ref(),
        ConfigValue::Object(s) => s.provenance.as_ref(),
        ConfigValue::Enum(s) => s.provenance.as_ref(),
        ConfigValue::Missing(_) => None,
    }
}

/// Context for parsing config files.
struct FileParseContext<'a> {
    schema: &'a Schema,
    config: &'a FileConfig,
    /// The config struct schema, if present
    config_schema: Option<&'a ConfigStructSchema>,
    /// The config field name from schema (e.g., "config" or "settings")
    config_field_name: Option<&'a str>,
    /// Parsed config value (if successful)
    value: Option<ConfigValue>,
    /// Unused keys found in the file
    unused_keys: Vec<UnusedKey>,
    /// Diagnostics collected during parsing
    diagnostics: Vec<Diagnostic>,
    /// File resolution tracking
    resolution: FileResolution,
}

impl<'a> FileParseContext<'a> {
    fn new(schema: &'a Schema, config: &'a FileConfig) -> Self {
        let (config_field_name, config_schema) = if let Some(cs) = schema.config() {
            (cs.field_name(), Some(cs))
        } else {
            (None, None)
        };

        Self {
            schema,
            config,
            config_schema,
            config_field_name,
            value: None,
            unused_keys: Vec::new(),
            diagnostics: Vec::new(),
            resolution: FileResolution::new(),
        }
    }

    fn parse(&mut self) {
        // Resolve which file to load
        let path = match self.resolve_path() {
            Some(p) => p,
            None => return, // No file to load (not an error if no explicit path)
        };

        // Read the file
        let contents = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) => {
                self.emit_error(format!("failed to read {}: {}", path, e));
                return;
            }
        };

        // Parse the file
        let parsed = match self.config.registry.parse_file(&path, &contents) {
            Ok(v) => v,
            Err(e) => {
                self.emit_error(format!("failed to parse {}: {}", path, e));
                return;
            }
        };

        // Validate against schema and collect unused keys
        if let Some(config_schema) = self.config_schema {
            self.validate_and_collect_unused(&parsed, config_schema, Vec::new());
        }

        self.value = Some(parsed);
    }

    /// Resolve which file path to use.
    ///
    /// Returns Some(path) if a file should be loaded, None otherwise.
    fn resolve_path(&mut self) -> Option<Utf8PathBuf> {
        // If explicit path provided, use it (error if not found)
        if let Some(explicit) = &self.config.explicit_path {
            let exists = explicit.exists();
            self.resolution.add_explicit(explicit.clone(), exists);

            if exists {
                // Mark defaults as not tried
                self.resolution
                    .mark_defaults_not_tried(&self.config.default_paths);
                return Some(explicit.clone());
            } else {
                self.emit_error(format!("config file not found: {}", explicit));
                return None;
            }
        }

        // Try default paths in order
        for default_path in &self.config.default_paths {
            if default_path.exists() {
                self.resolution
                    .add_default(default_path.clone(), FilePathStatus::Picked);
                return Some(default_path.clone());
            } else {
                self.resolution
                    .add_default(default_path.clone(), FilePathStatus::Absent);
            }
        }

        // No file found - this is not an error (file layer is optional)
        None
    }

    /// Validate parsed values against the schema and collect unused keys.
    fn validate_and_collect_unused(
        &mut self,
        value: &ConfigValue,
        schema: &ConfigStructSchema,
        path: Vec<String>,
    ) {
        if let ConfigValue::Object(obj) = value {
            for (key, val) in &obj.value {
                let mut key_path = path.clone();
                key_path.push(key.clone());

                // Check if this key exists in the schema
                if let Some(field_schema) = schema.fields().get(key) {
                    // Key exists - recurse if it's a struct
                    match &field_schema.value {
                        ConfigValueSchema::Struct(nested) => {
                            self.validate_and_collect_unused(val, nested, key_path);
                        }
                        ConfigValueSchema::Option { value: inner, .. } => {
                            if let ConfigValueSchema::Struct(nested) = inner.as_ref() {
                                self.validate_and_collect_unused(val, nested, key_path);
                            }
                        }
                        _ => {
                            // Leaf value - nothing to recurse
                        }
                    }
                } else {
                    // Unknown key - extract provenance from the value
                    let prov = get_provenance(val).cloned().unwrap_or(Provenance::Default);
                    self.unused_keys.push(UnusedKey {
                        key: key_path.clone(),
                        provenance: prov,
                    });

                    if self.config.strict {
                        self.emit_error(format!(
                            "unknown configuration key: {}",
                            key_path.join(".")
                        ));
                    }
                }
            }
        }
    }

    fn emit_error(&mut self, message: String) {
        self.diagnostics.push(Diagnostic {
            message,
            path: None,
            span: None,
            severity: Severity::Error,
        });
    }

    #[allow(dead_code)]
    fn emit_warning(&mut self, message: String) {
        self.diagnostics.push(Diagnostic {
            message,
            path: None,
            span: None,
            severity: Severity::Warning,
        });
    }

    fn into_result(self) -> FileParseResult {
        // Wrap the value under the config field name if needed
        let value = if let Some(parsed) = self.value {
            if let Some(field_name) = self.config_field_name {
                // Wrap: {config_field_name: parsed_value}
                let mut root = indexmap::IndexMap::default();
                root.insert(field_name.to_string(), parsed);
                Some(ConfigValue::Object(crate::config_value::Sourced {
                    value: root,
                    span: None,
                    provenance: None,
                }))
            } else {
                Some(parsed)
            }
        } else {
            None
        };

        FileParseResult {
            output: LayerOutput {
                value,
                unused_keys: self.unused_keys,
                diagnostics: self.diagnostics,
            },
            resolution: self.resolution,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as figue;
    use facet::Facet;
    use std::io::Write;
    use tempfile::NamedTempFile;

    // ========================================================================
    // Test schemas
    // ========================================================================

    #[derive(Facet)]
    struct ArgsWithConfig {
        #[facet(figue::named)]
        verbose: bool,

        #[facet(figue::config)]
        config: ServerConfig,
    }

    #[derive(Facet)]
    struct ServerConfig {
        port: u16,
        host: String,
    }

    #[derive(Facet)]
    struct ArgsWithNestedConfig {
        #[facet(figue::config)]
        settings: AppSettings,
    }

    #[derive(Facet)]
    struct AppSettings {
        port: u16,
        smtp: SmtpConfig,
    }

    #[derive(Facet)]
    struct SmtpConfig {
        host: String,
        connection_timeout: u64,
    }

    // ========================================================================
    // Helper functions
    // ========================================================================

    fn create_temp_json(content: &str) -> NamedTempFile {
        let mut file = NamedTempFile::with_suffix(".json").unwrap();
        write!(file, "{}", content).unwrap();
        file
    }

    fn get_nested<'a>(cv: &'a ConfigValue, path: &[&str]) -> Option<&'a ConfigValue> {
        let mut current = cv;
        for key in path {
            match current {
                ConfigValue::Object(obj) => {
                    current = obj.value.get(*key)?;
                }
                _ => return None,
            }
        }
        Some(current)
    }

    fn get_integer(cv: &ConfigValue) -> Option<i64> {
        match cv {
            ConfigValue::Integer(i) => Some(i.value),
            _ => None,
        }
    }

    fn get_string(cv: &ConfigValue) -> Option<&str> {
        match cv {
            ConfigValue::String(s) => Some(&s.value),
            _ => None,
        }
    }

    // ========================================================================
    // Tests: Basic parsing
    // ========================================================================

    #[test]
    fn test_parse_simple_json() {
        let file = create_temp_json(r#"{"port": 8080, "host": "localhost"}"#);
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().path(path);

        let result = parse_file(&schema, &config);

        assert!(result.output.diagnostics.is_empty());
        assert!(result.output.unused_keys.is_empty());

        let value = result.output.value.expect("should have value");
        let port = get_nested(&value, &["config", "port"]).expect("config.port");
        assert_eq!(get_integer(port), Some(8080));

        let host = get_nested(&value, &["config", "host"]).expect("config.host");
        assert_eq!(get_string(host), Some("localhost"));
    }

    #[test]
    fn test_parse_nested_json() {
        let file = create_temp_json(
            r#"{"port": 8080, "smtp": {"host": "mail.example.com", "connection_timeout": 30}}"#,
        );
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithNestedConfig::SHAPE).unwrap();
        let config = FileConfig::new().path(path);

        let result = parse_file(&schema, &config);

        assert!(result.output.diagnostics.is_empty());
        let value = result.output.value.expect("should have value");

        let port = get_nested(&value, &["settings", "port"]).expect("settings.port");
        assert_eq!(get_integer(port), Some(8080));

        let smtp_host =
            get_nested(&value, &["settings", "smtp", "host"]).expect("settings.smtp.host");
        assert_eq!(get_string(smtp_host), Some("mail.example.com"));
    }

    // ========================================================================
    // Tests: File resolution
    // ========================================================================

    #[test]
    fn test_explicit_path_not_found() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().path("/nonexistent/config.json");

        let result = parse_file(&schema, &config);

        assert!(!result.output.diagnostics.is_empty());
        assert!(
            result
                .output
                .diagnostics
                .iter()
                .any(|d| d.message.contains("not found"))
        );
    }

    #[test]
    fn test_no_file_configured() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new(); // No path

        let result = parse_file(&schema, &config);

        // No file = no error, just no value
        assert!(result.output.diagnostics.is_empty());
        assert!(result.output.value.is_none());
    }

    #[test]
    fn test_default_paths_tried_in_order() {
        let file = create_temp_json(r#"{"port": 9000, "host": "default"}"#);
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().default_paths([
            Utf8PathBuf::from("/nonexistent/first.json"),
            path.clone(),
            Utf8PathBuf::from("/nonexistent/third.json"),
        ]);

        let result = parse_file(&schema, &config);

        assert!(result.output.diagnostics.is_empty());
        assert!(result.output.value.is_some());

        // Check resolution tracking
        assert_eq!(result.resolution.paths.len(), 2); // first absent, second picked
        assert!(matches!(
            result.resolution.paths[0].status,
            FilePathStatus::Absent
        ));
        assert!(matches!(
            result.resolution.paths[1].status,
            FilePathStatus::Picked
        ));
    }

    // ========================================================================
    // Tests: Unused keys
    // ========================================================================

    #[test]
    fn test_unknown_key_tracked() {
        let file = create_temp_json(r#"{"port": 8080, "host": "localhost", "unknown_field": 123}"#);
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().path(path);

        let result = parse_file(&schema, &config);

        // Should track the unknown key
        assert!(!result.output.unused_keys.is_empty());
        assert!(
            result
                .output
                .unused_keys
                .iter()
                .any(|k| k.key.contains(&"unknown_field".to_string()))
        );

        // But no error in non-strict mode
        assert!(result.output.diagnostics.is_empty());
    }

    #[test]
    fn test_unknown_key_error_in_strict_mode() {
        let file = create_temp_json(r#"{"port": 8080, "host": "localhost", "unknown_field": 123}"#);
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().path(path).strict();

        let result = parse_file(&schema, &config);

        // Should have error in strict mode
        assert!(!result.output.diagnostics.is_empty());
        assert!(
            result
                .output
                .diagnostics
                .iter()
                .any(|d| d.message.contains("unknown"))
        );
    }

    // ========================================================================
    // Tests: Provenance
    // ========================================================================

    #[test]
    fn test_file_provenance() {
        let file = create_temp_json(r#"{"port": 8080, "host": "localhost"}"#);
        let path = Utf8PathBuf::from_path_buf(file.path().to_path_buf()).unwrap();

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let config = FileConfig::new().path(path.clone());

        let result = parse_file(&schema, &config);

        let value = result.output.value.expect("should have value");
        let port = get_nested(&value, &["config", "port"]).expect("config.port");

        // Check provenance is set
        let prov = get_provenance(port).expect("should have provenance");
        assert!(prov.is_file());
        if let Provenance::File {
            file: config_file, ..
        } = prov
        {
            assert_eq!(config_file.path, path);
        }
    }

    // ========================================================================
    // Tests: Format registry
    // ========================================================================

    #[test]
    fn test_format_registry_with_defaults() {
        let registry = FormatRegistry::with_defaults();
        assert!(registry.find_by_extension("json").is_some());
        assert!(registry.find_by_extension("JSON").is_some()); // case insensitive
        assert!(registry.find_by_extension("toml").is_none());
    }

    #[test]
    fn test_format_registry_extensions() {
        let registry = FormatRegistry::with_defaults();
        let extensions = registry.extensions();
        assert!(extensions.contains(&"json"));
    }
}
