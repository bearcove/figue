//! Schema-driven environment variable parser that outputs ConfigValue with provenance.
//!
//!
//! This parser:
//! - Uses the pre-built Schema to know the config field structure
//! - Outputs LayerOutput (ConfigValue + diagnostics), not a Partial
//! - Does NOT validate types (that's the driver's job)
//! - Reports malformed env var names as diagnostics
//! - Tracks unused keys (env vars that don't match schema fields)
//!
//! # Naming Convention
//!
//! Given a prefix like `"REEF"` and a config struct:
//!
//! ```rust,ignore
//! struct ServerConfig {
//!     port: u16,
//!     smtp: SmtpConfig,
//! }
//!
//! struct SmtpConfig {
//!     host: String,
//!     connection_timeout: u64,
//! }
//! ```
//!
//! The corresponding environment variable names are:
//! - `REEF__PORT` → config.port
//! - `REEF__SMTP__HOST` → config.smtp.host
//! - `REEF__SMTP__CONNECTION_TIMEOUT` → config.smtp.connection_timeout
//!
//! Rules:
//! - Prefix implies the config field (env vars only set config, not CLI args)
//! - All SCREAMING_SNAKE_CASE
//! - Double underscore (`__`) as separator (to allow single `_` in field names)

use std::hash::RandomState;
use std::string::{String, ToString};
use std::vec::Vec;

use indexmap::IndexMap;

use crate::config_value::{ConfigValue, Sourced};
use crate::driver::{Diagnostic, LayerOutput, Severity, UnusedKey};
use crate::provenance::Provenance;
use crate::schema::{ConfigEnumSchema, ConfigStructSchema, ConfigValueSchema, Schema};

// ============================================================================
// EnvSource trait
// ============================================================================

/// Trait for abstracting over environment variable sources.
///
/// This allows testing without modifying the actual environment.
pub trait EnvSource {
    /// Get the value of an environment variable by name.
    fn get(&self, name: &str) -> Option<String>;

    /// Iterate over all environment variables.
    fn vars(&self) -> Box<dyn Iterator<Item = (String, String)> + '_>;
}

/// Environment source that reads from the actual process environment.
#[derive(Debug, Clone, Copy, Default)]
pub struct StdEnv;

impl EnvSource for StdEnv {
    fn get(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn vars(&self) -> Box<dyn Iterator<Item = (String, String)> + '_> {
        Box::new(std::env::vars())
    }
}

/// Environment source backed by a map (for testing).
#[derive(Debug, Clone, Default)]
pub struct MockEnv {
    vars: IndexMap<String, String, std::hash::RandomState>,
}

impl MockEnv {
    /// Create a new empty mock environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a mock environment from an iterator of key-value pairs.
    pub fn from_pairs<I, K, V>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        Self {
            vars: iter
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        }
    }

    /// Set an environment variable.
    pub fn set(&mut self, name: impl Into<String>, value: impl Into<String>) {
        self.vars.insert(name.into(), value.into());
    }
}

impl EnvSource for MockEnv {
    fn get(&self, name: &str) -> Option<String> {
        self.vars.get(name).cloned()
    }

    fn vars(&self) -> Box<dyn Iterator<Item = (String, String)> + '_> {
        Box::new(self.vars.iter().map(|(k, v)| (k.clone(), v.clone())))
    }
}

// ============================================================================
// EnvConfig
// ============================================================================

/// Configuration for environment variable parsing.
pub struct EnvConfig {
    /// The prefix to look for (e.g., `MYAPP`). For example, configuration variable
    /// foo.bar will be overrideable via `MYAPP__FOO__BAR`.
    pub prefix: String,

    /// Whether to error out if any env vars that start with `MYAPP__` should be reported
    /// as errors and stop the program entirely (to try and catch typos)
    pub strict: bool,

    /// Custom environment source (for testing). If None, uses StdEnv.
    pub source: Option<Box<dyn EnvSource>>,
}

impl EnvConfig {
    /// Create a new EnvConfig with the given prefix.
    pub fn new(prefix: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            strict: false,
            source: None,
        }
    }

    /// Enable strict mode.
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Get the env source, or StdEnv if none set.
    pub fn source(&self) -> &dyn EnvSource {
        self.source.as_ref().map(|s| s.as_ref()).unwrap_or(&StdEnv)
    }
}

/// Builder for environment variable configuration.
#[derive(Default)]
pub struct EnvConfigBuilder {
    prefix: String,
    strict: bool,
    source: Option<Box<dyn EnvSource>>,
}

impl EnvConfigBuilder {
    /// Create a new env config builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the environment variable prefix.
    pub fn prefix(mut self, prefix: impl Into<String>) -> Self {
        self.prefix = prefix.into();
        self
    }

    /// Enable strict mode - error on unknown env vars with the prefix.
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Use a custom environment source (for testing).
    pub fn source(mut self, source: impl EnvSource + 'static) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    /// Build the env configuration.
    pub fn build(self) -> EnvConfig {
        let mut config = EnvConfig::new(self.prefix);
        if self.strict {
            config = config.strict();
        }
        config.source = self.source;
        config
    }
}

/// Parse environment variables using the schema, returning a LayerOutput.
///
/// This reads env vars with the configured prefix and builds a ConfigValue tree
/// under the schema's config field.
pub fn parse_env(schema: &Schema, env_config: &EnvConfig, source: &dyn EnvSource) -> LayerOutput {
    let mut ctx = EnvParseContext::new(schema, env_config);
    ctx.parse(source);
    ctx.into_output()
}

/// Context for parsing environment variables.
struct EnvParseContext<'a> {
    env_config: &'a EnvConfig,
    /// The config field name from schema (e.g., "config" or "settings")
    config_field_name: Option<&'a str>,
    /// The config struct schema, if present
    config_schema: Option<&'a ConfigStructSchema>,
    /// Result being built: fields under the config object
    config_fields: IndexMap<String, ConfigValue, RandomState>,
    /// Unused keys (env vars that don't match schema)
    unused_keys: Vec<UnusedKey>,
    /// Diagnostics collected during parsing
    diagnostics: Vec<Diagnostic>,
}

impl<'a> EnvParseContext<'a> {
    fn new(schema: &'a Schema, env_config: &'a EnvConfig) -> Self {
        let (config_field_name, config_schema) = if let Some(cs) = schema.config() {
            (cs.field_name(), Some(cs))
        } else {
            (None, None)
        };

        Self {
            env_config,
            config_field_name,
            config_schema,
            config_fields: IndexMap::default(),
            unused_keys: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn parse(&mut self, source: &dyn EnvSource) {
        // Use explicit prefix from config, or fall back to schema's env_prefix
        let prefix = if self.env_config.prefix.is_empty() {
            self.config_schema
                .and_then(|cs| cs.env_prefix())
                .unwrap_or("")
        } else {
            &self.env_config.prefix
        };

        let prefix_with_sep = format!("{}__", prefix);

        // First pass: collect all prefixed env vars (higher priority)
        for (name, value) in source.vars() {
            // Check if this var matches our prefix
            if !name.starts_with(&prefix_with_sep) {
                continue;
            }

            // Extract the path after the prefix
            let rest = &name[prefix_with_sep.len()..];
            if rest.is_empty() {
                self.emit_warning(format!(
                    "invalid environment variable name: {} (empty after prefix)",
                    name
                ));
                continue;
            }

            // Parse the path segments
            let segments: Vec<&str> = rest.split("__").collect();

            // Check for empty segments
            if segments.iter().any(|s| s.is_empty()) {
                self.emit_warning(format!(
                    "invalid environment variable name: {} (contains empty segment)",
                    name
                ));
                continue;
            }

            // Convert to lowercase for field matching
            let path: Vec<String> = segments.iter().map(|s| s.to_lowercase()).collect();

            // If no config schema, all env vars are unused
            if self.config_schema.is_none() {
                self.add_unused_key(&path, &name);
                continue;
            }

            // Check if path exists in schema and get the target path for insertion
            let config_schema = self.config_schema.unwrap();
            let target_path = match self.resolve_path_in_schema(config_schema, &path) {
                Some(target) => target,
                None => {
                    self.add_unused_key(&path, &name);
                    if self.env_config.strict {
                        self.emit_error(format!("unknown configuration key: {}", path.join(".")));
                    }
                    continue;
                }
            };

            // Parse the value with schema awareness and insert it at the target path
            let config_value = self.parse_value_with_schema(&value, &name, &target_path);
            self.insert_at_path(&target_path, config_value);
        }

        // Second pass: check env aliases (lower priority than prefixed vars)
        // Only set if the field wasn't already set by a prefixed var
        if let Some(config_schema) = self.config_schema {
            self.check_env_aliases(config_schema, source, &[]);
        }
    }

    /// Recursively check env aliases for all fields in a config struct.
    fn check_env_aliases(
        &mut self,
        schema: &ConfigStructSchema,
        source: &dyn EnvSource,
        parent_path: &[String],
    ) {
        for (field_name, field_schema) in schema.fields() {
            let mut field_path = parent_path.to_vec();
            field_path.push(field_name.clone());

            // Check if this field has aliases and wasn't already set
            for alias in field_schema.env_aliases() {
                if !self.has_value_at_path(&field_path)
                    && let Some(value) = source.get(alias)
                {
                    let config_value = self.parse_value_with_schema(&value, alias, &field_path);
                    self.insert_at_path(&field_path, config_value);
                    // Only use the first matching alias
                    break;
                }
            }

            // Recurse into nested structs
            match field_schema.value() {
                ConfigValueSchema::Struct(nested) => {
                    self.check_env_aliases(nested, source, &field_path);
                }
                ConfigValueSchema::Option { value, .. } => {
                    if let ConfigValueSchema::Struct(nested) = value.as_ref() {
                        self.check_env_aliases(nested, source, &field_path);
                    }
                }
                _ => {}
            }
        }
    }

    /// Check if a value exists at the given path in config_fields.
    fn has_value_at_path(&self, path: &[String]) -> bool {
        if path.is_empty() {
            return false;
        }

        let mut current = &self.config_fields;
        for (i, segment) in path.iter().enumerate() {
            match current.get(segment) {
                Some(ConfigValue::Object(obj)) if i < path.len() - 1 => {
                    current = &obj.value;
                }
                Some(_) if i == path.len() - 1 => {
                    return true;
                }
                _ => return false,
            }
        }
        false
    }

    /// Resolve an input path against the schema, validating it exists.
    ///
    /// Returns the path to use for insertion (using the effective names from the schema).
    /// The input path uses lowercase field names (from env var conversion), and this
    /// validates they exist in the schema. The returned path uses the schema's field keys
    /// (effective names after rename).
    fn resolve_path_in_schema(
        &self,
        schema: &ConfigStructSchema,
        path: &[String],
    ) -> Option<Vec<String>> {
        if path.is_empty() {
            return Some(vec![]);
        }

        let first = &path[0];
        if let Some((effective_name, field)) = schema.fields().get_key_value(first) {
            if path.len() == 1 {
                // Leaf field - return the effective name
                return Some(vec![effective_name.clone()]);
            }
            // Check nested
            match field.value() {
                ConfigValueSchema::Struct(nested) => {
                    // Recurse into nested struct
                    if let Some(mut nested_path) = self.resolve_path_in_schema(nested, &path[1..]) {
                        // Prepend the effective name for this field
                        let mut result = vec![effective_name.clone()];
                        result.append(&mut nested_path);
                        Some(result)
                    } else {
                        None
                    }
                }
                ConfigValueSchema::Option { value, .. } => {
                    // Unwrap option and check inner type
                    self.resolve_path_in_value_schema(
                        value.as_ref(),
                        &path[1..],
                        effective_name.clone(),
                    )
                }
                ConfigValueSchema::Enum(enum_schema) => {
                    // Next path segment should be a variant name
                    self.resolve_path_in_enum(enum_schema, &path[1..], effective_name.clone())
                }
                _ => {
                    // Leaf with more path segments - invalid
                    None
                }
            }
        } else {
            None
        }
    }

    /// Helper to resolve path through a ConfigValueSchema (handles Option/Struct/Enum).
    fn resolve_path_in_value_schema(
        &self,
        value_schema: &ConfigValueSchema,
        path: &[String],
        prefix: String,
    ) -> Option<Vec<String>> {
        match value_schema {
            ConfigValueSchema::Struct(nested) => {
                if let Some(mut nested_path) = self.resolve_path_in_schema(nested, path) {
                    let mut result = vec![prefix];
                    result.append(&mut nested_path);
                    Some(result)
                } else {
                    None
                }
            }
            ConfigValueSchema::Enum(enum_schema) => {
                self.resolve_path_in_enum(enum_schema, path, prefix)
            }
            ConfigValueSchema::Option { value, .. } => {
                // Recursively unwrap nested Option
                self.resolve_path_in_value_schema(value.as_ref(), path, prefix)
            }
            _ => {
                // Non-nested type with more path - invalid
                None
            }
        }
    }

    /// Resolve path within an enum schema.
    ///
    /// The first segment of path should be a variant name, and remaining segments
    /// are fields within that variant.
    fn resolve_path_in_enum(
        &self,
        enum_schema: &ConfigEnumSchema,
        path: &[String],
        prefix: String,
    ) -> Option<Vec<String>> {
        if path.is_empty() {
            // Can't set an enum directly via env var path - need to specify variant
            return None;
        }

        let variant_name = &path[0];

        // Find variant by case-insensitive match
        let (effective_variant_name, variant_schema) = enum_schema
            .variants()
            .iter()
            .find(|(name, _)| name.to_lowercase() == *variant_name)?;

        if path.len() == 1 {
            // Just the variant name, no field - this would set the enum to this variant
            // without any field data. Return the path including the variant name.
            return Some(vec![prefix, effective_variant_name.clone()]);
        }

        // Resolve remaining path within the variant's fields
        let field_path = &path[1..];
        let field_name = &field_path[0];

        let (effective_field_name, field_schema) = variant_schema
            .fields()
            .iter()
            .find(|(name, _)| name.to_lowercase() == *field_name)?;

        if field_path.len() == 1 {
            // Leaf field within the variant
            return Some(vec![
                prefix,
                effective_variant_name.clone(),
                effective_field_name.clone(),
            ]);
        }

        // More path segments - recurse into the field's value schema
        match field_schema.value() {
            ConfigValueSchema::Struct(nested) => {
                if let Some(mut nested_path) =
                    self.resolve_path_in_schema(nested, &field_path[1..])
                {
                    let mut result = vec![
                        prefix,
                        effective_variant_name.clone(),
                        effective_field_name.clone(),
                    ];
                    result.append(&mut nested_path);
                    Some(result)
                } else {
                    None
                }
            }
            ConfigValueSchema::Option { value, .. } => self.resolve_path_in_value_schema(
                value.as_ref(),
                &field_path[1..],
                format!(
                    "{}.{}.{}",
                    prefix, effective_variant_name, effective_field_name
                ),
            ),
            ConfigValueSchema::Enum(nested_enum) => {
                // Nested enum within variant field
                self.resolve_path_in_enum(
                    nested_enum,
                    &field_path[1..],
                    format!(
                        "{}.{}.{}",
                        prefix, effective_variant_name, effective_field_name
                    ),
                )
            }
            _ => None,
        }
    }

    /// Get the ConfigValueSchema for a path, if available.
    fn get_value_schema_at_path(&self, path: &[String]) -> Option<&ConfigValueSchema> {
        let config_schema = self.config_schema?;
        config_schema.get_by_path(&path.to_vec())
    }

    /// Parse a value with schema awareness.
    ///
    /// If the target field is an enum, validates that the value is a known variant.
    fn parse_value_with_schema(
        &mut self,
        value: &str,
        var_name: &str,
        target_path: &[String],
    ) -> ConfigValue {
        let prov = Some(Provenance::env(var_name, value));

        // Check if we have schema info for this path
        if let Some(value_schema) = self.get_value_schema_at_path(target_path) {
            // Unwrap Option wrapper if present
            let inner_schema = match value_schema {
                ConfigValueSchema::Option { value: inner, .. } => inner.as_ref(),
                other => other,
            };

            // For enum fields, validate the value is a known variant
            if let ConfigValueSchema::Enum(enum_schema) = inner_schema {
                let variants = enum_schema.variants();
                if !variants.contains_key(value) {
                    // Unknown variant - emit a helpful error
                    let valid_variants: Vec<&String> = variants.keys().collect();
                    self.emit_warning(format!(
                        "{}: unknown variant '{}' for {}. Valid variants are: {}",
                        var_name,
                        value,
                        target_path.join("."),
                        valid_variants
                            .iter()
                            .map(|v| format!("'{}'", v))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }
            }
        }

        // Check for comma-separated list
        if value.contains(',') {
            let elements = parse_comma_separated(value);
            if elements.len() > 1 {
                // Multiple elements - return as array
                let array_elements: Vec<ConfigValue> = elements
                    .into_iter()
                    .map(|s| {
                        ConfigValue::String(Sourced {
                            value: s,
                            span: None,
                            provenance: Some(Provenance::env(var_name, value)),
                        })
                    })
                    .collect();
                return ConfigValue::Array(Sourced {
                    value: array_elements,
                    span: None,
                    provenance: prov,
                });
            } else if elements.len() == 1 {
                // Single element after processing escapes
                return ConfigValue::String(Sourced {
                    value: elements.into_iter().next().unwrap(),
                    span: None,
                    provenance: prov,
                });
            }
        }

        // Simple string value
        ConfigValue::String(Sourced {
            value: value.to_string(),
            span: None,
            provenance: prov,
        })
    }

    fn insert_at_path(&mut self, path: &[String], value: ConfigValue) {
        if path.is_empty() {
            return;
        }

        if path.len() == 1 {
            self.config_fields.insert(path[0].clone(), value);
            return;
        }

        // Navigate/create nested objects
        let first = &path[0];
        let rest = &path[1..];

        let entry = self.config_fields.entry(first.clone()).or_insert_with(|| {
            ConfigValue::Object(Sourced {
                value: IndexMap::default(),
                span: None,
                provenance: None,
            })
        });

        if let ConfigValue::Object(obj) = entry {
            insert_nested(&mut obj.value, rest, value);
        }
    }

    fn add_unused_key(&mut self, path: &[String], var_name: &str) {
        self.unused_keys.push(UnusedKey {
            key: path.to_vec(),
            provenance: Provenance::env(var_name, ""),
        });
    }

    fn emit_warning(&mut self, message: String) {
        self.diagnostics.push(Diagnostic {
            message,
            path: None,
            span: None,
            severity: Severity::Warning,
        });
    }

    fn emit_error(&mut self, message: String) {
        self.diagnostics.push(Diagnostic {
            message,
            path: None,
            span: None,
            severity: Severity::Error,
        });
    }

    fn into_output(self) -> LayerOutput {
        let value = if self.config_fields.is_empty() && self.config_field_name.is_none() {
            // No config field in schema and no values - return empty object
            Some(ConfigValue::Object(Sourced {
                value: IndexMap::default(),
                span: None,
                provenance: None,
            }))
        } else if self.config_fields.is_empty() {
            // Config field exists but no env vars matched - return object with empty config
            let mut root = IndexMap::default();
            if let Some(field_name) = self.config_field_name {
                root.insert(
                    field_name.to_string(),
                    ConfigValue::Object(Sourced {
                        value: IndexMap::default(),
                        span: None,
                        provenance: None,
                    }),
                );
            }
            Some(ConfigValue::Object(Sourced {
                value: root,
                span: None,
                provenance: None,
            }))
        } else {
            // Wrap config_fields under the config field name
            let mut root = IndexMap::default();
            if let Some(field_name) = self.config_field_name {
                root.insert(
                    field_name.to_string(),
                    ConfigValue::Object(Sourced {
                        value: self.config_fields,
                        span: None,
                        provenance: None,
                    }),
                );
            }
            Some(ConfigValue::Object(Sourced {
                value: root,
                span: None,
                provenance: None,
            }))
        };

        LayerOutput {
            value,
            unused_keys: self.unused_keys,
            diagnostics: self.diagnostics,
        }
    }
}

/// Parse a comma-separated string, handling escaping.
fn parse_comma_separated(input: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next) = chars.peek() {
                if next == ',' {
                    chars.next();
                    current.push(',');
                } else {
                    current.push(ch);
                }
            } else {
                current.push(ch);
            }
        } else if ch == ',' {
            let trimmed = current.trim().to_string();
            if !trimmed.is_empty() {
                result.push(trimmed);
            }
            current.clear();
        } else {
            current.push(ch);
        }
    }

    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        result.push(trimmed);
    }

    if result.is_empty() {
        result.push(input.to_string());
    }

    result
}

/// Insert a value at a nested path in an IndexMap.
fn insert_nested(
    map: &mut IndexMap<String, ConfigValue, RandomState>,
    path: &[String],
    value: ConfigValue,
) {
    if path.is_empty() {
        return;
    }

    if path.len() == 1 {
        map.insert(path[0].clone(), value);
        return;
    }

    let key = path[0].clone();
    let entry = map.entry(key).or_insert_with(|| {
        ConfigValue::Object(Sourced {
            value: IndexMap::default(),
            span: None,
            provenance: None,
        })
    });

    if let ConfigValue::Object(sourced) = entry {
        insert_nested(&mut sourced.value, &path[1..], value);
    }
}

#[cfg(test)]
mod tests {
    use facet::Facet;
    use figue_attrs as args;

    use crate::config_value::ConfigValue;
    use crate::driver::Severity;
    use crate::schema::Schema;

    use super::*;

    // ========================================================================
    // Test schemas
    // ========================================================================

    #[derive(Facet)]
    struct ArgsWithConfig {
        #[facet(args::named)]
        verbose: bool,

        #[facet(args::config)]
        config: ServerConfig,
    }

    #[derive(Facet)]
    struct ServerConfig {
        port: u16,
        host: String,
    }

    #[derive(Facet)]
    struct ArgsWithNestedConfig {
        #[facet(args::config)]
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

    #[derive(Facet)]
    struct ArgsWithListConfig {
        #[facet(args::config)]
        config: ListConfig,
    }

    #[derive(Facet)]
    struct ListConfig {
        ports: Vec<u16>,
        allowed_hosts: Vec<String>,
    }

    // ========================================================================
    // Helper functions
    // ========================================================================

    fn env_config(prefix: &str) -> EnvConfig {
        EnvConfigBuilder::new().prefix(prefix).build()
    }

    fn env_config_strict(prefix: &str) -> EnvConfig {
        EnvConfigBuilder::new().prefix(prefix).strict().build()
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

    fn get_string(cv: &ConfigValue) -> Option<&str> {
        match cv {
            ConfigValue::String(s) => Some(&s.value),
            _ => None,
        }
    }

    fn get_array_len(cv: &ConfigValue) -> Option<usize> {
        match cv {
            ConfigValue::Array(arr) => Some(arr.value.len()),
            _ => None,
        }
    }

    // ========================================================================
    // Tests: Basic parsing
    // ========================================================================

    #[test]
    fn test_empty_env() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::new();
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        assert!(output.unused_keys.is_empty());
        // Empty env should produce an empty object (or None?)
        // Let's say it produces an object with just the config field as empty object
    }

    #[test]
    fn test_single_flat_field() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        // Should be {config: {port: "8080"}}
        let port = get_nested(&value, &["config", "port"]).expect("should have config.port");
        assert_eq!(get_string(port), Some("8080"));
    }

    #[test]
    fn test_multiple_flat_fields() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080"), ("REEF__HOST", "localhost")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        let port = get_nested(&value, &["config", "port"]).expect("should have config.port");
        assert_eq!(get_string(port), Some("8080"));

        let host = get_nested(&value, &["config", "host"]).expect("should have config.host");
        assert_eq!(get_string(host), Some("localhost"));
    }

    #[test]
    fn test_nested_field() {
        let schema = Schema::from_shape(ArgsWithNestedConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__SMTP__HOST", "mail.example.com")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        // Config field is named "settings" in this schema
        let host = get_nested(&value, &["settings", "smtp", "host"])
            .expect("should have settings.smtp.host");
        assert_eq!(get_string(host), Some("mail.example.com"));
    }

    #[test]
    fn test_deeply_nested() {
        let schema = Schema::from_shape(ArgsWithNestedConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("REEF__PORT", "8080"),
            ("REEF__SMTP__HOST", "mail.example.com"),
            ("REEF__SMTP__CONNECTION_TIMEOUT", "30"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        let port = get_nested(&value, &["settings", "port"]).expect("port");
        assert_eq!(get_string(port), Some("8080"));

        let host = get_nested(&value, &["settings", "smtp", "host"]).expect("smtp.host");
        assert_eq!(get_string(host), Some("mail.example.com"));

        let timeout = get_nested(&value, &["settings", "smtp", "connection_timeout"])
            .expect("smtp.connection_timeout");
        assert_eq!(get_string(timeout), Some("30"));
    }

    // ========================================================================
    // Tests: Value handling
    // ========================================================================

    #[test]
    fn test_comma_separated_list() {
        let schema = Schema::from_shape(ArgsWithListConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORTS", "8080,8081,8082")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        let ports = get_nested(&value, &["config", "ports"]).expect("config.ports");
        assert_eq!(get_array_len(ports), Some(3));
    }

    #[test]
    fn test_escaped_comma() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__HOST", r"hello\, world")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");

        let host = get_nested(&value, &["config", "host"]).expect("config.host");
        assert_eq!(get_string(host), Some("hello, world"));
    }

    #[test]
    fn test_values_stay_as_strings() {
        // We don't parse "8080" into an integer - that's the driver's job
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        let value = output.value.expect("should have value");
        let port = get_nested(&value, &["config", "port"]).expect("config.port");

        // Should be a string, not an integer
        assert!(matches!(port, ConfigValue::String(_)));
    }

    // ========================================================================
    // Tests: Provenance
    // ========================================================================

    #[test]
    fn test_provenance_is_set() {
        use crate::provenance::Provenance;

        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);
        let value = output.value.expect("should have value");

        let port = get_nested(&value, &["config", "port"]).expect("config.port");
        if let ConfigValue::String(s) = port {
            let prov = s.provenance.as_ref().expect("should have provenance");
            assert!(matches!(prov, Provenance::Env { .. }));
            if let Provenance::Env { var, value } = prov {
                assert_eq!(var, "REEF__PORT");
                assert_eq!(value, "8080");
            }
        } else {
            panic!("expected string");
        }
    }

    // ========================================================================
    // Tests: Malformed names (diagnostics)
    // ========================================================================

    #[test]
    fn test_empty_segment_diagnostic() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__FOO____BAR", "x")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have a diagnostic about invalid env var name
        assert!(!output.diagnostics.is_empty());
        assert!(
            output
                .diagnostics
                .iter()
                .any(|d| d.message.contains("empty segment") || d.message.contains("invalid"))
        );
    }

    #[test]
    fn test_just_prefix_diagnostic() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__", "x")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have a diagnostic
        assert!(!output.diagnostics.is_empty());
    }

    #[test]
    fn test_wrong_prefix_ignored() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("OTHER__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // No diagnostics - it's just a different prefix
        assert!(output.diagnostics.is_empty());
        assert!(output.unused_keys.is_empty());
        // No value or empty object
    }

    #[test]
    fn test_single_underscore_ignored() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF_PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Single underscore doesn't match PREFIX__ pattern
        assert!(output.diagnostics.is_empty());
        assert!(output.unused_keys.is_empty());
    }

    // ========================================================================
    // Tests: Unused keys (schema-aware)
    // ========================================================================

    #[test]
    fn test_unknown_field_unused_key() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        // Typo: PORTT instead of PORT
        let env = MockEnv::from_pairs([("REEF__PORTT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should be in unused_keys
        assert!(!output.unused_keys.is_empty());
        assert!(output.unused_keys.iter().any(|k| {
            // The key path should contain "portt"
            k.key.iter().any(|s| s == "portt")
        }));
    }

    #[test]
    fn test_unknown_nested_field_unused_key() {
        let schema = Schema::from_shape(ArgsWithNestedConfig::SHAPE).unwrap();
        // Typo: HOSTT instead of HOST
        let env = MockEnv::from_pairs([("REEF__SMTP__HOSTT", "x")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(!output.unused_keys.is_empty());
    }

    #[test]
    fn test_strict_mode_unknown_field_error() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORTT", "8080")]);
        let config = env_config_strict("REEF");

        let output = parse_env(&schema, &config, &env);

        // In strict mode, unknown keys should produce an error diagnostic
        assert!(
            output
                .diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error)
        );
    }

    // ========================================================================
    // Tests: Edge cases
    // ========================================================================

    #[test]
    fn test_case_matching() {
        // Env vars are SCREAMING_SNAKE, schema fields are snake_case
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");
        // "PORT" should match "port"
        assert!(get_nested(&value, &["config", "port"]).is_some());
    }

    #[test]
    fn test_field_with_underscore() {
        // connection_timeout in schema should match CONNECTION_TIMEOUT in env
        let schema = Schema::from_shape(ArgsWithNestedConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__SMTP__CONNECTION_TIMEOUT", "30")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");
        assert!(get_nested(&value, &["settings", "smtp", "connection_timeout"]).is_some());
    }

    #[test]
    fn test_empty_value() {
        let schema = Schema::from_shape(ArgsWithConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Empty value should still be set (as empty string), not skipped
        assert!(output.diagnostics.is_empty());
        let value = output.value.expect("should have value");
        let port = get_nested(&value, &["config", "port"]).expect("config.port");
        assert_eq!(get_string(port), Some(""));
    }

    // ========================================================================
    // Tests: No config field in schema
    // ========================================================================

    #[derive(Facet)]
    struct ArgsWithoutConfig {
        #[facet(args::named)]
        verbose: bool,
    }

    #[test]
    fn test_no_config_field_in_schema() {
        // If the schema has no config field, env vars matching the prefix
        // should all be unused keys (or we could emit a warning?)
        let schema = Schema::from_shape(ArgsWithoutConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // No config field means all env vars are unused
        assert!(!output.unused_keys.is_empty());
    }

    // ========================================================================
    // Tests: Flattened config fields
    // ========================================================================

    #[derive(Facet)]
    struct CommonConfig {
        log_level: String,
        debug: bool,
    }

    #[derive(Facet)]
    struct ServerConfigWithFlatten {
        port: u16,
        #[facet(flatten)]
        common: CommonConfig,
    }

    #[derive(Facet)]
    struct ArgsWithFlattenConfig {
        #[facet(args::named)]
        verbose: bool,

        #[facet(args::config)]
        config: ServerConfigWithFlatten,
    }

    #[test]
    fn test_flatten_config_parses_flattened_field() {
        // With flatten, REEF__LOG_LEVEL (not REEF__COMMON__LOG_LEVEL) sets log_level
        // The schema hoists flattened fields to the parent level
        let schema = Schema::from_shape(ArgsWithFlattenConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__LOG_LEVEL", "debug")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        // Flattened field appears at the flattened level, not nested
        let log_level = get_nested(&value, &["config", "log_level"]).expect("config.log_level");
        assert_eq!(get_string(log_level), Some("debug"));
    }

    #[test]
    fn test_flatten_config_top_level_and_flattened() {
        // Mix of top-level (port) and flattened (debug from common) config fields
        let schema = Schema::from_shape(ArgsWithFlattenConfig::SHAPE).unwrap();
        // PORT is not flattened, DEBUG is flattened from common
        let env = MockEnv::from_pairs([("REEF__PORT", "8080"), ("REEF__DEBUG", "true")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        let port = get_nested(&value, &["config", "port"]).expect("config.port");
        assert_eq!(get_string(port), Some("8080"));

        // Flattened field appears at the flattened level
        let debug = get_nested(&value, &["config", "debug"]).expect("config.debug");
        assert_eq!(get_string(debug), Some("true"));
    }

    // ------------------------------------------------------------------------
    // Two-level flatten tests
    // ------------------------------------------------------------------------

    #[derive(Facet)]
    struct DeepConfig {
        trace: bool,
    }

    #[derive(Facet)]
    struct MiddleConfig {
        #[facet(flatten)]
        deep: DeepConfig,
        verbose: bool,
    }

    #[derive(Facet)]
    struct OuterConfigWithDeepFlatten {
        name: String,
        #[facet(flatten)]
        middle: MiddleConfig,
    }

    #[derive(Facet)]
    struct ArgsWithDeepFlattenConfig {
        #[facet(args::config)]
        config: OuterConfigWithDeepFlatten,
    }

    #[test]
    fn test_two_level_flatten_config() {
        // trace is flattened from deep -> middle -> outer
        // So REEF__TRACE should work (not REEF__MIDDLE__DEEP__TRACE)
        let schema = Schema::from_shape(ArgsWithDeepFlattenConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("REEF__NAME", "myapp"),
            ("REEF__VERBOSE", "true"),
            ("REEF__TRACE", "true"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");

        // All fields appear at the top config level due to flattening
        let name = get_nested(&value, &["config", "name"]).expect("config.name");
        assert_eq!(get_string(name), Some("myapp"));

        let verbose = get_nested(&value, &["config", "verbose"]).expect("config.verbose");
        assert_eq!(get_string(verbose), Some("true"));

        let trace = get_nested(&value, &["config", "trace"]).expect("config.trace");
        assert_eq!(get_string(trace), Some("true"));
    }

    #[test]
    fn test_nested_path_rejected_for_flattened_field() {
        // REEF__COMMON__LOG_LEVEL should be rejected because "common" doesn't exist
        // in the flattened schema (log_level is hoisted to the parent)
        let schema = Schema::from_shape(ArgsWithFlattenConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__COMMON__LOG_LEVEL", "debug")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have an unused key because "common" doesn't exist in schema
        assert!(
            !output.unused_keys.is_empty(),
            "should reject nested path for flattened field"
        );
        assert!(
            output
                .unused_keys
                .iter()
                .any(|k| k.key.contains(&"common".to_string())),
            "unused key should contain 'common': {:?}",
            output.unused_keys
        );
    }

    // ========================================================================
    // Tests: Env aliases
    // ========================================================================

    #[derive(Facet)]
    struct ConfigWithAlias {
        /// Database URL with standard alias
        #[facet(args::env_alias = "DATABASE_URL")]
        database_url: String,

        /// Port without alias
        port: u16,
    }

    #[derive(Facet)]
    struct ArgsWithAliasConfig {
        #[facet(args::config)]
        config: ConfigWithAlias,
    }

    #[test]
    fn test_env_alias_basic() {
        // DATABASE_URL should be read via the alias
        let schema = Schema::from_shape(ArgsWithAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("DATABASE_URL", "postgres://localhost/mydb")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        let value = output.value.expect("should have value");

        let db_url = get_nested(&value, &["config", "database_url"]).expect("config.database_url");
        assert_eq!(get_string(db_url), Some("postgres://localhost/mydb"));
    }

    #[test]
    fn test_env_alias_prefixed_wins() {
        // When both prefixed and alias are set, prefixed wins
        let schema = Schema::from_shape(ArgsWithAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("DATABASE_URL", "alias_value"),
            ("REEF__DATABASE_URL", "prefixed_value"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        let value = output.value.expect("should have value");

        let db_url = get_nested(&value, &["config", "database_url"]).expect("config.database_url");
        // Prefixed var should win over alias
        assert_eq!(get_string(db_url), Some("prefixed_value"));
    }

    #[test]
    fn test_env_alias_only_alias_set() {
        // Only alias set, no prefixed - alias should be used
        let schema = Schema::from_shape(ArgsWithAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("DATABASE_URL", "alias_value"), ("REEF__PORT", "8080")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        let value = output.value.expect("should have value");

        let db_url = get_nested(&value, &["config", "database_url"]).expect("config.database_url");
        assert_eq!(get_string(db_url), Some("alias_value"));

        let port = get_nested(&value, &["config", "port"]).expect("config.port");
        assert_eq!(get_string(port), Some("8080"));
    }

    #[derive(Facet)]
    struct ConfigWithMultipleAliases {
        /// Database URL with multiple aliases
        #[facet(args::env_alias = "DATABASE_URL", args::env_alias = "DB_URL")]
        database_url: String,
    }

    #[derive(Facet)]
    struct ArgsWithMultipleAliasConfig {
        #[facet(args::config)]
        config: ConfigWithMultipleAliases,
    }

    #[test]
    fn test_env_alias_multiple_aliases_first_wins() {
        // When multiple aliases exist, the first one found is used
        let schema = Schema::from_shape(ArgsWithMultipleAliasConfig::SHAPE).unwrap();
        // Only the second alias is set
        let env = MockEnv::from_pairs([("DB_URL", "second_alias_value")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        let value = output.value.expect("should have value");

        let db_url = get_nested(&value, &["config", "database_url"]).expect("config.database_url");
        assert_eq!(get_string(db_url), Some("second_alias_value"));
    }

    #[test]
    fn test_env_alias_provenance() {
        // Provenance should show the actual alias var that was used
        use crate::provenance::Provenance;

        let schema = Schema::from_shape(ArgsWithAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("DATABASE_URL", "postgres://localhost/mydb")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        let value = output.value.expect("should have value");
        let db_url = get_nested(&value, &["config", "database_url"]).expect("config.database_url");

        if let ConfigValue::String(s) = db_url {
            let prov = s.provenance.as_ref().expect("should have provenance");
            if let Provenance::Env { var, value } = prov {
                // Should show DATABASE_URL, not REEF__DATABASE_URL
                assert_eq!(var, "DATABASE_URL");
                assert_eq!(value, "postgres://localhost/mydb");
            } else {
                panic!("expected Env provenance");
            }
        } else {
            panic!("expected string");
        }
    }

    #[derive(Facet)]
    struct NestedConfigWithAlias {
        db: DbConfig,
    }

    #[derive(Facet)]
    struct DbConfig {
        #[facet(args::env_alias = "DATABASE_URL")]
        url: String,
    }

    #[derive(Facet)]
    struct ArgsWithNestedAliasConfig {
        #[facet(args::config)]
        config: NestedConfigWithAlias,
    }

    #[test]
    fn test_env_alias_in_nested_struct() {
        // Alias should work even when field is in a nested struct
        let schema = Schema::from_shape(ArgsWithNestedAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("DATABASE_URL", "postgres://localhost/mydb")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "diagnostics: {:?}",
            output.diagnostics
        );
        let value = output.value.expect("should have value");

        let url = get_nested(&value, &["config", "db", "url"]).expect("config.db.url");
        assert_eq!(get_string(url), Some("postgres://localhost/mydb"));
    }

    #[test]
    fn test_env_alias_nested_prefixed_wins() {
        // Prefixed var should win over alias even in nested struct
        let schema = Schema::from_shape(ArgsWithNestedAliasConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("DATABASE_URL", "alias_value"),
            ("REEF__DB__URL", "prefixed_value"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        let value = output.value.expect("should have value");
        let url = get_nested(&value, &["config", "db", "url"]).expect("config.db.url");
        assert_eq!(get_string(url), Some("prefixed_value"));
    }

    // ========================================================================
    // Tests: Schema-guided enum validation
    // ========================================================================

    /// Log level enum for testing enum validation
    #[derive(Facet)]
    #[repr(u8)]
    #[allow(dead_code)]
    enum LogLevel {
        Debug,
        Info,
        Warn,
        Error,
    }

    #[derive(Facet)]
    struct ConfigWithEnum {
        log_level: LogLevel,
        port: u16,
    }

    #[derive(Facet)]
    struct ArgsWithEnumConfig {
        #[facet(args::config)]
        config: ConfigWithEnum,
    }

    #[test]
    fn test_enum_valid_variant_no_warning() {
        // Valid variant should not produce a warning
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__LOG_LEVEL", "Debug")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "valid enum variant should not produce warnings: {:?}",
            output.diagnostics
        );

        let value = output.value.expect("should have value");
        let log_level = get_nested(&value, &["config", "log_level"]).expect("config.log_level");
        assert_eq!(get_string(log_level), Some("Debug"));
    }

    #[test]
    fn test_enum_invalid_variant_produces_warning() {
        // Invalid variant should produce a warning with helpful message
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__LOG_LEVEL", "Debugg")]); // typo
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have a warning
        assert!(
            !output.diagnostics.is_empty(),
            "invalid enum variant should produce a warning"
        );

        // Warning should mention the invalid value and valid variants
        let warning = &output.diagnostics[0];
        assert!(
            warning.message.contains("Debugg"),
            "warning should mention the invalid value: {}",
            warning.message
        );
        assert!(
            warning.message.contains("Debug")
                && warning.message.contains("Info")
                && warning.message.contains("Warn")
                && warning.message.contains("Error"),
            "warning should list valid variants: {}",
            warning.message
        );

        // Value should still be set (the driver will do the actual parsing/rejection)
        let value = output.value.expect("should have value");
        let log_level = get_nested(&value, &["config", "log_level"]).expect("config.log_level");
        assert_eq!(get_string(log_level), Some("Debugg"));
    }

    #[derive(Facet)]
    struct ConfigWithOptionalEnum {
        log_level: Option<LogLevel>,
    }

    #[derive(Facet)]
    struct ArgsWithOptionalEnumConfig {
        #[facet(args::config)]
        config: ConfigWithOptionalEnum,
    }

    #[test]
    fn test_optional_enum_validation() {
        // Optional enum should also be validated
        let schema = Schema::from_shape(ArgsWithOptionalEnumConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__LOG_LEVEL", "invalid")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have a warning even for optional enum
        assert!(
            !output.diagnostics.is_empty(),
            "invalid optional enum variant should produce a warning"
        );
    }

    #[derive(Facet)]
    struct NestedConfigWithEnum {
        logging: LoggingConfig,
    }

    #[derive(Facet)]
    struct LoggingConfig {
        level: LogLevel,
    }

    #[derive(Facet)]
    struct ArgsWithNestedEnumConfig {
        #[facet(args::config)]
        config: NestedConfigWithEnum,
    }

    #[test]
    fn test_nested_enum_validation() {
        // Enum in nested struct should be validated
        let schema = Schema::from_shape(ArgsWithNestedEnumConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__LOGGING__LEVEL", "unknown")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have a warning
        assert!(
            !output.diagnostics.is_empty(),
            "invalid nested enum variant should produce a warning"
        );
    }

    // ========================================================================
    // Tests: Enum variant field setting (issue #37)
    // ========================================================================

    /// Storage enum with struct variants for testing enum variant field paths
    #[derive(Facet)]
    #[repr(u8)]
    #[allow(dead_code)]
    enum Storage {
        S3 { bucket: String, region: String },
        Gcp { project: String, zone: String },
        Local { path: String },
    }

    #[derive(Facet)]
    struct ConfigWithEnumVariants {
        storage: Storage,
        port: u16,
    }

    #[derive(Facet)]
    struct ArgsWithEnumVariantConfig {
        #[facet(args::config)]
        config: ConfigWithEnumVariants,
    }

    #[test]
    fn test_enum_variant_field_single() {
        // Setting a single field within an enum variant: REEF__STORAGE__S3__BUCKET
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__STORAGE__S3__BUCKET", "my-bucket")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "should not have diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "should not have unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        // Should produce: {config: {storage: {s3: {bucket: "my-bucket"}}}}
        let bucket = get_nested(&value, &["config", "storage", "S3", "bucket"])
            .expect("should have config.storage.S3.bucket");
        assert_eq!(get_string(bucket), Some("my-bucket"));
    }

    #[test]
    fn test_enum_variant_field_multiple() {
        // Setting multiple fields within the same enum variant
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("REEF__STORAGE__S3__BUCKET", "my-bucket"),
            ("REEF__STORAGE__S3__REGION", "us-east-1"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "should not have diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "should not have unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        let bucket = get_nested(&value, &["config", "storage", "S3", "bucket"])
            .expect("should have config.storage.S3.bucket");
        assert_eq!(get_string(bucket), Some("my-bucket"));

        let region = get_nested(&value, &["config", "storage", "S3", "region"])
            .expect("should have config.storage.S3.region");
        assert_eq!(get_string(region), Some("us-east-1"));
    }

    #[test]
    fn test_enum_variant_field_different_variant() {
        // Setting fields in a different variant (Gcp instead of S3)
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__STORAGE__GCP__PROJECT", "my-project")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "should not have diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "should not have unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        let project = get_nested(&value, &["config", "storage", "Gcp", "project"])
            .expect("should have config.storage.Gcp.project");
        assert_eq!(get_string(project), Some("my-project"));
    }

    #[test]
    fn test_enum_variant_field_with_regular_field() {
        // Mix of enum variant field and regular config field
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([
            ("REEF__STORAGE__S3__BUCKET", "my-bucket"),
            ("REEF__PORT", "8080"),
        ]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "should not have diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "should not have unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        let bucket = get_nested(&value, &["config", "storage", "S3", "bucket"])
            .expect("should have config.storage.S3.bucket");
        assert_eq!(get_string(bucket), Some("my-bucket"));

        let port = get_nested(&value, &["config", "port"]).expect("should have config.port");
        assert_eq!(get_string(port), Some("8080"));
    }

    #[test]
    fn test_enum_variant_unknown_variant_rejected() {
        // Unknown variant name should be rejected
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__STORAGE__AZURE__CONTAINER", "my-container")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have an unused key
        assert!(
            !output.unused_keys.is_empty(),
            "unknown variant should produce unused key"
        );
        assert!(
            output
                .unused_keys
                .iter()
                .any(|k| k.key.iter().any(|s| s == "azure")),
            "unused key should mention azure: {:?}",
            output.unused_keys
        );
    }

    #[test]
    fn test_enum_variant_unknown_field_rejected() {
        // Unknown field within a valid variant should be rejected
        let schema = Schema::from_shape(ArgsWithEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__STORAGE__S3__UNKNOWN_FIELD", "value")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        // Should have an unused key
        assert!(
            !output.unused_keys.is_empty(),
            "unknown field in variant should produce unused key"
        );
    }

    #[derive(Facet)]
    struct ConfigWithOptionalEnumVariants {
        storage: Option<Storage>,
    }

    #[derive(Facet)]
    struct ArgsWithOptionalEnumVariantConfig {
        #[facet(args::config)]
        config: ConfigWithOptionalEnumVariants,
    }

    #[test]
    fn test_optional_enum_variant_field() {
        // Setting field within optional enum variant
        let schema = Schema::from_shape(ArgsWithOptionalEnumVariantConfig::SHAPE).unwrap();
        let env = MockEnv::from_pairs([("REEF__STORAGE__LOCAL__PATH", "/data")]);
        let config = env_config("REEF");

        let output = parse_env(&schema, &config, &env);

        assert!(
            output.diagnostics.is_empty(),
            "should not have diagnostics: {:?}",
            output.diagnostics
        );
        assert!(
            output.unused_keys.is_empty(),
            "should not have unused keys: {:?}",
            output.unused_keys
        );

        let value = output.value.expect("should have value");
        let path = get_nested(&value, &["config", "storage", "Local", "path"])
            .expect("should have config.storage.Local.path");
        assert_eq!(get_string(path), Some("/data"));
    }
}
