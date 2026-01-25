//! Schema-driven CLI argument parser that outputs ConfigValue with provenance.
//!
//! This module is under active development and not yet wired into the main API.
#![allow(dead_code)]
//!
//! This parser:
//! - Uses the pre-built Schema (not raw attribute lookups)
//! - Outputs LayerOutput (ConfigValue + diagnostics), not a Partial
//! - Does NOT set defaults (that's the driver's job)
//! - Reports errors properly (no silent skipping)

use std::hash::RandomState;
use std::string::{String, ToString};
use std::vec::Vec;

use heck::{ToKebabCase, ToSnakeCase};
use indexmap::IndexMap;

use crate::config_value::{ConfigValue, EnumValue, Sourced};
use crate::driver::{Diagnostic, LayerOutput, Severity};
use crate::provenance::Provenance;
use crate::schema::{ArgKind, ArgLevelSchema, ArgSchema, Schema, Subcommand};

// ============================================================================
// CliConfig
// ============================================================================

/// Configuration for CLI argument parsing.
#[derive(Debug, Clone, Default)]
pub struct CliConfig {
    /// Raw CLI arguments.
    args: Vec<String>,
    /// Whether to error on unknown arguments.
    strict: bool,
}

impl CliConfig {
    /// Get the CLI arguments.
    pub fn args(&self) -> &[String] {
        &self.args
    }

    /// Check if strict mode is enabled.
    pub fn strict(&self) -> bool {
        self.strict
    }
}

/// Builder for CLI configuration.
#[derive(Debug, Default)]
pub struct CliConfigBuilder {
    config: CliConfig,
}

impl CliConfigBuilder {
    /// Create a new CLI config builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the CLI arguments to parse.
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.config.args = args.into_iter().map(|s| s.into()).collect();
        self
    }

    /// Set CLI arguments from OsString iterator (e.g., std::env::args_os()).
    pub fn args_os<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<std::ffi::OsStr>,
    {
        self.config.args = args
            .into_iter()
            .filter_map(|s| s.as_ref().to_str().map(|s| s.to_string()))
            .collect();
        self
    }

    /// Enable strict mode - error on unknown arguments.
    pub fn strict(mut self) -> Self {
        self.config.strict = true;
        self
    }

    /// Build the CLI configuration.
    pub fn build(self) -> CliConfig {
        self.config
    }
}

/// Parse CLI arguments using the schema, returning a LayerOutput.
///
/// This handles both:
/// - `schema.args`: top-level flags, positionals, subcommands
/// - `schema.config` overrides: `--config.port 8080` style dotted paths
pub fn parse_cli(schema: &Schema, cli_config: &CliConfig) -> LayerOutput {
    let args: Vec<&str> = cli_config.args().iter().map(|s| s.as_str()).collect();
    let mut ctx = ParseContext::new(&args, schema, cli_config.strict());
    ctx.parse();
    ctx.into_output()
}

/// Accumulator for a counted flag.
struct CountedAccumulator {
    /// Number of times the flag was seen.
    count: u64,
    /// Target path for inserting the value (from schema).
    target_path: Vec<String>,
}

/// Parser context holding state during CLI parsing.
struct ParseContext<'a> {
    /// Input arguments
    args: &'a [&'a str],
    /// Current position in args
    index: usize,
    /// Schema being parsed against
    schema: &'a Schema,
    /// Result object being built
    result: IndexMap<String, ConfigValue, RandomState>,
    /// Diagnostics collected during parsing
    diagnostics: Vec<Diagnostic>,
    /// Whether we've seen `--` (positional-only mode)
    positional_only: bool,
    /// Counted flag accumulators: field_name -> accumulator with target_path
    counted: IndexMap<String, CountedAccumulator, RandomState>,
    /// Whether to error on unknown arguments
    strict: bool,
    /// Byte offset where each argument starts in the flattened string (args joined by spaces)
    arg_offsets: Vec<usize>,
}

impl<'a> ParseContext<'a> {
    fn new(args: &'a [&'a str], schema: &'a Schema, strict: bool) -> Self {
        // Compute byte offsets for each argument in the flattened string
        // When args are joined with spaces: "arg0 arg1 arg2"
        // arg0 starts at 0, arg1 starts at len(arg0)+1, etc.
        let mut arg_offsets = Vec::with_capacity(args.len());
        let mut offset = 0;
        for (i, arg) in args.iter().enumerate() {
            arg_offsets.push(offset);
            offset += arg.len();
            if i < args.len() - 1 {
                offset += 1; // space separator
            }
        }

        Self {
            args,
            index: 0,
            schema,
            result: IndexMap::default(),
            diagnostics: Vec::new(),
            positional_only: false,
            counted: IndexMap::default(),
            strict,
            arg_offsets,
        }
    }

    /// Get the span (byte offset and length) for argument at the given index.
    fn span_for_arg(&self, arg_index: usize) -> facet_reflect::Span {
        let offset = self.arg_offsets.get(arg_index).copied().unwrap_or(0);
        let len = self.args.get(arg_index).map(|s| s.len()).unwrap_or(0);
        facet_reflect::Span::new(offset, len)
    }

    /// Get the span for the current argument.
    fn current_span(&self) -> facet_reflect::Span {
        self.span_for_arg(self.index)
    }

    /// Get a span covering a substring within the current argument.
    /// `sub_offset` is the byte offset within the argument, `sub_len` is the length.
    fn span_within_current(&self, sub_offset: usize, sub_len: usize) -> facet_reflect::Span {
        let base = self.arg_offsets.get(self.index).copied().unwrap_or(0);
        facet_reflect::Span::new(base + sub_offset, sub_len)
    }

    fn parse(&mut self) {
        self.parse_level(self.schema.args());
        self.apply_counted_fields();
    }

    fn parse_level(&mut self, level: &ArgLevelSchema) {
        while self.index < self.args.len() {
            let arg = self.args[self.index];

            // Handle `--` separator
            if arg == "--" {
                self.positional_only = true;
                self.index += 1;
                continue;
            }

            // In positional-only mode, everything is a positional
            if self.positional_only {
                if !self.try_parse_positional(level) && !self.try_parse_subcommand(level) {
                    self.emit_error(format!("unexpected positional argument: {}", arg));
                    self.index += 1;
                }
                continue;
            }

            // Try to parse as a flag or positional
            if arg.starts_with("--") {
                self.parse_long_flag(arg, level);
            } else if arg.starts_with('-') && arg.len() > 1 {
                self.parse_short_flag(arg, level);
            } else {
                // Positional or subcommand
                if !self.try_parse_subcommand(level) && !self.try_parse_positional(level) {
                    self.emit_error(format!("unexpected argument: {}", arg));
                    self.index += 1;
                }
            }
        }
    }

    fn parse_long_flag(&mut self, arg: &str, level: &ArgLevelSchema) {
        let flag = &arg[2..]; // strip "--"

        // Reject flags that start with another dash (e.g., ---verbose)
        if flag.starts_with('-') {
            self.emit_error(format!("unknown flag: {}", arg));
            self.index += 1;
            return;
        }

        // Check for `--flag=value` syntax
        let (flag_name, inline_value) = if let Some(eq_pos) = flag.find('=') {
            (&flag[..eq_pos], Some(&flag[eq_pos + 1..]))
        } else {
            (flag, None)
        };

        // Check if this is a config override (e.g., --config.port)
        if let Some(config_schema) = self.schema.config()
            && let Some(config_field_name) = config_schema.field_name()
            && flag_name.starts_with(config_field_name)
            && flag_name.len() > config_field_name.len()
            && flag_name.as_bytes()[config_field_name.len()] == b'.'
        {
            self.parse_config_override(arg, flag_name, inline_value, config_field_name);
            return;
        }

        // Look up in schema
        let flag_snake = flag_name.to_snake_case();
        if let Some(arg_schema) = level.args().get(&flag_snake) {
            if let ArgKind::Named { counted, .. } = arg_schema.kind()
                && *counted
            {
                self.increment_counted(&flag_snake, arg_schema.target_path());
                self.index += 1;
                return;
            }
            self.parse_flag_value(arg, &flag_snake, arg_schema, inline_value);
        } else {
            self.emit_error(format!("unknown flag: --{}", flag_name));
            self.index += 1;
        }
    }

    fn parse_short_flag(&mut self, arg: &str, level: &ArgLevelSchema) {
        let flag_part = &arg[1..]; // strip "-"

        // Check for `-k=value` syntax (single short flag with equals)
        if let Some(eq_pos) = flag_part.find('=') {
            let flag_char = flag_part[..eq_pos].chars().next();
            if eq_pos == 1 {
                // Valid: -k=value (single char before =)
                if let Some(ch) = flag_char {
                    let value_str = &flag_part[eq_pos + 1..];
                    self.parse_short_flag_with_value(ch, value_str, level);
                    self.index += 1;
                    return;
                }
            }
            // Invalid: -abc=value or -=value - fall through to error handling
        }

        let chars: Vec<char> = flag_part.chars().collect();

        for (i, ch) in chars.iter().enumerate() {
            // Find argument with this short flag
            let found = level.args().iter().find(|(_, schema)| {
                if let ArgKind::Named { short: Some(s), .. } = schema.kind() {
                    *s == *ch
                } else {
                    false
                }
            });

            if let Some((name, arg_schema)) = found {
                if let ArgKind::Named { counted, .. } = arg_schema.kind()
                    && *counted
                {
                    self.increment_counted(name, arg_schema.target_path());
                    continue;
                }

                let is_bool = arg_schema
                    .value()
                    .inner_if_option()
                    .is_bool_or_vec_of_bool();
                let is_last = i == chars.len() - 1;

                if is_bool {
                    // Bool flag: set to true
                    let prov = Provenance::cli(format!("-{}", ch), "true");
                    self.insert_at_path(
                        arg_schema.target_path(),
                        ConfigValue::Bool(Sourced {
                            value: true,
                            span: None,
                            provenance: Some(prov),
                        }),
                    );
                } else if is_last {
                    // Non-bool flag at end: look for value
                    let flag_span = self.current_span(); // Save span before incrementing
                    self.index += 1;
                    if self.index < self.args.len() {
                        let value_span = self.current_span();
                        let value_str = self.args[self.index];
                        let prov_arg = format!("-{}", ch);
                        let value = self.parse_value_string(value_str, &prov_arg, value_span);
                        self.insert_at_path(arg_schema.target_path(), value);
                    } else {
                        self.emit_error_at(format!("flag -{} requires a value", ch), flag_span);
                    }
                } else {
                    // Non-bool flag with attached value: -p8080
                    // Span for attached value is within the current arg
                    let rest: String = chars[i + 1..].iter().collect();
                    let value_span = self.span_within_current(i + 1, rest.len());
                    let prov_arg = format!("-{}", ch);
                    let value = self.parse_value_string(&rest, &prov_arg, value_span);
                    self.insert_at_path(arg_schema.target_path(), value);
                    break; // Consumed rest of chars
                }
            } else {
                self.emit_error(format!("unknown flag: -{}", ch));
            }
        }
        self.index += 1;
    }

    /// Parse a short flag with an inline value (e.g., -k=3)
    fn parse_short_flag_with_value(&mut self, ch: char, value_str: &str, level: &ArgLevelSchema) {
        let found = level.args().iter().find(|(_, schema)| {
            if let ArgKind::Named { short: Some(s), .. } = schema.kind() {
                *s == ch
            } else {
                false
            }
        });

        if let Some((name, arg_schema)) = found {
            if let ArgKind::Named { counted, .. } = arg_schema.kind()
                && *counted
            {
                self.increment_counted(name, arg_schema.target_path());
                return;
            }

            let is_bool = arg_schema.value().inner_if_option().is_bool();
            let prov_arg = format!("-{}", ch);

            if is_bool {
                // --flag=true or --flag=false
                let value = matches!(
                    value_str.to_lowercase().as_str(),
                    "true" | "yes" | "1" | "on" | ""
                );
                let prov = Provenance::cli(&prov_arg, value.to_string());
                self.insert_at_path(
                    arg_schema.target_path(),
                    ConfigValue::Bool(Sourced {
                        value,
                        span: None,
                        provenance: Some(prov),
                    }),
                );
            } else {
                // Value starts after the '=' which is at position 2 (after -k)
                let value_span = self.span_within_current(3, value_str.len());
                let value = self.parse_value_string(value_str, &prov_arg, value_span);
                self.insert_at_path(arg_schema.target_path(), value);
            }
        } else {
            self.emit_error(format!("unknown flag: -{}", ch));
        }
    }

    fn parse_flag_value(
        &mut self,
        arg: &str,
        _name: &str,
        schema: &ArgSchema,
        inline_value: Option<&str>,
    ) {
        let is_bool = schema.value().inner_if_option().is_bool();

        if is_bool {
            // Bool flag: presence means true
            let value = if let Some(v) = inline_value {
                // --flag=true or --flag=false
                matches!(v.to_lowercase().as_str(), "true" | "yes" | "1" | "on" | "")
            } else {
                true
            };
            let prov = Provenance::cli(arg, value.to_string());
            self.insert_at_path(
                schema.target_path(),
                ConfigValue::Bool(Sourced {
                    value,
                    span: None,
                    provenance: Some(prov),
                }),
            );
            self.index += 1;
        } else {
            // Non-bool: need a value
            let flag_span = self.current_span(); // Save span before incrementing
            let (value_str, value_span) = if let Some(v) = inline_value {
                // --flag=value: value starts after the '='
                let eq_pos = arg.find('=').unwrap_or(0) + 1;
                let span = self.span_within_current(eq_pos, v.len());
                self.index += 1;
                (v, span)
            } else {
                self.index += 1;
                if self.index < self.args.len() {
                    let span = self.current_span();
                    let v = self.args[self.index];
                    self.index += 1;
                    (v, span)
                } else {
                    self.emit_error_at(format!("flag {} requires a value", arg), flag_span);
                    return;
                }
            };

            let prov_arg = arg.split('=').next().unwrap_or(arg);
            let value = self.parse_value_string(value_str, prov_arg, value_span);
            self.insert_at_path(schema.target_path(), value);
        }
    }

    fn parse_config_override(
        &mut self,
        _arg: &str,
        flag_name: &str,
        inline_value: Option<&str>,
        config_field_name: &str,
    ) {
        // Extract the path after "config."
        let path = &flag_name[config_field_name.len() + 1..];
        let parts: Vec<&str> = path.split('.').collect();

        // Get the value
        let flag_span = self.current_span(); // Save span before incrementing
        let (value_str, value_span) = if let Some(v) = inline_value {
            // --config.key=value: value starts after the '='
            let eq_pos = flag_name.len() + 3 + 1; // "--" + flag_name + "="
            let span = self.span_within_current(eq_pos, v.len());
            self.index += 1;
            (v, span)
        } else {
            self.index += 1;
            if self.index < self.args.len() {
                let span = self.current_span();
                let v = self.args[self.index];
                self.index += 1;
                (v, span)
            } else {
                self.emit_error_at(format!("flag --{} requires a value", flag_name), flag_span);
                return;
            }
        };

        let prov_arg = format!("--{}", flag_name);
        let value = self.parse_value_string(value_str, &prov_arg, value_span);

        // Insert into nested config structure
        self.insert_config_value(config_field_name, &parts, value);
    }

    fn insert_config_value(&mut self, config_field_name: &str, path: &[&str], value: ConfigValue) {
        // Get or create the config object
        let config_obj = self
            .result
            .entry(config_field_name.to_string())
            .or_insert_with(|| {
                ConfigValue::Object(Sourced {
                    value: IndexMap::default(),
                    span: None,
                    provenance: None,
                })
            });

        if let ConfigValue::Object(sourced) = config_obj {
            insert_nested(&mut sourced.value, path, value);
        }
    }

    fn try_parse_subcommand(&mut self, level: &ArgLevelSchema) -> bool {
        let Some(field_name) = level.subcommand_field_name() else {
            return false;
        };

        let arg = self.args[self.index];
        let arg_kebab = arg.to_kebab_case();

        if let Some(subcommand) = level.subcommands().get(&arg_kebab) {
            self.index += 1;
            let fields = self.parse_subcommand_args(subcommand);

            // Use the original Rust variant name for deserialization
            // (facet-format expects "Build", not "build")
            let enum_value = ConfigValue::Enum(Sourced {
                value: EnumValue {
                    variant: subcommand.variant_name().to_string(),
                    fields,
                },
                span: None,
                provenance: Some(Provenance::cli(arg, "")),
            });

            self.result.insert(field_name.to_string(), enum_value);
            return true;
        }

        false
    }

    fn parse_subcommand_args(
        &mut self,
        subcommand: &Subcommand,
    ) -> IndexMap<String, ConfigValue, RandomState> {
        // Save current state
        let old_result = core::mem::take(&mut self.result);
        let old_counted = core::mem::take(&mut self.counted);

        // Parse subcommand arguments
        self.parse_level(subcommand.args());
        self.apply_counted_fields();

        // Restore and return
        let subcommand_result = core::mem::replace(&mut self.result, old_result);
        self.counted = old_counted;

        subcommand_result
    }

    fn try_parse_positional(&mut self, level: &ArgLevelSchema) -> bool {
        let arg = self.args[self.index];

        // Find the next unset positional argument
        for (_name, schema) in level.args() {
            if !matches!(schema.kind(), ArgKind::Positional) {
                continue;
            }

            // Check if already set (unless it's multiple/list)
            if self.has_value_at_path(schema.target_path()) && !schema.multiple() {
                continue;
            }

            let value_span = self.current_span();
            let value = self.parse_value_string(arg, arg, value_span);
            self.insert_at_path(schema.target_path(), value);
            self.index += 1;
            return true;
        }

        false
    }

    fn parse_value_string(
        &self,
        s: &str,
        arg_name: &str,
        span: facet_reflect::Span,
    ) -> ConfigValue {
        let prov = Some(Provenance::cli(arg_name, s));

        // Keep values as strings - type coercion happens during deserialization
        // based on the schema's expected types. This is consistent with the env layer.
        ConfigValue::String(Sourced {
            value: s.to_string(),
            span: Some(span),
            provenance: prov,
        })
    }

    /// Insert a value at the given target path.
    fn insert_at_path(&mut self, target_path: &[String], value: ConfigValue) {
        let path_refs: Vec<&str> = target_path.iter().map(|s| s.as_str()).collect();
        insert_nested(&mut self.result, &path_refs, value);
    }

    /// Check if a value exists at the given target path.
    fn has_value_at_path(&self, target_path: &[String]) -> bool {
        let mut current: &IndexMap<String, ConfigValue, RandomState> = &self.result;
        for (i, key) in target_path.iter().enumerate() {
            match current.get(key) {
                Some(ConfigValue::Object(sourced)) if i < target_path.len() - 1 => {
                    current = &sourced.value;
                }
                Some(_) if i == target_path.len() - 1 => return true,
                _ => return false,
            }
        }
        false
    }

    fn increment_counted(&mut self, name: &str, target_path: &[String]) {
        let acc = self
            .counted
            .entry(name.to_string())
            .or_insert_with(|| CountedAccumulator {
                count: 0,
                target_path: target_path.to_vec(),
            });
        acc.count += 1;
    }

    fn apply_counted_fields(&mut self) {
        for (name, acc) in &self.counted {
            let prov = Provenance::cli(
                format!("-{}", name.chars().next().unwrap_or('?')),
                acc.count.to_string(),
            );
            let value = ConfigValue::Integer(Sourced {
                value: acc.count as i64,
                span: None,
                provenance: Some(prov),
            });
            let path_refs: Vec<&str> = acc.target_path.iter().map(|s| s.as_str()).collect();
            insert_nested(&mut self.result, &path_refs, value);
        }
    }

    /// Emit an error diagnostic for the current argument.
    fn emit_error(&mut self, message: String) {
        self.emit_error_at(message, self.current_span());
    }

    /// Emit an error diagnostic at a specific span.
    fn emit_error_at(&mut self, message: String, span: facet_reflect::Span) {
        self.diagnostics.push(Diagnostic {
            message,
            path: None,
            span: Some(crate::span::Span::new(span.offset, span.len)),
            severity: Severity::Error,
        });
    }

    fn into_output(self) -> LayerOutput {
        let value = if self.result.is_empty() {
            Some(ConfigValue::Object(Sourced {
                value: IndexMap::default(),
                span: None,
                provenance: None,
            }))
        } else {
            Some(ConfigValue::Object(Sourced {
                value: self.result,
                span: None,
                provenance: None,
            }))
        };

        LayerOutput {
            value,
            unused_keys: Vec::new(),
            diagnostics: self.diagnostics,
        }
    }
}

/// Insert a value at a nested path in an IndexMap.
/// If the path already has a value, accumulates into an array.
fn insert_nested(
    map: &mut IndexMap<String, ConfigValue, RandomState>,
    path: &[&str],
    value: ConfigValue,
) {
    if path.is_empty() {
        return;
    }

    if path.len() == 1 {
        let key = path[0].to_string();
        // Check if there's already a value at this path
        if let Some(existing) = map.get_mut(&key) {
            // Accumulate into an array
            match existing {
                ConfigValue::Array(arr) => {
                    // Already an array, push the new value
                    arr.value.push(value);
                }
                _ => {
                    // Convert existing scalar to array with both values
                    let old_value = std::mem::replace(
                        existing,
                        ConfigValue::Array(Sourced {
                            value: Vec::new(),
                            span: None,
                            provenance: None,
                        }),
                    );
                    if let ConfigValue::Array(arr) = existing {
                        arr.value.push(old_value);
                        arr.value.push(value);
                    }
                }
            }
        } else {
            map.insert(key, value);
        }
        return;
    }

    let key = path[0].to_string();
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
    use super::*;
    use crate as args;
    use facet::Facet;

    // ========================================================================
    // Test helpers for building ConfigValue concisely
    // ========================================================================

    /// Build a ConfigValue with CLI provenance
    mod cv {
        use super::*;

        pub fn bool(value: bool, arg: &str) -> ConfigValue {
            ConfigValue::Bool(Sourced {
                value,
                span: None,
                provenance: Some(Provenance::cli(arg, value.to_string())),
            })
        }

        pub fn int(value: i64, arg: &str) -> ConfigValue {
            ConfigValue::Integer(Sourced {
                value,
                span: None,
                provenance: Some(Provenance::cli(arg, value.to_string())),
            })
        }

        pub fn string(value: impl Into<String>, arg: &str) -> ConfigValue {
            let s = value.into();
            ConfigValue::String(Sourced {
                value: s.clone(),
                span: None,
                provenance: Some(Provenance::cli(arg, s)),
            })
        }

        pub fn object(
            fields: impl IntoIterator<Item = (&'static str, ConfigValue)>,
        ) -> ConfigValue {
            let map: IndexMap<String, ConfigValue, std::hash::RandomState> = fields
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect();
            ConfigValue::Object(Sourced {
                value: map,
                span: None,
                provenance: Some(Provenance::Cli {
                    arg: String::new(),
                    value: String::new(),
                }),
            })
        }

        pub fn enumv(
            variant: &str,
            fields: impl IntoIterator<Item = (&'static str, ConfigValue)>,
        ) -> ConfigValue {
            use crate::config_value::EnumValue;
            let fields_map: IndexMap<String, ConfigValue, std::hash::RandomState> = fields
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect();
            ConfigValue::Enum(Sourced {
                value: EnumValue {
                    variant: variant.to_string(),
                    fields: fields_map,
                },
                span: None,
                provenance: Some(Provenance::Cli {
                    arg: variant.to_string(),
                    value: String::new(),
                }),
            })
        }
    }

    /// Build a CliConfig from a slice of string slices (for tests)
    fn cli_config(args: &[&str]) -> CliConfig {
        CliConfigBuilder::new()
            .args(args.iter().map(|s| s.to_string()))
            .build()
    }

    /// Assert that parsing produces the expected ConfigValue
    fn assert_parses_to<T: Facet<'static>>(args: &[&str], expected: ConfigValue) {
        let schema = Schema::from_shape(T::SHAPE).expect("valid schema");
        let config = cli_config(args);
        let output = parse_cli(&schema, &config);

        assert!(
            output.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            output.diagnostics
        );

        let value = output.value.expect("expected a value");
        assert_config_value_eq(&value, &expected);
    }

    /// Assert that parsing produces a diagnostic containing the given message
    fn assert_diagnostic_contains<T: Facet<'static>>(args: &[&str], expected_msg: &str) {
        let schema = Schema::from_shape(T::SHAPE).expect("valid schema");
        let config = cli_config(args);
        let output = parse_cli(&schema, &config);

        assert!(
            output
                .diagnostics
                .iter()
                .any(|d| d.message.contains(expected_msg)),
            "expected diagnostic containing {:?}, got: {:?}",
            expected_msg,
            output.diagnostics
        );
    }

    /// Deep equality check for ConfigValue (ignoring provenance details)
    fn assert_config_value_eq(actual: &ConfigValue, expected: &ConfigValue) {
        match (actual, expected) {
            (ConfigValue::Bool(a), ConfigValue::Bool(e)) => {
                assert_eq!(a.value, e.value, "bool mismatch");
            }
            (ConfigValue::Integer(a), ConfigValue::Integer(e)) => {
                assert_eq!(a.value, e.value, "integer mismatch");
            }
            (ConfigValue::String(a), ConfigValue::String(e)) => {
                assert_eq!(a.value, e.value, "string mismatch");
            }
            (ConfigValue::Object(a), ConfigValue::Object(e)) => {
                let mut actual_keys: Vec<_> = a.value.keys().collect();
                let mut expected_keys: Vec<_> = e.value.keys().collect();
                actual_keys.sort();
                expected_keys.sort();
                assert_eq!(actual_keys, expected_keys, "object keys mismatch");
                for (key, expected_val) in &e.value {
                    let actual_val = a
                        .value
                        .get(key)
                        .unwrap_or_else(|| panic!("missing key: {}", key));
                    assert_config_value_eq(actual_val, expected_val);
                }
            }
            (ConfigValue::Array(a), ConfigValue::Array(e)) => {
                assert_eq!(a.value.len(), e.value.len(), "array length mismatch");
                for (av, ev) in a.value.iter().zip(e.value.iter()) {
                    assert_config_value_eq(av, ev);
                }
            }
            (ConfigValue::Enum(a), ConfigValue::Enum(e)) => {
                assert_eq!(a.value.variant, e.value.variant, "enum variant mismatch");
                assert_eq!(
                    a.value.fields.keys().collect::<Vec<_>>(),
                    e.value.fields.keys().collect::<Vec<_>>(),
                    "enum fields mismatch"
                );
                for (key, expected_val) in &e.value.fields {
                    let actual_val = a
                        .value
                        .fields
                        .get(key)
                        .unwrap_or_else(|| panic!("missing enum field: {}", key));
                    assert_config_value_eq(actual_val, expected_val);
                }
            }
            (ConfigValue::Null(_), ConfigValue::Null(_)) => {}
            _ => panic!(
                "ConfigValue variant mismatch: {:?} vs {:?}",
                core::mem::discriminant(actual),
                core::mem::discriminant(expected)
            ),
        }
    }

    // ========================================================================
    // Test schemas
    // ========================================================================

    #[derive(Facet)]
    struct SimpleArgs {
        /// Enable verbose output
        #[facet(args::named, args::short = 'v')]
        verbose: bool,

        /// Port number
        #[facet(args::named, args::short = 'p')]
        port: Option<u16>,
    }

    #[derive(Facet)]
    struct ArgsWithPositional {
        /// The input file
        #[facet(args::positional)]
        input: String,

        /// The output file
        #[facet(args::positional)]
        output: Option<String>,
    }

    #[derive(Facet)]
    struct ArgsWithConfig {
        #[facet(args::named, args::short = 'v')]
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
    struct ArgsWithSubcommand {
        #[facet(args::named, args::short = 'v')]
        verbose: bool,

        #[facet(args::subcommand)]
        command: Command,
    }

    #[derive(Facet)]
    #[repr(u8)]
    enum Command {
        /// Build the project
        Build(BuildArgs),
        /// Run tests
        Test(TestArgs),
    }

    #[derive(Facet)]
    struct BuildArgs {
        /// Build in release mode
        #[facet(args::named, args::short = 'r')]
        release: bool,
    }

    #[derive(Facet)]
    struct TestArgs {
        /// Filter tests by name
        #[facet(args::named)]
        filter: Option<String>,
    }

    #[derive(Facet)]
    struct CountedArgs {
        /// Verbosity level
        #[facet(args::named, args::short = 'v', args::counted)]
        verbose: u8,
    }

    // ========================================================================
    // Tests: Basic flag parsing
    // ========================================================================

    #[test]
    fn test_empty_args() {
        assert_parses_to::<SimpleArgs>(&[], cv::object([]));
    }

    #[test]
    fn test_long_bool_flag() {
        assert_parses_to::<SimpleArgs>(
            &["--verbose"],
            cv::object([("verbose", cv::bool(true, "--verbose"))]),
        );
    }

    #[test]
    fn test_short_bool_flag() {
        assert_parses_to::<SimpleArgs>(&["-v"], cv::object([("verbose", cv::bool(true, "-v"))]));
    }

    #[test]
    fn test_long_flag_with_value() {
        assert_parses_to::<SimpleArgs>(
            &["--port", "8080"],
            cv::object([("port", cv::string("8080", "--port"))]),
        );
    }

    #[test]
    fn test_long_flag_with_equals() {
        assert_parses_to::<SimpleArgs>(
            &["--port=8080"],
            cv::object([("port", cv::string("8080", "--port"))]),
        );
    }

    #[test]
    fn test_short_flag_with_value() {
        assert_parses_to::<SimpleArgs>(
            &["-p", "8080"],
            cv::object([("port", cv::string("8080", "-p"))]),
        );
    }

    #[test]
    fn test_short_flag_attached_value() {
        // -p8080 (no space)
        assert_parses_to::<SimpleArgs>(
            &["-p8080"],
            cv::object([("port", cv::string("8080", "-p"))]),
        );
    }

    #[test]
    fn test_multiple_flags() {
        assert_parses_to::<SimpleArgs>(
            &["--verbose", "--port", "8080"],
            cv::object([
                ("verbose", cv::bool(true, "--verbose")),
                ("port", cv::string("8080", "--port")),
            ]),
        );
    }

    // ========================================================================
    // Tests: Positional arguments
    // ========================================================================

    #[test]
    fn test_single_positional() {
        assert_parses_to::<ArgsWithPositional>(
            &["input.txt"],
            cv::object([("input", cv::string("input.txt", "input.txt"))]),
        );
    }

    #[test]
    fn test_multiple_positionals() {
        assert_parses_to::<ArgsWithPositional>(
            &["input.txt", "output.txt"],
            cv::object([
                ("input", cv::string("input.txt", "input.txt")),
                ("output", cv::string("output.txt", "output.txt")),
            ]),
        );
    }

    #[test]
    fn test_positional_after_double_dash() {
        // -- means everything after is positional
        assert_parses_to::<ArgsWithPositional>(
            &["--", "--input.txt"],
            cv::object([("input", cv::string("--input.txt", "--input.txt"))]),
        );
    }

    // ========================================================================
    // Tests: Config overrides via CLI
    // ========================================================================

    #[test]
    fn test_config_dotted_path() {
        assert_parses_to::<ArgsWithConfig>(
            &["--config.port", "8080"],
            cv::object([(
                "config",
                cv::object([("port", cv::string("8080", "--config.port"))]),
            )]),
        );
    }

    #[test]
    fn test_config_nested_dotted_path() {
        // If config had nested structs: --config.server.host
        assert_parses_to::<ArgsWithConfig>(
            &["--config.host", "localhost"],
            cv::object([(
                "config",
                cv::object([("host", cv::string("localhost", "--config.host"))]),
            )]),
        );
    }

    #[test]
    fn test_args_and_config_together() {
        assert_parses_to::<ArgsWithConfig>(
            &["--verbose", "--config.port", "8080"],
            cv::object([
                ("verbose", cv::bool(true, "--verbose")),
                (
                    "config",
                    cv::object([("port", cv::string("8080", "--config.port"))]),
                ),
            ]),
        );
    }

    // ========================================================================
    // Tests: Config overrides with flattened config
    // ========================================================================

    /// Common configuration shared across services
    #[derive(Facet)]
    struct CommonConfigCli {
        log_level: String,
        debug: bool,
    }

    /// Server config with flattened common settings
    #[derive(Facet)]
    struct ServerConfigWithFlattenCli {
        port: u16,
        #[facet(flatten)]
        common: CommonConfigCli,
    }

    #[derive(Facet)]
    struct ArgsWithFlattenedConfigCli {
        #[facet(args::named)]
        verbose: bool,

        #[facet(args::config)]
        config: ServerConfigWithFlattenCli,
    }

    #[test]
    fn test_config_override_with_flattened_struct() {
        // CLI config overrides use the full path to the target location.
        // Even though the schema has `log_level` as a flattened field,
        // the CLI override uses --config.common.log_level to specify
        // the actual nested path where the value will be stored.
        assert_parses_to::<ArgsWithFlattenedConfigCli>(
            &["--config.common.log_level", "debug"],
            cv::object([(
                "config",
                cv::object([(
                    "common",
                    cv::object([(
                        "log_level",
                        cv::string("debug", "--config.common.log_level"),
                    )]),
                )]),
            )]),
        );
    }

    #[test]
    fn test_config_override_flattened_and_regular_fields() {
        // Mix of regular (port) and flattened (common.debug) fields
        // Note: CLI config overrides parse values as strings; type coercion happens at deserialization
        assert_parses_to::<ArgsWithFlattenedConfigCli>(
            &["--config.port", "8080", "--config.common.debug", "true"],
            cv::object([(
                "config",
                cv::object([
                    ("port", cv::string("8080", "--config.port")),
                    (
                        "common",
                        cv::object([("debug", cv::string("true", "--config.common.debug"))]),
                    ),
                ]),
            )]),
        );
    }

    // ========================================================================
    // Tests: Subcommands
    // ========================================================================

    #[test]
    fn test_subcommand_basic() {
        assert_parses_to::<ArgsWithSubcommand>(
            &["build"],
            // Variant name is PascalCase for facet-format deserialization
            cv::object([("command", cv::enumv("Build", []))]),
        );
    }

    #[test]
    fn test_subcommand_with_args() {
        assert_parses_to::<ArgsWithSubcommand>(
            &["build", "--release"],
            cv::object([(
                "command",
                // Variant name is PascalCase for facet-format deserialization
                cv::enumv("Build", [("release", cv::bool(true, "--release"))]),
            )]),
        );
    }

    #[test]
    fn test_global_flag_before_subcommand() {
        assert_parses_to::<ArgsWithSubcommand>(
            &["--verbose", "build", "--release"],
            cv::object([
                ("verbose", cv::bool(true, "--verbose")),
                (
                    "command",
                    // Variant name is PascalCase for facet-format deserialization
                    cv::enumv("Build", [("release", cv::bool(true, "--release"))]),
                ),
            ]),
        );
    }

    // ========================================================================
    // Tests: Counted flags
    // ========================================================================

    #[test]
    fn test_counted_single() {
        assert_parses_to::<CountedArgs>(&["-v"], cv::object([("verbose", cv::int(1, "-v"))]));
    }

    #[test]
    fn test_counted_chained() {
        assert_parses_to::<CountedArgs>(&["-vvv"], cv::object([("verbose", cv::int(3, "-vvv"))]));
    }

    #[test]
    fn test_counted_repeated() {
        assert_parses_to::<CountedArgs>(
            &["-v", "-v", "-v"],
            cv::object([("verbose", cv::int(3, "-v"))]),
        );
    }

    // ========================================================================
    // Tests: Error cases
    // ========================================================================

    #[test]
    fn test_unknown_long_flag() {
        assert_diagnostic_contains::<SimpleArgs>(&["--unknown"], "unknown");
    }

    #[test]
    fn test_unknown_short_flag() {
        assert_diagnostic_contains::<SimpleArgs>(&["-x"], "unknown");
    }

    #[test]
    fn test_missing_value_for_flag() {
        assert_diagnostic_contains::<SimpleArgs>(&["--port"], "requires a value");
    }

    #[test]
    fn test_triple_dash_flag() {
        assert_diagnostic_contains::<SimpleArgs>(&["---verbose"], "unknown");
    }

    // Note: parser2 does NOT validate types - it parses strings/ints/floats generically.
    // Type validation is the driver's responsibility when emitting values.
    // Note: parser2 does NOT handle -h/--help specially - that's the driver's job.
    // If -h or --help is needed, they should be defined in the schema.

    // ========================================================================
    // Flatten tests
    // ========================================================================

    /// Common arguments that can be flattened into other structs
    #[derive(Facet)]
    struct CommonArgs {
        /// Enable verbose output
        #[facet(args::named, args::short = 'v')]
        verbose: bool,

        /// Enable quiet mode
        #[facet(args::named, args::short = 'q')]
        quiet: bool,
    }

    /// Args struct with flattened common args
    #[derive(Facet)]
    struct ArgsWithFlatten {
        /// Input file
        #[facet(args::positional)]
        input: String,

        /// Common options
        #[facet(flatten)]
        common: CommonArgs,
    }

    #[test]
    fn test_flatten_parses_flags_into_nested_structure() {
        // Parse --verbose and check it ends up nested under "common"
        let expected = cv::object([
            ("input", cv::string("file.txt", "file.txt")),
            ("common", cv::object([("verbose", cv::bool(true, "-v"))])),
        ]);
        assert_parses_to::<ArgsWithFlatten>(&["-v", "file.txt"], expected);
    }

    #[test]
    fn test_flatten_multiple_flags() {
        let expected = cv::object([
            ("input", cv::string("test.txt", "test.txt")),
            (
                "common",
                cv::object([
                    ("verbose", cv::bool(true, "-v")),
                    ("quiet", cv::bool(true, "-q")),
                ]),
            ),
        ]);
        assert_parses_to::<ArgsWithFlatten>(&["-v", "-q", "test.txt"], expected);
    }

    #[test]
    fn test_flatten_long_flags() {
        let expected = cv::object([
            ("input", cv::string("data.txt", "data.txt")),
            (
                "common",
                cv::object([("verbose", cv::bool(true, "--verbose"))]),
            ),
        ]);
        assert_parses_to::<ArgsWithFlatten>(&["--verbose", "data.txt"], expected);
    }

    /// Nested flatten - common inside options inside top-level
    #[derive(Facet)]
    struct NestedCommon {
        /// Enable debug mode
        #[facet(args::named)]
        debug: bool,
    }

    #[derive(Facet)]
    struct MiddleLayer {
        /// Nested common args
        #[facet(flatten)]
        nested: NestedCommon,

        /// Log level
        #[facet(args::named)]
        log_level: Option<String>,
    }

    #[derive(Facet)]
    struct DeepFlatten {
        /// Input path
        #[facet(args::positional)]
        path: String,

        /// Middle layer options
        #[facet(flatten)]
        options: MiddleLayer,
    }

    #[test]
    fn test_deeply_nested_flatten() {
        let expected = cv::object([
            ("path", cv::string("file.txt", "file.txt")),
            (
                "options",
                cv::object([("nested", cv::object([("debug", cv::bool(true, "--debug"))]))]),
            ),
        ]);
        assert_parses_to::<DeepFlatten>(&["--debug", "file.txt"], expected);
    }
}
