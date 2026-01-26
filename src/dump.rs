//! Debug dump utilities for configuration values.
#![allow(dead_code)]

use crate::{
    config_value::ConfigValue,
    provenance::{FilePathStatus, FileResolution, Provenance},
    schema::{ArgSchema, ConfigFieldSchema, ConfigValueSchema, Schema},
};
use owo_colors::OwoColorize;
use std::collections::HashMap;
use std::io::Write;
use unicode_width::UnicodeWidthStr;

/// A line to be printed in the config dump.
#[derive(Debug)]
struct DumpLine {
    indent: usize,
    key: String,
    value: String,
    provenance: String,
    is_header: bool,
}

/// Context for dumping configuration with provenance.
#[derive(Debug)]
struct DumpContext {
    config_field_name: String,
    env_prefix: Option<String>,
    max_string_length: usize,
    max_value_width: usize, // Max width for value column before wrapping
}

/// State tracking for the dump operation.
struct DumpState {
    had_truncation: bool,
}

impl DumpState {
    fn new() -> Self {
        Self {
            had_truncation: false,
        }
    }
}

impl DumpContext {
    /// Extract dump context from the schema.
    fn from_schema(schema: &Schema) -> Self {
        let config = schema.config();
        let config_field_name = config
            .and_then(|c| c.field_name())
            .unwrap_or("settings")
            .to_string();

        let env_prefix = config.and_then(|c| c.env_prefix()).map(|s| s.to_string());

        // Check for FACET_ARGS_BLAST_IT env var to disable truncation
        let blast_it = std::env::var("FACET_ARGS_BLAST_IT")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false);

        Self {
            config_field_name,
            env_prefix,
            max_string_length: if blast_it { usize::MAX } else { 50 },
            max_value_width: 50,
        }
    }
}

/// Dump the ConfigValue tree with provenance information to a writer, using Schema.
/// This version properly handles flattened fields.
pub(crate) fn dump_config_with_schema(
    w: &mut impl Write,
    value: &ConfigValue,
    file_resolution: &FileResolution,
    schema: &Schema,
) {
    // Extract context from schema
    let ctx = DumpContext::from_schema(schema);

    dump_config_impl(w, value, file_resolution, &ctx, |w, value, ctx| {
        write_config_values_with_schema(w, value, schema, ctx)
    });
}

/// Common implementation for config dump.
fn dump_config_impl(
    w: &mut impl Write,
    value: &ConfigValue,
    file_resolution: &FileResolution,
    ctx: &DumpContext,
    write_values: impl FnOnce(&mut dyn Write, &ConfigValue, &DumpContext) -> DumpState,
) {
    // Show sources
    writeln!(w, "Sources:").ok();

    // Config files - show resolution info with alignment
    if !file_resolution.paths.is_empty() {
        writeln!(w, "  file:").ok();

        // Find max path length for alignment
        let max_path_len = file_resolution
            .paths
            .iter()
            .map(|p| p.path.as_str().len())
            .max()
            .unwrap_or(0);

        for path_info in &file_resolution.paths {
            let status_label = match path_info.status {
                FilePathStatus::Picked => "  (picked)",
                FilePathStatus::NotTried => "(not tried)",
                FilePathStatus::Absent => "  (absent)",
            };

            // Calculate dots needed for alignment
            let path_str = path_info.path.as_str();
            let dots_needed = max_path_len.saturating_sub(path_str.len());
            let dots = ".".repeat(dots_needed);

            let suffix = if path_info.explicit {
                " (via --config)"
            } else {
                ""
            };

            // Color the path (purple/magenta for picked, dimmed for others)
            let colored_path = match path_info.status {
                FilePathStatus::Picked => path_str.magenta().to_string(),
                _ => path_str.dimmed().to_string(),
            };

            // Color the status
            let colored_status = match path_info.status {
                FilePathStatus::Picked => status_label.to_string(),
                _ => status_label.dimmed().to_string(),
            };

            writeln!(
                w,
                "    {} {}{} {}",
                colored_status, colored_path, dots, suffix
            )
            .ok();
        }
    } else if file_resolution.had_explicit {
        writeln!(w, "  file: (none - explicit --config not provided)").ok();
    }

    // Environment variables - show actual prefix from shape
    if let Some(ref env_prefix) = ctx.env_prefix {
        writeln!(w, "  env {}", format!("${}__*", env_prefix).yellow()).ok();
    }

    // CLI args - show pattern for config field overrides
    writeln!(
        w,
        "  cli {}",
        format!("--{}.*", ctx.config_field_name).cyan()
    )
    .ok();

    // Defaults
    writeln!(w, "  defaults").ok();

    writeln!(w).ok();

    // Write the config values
    let state = write_values(w, value, ctx);

    // Show truncation notice if any values were truncated
    if state.had_truncation {
        writeln!(w).ok();
        writeln!(
            w,
            "Some values were truncated. To show full values, rerun with {}=1",
            "FACET_ARGS_BLAST_IT".yellow()
        )
        .ok();
    }
}

/// Write collected lines to a writer with proper formatting.
fn write_lines(w: &mut dyn Write, lines: &[DumpLine], ctx: &DumpContext) {
    // Calculate max widths per indent level
    let mut max_key_per_indent: HashMap<usize, usize> = HashMap::new();
    let mut max_val_per_indent: HashMap<usize, usize> = HashMap::new();

    for line in lines {
        if !line.is_header {
            let key_width = visual_width(&line.key);
            let val_width = visual_width(&line.value);

            let key_max = max_key_per_indent.entry(line.indent).or_insert(0);
            *key_max = (*key_max).max(key_width);

            let val_max = max_val_per_indent.entry(line.indent).or_insert(0);
            *val_max = (*val_max).max(val_width);
        }
    }

    // Write all lines with proper alignment
    for line in lines {
        let indent_str = "  ".repeat(line.indent);

        if line.is_header {
            writeln!(w, "{}{}", indent_str, line.key).ok();
        } else {
            let key_width = visual_width(&line.key);
            let val_width = visual_width(&line.value);

            let max_key = max_key_per_indent.get(&line.indent).copied().unwrap_or(0);
            let max_val = max_val_per_indent.get(&line.indent).copied().unwrap_or(0);

            let key_padding = if key_width < max_key {
                ".".repeat(max_key - key_width)
            } else {
                String::new()
            };

            // Check if value needs wrapping
            if val_width > ctx.max_value_width {
                // Wrap value within its column
                let wrapped_lines = wrap_value(&line.value, ctx.max_value_width);

                for (i, wrapped_line) in wrapped_lines.iter().enumerate() {
                    if i == 0 {
                        // First line: show key, dots, value start, and provenance
                        let wrap_width = visual_width(wrapped_line);
                        let val_padding = if wrap_width < ctx.max_value_width {
                            ".".repeat(ctx.max_value_width - wrap_width)
                        } else {
                            String::new()
                        };
                        writeln!(
                            w,
                            "{}{}{}  {}{} {}",
                            indent_str,
                            line.key,
                            key_padding.bright_black(),
                            wrapped_line,
                            val_padding.bright_black(),
                            line.provenance,
                        )
                        .ok();
                    } else {
                        // Continuation lines: indent to value column
                        let continuation_indent = indent_str.len() + max_key + 2;
                        let spaces = " ".repeat(continuation_indent);
                        writeln!(w, "{}{}", spaces, wrapped_line).ok();
                    }
                }
            } else {
                // Normal single-line format with dot padding
                let val_padding = if val_width < max_val.min(ctx.max_value_width) {
                    ".".repeat(max_val.min(ctx.max_value_width) - val_width)
                } else {
                    String::new()
                };

                writeln!(
                    w,
                    "{}{}{}  {}{} {}",
                    indent_str,
                    line.key,
                    key_padding.bright_black(),
                    line.value,
                    val_padding.bright_black(),
                    line.provenance,
                )
                .ok();
            }
        }
    }
}

/// Write just the config values (without the Sources header) to a writer, using Schema.
/// Returns the DumpState for checking if truncation occurred.
fn write_config_values_with_schema(
    w: &mut dyn Write,
    value: &ConfigValue,
    schema: &Schema,
    ctx: &DumpContext,
) -> DumpState {
    // Collect all lines by walking the schema
    let mut lines = Vec::new();
    let mut state = DumpState::new();

    if let ConfigValue::Object(sourced) = value {
        // Dump top-level args from schema (already flattened)
        for (name, arg_schema) in schema.args().args() {
            if let Some(val) = sourced.value.get(name.as_str()) {
                collect_dump_lines_for_arg(val, name, 0, arg_schema, &mut lines, ctx, &mut state);
            } else {
                // Field is missing - check if required
                if arg_schema.required() {
                    let type_name = get_arg_type_name(arg_schema);
                    let colored_value = format!("❌ MISSING <{}>", type_name)
                        .red()
                        .bold()
                        .to_string();
                    lines.push(DumpLine {
                        indent: 0,
                        key: name.to_string(),
                        value: colored_value,
                        provenance: String::new(),
                        is_header: false,
                    });

                    // Show doc comment as help text if available
                    if let Some(summary) = arg_schema.docs().summary() {
                        let help_text = format!("  {}", summary).dimmed().to_string();
                        lines.push(DumpLine {
                            indent: 0,
                            key: String::new(),
                            value: help_text,
                            provenance: String::new(),
                            is_header: false,
                        });
                    }
                } else {
                    // Optional field - show as <default>
                    let colored_value = "<default>".bright_black().to_string();
                    lines.push(DumpLine {
                        indent: 0,
                        key: name.to_string(),
                        value: colored_value,
                        provenance: "DEFAULT".bright_black().to_string(),
                        is_header: false,
                    });
                }
            }
        }

        // Dump subcommand if present
        if let Some(subcommand_field) = schema.args().subcommand_field_name() {
            if let Some(val) = sourced.value.get(subcommand_field) {
                collect_dump_lines_for_subcommand(
                    val,
                    subcommand_field,
                    0,
                    schema,
                    &mut lines,
                    ctx,
                    &mut state,
                );
            } else {
                // Subcommand is missing - show as required
                let colored_value = format!("❌ MISSING <{}>", "Command")
                    .red()
                    .bold()
                    .to_string();
                lines.push(DumpLine {
                    indent: 0,
                    key: subcommand_field.to_string(),
                    value: colored_value,
                    provenance: String::new(),
                    is_header: false,
                });
            }
        }
    }

    write_lines(w, &lines, ctx);
    state
}

/// Get a human-readable type name from an ArgSchema.
fn get_arg_type_name(arg: &ArgSchema) -> String {
    use crate::schema::ValueSchema;
    match arg.value() {
        ValueSchema::Leaf(leaf) => leaf.shape.to_string(),
        ValueSchema::Option { shape, .. } => shape.to_string(),
        ValueSchema::Vec { shape, .. } => shape.to_string(),
        ValueSchema::Struct { shape, .. } => shape.to_string(),
    }
}

/// Collect dump lines for an argument value.
fn collect_dump_lines_for_arg(
    value: &ConfigValue,
    name: &str,
    indent: usize,
    _arg_schema: &ArgSchema,
    lines: &mut Vec<DumpLine>,
    ctx: &DumpContext,
    state: &mut DumpState,
) {
    // For now, just dump the value without deep schema introspection
    collect_dump_lines_simple(value, name, indent, false, lines, ctx, state);
}

/// Collect dump lines for a subcommand (enum variant).
fn collect_dump_lines_for_subcommand(
    value: &ConfigValue,
    name: &str,
    indent: usize,
    _schema: &Schema,
    lines: &mut Vec<DumpLine>,
    ctx: &DumpContext,
    state: &mut DumpState,
) {
    // Dump the subcommand value
    collect_dump_lines_simple(value, name, indent, false, lines, ctx, state);
}

/// Collect dump lines for a config struct field.
fn collect_dump_lines_for_config_field(
    value: &ConfigValue,
    name: &str,
    indent: usize,
    field_schema: &ConfigFieldSchema,
    lines: &mut Vec<DumpLine>,
    ctx: &DumpContext,
    state: &mut DumpState,
) {
    collect_dump_lines_for_config_value(
        value,
        name,
        indent,
        field_schema.value(),
        field_schema.is_sensitive(),
        lines,
        ctx,
        state,
    );
}

/// Collect dump lines for a config value schema.
#[allow(clippy::too_many_arguments)]
fn collect_dump_lines_for_config_value(
    value: &ConfigValue,
    name: &str,
    indent: usize,
    value_schema: &ConfigValueSchema,
    is_sensitive: bool,
    lines: &mut Vec<DumpLine>,
    ctx: &DumpContext,
    state: &mut DumpState,
) {
    match (value, value_schema) {
        (ConfigValue::Object(sourced), ConfigValueSchema::Struct(struct_schema)) => {
            // Add header for the struct
            if !name.is_empty() {
                lines.push(DumpLine {
                    indent,
                    key: name.to_string(),
                    value: String::new(),
                    provenance: String::new(),
                    is_header: true,
                });
            }

            // Iterate over struct fields from schema
            for (field_name, field_schema) in struct_schema.fields() {
                if let Some(field_value) = sourced.value.get(field_name.as_str()) {
                    collect_dump_lines_for_config_field(
                        field_value,
                        field_name,
                        indent + 1,
                        field_schema,
                        lines,
                        ctx,
                        state,
                    );
                } else {
                    // Field is missing - check if it's required based on schema
                    let is_optional =
                        matches!(field_schema.value(), ConfigValueSchema::Option { .. });
                    if is_optional {
                        let colored_value = "<default>".bright_black().to_string();
                        lines.push(DumpLine {
                            indent: indent + 1,
                            key: field_name.to_string(),
                            value: colored_value,
                            provenance: "DEFAULT".bright_black().to_string(),
                            is_header: false,
                        });
                    } else {
                        let type_name = get_config_value_type_name(field_schema.value());
                        let colored_value = format!("❌ MISSING <{}>", type_name)
                            .red()
                            .bold()
                            .to_string();
                        lines.push(DumpLine {
                            indent: indent + 1,
                            key: field_name.to_string(),
                            value: colored_value,
                            provenance: String::new(),
                            is_header: false,
                        });

                        // Show doc comment as help text if available
                        if let Some(summary) = field_schema.docs().summary() {
                            let help_text = format!("  {}", summary).dimmed().to_string();
                            lines.push(DumpLine {
                                indent: indent + 1,
                                key: String::new(),
                                value: help_text,
                                provenance: String::new(),
                                is_header: false,
                            });
                        }
                    }
                }
            }
        }
        (ConfigValue::Array(sourced), ConfigValueSchema::Vec(vec_schema)) => {
            // Add header for array
            lines.push(DumpLine {
                indent,
                key: name.to_string(),
                value: String::new(),
                provenance: String::new(),
                is_header: true,
            });

            for (i, item) in sourced.value.iter().enumerate() {
                collect_dump_lines_for_config_value(
                    item,
                    &format!("[{}]", i),
                    indent + 1,
                    vec_schema.element(),
                    is_sensitive, // Propagate sensitivity through arrays
                    lines,
                    ctx,
                    state,
                );
            }
        }
        (
            value,
            ConfigValueSchema::Option {
                value: inner_schema,
                ..
            },
        ) => {
            // Unwrap the option and recurse
            collect_dump_lines_for_config_value(
                value,
                name,
                indent,
                inner_schema,
                is_sensitive,
                lines,
                ctx,
                state,
            );
        }
        // Leaf values - use simple dump with sensitivity
        _ => {
            collect_dump_lines_simple(value, name, indent, is_sensitive, lines, ctx, state);
        }
    }
}

/// Get a human-readable type name from a ConfigValueSchema.
fn get_config_value_type_name(schema: &ConfigValueSchema) -> String {
    match schema {
        ConfigValueSchema::Struct(s) => s.shape().to_string(),
        ConfigValueSchema::Vec(v) => v.shape().to_string(),
        ConfigValueSchema::Option { value, .. } => {
            format!("Option<{}>", get_config_value_type_name(value))
        }
        ConfigValueSchema::Leaf(leaf) => leaf.shape.to_string(),
    }
}

/// Simple dump without schema introspection - just walks the ConfigValue.
fn collect_dump_lines_simple(
    value: &ConfigValue,
    path: &str,
    indent: usize,
    is_sensitive: bool,
    lines: &mut Vec<DumpLine>,
    ctx: &DumpContext,
    state: &mut DumpState,
) {
    match value {
        ConfigValue::Object(sourced) => {
            // Add header line for this object
            if !path.is_empty() {
                lines.push(DumpLine {
                    indent,
                    key: path.to_string(),
                    value: String::new(),
                    provenance: String::new(),
                    is_header: true,
                });
            }

            // Iterate in insertion order (no schema to guide us)
            for (key, val) in sourced.value.iter() {
                collect_dump_lines_simple(val, key, indent + 1, false, lines, ctx, state);
            }
        }
        ConfigValue::Array(sourced) => {
            // Add header for array
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: String::new(),
                provenance: String::new(),
                is_header: true,
            });

            for (i, item) in sourced.value.iter().enumerate() {
                collect_dump_lines_simple(
                    item,
                    &format!("[{}]", i),
                    indent + 1,
                    false,
                    lines,
                    ctx,
                    state,
                );
            }
        }
        ConfigValue::String(sourced) => {
            let colored_value = if is_sensitive {
                let len = sourced.value.len();
                format!("🔒 [REDACTED ({} bytes)]", len)
                    .bright_magenta()
                    .to_string()
            } else {
                let escaped = sourced.value.replace('\n', "↵");
                let (truncated, was_truncated) = truncate_middle(&escaped, ctx.max_string_length);
                if was_truncated {
                    state.had_truncation = true;
                }
                format!("\"{}\"", truncated).green().to_string()
            };
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: colored_value,
                provenance: format_provenance(&sourced.provenance),
                is_header: false,
            });
        }
        ConfigValue::Integer(sourced) => {
            let colored_value = sourced.value.to_string().blue().to_string();
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: colored_value,
                provenance: format_provenance(&sourced.provenance),
                is_header: false,
            });
        }
        ConfigValue::Float(sourced) => {
            let colored_value = sourced.value.to_string().bright_blue().to_string();
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: colored_value,
                provenance: format_provenance(&sourced.provenance),
                is_header: false,
            });
        }
        ConfigValue::Bool(sourced) => {
            let colored_value = if sourced.value {
                sourced.value.to_string().green().to_string()
            } else {
                sourced.value.to_string().red().to_string()
            };
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: colored_value,
                provenance: format_provenance(&sourced.provenance),
                is_header: false,
            });
        }
        ConfigValue::Null(sourced) => {
            let colored_value = "null".bright_black().to_string();
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: colored_value,
                provenance: format_provenance(&sourced.provenance),
                is_header: false,
            });
        }
        ConfigValue::Enum(sourced) => {
            let variant_display = format!("{}::", sourced.value.variant).cyan().to_string();
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: variant_display,
                provenance: format_provenance(&sourced.provenance),
                is_header: true,
            });

            for (key, val) in sourced.value.fields.iter() {
                collect_dump_lines_simple(val, key, indent + 1, false, lines, ctx, state);
            }
        }
    }
}

/// Calculate visual width of a string after stripping ANSI codes.
fn visual_width(s: &str) -> usize {
    let bytes = s.as_bytes();
    let stripped = strip_ansi_escapes::strip(bytes);
    let stripped_str = core::str::from_utf8(&stripped).unwrap_or(s);
    stripped_str.width()
}

/// Truncate a string in the middle if it exceeds max_length.
/// For example: "this is a very long string" -> "this is a...g string"
/// Returns (truncated_string, was_truncated)
fn truncate_middle(s: &str, max_length: usize) -> (String, bool) {
    if s.len() <= max_length {
        return (s.to_string(), false);
    }

    // Reserve 3 chars for "..."
    if max_length < 3 {
        return ("...".to_string(), true);
    }

    let available = max_length - 3;
    let start_len = available.div_ceil(2); // Round up for start
    let end_len = available / 2;

    let start = s.chars().take(start_len).collect::<String>();
    let end = s
        .chars()
        .rev()
        .take(end_len)
        .collect::<String>()
        .chars()
        .rev()
        .collect::<String>();

    (format!("{}...{}", start, end), true)
}

/// Wrap a value string to fit within max_width, preserving ANSI color codes.
/// Returns a vector of lines with color codes reapplied to each line.
fn wrap_value(value: &str, max_width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_line = String::new();
    let mut current_width = 0;
    let mut in_ansi = false;
    let mut ansi_buffer = String::new();
    let mut active_color = String::new(); // Track the last color code

    for ch in value.chars() {
        if ch == '\x1b' {
            // Start of ANSI escape sequence
            in_ansi = true;
            ansi_buffer.push(ch);
        } else if in_ansi {
            ansi_buffer.push(ch);
            if ch == 'm' {
                // End of ANSI escape sequence
                current_line.push_str(&ansi_buffer);
                active_color = ansi_buffer.clone(); // Save this color
                ansi_buffer.clear();
                in_ansi = false;
            }
        } else {
            // Regular character
            if current_width >= max_width {
                // Need to wrap - close current line and start new one with same color
                lines.push(current_line);
                current_line = String::new();
                if !active_color.is_empty() {
                    current_line.push_str(&active_color); // Reapply color to new line
                }
                current_width = 0;
            }
            current_line.push(ch);
            current_width += 1;
        }
    }

    // Push remaining content
    if !current_line.is_empty() || !ansi_buffer.is_empty() {
        current_line.push_str(&ansi_buffer);
        lines.push(current_line);
    }

    if lines.is_empty() {
        lines.push(String::new());
    }

    lines
}

/// Format provenance with colors.
fn format_provenance(prov: &Option<Provenance>) -> String {
    match prov {
        Some(Provenance::Cli { arg, .. }) => format!("{}", arg.cyan()),
        Some(Provenance::Env { var, .. }) => format!("{}", format!("${}", var).yellow()),
        Some(Provenance::File { file, offset, .. }) => {
            // Calculate line number from byte offset
            let line_num = calculate_line_number(&file.contents, *offset);
            // Extract just filename if full path was shown at start
            let filename = std::path::Path::new(file.path.as_str())
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or(file.path.as_str());
            format!("{}:{}", filename, line_num).magenta().to_string()
        }
        Some(Provenance::Default) => "DEFAULT".bright_black().to_string(),
        None => "".to_string(),
    }
}

/// Calculate line number (1-based) from byte offset in file contents.
fn calculate_line_number(contents: &str, offset: usize) -> usize {
    if offset == 0 {
        return 1;
    }

    // Count newlines before the offset
    let line_count = contents[..offset.min(contents.len())]
        .chars()
        .filter(|&c| c == '\n')
        .count();

    line_count + 1
}
