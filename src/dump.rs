//! Debug dump utilities for configuration values.
#![allow(dead_code)]

use crate::{
    config_value::ConfigValue,
    provenance::{ConfigResult, FilePathStatus, FileResolution, Provenance},
    reflection::{coerce_types_from_shape, find_config_field, get_env_prefix},
};
use facet::Facet;
use owo_colors::OwoColorize;
use std::collections::HashMap;
use std::io::Write;
use unicode_width::UnicodeWidthStr;

/// Dump config with special markers for missing required fields to stdout.
#[deprecated(note = "just use dump_config_with_provenance")]
pub(crate) fn dump_config_with_missing_fields<T: Facet<'static>>(
    value: &ConfigValue,
    file_resolution: &FileResolution,
    _missing_fields: &[MissingFieldInfo],
    _env_prefix: &str,
) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    dump_config_with_provenance::<T>(&mut handle, value, file_resolution);
}

/// Collect missing required fields by walking the schema and checking against the ConfigValue.
/// A field is "missing" if it's not in the ConfigValue AND has no default AND is not Option.
pub(crate) fn collect_missing_fields(
    value: &ConfigValue,
    shape: &'static facet_core::Shape,
    path_prefix: &str,
    missing: &mut Vec<MissingFieldInfo>,
) {
    if let ConfigValue::Object(sourced) = value
        && let facet_core::Type::User(facet_core::UserType::Struct(s)) = &shape.ty
    {
        for field in s.fields {
            let field_shape = field.shape.get();

            // Handle flattened fields: their inner fields are at the current level
            if field.is_flattened() {
                if let facet_core::Type::User(facet_core::UserType::Struct(inner_s)) =
                    &field_shape.ty
                {
                    for inner_field in inner_s.fields {
                        let inner_path = if path_prefix.is_empty() {
                            inner_field.name.to_string()
                        } else {
                            format!("{}.{}", path_prefix, inner_field.name)
                        };
                        let inner_shape = inner_field.shape.get();

                        if let Some(val) = sourced.value.get(inner_field.name) {
                            // Inner field exists at current level - recurse
                            collect_missing_fields(val, inner_shape, &inner_path, missing);
                        } else {
                            // Inner field is missing - check if required
                            let has_default = inner_field.default.is_some();
                            let is_option = inner_shape.type_identifier.contains("Option");

                            if !has_default && !is_option {
                                let doc_comment = if inner_field.doc.is_empty() {
                                    None
                                } else {
                                    Some(inner_field.doc.join("\n"))
                                };
                                missing.push(MissingFieldInfo {
                                    field_name: inner_field.name.to_string(),
                                    field_path: inner_path,
                                    type_name: inner_shape.type_identifier.to_string(),
                                    doc_comment,
                                });
                            }
                        }
                    }
                }
                continue;
            }

            let field_path = if path_prefix.is_empty() {
                field.name.to_string()
            } else {
                format!("{}.{}", path_prefix, field.name)
            };

            if let Some(val) = sourced.value.get(field.name) {
                // Field exists - recurse into nested structs
                collect_missing_fields(val, field_shape, &field_path, missing);
            } else {
                // Field is missing - check if it's required
                let has_default = field.default.is_some();
                let is_option = field_shape.type_identifier.contains("Option");

                if !has_default && !is_option {
                    // Required field without default - it's missing
                    let doc_comment = if field.doc.is_empty() {
                        None
                    } else {
                        Some(field.doc.join("\n"))
                    };
                    missing.push(MissingFieldInfo {
                        field_name: field.name.to_string(),
                        field_path,
                        type_name: field_shape.type_identifier.to_string(),
                        doc_comment,
                    });
                }
            }
        }
    }

    // Also recurse into arrays
    if let ConfigValue::Array(sourced) = value {
        let element_shape = shape.inner.unwrap_or(shape);
        for (i, item) in sourced.value.iter().enumerate() {
            let item_path = if path_prefix.is_empty() {
                format!("[{}]", i)
            } else {
                format!("{}[{}]", path_prefix, i)
            };
            collect_missing_fields(item, element_shape, &item_path, missing);
        }
    }
}

/// Information about a missing required field.
#[derive(Debug, Clone)]
pub struct MissingFieldInfo {
    /// Field name (e.g., "email" or "host")
    pub field_name: String,
    /// Full path (e.g., "config.server.host")
    pub field_path: String,
    /// Type name
    pub type_name: String,
    /// Documentation comment if available
    pub doc_comment: Option<String>,
}

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
    config_field_name: &'static str,
    env_prefix: Option<&'static str>,
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
    /// Extract dump context from the shape.
    fn from_shape<T: Facet<'static>>() -> Self {
        let config_field = find_config_field(T::SHAPE);
        let (config_field_name, env_prefix) = if let Some(field) = config_field {
            (field.name, get_env_prefix(field))
        } else {
            ("settings", None)
        };

        // Check for FACET_ARGS_BLAST_IT env var to disable truncation
        let blast_it = std::env::var("FACET_ARGS_BLAST_IT")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false);

        Self {
            config_field_name,
            env_prefix,
            max_string_length: if blast_it { usize::MAX } else { 50 },
            max_value_width: 50, // Maximum width for value column
        }
    }
}

/// Dump the ConfigValue tree with provenance information from a ConfigResult to stdout.
#[deprecated(note = "just use dump_config_with_provenance")]
pub(crate) fn dump_config_from_result<T: Facet<'static>>(result: &ConfigResult<ConfigValue>) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    dump_config_with_provenance::<T>(&mut handle, &result.value, &result.file_resolution);
}

/// Dump the ConfigValue tree with provenance information to a writer.
pub(crate) fn dump_config_with_provenance<T: Facet<'static>>(
    w: &mut impl Write,
    value: &ConfigValue,
    file_resolution: &FileResolution,
) {
    // Extract context from shape
    let ctx = DumpContext::from_shape::<T>();

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
    if let Some(env_prefix) = ctx.env_prefix {
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
    let state = write_config_values::<T>(w, value);

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

/// Write just the config values (without the Sources header) to a writer.
/// Returns the DumpState for checking if truncation occurred.
fn write_config_values<T: Facet<'static>>(w: &mut impl Write, value: &ConfigValue) -> DumpState {
    let ctx = DumpContext::from_shape::<T>();

    // Step 1: Coerce string values to their target types (for env vars)
    let coerced_value = coerce_types_from_shape(value, T::SHAPE);

    // Step 2: Collect all lines by walking the schema
    let mut lines = Vec::new();
    let mut state = DumpState::new();
    if let ConfigValue::Object(sourced) = &coerced_value
        && let facet_core::Type::User(facet_core::UserType::Struct(s)) = &T::SHAPE.ty
    {
        for field in s.fields {
            let key = field.name;
            let is_sensitive = field.flags.contains(facet_core::FieldFlags::SENSITIVE);
            let field_shape = field.shape.get();

            if let Some(val) = sourced.value.get(key) {
                collect_dump_lines(
                    val,
                    key,
                    0,
                    field_shape,
                    is_sensitive,
                    &mut lines,
                    &ctx,
                    &mut state,
                );
            } else {
                // Field is missing from ConfigValue - check if it has a default
                let has_default = field.default.is_some();
                let is_option = field_shape.type_identifier.contains("Option");

                if has_default || is_option {
                    // Field has a default or is Option - show as <default>
                    let colored_value = "<default>".bright_black().to_string();
                    lines.push(DumpLine {
                        indent: 0,
                        key: key.to_string(),
                        value: colored_value,
                        provenance: "DEFAULT".bright_black().to_string(),
                        is_header: false,
                    });
                } else {
                    // Required field without default - show MISSING marker
                    let colored_value = format!("❌ MISSING <{}>", field_shape.type_identifier)
                        .red()
                        .bold()
                        .to_string();
                    lines.push(DumpLine {
                        indent: 0,
                        key: key.to_string(),
                        value: colored_value,
                        provenance: String::new(),
                        is_header: false,
                    });

                    // Show doc comment as help text if available
                    if !field.doc.is_empty() {
                        let help_text = format!("  {}", field.doc.join(" ")).dimmed().to_string();
                        lines.push(DumpLine {
                            indent: 0,
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

    // Step 3: Calculate max widths per indent level
    let mut max_key_per_indent: HashMap<usize, usize> = HashMap::new();
    let mut max_val_per_indent: HashMap<usize, usize> = HashMap::new();

    for line in &lines {
        if !line.is_header {
            let key_width = visual_width(&line.key);
            let val_width = visual_width(&line.value);

            let key_max = max_key_per_indent.entry(line.indent).or_insert(0);
            *key_max = (*key_max).max(key_width);

            let val_max = max_val_per_indent.entry(line.indent).or_insert(0);
            *val_max = (*val_max).max(val_width);
        }
    }

    // Step 4: Write all lines with proper alignment
    for line in &lines {
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

    state
}

/// Recursively collect lines to be printed.
#[allow(clippy::too_many_arguments)]
fn collect_dump_lines(
    value: &ConfigValue,
    path: &str,
    indent: usize,
    shape: &'static facet_core::Shape,
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

            // Iterate in struct field order
            if let facet_core::Type::User(facet_core::UserType::Struct(s)) = &shape.ty {
                for field in s.fields {
                    let key = field.name;
                    let is_sensitive = field.flags.contains(facet_core::FieldFlags::SENSITIVE);
                    let field_shape = field.shape.get();

                    if let Some(val) = sourced.value.get(key) {
                        collect_dump_lines(
                            val,
                            key,
                            indent + 1,
                            field_shape,
                            is_sensitive,
                            lines,
                            ctx,
                            state,
                        );
                    } else {
                        // Field is missing from ConfigValue - check if it has a default
                        let has_default = field.default.is_some();
                        let is_option = field_shape.type_identifier.contains("Option");

                        if has_default || is_option {
                            // Field has a default or is Option - show as <default>
                            let colored_value = "<default>".bright_black().to_string();
                            lines.push(DumpLine {
                                indent: indent + 1,
                                key: key.to_string(),
                                value: colored_value,
                                provenance: "DEFAULT".bright_black().to_string(),
                                is_header: false,
                            });
                        } else {
                            // Required field without default - show MISSING marker
                            let colored_value =
                                format!("❌ MISSING <{}>", field_shape.type_identifier)
                                    .red()
                                    .bold()
                                    .to_string();
                            lines.push(DumpLine {
                                indent: indent + 1,
                                key: key.to_string(),
                                value: colored_value,
                                provenance: String::new(),
                                is_header: false,
                            });

                            // Show doc comment as help text if available
                            if !field.doc.is_empty() {
                                let help_text =
                                    format!("  {}", field.doc.join(" ")).dimmed().to_string();
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
            } else {
                // Fallback: iterate in insertion order
                for (key, val) in sourced.value.iter() {
                    collect_dump_lines(val, key, indent + 1, shape, false, lines, ctx, state);
                }
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
                let element_shape = shape.inner.unwrap_or(shape);
                collect_dump_lines(
                    item,
                    &format!("[{}]", i),
                    indent + 1,
                    element_shape,
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
                // Replace newlines with visual indicator
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
            // Add header line showing variant name
            let variant_display = format!("{}::", sourced.value.variant).cyan().to_string();
            lines.push(DumpLine {
                indent,
                key: path.to_string(),
                value: variant_display,
                provenance: format_provenance(&sourced.provenance),
                is_header: true,
            });

            // Dump the enum's fields
            for (key, val) in sourced.value.fields.iter() {
                collect_dump_lines(val, key, indent + 1, shape, false, lines, ctx, state);
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
