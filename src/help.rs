//! Help text generation for command-line interfaces.
//!
//! This module provides utilities to generate help text from Facet type metadata,
//! including doc comments, field names, and attribute information.

use facet_core::{Def, Facet, Field, Shape, StructKind, Type, UserType, Variant};
use heck::ToKebabCase;
use owo_colors::OwoColorize;
use std::string::String;
use std::vec::Vec;

use crate::reflection::{is_counted_field, is_supported_counted_type};

/// Configuration for help text generation.
#[derive(Debug, Clone)]
pub struct HelpConfig {
    /// Program name (defaults to executable name)
    pub program_name: Option<String>,
    /// Program version
    pub version: Option<String>,
    /// Additional description to show after the auto-generated one
    pub description: Option<String>,
    /// Width for wrapping text (0 = no wrapping)
    pub width: usize,
}

impl Default for HelpConfig {
    fn default() -> Self {
        Self {
            program_name: None,
            version: None,
            description: None,
            width: 80,
        }
    }
}

/// Generate help text for a Facet type.
pub fn generate_help<T: facet_core::Facet<'static>>(config: &HelpConfig) -> String {
    generate_help_for_shape(T::SHAPE, config)
}

/// Generate help text for a specific subcommand path.
///
/// `subcommand_path` is a list of variant names (e.g., `["Repo", "Clone"]` for `myapp repo clone --help`).
/// This navigates through the type's shape to find the target subcommand and generates help for it.
pub fn generate_help_for_subcommand(
    shape: &'static Shape,
    subcommand_path: &[String],
    config: &HelpConfig,
) -> String {
    if subcommand_path.is_empty() {
        return generate_help_for_shape(shape, config);
    }

    // Navigate to the subcommand variant
    if let Some(variant) = find_subcommand_variant(shape, subcommand_path) {
        generate_help_for_variant(&variant, config)
    } else {
        // Fallback to root help if we can't find the subcommand
        generate_help_for_shape(shape, config)
    }
}

/// Generate help for a specific subcommand variant.
fn generate_help_for_variant(variant: &FoundVariant, config: &HelpConfig) -> String {
    let mut out = String::new();

    // Program name
    let program_name = config
        .program_name
        .clone()
        .or_else(|| std::env::args().next())
        .unwrap_or_else(|| "program".to_string());

    // Full command (e.g., "myapp repo clone")
    let full_command = if variant.command_path.is_empty() {
        program_name.clone()
    } else {
        format!("{} {}", program_name, variant.command_path.join(" "))
    };

    // Header with full command
    out.push_str(&format!("{full_command}\n"));

    // Doc comment for the variant/subcommand
    if !variant.doc.is_empty() {
        out.push('\n');
        for line in variant.doc {
            out.push_str(line.trim());
            out.push('\n');
        }
    }

    out.push('\n');

    // Generate based on the variant's fields
    // Collect flags, positionals, and nested subcommand from variant fields
    let mut flags: Vec<&Field> = Vec::new();
    let mut positionals: Vec<&Field> = Vec::new();
    let mut subcommand: Option<&Field> = None;

    collect_fields_recursive(
        variant.fields,
        &mut flags,
        &mut positionals,
        &mut subcommand,
    );

    generate_struct_help_inner(&mut out, &full_command, flags, positionals, subcommand);

    out
}

/// Information about a found subcommand variant.
struct FoundVariant {
    /// The variant's fields (from data.fields)
    fields: &'static [Field],
    /// The variant's doc comment
    doc: &'static [&'static str],
    /// The command path for display (kebab-case)
    command_path: Vec<String>,
}

/// Find a subcommand variant by navigating through the type hierarchy.
/// Returns variant info for generating help.
fn find_subcommand_variant(root_shape: &'static Shape, path: &[String]) -> Option<FoundVariant> {
    if path.is_empty() {
        return None;
    }

    let mut current_fields: &'static [Field] = match &root_shape.ty {
        Type::User(UserType::Struct(st)) => st.fields,
        _ => return None,
    };
    let mut command_path = Vec::new();
    let mut last_variant: Option<&'static Variant> = None;

    for variant_name in path {
        // Find the subcommand field in current fields
        let subcommand_field = current_fields
            .iter()
            .find(|f| f.has_attr(Some("args"), "subcommand"));

        let Some(field) = subcommand_field else {
            // No subcommand field - check if we already have a variant with nested subcommand
            if let Some(v) = last_variant {
                // Check the variant's fields for a subcommand
                let nested_sub = v
                    .data
                    .fields
                    .iter()
                    .find(|f| f.has_attr(Some("args"), "subcommand"));
                if let Some(nested_field) = nested_sub {
                    // Get the enum shape
                    let enum_shape = match nested_field.shape().def {
                        Def::Option(opt) => opt.t,
                        _ => nested_field.shape(),
                    };

                    // Find the variant
                    if let Type::User(UserType::Enum(e)) = &enum_shape.ty
                        && let Some(v2) = e.variants.iter().find(|v| v.name == variant_name)
                    {
                        command_path.push(v2.name.to_kebab_case());
                        last_variant = Some(v2);
                        current_fields = v2.data.fields;
                        continue;
                    }
                }
            }
            return None;
        };

        // Get the enum shape (handling Option<Enum>)
        let enum_shape = match field.shape().def {
            Def::Option(opt) => opt.t,
            _ => field.shape(),
        };

        // Find the variant in the enum
        // The variant_name comes from CLI (kebab-case or renamed), so we need to match
        // using effective_name() which handles renames
        let variant = match &enum_shape.ty {
            Type::User(UserType::Enum(e)) => {
                trace!(
                    "enum has variants: {:?}, looking for {:?}",
                    e.variants
                        .iter()
                        .map(|v| v.effective_name())
                        .collect::<Vec<_>>(),
                    variant_name
                );
                e.variants
                    .iter()
                    .find(|v| v.effective_name() == *variant_name)
            }
            _ => {
                trace!("not an enum type");
                None
            }
        };

        trace!("found variant = {:?}", variant.map(|v| v.name));

        let Some(v) = variant else {
            debug!("variant {:?} not found, returning None", variant_name);
            return None;
        };

        // Add to command path (kebab-case for display)
        command_path.push(v.name.to_kebab_case());

        // Update for next iteration
        last_variant = Some(v);
        current_fields = v.data.fields;
    }

    last_variant.map(|v| FoundVariant {
        fields: v.data.fields,
        doc: v.doc,
        command_path,
    })
}
/// Generate help text for a shape.
pub fn generate_help_for_shape(shape: &'static Shape, config: &HelpConfig) -> String {
    let mut out = String::new();

    // Program name and version
    let program_name = config
        .program_name
        .clone()
        .or_else(|| std::env::args().next())
        .unwrap_or_else(|| "program".to_string());

    if let Some(version) = &config.version {
        out.push_str(&format!("{program_name} {version}\n"));
    } else {
        out.push_str(&format!("{program_name}\n"));
    }

    // Type doc comment
    if !shape.doc.is_empty() {
        out.push('\n');
        for line in shape.doc {
            out.push_str(line.trim());
            out.push('\n');
        }
    }

    // Additional description
    if let Some(desc) = &config.description {
        out.push('\n');
        out.push_str(desc);
        out.push('\n');
    }

    out.push('\n');

    // Generate based on type
    match &shape.ty {
        Type::User(UserType::Struct(struct_type)) => {
            generate_struct_help(&mut out, &program_name, struct_type.fields);
        }
        Type::User(UserType::Enum(enum_type)) => {
            generate_enum_help(&mut out, &program_name, enum_type.variants);
        }
        _ => {
            out.push_str("(No help available for this type)\n");
        }
    }

    out
}

fn generate_struct_help(out: &mut String, program_name: &str, fields: &'static [Field]) {
    // Collect flags, positionals, and subcommand
    let mut flags: Vec<&Field> = Vec::new();
    let mut positionals: Vec<&Field> = Vec::new();
    let mut subcommand: Option<&Field> = None;

    // Recursively collect fields, handling flatten
    collect_fields_recursive(fields, &mut flags, &mut positionals, &mut subcommand);

    // Generate the help output
    generate_struct_help_inner(out, program_name, flags, positionals, subcommand);
}

fn collect_fields_recursive<'a>(
    fields: &'static [Field],
    flags: &mut Vec<&'a Field>,
    positionals: &mut Vec<&'a Field>,
    subcommand: &mut Option<&'a Field>,
) where
    'static: 'a,
{
    for field in fields {
        // Handle flattened fields - recurse into the inner struct
        if field.is_flattened() {
            if let Type::User(UserType::Struct(inner)) = &field.shape().ty {
                collect_fields_recursive(inner.fields, flags, positionals, subcommand);
            }
            continue;
        }

        if field.has_attr(Some("args"), "subcommand") {
            *subcommand = Some(field);
        } else if field.has_attr(Some("args"), "positional") {
            positionals.push(field);
        } else {
            flags.push(field);
        }
    }
}

fn generate_struct_help_inner(
    out: &mut String,
    program_name: &str,
    flags: Vec<&Field>,
    positionals: Vec<&Field>,
    subcommand: Option<&Field>,
) {
    // Usage line
    out.push_str(&format!("{}:\n    ", "USAGE".yellow().bold()));
    out.push_str(program_name);

    if !flags.is_empty() {
        out.push_str(" [OPTIONS]");
    }

    for pos in &positionals {
        let name = pos.name.to_kebab_case().to_uppercase();
        let is_optional = matches!(pos.shape().def, Def::Option(_)) || pos.has_default();
        if is_optional {
            out.push_str(&format!(" [{name}]"));
        } else {
            out.push_str(&format!(" <{name}>"));
        }
    }

    if let Some(sub) = subcommand {
        let is_optional = matches!(sub.shape().def, Def::Option(_));
        if is_optional {
            out.push_str(" [COMMAND]");
        } else {
            out.push_str(" <COMMAND>");
        }
    }

    out.push_str("\n\n");

    // Positional arguments
    if !positionals.is_empty() {
        out.push_str(&format!("{}:\n", "ARGUMENTS".yellow().bold()));
        for field in &positionals {
            write_field_help(out, field, true);
        }
        out.push('\n');
    }

    // Options
    if !flags.is_empty() {
        out.push_str(&format!("{}:\n", "OPTIONS".yellow().bold()));
        for field in &flags {
            write_field_help(out, field, false);
        }
        out.push('\n');
    }

    // Subcommands
    if let Some(sub_field) = subcommand {
        let sub_shape = sub_field.shape();
        // Handle Option<Enum> or direct Enum
        let enum_shape = if let Def::Option(opt) = sub_shape.def {
            opt.t
        } else {
            sub_shape
        };

        if let Type::User(UserType::Enum(enum_type)) = enum_shape.ty {
            out.push_str(&format!("{}:\n", "COMMANDS".yellow().bold()));
            for variant in enum_type.variants {
                write_variant_help(out, variant);
            }
            out.push('\n');
        }
    }
}

fn generate_enum_help(out: &mut String, program_name: &str, variants: &'static [Variant]) {
    // For top-level enum, show subcommands
    out.push_str(&format!("{}:\n    ", "USAGE".yellow().bold()));
    out.push_str(program_name);
    out.push_str(" <COMMAND>\n\n");

    out.push_str(&format!("{}:\n", "COMMANDS".yellow().bold()));
    for variant in variants {
        write_variant_help(out, variant);
    }
    out.push('\n');
}

fn write_field_help(out: &mut String, field: &Field, is_positional: bool) {
    out.push_str("    ");

    // Short flag
    let short = get_short_flag(field);
    if let Some(c) = short {
        out.push_str(&format!("{}, ", format!("-{c}").green()));
    } else {
        out.push_str("    ");
    }

    // Long flag or positional name
    let kebab_name = field.name.to_kebab_case();
    let is_counted = is_counted_field(field) && is_supported_counted_type(field.shape());

    if is_positional {
        out.push_str(&format!(
            "{}",
            format!("<{}>", kebab_name.to_uppercase()).green()
        ));
    } else {
        out.push_str(&format!("{}", format!("--{kebab_name}").green()));

        // Show value placeholder for non-bool, non-counted types
        let shape = field.shape();
        if !is_counted && !shape.is_shape(bool::SHAPE) {
            out.push_str(&format!(" <{}>", shape.type_identifier.to_uppercase()));
        }
    }

    // Doc comment
    if let Some(doc) = field.doc.first() {
        out.push_str("\n            ");
        out.push_str(doc.trim());
    }

    if is_counted {
        out.push_str("\n            ");
        out.push_str("[can be repeated]");
    }

    out.push('\n');
}

fn write_variant_help(out: &mut String, variant: &Variant) {
    out.push_str("    ");

    // Variant name (check for rename)
    let name = variant
        .get_builtin_attr("rename")
        .and_then(|attr| attr.get_as::<&str>())
        .map(|s| (*s).to_string())
        .unwrap_or_else(|| variant.name.to_kebab_case());

    out.push_str(&format!("{}", name.green()));

    // Doc comment
    if let Some(doc) = variant.doc.first() {
        out.push_str("\n            ");
        out.push_str(doc.trim());
    }

    out.push('\n');
}

/// Get the short flag character for a field, if any
fn get_short_flag(field: &Field) -> Option<char> {
    field
        .get_attr(Some("args"), "short")
        .and_then(|attr| attr.get_as::<crate::Attr>())
        .and_then(|attr| {
            if let crate::Attr::Short(c) = attr {
                // If explicit char provided, use it; otherwise use first char of field name
                c.or_else(|| field.name.chars().next())
            } else {
                None
            }
        })
}

/// Generate help for a specific subcommand variant.
#[allow(dead_code)]
pub fn generate_subcommand_help(
    variant: &'static Variant,
    parent_program: &str,
    config: &HelpConfig,
) -> String {
    let mut out = String::new();

    let variant_name = variant
        .get_builtin_attr("rename")
        .and_then(|attr| attr.get_as::<&str>())
        .map(|s| (*s).to_string())
        .unwrap_or_else(|| variant.name.to_kebab_case());

    let full_name = format!("{parent_program} {variant_name}");

    // Header
    if let Some(version) = &config.version {
        out.push_str(&format!("{full_name} {version}\n"));
    } else {
        out.push_str(&format!("{full_name}\n"));
    }

    // Variant doc comment
    if !variant.doc.is_empty() {
        out.push('\n');
        for line in variant.doc {
            out.push_str(line.trim());
            out.push('\n');
        }
    }

    out.push('\n');

    // Generate help for variant fields
    // Handle tuple variant with single struct field (newtype pattern)
    // e.g., `Build(BuildArgs)` should flatten BuildArgs fields
    // This matches clap's behavior: "automatically flattened with a tuple-variant"
    let fields = variant.data.fields;
    if variant.data.kind == StructKind::TupleStruct && fields.len() == 1 {
        let inner_shape = fields[0].shape();
        if let Type::User(UserType::Struct(struct_type)) = inner_shape.ty {
            // Use the inner struct's fields instead of the tuple field
            generate_struct_help(&mut out, &full_name, struct_type.fields);
            return out;
        }
    }

    generate_struct_help(&mut out, &full_name, fields);

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use facet::Facet;

    /// Common arguments that can be flattened into other structs
    #[derive(Facet)]
    struct CommonArgs {
        /// Enable verbose output
        #[facet(crate::named, crate::short = 'v')]
        verbose: bool,

        /// Enable quiet mode
        #[facet(crate::named, crate::short = 'q')]
        quiet: bool,
    }

    /// Args struct with flattened common args
    #[derive(Facet)]
    struct ArgsWithFlatten {
        /// Input file
        #[facet(crate::positional)]
        input: String,

        /// Common options
        #[facet(flatten)]
        common: CommonArgs,
    }

    #[test]
    fn test_flatten_args_appear_in_help() {
        let help = generate_help::<ArgsWithFlatten>(&HelpConfig::default());

        // Flattened fields should appear at top level
        assert!(
            help.contains("--verbose"),
            "help should contain --verbose from flattened CommonArgs"
        );
        assert!(help.contains("-v"), "help should contain -v short flag");
        assert!(
            help.contains("--quiet"),
            "help should contain --quiet from flattened CommonArgs"
        );
        assert!(help.contains("-q"), "help should contain -q short flag");

        // The flattened field name 'common' should NOT appear as a flag
        assert!(
            !help.contains("--common"),
            "help should not show --common as a flag"
        );
    }

    #[test]
    fn test_flatten_docs_preserved() {
        let help = generate_help::<ArgsWithFlatten>(&HelpConfig::default());

        // Doc comments from flattened fields should be present
        assert!(
            help.contains("verbose output"),
            "help should contain verbose field doc"
        );
        assert!(
            help.contains("quiet mode"),
            "help should contain quiet field doc"
        );
    }
}
