//! Schema-aware value builder for constructing ConfigValue trees.
//!
//! `ValueBuilder` is the single source of truth for building `ConfigValue` trees
//! with full schema awareness. Layer parsers (CLI, env, file) use this builder
//! instead of directly manipulating `ConfigValue` objects.
//!
//! The builder:
//! - Validates paths exist in the schema
//! - Tracks enum variant selection and detects conflicts
//! - Coerces values to the expected type
//! - Wraps values with provenance and span information
//! - Tracks unused keys

use std::hash::RandomState;

use facet_reflect::Span;
use indexmap::IndexMap;

use crate::config_value::{ConfigValue, ObjectMap, Sourced};
use crate::driver::{Diagnostic, LayerOutput, Severity, UnusedKey};
use crate::path::Path;
use crate::provenance::Provenance;
use crate::schema::{
    ConfigEnumSchema, ConfigEnumVariantSchema, ConfigStructSchema, ConfigValueSchema,
};

/// A leaf value that can be set via the builder.
///
/// These are the primitive values that layer parsers work with.
/// The builder handles converting these to `ConfigValue` with proper
/// provenance and type coercion.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum LeafValue {
    String(String),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Null,
    /// Array of leaf values (for comma-separated env vars).
    /// Each element shares the same provenance as the array.
    StringArray(Vec<String>),
}

impl LeafValue {
    /// Convert to ConfigValue with the given span and provenance.
    fn into_config_value(self, span: Option<Span>, provenance: Provenance) -> ConfigValue {
        let prov = Some(provenance);
        match self {
            LeafValue::String(s) => ConfigValue::String(Sourced {
                value: s,
                span,
                provenance: prov,
            }),
            LeafValue::Bool(b) => ConfigValue::Bool(Sourced {
                value: b,
                span,
                provenance: prov,
            }),
            LeafValue::Integer(i) => ConfigValue::Integer(Sourced {
                value: i,
                span,
                provenance: prov,
            }),
            LeafValue::Float(f) => ConfigValue::Float(Sourced {
                value: f,
                span,
                provenance: prov,
            }),
            LeafValue::Null => ConfigValue::Null(Sourced {
                value: (),
                span,
                provenance: prov,
            }),
            LeafValue::StringArray(strings) => {
                let elements: Vec<ConfigValue> = strings
                    .into_iter()
                    .map(|s| {
                        ConfigValue::String(Sourced {
                            value: s,
                            span,
                            provenance: prov.clone(),
                        })
                    })
                    .collect();
                ConfigValue::Array(Sourced {
                    value: elements,
                    span,
                    provenance: prov,
                })
            }
        }
    }
}

/// A schema-aware builder for constructing ConfigValue trees.
///
/// Layer parsers create a ValueBuilder, call `set()` for each path/value pair,
/// and then call `into_output()` to get the final LayerOutput.
pub struct ValueBuilder<'a> {
    /// The config struct schema to validate against.
    schema: &'a ConfigStructSchema,

    /// The ConfigValue being built (always an object at the root).
    root: ObjectMap,

    /// Tracks which enum variant has been selected at each enum path.
    /// Key is the path to the enum field (e.g., ["storage"]).
    /// Value is (variant_name, provenance of first field set).
    enum_variants: IndexMap<Path, (String, Provenance), RandomState>,

    /// Unused keys (paths that don't match the schema).
    unused_keys: Vec<UnusedKey>,

    /// Diagnostics collected during building.
    diagnostics: Vec<Diagnostic>,
}

impl<'a> ValueBuilder<'a> {
    /// Create a new value builder.
    ///
    /// # Arguments
    /// * `schema` - The config struct schema to validate against
    pub fn new(schema: &'a ConfigStructSchema) -> Self {
        Self {
            schema,
            root: IndexMap::default(),
            enum_variants: IndexMap::default(),
            unused_keys: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Set a leaf value at the given path.
    ///
    /// The path is validated against the schema. If the path navigates through
    /// an enum field, the variant is tracked for conflict detection.
    ///
    /// For arrays, use numeric indices in the path: `["ports", "0"]`
    ///
    /// # Arguments
    /// * `path` - The path to set (e.g., `["storage", "s3", "bucket"]`)
    /// * `value` - The leaf value to set
    /// * `span` - Optional source span for error reporting
    /// * `provenance` - Where this value came from (required for all values)
    ///
    /// Returns `true` if the path was valid and the value was set.
    pub fn set(
        &mut self,
        path: &Path,
        value: LeafValue,
        span: Option<Span>,
        provenance: Provenance,
    ) -> bool {
        if path.is_empty() {
            return false;
        }

        // Resolve the path against the schema
        let resolved = match self.resolve_path(path) {
            Some(r) => r,
            None => {
                self.unused_keys.push(UnusedKey {
                    key: path.clone(),
                    provenance: provenance.clone(),
                });
                return false;
            }
        };

        // Check for enum variant conflicts
        for selection in &resolved.enum_selections {
            if !self.check_enum_variant_conflict(
                &selection.enum_path,
                &selection.variant_name,
                &provenance,
            ) {
                return false;
            }
        }

        // Validate enum string values (for unit enums like LogLevel)
        if let LeafValue::String(ref s) = value {
            self.validate_enum_string_value(path, s);
        }

        // Convert leaf value to ConfigValue with provenance
        let config_value = value.into_config_value(span, provenance);

        // Insert at the resolved path
        self.insert_at_path(&resolved.insertion_path, config_value);
        true
    }

    /// Validate that a string value at an enum path is a valid variant name.
    fn validate_enum_string_value(&mut self, path: &[String], value: &str) {
        // Get the schema for this path
        let Some(value_schema) = self.schema.get_by_path(&path.to_vec()) else {
            return;
        };

        // Unwrap Option wrapper if present
        let inner_schema = match value_schema {
            ConfigValueSchema::Option { value: inner, .. } => inner.as_ref(),
            other => other,
        };

        // For enum fields, validate the value is a known variant
        if let ConfigValueSchema::Enum(enum_schema) = inner_schema {
            let variants = enum_schema.variants();
            if !variants.contains_key(value) {
                let valid_variants: Vec<&String> = variants.keys().collect();
                self.warn(format!(
                    "unknown variant '{}' for {}. Valid variants are: {}",
                    value,
                    path.join("."),
                    valid_variants
                        .iter()
                        .map(|v| format!("'{}'", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
        }
    }

    /// Check if a value exists at the given path.
    #[allow(dead_code)]
    pub fn has_value_at(&self, path: &Path) -> bool {
        if path.is_empty() {
            return false;
        }

        let mut current = &self.root;
        for (i, segment) in path.iter().enumerate() {
            match current.get(segment) {
                Some(ConfigValue::Object(obj)) if i < path.len() - 1 => {
                    current = &obj.value;
                }
                Some(ConfigValue::Array(arr)) if i < path.len() - 1 => {
                    // Next segment should be a numeric index
                    if let Some(next_segment) = path.get(i + 1)
                        && let Ok(idx) = next_segment.parse::<usize>()
                        && let Some(ConfigValue::Object(obj)) = arr.value.get(idx)
                    {
                        current = &obj.value;
                        continue;
                    }
                    return false;
                }
                Some(_) if i == path.len() - 1 => {
                    return true;
                }
                _ => return false,
            }
        }
        false
    }

    /// Emit a warning diagnostic.
    pub fn warn(&mut self, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            message: message.into(),
            path: None,
            span: None,
            severity: Severity::Warning,
        });
    }

    /// Emit an error diagnostic.
    pub fn error(&mut self, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            message: message.into(),
            path: None,
            span: None,
            severity: Severity::Error,
        });
    }

    /// Consume the builder and return the built ConfigValue wrapped in a LayerOutput.
    ///
    /// The `field_name` parameter is the name of the config field in the parent struct
    /// (e.g., "config" or "settings"). If provided, the result will be wrapped as
    /// `{field_name: {...}}`.
    pub fn into_output(self, field_name: Option<&str>) -> LayerOutput {
        let value = if self.root.is_empty() {
            None
        } else if let Some(name) = field_name {
            // Wrap under the config field name
            let mut root = IndexMap::default();
            root.insert(
                name.to_string(),
                ConfigValue::Object(Sourced::new(self.root)),
            );
            Some(ConfigValue::Object(Sourced::new(root)))
        } else {
            Some(ConfigValue::Object(Sourced::new(self.root)))
        };

        LayerOutput {
            value,
            unused_keys: self.unused_keys,
            diagnostics: self.diagnostics,
        }
    }

    /// Get access to the schema.
    #[allow(dead_code)]
    pub fn schema(&self) -> &'a ConfigStructSchema {
        self.schema
    }

    /// Get access to unused keys collected so far.
    pub fn unused_keys(&self) -> &[UnusedKey] {
        &self.unused_keys
    }

    /// Get access to diagnostics collected so far.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    // ========================================================================
    // CLI-specific methods
    // ========================================================================

    /// Increment a counted flag at the given path.
    ///
    /// Used for CLI flags like `-v -v -v` that accumulate a count.
    /// If the path doesn't exist yet, initializes to 1. Otherwise increments.
    ///
    /// Returns `true` if the path was valid.
    pub fn increment(
        &mut self,
        path: &Path,
        span: Option<Span>,
        provenance: Provenance,
    ) -> bool {
        if path.is_empty() {
            return false;
        }

        // Resolve the path against the schema
        let resolved = match self.resolve_path(path) {
            Some(r) => r,
            None => {
                self.unused_keys.push(UnusedKey {
                    key: path.clone(),
                    provenance: provenance.clone(),
                });
                return false;
            }
        };

        // Check for enum variant conflicts
        for selection in &resolved.enum_selections {
            if !self.check_enum_variant_conflict(
                &selection.enum_path,
                &selection.variant_name,
                &provenance,
            ) {
                return false;
            }
        }

        // Get or create the integer at this path
        let current = self.get_integer_at_path(&resolved.insertion_path);
        let new_value = current + 1;

        let config_value = ConfigValue::Integer(Sourced {
            value: new_value,
            span,
            provenance: Some(provenance),
        });

        self.insert_at_path(&resolved.insertion_path, config_value);
        true
    }

    /// Append a value to an array at the given path.
    ///
    /// Used for CLI flags that can be repeated: `--tag foo --tag bar`.
    /// If the path doesn't exist, creates an array with one element.
    /// If the path has a scalar, converts to array with both values.
    /// If the path has an array, appends to it.
    ///
    /// Returns `true` if the path was valid.
    pub fn append(
        &mut self,
        path: &Path,
        value: LeafValue,
        span: Option<Span>,
        provenance: Provenance,
    ) -> bool {
        if path.is_empty() {
            return false;
        }

        // Resolve the path against the schema
        let resolved = match self.resolve_path(path) {
            Some(r) => r,
            None => {
                self.unused_keys.push(UnusedKey {
                    key: path.clone(),
                    provenance: provenance.clone(),
                });
                return false;
            }
        };

        // Check for enum variant conflicts
        for selection in &resolved.enum_selections {
            if !self.check_enum_variant_conflict(
                &selection.enum_path,
                &selection.variant_name,
                &provenance,
            ) {
                return false;
            }
        }

        // Convert leaf value to ConfigValue
        let config_value = value.into_config_value(span, provenance);

        // Append at the resolved path
        self.append_at_path(&resolved.insertion_path, config_value);
        true
    }

    /// Select an enum variant at the given path.
    ///
    /// Used for CLI subcommands: `myapp serve --port 8080` selects the "serve" variant.
    /// Returns the path to use for setting fields within this variant.
    ///
    /// This records the variant selection for conflict detection but doesn't
    /// insert any value yet - the caller should use `set()` to set fields.
    ///
    /// Returns `Some(variant_path)` if the selection was valid (variant_path includes
    /// the variant name), or `None` if the enum path was invalid or there's a conflict.
    pub fn select_variant(
        &mut self,
        enum_path: &Path,
        variant_name: &str,
        provenance: &Provenance,
    ) -> Option<Path> {
        if enum_path.is_empty() {
            return None;
        }

        // Resolve the enum path (without the variant)
        let resolved = match self.resolve_path(enum_path) {
            Some(r) => r,
            None => return None,
        };

        // Check for enum variant conflicts using the enum_path
        if !self.check_enum_variant_conflict(enum_path, variant_name, provenance) {
            return None;
        }

        // Build the full path including the variant name
        let mut variant_path = resolved.insertion_path;
        variant_path.push(variant_name.to_string());

        Some(variant_path)
    }

    /// Set an enum value directly (for subcommands with their parsed fields).
    ///
    /// This is used when the CLI parser has already collected all fields for
    /// a subcommand variant and wants to set the complete enum value.
    ///
    /// Returns `true` if the path was valid and no conflict occurred.
    pub fn set_enum(
        &mut self,
        path: &Path,
        variant_name: String,
        fields: IndexMap<String, ConfigValue, RandomState>,
        span: Option<Span>,
        provenance: Provenance,
    ) -> bool {
        if path.is_empty() {
            return false;
        }

        // Resolve the path against the schema
        let resolved = match self.resolve_path(path) {
            Some(r) => r,
            None => {
                self.unused_keys.push(UnusedKey {
                    key: path.clone(),
                    provenance: provenance.clone(),
                });
                return false;
            }
        };

        // Check for enum variant conflicts at this path
        if !self.check_enum_variant_conflict(&resolved.insertion_path, &variant_name, &provenance) {
            return false;
        }

        // Create the enum value
        let config_value = ConfigValue::Enum(Sourced {
            value: crate::config_value::EnumValue {
                variant: variant_name,
                fields,
            },
            span,
            provenance: Some(provenance),
        });

        self.insert_at_path(&resolved.insertion_path, config_value);
        true
    }

    /// Get the current integer value at a path, or 0 if not set.
    fn get_integer_at_path(&self, path: &[String]) -> i64 {
        if path.is_empty() {
            return 0;
        }

        let mut current = &self.root;
        for (i, segment) in path.iter().enumerate() {
            match current.get(segment) {
                Some(ConfigValue::Object(obj)) if i < path.len() - 1 => {
                    current = &obj.value;
                }
                Some(ConfigValue::Integer(int)) if i == path.len() - 1 => {
                    return int.value;
                }
                _ => return 0,
            }
        }
        0
    }

    /// Append a value to an array at a path, handling scalar-to-array conversion.
    fn append_at_path(&mut self, path: &[String], value: ConfigValue) {
        if path.is_empty() {
            return;
        }

        if path.len() == 1 {
            let key = &path[0];
            match self.root.entry(key.clone()) {
                indexmap::map::Entry::Vacant(entry) => {
                    // First value - create array with single element
                    entry.insert(ConfigValue::Array(Sourced {
                        value: vec![value],
                        span: None,
                        provenance: None,
                    }));
                }
                indexmap::map::Entry::Occupied(mut entry) => {
                    let existing = entry.get_mut();
                    if let ConfigValue::Array(arr) = existing {
                        // Already an array, append
                        arr.value.push(value);
                    } else {
                        // Convert scalar to array with both values
                        let old = std::mem::replace(
                            existing,
                            ConfigValue::Null(Sourced::new(())),
                        );
                        *existing = ConfigValue::Array(Sourced {
                            value: vec![old, value],
                            span: None,
                            provenance: None,
                        });
                    }
                }
            }
            return;
        }

        // Navigate to parent and append there
        let parent_path = &path[..path.len() - 1];
        let key = &path[path.len() - 1];

        // Ensure parent exists as object
        let first = &parent_path[0];
        let entry = self.root.entry(first.clone()).or_insert_with(|| {
            ConfigValue::Object(Sourced::new(IndexMap::default()))
        });

        if parent_path.len() == 1 {
            if let ConfigValue::Object(obj) = entry {
                append_to_map(&mut obj.value, key.clone(), value);
            }
        } else {
            // Navigate deeper
            let rest = &parent_path[1..];
            ensure_path_exists(entry, rest);
            if let Some(parent_obj) = get_object_at_path_mut(entry, rest) {
                append_to_map(parent_obj, key.clone(), value);
            }
        }
    }

    /// Import values from an already-parsed ConfigValue tree.
    ///
    /// This walks the tree and calls `set()` for each leaf value, which means:
    /// - Path validation happens (invalid paths → unused keys)
    /// - Enum variant conflicts are detected
    /// - Enum values are validated
    ///
    /// Used by file parsers that receive structured data (JSON, TOML).
    pub fn import_tree(&mut self, value: &ConfigValue) {
        self.import_tree_recursive(value, Vec::new());
    }

    fn import_tree_recursive(&mut self, value: &ConfigValue, path: Vec<String>) {
        match value {
            ConfigValue::Object(obj) => {
                for (key, val) in &obj.value {
                    let mut key_path = path.clone();
                    key_path.push(key.clone());
                    self.import_tree_recursive(val, key_path);
                }
            }
            ConfigValue::Array(arr) => {
                for (idx, val) in arr.value.iter().enumerate() {
                    let mut idx_path = path.clone();
                    idx_path.push(idx.to_string());
                    self.import_tree_recursive(val, idx_path);
                }
            }
            // Leaf values - call set()
            ConfigValue::String(s) => {
                let prov = s.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::String(s.value.clone()), s.span, prov);
            }
            ConfigValue::Bool(b) => {
                let prov = b.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::Bool(b.value), b.span, prov);
            }
            ConfigValue::Integer(i) => {
                let prov = i.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::Integer(i.value), i.span, prov);
            }
            ConfigValue::Float(f) => {
                let prov = f.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::Float(f.value), f.span, prov);
            }
            ConfigValue::Null(n) => {
                let prov = n.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::Null, n.span, prov);
            }
            ConfigValue::Enum(e) => {
                // Enum values from file parsing - treat as string for validation
                let prov = e.provenance.clone().unwrap_or(Provenance::Default);
                self.set(&path, LeafValue::String(e.value.variant.clone()), e.span, prov);
            }
        }
    }

    /// Consume the builder and return a LayerOutput with the given ConfigValue.
    ///
    /// This is used after `import_tree()` to return the original parsed value
    /// along with collected diagnostics and unused keys.
    pub fn into_output_with_value(self, value: Option<ConfigValue>, field_name: Option<&str>) -> LayerOutput {
        let value = match value {
            Some(parsed) if field_name.is_some() => {
                // Wrap under the config field name
                let mut root = IndexMap::default();
                root.insert(field_name.unwrap().to_string(), parsed);
                Some(ConfigValue::Object(Sourced::new(root)))
            }
            other => other,
        };

        LayerOutput {
            value,
            unused_keys: self.unused_keys,
            diagnostics: self.diagnostics,
        }
    }

    // ========================================================================
    // Private implementation
    // ========================================================================

    /// Resolve a path against the schema.
    fn resolve_path(&self, path: &[String]) -> Option<ResolvedPath> {
        if path.is_empty() {
            return None;
        }

        let mut result = ResolvedPath {
            insertion_path: Vec::new(),
            enum_selections: Vec::new(),
        };

        self.resolve_struct_path(self.schema, path, &mut result)?;
        Some(result)
    }

    fn resolve_struct_path(
        &self,
        struct_schema: &ConfigStructSchema,
        path: &[String],
        result: &mut ResolvedPath,
    ) -> Option<()> {
        if path.is_empty() {
            return Some(());
        }

        let segment = &path[0];

        // Find the field (case-insensitive)
        let (effective_name, field_schema) = struct_schema
            .fields()
            .iter()
            .find(|(k, _)| k.to_lowercase() == segment.to_lowercase())?;

        result.insertion_path.push(effective_name.clone());

        if path.len() == 1 {
            return Some(());
        }

        // Navigate into the field's value
        self.resolve_value_path(field_schema.value(), &path[1..], result)
    }

    fn resolve_value_path(
        &self,
        schema: &ConfigValueSchema,
        path: &[String],
        result: &mut ResolvedPath,
    ) -> Option<()> {
        if path.is_empty() {
            return Some(());
        }

        match schema {
            ConfigValueSchema::Option { value, .. } => {
                // Unwrap option and continue
                self.resolve_value_path(value, path, result)
            }
            ConfigValueSchema::Struct(struct_schema) => {
                self.resolve_struct_path(struct_schema, path, result)
            }
            ConfigValueSchema::Enum(enum_schema) => {
                self.resolve_enum_path(enum_schema, path, result)
            }
            ConfigValueSchema::Vec(vec_schema) => {
                // Path segment should be a numeric index
                let index_segment = &path[0];
                if index_segment.parse::<usize>().is_err() {
                    return None;
                }

                result.insertion_path.push(index_segment.clone());

                if path.len() == 1 {
                    return Some(());
                }

                // Continue into the element type
                self.resolve_value_path(vec_schema.element(), &path[1..], result)
            }
            ConfigValueSchema::Leaf(_) => {
                // Can't navigate into a leaf
                None
            }
        }
    }

    fn resolve_enum_path(
        &self,
        enum_schema: &ConfigEnumSchema,
        path: &[String],
        result: &mut ResolvedPath,
    ) -> Option<()> {
        if path.is_empty() {
            return None; // Can't set an enum directly, need variant
        }

        let variant_segment = &path[0];

        // Find the variant (case-insensitive)
        let (variant_name, variant_schema) = enum_schema
            .variants()
            .iter()
            .find(|(k, _)| k.to_lowercase() == variant_segment.to_lowercase())?;

        // Record enum selection for conflict detection
        result.enum_selections.push(EnumSelection {
            enum_path: result.insertion_path.clone(),
            variant_name: variant_name.clone(),
        });

        result.insertion_path.push(variant_name.clone());

        if path.len() == 1 {
            // Just selecting the variant, no fields
            return Some(());
        }

        // Navigate into variant's fields
        self.resolve_variant_path(variant_schema, &path[1..], result)
    }

    fn resolve_variant_path(
        &self,
        variant_schema: &ConfigEnumVariantSchema,
        path: &[String],
        result: &mut ResolvedPath,
    ) -> Option<()> {
        if path.is_empty() {
            return Some(());
        }

        let field_segment = &path[0];

        // Find the field in the variant (case-insensitive)
        let (field_name, field_schema) = variant_schema
            .fields()
            .iter()
            .find(|(k, _)| k.to_lowercase() == field_segment.to_lowercase())?;

        result.insertion_path.push(field_name.clone());

        if path.len() == 1 {
            return Some(());
        }

        // Continue navigating
        self.resolve_value_path(field_schema.value(), &path[1..], result)
    }

    /// Check and record enum variant selection. Returns false if there's a conflict.
    fn check_enum_variant_conflict(
        &mut self,
        enum_path: &[String],
        variant_name: &str,
        provenance: &Provenance,
    ) -> bool {
        let key = enum_path.to_vec();

        if let Some((existing_variant, existing_prov)) = self.enum_variants.get(&key) {
            if existing_variant != variant_name {
                let existing_source = existing_prov.source_description();
                let new_source = provenance.source_description();

                self.error(format!(
                    "Conflicting enum variants for `{}`: variant '{}' (from {}) conflicts with '{}' (from {})",
                    enum_path.join("."),
                    variant_name,
                    new_source,
                    existing_variant,
                    existing_source,
                ));
                return false;
            }
        } else {
            self.enum_variants
                .insert(key, (variant_name.to_string(), provenance.clone()));
        }

        true
    }

    /// Insert a value at the given path, creating intermediate objects/arrays as needed.
    fn insert_at_path(&mut self, path: &[String], value: ConfigValue) {
        if path.is_empty() {
            return;
        }

        if path.len() == 1 {
            self.root.insert(path[0].clone(), value);
            return;
        }

        let first = &path[0];
        let rest = &path[1..];

        // Check if next segment is numeric (array) or not (object)
        let is_array = rest
            .first()
            .map(|s| s.parse::<usize>().is_ok())
            .unwrap_or(false);

        let entry = self.root.entry(first.clone()).or_insert_with(|| {
            if is_array {
                ConfigValue::Array(Sourced::new(Vec::new()))
            } else {
                ConfigValue::Object(Sourced::new(IndexMap::default()))
            }
        });

        insert_nested(entry, rest, value);
    }
}

/// A fully resolved path with enum selections.
struct ResolvedPath {
    insertion_path: Path,
    enum_selections: Vec<EnumSelection>,
}

/// An enum variant selection made while navigating a path.
struct EnumSelection {
    enum_path: Path,
    variant_name: String,
}

/// Append a value to an array in a map, handling scalar-to-array conversion.
fn append_to_map(map: &mut ObjectMap, key: String, value: ConfigValue) {
    match map.entry(key) {
        indexmap::map::Entry::Vacant(entry) => {
            entry.insert(ConfigValue::Array(Sourced {
                value: vec![value],
                span: None,
                provenance: None,
            }));
        }
        indexmap::map::Entry::Occupied(mut entry) => {
            let existing = entry.get_mut();
            if let ConfigValue::Array(arr) = existing {
                arr.value.push(value);
            } else {
                let old = std::mem::replace(existing, ConfigValue::Null(Sourced::new(())));
                *existing = ConfigValue::Array(Sourced {
                    value: vec![old, value],
                    span: None,
                    provenance: None,
                });
            }
        }
    }
}

/// Ensure all segments of a path exist as objects.
fn ensure_path_exists(current: &mut ConfigValue, path: &[String]) {
    if path.is_empty() {
        return;
    }

    if let ConfigValue::Object(obj) = current {
        let entry = obj.value.entry(path[0].clone()).or_insert_with(|| {
            ConfigValue::Object(Sourced::new(IndexMap::default()))
        });
        if path.len() > 1 {
            ensure_path_exists(entry, &path[1..]);
        }
    }
}

/// Get a mutable reference to the object map at a path.
fn get_object_at_path_mut<'a>(current: &'a mut ConfigValue, path: &[String]) -> Option<&'a mut ObjectMap> {
    if path.is_empty() {
        if let ConfigValue::Object(obj) = current {
            return Some(&mut obj.value);
        }
        return None;
    }

    if let ConfigValue::Object(obj) = current {
        if let Some(next) = obj.value.get_mut(&path[0]) {
            return get_object_at_path_mut(next, &path[1..]);
        }
    }
    None
}

/// Insert a value at a nested path, creating intermediate structures as needed.
fn insert_nested(current: &mut ConfigValue, path: &[String], value: ConfigValue) {
    if path.is_empty() {
        return;
    }

    if path.len() == 1 {
        match current {
            ConfigValue::Object(obj) => {
                obj.value.insert(path[0].clone(), value);
            }
            ConfigValue::Array(arr) => {
                if let Ok(idx) = path[0].parse::<usize>() {
                    // Extend array if needed
                    while arr.value.len() <= idx {
                        arr.value.push(ConfigValue::Null(Sourced::new(())));
                    }
                    arr.value[idx] = value;
                }
            }
            _ => {}
        }
        return;
    }

    let key = &path[0];
    let rest = &path[1..];

    // Determine if next level should be array or object
    let next_is_array = rest
        .first()
        .map(|s| s.parse::<usize>().is_ok())
        .unwrap_or(false);

    match current {
        ConfigValue::Object(obj) => {
            let entry = obj.value.entry(key.clone()).or_insert_with(|| {
                if next_is_array {
                    ConfigValue::Array(Sourced::new(Vec::new()))
                } else {
                    ConfigValue::Object(Sourced::new(IndexMap::default()))
                }
            });
            insert_nested(entry, rest, value);
        }
        ConfigValue::Array(arr) => {
            if let Ok(idx) = key.parse::<usize>() {
                // Extend array if needed
                while arr.value.len() <= idx {
                    if next_is_array {
                        arr.value.push(ConfigValue::Array(Sourced::new(Vec::new())));
                    } else {
                        arr.value
                            .push(ConfigValue::Object(Sourced::new(IndexMap::default())));
                    }
                }
                insert_nested(&mut arr.value[idx], rest, value);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::Schema;
    use facet::Facet;

    #[derive(Facet)]
    struct SimpleConfig {
        port: u16,
        host: String,
    }

    #[derive(Facet)]
    struct ArgsWithSimpleConfig {
        #[facet(crate::config)]
        config: SimpleConfig,
    }

    #[derive(Facet)]
    #[repr(u8)]
    #[allow(dead_code)]
    enum Storage {
        S3 { bucket: String, region: String },
        Gcp { project: String, zone: String },
        Local { path: String },
    }

    #[derive(Facet)]
    struct ConfigWithEnum {
        storage: Storage,
        port: u16,
    }

    #[derive(Facet)]
    struct ArgsWithEnumConfig {
        #[facet(crate::config)]
        config: ConfigWithEnum,
    }

    fn path(segments: &[&str]) -> Path {
        segments.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn test_simple_set() {
        let schema = Schema::from_shape(ArgsWithSimpleConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        let success = builder.set(
            &path(&["port"]),
            LeafValue::String("8080".into()),
            None,
            Provenance::Default,
        );
        assert!(success);

        let output = builder.into_output(Some("config"));
        assert!(output.value.is_some());
        assert!(output.diagnostics.is_empty());
    }

    #[test]
    fn test_invalid_path() {
        let schema = Schema::from_shape(ArgsWithSimpleConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        let success = builder.set(
            &path(&["invalid"]),
            LeafValue::String("value".into()),
            None,
            Provenance::Default,
        );
        assert!(!success);
        assert!(!builder.unused_keys.is_empty());
    }

    #[test]
    fn test_enum_variant_path() {
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        let prov_bucket = Provenance::env("TEST__STORAGE__S3__BUCKET", "my-bucket");
        let success = builder.set(
            &path(&["storage", "s3", "bucket"]),
            LeafValue::String("my-bucket".into()),
            None,
            prov_bucket,
        );
        assert!(success);

        // Same variant, different field
        let prov_region = Provenance::env("TEST__STORAGE__S3__REGION", "us-east-1");
        let success = builder.set(
            &path(&["storage", "s3", "region"]),
            LeafValue::String("us-east-1".into()),
            None,
            prov_region,
        );
        assert!(success);

        let output = builder.into_output(Some("config"));
        assert!(output.diagnostics.is_empty());
    }

    #[test]
    fn test_enum_variant_conflict() {
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        // Set S3 variant
        let prov_s3 = Provenance::env("TEST__STORAGE__S3__BUCKET", "my-bucket");
        builder.set(
            &path(&["storage", "s3", "bucket"]),
            LeafValue::String("my-bucket".into()),
            None,
            prov_s3,
        );

        // Try GCP variant - should conflict
        let prov_gcp = Provenance::env("TEST__STORAGE__GCP__PROJECT", "my-project");
        let success = builder.set(
            &path(&["storage", "gcp", "project"]),
            LeafValue::String("my-project".into()),
            None,
            prov_gcp,
        );
        assert!(!success);
        assert!(!builder.diagnostics.is_empty());
        assert!(builder.diagnostics[0].message.contains("Conflicting"));
    }

    #[test]
    fn test_has_value_at() {
        let schema = Schema::from_shape(ArgsWithSimpleConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        assert!(!builder.has_value_at(&path(&["port"])));

        builder.set(
            &path(&["port"]),
            LeafValue::String("8080".into()),
            None,
            Provenance::Default,
        );

        assert!(builder.has_value_at(&path(&["port"])));
        assert!(!builder.has_value_at(&path(&["host"])));
    }

    // ========================================================================
    // Tests for CLI-specific methods
    // ========================================================================

    #[derive(Facet)]
    struct ConfigWithVerbose {
        verbose: u8,
        tags: Vec<String>,
    }

    #[derive(Facet)]
    struct ArgsWithVerboseConfig {
        #[facet(crate::config)]
        config: ConfigWithVerbose,
    }

    #[test]
    fn test_increment_counted_flag() {
        let schema = Schema::from_shape(ArgsWithVerboseConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        // First increment: 0 -> 1
        let success = builder.increment(
            &path(&["verbose"]),
            None,
            Provenance::cli("-v", ""),
        );
        assert!(success);

        // Second increment: 1 -> 2
        let success = builder.increment(
            &path(&["verbose"]),
            None,
            Provenance::cli("-v", ""),
        );
        assert!(success);

        // Third increment: 2 -> 3
        let success = builder.increment(
            &path(&["verbose"]),
            None,
            Provenance::cli("-v", ""),
        );
        assert!(success);

        // Check the final value is 3
        let output = builder.into_output(Some("config"));
        assert!(output.value.is_some());
        if let Some(ConfigValue::Object(root)) = &output.value {
            if let Some(ConfigValue::Object(config)) = root.value.get("config") {
                if let Some(ConfigValue::Integer(int)) = config.value.get("verbose") {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("verbose should be an integer");
                }
            } else {
                panic!("config should exist");
            }
        } else {
            panic!("should have root object");
        }
    }

    #[test]
    fn test_append_repeated_flag() {
        let schema = Schema::from_shape(ArgsWithVerboseConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        // First append
        let success = builder.append(
            &path(&["tags"]),
            LeafValue::String("foo".into()),
            None,
            Provenance::cli("--tag", "foo"),
        );
        assert!(success);

        // Second append
        let success = builder.append(
            &path(&["tags"]),
            LeafValue::String("bar".into()),
            None,
            Provenance::cli("--tag", "bar"),
        );
        assert!(success);

        // Check the array has both values
        let output = builder.into_output(Some("config"));
        assert!(output.value.is_some());
        if let Some(ConfigValue::Object(root)) = &output.value {
            if let Some(ConfigValue::Object(config)) = root.value.get("config") {
                if let Some(ConfigValue::Array(arr)) = config.value.get("tags") {
                    assert_eq!(arr.value.len(), 2);
                    if let ConfigValue::String(s) = &arr.value[0] {
                        assert_eq!(s.value, "foo");
                    }
                    if let ConfigValue::String(s) = &arr.value[1] {
                        assert_eq!(s.value, "bar");
                    }
                } else {
                    panic!("tags should be an array");
                }
            }
        }
    }

    #[test]
    fn test_set_enum_subcommand() {
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        // Set enum with fields
        let mut fields = IndexMap::default();
        fields.insert(
            "bucket".to_string(),
            ConfigValue::String(Sourced {
                value: "my-bucket".to_string(),
                span: None,
                provenance: Some(Provenance::cli("--bucket", "my-bucket")),
            }),
        );
        fields.insert(
            "region".to_string(),
            ConfigValue::String(Sourced {
                value: "us-east-1".to_string(),
                span: None,
                provenance: Some(Provenance::cli("--region", "us-east-1")),
            }),
        );

        let success = builder.set_enum(
            &path(&["storage"]),
            "S3".to_string(),
            fields,
            None,
            Provenance::cli("s3", ""),
        );
        assert!(success);

        let output = builder.into_output(Some("config"));
        assert!(output.diagnostics.is_empty());
        assert!(output.value.is_some());
    }

    #[test]
    fn test_set_enum_conflict() {
        let schema = Schema::from_shape(ArgsWithEnumConfig::SHAPE).unwrap();
        let config_schema = schema.config().unwrap();
        let mut builder = ValueBuilder::new(config_schema);

        // Set S3 variant first
        let success = builder.set_enum(
            &path(&["storage"]),
            "S3".to_string(),
            IndexMap::default(),
            None,
            Provenance::cli("s3", ""),
        );
        assert!(success);

        // Try to set Gcp variant - should conflict
        let success = builder.set_enum(
            &path(&["storage"]),
            "Gcp".to_string(),
            IndexMap::default(),
            None,
            Provenance::cli("gcp", ""),
        );
        assert!(!success);
        assert!(!builder.diagnostics.is_empty());
        assert!(builder.diagnostics[0].message.contains("Conflicting"));
    }
}
