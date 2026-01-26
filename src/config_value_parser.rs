//! Parser that converts `ConfigValue` trees into `ParseEvent` streams for deserialization.
#![allow(dead_code)]
//!
//! This allows us to deserialize `ConfigValue` into arbitrary Facet types using the
//! standard `facet-format` deserializer infrastructure.

use std::vec::Vec;

use facet_core::{Facet, Shape, Type, UserType};
// Note: Shape is still used by fill_defaults_from_shape and coerce_types_from_shape
use facet_format::{
    ContainerKind, FieldKey, FieldLocationHint, FormatDeserializer, FormatParser, ParseEvent,
    ScalarValue,
};
use facet_reflect::Span;
use indexmap::IndexMap;

use crate::config_value::{ConfigValue, Sourced};
use crate::provenance::Provenance;

/// Deserialize a `ConfigValue` into a Facet type.
///
/// This is the main entry point for converting merged configuration values
/// into strongly-typed structs.
///
/// # Example
///
/// ```ignore
/// use figue::config_value::ConfigValue;
/// use figue::config_value_parser::from_config_value;
///
/// #[derive(facet::Facet)]
/// struct Config {
///     port: u16,
///     host: String,
/// }
///
/// let config_value = /* ... merged from CLI/env/file ... */;
/// let config: Config = from_config_value(&config_value)?;
/// ```
pub fn from_config_value<T>(value: &ConfigValue) -> Result<T, ConfigValueDeserializeError>
where
    T: Facet<'static>,
{
    tracing::trace!(
        shape = T::SHAPE.type_identifier,
        ?value,
        "from_config_value: starting"
    );

    // First, fill in defaults for missing fields based on the target shape
    let value_with_defaults = fill_defaults_from_shape(value, T::SHAPE);
    tracing::trace!(
        ?value_with_defaults,
        "from_config_value: after fill_defaults_from_shape"
    );

    // Coerce string values to their target types (CLI/env values come in as strings)
    let value_coerced = crate::reflection::coerce_types_from_shape(&value_with_defaults, T::SHAPE);
    tracing::trace!(
        ?value_coerced,
        "from_config_value: after coerce_types_from_shape"
    );

    let parser = ConfigValueParser::new(&value_coerced);
    let mut deserializer = FormatDeserializer::new_owned(parser);
    deserializer
        .deserialize()
        .map_err(ConfigValueDeserializeError::Deserialize)
}

/// Walk the shape and insert default values for missing fields in the ConfigValue tree.
/// This allows proper provenance tracking (defaults are marked as coming from Default).
pub(crate) fn fill_defaults_from_shape(value: &ConfigValue, shape: &'static Shape) -> ConfigValue {
    fill_defaults_from_shape_recursive(value, shape, "")
}

fn fill_defaults_from_shape_recursive(
    value: &ConfigValue,
    shape: &'static Shape,
    path_prefix: &str,
) -> ConfigValue {
    tracing::debug!(
        shape = shape.type_identifier,
        path_prefix,
        "fill_defaults_from_shape_recursive: entering"
    );

    // Special handling for Option types: unwrap to inner type
    if let Ok(option_def) = shape.def.into_option() {
        // For Option types, if the value is an Object, recurse with the inner type
        return fill_defaults_from_shape_recursive(value, option_def.t, path_prefix);
    }

    match value {
        ConfigValue::Object(sourced) => {
            // Get struct fields from shape
            let fields = match &shape.ty {
                Type::User(UserType::Struct(s)) => &s.fields,
                _ => return value.clone(),
            };

            let mut new_map = sourced.value.clone();

            // For each field in the struct shape, check if it's missing in the ConfigValue
            for field in fields.iter() {
                // Handle flattened fields: their inner fields should appear at the
                // CURRENT level, not nested under the field name.
                // The facet-format deserializer handles collecting them into the nested struct.
                if field.is_flattened() {
                    let inner_shape = field.shape.get();
                    let inner_fields = match &inner_shape.ty {
                        Type::User(UserType::Struct(s)) => &s.fields,
                        _ => continue, // Skip if not a struct (shouldn't happen for flatten)
                    };

                    // For each inner field, add default if missing at the CURRENT level
                    for inner_field in inner_fields.iter() {
                        if !new_map.contains_key(inner_field.name) {
                            let inner_path = if path_prefix.is_empty() {
                                inner_field.name.to_string()
                            } else {
                                format!("{}.{}", path_prefix, inner_field.name)
                            };
                            if let Some(default_value) =
                                get_default_config_value(inner_field, &inner_path)
                            {
                                tracing::debug!(
                                    field = inner_field.name,
                                    parent_field = field.name,
                                    shape = shape.type_identifier,
                                    ?default_value,
                                    "fill_defaults_from_shape_recursive: inserting default for flattened field"
                                );
                                new_map.insert(inner_field.name.to_string(), default_value);
                            }
                        }
                    }
                    // Don't create a nested object for flattened fields
                    continue;
                }

                if !new_map.contains_key(field.name) {
                    // Field is missing - get default or create Missing marker
                    // Returns None if the field has a default that can't be serialized -
                    // in that case we don't insert anything and let facet handle the default
                    if let Some(default_value) = get_default_config_value(field, path_prefix) {
                        tracing::debug!(
                            field = field.name,
                            shape = shape.type_identifier,
                            ?default_value,
                            "fill_defaults_from_shape_recursive: inserting default for missing field"
                        );
                        new_map.insert(field.name.to_string(), default_value);
                    } else {
                        tracing::debug!(
                            field = field.name,
                            shape = shape.type_identifier,
                            "fill_defaults_from_shape_recursive: field has unserializable default, letting facet handle it"
                        );
                    }
                }
            }

            // Recursively process nested objects
            for (key, val) in new_map.iter_mut() {
                // Find the corresponding field shape
                // First check direct (non-flattened) fields
                if let Some(field) = fields.iter().find(|f| f.name == key && !f.is_flattened()) {
                    let field_path = if path_prefix.is_empty() {
                        key.to_string()
                    } else {
                        format!("{}.{}", path_prefix, key)
                    };
                    *val = fill_defaults_from_shape_recursive(val, field.shape.get(), &field_path);
                } else {
                    // Check if key matches an inner field of a flattened struct
                    for field in fields.iter().filter(|f| f.is_flattened()) {
                        let inner_shape = field.shape.get();
                        if let Type::User(UserType::Struct(s)) = &inner_shape.ty
                            && let Some(inner_field) = s.fields.iter().find(|f| f.name == key)
                        {
                            let field_path = if path_prefix.is_empty() {
                                key.to_string()
                            } else {
                                format!("{}.{}", path_prefix, key)
                            };
                            *val = fill_defaults_from_shape_recursive(
                                val,
                                inner_field.shape.get(),
                                &field_path,
                            );
                            break;
                        }
                    }
                }
            }

            let result = ConfigValue::Object(Sourced {
                value: new_map,
                span: sourced.span,
                provenance: sourced.provenance.clone(),
            });
            tracing::debug!(
                shape = shape.type_identifier,
                "fill_defaults_from_shape: completed Object"
            );
            result
        }
        ConfigValue::Array(sourced) => {
            // Recursively process array items
            // TODO: get element shape from array def
            let items: Vec<_> = sourced.value.to_vec();

            ConfigValue::Array(Sourced {
                value: items,
                span: sourced.span,
                provenance: sourced.provenance.clone(),
            })
        }
        ConfigValue::Enum(sourced) => {
            // For enum variants, fill defaults in the variant's fields
            // First, find the variant in the enum type
            let enum_type = match &shape.ty {
                Type::User(UserType::Enum(e)) => *e,
                _ => return value.clone(),
            };

            // Find the variant by name
            let variant = enum_type
                .variants
                .iter()
                .find(|v| v.name == sourced.value.variant);

            let Some(variant) = variant else {
                return value.clone();
            };

            // Get the variant's fields (if any)
            let variant_fields = variant.data.fields;

            // Unit variants have no fields to fill
            if variant_fields.is_empty() {
                return value.clone();
            }

            // Create a new fields map with defaults filled in
            let mut new_fields = sourced.value.fields.clone();

            for field in variant_fields.iter() {
                if !new_fields.contains_key(field.name) {
                    let field_path = if path_prefix.is_empty() {
                        field.name.to_string()
                    } else {
                        format!("{}.{}", path_prefix, field.name)
                    };
                    if let Some(default_value) = get_default_config_value(field, &field_path) {
                        new_fields.insert(field.name.to_string(), default_value);
                    }
                }
            }

            // Recursively fill defaults in nested values
            for (key, val) in new_fields.iter_mut() {
                if let Some(field) = variant_fields.iter().find(|f| f.name == key) {
                    let field_path = if path_prefix.is_empty() {
                        key.to_string()
                    } else {
                        format!("{}.{}", path_prefix, key)
                    };
                    *val = fill_defaults_from_shape_recursive(val, field.shape.get(), &field_path);
                }
            }

            ConfigValue::Enum(Sourced {
                value: crate::config_value::EnumValue {
                    variant: sourced.value.variant.clone(),
                    fields: new_fields,
                },
                span: sourced.span,
                provenance: sourced.provenance.clone(),
            })
        }
        _ => value.clone(),
    }
}

/// Get a default ConfigValue for a field.
///
/// For fields with #[facet(default)], calls the default function and serializes to ConfigValue.
/// For Option<T> types, provides None (null) as implicit default.
/// For scalar fields without defaults, provides CLI-friendly defaults (false/0).
/// For struct fields without defaults, creates an empty Object for recursive filling.
/// For required fields without defaults, returns ConfigValue::Missing with field info.
///
/// Returns None when the field has a default but it can't be serialized to ConfigValue.
/// In that case, the caller should skip inserting the field and let facet's deserializer
/// apply the field's default function directly.
fn get_default_config_value(
    field: &'static facet_core::Field,
    _path_prefix: &str,
) -> Option<ConfigValue> {
    use facet_core::ScalarType;

    let shape = field.shape.get();

    // If field has explicit default, invoke it and serialize to ConfigValue
    if let Some(default_source) = &field.default {
        tracing::debug!(
            field = field.name,
            "get_default_config_value: field has default, invoking"
        );

        if let Ok(config_value) = serialize_default_to_config_value(default_source, shape) {
            // Check if serialization actually produced a useful value
            // (not null for non-nullable types)
            if !matches!(config_value, ConfigValue::Null(_)) {
                tracing::debug!(
                    field = field.name,
                    ?config_value,
                    "get_default_config_value: successfully serialized default"
                );
                return Some(config_value);
            }
            // Null result for a type that has a default but can't serialize -
            // fall through to let facet handle the default during deserialization
            tracing::debug!(
                field = field.name,
                "get_default_config_value: serialized to null, letting facet handle default"
            );
        } else {
            // Serialization failed - the type has a default but can't be represented
            // as ConfigValue. Let facet handle it during deserialization.
            tracing::debug!(
                field = field.name,
                "get_default_config_value: serialization failed, letting facet handle default"
            );
        }
        // The field has a default but we can't represent it as ConfigValue.
        // Return None so the caller skips inserting this field, letting facet's
        // deserializer apply the field's default function.
        return None;
    }

    // Option<T> implicitly has Default semantics (None)
    // Check type_identifier for "Option" - handles both std::option::Option and core::option::Option
    if shape.type_identifier.contains("Option") {
        return Some(ConfigValue::Null(Sourced {
            value: (),
            span: None,
            provenance: Some(Provenance::Default),
        }));
    }

    // For struct types without explicit defaults, create empty object for recursive filling
    if let Type::User(UserType::Struct(_)) = &shape.ty {
        return Some(ConfigValue::Object(Sourced {
            value: IndexMap::default(),
            span: None,
            provenance: Some(Provenance::Default),
        }));
    }

    // For scalar types without explicit defaults, emit CLI-friendly defaults
    if let Some(scalar_type) = shape.scalar_type() {
        return Some(match scalar_type {
            ScalarType::Bool => ConfigValue::Bool(Sourced {
                value: false,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarType::U8
            | ScalarType::U16
            | ScalarType::U32
            | ScalarType::U64
            | ScalarType::U128
            | ScalarType::USize
            | ScalarType::I8
            | ScalarType::I16
            | ScalarType::I32
            | ScalarType::I64
            | ScalarType::I128
            | ScalarType::ISize => ConfigValue::Integer(Sourced {
                value: 0,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            _ => {
                // No sensible default for other scalar types - don't fill
                return None;
            }
        });
    }

    // No default available - don't fill, let schema-driven logic handle missing fields
    None
}

/// Serialize a default value to ConfigValue by invoking the default function
/// and using a ConfigValueSerializer.
#[allow(unsafe_code)]
fn serialize_default_to_config_value(
    default_source: &facet_core::DefaultSource,
    shape: &'static facet_core::Shape,
) -> Result<ConfigValue, String> {
    use facet_core::{DefaultSource, TypeOps};

    // Allocate space for the default value
    let mut storage: [u8; 1024] = [0; 1024]; // Stack buffer for small types
    let ptr = storage.as_mut_ptr() as *mut ();

    // Call the appropriate default function to initialize the value
    match default_source {
        DefaultSource::FromTrait => {
            // Get default_in_place from type_ops
            let type_ops = shape
                .type_ops
                .ok_or_else(|| format!("Shape {} has no type_ops", shape.type_identifier))?;

            match type_ops {
                TypeOps::Direct(ops) => {
                    let default_fn = ops.default_in_place.ok_or_else(|| {
                        format!("Shape {} has no default function", shape.type_identifier)
                    })?;
                    // Direct ops default: unsafe fn(*mut ()) - initializes in place, no return value
                    unsafe { (default_fn)(ptr) };
                }
                TypeOps::Indirect(_) => {
                    Err(
                        "Indirect type ops not yet supported for default serialization".to_string(),
                    )?;
                }
            }
        }
        DefaultSource::Custom(fn_ptr) => {
            // Custom default: unsafe fn(PtrUninit) -> PtrMut
            unsafe {
                let ptr_uninit = facet_core::PtrUninit::new_sized(ptr);
                (*fn_ptr)(ptr_uninit);
            }
        }
    }

    // Create a Peek from the initialized value
    let peek = unsafe {
        let ptr_const = facet_core::PtrConst::new_sized(ptr as *const ());
        facet_reflect::Peek::unchecked_new(ptr_const, shape)
    };

    // Serialize to ConfigValue using our serializer
    let mut serializer = ConfigValueSerializer::new();
    facet_format::serialize_root(&mut serializer, peek)
        .map_err(|e| format!("Serialization failed: {:?}", e))?;

    let result = serializer.finish();

    // If serialization returned Null but the type has Display, use the string representation.
    // This handles opaque types like IpAddr, PathBuf, etc. that don't serialize natively
    // but can be represented as strings for CLI/env/config purposes.
    if matches!(result, ConfigValue::Null(_)) && shape.vtable.has_display() {
        let string_value = peek.to_string();
        return Ok(ConfigValue::String(Sourced {
            value: string_value,
            span: None,
            provenance: Some(Provenance::Default),
        }));
    }

    Ok(result)
}

/// Errors that can occur during ConfigValue deserialization.
#[derive(Debug)]
pub enum ConfigValueDeserializeError {
    /// Error during deserialization.
    Deserialize(facet_format::DeserializeError<ConfigValueParseError>),
}

impl core::fmt::Display for ConfigValueDeserializeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ConfigValueDeserializeError::Deserialize(e) => write!(f, "{}", e),
        }
    }
}

impl core::error::Error for ConfigValueDeserializeError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            ConfigValueDeserializeError::Deserialize(e) => Some(e),
        }
    }
}

impl ConfigValueDeserializeError {
    /// Get the span associated with this error, if any.
    pub fn span(&self) -> Option<facet_reflect::Span> {
        match self {
            ConfigValueDeserializeError::Deserialize(e) => match e {
                facet_format::DeserializeError::Reflect { span, .. } => *span,
                facet_format::DeserializeError::TypeMismatch { span, .. } => *span,
                facet_format::DeserializeError::UnknownField { span, .. } => *span,
                facet_format::DeserializeError::MissingField { span, .. } => *span,
                _ => None,
            },
        }
    }
}

/// Parser that emits events from a `ConfigValue` tree.
///
/// This is a simple parser that just emits what's in the ConfigValue.
/// It does NOT do any Shape-based transformations - that's the deserializer's job.
pub struct ConfigValueParser<'input> {
    /// Stack of values to process.
    stack: Vec<StackFrame<'input>>,
    /// The most recent span (for error reporting).
    last_span: Option<Span>,
    /// Peeked event (cached for peek_event).
    peeked: Option<ParseEvent<'input>>,
}

/// A frame on the parsing stack.
enum StackFrame<'input> {
    /// Processing an object - emit key-value pairs.
    Object {
        entries: Vec<(&'input str, &'input ConfigValue)>,
        index: usize,
    },
    /// Processing an array - emit items in sequence.
    Array {
        items: &'input [ConfigValue],
        index: usize,
    },
    /// A single value to emit.
    Value(&'input ConfigValue),
    /// Processing an enum variant - externally tagged format: {"VariantName": {...}}
    /// Phase 0: emit FieldKey with variant name
    /// Phase 1: emit variant content (struct with fields, or Unit for unit variants)
    /// Phase 2: emit StructEnd
    Enum {
        variant: &'input str,
        fields: Vec<(&'input str, &'input ConfigValue)>,
        phase: u8,
        /// Whether this is a unit variant (no fields) vs struct/tuple variant
        is_unit: bool,
    },
}

impl<'input> ConfigValueParser<'input> {
    /// Create a new parser from a `ConfigValue`.
    pub fn new(value: &'input ConfigValue) -> Self {
        Self {
            stack: vec![StackFrame::Value(value)],
            last_span: None,
            peeked: None,
        }
    }

    /// Get the most recent span.
    pub fn last_span(&self) -> Option<Span> {
        self.last_span
    }

    /// Update the last span from a `Sourced` wrapper.
    fn update_span<T>(&mut self, sourced: &Sourced<T>) {
        if let Some(span) = sourced.span {
            self.last_span = Some(span);
        }
    }

    /// Build probe evidence from the current value.
    /// Returns field names and optionally scalar values for the solver.
    fn build_probe_evidence(&self) -> Vec<facet_format::FieldEvidence<'input>> {
        use std::borrow::Cow;

        tracing::debug!(
            stack_len = self.stack.len(),
            "build_probe_evidence: starting"
        );

        // Get the root value from the stack
        let root_value = match self.stack.first() {
            Some(StackFrame::Value(v)) => {
                tracing::debug!("build_probe_evidence: found Value frame");
                *v
            }
            Some(StackFrame::Object { entries, .. }) => {
                tracing::debug!(
                    num_entries = entries.len(),
                    "build_probe_evidence: found Object frame (already started)"
                );
                // If we've already started processing the object, we need to look at its entries
                // This shouldn't happen normally, but let's handle it
                return entries
                    .iter()
                    .map(|(key, _value)| {
                        facet_format::FieldEvidence::new(
                            Cow::Borrowed(*key),
                            FieldLocationHint::KeyValue,
                            None,
                        )
                    })
                    .collect();
            }
            Some(_) => {
                tracing::debug!("build_probe_evidence: unexpected frame type (Array or Enum)");
                return Vec::new();
            }
            None => {
                tracing::debug!("build_probe_evidence: empty stack");
                return Vec::new();
            }
        };

        // If it's an object, collect field evidence
        match root_value {
            ConfigValue::Object(sourced) => {
                tracing::debug!(
                    num_fields = sourced.value.len(),
                    "build_probe_evidence: found Object"
                );
                sourced
                    .value
                    .iter()
                    .map(|(key, value)| {
                        // Capture scalar values for tag disambiguation
                        let scalar_value = match value {
                            ConfigValue::String(s) => {
                                Some(ScalarValue::Str(Cow::Borrowed(&s.value)))
                            }
                            ConfigValue::Bool(b) => Some(ScalarValue::Bool(b.value)),
                            ConfigValue::Integer(i) => Some(ScalarValue::I64(i.value)),
                            ConfigValue::Float(f) => Some(ScalarValue::F64(f.value)),
                            ConfigValue::Null(_) => Some(ScalarValue::Null),
                            _ => None, // Objects/arrays are not captured
                        };

                        if let Some(sv) = scalar_value {
                            facet_format::FieldEvidence::with_scalar_value(
                                Cow::Borrowed(key.as_str()),
                                FieldLocationHint::KeyValue,
                                None,
                                sv,
                            )
                        } else {
                            facet_format::FieldEvidence::new(
                                Cow::Borrowed(key.as_str()),
                                FieldLocationHint::KeyValue,
                                None,
                            )
                        }
                    })
                    .collect()
            }
            ConfigValue::Enum(sourced) => {
                // For enum variants, the variant name IS the field key
                // This matches the externally-tagged format: {"VariantName": {...}}
                vec![facet_format::FieldEvidence::new(
                    Cow::Borrowed(sourced.value.variant.as_str()),
                    FieldLocationHint::KeyValue,
                    None,
                )]
            }
            _ => Vec::new(),
        }
    }
}

impl<'input> FormatParser<'input> for ConfigValueParser<'input> {
    type Error = ConfigValueParseError;
    type Probe<'a>
        = ConfigValueProbe<'input>
    where
        Self: 'a;

    fn next_event(&mut self) -> Result<Option<ParseEvent<'input>>, Self::Error> {
        // If we have a peeked event, return it
        if let Some(event) = self.peeked.take() {
            return Ok(Some(event));
        }

        loop {
            let frame = match self.stack.pop() {
                Some(f) => f,
                None => return Ok(None), // Done
            };

            match frame {
                StackFrame::Value(value) => {
                    return Ok(Some(self.emit_value(value)?));
                }
                StackFrame::Object { entries, index } => {
                    if index < entries.len() {
                        // Emit the next key-value pair
                        let (key, value) = entries[index];

                        // Push continuation for next entry
                        self.stack.push(StackFrame::Object {
                            entries: entries.clone(),
                            index: index + 1,
                        });

                        // Push value to process after key
                        self.stack.push(StackFrame::Value(value));

                        // Emit key as-is (no Shape-based translation)
                        return Ok(Some(ParseEvent::FieldKey(FieldKey::new(
                            key,
                            FieldLocationHint::KeyValue,
                        ))));
                    } else {
                        // Object entries done
                        return Ok(Some(ParseEvent::StructEnd));
                    }
                }
                StackFrame::Array { items, index } => {
                    if index < items.len() {
                        // Push continuation for next item
                        self.stack.push(StackFrame::Array {
                            items,
                            index: index + 1,
                        });

                        // Push item to process
                        self.stack.push(StackFrame::Value(&items[index]));

                        // Continue to process the value
                        continue;
                    } else {
                        // Array is done
                        return Ok(Some(ParseEvent::SequenceEnd));
                    }
                }
                StackFrame::Enum {
                    variant,
                    fields,
                    phase,
                    is_unit,
                } => {
                    match phase {
                        0 => {
                            // Phase 0: emit FieldKey with variant name, then continue to phase 1
                            self.stack.push(StackFrame::Enum {
                                variant,
                                fields,
                                phase: 1,
                                is_unit,
                            });
                            return Ok(Some(ParseEvent::FieldKey(FieldKey::new(
                                variant,
                                FieldLocationHint::KeyValue,
                            ))));
                        }
                        1 => {
                            // Phase 1: emit variant content
                            // Push phase 2 to emit StructEnd after content
                            self.stack.push(StackFrame::Enum {
                                variant,
                                fields: Vec::new(), // no longer needed
                                phase: 2,
                                is_unit: true, // not used in phase 2
                            });

                            if is_unit {
                                // Unit variant: emit Unit scalar
                                return Ok(Some(ParseEvent::Scalar(ScalarValue::Unit)));
                            } else {
                                // Struct/tuple variant: emit StructStart and push Object processing
                                self.stack.push(StackFrame::Object {
                                    entries: fields,
                                    index: 0,
                                });
                                return Ok(Some(ParseEvent::StructStart(ContainerKind::Object)));
                            }
                        }
                        _ => {
                            // Phase 2: emit outer StructEnd (the enum wrapper)
                            return Ok(Some(ParseEvent::StructEnd));
                        }
                    }
                }
            }
        }
    }

    fn peek_event(&mut self) -> Result<Option<ParseEvent<'input>>, Self::Error> {
        if self.peeked.is_none() {
            self.peeked = self.next_event()?;
        }
        Ok(self.peeked.clone())
    }

    fn skip_value(&mut self) -> Result<(), Self::Error> {
        // Pop and discard the next value
        self.next_event()?;
        Ok(())
    }

    fn begin_probe(&mut self) -> Result<Self::Probe<'_>, Self::Error> {
        // Build probe evidence from the current value
        // We look at the first stack frame which should be the root value
        let evidence = self.build_probe_evidence();
        Ok(ConfigValueProbe { evidence, index: 0 })
    }

    fn current_span(&self) -> Option<Span> {
        // Return the last span we saw - this is a "virtual" span that maps back
        // to the real source location via the SpanRegistry
        self.last_span
    }
}

/// Probe stream for ConfigValueParser that yields field evidence.
pub struct ConfigValueProbe<'input> {
    evidence: Vec<facet_format::FieldEvidence<'input>>,
    index: usize,
}

impl<'input> facet_format::ProbeStream<'input> for ConfigValueProbe<'input> {
    type Error = ConfigValueParseError;

    fn next(&mut self) -> Result<Option<facet_format::FieldEvidence<'input>>, Self::Error> {
        if self.index < self.evidence.len() {
            let ev = self.evidence[self.index].clone();
            self.index += 1;
            Ok(Some(ev))
        } else {
            Ok(None)
        }
    }
}

/// Helper methods for emitting values.
impl<'input> ConfigValueParser<'input> {
    /// Emit an event for a single value.
    fn emit_value(
        &mut self,
        value: &'input ConfigValue,
    ) -> Result<ParseEvent<'input>, ConfigValueParseError> {
        match value {
            ConfigValue::Null(sourced) => {
                self.update_span(sourced);
                Ok(ParseEvent::Scalar(ScalarValue::Null))
            }
            ConfigValue::Bool(sourced) => {
                self.update_span(sourced);
                Ok(ParseEvent::Scalar(ScalarValue::Bool(sourced.value)))
            }
            ConfigValue::Integer(sourced) => {
                self.update_span(sourced);
                Ok(ParseEvent::Scalar(ScalarValue::I64(sourced.value)))
            }
            ConfigValue::Float(sourced) => {
                self.update_span(sourced);
                Ok(ParseEvent::Scalar(ScalarValue::F64(sourced.value)))
            }
            ConfigValue::String(sourced) => {
                self.update_span(sourced);
                Ok(ParseEvent::Scalar(ScalarValue::Str(
                    std::borrow::Cow::Borrowed(&sourced.value),
                )))
            }
            ConfigValue::Array(sourced) => {
                self.update_span(sourced);

                // Push array processing
                self.stack.push(StackFrame::Array {
                    items: &sourced.value,
                    index: 0,
                });

                Ok(ParseEvent::SequenceStart(ContainerKind::Array))
            }
            ConfigValue::Object(sourced) => {
                self.update_span(sourced);

                // Collect entries - just emit what's in the ConfigValue
                let entries: Vec<(&str, &ConfigValue)> =
                    sourced.value.iter().map(|(k, v)| (k.as_str(), v)).collect();

                // Push object processing
                self.stack.push(StackFrame::Object { entries, index: 0 });

                Ok(ParseEvent::StructStart(ContainerKind::Object))
            }
            ConfigValue::Enum(sourced) => {
                self.update_span(sourced);

                // Collect entries - just emit what's in the ConfigValue
                let fields: Vec<(&str, &ConfigValue)> = sourced
                    .value
                    .fields
                    .iter()
                    .map(|(k, v)| (k.as_str(), v))
                    .collect();

                // Always treat enum variants as struct variants (emit StructStart/StructEnd).
                // We can't distinguish unit variants from struct variants with all-default fields
                // based on empty `fields` alone. The facet deserializer can handle empty structs.
                let is_unit = false;

                // Push enum processing frame (phase 0)
                // This emits: StructStart, FieldKey(variant), content, StructEnd
                // which is the externally-tagged format: {"VariantName": {...}}
                self.stack.push(StackFrame::Enum {
                    variant: &sourced.value.variant,
                    fields,
                    phase: 0,
                    is_unit,
                });

                // Emit the outer struct start (the enum wrapper)
                Ok(ParseEvent::StructStart(ContainerKind::Object))
            }
        }
    }
}

/// Errors that can occur while parsing a `ConfigValue`.
#[derive(Debug)]
pub enum ConfigValueParseError {
    /// Generic error message.
    Message(String),
}

impl core::fmt::Display for ConfigValueParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ConfigValueParseError::Message(msg) => write!(f, "{}", msg),
        }
    }
}

impl core::error::Error for ConfigValueParseError {}

/// Serializer that builds a ConfigValue tree.
pub struct ConfigValueSerializer {
    stack: Vec<BuildFrame>,
}

impl Default for ConfigValueSerializer {
    fn default() -> Self {
        Self {
            stack: vec![BuildFrame::Root(None)],
        }
    }
}

enum BuildFrame {
    Root(Option<ConfigValue>),
    Object {
        map: IndexMap<String, ConfigValue, std::hash::RandomState>,
    },
    Array {
        items: Vec<ConfigValue>,
    },
    PendingField {
        map: IndexMap<String, ConfigValue, std::hash::RandomState>,
        key: String,
    },
}

impl ConfigValueSerializer {
    /// Create a new ConfigValueSerializer.
    pub fn new() -> Self {
        Default::default()
    }

    /// Finish serialization and return the final ConfigValue.
    pub fn finish(mut self) -> ConfigValue {
        match self.stack.pop() {
            Some(BuildFrame::Root(Some(value))) if self.stack.is_empty() => value,
            Some(BuildFrame::Root(None)) if self.stack.is_empty() => {
                // No value was serialized, return null
                ConfigValue::Null(Sourced {
                    value: (),
                    span: None,
                    provenance: Some(Provenance::Default),
                })
            }
            _ => panic!("Serializer finished in unexpected state"),
        }
    }

    fn attach_value(&mut self, value: ConfigValue) -> Result<(), String> {
        let parent = self.stack.pop().ok_or("Stack underflow")?;
        match parent {
            BuildFrame::Root(None) => {
                self.stack.push(BuildFrame::Root(Some(value)));
                Ok(())
            }
            BuildFrame::Root(Some(_)) => Err("Root already has a value")?,
            BuildFrame::PendingField { mut map, key } => {
                map.insert(key, value);
                self.stack.push(BuildFrame::Object { map });
                Ok(())
            }
            BuildFrame::Array { mut items } => {
                items.push(value);
                self.stack.push(BuildFrame::Array { items });
                Ok(())
            }
            BuildFrame::Object { .. } => {
                Err("Cannot attach value directly to Object without field_key")?
            }
        }
    }
}

impl facet_format::FormatSerializer for ConfigValueSerializer {
    type Error = String;

    fn begin_struct(&mut self) -> Result<(), Self::Error> {
        self.stack.push(BuildFrame::Object {
            map: IndexMap::default(),
        });
        Ok(())
    }

    fn field_key(&mut self, key: &str) -> Result<(), Self::Error> {
        let frame = self.stack.pop().ok_or("Stack underflow")?;
        match frame {
            BuildFrame::Object { map } => {
                self.stack.push(BuildFrame::PendingField {
                    map,
                    key: key.to_string(),
                });
                Ok(())
            }
            _ => Err("field_key called outside of struct")?,
        }
    }

    fn end_struct(&mut self) -> Result<(), Self::Error> {
        let frame = self.stack.pop().ok_or("Stack underflow")?;
        match frame {
            BuildFrame::Object { map } => {
                let value = ConfigValue::Object(Sourced {
                    value: map,
                    span: None,
                    provenance: Some(Provenance::Default),
                });
                self.attach_value(value)
            }
            _ => Err("end_struct called without matching begin_struct")?,
        }
    }

    fn begin_seq(&mut self) -> Result<(), Self::Error> {
        self.stack.push(BuildFrame::Array { items: Vec::new() });
        Ok(())
    }

    fn end_seq(&mut self) -> Result<(), Self::Error> {
        let frame = self.stack.pop().ok_or("Stack underflow")?;
        match frame {
            BuildFrame::Array { items } => {
                let value = ConfigValue::Array(Sourced {
                    value: items,
                    span: None,
                    provenance: Some(Provenance::Default),
                });
                self.attach_value(value)
            }
            _ => Err("end_seq called without matching begin_seq")?,
        }
    }

    fn scalar(&mut self, value: facet_format::ScalarValue) -> Result<(), Self::Error> {
        use facet_format::ScalarValue;

        let config_value = match value {
            ScalarValue::Unit | ScalarValue::Null => ConfigValue::Null(Sourced {
                value: (),
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::Bool(b) => ConfigValue::Bool(Sourced {
                value: b,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::I64(i) => ConfigValue::Integer(Sourced {
                value: i,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::U64(u) => ConfigValue::Integer(Sourced {
                value: u as i64,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::I128(i) => ConfigValue::Integer(Sourced {
                value: i as i64,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::U128(u) => ConfigValue::Integer(Sourced {
                value: u as i64,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::F64(f) => ConfigValue::Float(Sourced {
                value: f,
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::Char(c) => ConfigValue::String(Sourced {
                value: c.to_string(),
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::Str(s) => ConfigValue::String(Sourced {
                value: s.into_owned(),
                span: None,
                provenance: Some(Provenance::Default),
            }),
            ScalarValue::Bytes(_) => {
                return Err("Bytes not supported in ConfigValue")?;
            }
        };

        self.attach_value(config_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::string::ToString;

    #[test]
    fn test_parse_null() {
        let value = ConfigValue::Null(Sourced::new(()));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::Scalar(ScalarValue::Null))));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_parse_bool() {
        let value = ConfigValue::Bool(Sourced::new(true));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::Bool(true)))
        ));
    }

    #[test]
    fn test_parse_integer() {
        let value = ConfigValue::Integer(Sourced::new(42));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(42)))
        ));
    }

    #[test]
    fn test_parse_string() {
        let value = ConfigValue::String(Sourced::new("hello".to_string()));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        if let Some(ParseEvent::Scalar(ScalarValue::Str(s))) = event {
            assert_eq!(s.as_ref(), "hello");
        } else {
            panic!("expected string scalar");
        }
    }

    #[test]
    fn test_parse_empty_array() {
        let value = ConfigValue::Array(Sourced::new(vec![]));
        let mut parser = ConfigValueParser::new(&value);

        // Should emit SequenceStart, then SequenceEnd
        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::SequenceStart(ContainerKind::Array))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::SequenceEnd)));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_parse_array_with_items() {
        let value = ConfigValue::Array(Sourced::new(vec![
            ConfigValue::Integer(Sourced::new(1)),
            ConfigValue::Integer(Sourced::new(2)),
            ConfigValue::Integer(Sourced::new(3)),
        ]));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::SequenceStart(_))));

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(1)))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(2)))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(3)))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::SequenceEnd)));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_parse_empty_object() {
        let value = ConfigValue::Object(Sourced::new(indexmap::IndexMap::default()));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::StructStart(ContainerKind::Object))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::StructEnd)));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_parse_object_with_fields() {
        let mut map = indexmap::IndexMap::default();
        map.insert(
            "name".to_string(),
            ConfigValue::String(Sourced::new("Alice".to_string())),
        );
        map.insert("age".to_string(), ConfigValue::Integer(Sourced::new(30)));

        let value = ConfigValue::Object(Sourced::new(map));
        let mut parser = ConfigValueParser::new(&value);

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::StructStart(_))));

        // First field
        let event = parser.next_event().unwrap();
        if let Some(ParseEvent::FieldKey(key)) = event {
            assert_eq!(key.name.as_ref().map(|s| s.as_ref()), Some("name"));
        } else {
            panic!("expected FieldKey");
        }

        let event = parser.next_event().unwrap();
        if let Some(ParseEvent::Scalar(ScalarValue::Str(s))) = event {
            assert_eq!(s.as_ref(), "Alice");
        } else {
            panic!("expected string value");
        }

        // Second field
        let event = parser.next_event().unwrap();
        if let Some(ParseEvent::FieldKey(key)) = event {
            assert_eq!(key.name.as_ref().map(|s| s.as_ref()), Some("age"));
        } else {
            panic!("expected FieldKey");
        }

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(30)))
        ));

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::StructEnd)));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_from_config_value_simple() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq)]
        struct SimpleConfig {
            port: i64,
            enabled: bool,
        }

        let mut map = indexmap::IndexMap::default();
        map.insert("port".to_string(), ConfigValue::Integer(Sourced::new(8080)));
        map.insert("enabled".to_string(), ConfigValue::Bool(Sourced::new(true)));

        let value = ConfigValue::Object(Sourced::new(map));
        let config: SimpleConfig = from_config_value(&value).expect("should deserialize");

        assert_eq!(config.port, 8080);
        assert!(config.enabled);
    }

    #[test]
    fn test_from_config_value_nested() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq)]
        struct SmtpConfig {
            host: String,
            port: i64,
        }

        #[derive(Debug, Facet, PartialEq)]
        struct ServerConfig {
            port: i64,
            smtp: SmtpConfig,
        }

        // Build nested config value
        let mut smtp_map = indexmap::IndexMap::default();
        smtp_map.insert(
            "host".to_string(),
            ConfigValue::String(Sourced::new("smtp.example.com".to_string())),
        );
        smtp_map.insert("port".to_string(), ConfigValue::Integer(Sourced::new(587)));

        let mut server_map = indexmap::IndexMap::default();
        server_map.insert("port".to_string(), ConfigValue::Integer(Sourced::new(8080)));
        server_map.insert(
            "smtp".to_string(),
            ConfigValue::Object(Sourced::new(smtp_map)),
        );

        let value = ConfigValue::Object(Sourced::new(server_map));
        let config: ServerConfig = from_config_value(&value).expect("should deserialize");

        assert_eq!(config.port, 8080);
        assert_eq!(config.smtp.host, "smtp.example.com");
        assert_eq!(config.smtp.port, 587);
    }

    // ========================================================================
    // Tests: fill_defaults_from_shape
    // ========================================================================

    #[test]
    fn test_fill_defaults_simple_struct() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq)]
        struct Config {
            #[facet(default)]
            enabled: bool,
            port: i64,
        }

        // Input has port but not enabled
        let mut map = indexmap::IndexMap::default();
        map.insert("port".to_string(), ConfigValue::Integer(Sourced::new(8080)));
        let input = ConfigValue::Object(Sourced::new(map));

        let result = fill_defaults_from_shape(&input, Config::SHAPE);

        // Should have both fields
        if let ConfigValue::Object(obj) = result {
            assert!(obj.value.contains_key("port"), "should have port");
            assert!(
                obj.value.contains_key("enabled"),
                "should have enabled with default"
            );
            if let ConfigValue::Bool(b) = &obj.value["enabled"] {
                assert!(!b.value, "enabled default should be false");
            } else {
                panic!("enabled should be a bool");
            }
        } else {
            panic!("expected object");
        }
    }

    #[test]
    fn test_fill_defaults_with_flatten() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq, Default)]
        struct CommonOpts {
            #[facet(default)]
            verbose: bool,
            #[facet(default)]
            debug: bool,
        }

        #[derive(Debug, Facet, PartialEq)]
        struct Config {
            name: String,
            #[facet(flatten)]
            common: CommonOpts,
        }

        // Input has only name - common fields should be flattened as defaults
        let mut map = indexmap::IndexMap::default();
        map.insert(
            "name".to_string(),
            ConfigValue::String(Sourced::new("test".to_string())),
        );
        let input = ConfigValue::Object(Sourced::new(map));

        let result = fill_defaults_from_shape(&input, Config::SHAPE);

        // For flattened fields, the defaults should be at the TOP level
        // (verbose, debug) NOT nested under "common"
        if let ConfigValue::Object(obj) = result {
            assert!(obj.value.contains_key("name"), "should have name");

            // The key insight: flattened fields should NOT create a nested "common" object.
            // Instead, verbose and debug should be at the top level.
            assert!(
                obj.value.contains_key("verbose"),
                "should have verbose at top level (flattened)"
            );
            assert!(
                obj.value.contains_key("debug"),
                "should have debug at top level (flattened)"
            );
            assert!(
                !obj.value.contains_key("common"),
                "should NOT have nested 'common' object"
            );
        } else {
            panic!("expected object");
        }
    }

    #[test]
    fn test_fill_defaults_flatten_preserves_existing() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq, Default)]
        struct CommonOpts {
            #[facet(default)]
            verbose: bool,
            #[facet(default)]
            debug: bool,
        }

        #[derive(Debug, Facet, PartialEq)]
        struct Config {
            name: String,
            #[facet(flatten)]
            common: CommonOpts,
        }

        // Input has name and verbose (already provided)
        let mut map = indexmap::IndexMap::default();
        map.insert(
            "name".to_string(),
            ConfigValue::String(Sourced::new("test".to_string())),
        );
        map.insert("verbose".to_string(), ConfigValue::Bool(Sourced::new(true)));
        let input = ConfigValue::Object(Sourced::new(map));

        let result = fill_defaults_from_shape(&input, Config::SHAPE);

        if let ConfigValue::Object(obj) = result {
            // verbose should be preserved as true
            if let ConfigValue::Bool(b) = &obj.value["verbose"] {
                assert!(b.value, "verbose should be true (preserved)");
            }
            // debug should be filled with default false
            assert!(obj.value.contains_key("debug"), "should have debug");
            if let ConfigValue::Bool(b) = &obj.value["debug"] {
                assert!(!b.value, "debug should be false (default)");
            }
        } else {
            panic!("expected object");
        }
    }

    #[test]
    fn test_fill_defaults_subcommand_with_flatten() {
        use facet::Facet;

        // This reproduces the exact issue from GitHub #3

        #[derive(Debug, Facet, PartialEq, Default)]
        struct Builtins {
            #[facet(default)]
            help: bool,
            #[facet(default)]
            version: bool,
        }

        #[derive(Debug, Facet, PartialEq)]
        #[repr(u8)]
        enum Command {
            Build { release: bool },
        }

        #[derive(Debug, Facet)]
        struct Args {
            command: Command,
            #[facet(flatten)]
            builtins: Builtins,
        }

        // Input: command is present, builtins fields should be flattened
        let mut fields = indexmap::IndexMap::default();
        fields.insert("release".to_string(), ConfigValue::Bool(Sourced::new(true)));
        let enum_value = ConfigValue::Enum(Sourced::new(crate::config_value::EnumValue {
            variant: "Build".to_string(),
            fields,
        }));

        let mut map = indexmap::IndexMap::default();
        map.insert("command".to_string(), enum_value);
        let input = ConfigValue::Object(Sourced::new(map));

        let result = fill_defaults_from_shape(&input, Args::SHAPE);

        if let ConfigValue::Object(obj) = result {
            // command should be preserved
            assert!(obj.value.contains_key("command"), "should have command");

            // builtins fields should be flattened at top level
            assert!(
                obj.value.contains_key("help"),
                "should have help at top level"
            );
            assert!(
                obj.value.contains_key("version"),
                "should have version at top level"
            );
            assert!(
                !obj.value.contains_key("builtins"),
                "should NOT have nested 'builtins'"
            );
        } else {
            panic!("expected object");
        }
    }

    #[facet_testhelpers::test]
    fn test_full_deserialization_with_flatten() {
        use facet::Facet;

        #[derive(Debug, Facet, PartialEq, Default)]
        struct CommonOpts {
            #[facet(default)]
            verbose: bool,
        }

        #[derive(Debug, Facet, PartialEq)]
        struct Config {
            name: String,
            #[facet(flatten)]
            common: CommonOpts,
        }

        // Build ConfigValue with flattened structure (as CLI parser would produce)
        let mut map = indexmap::IndexMap::default();
        map.insert(
            "name".to_string(),
            ConfigValue::String(Sourced::new("test".to_string())),
        );
        map.insert("verbose".to_string(), ConfigValue::Bool(Sourced::new(true)));
        let input = ConfigValue::Object(Sourced::new(map));

        // This should deserialize correctly with flattened fields at top level
        let config: Config = from_config_value(&input).expect("should deserialize");

        assert_eq!(config.name, "test");
        assert!(config.common.verbose);
    }
}
