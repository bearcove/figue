//! Parser that converts `ConfigValue` trees into `ParseEvent` streams for deserialization.
#![allow(dead_code)]
//!
//! This allows us to deserialize `ConfigValue` into arbitrary Facet types using the
//! standard `facet-format` deserializer infrastructure.

use std::vec::Vec;

use facet_core::{Facet, Shape, Type, UserType};
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

    let parser = ConfigValueParser::new(&value_coerced, T::SHAPE);
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
                if let Some(field) = fields.iter().find(|f| f.name == key) {
                    let field_path = if path_prefix.is_empty() {
                        key.to_string()
                    } else {
                        format!("{}.{}", path_prefix, key)
                    };
                    *val = fill_defaults_from_shape_recursive(val, field.shape.get(), &field_path);
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
/// This parser tracks shape context as it descends into nested structures,
/// allowing it to translate original field names (used in ConfigValue) to
/// effective names (respecting `#[facet(rename = "...")]` attributes).
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
    /// The shape is the struct shape, used to look up field metadata for rename translation.
    Object {
        entries: Vec<(&'input str, &'input ConfigValue, Option<&'static Shape>)>,
        index: usize,
        shape: Option<&'static Shape>,
    },
    /// Processing an array - emit items in sequence.
    Array {
        items: &'input [ConfigValue],
        index: usize,
        element_shape: Option<&'static Shape>,
    },
    /// A single value to emit with its expected shape.
    Value(&'input ConfigValue, Option<&'static Shape>),
    /// Processing an enum variant - externally tagged format: {"VariantName": {...}}
    /// Phase 0: emit FieldKey with variant name
    /// Phase 1: emit variant content (struct with fields, or Unit for unit variants)
    /// Phase 2: emit StructEnd
    Enum {
        variant: &'input str,
        fields: Vec<(&'input str, &'input ConfigValue, Option<&'static Shape>)>,
        phase: u8,
        shape: Option<&'static Shape>,
    },
}

impl<'input> ConfigValueParser<'input> {
    /// Create a new parser from a `ConfigValue`.
    pub fn new(value: &'input ConfigValue, target_shape: &'static Shape) -> Self {
        Self {
            stack: vec![StackFrame::Value(value, Some(target_shape))],
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

    /// Look up a field by its original name in a struct shape and return its effective name.
    /// Falls back to the original name if the shape is not a struct or field is not found.
    fn get_effective_field_name(
        original_name: &'input str,
        shape: Option<&'static Shape>,
    ) -> &'input str {
        if let Some(shape) = shape
            && let Type::User(UserType::Struct(s)) = &shape.ty
            && let Some(field) = s.fields.iter().find(|f| f.name == original_name)
        {
            // SAFETY: effective_name returns &'static str which outlives 'input
            return field.effective_name();
        }
        original_name
    }

    /// Get the shape of a field by its original name from a struct shape.
    fn get_field_shape(
        original_name: &str,
        shape: Option<&'static Shape>,
    ) -> Option<&'static Shape> {
        if let Some(shape) = shape
            && let Type::User(UserType::Struct(s)) = &shape.ty
            && let Some(field) = s.fields.iter().find(|f| f.name == original_name)
        {
            return Some(field.shape.get());
        }
        None
    }
}

impl<'input> FormatParser<'input> for ConfigValueParser<'input> {
    type Error = ConfigValueParseError;
    type Probe<'a>
        = EmptyProbe
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
                StackFrame::Value(value, shape) => {
                    return Ok(Some(self.emit_value(value, shape)?));
                }
                StackFrame::Object {
                    entries,
                    index,
                    shape,
                } => {
                    if index < entries.len() {
                        // Emit the next key-value pair
                        let (original_key, value, field_shape) = entries[index];

                        // Push continuation for next entry
                        self.stack.push(StackFrame::Object {
                            entries: entries.clone(),
                            index: index + 1,
                            shape,
                        });

                        // Push value to process after key (with the field's shape)
                        self.stack.push(StackFrame::Value(value, field_shape));

                        // Emit key using effective name (respects #[facet(rename = "...")])
                        let effective_key = Self::get_effective_field_name(original_key, shape);
                        return Ok(Some(ParseEvent::FieldKey(FieldKey::new(
                            effective_key,
                            FieldLocationHint::KeyValue,
                        ))));
                    } else {
                        // Object entries done
                        return Ok(Some(ParseEvent::StructEnd));
                    }
                }
                StackFrame::Array {
                    items,
                    index,
                    element_shape,
                } => {
                    if index < items.len() {
                        // Push continuation for next item
                        self.stack.push(StackFrame::Array {
                            items,
                            index: index + 1,
                            element_shape,
                        });

                        // Push item to process with element shape
                        self.stack
                            .push(StackFrame::Value(&items[index], element_shape));

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
                    shape,
                } => {
                    match phase {
                        0 => {
                            // Phase 0: emit FieldKey with variant name, then continue to phase 1
                            self.stack.push(StackFrame::Enum {
                                variant,
                                fields,
                                phase: 1,
                                shape,
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
                                shape: None,
                            });

                            if fields.is_empty() {
                                // Unit variant: emit Unit scalar
                                return Ok(Some(ParseEvent::Scalar(ScalarValue::Unit)));
                            } else {
                                // Struct variant: emit StructStart and push Object processing
                                self.stack.push(StackFrame::Object {
                                    entries: fields,
                                    index: 0,
                                    shape,
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
        // We don't need probing for ConfigValue (it's already parsed)
        Ok(EmptyProbe)
    }

    fn current_span(&self) -> Option<Span> {
        // Return the last span we saw - this is a "virtual" span that maps back
        // to the real source location via the SpanRegistry
        self.last_span
    }
}

/// Empty probe stream for ConfigValueParser (we don't need evidence collection).
pub struct EmptyProbe;

impl<'de> facet_format::ProbeStream<'de> for EmptyProbe {
    type Error = ConfigValueParseError;

    fn next(&mut self) -> Result<Option<facet_format::FieldEvidence<'de>>, Self::Error> {
        Ok(None)
    }
}

/// Helper methods for emitting values.
impl<'input> ConfigValueParser<'input> {
    /// Emit an event for a single value, tracking the shape for nested structures.
    fn emit_value(
        &mut self,
        value: &'input ConfigValue,
        shape: Option<&'static Shape>,
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

                // Get element shape from the array/vec shape
                let element_shape = shape.and_then(|s| s.inner);

                // Push array processing
                self.stack.push(StackFrame::Array {
                    items: &sourced.value,
                    index: 0,
                    element_shape,
                });

                Ok(ParseEvent::SequenceStart(ContainerKind::Array))
            }
            ConfigValue::Object(sourced) => {
                self.update_span(sourced);

                // Collect entries with their field shapes looked up from the struct shape
                let entries: Vec<(&str, &ConfigValue, Option<&'static Shape>)> = sourced
                    .value
                    .iter()
                    .map(|(k, v)| {
                        let field_shape = Self::get_field_shape(k.as_str(), shape);
                        (k.as_str(), v, field_shape)
                    })
                    .collect();

                // Push object processing with shape for field name translation
                self.stack.push(StackFrame::Object {
                    entries,
                    index: 0,
                    shape,
                });

                Ok(ParseEvent::StructStart(ContainerKind::Object))
            }
            ConfigValue::Enum(sourced) => {
                self.update_span(sourced);

                // For enums, find the variant's fields from the enum shape
                let variant_fields: Option<&'static [facet_core::Field]> = shape.and_then(|s| {
                    if let Type::User(UserType::Enum(e)) = &s.ty {
                        e.variants
                            .iter()
                            .find(|v| v.name == sourced.value.variant)
                            .map(|v| v.data.fields)
                    } else {
                        None
                    }
                });

                // Collect entries with their field shapes from the variant's fields
                let fields: Vec<(&str, &ConfigValue, Option<&'static Shape>)> = sourced
                    .value
                    .fields
                    .iter()
                    .map(|(k, v)| {
                        let field_shape = variant_fields.and_then(|fields| {
                            fields
                                .iter()
                                .find(|f| f.name == k.as_str())
                                .map(|f| f.shape.get())
                        });
                        (k.as_str(), v, field_shape)
                    })
                    .collect();

                // Push enum processing frame (phase 0)
                // This emits: StructStart, FieldKey(variant), content, StructEnd
                // which is the externally-tagged format: {"VariantName": {...}}
                self.stack.push(StackFrame::Enum {
                    variant: &sourced.value.variant,
                    fields,
                    phase: 0,
                    shape: None, // Enum variants use fields directly, not a shape wrapper
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
        let mut parser = ConfigValueParser::new(&value, <()>::SHAPE);

        let event = parser.next_event().unwrap();
        assert!(matches!(event, Some(ParseEvent::Scalar(ScalarValue::Null))));

        let event = parser.next_event().unwrap();
        assert!(event.is_none());
    }

    #[test]
    fn test_parse_bool() {
        let value = ConfigValue::Bool(Sourced::new(true));
        let mut parser = ConfigValueParser::new(&value, <bool>::SHAPE);

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::Bool(true)))
        ));
    }

    #[test]
    fn test_parse_integer() {
        let value = ConfigValue::Integer(Sourced::new(42));
        let mut parser = ConfigValueParser::new(&value, <i64>::SHAPE);

        let event = parser.next_event().unwrap();
        assert!(matches!(
            event,
            Some(ParseEvent::Scalar(ScalarValue::I64(42)))
        ));
    }

    #[test]
    fn test_parse_string() {
        let value = ConfigValue::String(Sourced::new("hello".to_string()));
        let mut parser = ConfigValueParser::new(&value, <String>::SHAPE);

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
        let mut parser = ConfigValueParser::new(&value, <Vec<i32>>::SHAPE);

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
        let mut parser = ConfigValueParser::new(&value, <Vec<i64>>::SHAPE);

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
        let mut parser = ConfigValueParser::new(&value, <()>::SHAPE);

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
        let mut parser = ConfigValueParser::new(&value, <()>::SHAPE);

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
}
