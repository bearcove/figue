+++
title = "JSON Schema Output"
weight = 9
insert_anchor_links = "heading"
+++

The exact shape of schemas produced by `--export-jsonschemas`,
`generate_json_schemas`, and `write_json_schemas`. The
[JSON Schema Export guide](@/guide/json-schema.md) shows how to use them.

## What is emitted

- **One file per `#[facet(args::config)]` root**, named
  `<root-effective-name>.schema.json` (sanitized to `[A-Za-z0-9_.-]`, falling
  back to `config`).
- Dialect: `"$schema": "https://json-schema.org/draft/2020-12/schema"`.
- `title` = the config struct's Rust type name. `description` = the config root
  field's doc comment, if any.
- Every struct is `{"type":"object","additionalProperties":false,"properties":…,
  "required":[…]}` (the `required` array is omitted when empty).
- A synthetic `"$schema"` string property is injected as the **first** property
  of each config-root object, so users can add
  `"$schema": "./config.schema.json"` without violating
  `additionalProperties:false`.

## Type mapping

| Rust | JSON Schema |
|---|---|
| `bool` | `{"type":"boolean"}` |
| `String`, `PathBuf`, `IpAddr`, other "other" scalars | `{"type":"string"}` |
| integer types | `{"type":"integer"}` |
| `f32`, `f64` | `{"type":"number"}` |
| `Vec<T>` | `{"type":"array","items":<T>}` |
| `Option<T>` | `{"anyOf":[<T>,{"type":"null"}]}` |
| nested struct | recursive object schema |
| unit-only enum | `{"type":"string","enum":[<variant names>]}` |
| enum with ≥1 data variant | `{"oneOf":[…]}` — unit variants → `{"const":"name"}`; struct variants → externally-tagged `{type:object, properties:{ "<variant>": {...} }, required:["<variant>"]}` |

Per-field decorations:

- doc comment → `description` (summary, then `\n\n` + details when both exist).
- `#[facet(default = …)]` → `default` (the value serialized to JSON).
- `#[facet(sensitive)]` → `"writeOnly": true`.

## Required rule (schema-specific)

For JSON Schema, a property is in `required` iff it is **not** `Option<…>`
**and** has **no** default. This differs from CLI parsing: here a `bool`
without a default *is* required, and an enum-with-data field without a default
*is* required. (Parsing's `required` additionally excludes `bool` and counted
integers.) This only affects the emitted document.

## Naming and casing

Property names and enum values use the field/variant **effective name**:
`#[facet(rename = "max-retries")]` and `#[facet(rename_all = "kebab-case")]` are
reflected directly (so a kebab-cased enum yields lower-kebab `oneOf` keys, while
an un-renamed enum keeps PascalCase variant names in its `enum`/`const`).

## Example

```rust,noexec
#[derive(Facet)]
struct AppConfig {
    /// Server hostname.
    #[facet(default = "localhost")]
    host: String,
    /// Additional hostnames.
    #[facet(default)]
    aliases: Vec<String>,
    /// Storage backend.
    storage: Storage,
}

#[derive(Facet)]
#[facet(rename_all = "kebab-case")]
#[repr(u8)]
enum Storage {
    /// Local filesystem storage.
    Local { path: String },
    /// In-memory storage.
    Memory,
}
```

emits:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "AppConfig",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "$schema": { "type": "string", "description": "Path or URL of the JSON Schema this file conforms to." },
    "host": { "type": "string", "description": "Server hostname.", "default": "localhost" },
    "aliases": { "type": "array", "items": { "type": "string" }, "description": "Additional hostnames." },
    "storage": {
      "oneOf": [
        { "type": "object", "additionalProperties": false,
          "properties": { "local": { "type": "object", "additionalProperties": false,
            "properties": { "path": { "type": "string" } }, "required": ["path"] } },
          "required": ["local"], "description": "Local filesystem storage." },
        { "const": "memory", "description": "In-memory storage." }
      ],
      "description": "Storage backend."
    }
  },
  "required": ["aliases", "storage"]
}
```

Only `aliases` (a `Vec` with no usable default value) and `storage` (no
default) are required; `host` has a default and is omitted from `required`.

## API surface

| Call | Returns |
|---|---|
| `builder::<T>()?.generate_json_schemas()` | `Vec<JsonSchemaFile>` (`{ file_name, contents }`) |
| `builder::<T>()?.write_json_schemas(dir)` | `Result<Vec<PathBuf>, JsonSchemaError>` |
| `figue::generate_json_schemas::<T>()` | `Result<Vec<JsonSchemaFile>, JsonSchemaError>` |
| `figue::write_json_schemas::<T>(dir)` | `Result<Vec<PathBuf>, JsonSchemaError>` |
| `--export-jsonschemas <DIR>` | writes files, `DriverError::JsonSchemasExported` (exit 0) |

`Config<T>` (post-`build()`) has the same `generate`/`write` methods. Output is
pretty-printed with 2-space indentation and a trailing newline.
`JsonSchemaError` is `Schema(SchemaError) | Io(std::io::Error)`.
