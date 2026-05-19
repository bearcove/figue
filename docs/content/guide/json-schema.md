+++
title = "JSON Schema Export"
weight = 10
insert_anchor_links = "heading"
+++

Your config structs already describe the shape of a valid config file. figue can
emit that description as a [JSON Schema](https://json-schema.org/) (draft
2020-12) so editors give your users autocomplete, inline docs, and validation
for free.

## From the command line

If you flattened [`FigueBuiltins`](@/guide/help-and-completions.md#figuebuiltins),
you already have the flag:

```bash
myapp --export-jsonschemas ./schemas
# Wrote JSON Schema files:
# ./schemas/config.schema.json
```

One file is written per `#[facet(args::config)]` root, named
`<rootfield>.schema.json`. The directory is created if needed. The process exits
`0`.

## From code

`builder` and `Config` expose the same thing programmatically — useful in a
`build.rs`, an `xtask`, or a test that keeps a checked-in schema fresh:

```rust,noexec
use figue::builder;

let b = builder::<Args>().unwrap();

// In-memory: Vec<JsonSchemaFile> { file_name, contents }
let files = b.generate_json_schemas();

// Or straight to disk:
let paths = b.write_json_schemas("./schemas").unwrap();
```

## What the schema looks like

Given:

```rust,noexec
#[derive(Facet)]
struct AppConfig {
    /// Server hostname.
    #[facet(default = "localhost")]
    host: String,
    /// Maximum retry attempts.
    #[facet(rename = "max-retries", default = 3)]
    max_retries: u32,
    /// Optional TLS settings.
    #[facet(default)]
    tls: Option<TlsConfig>,
}

#[derive(Facet)]
struct TlsConfig {
    cert_path: String,
    key_path: String,
}
```

figue emits:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "AppConfig",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "$schema": {
      "type": "string",
      "description": "Path or URL of the JSON Schema this file conforms to."
    },
    "host": { "type": "string", "description": "Server hostname.", "default": "localhost" },
    "max-retries": { "type": "integer", "description": "Maximum retry attempts.", "default": 3 },
    "tls": {
      "anyOf": [
        {
          "type": "object",
          "additionalProperties": false,
          "properties": {
            "cert_path": { "type": "string" },
            "key_path": { "type": "string" }
          },
          "required": ["cert_path", "key_path"]
        },
        { "type": "null" }
      ],
      "description": "Optional TLS settings."
    }
  }
}
```

Highlights:

- A synthetic `$schema` string property is injected first so users can write
  `"$schema": "./config.schema.json"` without tripping
  `additionalProperties: false`.
- Doc comments become `description`. `#[facet(default = …)]` becomes `default`.
  `#[facet(sensitive)]` becomes `"writeOnly": true`.
- `Option<T>` → `anyOf [T, null]`. `Vec<T>` → `array`. Enums with data →
  `oneOf` of externally-tagged objects; unit-only enums → a string `enum`.
- Renames (`max-retries`) and `rename_all` are honored in property names and
  enum values.

The exact type mapping table is in the
[JSON Schema reference](@/reference/json-schema.md).

## Wiring it into an editor

Point your editor at the file. For VS Code, in `.vscode/settings.json`:

```json
{
  "json.schemas": [
    { "fileMatch": ["myapp.json"], "url": "./schemas/config.schema.json" }
  ]
}
```

…or just add `"$schema": "./schemas/config.schema.json"` as the first key of the
config file itself. Either way your users get red squiggles on typos and
autocomplete on every key, generated from the same structs that parse the
config.

Next: [Requirements Extraction](@/guide/requirements-extraction.md).
