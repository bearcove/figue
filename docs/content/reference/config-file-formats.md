+++
title = "Config File Formats"
weight = 6
insert_anchor_links = "heading"
+++

What figue can read as a config file, and how to add formats.

## Built-in formats

| Format | Extensions | Backed by | Registered by default? |
|---|---|---|---|
| `JsonFormat` | `.json` | `facet_json::from_str` | **yes** |
| `JsoncFormat` | `.jsonc` | `facet_json::from_str_jsonc` (`//`, `/* */`) | **no — register it** |

The default registry contains **JSON only**. To accept JSONC (or anything
else), register the format on the file layer:

```rust,noexec
use figue::JsoncFormat;

builder::<Args>().unwrap()
    .file(|f| f.format(JsoncFormat).default_paths(["app.jsonc", "app.json"]))
    .build();
```

Extension matching is case-insensitive.

## File resolution

- An explicit path (the generated `--<root> <PATH>` flag) that does not exist is
  a **hard error** ("config file not found").
- Otherwise `default_paths` are tried in order; the **first existing** file is
  used. If none exist, that is **not** an error — the file layer is optional and
  figue falls back to env/defaults.
- No `~` / `$HOME` expansion. Paths are used verbatim via UTF-8 path + an
  existence check. Resolve home directories yourself and pass absolute paths.
- `.content(text, filename)` bypasses disk entirely; `filename` only drives
  format detection (used for tests).

## Multiple config roots in one file

With more than one `args::config` root, the file must be a top-level **object**;
each root's effective field name is a top-level key:

```json
{ "cfg": { "port": 8080 }, "eval": { "dataset": "/data" } }
```

A non-object file with multiple roots is an error
("config file with multiple config roots must be an object"). Each root is
validated independently; unknown keys are reported prefixed with the root name.

## Unknown keys / strict mode

Unknown keys are always tracked. Without `.strict()` they are ignored
(forward-compatible). With `.strict()` they are reported by the driver
(alongside the dump, with suggestions) — not at file-parse time. Flattened
fields must appear *flat* in the file; a nested object under the flattened
struct's name is an unknown key.

## The `ConfigFormat` trait

Implement this to support TOML, YAML, or any custom syntax. A format maps
extensions to a parser that returns a [`ConfigValue`](#configvalue):

```rust,noexec
use figue::{ConfigFormat, ConfigFormatError, ConfigValue};

struct TomlFormat;

impl ConfigFormat for TomlFormat {
    fn extensions(&self) -> &[&str] {
        &["toml"]
    }

    fn parse(&self, text: &str) -> Result<ConfigValue, ConfigFormatError> {
        // Parse `text` into a ConfigValue tree
        // (use facet-toml, or any parser, then build the ConfigValue).
        todo!()
    }
}

// Register it:
builder::<Args>().unwrap()
    .file(|f| f.format(TomlFormat).default_paths(["app.toml"]))
    .build();
```

Custom formats are responsible for the spans used in diagnostics; values without
spans still work but won't underline the offending line in the source file.

## ConfigValue

`figue::ConfigValue` is the dynamic value tree all layers normalize into before
merging and deserialization (object / array / string / integer / float / bool /
null / enum). You only touch it when writing a custom `ConfigFormat`; in that
case build the tree from your parser's output and return it from `parse`.

See [Merge Precedence](@/reference/merge-precedence.md) for how the resulting
tree combines with the other layers.
