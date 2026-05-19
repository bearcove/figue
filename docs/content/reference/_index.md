+++
title = "Reference"
sort_by = "weight"
weight = 3
insert_anchor_links = "heading"
+++

Exhaustive, look-it-up-fast documentation. Where the [Guide](@/guide/_index.md)
teaches, the reference states the exact rules.

- [Attributes](@/reference/attributes.md) — every `#[facet(args::…)]` and relevant facet attribute
- [Entry Points](@/reference/entry-points.md) — `from_slice`, `from_std_args`, `builder`, `FigueBuiltins`
- [Builder API](@/reference/builder-api.md) — `ConfigBuilder`, `Driver`, `DriverOutcome`, layer builders
- [Supported Types](@/reference/supported-types.md) — which Rust types parse, and how
- [CLI Grammar](@/reference/cli-grammar.md) — the exact command-line parsing rules
- [Environment Variables](@/reference/environment-variables.md) — exact env var naming and value rules
- [Config File Formats](@/reference/config-file-formats.md) — JSON, JSONC, and the `ConfigFormat` trait
- [Merge Precedence](@/reference/merge-precedence.md) — how layers combine, field by field
- [Errors](@/reference/errors.md) — `DriverError`, `SchemaError`, exit codes, the full catalog
- [JSON Schema Output](@/reference/json-schema.md) — the exact shape of exported schemas
