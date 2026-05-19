+++
title = "Supported Types"
weight = 3
insert_anchor_links = "heading"
+++

Which Rust field types figue can fill, and the exact coercion from the string a
CLI/env/file value arrives as.

## Scalars

| Type | Parsing | Notes |
|---|---|---|
| `String`, `Cow<str>` | passthrough | no coercion |
| `char` | facet's scalar parse | single character |
| `bool` | `true/1/yes/on` ↔ `false/0/no/off` | trimmed, case-insensitive; anything else stays a string → deserialize error |
| `i8 i16 i32 i64 i128 isize` | parsed as `i64`, range-checked | out-of-range value is left as a string so facet emits a precise range error |
| `u8 u16 u32 u64 u128 usize` | parsed as `i64`, must be ≥ 0 and ≤ MAX | negatives left as string → error |
| `f32`, `f64` | `parse::<f64>()` | accepts `inf`, `infinity`, integer literals |

Empty string never coerces to a number. Invalid numeric strings are passed
through so the deserializer produces the exact "failed to parse … as …"
message, pointed at the source.

## "Other" scalars (FromStr-style)

Types figue doesn't special-case are passed through as strings and parsed by
facet's scalar machinery. This includes, among others:

- `std::path::PathBuf`, `camino::Utf8PathBuf`
- `std::net::IpAddr`, `Ipv4Addr`, `Ipv6Addr`, `SocketAddr`
- any other type facet knows how to parse from a string

```rust,noexec
use std::net::IpAddr;
use std::path::PathBuf;

#[derive(facet::Facet, Debug)]
struct Args {
    #[facet(figue::named, default)]
    bind: Option<IpAddr>,        // --bind 127.0.0.1
    #[facet(figue::named, default)]
    out: Option<PathBuf>,        // --out ./dist
}
```

## Collections

| Type | CLI | Env | File |
|---|---|---|---|
| `Vec<T>` | repeated flag, or repeated positional | comma-separated (escape `\,`); single value = 1-element | JSON array |
| `Option<T>` | absent ⇒ `None`, present ⇒ `Some` | same | `null` or absent ⇒ `None` |
| `HashMap<K,V>`, `IndexMap<K,V>` | — | — | JSON object; keys verbatim |

A scalar destined for a `Vec<T>` is wrapped into a one-element list. Across
layers, lists **replace** rather than append — see
[Merge Precedence](@/reference/merge-precedence.md).

## Structs

Nested structs are filled field-by-field by matching effective names
(case-insensitive, kebab-aware).

In an **args** struct (not a config root) a struct-typed field **must** be
`#[facet(flatten)]` or be a `#[facet(args::config)]` root — otherwise it's a
[schema error](@/reference/errors.md#schemaerror). Inside a **config** struct,
nesting is free; flattening still works.

## Enums

`#[repr(u8)]` is required for any data-carrying enum.

| Variant kind | CLI / env value | File representation |
|---|---|---|
| Unit (`Memory`) | the kebab/renamed name (`memory`) | `"memory"` |
| Struct (`S3 { bucket, … }`) | descend into fields | `{ "s3": { "bucket": … } }` (externally tagged) |
| Newtype around a struct (`Foo(Inner)`) | fields flattened, no `"0"` | fields flattened |

`#[facet(rename = "…")]` on a variant and `#[facet(rename_all = "kebab-case")]`
on the enum are honored everywhere (matching, JSON Schema `enum`/`oneOf`,
completions). An unknown enum value from env/file is a *warning*; the raw value
still flows to deserialization which produces the final error. CLI enum values
are validated at deserialize time.

## Defaults and `required`

A field is **required** (for CLI parsing and the dump) unless it is `Option<_>`,
has `#[facet(default[ = …]]`, is a `bool`, is a counted integer, or is an
optional subcommand. `#[facet(default = expr)]` values are serialized at
schema-build time and shown as `DEFAULT`; if a default can't be represented as a
config value, the field is left absent so facet applies it directly.

> JSON Schema export uses a different `required` rule (a `bool` without a
> default *is* required there). That affects only the emitted schema. See
> [JSON Schema Output](@/reference/json-schema.md).

## Special figue types

| Type | Use |
|---|---|
| `figue::Shell` | `Bash \| Zsh \| Fish`, the value of `--completions` |
| `figue::FigueBuiltins` | flatten for the standard exit-early flags |
| `figue::ConfigValue` | the dynamic value tree (custom `ConfigFormat`s produce it) |
