+++
title = "Attributes"
weight = 0
insert_anchor_links = "heading"
+++

Every attribute figue understands, what it does, and what it requires. Attributes
are written `#[facet(args::NAME)]` after `use figue as args;` (equivalently
`#[facet(figue::NAME)]`). figue also relies on several plain `#[facet(…)]`
attributes from facet itself; those are listed at the end.

## Role attributes

Every non-flattened, non-config field needs exactly one role.

### `args::positional`

Consume a bare argument, in declaration order. `Vec<T>` collects greedily.
Incompatible with `args::short`.

```rust,noexec
#[facet(args::positional)]
input: String,
```

### `args::named`

Create a `--long-flag` (field name in kebab-case). Combine with `args::short`,
`args::counted`. A `bool` field also gets a `--no-` negation.

```rust,noexec
#[facet(args::named)]
out_dir: String, // --out-dir
```

### `args::short`

`args::short = 'v'` adds `-v`. Bare `args::short` uses the first letter of the
field name. Subcommand variants may also carry `args::short` (separate
namespace from flags).

```rust,noexec
#[facet(args::named, args::short = 'j')]
jobs: usize,
```

### `args::counted`

Count occurrences (`-vvv` ⇒ 3, saturating). Field must be an integer type;
defaults to `0`.

```rust,noexec
#[facet(args::named, args::short = 'v', args::counted, default)]
verbose: u8,
```

### `args::subcommand`

Field is an enum (or `Option<enum>`); each variant is a subcommand. Enum must be
`#[repr(u8)]`. Only one subcommand field per level.

```rust,noexec
#[facet(args::subcommand)]
command: Command,
```

### `args::config`

Field is a struct populated from the merged layers (CLI overrides, env, file,
default). Generates `--<field> <PATH>` and `--<field>.dotted.path` overrides.
Must be a struct (or `Option<struct>`). Not allowed inside a subcommand variant.

```rust,noexec
#[facet(args::config, args::env_prefix = "MYAPP")]
config: AppConfig,
```

## Config-layer attributes

Used on `args::config` roots and the fields inside them.

| Attribute | Where | Effect |
|---|---|---|
| `args::env_prefix = "MYAPP"` | on the config root | env vars are `MYAPP__FIELD__SUBFIELD`. Requires `args::config`. |
| `args::env_alias = "DATABASE_URL"` | on a config field | also read this exact (unprefixed) variable. Repeatable. |
| `args::env_subst` | on a config field | expand `${VAR}` in this field's value. |
| `args::env_subst_all` | on a struct | apply `env_subst` to all direct fields (and flattened ones), not nested structs. |
| `args::origin = "config.db_url"` | on a requirements struct field | source path for [requirements extraction](@/guide/requirements-extraction.md). |

`args::env_prefix` without `args::config` is a [schema error](@/reference/errors.md#schemaerror).

## Builtin-flag attributes

Mark a field as one of the standard exit-early flags. Flattening
[`FigueBuiltins`](@/reference/entry-points.md#figuebuiltins) gives you all of
them; use these only if you want custom names/placement.

| Attribute | Field type | Effect |
|---|---|---|
| `args::help` | `bool` | show help, exit 0 |
| `args::version` | `bool` | show version, exit 0 |
| `args::completions` | `Option<Shell>` | print completions, exit 0 |
| `args::export_jsonschemas` | `Option<String>` | write JSON Schemas, exit 0 |

Detection is by *attribute*, not field name — you can `rename` them freely:

```rust,noexec
#[facet(args::named, args::help, rename = "print-docs")]
show_help: bool, // responds to --print-docs
```

## Presentation

### `args::label = "TEXT"`

Override the `<TYPE>` placeholder in help output. Help-only; does not affect
parsing.

```rust,noexec
#[facet(args::named, args::label = "COUNT")]
jobs: usize, // help shows: --jobs <COUNT>
```

## facet attributes figue honors

These come from facet, not figue, but change figue's behavior:

| Attribute | Effect in figue |
|---|---|
| `#[facet(flatten)]` | lift a nested struct's fields to this level (args *and* config). Struct fields in an args struct **must** be flattened or be an `args::config` root. |
| `#[facet(default)]` | use `Default::default()`; makes the field optional. |
| `#[facet(default = expr)]` | use `expr`; shown as `DEFAULT` in help/dump; emitted as `default` in JSON Schema. |
| `#[facet(rename = "name")]` | rename the flag / config key / subcommand. |
| `#[facet(rename_all = "kebab-case")]` | rename all fields/variants of a struct/enum (any case style facet supports). |
| `#[facet(sensitive)]` | redact in the config dump (`🔒 [REDACTED]`); `writeOnly: true` in JSON Schema. |
| `#[repr(u8)]` | required on any data-carrying enum used as a subcommand or config enum. |

See [Supported Types](@/reference/supported-types.md) for which field *types*
parse, and [Errors](@/reference/errors.md#schemaerror) for the illegal
combinations and their messages.
