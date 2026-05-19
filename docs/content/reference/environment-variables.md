+++
title = "Environment Variables"
weight = 5
insert_anchor_links = "heading"
+++

The exact rules for the environment layer. The
[Environment Variables guide](@/guide/environment-variables.md) teaches these.

## Name construction

```
<PREFIX>__<SEGMENT>__<SEGMENT>...
```

- `<PREFIX>` comes from `#[facet(args::env_prefix = "…")]` on the config root
  (or `EnvConfigBuilder::prefix(…)` when there is exactly one root and the
  builder prefix is non-empty).
- The separator between every level is **`__` (double underscore)**, so single
  underscores inside a field name are preserved:
  `MYAPP__SMTP__CONNECTION_TIMEOUT` ⇒ `smtp.connection_timeout`.
- Each segment is lower-cased and matched against fields case-insensitively and
  kebab-aware.

## Prefix matching

- `MYAPP__PORT` matches prefix `MYAPP`. `MYAPP_PORT` (single underscore) does
  **not**.
- A variable with the wrong prefix is silently ignored — no diagnostic, not
  even an unused key.
- `MYAPP__` (nothing after the prefix) or an empty segment
  (`MYAPP__FOO____BAR`) emits a warning and the variable is skipped.
- With multiple config roots, each uses its own `env_prefix`; they are
  independent (`BEE__…`, `BEE_EVAL__…`).

## Flattened fields

`#[facet(flatten)]`ed config fields are addressed at the **parent** level
(`MYAPP__LOG_LEVEL`, not `MYAPP__COMMON__LOG_LEVEL`). The nested form is treated
as an unused key.

## `env_alias`

`#[facet(args::env_alias = "DATABASE_URL")]` reads an absolute, *unprefixed*
variable. Aliases are resolved in a second pass with **lower priority** than the
prefixed form. Multiple aliases: first found wins. Two fields sharing one alias
is a [schema error](@/reference/errors.md#schemaerror). Aliases work on nested
config fields too.

Priority for a single field: `PREFIX__PATH` > first matching `env_alias` >
file > default (CLI still beats all env).

## Values

All env values are captured as strings; coercion happens at deserialization.

| Shape | Representation |
|---|---|
| scalar | the string |
| list (`Vec<T>`) | comma-separated; `\,` is a literal comma; a value with no comma stays a single scalar |
| bool | `true/false/1/0/yes/no/on/off` |
| unit enum | the kebab/renamed variant name |
| struct enum | descend: `PREFIX__STORAGE__S3__BUCKET` |
| empty (`MYAPP__X=""`) | set as empty string — **not** treated as "unset"/`None` |
| `Option<T>` | only `None` when the variable is *absent* (empty string is a real value) |

An unknown enum variant from the environment produces a **warning** (with a
"did you mean" suggestion); the raw value is still passed to deserialization,
which then produces the final error.

## Unknown variables and strict mode

- A correctly-prefixed but unrecognized variable is recorded as an *unused key*.
- `EnvConfigBuilder::strict()` marks the layer strict; it does **not** error at
  parse time. The driver turns unused keys into an error and prints them
  alongside the config dump, after intercepting `--help`/`-h` (so help still
  works).
- If the schema has no config root at all, every prefixed variable becomes an
  unused key.

## Substitution vs. sourcing

Reading a field *from* `MYAPP__X` is the env **layer** (this page). Expanding
`${X}` *inside* a value is env **substitution**
([`args::env_subst`](@/guide/env-substitution.md)) — a separate, opt-in,
post-merge pass.
