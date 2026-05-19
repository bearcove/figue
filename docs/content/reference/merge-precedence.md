+++
title = "Merge Precedence"
weight = 7
insert_anchor_links = "heading"
+++

How the four layers combine into the value that gets deserialized.

## Layer order

The driver folds layers in this order (later wins):

```
defaults  →  file  →  env  →  cli
```

So the effective precedence is:

> **CLI  >  environment  >  config file  >  code defaults**

Internally each layer is a `ConfigValue` tree tagged with provenance
(`Cli` = priority 3, `Env` = 2, `File` = 1, `Default` = 0). Defaults are a real
layer, not a fallback applied afterward.

## The merge rule

Folding two trees, "upper" (higher priority) over "lower":

- **object vs object** → deep-merge recursively. Keys present in only one side
  are kept; shared keys recurse.
- **anything else** (scalar, array, enum, or object-vs-non-object) → upper
  **replaces** lower entirely.

The crucial consequence: **arrays do not merge across layers — they replace.**
A `Vec` set on the CLI completely supersedes the same `Vec` from a file; there
is no element-wise union or append. (Within a *single* CLI invocation, repeated
flags accumulate into one array *before* this cross-layer merge.)

## Field-by-field, not layer-by-layer

Because objects deep-merge, each leaf is resolved independently. Given:

- file: `{ "config": { "host": "file-host", "port": 3000, "db": { "url": "u", "pool": 5 } } }`
- env: `MYAPP__DB__URL=env-url`
- cli: `--config.host cli-host`

the result is `host = "cli-host"` (cli), `port = 3000` (file),
`db.url = "env-url"` (env), `db.pool = 5` (file). No layer "wins the struct" —
they win individual leaves.

## Defaults and `Option`

- A `#[facet(default)]` participates as the lowest layer with provenance
  `Default`. Any real file/env/cli value replaces it via the scalar rule above.
- A missing optional field stays absent (so facet yields `None`); it is not
  synthesized.
- An **optional config root** (`Option<RootStruct>` + `#[facet(default)]`) stays
  `None` rather than being built from its inner defaults — "off unless
  configured".

## Overrides are recorded

When a value from one provenance replaces another, the driver records an
`Override { path, winner, loser }`. These power the dump line that reads, e.g.,
`port 4000 $MYAPP__PORT` (env beat the file's 3000) and the
"X overrides Y" notes. There is no public API to query overrides yet; they
surface through the rendered report / config dump.

## Post-merge passes

After merging, in order:

1. **Environment substitution** — `${VAR}` expanded in `args::env_subst` fields
   on the *merged* value (so a CLI- or file-supplied string is substituted too).
2. **Enum-variant conflict detection** — if two variants of the same config enum
   end up initialized from different sources, that's a hard error naming each
   variant and its source.
3. **Defaults fill / missing & unknown reporting**.
4. **Deserialization** into your type; type errors map back through a span
   registry to the original CLI token / env line / file location.

See [Errors](@/reference/errors.md) for what each post-merge failure looks like.
