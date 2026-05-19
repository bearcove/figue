+++
title = "CLI Grammar"
weight = 4
insert_anchor_links = "heading"
+++

The exact command-line parsing rules, including every edge case. The
[CLI Arguments guide](@/guide/cli-arguments.md) teaches these; this page is the
specification.

## Names

A `--long` flag's name is the field's effective name (after `rename` /
`rename_all`) converted to **kebab-case**. Subcommands likewise. Matching a
config-override path segment is case-insensitive and kebab-aware.

## Long flags

- `--name value` and `--name=value` (split at the first `=`).
- A flag value beginning with extra dashes (`---x`) is an "unknown flag" error.
- Unknown `--flag` → error with a "did you mean?" suggestion when one is close.

## Boolean flags

- Bare `--flag` ⇒ `true`.
- `--flag=V`: `true` iff `V` lowercased ∈ {`true`,`yes`,`1`,`on`,`""`};
  otherwise `false`. (`parse_bool_literal` also recognizes
  `false`/`no`/`0`/`off`.)
- `--no-flag` ⇒ `false`, but only if the (Option-unwrapped) field is a `bool`;
  `--no-x` on a non-bool is an "unknown flag" error.
- `--no-` works through the parent-stack bubbling (subcommands).

## Short flags

- `-n value`, `-nvalue`, `-n=value` (the `=` form requires exactly one char
  before `=`).
- **Clustering** `-abc`: each char is resolved left to right.
  - bools in a cluster all become `true`.
  - a non-bool flag that is the **last** char consumes the next argv item
    (`-abj 4`).
  - a non-bool flag **not** last treats the remaining characters as its attached
    value (`-abj4` ⇒ `a`, `b`, `jobs=4`).
  - an unknown char errors (`unknown flag: -x`) and parsing continues.
- `-a=true` works for bools (truthy set as above).
- Bare `#[facet(args::short)]` uses the first character of the effective field
  name.

## Counted flags

`#[facet(args::counted)]` increments per occurrence: `-vvv`, `-v -v -v`, and
`--verbose --verbose` all count; short and long mix. Final value is the integer
count (saturating). The field must be an integer; defaults to `0`; never
"required". In subcommands the count bubbles to the declaring (parent) level.

## Positionals

- Filled in field-declaration order; each consumes one argument unless its type
  is a list (`Vec<T>`), which keeps absorbing.
- `Option<T>` positional: present ⇒ `Some`, absent ⇒ `None` (with `default`).
- A required positional (non-Option, no default, not bool) that is missing → a
  "missing required argument" diagnostic.
- An extra positional with nowhere to go → "unexpected argument" (with a
  subcommand suggestion if a subcommand was expected).

## Lists

- Repeating a flag accumulates: the second occurrence converts the value to an
  array and appends. `-n a -n b -n c` ⇒ `["a","b","c"]`.
- `Vec<bool>` with a short flag: `-vvv` ⇒ `[true,true,true]`.
- No comma-splitting on the CLI (that is environment-only). A lone scalar for a
  `Vec<T>` becomes a one-element list.

## The `--` terminator

The **first** `--` ends option parsing; everything after is positional, even if
dash-prefixed. A subsequent literal `--` is collected as an ordinary positional.
Flags *before* `--` still parse. After a subcommand, `--` lets values that
collide with subcommand names be taken as positionals of the current level.

## Config roots

For a config root with effective field name `F` (CLI root = `F` kebab-cased):

- **Dotted overrides:** `--F.a.b VALUE`, `--F.a.b=VALUE`, and `--no-F.a.b` (for
  bools). Numeric segments index into `Vec`s; enum variants and struct-variant
  fields are navigable. Unknown override path → "unknown flag" with a
  config-path-aware suggestion.
- **File path flag:** `--F <PATH>` (and `--F=PATH`) sets that root's config-file
  path. `--F` with no following value → "flag … requires a file path" error.
- Ordering: the dotted-override match is attempted **before** the path-flag
  match, so `--F.foo` is an override and `--F` alone is the path flag — they do
  not conflict.
- Multiple roots each get their own override namespace and path flag; unknown
  keys are reported prefixed with the root field name.

## Subcommands

- Selected by kebab-cased variant name or its one-character `args::short` alias.
- "Adoption agency": a flag/`--no-`/counted flag not found at the current
  subcommand level bubbles up the parent stack before becoming an error — this
  is what makes parent/global flags usable after the subcommand.

## Value capture

All scalar CLI values are captured as **strings** with provenance; type coercion
to `u16`/`bool`/`IpAddr`/etc. happens later during deserialization (so type
errors point back at the exact argv token). The only special case: a
`completions` field may be given with no value, recorded as the sentinel
`auto` (auto-detect the shell).

## Edge cases worth knowing

- `--flag=` (empty) on a bool ⇒ `true` (empty ∈ truthy set).
- `--flag=typo` on a bool ⇒ `false` silently — prefer `--flag` / `--no-flag`.
- Out-of-range / invalid numerics are *not* rejected by the CLI layer; they pass
  through and the deserializer produces the final, source-located error.
- Strict mode does **not** fail at parse time; the driver reports unknown keys
  alongside the config dump, and `--help` still works even with unknown strict
  keys.
