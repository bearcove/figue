+++
title = "Errors"
weight = 8
insert_anchor_links = "heading"
+++

The complete error catalog: schema errors (your code is wrong), driver errors
(the run's outcome), and the runtime parse-error kinds. For how these *look* and
how to handle them, see
[Errors & Diagnostics](@/guide/errors-and-diagnostics.md).

## DriverError

The outcome of `Driver::run()`. Variants, exit codes, and whether they mean
success:

| Variant | exit | success | When |
|---|---|---|---|
| `Help { text, suggestion }` | 0 | ✓ | `--help`, or guided help when only a subcommand / required CLI arg is missing |
| `HtmlHelp { path }` | 0 | ✓ | `--html-help` wrote a page (browser opens on `.unwrap()`) |
| `Completions { script }` | 0 | ✓ | `--completions <shell>` |
| `Version { text }` | 0 | ✓ | `--version` |
| `JsonSchemasExported { paths }` | 0 | ✓ | `--export-jsonschemas <dir>` |
| `Failed { report }` | 1 | ✗ | parse / type / missing / conflict error |
| `Builder { error }` | 1 | ✗ | invalid schema — see [BuilderError](#buildererror) |
| `JsonSchemaExport { error }` | 1 | ✗ | could not write schema files |
| `EnvSubst { error }` | 1 | ✗ | `${VAR}` undefined with no default |
| `HtmlHelpFailed { error }` | 1 | ✗ | could not write/open HTML help |

Helpers: `exit_code() -> i32`, `is_success() -> bool`, `is_help() -> bool`
(matches `Help` only, **not** `HtmlHelp`), `help_text() -> Option<&str>`.
Implements `std::process::Termination`.

## BuilderError

Returned by `builder::<T>()` / wrapped in `DriverError::Builder`. Setup-phase
failures: `SchemaError`, `Alloc`, `FileNotFound`, `FileRead`, `FileParse`,
`CliParse`, `UnknownKey { key, source, suggestion }`, `MissingRequired`. The
common one is `SchemaError`.

## SchemaError

Raised *before any parsing* when the type itself is not a valid figue target.
Rendered with ariadne, pointing at the offending field (the "defined at
file:line" label needs facet's `doc` feature). Fix it once and never see it
again. The full catalog:

| Message | Trigger |
|---|---|
| `top-level shape must be a struct` | root type `T` is an enum / not a struct |
| `field \`x\` is missing a #[facet(args::...)] annotation` | a plain field with no role and no `flatten`/`config` |
| `field \`x\` uses args::env_prefix without args::config` | `env_prefix` on a non-config field |
| `flattened field \`x\` must be a struct` | `#[facet(flatten)]` on a non-struct |
| `struct fields in args must use #[facet(flatten)]` | a struct-typed arg field that isn't flattened or a config root |
| `config field must be a struct` | `args::config` on a non-struct |
| `flattened config field \`x\` must be a struct` | `flatten` inside a config struct on a non-struct |
| `#[facet(args::positional)] is not compatible with #[facet(args::short)]` | both on one field |
| `field \`x\` marked as counted must be an integer` | `args::counted` on a non-integer (incl. `bool`) |
| `field \`x\` marked as subcommand must be an enum` | `args::subcommand` on a non-enum |
| `only one field may be marked with #[facet(args::subcommand)] at this level` | two subcommand fields (incl. via flatten) |
| `#[facet(args::config)] inside a subcommand variant is not supported` | a config field within a subcommand variant |
| `duplicate flag \`--x\`` / `duplicate flag \`-x\`` | two args resolve to the same long/short flag |
| `duplicate argument \`x\`` | two args share an effective name |
| `duplicate subcommand name \`x\`` | two variants resolve to the same CLI name |
| `duplicate subcommand short alias \`x\`` | two variants share an `args::short` |
| `subcommand short alias \`x\` conflicts with existing subcommand name` | alias equals another subcommand's name |
| `env alias \`x\` is used by both \`a\` and \`b\`` | two fields share an `args::env_alias` |

(Several of the duplicate messages have "(from flattened field)" /
"(from flattened config root)" variants when the collision is introduced by a
`#[facet(flatten)]`.)

Note: a subcommand short alias and a *flag* short of the same letter do **not**
conflict — they live in separate namespaces.

A rendered example:

```text
Error: field `foo` is missing a #[facet(args::...)] annotation
   ╭─[ <schema>:3:5 ]
   │
 2 │ struct MissingArgsAnnotation {
   │        ──────────┬──────────
   │                  ╰──────────── defined at src/main.rs:65
 3 │     foo: String,
   │     ─────┬─────
   │          ╰─────── field `foo` is missing a #[facet(args::...)] annotation
 4 │ }
───╯
```

## Runtime error kinds (ArgsErrorKind)

When a `Failed` report is produced, the underlying parse problem is one of
(`#[non_exhaustive]`):

| Kind | `code()` | Typical message / help |
|---|---|---|
| `UnknownLongFlag` | `args::unknown_long_flag` | `unknown flag --x` · *did you mean `--y`?* |
| `UnknownShortFlag` | `args::unknown_short_flag` | `unknown flag -x` (precise char span in clusters) |
| `MissingArgument` | `args::missing_argument` | `missing required argument --x` · *provide a value with `--x <T>`* |
| `ExpectedValueGotEof` | `args::expected_value` | `expected <T> value` · *provide a value after the flag* |
| `UnexpectedPositionalArgument` | `args::unexpected_positional` | lists available options / hints a missing `args::subcommand` |
| `UnknownSubcommand` | `args::unknown_subcommand` | `unknown subcommand x` · *did you mean `y`?* |
| `MissingSubcommand` | `args::missing_subcommand` | `expected a subcommand` · lists available |
| `NoFields` | `args::no_fields` | type has nothing to parse into |
| `EnumWithoutSubcommandAttribute` | `args::enum_without_subcommand_attribute` | enum field needs `#[facet(args::subcommand)]` |
| `MissingArgsAnnotation` | `args::missing_args_annotation` | (also surfaces as a schema error) |
| `ReflectError` | `args::reflect_error` | type/parse failures, e.g. `failed to parse "abc" as u16 at config.port` |
| `Help` / `Version` / `Completions` Requested | `args::help` etc. | the success exits |

Type/parse failures (`ReflectError`) carry a span that the driver maps back —
through a span registry — to the original CLI token, the synthetic `<env>`
line, or the real config-file location, so the underline always points at what
the user actually wrote.

## Suggestions

"Did you mean …?" uses Jaro–Winkler similarity (threshold ≈ 0.8,
case-insensitive) for flags, subcommands, and config-override paths (the config
path suggester walks the schema segment by segment to find the first wrong
segment).

## Color and snapshots

Diagnostics are colored when supported; `NO_COLOR` / `FORCE_COLOR` are honored,
and color is force-disabled when any `INSTA_*` env var is set so snapshot tests
stay stable. There is no separate "plain vs rich" mode — the ariadne layout is
always produced, only color toggles.
