+++
title = "Entry Points"
weight = 1
insert_anchor_links = "heading"
+++

Four ways into figue. The first two are CLI-only conveniences; the builder is
the full layered path; `FigueBuiltins` is the struct you flatten for free flags.

## `figue::from_std_args::<T>() -> DriverOutcome<T>`

CLI-only. Reads `std::env::args().skip(1)`, no env vars, no config files.
Equivalent to `from_slice` on the real argv.

```rust,noexec
let args: Args = figue::from_std_args().unwrap();
```

## `figue::from_slice::<T>(args: &[&str]) -> DriverOutcome<T>`

CLI-only, from an explicit slice — the standard way to unit-test a parser
without touching the process environment.

```rust,noexec
let args: Args = figue::from_slice(&["--verbose", "input.txt"]).unwrap();
```

Internally it is exactly `builder::<T>()` + `.cli(args)` + `Driver::run()`, so
behavior matches the full path; only env/file layers are absent. If the schema
is invalid it returns a `DriverOutcome` carrying `DriverError::Builder` rather
than panicking.

## `figue::builder::<T>() -> Result<ConfigBuilder<T>, BuilderError>`

The full layered entry point: configure `.cli`, `.env`, `.file`, `.help`, then
`.build()` and run it through a [`Driver`](@/reference/builder-api.md). Use this
whenever you need environment variables, config files, a custom program
name/version, or JSON Schema export. Covered in detail in the
[Builder API](@/reference/builder-api.md).

```rust,noexec
let config = figue::builder::<Args>()
    .unwrap()
    .cli(|c| c.args(std::env::args().skip(1)))
    .env(|e| e)
    .file(|f| f.default_paths(["app.json"]))
    .build();
let args: Args = figue::Driver::new(config).run().unwrap();
```

`builder` returns `Err(BuilderError::SchemaError(_))` if `T` is not a valid args
type (see [schema errors](@/reference/errors.md#schemaerror)).

## `FigueBuiltins`

A struct you `#[facet(flatten)]` into your args to get the standard exit-early
flags. Definition:

```rust,noexec
#[derive(Facet, Default, Debug)]
pub struct FigueBuiltins {
    #[facet(args::named, args::short = 'h', args::help, default)]
    pub help: bool,

    #[facet(args::named, default)]
    pub html_help: bool,

    #[facet(args::named, args::short = 'V', args::version, default)]
    pub version: bool,

    #[facet(args::named, args::completions, default)]
    pub completions: Option<Shell>,

    #[facet(args::named, args::export_jsonschemas, args::label = "DIR", default)]
    pub export_jsonschemas: Option<String>,
}
```

| Flag | Driver action with `.unwrap()` |
|---|---|
| `--help` / `-h` | print help, exit `0` |
| `--html-help` | open interactive HTML help in browser, exit `0` |
| `--version` / `-V` | print `program version`, exit `0` |
| `--completions <bash\|zsh\|fish>` | print completion script, exit `0` |
| `--export-jsonschemas <DIR>` | write one schema per config root, exit `0` |

These are intercepted by the driver *before* deserialization, so they work even
with otherwise-incomplete arguments (`myapp install --help` is fine). You can
read the fields after parsing too (`args.builtins.help`), but with `.unwrap()`
the driver has already acted on them.

`Shell` (`figue::Shell`) is the enum `Bash | Zsh | Fish`; figue also re-exports
`generate_completions_for_shape` if you need to produce a script directly.

## Choosing

| You need | Use |
|---|---|
| Just parse argv, nothing else | `from_std_args` |
| Same, in a test | `from_slice` |
| Env vars and/or config files, custom version/help, JSON Schema | `builder` + `Driver` |
| `--help`/`--version`/completions for free | flatten `FigueBuiltins` (works with any of the above) |
