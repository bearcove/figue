+++
title = "Builder API"
weight = 2
insert_anchor_links = "heading"
+++

The full surface of `builder` → `ConfigBuilder` → `Config` → `Driver` →
`DriverOutcome`.

## `ConfigBuilder<T>`

Returned by `builder::<T>()`. Fluent; every layer is optional.

| Method | Purpose |
|---|---|
| `.cli(\|c\| …)` | configure the CLI layer (see `CliConfigBuilder`) |
| `.env(\|e\| …)` | configure the env layer (see `EnvConfigBuilder`) |
| `.file(\|f\| …)` | configure the file layer (see `FileConfigBuilder`) |
| `.help(\|h\| …)` | configure help/version text (see `HelpConfigBuilder`) |
| `.build() -> Config<T>` | finalize; hand to `Driver::new` |
| `.generate_json_schemas() -> Vec<JsonSchemaFile>` | schemas in memory |
| `.write_json_schemas(dir) -> Result<Vec<PathBuf>, JsonSchemaError>` | schemas to disk |

A layer you never configure simply contributes nothing (e.g. omit `.file` and
no config file is read). `Config<T>` also exposes
`generate_json_schemas` / `write_json_schemas`.

### `CliConfigBuilder`

| Method | Purpose |
|---|---|
| `.args(iter)` | the arguments to parse (any `IntoIterator<Item = impl Into<String>>`) |
| `.strict()` | unknown keys become errors (surfaced via the driver) |

```rust,noexec
.cli(|c| c.args(std::env::args().skip(1)))
.cli(|c| c.args(["--port", "8080"]).strict())
```

### `EnvConfigBuilder`

| Method | Purpose |
|---|---|
| `.source(MockEnv)` | read from a `MockEnv` instead of the process env (tests) |
| `.prefix("MYAPP")` | override the prefix (single-config-root case) |
| `.strict()` | unknown prefixed vars become errors |

With no `.source`, the real environment is read. `MockEnv::from_pairs([(k, v),…])`
builds a fake environment.

### `FileConfigBuilder`

| Method | Purpose |
|---|---|
| `.default_paths([…])` | ordered candidate paths; first existing wins |
| `.content(text, filename)` | inline content (tests); filename drives format |
| `.format(fmt)` | register an extra [`ConfigFormat`](@/reference/config-file-formats.md) (e.g. `JsoncFormat`) |
| `.strict()` | unknown keys become errors |

The default registry is **JSON only**; add `JsoncFormat` (or your own) for more.
No `~`/`$HOME` expansion — pass absolute paths.

### `HelpConfigBuilder`

| Method | Default | Purpose |
|---|---|---|
| `.program_name(s)` | normalized `argv[0]` | name in help/version/completions |
| `.version(s)` | `"unknown"` | string for `--version` |
| `.description(s)` | none | extra text under the summary |
| `.width(n)` | `80` | wrap width; `0` disables wrapping |

Use `env!("CARGO_PKG_VERSION")` / `env!("CARGO_PKG_NAME")` so the values come
from *your* crate.

## `Driver`

```rust,noexec
let outcome: DriverOutcome<T> = Driver::new(config).run();
```

`run()` parses CLI (first, to discover `--<root>` file paths), then file, then
env; intercepts `--help`/`--version`/`--completions`/`--html-help`/
`--export-jsonschemas`; merges (defaults < file < env < cli); runs `${VAR}`
substitution; detects enum-variant conflicts; fills defaults; reports
missing/unknown fields; deserializes into `T`.

## `DriverOutcome<T>`

`#[must_use]` wrapper around `Result<DriverOutput<T>, DriverError>`. It
deliberately does **not** implement `Try` (no accidental `?`).

| Method | Returns | Notes |
|---|---|---|
| `.unwrap()` | `T` | batteries included: prints + exits for help/version/completions/errors |
| `.into_result()` | `Result<DriverOutput<T>, DriverError>` | manual handling; do **not** blindly `?` |
| `.is_ok()` / `.is_err()` | `bool` | |
| `.unwrap_err()` | `DriverError` | panics on success |

## `DriverOutput<T>`

| Method | Purpose |
|---|---|
| `.get() -> T` | print warnings to stderr, return the value |
| `.get_silent() -> T` | return the value, no warnings |
| `.into_parts() -> (T, DriverReport)` | value plus the report |
| `.print_warnings()` | print warnings only |
| `.extract::<R>() -> Result<R, ExtractError>` | [requirements extraction](@/guide/requirements-extraction.md) |

`DriverReport` carries the diagnostics, the per-layer values, file resolution,
and the list of overrides; its `Display` is the rendered diagnostic/dump.

## `DriverError`

| Variant | `exit_code()` | `is_success()` | Meaning |
|---|---|---|---|
| `Help { text, suggestion }` | 0 | yes | `--help` (or guided help on missing args) |
| `HtmlHelp { path }` | 0 | yes | `--html-help` wrote a page |
| `Completions { script }` | 0 | yes | `--completions` |
| `Version { text }` | 0 | yes | `--version` |
| `JsonSchemasExported { paths }` | 0 | yes | `--export-jsonschemas` |
| `Failed { report }` | 1 | no | parse / missing / type / conflict error |
| `Builder { error }` | 1 | no | invalid schema (`BuilderError`) |
| `JsonSchemaExport { error }` | 1 | no | could not write schemas |
| `EnvSubst { error }` | 1 | no | undefined `${VAR}` with no default |
| `HtmlHelpFailed { error }` | 1 | no | could not write/open HTML help |

Helpers: `.exit_code() -> i32`, `.is_success() -> bool`, `.is_help() -> bool`
(only `Help`, not `HtmlHelp`), `.help_text() -> Option<&str>`. `DriverError`
also implements `std::process::Termination`, so `fn main() -> DriverError`
prints and exits correctly. `Display`/`Debug` render the user-facing text.

The canonical `.into_result()` skeleton:

```rust,noexec
match Driver::new(config).run().into_result() {
    Ok(out)                       => use_value(out.get()),
    Err(e) if e.is_success()      => { print!("{e}"); std::process::exit(0); }
    Err(e)                        => { eprint!("{e}"); std::process::exit(e.exit_code()); }
}
```

See [Errors](@/reference/errors.md) for `BuilderError`/`SchemaError` and the
runtime error catalog.
