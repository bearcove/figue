+++
title = "Layered Configuration"
weight = 5
insert_anchor_links = "heading"
+++

This is the heart of figue: take the *same* set of structs and resolve every
field from whichever source has the highest priority that provides it.

## The precedence

From highest to lowest:

> **CLI arguments  >  environment variables  >  config files  >  code defaults**

A field is resolved independently. If `port` comes from the file and `host` from
the CLI, you get both — there is no "the CLI wins, so the file is ignored".
The exact merge algorithm (objects deep-merge, scalars and arrays replace) is in
the [Merge Precedence reference](@/reference/merge-precedence.md).

## The builder and the driver

Two-step API:

1. **`builder::<T>()`** validates your schema and lets you configure each layer
   (`.cli`, `.env`, `.file`, `.help`), then `.build()` produces a `Config<T>`.
2. **`Driver::new(config).run()`** parses every layer, merges them, intercepts
   `--help`/`--version`/etc., deserializes into `T`, and hands back a
   `DriverOutcome<T>`.

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver};

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config, args::env_prefix = "MYAPP")]
    config: AppConfig,
}

#[derive(Facet, Debug)]
struct AppConfig {
    #[facet(default = "localhost")]
    host: String,
    #[facet(default = 8080)]
    port: u16,
    #[facet(default = false)]
    debug: bool,
}

fn main() {
    let config = builder::<Args>()
        .unwrap()
        .cli(|cli| cli.args(std::env::args().skip(1)))
        .env(|env| env)
        .file(|f| f.default_paths(["./myapp.json"]))
        .help(|h| h
            .program_name(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION")))
        .build();

    let args: Args = Driver::new(config).run().unwrap();
    println!("listening on {}:{}", args.config.host, args.config.port);
}
```

With `myapp.json` setting `port = 3000`, `MYAPP__DEBUG=true` in the environment,
and `--config.host 0.0.0.0` on the command line, the result is
`host = "0.0.0.0"` (CLI), `port = 3000` (file), `debug = true` (env). Everything
not mentioned anywhere falls back to its `#[facet(default)]`.

## CLI overrides into a config root

You don't need a separate flag for every config key. A config root named
`config` automatically accepts dotted overrides:

```bash
myapp --config.host 0.0.0.0 --config.port 9090 --config.database.timeout 60
```

`--config.server.port=9090` and `--no-config.debug` (for a bool) work too. This
keeps the CLI surface tiny while still letting users override anything.

## `unwrap()` vs `into_result()`

`run()` returns a `DriverOutcome<T>`. Two ways to consume it:

- **`.unwrap()`** — the batteries-included path. On `--help`/`--version`/
  `--completions` it prints and exits `0`; on error it prints the diagnostic and
  exits `1`; otherwise it returns `T`. This is what most `main()`s want.
- **`.into_result()`** — gives you `Result<DriverOutput<T>, DriverError>` so you
  can handle everything yourself. **Do not blindly `?` it**: `Help` and
  `Version` arrive as `Err`, and `?` would turn a help request into exit code 1.

```rust,noexec
use figue::{Driver, DriverError};

match Driver::new(config).run().into_result() {
    Ok(output) => {
        let args = output.get(); // prints any warnings, returns T
        // …
    }
    Err(e) if e.is_success() => {
        // --help / --version / --completions: print and exit 0
        print!("{e}");
        std::process::exit(0);
    }
    Err(e) => {
        eprint!("{e}");
        std::process::exit(e.exit_code());
    }
}
```

Full driver surface (every `DriverError` variant and exit code) is in the
[Builder API](@/reference/builder-api.md) and
[Errors](@/reference/errors.md) references.

## Who set this value? The provenance dump

When parsing fails or a required field is missing, figue prints a tree showing
*every* effective value and the source that won. This is the single most useful
debugging tool for layered config:

```text
Error: Missing required fields:

Sources:
├─ file:
│  └─ (picked) app.json  (via --config)
├─ env $MYAPP__*
├─ cli --config.*
└─ defaults

debug......... true..... --config.debug
host.......... 0.0.0.0.. app.json:3
port.......... 4000..... $MYAPP__PORT
name.......... ......... ⨯ MISSING

Missing:
  name <String> (--config.name or $MYAPP__NAME)

Run with --help for usage information.
```

Each leaf shows `key`, the resolved `value`, and where it came from: a `--flag`
(CLI), a `$VAR` (env), `file.json:LINE` (file), or `DEFAULT`. `⨯ MISSING` marks
a required field nobody supplied — and figue tells the user *exactly* which flag
or variable would set it. See
[Errors & Diagnostics](@/guide/errors-and-diagnostics.md) for the full story.

## Optional config roots

A config root can be `Option<T>` plus `#[facet(default)]`. If nothing supplies
it, it stays `None` rather than being synthesized from inner defaults — handy
for "TLS is off unless configured":

```rust,noexec
#[derive(Facet, Debug)]
struct AppConfig {
    /// null / absent means plain HTTP
    #[facet(default)]
    tls: Option<TlsConfig>,
}

#[derive(Facet, Debug)]
struct TlsConfig {
    cert_path: String,
    key_path: String,
}
```

Next: [Defaults & Optionals](@/guide/defaults-and-optionals.md) — precisely what
counts as required.
