+++
title = "Requirements Extraction"
weight = 11
insert_anchor_links = "heading"
+++

A common shape: your global config has many *optional* fields, but a particular
subcommand *requires* a specific subset. "`migrate` needs a database URL even
though most commands don't." Requirements extraction lets you declare that
subset as its own struct and get a precise, hint-rich error if anything is
missing — *after* a successful parse, scoped to the operation.

## The pattern

1. Parse normally with `.into_result()` so you keep the `DriverOutput`.
2. Define a small struct whose fields carry `#[facet(args::origin = "path")]`
   pointing at locations in the merged config.
3. Call `output.extract::<Requirements>()`.

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver};

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config, args::env_prefix = "MYAPP")]
    config: AppConfig,

    #[facet(args::subcommand)]
    command: Command,
}

#[derive(Facet, Debug)]
struct AppConfig {
    /// Optional in general — not every command touches the DB
    #[facet(default)]
    database_url: Option<String>,

    #[facet(default = 8080)]
    port: u16,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    Serve,
    Migrate,
}

/// What `migrate` needs, regardless of how it was supplied.
#[derive(Facet, Debug)]
struct MigrateRequirements {
    #[facet(args::origin = "config.database_url")]
    database_url: String, // required for THIS operation

    #[facet(args::origin = "config.port")]
    port: u16,
}

fn main() {
    let config = builder::<Args>()
        .unwrap()
        .cli(|cli| cli.args(std::env::args().skip(1)))
        .env(|env| env)
        .build();

    let output = Driver::new(config).run().into_result().unwrap_or_else(|e| {
        eprint!("{e}");
        std::process::exit(e.exit_code());
    });

    match output.value.command {
        Command::Serve => { /* … */ }
        Command::Migrate => {
            match output.extract::<MigrateRequirements>() {
                Ok(req) => run_migrations(&req.database_url, req.port),
                Err(e) => {
                    eprint!("{e}");
                    std::process::exit(1);
                }
            }
        }
    }
}
# fn run_migrations(_: &str, _: u16) {}
```

## Origin paths

`args::origin = "config.database_url"` is an absolute, dot-separated path into
the *merged* configuration (the same tree the provenance dump prints). It can
descend as deep as you like: `"config.server.tls.cert_path"`.

- Field present and non-null → used.
- Field missing/null and the requirements field is `Option<T>` → `None`, no
  error.
- Field missing/null and the requirements field is required → collected into the
  error.

Every field of the requirements struct **must** have an `args::origin`.

## The error

When required origins are missing, `ExtractError`'s `Display` produces a message
that names each field, its type, where it should have come from, and *both*
concrete ways to provide it:

```text
Missing required fields for this operation:

  database_url <String> at config.database_url
    Set via: --config.database-url or $MYAPP__CONFIG__DATABASE_URL
```

The CLI hint is the kebab-cased override flag; the env hint uses the config
root's `env_prefix` (or no prefix if there isn't a single one). It is the same
"tell the user exactly how to fix it" philosophy as the rest of figue, but
scoped to *this* subcommand instead of the whole program.

## Why not just make the field required?

Because it isn't required *in general*. `serve` shouldn't fail because there's
no database URL. Requirements extraction keeps the global config permissive
while still giving each operation a hard, well-explained contract — without
duplicating the config struct per subcommand.

That's the end of the guide. The [Reference](@/reference/_index.md) has the
exact rules; the [Recipes](@/recipes/_index.md) have complete programs.
