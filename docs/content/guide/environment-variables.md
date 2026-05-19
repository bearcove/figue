+++
title = "Environment Variables"
weight = 4
insert_anchor_links = "heading"
+++

Environment variables fill the same config root as files. They sit *above* files
and *below* CLI arguments in the precedence. This page covers naming, nesting,
and the small conveniences figue adds. Exact rules live in the
[Environment Variables reference](@/reference/environment-variables.md).

## Prefix and the `__` separator

Set a prefix on the config root with `args::env_prefix`. figue then reads
`PREFIX__FIELD`, using a **double underscore** to descend into nested structs.

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver, MockEnv};

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config, args::env_prefix = "MYAPP")]
    config: AppConfig,
}

#[derive(Facet, Debug)]
struct AppConfig {
    #[facet(default = 8080)]
    port: u16,
    database: Database,
}

#[derive(Facet, Debug)]
struct Database {
    #[facet(default = 30)]
    connection_timeout: u64,
}

let config = builder::<Args>()
    .unwrap()
    .env(|env| env.source(MockEnv::from_pairs([
        ("MYAPP__PORT", "9000"),
        ("MYAPP__DATABASE__CONNECTION_TIMEOUT", "60"),
    ])))
    .build();

let out = Driver::new(config).run().into_result().unwrap();
assert_eq!(out.value.config.port, 9000);
assert_eq!(out.value.config.database.connection_timeout, 60);
```

Why double underscore? Field names can contain single underscores
(`connection_timeout`). `__` unambiguously means "go one level deeper", so
`MYAPP__DATABASE__CONNECTION_TIMEOUT` is `database.connection_timeout`, not
`database.connection.timeout`.

The variable name is upper-cased by convention; matching against fields is
case-insensitive and kebab-aware.

`MockEnv` is for tests. In production read the real environment:

```rust,noexec
let config = builder::<Args>()
    .unwrap()
    .env(|env| env) // reads std::env
    .build();
```

## Standard aliases with `env_alias`

Sometimes you want to honor a conventional, *unprefixed* variable like
`DATABASE_URL` or `PORT`. Add one or more `args::env_alias`:

```rust,noexec
#[derive(Facet, Debug)]
struct AppConfig {
    /// Also read from $DATABASE_URL
    #[facet(args::env_alias = "DATABASE_URL")]
    database_url: String,

    /// Read from $PORT or $HTTP_PORT
    #[facet(args::env_alias = "PORT", args::env_alias = "HTTP_PORT")]
    port: u16,
}
```

The prefixed form (`MYAPP__PORT`) takes priority over aliases when both are set;
among aliases, the first one found wins.

## Lists from one variable

A value containing commas becomes a list. Escape a literal comma with `\,`:

```rust,noexec
#[derive(Facet, Debug)]
struct AppConfig {
    #[facet(default)]
    allowed_hosts: Vec<String>,
}

// MYAPP__ALLOWED_HOSTS=a.com,b.com,c.com  ->  ["a.com", "b.com", "c.com"]
```

This comma-splitting is an environment-only convenience; CLI lists use
[repeated flags](@/guide/cli-arguments.md#repeated-flags-lists) instead.

## Enums and booleans

Enum values use the (kebab-cased) variant name; booleans accept
`true/false/1/0/yes/no/on/off`:

```bash
MYAPP__STORAGE=memory
MYAPP__LOGGING__JSON=true
```

For an enum *struct* variant, descend into it like any nested struct:

```bash
MYAPP__STORAGE__S3__BUCKET=my-data
```

An unknown enum variant from the environment is a **warning**, not a hard error
— the raw value is still passed through to deserialization, which produces the
precise message.

## Multiple roots, multiple prefixes

With more than one config root, each uses its own `env_prefix`, so they never
collide:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config, args::env_prefix = "BEE")]
    bee: BeeConfig,
    #[facet(args::config, args::env_prefix = "BEE_EVAL")]
    eval: EvalConfig,
}
// BEE__PORT and BEE_EVAL__DATASET are independent
```

## Unknown variables

A misspelled prefixed variable (`MYAPP__PrOT`) is tracked as an *unused key*.
It's only turned into an error if you opt into `.env(|e| e.strict())` — and even
then it surfaces through the driver alongside the config dump, so the user sees
exactly what was understood. See
[Errors & Diagnostics](@/guide/errors-and-diagnostics.md).

Next: [Layered Configuration](@/guide/layered-configuration.md) — the builder,
the driver, and how all these layers combine.
