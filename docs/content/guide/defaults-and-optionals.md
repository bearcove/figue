+++
title = "Defaults & Optionals"
weight = 6
insert_anchor_links = "heading"
+++

"Is this field required?" has a precise answer in figue. This page gives you the
rule and the three ways to make something optional.

## The required rule

For **CLI args and the config dump**, a field is *required* unless at least one
of these is true:

- it is `Option<T>`,
- it has a `#[facet(default)]` or `#[facet(default = …)]`,
- it is a `bool` (defaults to `false`),
- it is a counted flag (defaults to `0`),
- it is an optional subcommand.

Everything else — a bare `String`, `u16`, `PathBuf`, a struct field, an enum
without a default — is required, and omitting it produces a "missing required
field" diagnostic that names the exact flag / env var / config key that would
set it.

> JSON Schema export uses a slightly different notion of required (a `bool`
> without a default *is* required there). That only affects the emitted schema,
> not parsing. See the [JSON Schema reference](@/reference/json-schema.md).

## Three ways to be optional

### 1. A default value

`#[facet(default = expr)]` supplies a compile-time value. The field is then
optional, and the default shows up in `--help` and the config dump as `DEFAULT`.

```rust,noexec
use facet::Facet;
use figue as args;

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, default = 8080)]
    port: u16,

    #[facet(args::named, default = "localhost")]
    host: String,

    #[facet(args::named, default = true)]
    color: bool,
}

let a: Args = figue::from_slice(&[]).unwrap();
assert_eq!(a.port, 8080);
assert_eq!(a.host, "localhost");
assert!(a.color);
```

### 2. A bare `#[facet(default)]`

Uses the type's `Default::default()`. Good for collections, bools, and types
that implement `Default`:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, default)]
    verbose: bool,            // false

    #[facet(args::named, default)]
    tags: Vec<String>,        // empty

    #[facet(args::named, default)]
    profile: Option<String>,  // None
}
```

### 3. `Option<T>`

`Option<T>` is optional with no default needed. Absent ⇒ `None`. Pair it with
`#[facet(default)]` so it can also be omitted on the CLI/config without an
error:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Only set when the user asks for it
    #[facet(args::named, default)]
    timeout_secs: Option<u64>,
}

let a: Args = figue::from_slice(&[]).unwrap();
assert_eq!(a.timeout_secs, None);

let a: Args = figue::from_slice(&["--timeout-secs", "30"]).unwrap();
assert_eq!(a.timeout_secs, Some(30));
```

## Defaults interact with layers

A `#[facet(default)]` is the **lowest-priority layer**. Any real value from a
file, env var, or the CLI overrides it. In the provenance dump a defaulted value
shows `DEFAULT`; the moment any source provides it, the dump shows that source
instead. This is what makes "ship sane defaults, override per environment" work
without special-casing.

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config)]
    config: Cfg,
}
#[derive(Facet, Debug)]
struct Cfg {
    #[facet(default = 8080)]
    port: u16,
}

use figue::{builder, Driver};

// nothing supplies port -> default
let c = builder::<Args>().unwrap().build();
assert_eq!(Driver::new(c).run().into_result().unwrap().value.config.port, 8080);

// file supplies it -> file wins over default
let c = builder::<Args>().unwrap()
    .file(|f| f.content(r#"{"config":{"port":3000}}"#, "c.json"))
    .build();
assert_eq!(Driver::new(c).run().into_result().unwrap().value.config.port, 3000);
```

## Required fields in nested config

The rule applies at every depth. A required field deep inside a config struct
that nobody fills is reported with its full path and the precise override:

```text
Missing:
  database.url <String> (--config.database.url or $MYAPP__DATABASE__URL)
```

Use `#[facet(default)]` / `Option<T>` on the inner field, or make the user
supply it — figue makes the requirement and the fix obvious.

Next: [Environment Substitution](@/guide/env-substitution.md) for `${VAR}`
interpolation inside values.
