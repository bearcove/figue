+++
title = "Config Files"
weight = 3
insert_anchor_links = "heading"
+++

So far every value came from `argv`. Real applications also read a config file.
In figue, a config file fills a struct marked `#[facet(args::config)]` — a
*config root*. The same struct also drives env vars and CLI overrides; this page
focuses on the file part. The whole picture is in
[Layered Configuration](@/guide/layered-configuration.md).

## A config root

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver};

#[derive(Facet, Debug)]
struct Args {
    /// Application configuration
    #[facet(args::config)]
    config: AppConfig,
}

#[derive(Facet, Debug)]
struct AppConfig {
    /// Server host
    #[facet(default = "localhost")]
    host: String,
    /// Server port
    #[facet(default = 8080)]
    port: u16,
}

let config = builder::<Args>()
    .unwrap()
    .file(|f| f.content(
        r#"{ "config": { "port": 9000 } }"#,
        "app.json",
    ))
    .build();

let out = Driver::new(config).run().into_result().unwrap();
assert_eq!(out.value.config.host, "localhost"); // default
assert_eq!(out.value.config.port, 9000);        // from file
```

Note the file's top-level key is `config` — the **field name** of the config
root. With multiple config roots, each gets its own top-level key.

`.content(text, filename)` is the testing-friendly way to supply a file: no disk
I/O, and the filename drives format detection. Production code uses
`.default_paths(…)` instead.

## Where figue looks for the file

`default_paths` is an ordered list; the **first file that exists** wins. If none
exist, that's fine — the file layer is optional and figue falls back to env vars
and defaults.

```rust,noexec
let config = builder::<Args>()
    .unwrap()
    .file(|f| f.default_paths([
        "./app.json",
        "/etc/myapp/app.json",
    ]))
    .build();
```

The user can always point at a specific file with the auto-generated
`--<root> <PATH>` flag. For a root field named `config`, that's `--config`:

```bash
myapp --config /tmp/override.json
```

> **`~` is not expanded.** Paths are used verbatim. `"~/.config/app.json"` looks
> for a literal `~` directory, not your home directory. Resolve `$HOME` yourself
> (e.g. with the [`directories`](https://docs.rs/directories) or
> [`etcetera`](https://docs.rs/etcetera) crates) and pass absolute paths.

## JSON and JSONC

The default registry understands **JSON only**. JSON-with-comments (`.jsonc`,
`//` and `/* */`) is one line away:

```rust,noexec
use figue::JsoncFormat;

let config = builder::<Args>()
    .unwrap()
    .file(|f| f
        .format(JsoncFormat)
        .default_paths(["app.jsonc", "app.json"]))
    .build();
```

```jsonc
{
  // dev box
  "config": {
    "port": 9000 /* avoid 8080, taken by nginx */
  }
}
```

## TOML, YAML, or anything else

Add a format by implementing [`ConfigFormat`](@/reference/config-file-formats.md)
and registering it with `.format(MyFormat)`. A format maps file extensions to a
parser that returns a `ConfigValue`. This is how you'd wire up `facet-toml` or
`facet-yaml`. The full trait and a worked custom format are in the
[Config File Formats reference](@/reference/config-file-formats.md).

## Nested and enum config

Config structs nest freely, and enums become externally-tagged objects:

```rust,noexec
#[derive(Facet, Debug)]
struct AppConfig {
    server: Server,
    storage: Storage,
}

#[derive(Facet, Debug)]
struct Server {
    #[facet(default = "0.0.0.0")]
    host: String,
    #[facet(default = 8080)]
    port: u16,
}

#[derive(Facet, Debug)]
#[facet(rename_all = "kebab-case")]
#[repr(u8)]
enum Storage {
    Local { path: String },
    S3 { bucket: String, #[facet(default = "us-east-1")] region: String },
    Memory,
}
```

```json
{
  "config": {
    "server": { "port": 9090 },
    "storage": { "s3": { "bucket": "my-data" } }
  }
}
```

`Memory` (a unit variant) would just be `"storage": "memory"`.

## Rejecting unknown keys

By default, keys in the file that don't match any field are *ignored* (handy for
forward-compatible configs). Call `.strict()` to turn unknown keys into an error
instead — figue still prints the config dump and a "did you mean …?" suggestion:

```rust,noexec
let config = builder::<Args>()
    .unwrap()
    .file(|f| f
        .strict()
        .content(r#"{ "config": { "prot": 9000 } }"#, "app.json"))
    .build();

// errors: unknown key `prot` (did you mean `port`?)
let result = Driver::new(config).run().into_result();
assert!(result.is_err());
```

## Where files sit in the precedence

A value from a file beats a code default, but loses to an environment variable
or a CLI argument:

> `CLI  >  environment  >  config file  >  code defaults`

The next page, [Environment Variables](@/guide/environment-variables.md), adds
the middle layer; [Layered Configuration](@/guide/layered-configuration.md) ties
it all together and shows the provenance dump that tells you which source won.
