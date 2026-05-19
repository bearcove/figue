+++
title = "Getting Started"
weight = 0
insert_anchor_links = "heading"
+++

This page takes you from an empty crate to a working CLI that parses arguments
and prints `--help`, in about five minutes.

## Prerequisites

- Rust stable, recent (figue's MSRV tracks a recent stable).
- `cargo` on your `PATH`.

## Create a project

```bash
cargo new figue-hello
cd figue-hello
```

## Add dependencies

figue is built on [facet](https://facet.rs). You need both `facet` (for the
`Facet` derive on your types) and `figue` (for the parsing):

```toml
# Cargo.toml
[dependencies]
facet = "0.46"
figue = "4"
```

(Check [crates.io/crates/figue](https://crates.io/crates/figue) for the current
release; figue tracks a specific `facet` version, so prefer the latest of both.)

> **Doc comments become help text.** figue turns the `///` comments on your
> fields into help descriptions. This relies on facet's `doc` feature, which is
> on by default — you don't need to do anything, but if you ever strip docs in
> release builds, your `--help` text goes with them.

## Your first parser

A figue parser is just a struct that derives `Facet`. Each field gets a
`#[facet(args::…)]` attribute that says *how* it appears on the command line.

```rust,noexec
// src/main.rs
use facet::Facet;
use figue::{self as args, FigueBuiltins};

/// Greet someone, optionally loudly.
#[derive(Facet, Debug)]
struct Args {
    /// Who to greet
    #[facet(args::positional)]
    name: String,

    /// Greet in all caps
    #[facet(args::named, args::short = 'l', default)]
    loud: bool,

    /// --help / --version / --completions, for free
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

fn main() {
    let args: Args = figue::from_std_args().unwrap();

    let greeting = format!("Hello, {}!", args.name);
    if args.loud {
        println!("{}", greeting.to_uppercase());
    } else {
        println!("{greeting}");
    }
}
```

The `use figue as args;` aliasing trick lets you write the short
`#[facet(args::positional)]` instead of `#[facet(figue::positional)]`. Both
work; the alias just reads better. This guide uses `args::` throughout.

## Run it

```bash
$ cargo run -- World
Hello, World!

$ cargo run -- --loud World
HELLO, WORLD!

$ cargo run -- -l World
HELLO, WORLD!
```

## You already have `--help`

Because you flattened [`FigueBuiltins`](@/reference/entry-points.md#figuebuiltins),
the standard flags work out of the box:

```bash
$ cargo run -- --help
figue-hello

Greet someone, optionally loudly.

USAGE:
    figue-hello [OPTIONS] <NAME>

ARGUMENTS:
        <NAME>
            Who to greet

OPTIONS:
    -l, --[no-]loud
            Greet in all caps
    -h, --[no-]help
            Show help message and exit.
        --[no-]html-help
            Open HTML help in the browser and exit.
    -V, --[no-]version
            Show version and exit.
        --completions <bash,zsh,fish>
            Generate shell completions.
        --export-jsonschemas <DIR>
            Export JSON Schema files for all config roots.
```

`--help` and `--version` exit with status `0`. A parse error (missing `name`,
an unknown flag) exits with status `1` and prints a diagnostic. You didn't write
any of that.

## Set your program name and version

By default `--version` prints `unknown`, because figue cannot see your crate's
`Cargo.toml` at compile time — only *your* crate can. Wire it up with the
[builder](@/guide/layered-configuration.md):

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver, FigueBuiltins};

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::positional)]
    name: String,
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

fn main() {
    let config = builder::<Args>()
        .unwrap()
        .cli(|cli| cli.args(std::env::args().skip(1)))
        .help(|h| h
            .program_name(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION")))
        .build();

    let args: Args = Driver::new(config).run().unwrap();
    println!("Hello, {}!", args.name);
}
```

`env!("CARGO_PKG_VERSION")` is expanded by *your* crate's compiler, so it picks
up the right version.

## Where to next

- [CLI Arguments](@/guide/cli-arguments.md) — positionals, flags, short options, counting
- [Subcommands](@/guide/subcommands.md) — `myapp build`, `myapp run`, …
- [Layered Configuration](@/guide/layered-configuration.md) — add env vars and config files
- [Errors & Diagnostics](@/guide/errors-and-diagnostics.md) — what users see when they get it wrong
