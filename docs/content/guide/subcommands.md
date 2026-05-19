+++
title = "Subcommands"
weight = 2
insert_anchor_links = "heading"
+++

A subcommand is an enum field marked `#[facet(args::subcommand)]`. Each variant
is a subcommand; its fields are that subcommand's arguments. This is how you
build `cargo build` / `cargo run` style tools.

## The basics

```rust,noexec
use facet::Facet;
use figue::{self as args, FigueBuiltins};

#[derive(Facet, Debug)]
struct Cli {
    #[facet(args::subcommand)]
    command: Command,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    /// Build the project
    Build {
        /// Build in release mode
        #[facet(args::named, args::short = 'r', default)]
        release: bool,
    },
    /// Run the project
    Run {
        /// Arguments passed to the program
        #[facet(args::positional, default)]
        args: Vec<String>,
    },
}

let cli: Cli = figue::from_slice(&["build", "--release"]).unwrap();
match cli.command {
    Command::Build { release } => assert!(release),
    Command::Run { .. } => unreachable!(),
}
```

The enum **must** be `#[repr(u8)]` (a facet requirement for data-carrying
enums). Variant names are matched in `kebab-case`: `PrintConfig` becomes
`print-config`.

## Unit variants

A variant with no fields is a subcommand that takes no arguments:

```rust,noexec
#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    /// Print the merged configuration and exit
    PrintConfig,
    /// Start the server
    Serve {
        #[facet(args::named, default)]
        port: Option<u16>,
    },
}
```

`myapp print-config` selects `Command::PrintConfig`.

## Renaming subcommands

Use facet's `rename` on a variant, or `rename_all` on the enum:

```rust,noexec
#[derive(Facet, Debug)]
#[repr(u8)]
enum RemoteAction {
    /// Add a remote
    Add {
        #[facet(args::positional)]
        name: String,
    },
    /// Remove a remote
    #[facet(rename = "rm")]
    Remove {
        #[facet(args::positional)]
        name: String,
    },
}
// `myapp rm origin` selects RemoteAction::Remove
```

## Nesting

A variant can itself contain a subcommand field, giving you
`git remote add`-style trees:

```rust,noexec
#[derive(Facet, Debug)]
struct Cli {
    #[facet(args::subcommand)]
    command: Command,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    /// Manage remotes
    Remote {
        #[facet(args::subcommand)]
        action: RemoteAction,
    },
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum RemoteAction {
    Add {
        #[facet(args::positional)]
        name: String,
        #[facet(args::positional)]
        url: String,
    },
    #[facet(rename = "rm")]
    Remove {
        #[facet(args::positional)]
        name: String,
    },
}

let cli: Cli = figue::from_slice(&["remote", "add", "origin", "git@…"]).unwrap();
```

`myapp remote add --help` and `myapp remote --help` both produce help for the
right level — see [Help & Completions](@/guide/help-and-completions.md).

## Global flags and the "adoption agency"

Flags declared *before* the subcommand field on the parent struct are global:
the user can pass them after the subcommand and they still bind to the parent.
figue calls this the *adoption agency* — an unrecognized flag at the subcommand
level bubbles up to parent levels before becoming an error.

```rust,noexec
#[derive(Facet, Debug)]
struct Cli {
    /// Verbose (works before OR after the subcommand)
    #[facet(args::named, args::short = 'v', args::counted, default)]
    verbose: u8,

    #[facet(args::subcommand)]
    command: Command,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    Build {
        #[facet(args::named, default)]
        release: bool,
    },
}

// -v after the subcommand still increments the parent's `verbose`
let cli: Cli = figue::from_slice(&["build", "--release", "-vv"]).unwrap();
assert_eq!(cli.verbose, 2);
```

## Optional subcommands

Make the subcommand optional with `Option<Enum>` plus `#[facet(default)]`. If
the user passes no subcommand, `command` is `None` instead of an error:

```rust,noexec
#[derive(Facet, Debug)]
struct Cli {
    #[facet(args::subcommand, default)]
    command: Option<Command>,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    Status,
}

let cli: Cli = figue::from_slice(&[]).unwrap();
assert!(cli.command.is_none());
```

If the subcommand is **required** and missing, figue shows the top-level help
rather than a bare error — guiding the user toward the available commands.

## Short aliases for subcommands

Variants may carry a one-letter alias with `#[facet(args::short = 'b')]`. Unlike
flag shorts, subcommand shorts live in their own namespace, so a `-d` flag and a
`-d`-aliased subcommand don't conflict. Two subcommands sharing an alias, or an
alias colliding with another subcommand's name, is rejected at startup with a
[schema error](@/reference/errors.md#schemaerror).

Continue to [Config Files](@/guide/config-files.md) to start layering
configuration sources.
