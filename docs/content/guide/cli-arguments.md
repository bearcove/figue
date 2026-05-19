+++
title = "CLI Arguments"
weight = 1
insert_anchor_links = "heading"
+++

Every field of your args struct needs exactly one *role* attribute that says how
it appears on the command line. This page covers all of them. For the exact
grammar (every edge case, in one place) see the
[CLI Grammar reference](@/reference/cli-grammar.md).

## Positional arguments

`#[facet(args::positional)]` consumes a bare argument, in declaration order.

```rust,noexec
use facet::Facet;
use figue as args;

#[derive(Facet, Debug)]
struct Args {
    /// Source path
    #[facet(args::positional)]
    src: String,

    /// Destination path
    #[facet(args::positional)]
    dst: String,
}

let a: Args = figue::from_slice(&["a.txt", "b.txt"]).unwrap();
assert_eq!(a.src, "a.txt");
assert_eq!(a.dst, "b.txt");
```

A positional whose type is `Vec<T>` greedily collects the rest:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::positional)]
    command: String,

    /// Everything after the command
    #[facet(args::positional, default)]
    rest: Vec<String>,
}

let a: Args = figue::from_slice(&["run", "--", "-x", "-y"]).unwrap();
assert_eq!(a.command, "run");
assert_eq!(a.rest, vec!["-x", "-y"]);
```

A required positional that the user omits produces a "missing required argument"
error (see [Errors & Diagnostics](@/guide/errors-and-diagnostics.md)). Make it
optional with `Option<T>` plus `#[facet(default)]`, or give it a default value.

## Named flags

`#[facet(args::named)]` creates a `--long-flag`. The flag name is the field name
in `kebab-case`:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Output directory
    #[facet(args::named)]
    out_dir: String,
}

// field `out_dir` -> flag `--out-dir`
let a: Args = figue::from_slice(&["--out-dir", "dist"]).unwrap();
assert_eq!(a.out_dir, "dist");

// `--flag=value` works too
let a: Args = figue::from_slice(&["--out-dir=dist"]).unwrap();
assert_eq!(a.out_dir, "dist");
```

Rename a flag explicitly with facet's `rename`:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, rename = "output")]
    out_dir: String, // now `--output`, not `--out-dir`
}
```

## Short flags

Add `#[facet(args::short = 'x')]` for a single-character alias:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Number of jobs
    #[facet(args::named, args::short = 'j')]
    jobs: usize,
}

for argv in [vec!["-j", "8"], vec!["-j8"], vec!["--jobs", "8"]] {
    let a: Args = figue::from_slice(&argv).unwrap();
    assert_eq!(a.jobs, 8);
}
```

`#[facet(args::short)]` with no character uses the first letter of the field
name.

### Clustering

Several short flags can be chained behind one `-`. Booleans stack; the last
character in a cluster may take a value:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, args::short = 'a', default)] all: bool,
    #[facet(args::named, args::short = 'b', default)] brief: bool,
    #[facet(args::named, args::short = 'j')] jobs: usize,
}

// -a -b together, then -j takes the next arg
let a: Args = figue::from_slice(&["-abj", "4"]).unwrap();
assert!(a.all && a.brief);
assert_eq!(a.jobs, 4);

// or attach the value: -abj4
let a: Args = figue::from_slice(&["-abj4"]).unwrap();
assert_eq!(a.jobs, 4);
```

## Boolean flags and negation

A `bool` field marked `args::named` is `true` when present, `false` otherwise.
Give it `#[facet(default)]` so the user can also just not pass it.

Every boolean flag automatically gets a `--no-` form to force it off — useful
when a later layer (env var, config file) turned it on:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Use the build cache
    #[facet(args::named, default)]
    cache: bool,
}

let a: Args = figue::from_slice(&["--cache"]).unwrap();
assert!(a.cache);

let a: Args = figue::from_slice(&["--no-cache"]).unwrap();
assert!(!a.cache);
```

> `--flag=VALUE` on a bool is lenient: the value counts as `true` only if it is
> one of `true`, `yes`, `1`, `on`, or empty (case-insensitive). Anything else —
> including a typo like `--cache=ture` — is treated as `false`. Prefer the bare
> `--cache` / `--no-cache` forms.

## Counted flags

For "more `-v` means more verbose", use `args::counted` on an integer field.
Each occurrence increments it, with saturating arithmetic:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Verbosity (-v, -vv, -vvv)
    #[facet(args::named, args::short = 'v', args::counted, default)]
    verbose: u8,
}

let a: Args = figue::from_slice(&["-vvv"]).unwrap();
assert_eq!(a.verbose, 3);

let a: Args = figue::from_slice(&["-v", "--verbose", "-v"]).unwrap();
assert_eq!(a.verbose, 3);

let a: Args = figue::from_slice(&[]).unwrap();
assert_eq!(a.verbose, 0); // counted flags default to 0
```

## Optional values

`Option<T>` makes a flag (or positional) optional without a default. Absent ⇒
`None`, present ⇒ `Some(value)`:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Optional config profile
    #[facet(args::named, default)]
    profile: Option<String>,
}

let a: Args = figue::from_slice(&[]).unwrap();
assert_eq!(a.profile, None);

let a: Args = figue::from_slice(&["--profile", "ci"]).unwrap();
assert_eq!(a.profile, Some("ci".to_string()));
```

## Repeated flags (lists)

A `Vec<T>` field marked `args::named` accumulates every occurrence:

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    /// Add a header (repeatable)
    #[facet(args::named, args::short = 'H', default)]
    header: Vec<String>,
}

let a: Args = figue::from_slice(
    &["-H", "A: 1", "-H", "B: 2", "--header", "C: 3"]
).unwrap();
assert_eq!(a.header, vec!["A: 1", "B: 2", "C: 3"]);
```

> On the command line, lists are built from **repeated flags**. figue does *not*
> split a single CLI value on commas — that splitting is an
> [environment-variable](@/guide/environment-variables.md) convenience only.

## The `--` terminator

The first `--` ends option parsing. Everything after it is positional, even if
it starts with a dash. A second `--` is just an ordinary positional.

```rust,noexec
#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, default)]
    verbose: bool,
    #[facet(args::positional, default)]
    passthrough: Vec<String>,
}

let a: Args = figue::from_slice(
    &["--verbose", "--", "--not-a-flag", "-x"]
).unwrap();
assert!(a.verbose);
assert_eq!(a.passthrough, vec!["--not-a-flag", "-x"]);
```

## Putting it together

```rust,noexec
use facet::Facet;
use figue::{self as args, FigueBuiltins};

/// Compress files.
#[derive(Facet, Debug)]
struct Args {
    /// Files to compress
    #[facet(args::positional)]
    files: Vec<String>,

    /// Compression level (0–9)
    #[facet(args::named, args::short = 'l', default)]
    level: Option<u8>,

    /// Keep original files
    #[facet(args::named, args::short = 'k', default)]
    keep: bool,

    /// Verbosity, repeatable
    #[facet(args::named, args::short = 'v', args::counted, default)]
    verbose: u8,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}

let a: Args = figue::from_slice(
    &["-kvv", "-l", "9", "a.txt", "b.txt"]
).unwrap();
assert_eq!(a.files, vec!["a.txt", "b.txt"]);
assert_eq!(a.level, Some(9));
assert!(a.keep);
assert_eq!(a.verbose, 2);
```

Next: [Subcommands](@/guide/subcommands.md), where the args struct holds an
enum.
