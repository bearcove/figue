+++
title = "Help & Completions"
weight = 9
insert_anchor_links = "heading"
+++

Flatten one struct and you get `--help`, `--html-help`, `--version`,
`--completions`, and `--export-jsonschemas` — all generated from your types and
doc comments.

## FigueBuiltins

```rust,noexec
use facet::Facet;
use figue::{self as args, FigueBuiltins};

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::positional)]
    input: String,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}
```

`FigueBuiltins` contributes these flags:

| Flag | Field type | Effect (with `.unwrap()`) |
|---|---|---|
| `--help` / `-h` | `bool` | print help, exit `0` |
| `--html-help` | `bool` | open interactive HTML help, exit `0` |
| `--version` / `-V` | `bool` | print `name version`, exit `0` |
| `--completions <shell>` | `Option<Shell>` | print completion script, exit `0` |
| `--export-jsonschemas <DIR>` | `Option<String>` | write JSON Schemas, exit `0` |

You don't have to flatten the whole struct — you can instead mark your own
fields with `args::help`, `args::version`, `args::completions`,
`args::export_jsonschemas`. Flattening `FigueBuiltins` is just the convenient
default.

## What help looks like

Help is built from the struct: the type's doc comment becomes the description,
each field's `///` becomes its entry, defaults and value placeholders are
filled in automatically.

```text
myapp 1.0.0

A sample CLI application.

USAGE:
    myapp [OPTIONS] <INPUT> [OUTPUT]

ARGUMENTS:
        <INPUT>
            Input file to process
        <OUTPUT>
            Output file (optional)

OPTIONS:
    -v, --[no-]verbose
            Enable verbose output
    -j, --jobs <COUNT>
            Number of parallel jobs
    -h, --[no-]help
            Show help message and exit.
        --completions <bash,zsh,fish>
            Generate shell completions.
```

Note the details: booleans render as `--[no-]verbose` (their negation is real,
see [CLI Arguments](@/guide/cli-arguments.md#boolean-flags-and-negation)),
enum-valued flags show their variants (`<bash,zsh,fish>`), and a custom
`#[facet(args::label = "COUNT")]` overrides the `<TYPE>` placeholder.

### Subcommand help

`myapp <cmd> --help` shows help for that command; nesting works too
(`myapp remote add --help`). When a required subcommand or required argument is
missing, figue shows the relevant help rather than a bare error — it teaches
instead of scolding.

### Width and program name

```rust,noexec
use figue::builder;

let config = builder::<Args>()
    .unwrap()
    .help(|h| h
        .program_name("myapp")
        .version(env!("CARGO_PKG_VERSION"))
        .description("Extra text shown under the summary.")
        .width(100)) // 0 disables wrapping
    .build();
```

## Interactive HTML help

`--html-help` writes a self-contained HTML page (search box, sidebar,
collapsible config schema, copy buttons) to a temp file and opens it in the
browser. It always renders from the root so search covers the whole CLI. Great
for tools with deep config trees — try it on a kitchen-sink config and search
for a buried field.

## Shell completions

`Shell` is `bash`, `zsh`, or `fish`. `--completions <shell>` prints a script to
stdout; redirect it into the right place:

```bash
# bash
myapp --completions bash | sudo tee /etc/bash_completion.d/myapp

# zsh — into a directory on your $fpath, as _myapp
myapp --completions zsh > ~/.zsh/completions/_myapp
# (or: eval "$(myapp --completions zsh)" in ~/.zshrc)

# fish
myapp --completions fish > ~/.config/fish/completions/myapp.fish
```

`--completions` with no argument tries to auto-detect from `$SHELL`. The
generated scripts complete flags, subcommands (recursively), and fall back to
file completion for value-taking flags.

> Set `program_name` via the builder. Without it, completion functions and
> `--version` fall back to `argv[0]` (which can be an ugly absolute path in
> tests) and the version prints `unknown`.

## JSON Schema for config files

`--export-jsonschemas <dir>` writes one `*.schema.json` per config root so
editors can validate and autocomplete your config files. See
[JSON Schema Export](@/guide/json-schema.md).

Next: [JSON Schema Export](@/guide/json-schema.md).
