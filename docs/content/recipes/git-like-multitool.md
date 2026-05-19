+++
title = "A git-like Multi-Tool"
weight = 1
insert_anchor_links = "heading"
+++

A CLI with nested subcommands, per-command flags, renamed commands, and a global
flag that works before *or* after the subcommand — the shape of `git`, `cargo`,
or `kubectl`.

## `src/main.rs`

```rust,noexec
use facet::Facet;
use figue::{self as args, FigueBuiltins};

/// A tiny version-control-ish tool.
#[derive(Facet, Debug)]
struct Cli {
    /// Verbose output (repeatable). Works anywhere: `vcs -vv clone …`
    /// or `vcs clone … -vv`.
    #[facet(args::named, args::short = 'v', args::counted, default)]
    verbose: u8,

    #[facet(args::subcommand)]
    command: Command,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    /// Clone a repository.
    Clone {
        /// Repository URL.
        #[facet(args::positional)]
        url: String,
        /// Target directory (defaults to the repo name).
        #[facet(args::positional, default)]
        directory: Option<String>,
        /// Create a shallow clone.
        #[facet(args::named, default)]
        depth: Option<u32>,
    },

    /// Show commit history.
    Log {
        /// Limit to N commits.
        #[facet(args::named, args::short = 'n', default)]
        count: Option<usize>,
        /// One line per commit.
        #[facet(args::named, default)]
        oneline: bool,
    },

    /// Manage remotes (has its own subcommands).
    Remote {
        #[facet(args::subcommand)]
        action: RemoteAction,
    },
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum RemoteAction {
    /// Add a remote.
    Add {
        #[facet(args::positional)]
        name: String,
        #[facet(args::positional)]
        url: String,
    },
    /// Remove a remote.
    #[facet(rename = "rm")]
    Remove {
        #[facet(args::positional)]
        name: String,
    },
    /// List remotes.
    #[facet(rename = "ls")]
    List {
        #[facet(args::named, args::short = 'v', default)]
        verbose: bool,
    },
}

fn main() {
    let cli: Cli = args::from_std_args().unwrap();

    if cli.verbose > 0 {
        eprintln!("(verbosity: {})", cli.verbose);
    }

    match cli.command {
        Command::Clone { url, directory, depth } => {
            println!("cloning {url} into {:?} (depth {:?})", directory, depth);
        }
        Command::Log { count, oneline } => {
            println!("log: count={count:?} oneline={oneline}");
        }
        Command::Remote { action } => match action {
            RemoteAction::Add { name, url } => println!("remote add {name} {url}"),
            RemoteAction::Remove { name }   => println!("remote rm {name}"),
            RemoteAction::List { verbose }  => println!("remote ls (verbose={verbose})"),
        },
    }
}
```

## Try it

```bash
vcs clone https://example.com/repo.git
vcs clone https://example.com/repo.git myrepo --depth 1
vcs -vv log -n 20 --oneline
vcs log --oneline -vv          # global -v AFTER the subcommand: still works
vcs remote add origin git@…    # nested subcommand
vcs remote rm origin           # renamed variant (Remove -> "rm")
vcs remote --help              # help for the intermediate level
vcs remote add --help          # help for the leaf
vcs                            # required subcommand missing -> shows top-level help
vcs remffff                    # unknown subcommand -> "did you mean 'remote'?"
```

## What this demonstrates

- **Nested subcommands**: `Command::Remote` carries another
  `#[facet(args::subcommand)]`, so `vcs remote add` is a two-level path. Help
  works at every level (`vcs remote --help`, `vcs remote add --help`).
- **The adoption agency**: `verbose` is declared on the parent before the
  subcommand, so `-v` binds to it whether the user types it before or after the
  subcommand — the flag bubbles up the parent stack.
- **Renames**: `#[facet(rename = "rm")]` / `"ls"` give friendly command names
  without renaming the Rust variants.
- **Guided failure**: a missing required subcommand shows the top-level help
  instead of a terse error; an unknown subcommand gets a "did you mean?".
- **Free batteries**: flattening `FigueBuiltins` gives `--help`, `--version`,
  and `--completions <shell>` across the whole tree — try
  `vcs --completions zsh`.
