+++
title = "figue"
insert_anchor_links = "heading"
+++

<div class="hero">

# figue

<p class="hero-tagline">Type-safe CLI arguments, environment variables, and config files for Rust — one <code>#[derive(Facet)]</code>, every layer, with diagnostics that actually point at the mistake.</p>

</div>

```rust,noexec
use facet::Facet;
use figue::{self as args, FigueBuiltins};

#[derive(Facet)]
struct Args {
    /// File to process
    #[facet(args::positional)]
    input: String,

    /// Enable verbose output
    #[facet(args::named, args::short = 'v', default)]
    verbose: bool,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}

let args: Args = figue::from_slice(&["-v", "input.txt"]).unwrap();
assert!(args.verbose);
assert_eq!(args.input, "input.txt");
```

figue (pronounced *fig*, like the fruit) reads configuration from **CLI arguments**,
**environment variables**, and **config files**, merges them with a predictable
precedence, and deserializes the result into your own structs. It is built on
[facet](https://facet.rs) reflection, so a single derive gives you parsing, help
text, shell completions, and JSON Schema export — no macros to hand-write, no
runtime reflection cost.

## Choose your path

<div class="guide-cards">
<a class="guide-card" href="/guide">
  <div class="guide-card__icon"><img src="/icons/guide.svg" alt="" loading="lazy"></div>
  <h3 id="guide">Guide</h3>
  <p class="tagline">Learn figue step by step</p>
  <p class="description">From your first parsed flag to layered config, subcommands, env substitution, completions, and great error messages.</p>
</a>
<a class="guide-card" href="/recipes">
  <div class="guide-card__icon"><img src="/icons/recipes.svg" alt="" loading="lazy"></div>
  <h3 id="recipes">Recipes</h3>
  <p class="tagline">Copy-paste solutions</p>
  <p class="description">Self-contained examples for common shapes: a 12-factor service, a git-like multi-tool, a deploy CLI, and more.</p>
</a>
<a class="guide-card" href="/reference">
  <div class="guide-card__icon"><img src="/icons/reference.svg" alt="" loading="lazy"></div>
  <h3 id="reference">Reference</h3>
  <p class="tagline">Look it up fast</p>
  <p class="description">Every attribute, the builder API, the exact parsing grammar, the merge rules, and the full error catalog.</p>
</a>
</div>

## Why figue

- **One source of truth.** Your Rust types *are* the schema. CLI flags, env var
  names, config keys, help text, completions, and JSON Schema are all derived
  from the same struct.
- **Layered, with a predictable precedence.** `CLI > environment > config file >
  code defaults`. When a value is overridden, figue can tell you exactly which
  source won.
- **Diagnostics that point at the mistake.** Errors are rendered with
  [ariadne](https://docs.rs/ariadne), underlining the offending argument, env
  var, or line in the config file — with "did you mean …?" suggestions.
- **Batteries included.** `--help`, interactive `--html-help`, `--version`,
  `--completions <shell>`, and `--export-jsonschemas <dir>` come for free by
  flattening one struct.

## Quick links

- [Getting Started](@/guide/getting-started.md) — install figue and parse your first argument
- [Layered Configuration](@/guide/layered-configuration.md) — combine CLI, env, and files
- [Attribute Reference](@/reference/attributes.md) — every `#[facet(args::…)]` attribute
- [GitHub](https://github.com/bearcove/figue) — source and issues
- [docs.rs/figue](https://docs.rs/figue) — API documentation
