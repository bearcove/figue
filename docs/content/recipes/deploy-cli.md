+++
title = "A Deploy CLI"
weight = 2
insert_anchor_links = "heading"
+++

A deployment tool with a shared config file, per-environment overrides via env
substitution, an enum-backed target, and a `deploy` subcommand whose required
inputs are validated *after* parsing with
[requirements extraction](@/guide/requirements-extraction.md).

## `src/main.rs`

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver, FigueBuiltins};

/// Deploy things to environments.
#[derive(Facet, Debug)]
struct App {
    #[facet(args::config, args::env_prefix = "DEPLOY")]
    config: Config,

    #[facet(args::subcommand)]
    command: Command,

    #[facet(flatten)]
    builtins: FigueBuiltins,
}

#[derive(Facet, Debug)]
struct Config {
    /// Registry host. `${REGISTRY:-ghcr.io}` lets CI override it.
    #[facet(args::env_subst, default = "${REGISTRY:-ghcr.io}")]
    registry: String,

    /// Image to deploy. Optional in general; required by `deploy`.
    #[facet(default)]
    image: Option<String>,

    /// Where to deploy.
    #[facet(default)]
    target: Target,

    /// Concurrency for rollout.
    #[facet(default = 3)]
    max_in_flight: u32,
}

#[derive(Facet, Debug, Default)]
#[facet(rename_all = "kebab-case")]
#[repr(u8)]
enum Target {
    #[default]
    Staging,
    Production {
        /// Refuse unless explicitly confirmed.
        #[facet(default)]
        confirmed: bool,
    },
}

#[derive(Facet, Debug)]
#[repr(u8)]
enum Command {
    /// Print the resolved configuration and exit.
    Plan,
    /// Actually deploy.
    Deploy,
}

/// What `deploy` requires, wherever it came from.
#[derive(Facet, Debug)]
struct DeployRequirements {
    #[facet(args::origin = "config.image")]
    image: String,
    #[facet(args::origin = "config.registry")]
    registry: String,
}

fn main() {
    let config = builder::<App>()
        .unwrap()
        .cli(|c| c.args(std::env::args().skip(1)))
        .env(|e| e)
        .file(|f| f
            .format(args::JsoncFormat)
            .default_paths(["deploy.jsonc", "deploy.json"]))
        .help(|h| h
            .program_name("deploy")
            .version(env!("CARGO_PKG_VERSION")))
        .build();

    let out = Driver::new(config).run().into_result().unwrap_or_else(|e| {
        eprint!("{e}");
        std::process::exit(e.exit_code());
    });

    match out.value.command {
        Command::Plan => {
            println!("registry      = {}", out.value.config.registry);
            println!("image         = {:?}", out.value.config.image);
            println!("target        = {:?}", out.value.config.target);
            println!("max_in_flight = {}", out.value.config.max_in_flight);
        }
        Command::Deploy => {
            let req = out.extract::<DeployRequirements>().unwrap_or_else(|e| {
                eprint!("{e}");
                std::process::exit(1);
            });
            if let Target::Production { confirmed: false } = out.value.config.target {
                eprintln!("refusing: production deploy needs \
                    --config.target.production.confirmed");
                std::process::exit(1);
            }
            println!("deploying {}/{} …", req.registry, req.image);
        }
    }
}
```

## `deploy.jsonc`

```jsonc
{
  "config": {
    // ${REGISTRY} is filled from the environment at load time
    "registry": "${REGISTRY:-ghcr.io}",
    "max_in_flight": 5,
    "target": "staging"
  }
}
```

## Sessions

```bash
# Plan against staging using the file's defaults
deploy plan

# CI: registry comes from the environment via ${REGISTRY}
REGISTRY=registry.internal deploy plan

# Deploy — image is required only for THIS subcommand
deploy deploy
#   Missing required fields for this operation:
#     image <String> at config.image
#       Set via: --config.image or $DEPLOY__CONFIG__IMAGE

deploy deploy --config.image app:1.4.2
deploy deploy --config.image app:1.4.2 \
       --config.target.production.confirmed
```

## What this demonstrates

- **Env substitution with a default**: `registry` uses
  `${REGISTRY:-ghcr.io}`, so it works locally *and* lets CI inject a value —
  without making `registry` an env-sourced field.
- **JSONC config**: comments in `deploy.jsonc`, enabled with one
  `.format(JsoncFormat)`.
- **Enum target with a struct variant**: `--config.target.production.confirmed`
  navigates into the active variant from the CLI.
- **Per-operation requirements**: `image` is optional globally (so `plan`
  doesn't need it) but `deploy` extracts `DeployRequirements` and fails with a
  precise, actionable message if it's missing.
- **Manual outcome handling**: `.into_result()` so we can run requirements
  extraction and a domain check (production confirmation) before doing work.
