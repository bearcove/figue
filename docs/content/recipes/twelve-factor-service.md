+++
title = "A 12-Factor Service"
weight = 0
insert_anchor_links = "heading"
+++

A network service that follows [12-factor](https://12factor.net/config)
principles: configuration is read from the environment, with an optional config
file for local development and CLI overrides for ad-hoc runs. Defaults are safe
for production.

Precedence is exactly what an operator expects:
`flags > env > file > built-in defaults`.

## `src/main.rs`

```rust,noexec
use facet::Facet;
use facet_pretty::FacetPretty;
use figue::{self as args, builder, Driver, FigueBuiltins};

/// A 12-factor HTTP service.
#[derive(Facet, Debug)]
struct App {
    /// Configuration (env: `SVC__…`, file: `service.json`, CLI: `--config.…`)
    #[facet(args::config, args::env_prefix = "SVC")]
    config: ServiceConfig,

    /// --help / --version / --completions / --export-jsonschemas
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

#[derive(Facet, Debug)]
struct ServiceConfig {
    /// Interface to bind.
    #[facet(default = "0.0.0.0")]
    host: String,

    /// TCP port. Override per-environment with `SVC__PORT`.
    #[facet(default = 8080)]
    port: u16,

    /// Database connection string. Required — there is no safe default.
    /// Honors the conventional `$DATABASE_URL` as well as `SVC__DATABASE_URL`.
    #[facet(args::env_alias = "DATABASE_URL")]
    database_url: String,

    /// Worker pool size.
    #[facet(default = 16)]
    workers: usize,

    /// Structured-logging toggle.
    #[facet(default)]
    json_logs: bool,

    /// TLS is off unless this whole block is configured.
    #[facet(default)]
    tls: Option<Tls>,
}

#[derive(Facet, Debug)]
struct Tls {
    cert_path: String,
    key_path: String,
}

fn main() {
    let config = builder::<App>()
        .unwrap()
        .cli(|cli| cli.args(std::env::args().skip(1)))
        .env(|env| env) // real process environment
        .file(|f| f.default_paths([
            "./service.json",
            "/etc/myservice/service.json",
        ]))
        .help(|h| h
            .program_name(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION"))
            .description("Set SVC__DATABASE_URL (or $DATABASE_URL) before starting."))
        .build();

    let app: App = Driver::new(config).run().unwrap();

    // `app.builtins.*` were already handled by `.unwrap()`.
    eprintln!("effective configuration:\n{}", app.config.pretty());
    serve(app.config);
}

fn serve(_cfg: ServiceConfig) { /* … */ }
```

## Running it

```bash
# Production: everything from the environment
SVC__DATABASE_URL=postgres://db/app SVC__PORT=443 myservice

# Local dev: a file supplies the boring parts, env supplies the secret
cat > service.json <<'JSON'
{ "config": { "port": 3000, "json_logs": false } }
JSON
DATABASE_URL=postgres://localhost/app_dev myservice

# Ad-hoc: override one thing without touching env or file
DATABASE_URL=postgres://localhost/app_dev myservice --config.workers 4
```

## Why this shape works

- **Secrets stay in the environment.** `database_url` has no default, so a
  missing value is a *loud, explained* startup error — never a silent fallback.
  `env_alias` lets it also accept the platform-conventional `$DATABASE_URL`.
- **The file is optional.** No file? No problem — env + defaults still produce a
  valid config. The file is a developer convenience, not a requirement.
- **`tls: Option<Tls>`** means TLS is genuinely off until an operator configures
  both paths; it is never half-synthesized from defaults.
- **Operators can introspect.** `myservice --export-jsonschemas ./schemas`
  gives them an editor-validated `service.json`, and any startup error prints
  the full provenance dump showing exactly which source set each value.
