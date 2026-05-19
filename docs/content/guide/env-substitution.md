+++
title = "Environment Substitution"
weight = 7
insert_anchor_links = "heading"
+++

Sometimes you don't want a value to *come from* an environment variable — you
want it to *contain* one. `${VAR}` substitution lets a config file say
`"${DATA_DIR}/cache"` and have it expanded at load time. It is **opt-in per
field**, so values that legitimately contain `${…}` aren't mangled.

## Opt in with `args::env_subst`

```rust,noexec
use facet::Facet;
use figue::{self as args, builder, Driver, MockEnv};
use std::path::PathBuf;

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::config)]
    config: Cfg,
}

#[derive(Facet, Debug)]
struct Cfg {
    /// "${BASE}/data" -> "/srv/app/data"
    #[facet(args::env_subst)]
    data_dir: PathBuf,
}

let config = builder::<Args>()
    .unwrap()
    .env(|env| env.source(MockEnv::from_pairs([("BASE", "/srv/app")])))
    .file(|f| f.content(
        r#"{ "config": { "data_dir": "${BASE}/data" } }"#,
        "app.json",
    ))
    .build();

let out = Driver::new(config).run().into_result().unwrap();
assert_eq!(out.value.config.data_dir, PathBuf::from("/srv/app/data"));
```

## Whole-struct opt-in

`#[facet(args::env_subst_all)]` on a struct applies substitution to all of its
**direct** fields. It does *not* recurse into nested structs (mirroring how
`rename_all` behaves), but it *does* apply to `#[facet(flatten)]`ed fields,
since those become direct children.

```rust,noexec
#[derive(Facet, Debug)]
#[facet(args::env_subst_all)]
struct Cfg {
    data_dir: PathBuf,   // substituted
    cache_dir: PathBuf,  // substituted
    nested: Other,       // nested.* is NOT substituted
}

#[derive(Facet, Debug)]
struct Other {
    log_dir: PathBuf,    // would need its own env_subst / env_subst_all
}
```

## The grammar

| Syntax | Meaning |
|---|---|
| `${VAR}` | value of `VAR`; **error** if undefined and no default |
| `${VAR:-fallback}` | value of `VAR`, or the literal `fallback` if unset |
| `${VAR:-}` | value of `VAR`, or empty string |
| `$$` | a literal `$` |
| `$5.00`, `$HOME` (no braces) | left as-is — only `${…}` and `$$` are special |
| `${unclosed` | emitted literally; no error |

Multiple substitutions per string are fine: `"${HOST}:${PORT}"`.

```rust,noexec
#[derive(Facet, Debug)]
struct Cfg {
    /// Falls back to 5432 if $DB_PORT is unset
    #[facet(args::env_subst)]
    dsn: String, // "postgres://db:${DB_PORT:-5432}/app"
}
```

## When it runs

Substitution happens **once, after merging**, on the final value of each
`env_subst` field — regardless of which layer that value came from. So a
substitutable string supplied on the CLI or via a config file is expanded the
same way. A field *without* `env_subst` keeps any `${…}` text verbatim.

An undefined variable with no `:-default` is a hard error
(`DriverError::EnvSubst`), exit code 1, with a clear message — it never silently
becomes an empty string.

Next: [Errors & Diagnostics](@/guide/errors-and-diagnostics.md).
