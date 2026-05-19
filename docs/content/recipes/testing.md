+++
title = "Testing figue Parsers"
weight = 3
insert_anchor_links = "heading"
+++

figue is designed to be tested through the *real* parsing path — no mocking of
internals, no special test mode. You feed it arguments, a fake environment, and
inline file content, and assert on the deserialized struct. These are the kinds
of tests worth writing: they exercise exactly what production does.

## CLI: `from_slice`

The simplest case. `from_slice` is the same code path as `from_std_args`,
minus reading the real argv.

```rust,noexec
use facet::Facet;
use figue as args;

#[derive(Facet, Debug, PartialEq)]
struct Args {
    #[facet(args::positional)]
    input: String,
    #[facet(args::named, args::short = 'j', default = 1)]
    jobs: usize,
    #[facet(args::named, default)]
    verbose: bool,
}

#[test]
fn parses_short_and_positional() {
    let a: Args = figue::from_slice(&["-j", "8", "in.txt"]).unwrap();
    assert_eq!(a, Args { input: "in.txt".into(), jobs: 8, verbose: false });
}

#[test]
fn defaults_apply() {
    let a: Args = figue::from_slice(&["in.txt"]).unwrap();
    assert_eq!(a.jobs, 1);
}

#[test]
fn missing_required_is_an_error() {
    let result = figue::from_slice::<Args>(&[]).into_result();
    assert!(result.is_err());
}
```

Use `.into_result()` in tests so a parse failure is an assertion, not a panic
that aborts the test binary.

## Asserting on help / version

`--help` and `--version` come back as `Err(DriverError::…)` whose `Display` is
the user-facing text — perfect for snapshotting.

```rust,noexec
use figue::DriverError;

#[test]
fn help_mentions_the_flag() {
    match figue::from_slice::<Args>(&["--help"]).into_result() {
        Err(e @ DriverError::Help { .. }) => {
            assert!(e.is_success());
            assert!(format!("{e}").contains("--jobs"));
        }
        other => panic!("expected help, got {other:?}"),
    }
}
```

Pair this with [`insta`](https://insta.rs) for golden-file help/diagnostic
tests; figue disables ANSI color under `insta` automatically, so snapshots are
stable.

## Environment: `MockEnv`

Never mutate the real process environment in tests (it's global and racy).
`MockEnv` is an in-memory environment for the env layer.

```rust,noexec
use figue::{builder, Driver, MockEnv};

#[derive(Facet, Debug)]
struct App {
    #[facet(args::config, args::env_prefix = "APP")]
    config: Cfg,
}

#[derive(Facet, Debug)]
struct Cfg {
    #[facet(default = 8080)]
    port: u16,
    #[facet(default)]
    debug: bool,
}

#[test]
fn env_overrides_default_but_loses_to_cli() {
    let config = builder::<App>()
        .unwrap()
        .cli(|c| c.args(["--config.port", "9999"]))
        .env(|e| e.source(MockEnv::from_pairs([
            ("APP__PORT", "3000"),
            ("APP__DEBUG", "true"),
        ])))
        .build();

    let out = Driver::new(config).run().into_result().unwrap();
    assert_eq!(out.value.config.port, 9999);  // CLI wins over env
    assert!(out.value.config.debug);          // env wins over default
}
```

## Files: inline content

`.content(text, filename)` injects a config file with no disk I/O. The filename
only drives format detection.

```rust,noexec
#[test]
fn file_fills_the_gaps() {
    let config = builder::<App>()
        .unwrap()
        .file(|f| f.content(
            r#"{ "config": { "port": 5000 } }"#,
            "app.json",
        ))
        .build();

    let out = Driver::new(config).run().into_result().unwrap();
    assert_eq!(out.value.config.port, 5000);   // from file
    assert!(!out.value.config.debug);          // still the default
}
```

For JSONC, register the format and name the file `*.jsonc`:

```rust,noexec
.file(|f| f.format(figue::JsoncFormat).content(
    r#"{ "config": { /* dev */ "port": 5000 } }"#,
    "app.jsonc",
))
```

## All layers at once

The highest-value test: assert the precedence end to end.

```rust,noexec
#[test]
fn full_precedence() {
    let config = builder::<App>()
        .unwrap()
        .cli(|c| c.args(["--config.port", "1"]))
        .env(|e| e.source(MockEnv::from_pairs([("APP__DEBUG", "true")])))
        .file(|f| f.content(
            r#"{ "config": { "port": 2, "debug": false } }"#, "app.json"))
        .build();

    let out = Driver::new(config).run().into_result().unwrap();
    assert_eq!(out.value.config.port, 1); // cli > file
    assert!(out.value.config.debug);      // env > file
}
```

## Tips

- Prefer `.into_result()` over `.unwrap()` in tests — `.unwrap()` calls
  `std::process::exit` on help/version/error, which kills the test process.
- Derive `PartialEq` on the args struct to assert the whole value at once.
- Snapshot the `Display` of `DriverError::Failed` to lock in your diagnostics;
  it includes the provenance dump, so the test documents the resolved config.
- `MockEnv::from_pairs` + `.content(...)` keep tests hermetic and parallel-safe.
