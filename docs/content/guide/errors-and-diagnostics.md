+++
title = "Errors & Diagnostics"
weight = 8
insert_anchor_links = "heading"
+++

A config library is only as good as its error messages. figue renders failures
with [ariadne](https://docs.rs/ariadne): it underlines the offending argument,
environment variable, or config-file line, and suggests a fix. This page shows
what users see and how to control it.

## Exit codes at a glance

| Outcome | `.unwrap()` behavior | Exit |
|---|---|---|
| Success | returns `T` (warnings → stderr) | continues |
| `--help` | help text → stdout | `0` |
| `--version` | `name version` → stdout | `0` |
| `--completions <shell>` | script → stdout | `0` |
| `--html-help` | opens browser | `0` |
| `--export-jsonschemas <dir>` | writes files | `0` |
| Parse / missing / type error | diagnostic → stderr | `1` |

So `myapp --help | less` works, and `myapp 2>err.log` captures only real
errors. Help is *success*, not failure.

## Unknown flag, with a suggestion

```text
Error: unknown flag: --c0ncurrency. Did you mean '--concurrency'?
   ╭─[ <cli>:1:1 ]
   │
 1 │ --c0ncurrency 4
   │ ──────┬──────
   │       ╰──────── unknown flag: --c0ncurrency. Did you mean '--concurrency'?
───╯
```

Suggestions use Jaro–Winkler similarity, so transpositions and single-character
slips are caught. The same machinery suggests subcommands
(`buidl` → `build`) and config keys (`prot` → `port`).

## A wrong type, pointed at the source

The span points back into wherever the bad value actually came from. From a
config file:

```text
Error: failed to parse "not_a_number" as u16 at config.port
   ╭─[ app.json:5:9 ]
   │
 5 │   "port": "not_a_number",
   │           ──────┬──────
   │                 ╰──────── failed to parse "not_a_number" as u16
───╯
```

From the environment, figue renders a synthetic source so the underline still
makes sense:

```text
Error: failed to parse "not_a_number" as u16 at port
   ╭─[ <env>:1:12 ]
   │
 1 │ APP__PORT="not_a_number"
   │            ──────┬──────
   │                  ╰────── failed to parse "not_a_number" as u16
───╯
```

## Missing required fields → the config dump

When something required is absent, figue prints the full provenance dump so the
user sees *everything that was understood* and exactly what's missing:

```text
Error: Missing required fields:

Sources:
├─ file:
│  └─ (picked) app.json  (via --config)
├─ env $MYAPP__*
├─ cli --settings.*
└─ defaults

debug......... true..... --debug
host.......... 0.0.0.0.. app.json:3
port.......... 4000..... $MYAPP__PORT
name.......... ......... ⨯ MISSING

Missing:
  name <String> (--settings.name or $MYAPP__NAME)

Run with --help for usage information.
```

`⨯ MISSING` is impossible to miss, and the `Missing:` block tells the user the
two concrete ways to provide it. `🔒 [REDACTED (N bytes)]` appears for any
field marked `#[facet(sensitive)]`, so secrets never leak into logs.

> The dump only appears on error paths. There is no `--dump` flag. If you want
> the dump for a *successful* parse during debugging, trigger an error
> deliberately, or inspect `DriverOutput` from `.into_result()`. The only
> related knob is the env var `FACET_ARGS_BLAST_IT=1`, which disables value
> truncation in the dump.

## Handling errors yourself

`.unwrap()` does the right thing for almost every CLI. When you need control
(custom logging, a TUI, structured output), use `.into_result()`:

```rust,noexec
use figue::{Driver, DriverError};

let outcome = Driver::new(config).run();
match outcome.into_result() {
    Ok(output) => {
        let args = output.get(); // returns T, prints warnings to stderr
        run(args);
    }
    // Help / Version / Completions are "errors" that mean success:
    Err(e) if e.is_success() => {
        print!("{e}");
        std::process::exit(0);
    }
    Err(e) => {
        eprint!("{e}");
        std::process::exit(e.exit_code());
    }
}
```

`DriverError::is_help()` is true only for `Help` (not `HtmlHelp`).
`is_success()` is true for every variant that should exit `0`. `exit_code()`
gives you the right status for any variant. The complete variant list is in the
[Errors reference](@/reference/errors.md).

## Color

Output is colored when the terminal supports it. figue honors `NO_COLOR` and
`FORCE_COLOR`, and disables color automatically under `insta` so snapshot tests
are stable. You don't configure anything.

## Schema errors are different

The errors above are *runtime* (the user typed something wrong). A **schema
error** means *your code* is wrong — e.g. an enum as the root type, or
`args::env_prefix` without `args::config`. These surface from
`builder::<T>()` / `from_slice` before any parsing, also rendered with ariadne,
pointing at the offending field. They're catalogued in the
[Errors reference](@/reference/errors.md#schemaerror); you'll see them once,
while wiring up types, and never again.

Next: [Help & Completions](@/guide/help-and-completions.md).
