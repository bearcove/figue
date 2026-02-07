# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.0](https://github.com/bearcove/figue/compare/figue-v0.1.0...figue-v2.0.0) - 2026-02-07

### Added

- *(cli)* recognize --<config-field-name> <path> to specify config file

### Fixed

- recurse into enum variants during type coercion (fixes #57) ([#58](https://github.com/bearcove/figue/pull/58))
- make shell completions work with inline sourcing
- default CLI args to std::env::args() when not explicitly set
- *(figue)* fill boolean defaults for subcommand args
- *(figue)* use tree-drawing characters in config dump to survive whitespace stripping
- *(figue)* report unknown config keys with full dump instead of early error

### Other

- Less color, remove redundant functions ([#68](https://github.com/bearcove/figue/pull/68))
- Support NO_COLOR, disable coloring for insta snapshots ([#67](https://github.com/bearcove/figue/pull/67))
- Minor cleanups, fix incorrect path to the license file ([#66](https://github.com/bearcove/figue/pull/66))
- Support `args::label` for showing better descriptions for types in help text  ([#64](https://github.com/bearcove/figue/pull/64))
- Improve subcommand validation error messages ([#62](https://github.com/bearcove/figue/pull/62))
- clarify why DriverOutcome exists and how to use it
- Add missing README
- Update for facet FormatDeserializer API change
- Update for facet Span type change (u32 fields)
- Upgrade to latest facet-format interface
- Port to new facet-format interface
- Bump fill_defaults down from debug to trace
- Add comprehensive documentation with working doc tests ([#55](https://github.com/bearcove/figue/pull/55))
- Replace ProbeStream with save/restore ([#54](https://github.com/bearcove/figue/pull/54))
- Address test coverage gaps and cleanup ([#25](https://github.com/bearcove/figue/pull/25)) ([#52](https://github.com/bearcove/figue/pull/52))
- Add 'did you mean...?' suggestions for typos using strsim ([#36](https://github.com/bearcove/figue/pull/36)) ([#51](https://github.com/bearcove/figue/pull/51))
- Add Ariadne error reporting for deserialization errors from all sources ([#50](https://github.com/bearcove/figue/pull/50))
- Add schema-level field defaults from #[facet(default)] attributes ([#49](https://github.com/bearcove/figue/pull/49))
- Migrate CLI config overrides to ValueBuilder ([#48](https://github.com/bearcove/figue/pull/48))
- Migrate file.rs parser to use ValueBuilder ([#47](https://github.com/bearcove/figue/pull/47))
- Convert to Cargo workspace structure ([#46](https://github.com/bearcove/figue/pull/46))
