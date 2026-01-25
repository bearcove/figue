use super::*;
use crate as args;
use facet::Facet;
use facet_testhelpers::test;

macro_rules! assert_schema_snapshot {
    ($result:expr) => {{
        match $result {
            Ok(value) => insta::assert_snapshot!(facet_json::to_string_pretty(&value).unwrap()),
            Err(err) => {
                let rendered = err.to_string();
                let stripped = strip_ansi_escapes::strip(rendered.as_bytes());
                let stripped = String::from_utf8_lossy(&stripped);
                insta::assert_snapshot!(stripped);
            }
        }
    }};
}

#[derive(Facet)]
struct BasicArgs {
    /// Verbose output
    #[facet(args::named, args::short = 'v')]
    verbose: bool,
    /// Input file
    #[facet(args::positional)]
    input: String,
    /// Include list
    #[facet(args::named)]
    include: Vec<String>,
    /// Quiet count
    #[facet(args::named, args::short = 'q', args::counted)]
    quiet: u32,
    /// Subcommand
    #[facet(args::subcommand)]
    command: Option<Command>,
    /// Config
    #[facet(args::config, args::env_prefix = "APP")]
    config: Option<AppConfig>,
}

#[derive(Facet)]
#[repr(u8)]
enum Command {
    /// Build stuff
    Build(BuildArgs),
    /// Clean
    #[facet(rename = "clean-all")]
    Clean,
}

#[derive(Facet)]
struct BuildArgs {
    /// Release build
    #[facet(args::named, args::short = 'r')]
    release: bool,
}

#[derive(Facet)]
struct AppConfig {
    host: String,
    port: u16,
}

#[derive(Facet)]
struct MissingArgsAnnotation {
    foo: String,
}

#[derive(Facet)]
#[repr(u8)]
enum SubA {
    A,
}

#[derive(Facet)]
#[repr(u8)]
enum SubB {
    B,
}

#[derive(Facet)]
struct MultipleSubcommands {
    #[facet(args::subcommand)]
    a: SubA,
    #[facet(args::subcommand)]
    b: SubB,
}

#[derive(Facet)]
struct SubcommandOnNonEnum {
    #[facet(args::subcommand)]
    value: String,
}

#[derive(Facet)]
struct CountedOnNonInteger {
    #[facet(args::named, args::counted)]
    value: bool,
}

#[derive(Facet)]
struct ShortOnPositional {
    #[facet(args::positional, args::short = 'p')]
    value: String,
}

#[derive(Facet)]
struct EnvPrefixWithoutConfig {
    #[facet(args::env_prefix = "APP")]
    value: String,
}

#[derive(Facet)]
struct ConflictingLongFlags {
    #[facet(args::named, rename = "dup")]
    a: bool,
    #[facet(args::named, rename = "dup")]
    b: bool,
}

#[derive(Facet)]
struct ConflictingShortFlags {
    #[facet(args::named, args::short = 'v')]
    a: bool,
    #[facet(args::named, args::short = 'v')]
    b: bool,
}

#[derive(Facet)]
struct BadConfigField {
    #[facet(args::config)]
    config: String,
}

#[derive(Facet)]
#[repr(u8)]
enum TopLevelEnum {
    Foo,
}

#[test]
fn snapshot_schema_basic() {
    assert_schema_snapshot!(Schema::from_shape(BasicArgs::SHAPE));
}

#[test]
fn snapshot_schema_top_level_enum() {
    assert_schema_snapshot!(Schema::from_shape(TopLevelEnum::SHAPE));
}

#[test]
fn snapshot_schema_missing_args_annotation() {
    assert_schema_snapshot!(Schema::from_shape(MissingArgsAnnotation::SHAPE));
}

#[test]
fn snapshot_schema_multiple_subcommands() {
    assert_schema_snapshot!(Schema::from_shape(MultipleSubcommands::SHAPE));
}

#[test]
fn snapshot_schema_subcommand_on_non_enum() {
    assert_schema_snapshot!(Schema::from_shape(SubcommandOnNonEnum::SHAPE));
}

#[test]
fn snapshot_schema_counted_on_non_integer() {
    assert_schema_snapshot!(Schema::from_shape(CountedOnNonInteger::SHAPE));
}

#[test]
fn snapshot_schema_short_on_positional() {
    assert_schema_snapshot!(Schema::from_shape(ShortOnPositional::SHAPE));
}

#[test]
fn snapshot_schema_env_prefix_without_config() {
    assert_schema_snapshot!(Schema::from_shape(EnvPrefixWithoutConfig::SHAPE));
}

#[test]
fn snapshot_schema_conflicting_long_flags() {
    assert_schema_snapshot!(Schema::from_shape(ConflictingLongFlags::SHAPE));
}

#[test]
fn snapshot_schema_conflicting_short_flags() {
    assert_schema_snapshot!(Schema::from_shape(ConflictingShortFlags::SHAPE));
}

#[test]
fn snapshot_schema_bad_config_field() {
    assert_schema_snapshot!(Schema::from_shape(BadConfigField::SHAPE));
}

// ============================================================================
// Flatten tests
// ============================================================================

/// Common args that can be flattened into other structs.
#[derive(Facet)]
struct CommonArgs {
    #[facet(args::named, args::short = 'v')]
    verbose: bool,
    #[facet(args::named, args::short = 'q')]
    quiet: bool,
}

/// Args struct that flattens CommonArgs.
#[derive(Facet)]
struct ArgsWithFlatten {
    #[facet(args::positional)]
    input: String,
    #[facet(flatten)]
    common: CommonArgs,
}

#[test]
fn test_flatten_schema_builds() {
    let schema = Schema::from_shape(ArgsWithFlatten::SHAPE).expect("schema should build");

    // The flattened args should appear at top level
    let args = schema.args();
    assert!(
        args.args.contains_key("verbose"),
        "verbose should be in args"
    );
    assert!(args.args.contains_key("quiet"), "quiet should be in args");
    assert!(args.args.contains_key("input"), "input should be in args");
}

#[test]
fn test_flatten_target_path() {
    let schema = Schema::from_shape(ArgsWithFlatten::SHAPE).expect("schema should build");
    let args = schema.args();

    // input is not flattened, so target_path should be ["input"]
    let input_arg = args.args.get("input").expect("input should exist");
    assert_eq!(input_arg.target_path, vec!["input".to_string()]);

    // verbose is flattened from common, so target_path should be ["common", "verbose"]
    let verbose_arg = args.args.get("verbose").expect("verbose should exist");
    assert_eq!(
        verbose_arg.target_path,
        vec!["common".to_string(), "verbose".to_string()]
    );

    // quiet is flattened from common, so target_path should be ["common", "quiet"]
    let quiet_arg = args.args.get("quiet").expect("quiet should exist");
    assert_eq!(
        quiet_arg.target_path,
        vec!["common".to_string(), "quiet".to_string()]
    );
}

/// Nested flattening test structs
#[derive(Facet)]
struct OutputArgs {
    #[facet(args::named, args::short = 'f')]
    format: Option<String>,
}

#[derive(Facet)]
struct ExtendedCommonArgs {
    #[facet(flatten)]
    common: CommonArgs,
    #[facet(flatten)]
    output: OutputArgs,
}

#[derive(Facet)]
struct ArgsWithNestedFlatten {
    #[facet(args::positional)]
    input: String,
    #[facet(flatten)]
    extended: ExtendedCommonArgs,
}

#[test]
fn test_flatten_nested_target_path() {
    let schema = Schema::from_shape(ArgsWithNestedFlatten::SHAPE).expect("schema should build");
    let args = schema.args();

    // input is not flattened
    let input_arg = args.args.get("input").expect("input should exist");
    assert_eq!(input_arg.target_path, vec!["input".to_string()]);

    // verbose is nested: extended.common.verbose
    let verbose_arg = args.args.get("verbose").expect("verbose should exist");
    assert_eq!(
        verbose_arg.target_path,
        vec![
            "extended".to_string(),
            "common".to_string(),
            "verbose".to_string()
        ]
    );

    // format is nested: extended.output.format
    let format_arg = args.args.get("format").expect("format should exist");
    assert_eq!(
        format_arg.target_path,
        vec![
            "extended".to_string(),
            "output".to_string(),
            "format".to_string()
        ]
    );
}

/// Test conflicting flags from flatten
#[derive(Facet)]
struct ConflictingFlattenArgs {
    #[facet(args::named, args::short = 'v')]
    version: bool,
    #[facet(flatten)]
    common: CommonArgs, // CommonArgs also has -v for verbose
}

#[test]
fn test_flatten_conflict_detected() {
    let result = Schema::from_shape(ConflictingFlattenArgs::SHAPE);
    assert!(result.is_err(), "should detect duplicate -v flag");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("duplicate") || err.contains("-v"),
        "error should mention duplicate: {}",
        err
    );
}
