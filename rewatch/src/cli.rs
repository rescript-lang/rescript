// We currently use https://docs.rs/clap/latest/clap/ v4 for command line parsing.
// However, it does not fully fit our use case as it does not support default commands,
// but we want to default to the "build" command if no other command is specified.
//
// Various workarounds exist, but each with its own drawbacks.
// The workaround implemented here (injecting "build" into the args at the right place
// and then parsing again if no other command matches at the first parse attempt)
// avoids flattening all build command options into the root help, but requires careful
// handling of edge cases regarding global flags.
// Correctness is ensured by a comprehensive test suite.
//
// However, we may want to revisit the decision to use clap after the v12 release.

use std::{env, ffi::OsString, ops::Deref};

use clap::{Args, CommandFactory, Parser, Subcommand, error::ErrorKind};
use clap_verbosity_flag::InfoLevel;
use regex::Regex;

fn parse_regex(s: &str) -> Result<Regex, regex::Error> {
    Regex::new(s)
}

use clap::ValueEnum;

#[derive(Debug, Clone, ValueEnum)]
pub enum FileExtension {
    #[value(name = ".res")]
    Res,
    #[value(name = ".resi")]
    Resi,
}

/// ReScript - Fast, Simple, Fully Typed JavaScript from the Future
#[derive(Parser, Debug)]
// The shipped binary is `rescript.exe` everywhere, but users invoke it as `rescript` (e.g.
// via `npm run rescript`). Without forcing `bin_name`, clap would print `rescript.exe` in help,
// which leaks the packaging detail into the CLI UX.
#[command(name = "rescript", bin_name = "rescript")]
#[command(version)]
#[command(after_help = "[1m[1m[4mNotes:[0m
  - If no command is provided, the [1mbuild[0m command is run by default. See `rescript help build` for more information.
  - For the legacy (pre-v12) build system, run `rescript-legacy` instead.")]
pub struct Cli {
    /// Verbosity:
    /// -v -> Debug
    /// -vv -> Trace
    /// -q -> Warn
    /// -qq -> Error
    /// -qqq -> Off.
    /// Default (/ no argument given): 'info'
    #[command(flatten)]
    pub verbose: clap_verbosity_flag::Verbosity<InfoLevel>,

    /// The command to run. If not provided it will default to build.
    #[command(subcommand)]
    pub command: Command,
}

/// Parse argv from the current process while treating `build` as the implicit default subcommand
/// when clap indicates the user omitted one. This keeps the top-level help compact while still
/// supporting bare `rescript …` invocations that expect to run the build.
pub fn parse_with_default() -> Result<Cli, clap::Error> {
    // Use `args_os` so non-UTF bytes still reach clap for proper error reporting on platforms that
    // allow arbitrary argv content.
    let raw_args: Vec<OsString> = env::args_os().collect();
    parse_with_default_from(&raw_args)
}

/// Parse the provided argv while applying the implicit `build` defaulting rules.
pub fn parse_with_default_from(raw_args: &[OsString]) -> Result<Cli, clap::Error> {
    match Cli::try_parse_from(raw_args) {
        Ok(cli) => Ok(cli),
        Err(err) => {
            if should_default_to_build(&err, raw_args) {
                let fallback_args = build_default_args(raw_args);
                Cli::try_parse_from(&fallback_args)
            } else {
                Err(err)
            }
        }
    }
}

fn should_default_to_build(err: &clap::Error, args: &[OsString]) -> bool {
    match err.kind() {
        ErrorKind::MissingSubcommand
        | ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand
        | ErrorKind::UnknownArgument
        | ErrorKind::InvalidSubcommand => {
            let first_non_global = first_non_global_arg(args);
            match first_non_global {
                Some(arg) => !is_known_subcommand(arg),
                None => true,
            }
        }
        _ => false,
    }
}

fn is_global_flag(arg: &OsString) -> bool {
    matches!(
        arg.to_str(),
        Some(
            "-v" | "-vv"
                | "-vvv"
                | "-vvvv"
                | "-q"
                | "-qq"
                | "-qqq"
                | "-qqqq"
                | "--verbose"
                | "--quiet"
                | "-h"
                | "--help"
                | "-V"
                | "--version"
        )
    )
}

fn first_non_global_arg(args: &[OsString]) -> Option<&OsString> {
    args.iter().skip(1).find(|arg| !is_global_flag(arg))
}

fn is_known_subcommand(arg: &OsString) -> bool {
    let Some(arg_str) = arg.to_str() else {
        return false;
    };

    Cli::command().get_subcommands().any(|subcommand| {
        subcommand.get_name() == arg_str || subcommand.get_all_aliases().any(|alias| alias == arg_str)
    })
}

fn build_default_args(raw_args: &[OsString]) -> Vec<OsString> {
    // Preserve clap's global flag handling semantics by keeping `-v/-q/-h/-V` in front of the
    // inserted `build` token while leaving the rest of the argv untouched. This mirrors clap's own
    // precedence rules so the second parse sees an argument layout it would have produced if the
    // user had typed `rescript build …` directly.
    let mut result = Vec::with_capacity(raw_args.len() + 1);
    if raw_args.is_empty() {
        return vec![OsString::from("build")];
    }

    let mut globals = Vec::new();
    let mut others = Vec::new();
    let mut saw_double_dash = false;

    for arg in raw_args.iter().skip(1) {
        if !saw_double_dash {
            if arg == "--" {
                saw_double_dash = true;
                others.push(arg.clone());
                continue;
            }

            if is_global_flag(arg) {
                globals.push(arg.clone());
                continue;
            }
        }

        others.push(arg.clone());
    }

    result.push(raw_args[0].clone());
    result.extend(globals);
    result.push(OsString::from("build"));
    result.extend(others);
    result
}

#[derive(Args, Debug, Clone)]
pub struct FolderArg {
    /// The relative path to where the main rescript.json resides. IE - the root of your project.
    #[arg(default_value = ".")]
    pub folder: String,
}

#[derive(Args, Debug, Clone)]
pub struct FilterArg {
    /// Filter files by regex
    ///
    /// Filter allows for a regex to be supplied which will filter the files to be compiled. For
    /// instance, to filter out test files for compilation while doing feature work.
    #[arg(short, long, value_parser = parse_regex)]
    pub filter: Option<Regex>,
}

#[derive(Args, Debug, Clone)]
pub struct AfterBuildArg {
    /// Action after build
    ///
    /// This allows one to pass an additional command to the watcher, which allows it to run when
    /// finished. For instance, to play a sound when done compiling, or to run a test suite.
    /// NOTE - You may need to add '--color=always' to your subcommand in case you want to output
    /// color as well
    #[arg(short, long)]
    pub after_build: Option<String>,
}

#[derive(Args, Debug, Clone, Copy)]
pub struct CreateSourceDirsArg {
    /// Create source_dirs.json
    ///
    /// This creates a source_dirs.json file at the root of the monorepo, which is needed when you
    /// want to use Reanalyze
    #[arg(short, long, default_value_t = false, num_args = 0..=1)]
    pub create_sourcedirs: bool,
}

#[derive(Args, Debug, Clone, Copy)]
pub struct DevArg {
    /// Deprecated: Build development dependencies
    ///
    /// This is the flag no longer does anything and will be removed in future versions.
    #[arg(long, default_value_t = false, num_args = 0..=1, hide = true)]
    pub dev: bool,
}

#[derive(Args, Debug, Clone, Copy)]
pub struct SnapshotOutputArg {
    /// simple output for snapshot testing
    #[arg(short, long, default_value = "false", num_args = 0..=1)]
    pub snapshot_output: bool,
}

#[derive(Args, Debug, Clone)]
pub struct BuildArgs {
    #[command(flatten)]
    pub folder: FolderArg,

    #[command(flatten)]
    pub filter: FilterArg,

    #[command(flatten)]
    pub after_build: AfterBuildArg,

    #[command(flatten)]
    pub create_sourcedirs: CreateSourceDirsArg,

    #[command(flatten)]
    pub dev: DevArg,

    /// Disable timing on the output
    #[arg(short, long, default_value_t = false, num_args = 0..=1)]
    pub no_timing: bool,

    #[command(flatten)]
    pub snapshot_output: SnapshotOutputArg,

    /// Watch mode (deprecated, use `rescript watch` instead)
    #[arg(short, default_value_t = false, num_args = 0..=1)]
    pub watch: bool,

    /// Warning numbers and whether to turn them into errors
    ///
    /// This flag overrides any warning configuration in rescript.json.
    /// Example: --warn-error "+3+8+11+12+26+27+31+32+33+34+35+39+44+45+110"
    /// This follows the same precedence behavior as the legacy bsb build system.
    #[arg(long)]
    pub warn_error: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::error::ErrorKind;
    use log::LevelFilter;

    fn parse(args: &[&str]) -> Result<Cli, clap::Error> {
        let raw_args: Vec<OsString> = args.iter().map(OsString::from).collect();
        parse_with_default_from(&raw_args)
    }

    // Default command behaviour.
    #[test]
    fn no_subcommand_defaults_to_build() {
        let cli = parse(&["rescript"]).expect("expected default build command");
        assert!(matches!(cli.command, Command::Build(_)));
    }

    #[test]
    fn defaults_to_build_with_folder_shortcut() {
        let cli = parse(&["rescript", "someFolder"]).expect("expected build command");

        match cli.command {
            Command::Build(build_args) => assert_eq!(build_args.folder.folder, "someFolder"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn trailing_global_flag_is_treated_as_global() {
        let cli = parse(&["rescript", "my-project", "-v"]).expect("expected build command");

        assert_eq!(cli.verbose.log_level_filter(), LevelFilter::Debug);
        match cli.command {
            Command::Build(build_args) => assert_eq!(build_args.folder.folder, "my-project"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn double_dash_keeps_following_args_positional() {
        let cli = parse(&["rescript", "--", "-v"]).expect("expected build command");

        assert_eq!(cli.verbose.log_level_filter(), LevelFilter::Info);
        match cli.command {
            Command::Build(build_args) => assert_eq!(build_args.folder.folder, "-v"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn unknown_subcommand_help_uses_global_help() {
        let err = parse(&["rescript", "xxx", "--help"]).expect_err("expected global help");
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
    }

    // Build command specifics.
    #[test]
    fn build_help_shows_subcommand_help() {
        let err = parse(&["rescript", "build", "--help"]).expect_err("expected subcommand help");
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
        let rendered = err.to_string();
        assert!(
            rendered.contains("Usage: rescript build"),
            "unexpected help: {rendered:?}"
        );
        assert!(!rendered.contains("Usage: rescript [OPTIONS] <COMMAND>"));
    }

    #[test]
    fn build_allows_global_verbose_flag() {
        let cli = parse(&["rescript", "build", "-v"]).expect("expected build command");
        assert_eq!(cli.verbose.log_level_filter(), LevelFilter::Debug);
        assert!(matches!(cli.command, Command::Build(_)));
    }

    #[test]
    fn build_option_is_parsed_normally() {
        let cli = parse(&["rescript", "build", "--no-timing"]).expect("expected build command");

        match cli.command {
            Command::Build(build_args) => assert!(build_args.no_timing),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    // Subcommand flag handling.
    #[test]
    fn respects_global_flag_before_subcommand() {
        let cli = parse(&["rescript", "-v", "watch"]).expect("expected watch command");

        assert!(matches!(cli.command, Command::Watch(_)));
    }

    #[test]
    fn invalid_option_for_subcommand_does_not_fallback() {
        let err = parse(&["rescript", "watch", "--no-timing"]).expect_err("expected watch parse failure");
        assert_eq!(err.kind(), ErrorKind::UnknownArgument);
    }

    // Version/help flag handling.
    #[test]
    fn version_flag_before_subcommand_displays_version() {
        let err = parse(&["rescript", "-V", "build"]).expect_err("expected version display");
        assert_eq!(err.kind(), ErrorKind::DisplayVersion);
    }

    #[test]
    fn version_flag_after_subcommand_is_rejected() {
        let err = parse(&["rescript", "build", "-V"]).expect_err("expected unexpected argument");
        assert_eq!(err.kind(), ErrorKind::UnknownArgument);
    }

    #[test]
    fn global_help_flag_shows_help() {
        let err = parse(&["rescript", "--help"]).expect_err("expected clap help error");
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
        let rendered = err.to_string();
        assert!(rendered.contains("Usage: rescript [OPTIONS] <COMMAND>"));
    }

    #[test]
    fn global_version_flag_shows_version() {
        let err = parse(&["rescript", "--version"]).expect_err("expected clap version error");
        assert_eq!(err.kind(), ErrorKind::DisplayVersion);
    }

    #[cfg(unix)]
    #[test]
    fn non_utf_argument_returns_error() {
        use std::os::unix::ffi::OsStringExt;

        let args = vec![OsString::from("rescript"), OsString::from_vec(vec![0xff])];
        let err = parse_with_default_from(&args).expect_err("expected clap to report invalid utf8");
        assert_eq!(err.kind(), ErrorKind::InvalidUtf8);
    }
}

#[derive(Args, Clone, Debug)]
pub struct WatchArgs {
    #[command(flatten)]
    pub folder: FolderArg,

    #[command(flatten)]
    pub filter: FilterArg,

    #[command(flatten)]
    pub after_build: AfterBuildArg,

    #[command(flatten)]
    pub create_sourcedirs: CreateSourceDirsArg,

    #[command(flatten)]
    pub dev: DevArg,

    #[command(flatten)]
    pub snapshot_output: SnapshotOutputArg,

    /// Warning numbers and whether to turn them into errors
    ///
    /// This flag overrides any warning configuration in rescript.json.
    /// Example: --warn-error "+3+8+11+12+26+27+31+32+33+34+35+39+44+45+110"
    /// This follows the same precedence behavior as the legacy bsb build system.
    #[arg(long)]
    pub warn_error: Option<String>,
}

impl From<BuildArgs> for WatchArgs {
    fn from(build_args: BuildArgs) -> Self {
        Self {
            folder: build_args.folder,
            filter: build_args.filter,
            after_build: build_args.after_build,
            create_sourcedirs: build_args.create_sourcedirs,
            dev: build_args.dev,
            snapshot_output: build_args.snapshot_output,
            warn_error: build_args.warn_error,
        }
    }
}

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Build the project (default command)
    Build(BuildArgs),
    /// Build, then start a watcher
    Watch(WatchArgs),
    /// Clean the build artifacts
    Clean {
        #[command(flatten)]
        folder: FolderArg,

        #[command(flatten)]
        snapshot_output: SnapshotOutputArg,

        #[command(flatten)]
        dev: DevArg,
    },
    /// Formats ReScript files.
    Format {
        /// Check formatting status without applying changes.
        #[arg(short, long)]
        check: bool,

        /// Read the code from stdin and print the formatted code to stdout.
        #[arg(
            short,
            long,
            group = "format_input_mode",
            value_enum,
            conflicts_with = "check"
        )]
        stdin: Option<FileExtension>,

        /// Files to format. If no files are provided, all files are formatted.
        #[arg(group = "format_input_mode")]
        files: Vec<String>,

        #[command(flatten)]
        dev: DevArg,
    },
    /// This prints the compiler arguments. It expects the path to a rescript file (.res or .resi).
    CompilerArgs {
        /// Path to a rescript file (.res or .resi)
        #[command()]
        path: String,
    },
    /// Generate JSON/OpenAPI schemas for Rewatch protocols
    Schema {
        /// Which schema to generate
        #[arg(value_enum)]
        what: SchemaWhat,

        /// Optional output directory; if omitted, prints to stdout
        #[arg(long)]
        output_dir: Option<String>,

        /// Also emit an OpenAPI 3.1 document with components
        #[arg(long, default_value_t = false, num_args = 0..=1)]
        openapi: bool,
    },
}

impl Deref for FolderArg {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.folder
    }
}

impl Deref for FilterArg {
    type Target = Option<Regex>;

    fn deref(&self) -> &Self::Target {
        &self.filter
    }
}

impl Deref for AfterBuildArg {
    type Target = Option<String>;

    fn deref(&self) -> &Self::Target {
        &self.after_build
    }
}

impl Deref for CreateSourceDirsArg {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.create_sourcedirs
    }
}

impl Deref for DevArg {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.dev
    }
}

impl Deref for SnapshotOutputArg {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.snapshot_output
    }
}

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum SchemaWhat {
    #[value(name = "embeds")]
    Embeds,
}
