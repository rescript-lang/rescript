use std::{ffi::OsString, ops::Deref};

use clap::{Args, Parser, Subcommand};
use clap_verbosity_flag::InfoLevel;

/// ReScript - Fast, Simple, Fully Typed JavaScript from the Future
#[derive(Parser, Debug)]
#[command(version)]
#[command(args_conflicts_with_subcommands = true)]
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
    pub command: Option<Command>,

    #[command(flatten)]
    pub build_args: BuildArgs,
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
    #[arg(short, long)]
    pub filter: Option<String>,
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
    /// Build development dependencies
    ///
    /// This is the flag to also compile development dependencies
    /// It's important to know that we currently do not discern between project src, and
    /// dependencies. So enabling this flag will enable building _all_ development dependencies of
    /// _all_ packages
    #[arg(long, default_value_t = false, num_args = 0..=1)]
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
}

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Build the project
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
    /// Alias to `legacy format`.
    #[command(disable_help_flag = true)]
    Format {
        #[arg(allow_hyphen_values = true, num_args = 0..)]
        format_args: Vec<OsString>,
    },
    /// Alias to `legacy dump`.
    #[command(disable_help_flag = true)]
    Dump {
        #[arg(allow_hyphen_values = true, num_args = 0..)]
        dump_args: Vec<OsString>,
    },
    /// This prints the compiler arguments. It expects the path to a rescript file (.res or .resi).
    CompilerArgs {
        /// Path to a rescript file (.res or .resi)
        #[command()]
        path: String,

        #[command(flatten)]
        dev: DevArg,
    },
    /// Use the legacy build system.
    ///
    /// After this command is encountered, the rest of the arguments are passed to the legacy build system.
    #[command(disable_help_flag = true, external_subcommand = true)]
    Legacy {
        #[arg(allow_hyphen_values = true, num_args = 0..)]
        legacy_args: Vec<OsString>,
    },
}

impl Deref for FolderArg {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.folder
    }
}

impl Deref for FilterArg {
    type Target = Option<String>;

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
