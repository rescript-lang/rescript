use anyhow::Result;
use clap::{Parser, ValueEnum};
use clap_verbosity_flag::InfoLevel;
use log::LevelFilter;
use regex::Regex;
use std::io::Write;

use rewatch::{build, cmd, lock, watcher};

#[derive(Debug, Clone, ValueEnum)]
enum Command {
    /// Build using Rewatch
    Build,
    /// Build, then start a watcher
    Watch,
    /// Clean the build artifacts
    Clean,
}

/// Rewatch is an alternative build system for the Rescript Compiler bsb (which uses Ninja internally). It strives
/// to deliver consistent and faster builds in monorepo setups with multiple packages, where the
/// default build system fails to pick up changed interfaces across multiple packages.
#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    #[arg(value_enum)]
    command: Option<Command>,

    /// The relative path to where the main rescript.json resides. IE - the root of your project.
    folder: Option<String>,

    /// Filter allows for a regex to be supplied which will filter the files to be compiled. For
    /// instance, to filter out test files for compilation while doing feature work.
    #[arg(short, long)]
    filter: Option<String>,

    /// This allows one to pass an additional command to the watcher, which allows it to run when
    /// finished. For instance, to play a sound when done compiling, or to run a test suite.
    /// NOTE - You may need to add '--color=always' to your subcommand in case you want to output
    /// colour as well
    #[arg(short, long)]
    after_build: Option<String>,

    #[arg(short, long)]
    no_timing: Option<bool>,

    /// Verbosity:
    /// -v -> Debug
    /// -vv -> Trace
    /// -q -> Warn
    /// -qq -> Error 
    /// -qqq -> Off. 
    /// Default (/ no argument given): 'info'
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity<InfoLevel>,

    /// This creates a source_dirs.json file at the root of the monorepo, which is needed when you
    /// want to use Reanalyze
    #[arg(short, long)]
    create_sourcedirs: Option<bool>,

    /// This prints the compiler arguments. It expects the path to a rescript.json file.
    /// This also requires --bsc-path and --rescript-version to be present
    #[arg(long)]
    compiler_args: Option<String>,

    /// To be used in conjunction with compiler_args
    #[arg(long)]
    rescript_version: Option<String>,

    /// A custom path to bsc
    #[arg(long)]
    bsc_path: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let log_level_filter = args.verbose.log_level_filter();

    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stdout)
        .init();

    let command = args.command.unwrap_or(Command::Build);
    let folder = args.folder.unwrap_or(".".to_string());
    let filter = args
        .filter
        .map(|filter| Regex::new(filter.as_ref()).expect("Could not parse regex"));

    match args.compiler_args {
        None => (),
        Some(path) => {
            println!(
                "{}",
                build::get_compiler_args(&path, args.rescript_version, args.bsc_path)?
            );
            std::process::exit(0);
        }
    }

    // The 'normal run' mode will show the 'pretty' formatted progress. But if we turn off the log
    // level, we should never show that.
    let show_progress = log_level_filter == LevelFilter::Info;

    match lock::get(&folder) {
        lock::Lock::Error(ref e) => {
            log::error!("Could not start Rewatch: {e}");
            std::process::exit(1)
        }
        lock::Lock::Aquired(_) => match command {
            Command::Clean => build::clean::clean(&folder, show_progress, args.bsc_path),
            Command::Build => {
                match build::build(
                    &filter,
                    &folder,
                    show_progress,
                    args.no_timing.unwrap_or(false),
                    args.create_sourcedirs.unwrap_or(false),
                    args.bsc_path,
                ) {
                    Err(e) => {
                        log::error!("{e}");
                        std::process::exit(1)
                    }
                    Ok(_) => {
                        if let Some(args_after_build) = args.after_build {
                            cmd::run(args_after_build)
                        }
                        std::process::exit(0)
                    }
                };
            }
            Command::Watch => {
                watcher::start(
                    &filter,
                    show_progress,
                    &folder,
                    args.after_build,
                    args.create_sourcedirs.unwrap_or(false),
                );

                Ok(())
            }
        },
    }
}
