use anyhow::Result;
use clap::Parser;
use log::LevelFilter;
use regex::Regex;
use std::{
    io::Write,
    path::{Path, PathBuf},
};

use rewatch::{build, cli, cmd, lock, watcher};

fn main() -> Result<()> {
    let args = cli::Cli::parse();

    let log_level_filter = args.verbose.log_level_filter();

    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stdout)
        .init();

    let command = args.command.unwrap_or(cli::Command::Build(args.build_args));

    // The 'normal run' mode will show the 'pretty' formatted progress. But if we turn off the log
    // level, we should never show that.
    let show_progress = log_level_filter == LevelFilter::Info;

    match command.clone() {
        cli::Command::CompilerArgs {
            path,
            dev,
        } => {
            println!(
                "{}",
                build::get_compiler_args(
                    Path::new(&path),
                    *dev
                )?
            );
            std::process::exit(0);
        }
        cli::Command::Build(build_args) => {
            let _lock = get_lock(&build_args.folder);

            let filter = build_args
                .filter
                .as_ref()
                .map(|filter| Regex::new(&filter).expect("Could not parse regex"));

            match build::build(
                &filter,
                Path::new(&build_args.folder as &str),
                show_progress,
                build_args.no_timing,
                *build_args.create_sourcedirs,
                build_args.bsc_path.as_ref().map(PathBuf::from),
                *build_args.dev,
                *build_args.snapshot_output,
            ) {
                Err(e) => {
                    println!("{e}");
                    std::process::exit(1)
                }
                Ok(_) => {
                    if let Some(args_after_build) = (*build_args.after_build).clone() {
                        cmd::run(args_after_build)
                    }
                    std::process::exit(0)
                }
            };
        }
        cli::Command::Watch(watch_args) => {
            let _lock = get_lock(&watch_args.folder);

            let filter = watch_args
                .filter
                .as_ref()
                .map(|filter| Regex::new(&filter).expect("Could not parse regex"));
            watcher::start(
                &filter,
                show_progress,
                &watch_args.folder,
                (*watch_args.after_build).clone(),
                *watch_args.create_sourcedirs,
                *watch_args.dev,
                (*watch_args.bsc_path).clone(),
                *watch_args.snapshot_output,
            );

            Ok(())
        }
        cli::Command::Clean {
            folder,
            bsc_path,
            snapshot_output,
        } => {
            let _lock = get_lock(&folder);

            build::clean::clean(
                Path::new(&folder as &str),
                show_progress,
                bsc_path.as_ref().map(PathBuf::from),
                *snapshot_output,
            )
        }
        cli::Command::Legacy { legacy_args } => {
            let code = build::pass_through_legacy(legacy_args);
            std::process::exit(code);
        }
        cli::Command::Format { mut format_args } => {
            format_args.insert(0, "format".into());
            let code = build::pass_through_legacy(format_args);
            std::process::exit(code);
        }
        cli::Command::Dump { mut dump_args } => {
            dump_args.insert(0, "dump".into());
            let code = build::pass_through_legacy(dump_args);
            std::process::exit(code);
        }
    }
}

fn get_lock(folder: &str) -> lock::Lock {
    match lock::get(folder) {
        lock::Lock::Error(error) => {
            println!("Could not start ReScript build: {error}");
            std::process::exit(1);
        }
        acquired_lock => acquired_lock,
    }
}
