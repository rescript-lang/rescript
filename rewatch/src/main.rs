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

    // handle those commands early, because we don't need a lock for them
    match command.clone() {
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
        cli::Command::CompilerArgs {
            path,
            dev,
            rescript_version,
            bsc_path,
        } => {
            println!(
                "{}",
                build::get_compiler_args(
                    Path::new(&path),
                    rescript_version,
                    &bsc_path.map(PathBuf::from),
                    dev
                )?
            );
            std::process::exit(0);
        }
        _ => (),
    }

    // The 'normal run' mode will show the 'pretty' formatted progress. But if we turn off the log
    // level, we should never show that.
    let show_progress = log_level_filter == LevelFilter::Info;

    match lock::get(&args.folder) {
        lock::Lock::Error(ref e) => {
            println!("Could not start Rewatch: {e}");
            std::process::exit(1)
        }
        lock::Lock::Aquired(_) => match command {
            cli::Command::Clean {
                bsc_path,
                snapshot_output,
            } => build::clean::clean(
                Path::new(&args.folder),
                show_progress,
                &bsc_path.map(PathBuf::from),
                snapshot_output,
            ),
            cli::Command::Build(build_args) => {
                let filter = build_args
                    .filter
                    .map(|filter| Regex::new(filter.as_ref()).expect("Could not parse regex"));

                match build::build(
                    &filter,
                    Path::new(&args.folder),
                    show_progress,
                    build_args.no_timing,
                    build_args.create_sourcedirs,
                    &build_args.bsc_path.map(PathBuf::from),
                    build_args.dev,
                    build_args.snapshot_output,
                ) {
                    Err(e) => {
                        println!("{e}");
                        std::process::exit(1)
                    }
                    Ok(_) => {
                        if let Some(args_after_build) = build_args.after_build {
                            cmd::run(args_after_build)
                        }
                        std::process::exit(0)
                    }
                };
            }
            cli::Command::Watch(watch_args) => {
                let filter = watch_args
                    .filter
                    .map(|filter| Regex::new(filter.as_ref()).expect("Could not parse regex"));
                watcher::start(
                    &filter,
                    show_progress,
                    &args.folder,
                    watch_args.after_build,
                    watch_args.create_sourcedirs,
                    watch_args.dev,
                    watch_args.bsc_path,
                    watch_args.snapshot_output,
                );

                Ok(())
            }
            cli::Command::CompilerArgs { .. }
            | cli::Command::Legacy { .. }
            | cli::Command::Format { .. }
            | cli::Command::Dump { .. } => {
                unreachable!("command already handled")
            }
        },
    }
}
