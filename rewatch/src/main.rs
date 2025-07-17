use anyhow::Result;
use clap::Parser;
use log::LevelFilter;
use std::{io::Write, path::Path};

use rewatch::{build, cli, cmd, format, lock, watcher};

fn main() -> Result<()> {
    let args = cli::Cli::parse();

    let log_level_filter = args.verbose.log_level_filter();

    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stdout)
        .init();

    let mut command = args.command.unwrap_or(cli::Command::Build(args.build_args));

    if let cli::Command::Build(build_args) = &command {
        if build_args.watch {
            log::warn!("`rescript build -w` is deprecated. Please use `rescript watch` instead.");
            command = cli::Command::Watch(build_args.clone().into());
        }
    }

    // The 'normal run' mode will show the 'pretty' formatted progress. But if we turn off the log
    // level, we should never show that.
    let show_progress = log_level_filter == LevelFilter::Info;

    match command {
        cli::Command::CompilerArgs { path, dev } => {
            println!("{}", build::get_compiler_args(Path::new(&path), *dev)?);
            std::process::exit(0);
        }
        cli::Command::Build(build_args) => {
            let _lock = get_lock(&build_args.folder);

            match build::build(
                &build_args.filter,
                Path::new(&build_args.folder as &str),
                show_progress,
                build_args.no_timing,
                *build_args.create_sourcedirs,
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

            watcher::start(
                &watch_args.filter,
                show_progress,
                &watch_args.folder,
                (*watch_args.after_build).clone(),
                *watch_args.create_sourcedirs,
                *watch_args.dev,
                *watch_args.snapshot_output,
            );

            Ok(())
        }
        cli::Command::Clean {
            folder,
            snapshot_output,
            dev,
        } => {
            let _lock = get_lock(&folder);

            build::clean::clean(
                Path::new(&folder as &str),
                show_progress,
                *snapshot_output,
                dev.dev,
            )
        }
        cli::Command::Legacy { legacy_args } => {
            let code = build::pass_through_legacy(legacy_args);
            std::process::exit(code);
        }
        cli::Command::Format {
            stdin,
            all,
            check,
            files,
        } => format::format(stdin, all, check, files),
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
