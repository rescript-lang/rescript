use anyhow::Result;
use clap::Parser;
use log::LevelFilter;
use std::{env, io::Write, path::Path};

use rescript::{build, cli, cmd, format, lock, watcher};

fn main() -> Result<()> {
    let mut args: Vec<String> = env::args().collect();
    handle_default_arg(&mut args);

    let cli = cli::Cli::parse_from(args);

    let log_level_filter = cli.verbose.log_level_filter();

    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stdout)
        .init();

    let mut command = cli.command;

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
        cli::Command::CompilerArgs { path } => {
            println!("{}", build::get_compiler_args(Path::new(&path))?);
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
                build_args.warn_error.clone(),
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
                watch_args.warn_error.clone(),
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
            check,
            files,
            dev,
        } => format::format(stdin, check, files, dev.dev),
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

fn handle_default_arg(args: &mut Vec<String>) {
    let first_arg = args.get(1);
    let global_flags = ["-h", "--help", "-V", "--version"];

    let needs_default_arg = match first_arg {
        Some(arg) if !arg.starts_with("-") || global_flags.contains(&arg.as_str()) => false,
        _ => true,
    };

    if needs_default_arg {
        args.insert(1, "build".to_string());
    }
}
