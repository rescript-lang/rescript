use anyhow::Result;
use clap::{Parser, error::ErrorKind};
use log::LevelFilter;
use std::{env, io::Write, path::Path};

use rescript::{build, cli, cmd, format, lock, watcher};

fn main() -> Result<()> {
    let raw_args: Vec<String> = env::args().collect();
    let cli = parse_cli(raw_args).unwrap_or_else(|err| err.exit());

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

fn parse_cli(raw_args: Vec<String>) -> Result<cli::Cli, clap::Error> {
    match cli::Cli::try_parse_from(&raw_args) {
        Ok(cli) => Ok(cli),
        Err(err) => {
            if should_default_to_build(&err, &raw_args) {
                let mut fallback_args = raw_args.clone();
                let insert_at = index_after_global_flags(&fallback_args);
                fallback_args.insert(insert_at, "build".into());

                match cli::Cli::try_parse_from(&fallback_args) {
                    Ok(cli) => Ok(cli),
                    Err(fallback_err) => Err(fallback_err),
                }
            } else {
                Err(err)
            }
        }
    }
}

fn should_default_to_build(err: &clap::Error, args: &[String]) -> bool {
    match err.kind() {
        ErrorKind::MissingSubcommand | ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => true,
        ErrorKind::UnknownArgument | ErrorKind::InvalidSubcommand => {
            args.iter().skip(1).any(|arg| !is_global_flag(arg))
        }
        _ => false,
    }
}

fn index_after_global_flags(args: &[String]) -> usize {
    let mut idx = 1;
    while let Some(arg) = args.get(idx) {
        if is_global_flag(arg) {
            idx += 1;
        } else {
            break;
        }
    }
    idx.min(args.len())
}

fn is_global_flag(arg: &str) -> bool {
    matches!(
        arg,
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
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(args: &[&str]) -> Result<cli::Cli, clap::Error> {
        parse_cli(args.iter().map(|arg| arg.to_string()).collect())
    }

    #[test]
    fn defaults_to_build_without_args() {
        let cli = parse(&["rescript"]).expect("expected default build command");

        match cli.command {
            cli::Command::Build(build_args) => assert_eq!(build_args.folder.folder, "."),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn defaults_to_build_with_folder_shortcut() {
        let cli = parse(&["rescript", "someFolder"]).expect("expected build command");

        match cli.command {
            cli::Command::Build(build_args) => assert_eq!(build_args.folder.folder, "someFolder"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn respects_global_flag_before_subcommand() {
        let cli = parse(&["rescript", "-v", "watch"]).expect("expected watch command");

        assert!(matches!(cli.command, cli::Command::Watch(_)));
    }

    #[test]
    fn help_flag_does_not_default_to_build() {
        let err = parse(&["rescript", "--help"]).expect_err("expected clap help error");
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
    }

    #[test]
    fn version_flag_does_not_default_to_build() {
        let err = parse(&["rescript", "--version"]).expect_err("expected clap version error");
        assert_eq!(err.kind(), ErrorKind::DisplayVersion);
    }
}
