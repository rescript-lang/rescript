use anyhow::Result;
use clap::{CommandFactory, Parser, error::ErrorKind};
use log::LevelFilter;
use std::{env, ffi::OsString, io::Write, path::Path};

use rescript::{build, cli, cmd, format, lock, watcher};

fn main() -> Result<()> {
    // Use `args_os` so non-UTF bytes still reach clap for proper error reporting on platforms that
    // allow arbitrary argv content.
    let raw_args: Vec<OsString> = env::args_os().collect();
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

fn parse_cli(raw_args: Vec<OsString>) -> Result<cli::Cli, clap::Error> {
    match cli::Cli::try_parse_from(&raw_args) {
        Ok(cli) => Ok(cli),
        Err(err) => {
            if should_default_to_build(&err, &raw_args) {
                let fallback_args = build_default_args(&raw_args);

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

    cli::Cli::command().get_subcommands().any(|subcommand| {
        subcommand.get_name() == arg_str || subcommand.get_all_aliases().any(|alias| alias == arg_str)
    })
}

fn build_default_args(raw_args: &[OsString]) -> Vec<OsString> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use log::LevelFilter;
    use std::ffi::OsString;

    fn parse(args: &[&str]) -> Result<cli::Cli, clap::Error> {
        parse_cli(args.iter().map(OsString::from).collect())
    }

    // Default command behaviour.
    #[test]
    fn no_subcommand_defaults_to_build() {
        let cli = parse(&["rescript"]).expect("expected default build command");
        assert!(matches!(cli.command, cli::Command::Build(_)));
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
    fn trailing_global_flag_is_treated_as_global() {
        let cli = parse(&["rescript", "my-project", "-v"]).expect("expected build command");

        assert_eq!(cli.verbose.log_level_filter(), LevelFilter::Debug);
        match cli.command {
            cli::Command::Build(build_args) => assert_eq!(build_args.folder.folder, "my-project"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn double_dash_keeps_following_args_positional() {
        let cli = parse(&["rescript", "--", "-v"]).expect("expected build command");

        assert_eq!(cli.verbose.log_level_filter(), LevelFilter::Info);
        match cli.command {
            cli::Command::Build(build_args) => assert_eq!(build_args.folder.folder, "-v"),
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
        assert!(matches!(cli.command, cli::Command::Build(_)));
    }

    #[test]
    fn build_option_is_parsed_normally() {
        let cli = parse(&["rescript", "build", "--no-timing"]).expect("expected build command");

        match cli.command {
            cli::Command::Build(build_args) => assert!(build_args.no_timing),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    // Subcommand flag handling.
    #[test]
    fn respects_global_flag_before_subcommand() {
        let cli = parse(&["rescript", "-v", "watch"]).expect("expected watch command");

        assert!(matches!(cli.command, cli::Command::Watch(_)));
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
        let err = parse_cli(args).expect_err("expected clap to report invalid utf8");
        assert_eq!(err.kind(), ErrorKind::InvalidUtf8);
    }
}
