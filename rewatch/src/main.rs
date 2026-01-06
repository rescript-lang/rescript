use anyhow::Result;
use console::Term;
use log::LevelFilter;
use std::{io::Write, path::Path};

use rescript::{build, cli, cmd, format, lock, watcher};

fn main() -> Result<()> {
    let cli = cli::parse_with_default().unwrap_or_else(|err| err.exit());

    let log_level_filter = cli.verbose.log_level_filter();

    let stdout_logger = env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stdout)
        .build();

    let stderr_logger = env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}:\n{}", record.level(), record.args()))
        .filter_level(log_level_filter)
        .target(env_logger::fmt::Target::Stderr)
        .build();

    log::set_max_level(log_level_filter);
    log::set_boxed_logger(Box::new(SplitLogger {
        stdout: stdout_logger,
        stderr: stderr_logger,
    }))
    .expect("Failed to initialize logger");

    let mut command = cli.command;

    if let cli::Command::Build(build_args) = &command
        && build_args.watch
    {
        log::warn!("`rescript build -w` is deprecated. Please use `rescript watch` instead.");
        command = cli::Command::Watch(build_args.clone().into());
    }

    let is_tty: bool = Term::stdout().is_term() && Term::stderr().is_term();
    let plain_output = !is_tty;

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

            if build_args.dev.dev {
                log::warn!(
                    "`--dev no longer has any effect. Please remove it from your command. It will be removed in a future version."
                );
            }

            if build_args.create_sourcedirs.was_explicitly_set() {
                log::warn!(
                    "`--create-sourcedirs` is deprecated: source_dirs.json is now always created. Please remove this flag from your command."
                );
            }

            match build::build(
                &build_args.filter,
                Path::new(&build_args.folder as &str),
                show_progress,
                build_args.no_timing,
                true, // create_sourcedirs is now always enabled
                plain_output,
                (*build_args.warn_error).clone(),
            ) {
                Err(e) => {
                    eprintln!("{:#}", e);
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

            if *watch_args.dev {
                log::warn!(
                    "`--dev no longer has any effect. Please remove it from your command. It will be removed in a future version."
                );
            }

            if watch_args.create_sourcedirs.was_explicitly_set() {
                log::warn!(
                    "`--create-sourcedirs` is deprecated: source_dirs.json is now always created. Please remove this flag from your command."
                );
            }

            match watcher::start(
                &watch_args.filter,
                show_progress,
                &watch_args.folder,
                (*watch_args.after_build).clone(),
                true, // create_sourcedirs is now always enabled
                plain_output,
                (*watch_args.warn_error).clone(),
            ) {
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1)
                }
                Ok(_) => Ok(()),
            }
        }
        cli::Command::Clean { folder, dev } => {
            let _lock = get_lock(&folder);

            if dev.dev {
                log::warn!(
                    "`--dev no longer has any effect. Please remove it from your command. It will be removed in a future version."
                );
            }

            build::clean::clean(Path::new(&folder as &str), show_progress, plain_output)
        }
        cli::Command::Format {
            stdin,
            check,
            files,
            dev,
        } => {
            if dev.dev {
                log::warn!(
                    "`--dev no longer has any effect. Please remove it from your command. It will be removed in a future version."
                );
            }
            format::format(stdin, check, files)
        }
    }
}

fn get_lock(folder: &str) -> lock::Lock {
    match lock::get(folder) {
        lock::Lock::Error(error) => {
            eprintln!("Could not start ReScript build: {error}");
            std::process::exit(1);
        }
        acquired_lock => acquired_lock,
    }
}

struct SplitLogger {
    stdout: env_logger::Logger,
    stderr: env_logger::Logger,
}

impl log::Log for SplitLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        self.stdout.enabled(metadata) || self.stderr.enabled(metadata)
    }

    fn log(&self, record: &log::Record) {
        match record.level() {
            log::Level::Error | log::Level::Warn => self.stderr.log(record),
            _ => self.stdout.log(record),
        }
    }

    fn flush(&self) {
        self.stdout.flush();
        self.stderr.flush();
    }
}
