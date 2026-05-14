use anyhow::Result;
use console::Term;
use log::LevelFilter;
use std::{io::Write, path::Path};

use rescript::{
    build, cli, cmd, format,
    lock::{LockKind, drop_lock, get_lock_or_exit},
    telemetry, watcher,
};

fn main() {
    // Initialize telemetry (only active if OTEL_EXPORTER_OTLP_ENDPOINT is set)
    let telemetry_guard = telemetry::init_telemetry();
    let otel_enabled = telemetry_guard.otel_enabled();

    let exit_code = run_main(otel_enabled);

    // std::process::exit terminates the process without running Drops for
    // values still on the stack, so the TelemetryGuard's Drop (which flushes
    // buffered OTLP spans) would be skipped. Drop explicitly first.
    drop(telemetry_guard);

    std::process::exit(exit_code);
}

fn run_main(otel_enabled: bool) -> i32 {
    let cli = cli::parse_with_default().unwrap_or_else(|err| err.exit());

    let log_level_filter = cli.verbose.log_level_filter();

    // Only set up custom logger if OTEL is not enabled (OTEL sets up its own subscriber)
    if !otel_enabled {
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
    }

    let is_tty: bool = Term::stdout().is_term() && Term::stderr().is_term();
    let plain_output = !is_tty;

    // Show progress messages (e.g. "Finished compilation") as long as logging is at Info level
    // or more verbose. This way `-v` and `-vv` add debug output without suppressing progress.
    let show_progress = log_level_filter >= LevelFilter::Info;

    match cli.command {
        cli::Command::CompilerArgs { path } => {
            exit_code(build::get_compiler_args(Path::new(&path)).map(|args| println!("{}", args)))
        }
        cli::Command::Build(build_args) => {
            let features = build_args.features.parsed();
            let result = build::build(
                &build_args.filter,
                build_args.folder.as_ref(),
                show_progress,
                build_args.no_timing,
                true, // create_sourcedirs is now always enabled
                plain_output,
                (*build_args.warn_error).clone(),
                build_args.prod,
                features,
            );
            if result.is_ok()
                && let Some(args_after_build) = (*build_args.after_build).clone()
            {
                cmd::run(args_after_build);
            }
            exit_code(result.map(|_| ()))
        }
        cli::Command::Watch(watch_args) => {
            let _lock = get_lock_or_exit(LockKind::Watch, &watch_args.folder);

            let features = watch_args.features.parsed();
            exit_code(watcher::start(
                &watch_args.filter,
                show_progress,
                &watch_args.folder,
                (*watch_args.after_build).clone(),
                true, // create_sourcedirs is now always enabled
                plain_output,
                (*watch_args.warn_error).clone(),
                watch_args.clear_screen,
                watch_args.prod,
                features,
            ))
        }
        cli::Command::Clean { folder, prod } => {
            let _lock = get_lock_or_exit(LockKind::Build, &folder);
            let code = exit_code(build::clean::clean(
                folder.as_ref(),
                show_progress,
                plain_output,
                prod,
            ));
            let _ = drop_lock(LockKind::Build, &folder);

            code
        }
        cli::Command::Format { stdin, check, files } => exit_code(format::format(stdin, check, files)),
    }
}

/// Map a command's Result<()> to a process exit code, printing the error on failure.
fn exit_code(result: Result<()>) -> i32 {
    match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{:#}", e);
            1
        }
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
