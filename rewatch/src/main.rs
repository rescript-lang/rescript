use console::Term;
use log::LevelFilter;
use std::{io::Write, path::Path};
use tracing::instrument;

use rescript::{build, cli, cmd, format, lock, telemetry, watcher};

fn main() {
    // Initialize telemetry (only active if OTEL_EXPORTER_OTLP_ENDPOINT is set)
    let telemetry_guard = telemetry::init_telemetry();

    let exit_code = run_main(&telemetry_guard);

    // Drop the telemetry guard explicitly to ensure spans are flushed before exit
    drop(telemetry_guard);

    std::process::exit(exit_code);
}

fn run_main(telemetry_guard: &telemetry::TelemetryGuard) -> i32 {
    let cli = cli::parse_with_default().unwrap_or_else(|err| err.exit());

    let log_level_filter = cli.verbose.log_level_filter();

    // Only set up custom logger if OTEL is not enabled (OTEL sets up its own subscriber)
    if !telemetry_guard.otel_enabled {
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
        cli::Command::CompilerArgs { path } => run_compiler_args(&path),
        cli::Command::Build(build_args) => {
            let _lock = match get_lock(&build_args.folder) {
                Ok(lock) => lock,
                Err(code) => return code,
            };
            run_build(build_args, show_progress, plain_output)
        }
        cli::Command::Watch(watch_args) => {
            let _lock = match get_lock(&watch_args.folder) {
                Ok(lock) => lock,
                Err(code) => return code,
            };
            run_watch(watch_args, show_progress, plain_output)
        }
        cli::Command::Clean { folder } => {
            let _lock = match get_lock(&folder) {
                Ok(lock) => lock,
                Err(code) => return code,
            };
            run_clean(&folder, show_progress, plain_output)
        }
        cli::Command::Format { stdin, check, files } => run_format(stdin, check, files),
    }
}

#[instrument(name = "rewatch.compiler_args", skip_all, fields(file_path = %path))]
fn run_compiler_args(path: &str) -> i32 {
    match build::get_compiler_args(Path::new(path)) {
        Ok(args) => {
            println!("{}", args);
            0
        }
        Err(e) => {
            eprintln!("{:#}", e);
            1
        }
    }
}

#[instrument(name = "rewatch.build", skip_all, fields(working_dir = %build_args.folder.folder))]
fn run_build(build_args: cli::BuildArgs, show_progress: bool, plain_output: bool) -> i32 {
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
            1
        }
        Ok(_) => {
            if let Some(args_after_build) = (*build_args.after_build).clone() {
                cmd::run(args_after_build)
            }
            0
        }
    }
}

#[instrument(name = "rewatch.watch", skip_all, fields(working_dir = %watch_args.folder.folder))]
fn run_watch(watch_args: cli::WatchArgs, show_progress: bool, plain_output: bool) -> i32 {
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
            1
        }
        Ok(_) => 0,
    }
}

#[instrument(name = "rewatch.clean", skip_all, fields(working_dir = %folder))]
fn run_clean(folder: &str, show_progress: bool, plain_output: bool) -> i32 {
    match build::clean::clean(Path::new(folder), show_progress, plain_output) {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("{:#}", e);
            1
        }
    }
}

#[instrument(name = "rewatch.format", skip_all, fields(check = check, is_stdin = stdin.is_some()))]
fn run_format(stdin: Option<cli::FileExtension>, check: bool, files: Vec<String>) -> i32 {
    match format::format(stdin, check, files) {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("{:#}", e);
            1
        }
    }
}

fn get_lock(folder: &str) -> Result<lock::Lock, i32> {
    match lock::get(folder) {
        lock::Lock::Error(error) => {
            eprintln!("Could not start ReScript build: {error}");
            Err(1)
        }
        acquired_lock => Ok(acquired_lock),
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
