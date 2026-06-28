pub mod build_types;
pub mod clean;
pub mod compile;
pub mod compiler_info;
pub mod deps;
pub mod logs;
pub mod namespaces;
pub mod packages;
pub mod parse;
pub mod read_compile_state;

use self::parse::parser_args;
use crate::build::compile::{mark_modules_with_deleted_deps_dirty, mark_modules_with_expired_deps_dirty};
use crate::build::compiler_info::{CompilerCheckResult, verify_compiler_info, write_compiler_info};
use crate::config::SourceMapCommand;
use crate::helpers::emojis::*;
use crate::helpers::{self};
use crate::lock::{LockKind, drop_lock, get_lock_or_exit};
use crate::project_context::ProjectContext;
use crate::sourcedirs;
use anyhow::{Context, Result, anyhow};
use build_types::*;
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use log::log_enabled;
use serde::Serialize;
use std::fmt;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tracing::{info_span, instrument};

fn is_dirty(module: &Module) -> bool {
    match module.source_type {
        SourceType::SourceFile(SourceFile {
            implementation: Implementation {
                parse_dirty: true, ..
            },
            ..
        }) => true,
        SourceType::SourceFile(SourceFile {
            interface: Some(Interface {
                parse_dirty: true, ..
            }),
            ..
        }) => true,
        SourceType::SourceFile(_) => false,
        SourceType::MlMap(MlMap {
            parse_dirty: dirty, ..
        }) => dirty,
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct CompilerArgs {
    pub compiler_args: Vec<String>,
    pub parser_args: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilationOutcome {
    Clean,
    Warnings,
}

fn has_output(output: &str) -> bool {
    helpers::contains_ascii_characters(output)
}

fn has_config_warnings(build_state: &BuildCommandState) -> bool {
    build_state.packages.iter().any(|(_, package)| {
        package.is_local_dep
            && (!package.config.get_unsupported_fields().is_empty()
                || !package.config.get_unknown_fields().is_empty())
    })
}

pub fn format_finished_compilation_message(
    compilation_kind: Option<&str>,
    outcome: CompilationOutcome,
    duration: Duration,
) -> String {
    let compilation_kind = compilation_kind
        .map(|kind| format!("{kind} "))
        .unwrap_or_default();
    let (status, warning_suffix) = match outcome {
        CompilationOutcome::Clean => (CHECKMARK, ""),
        CompilationOutcome::Warnings => (WARNING, " with warnings"),
    };

    format!(
        "{LINE_CLEAR}{status}Finished {compilation_kind}compilation{warning_suffix} in {:.2}s",
        duration.as_secs_f64()
    )
}

#[instrument(name = "rewatch.compiler_args", skip_all, fields(file_path = %rescript_file_path.display()))]
pub fn get_compiler_args(rescript_file_path: &Path) -> Result<String> {
    let filename = &helpers::get_abs_path(rescript_file_path);
    let current_package = helpers::get_abs_path(
        &helpers::get_nearest_config(rescript_file_path).expect("Couldn't find package root"),
    );
    let project_context = ProjectContext::new(&current_package)?;

    let is_type_dev = match filename.strip_prefix(&current_package) {
        Err(_) => false,
        Ok(relative_path) => project_context
            .current_config
            .find_is_type_dev_for_path(relative_path),
    };

    // make PathBuf from package root and get the relative path for filename
    let relative_filename = filename.strip_prefix(PathBuf::from(&current_package)).unwrap();

    let file_path = PathBuf::from(&current_package).join(filename);
    let contents = helpers::read_file(&file_path).expect("Error reading file");

    let (ast_path, parser_args) = parser_args(
        &project_context,
        &project_context.current_config,
        relative_filename,
        &contents,
        /* is_local_dep */ true,
        /* warn_error_override */ None,
    )?;
    let is_interface = filename.to_string_lossy().ends_with('i');
    let has_interface = if is_interface {
        true
    } else {
        let mut interface_filename = filename.to_string_lossy().to_string();
        interface_filename.push('i');
        PathBuf::from(&interface_filename).exists()
    };
    let compiler_args = compile::compiler_args(
        &project_context.current_config,
        &ast_path,
        relative_filename,
        is_interface,
        has_interface,
        &project_context,
        &None,
        is_type_dev,
        true,
        None, // No warn_error_override for compiler-args command
        SourceMapCommand::Build,
        &[], // Source dirs not available outside full build; gentype falls back to defaults.
    )?;

    let result = serde_json::to_string_pretty(&CompilerArgs {
        compiler_args,
        parser_args,
    })?;

    Ok(result)
}

pub fn get_compiler_info(project_context: &ProjectContext) -> Result<CompilerInfo> {
    let bsc_path = helpers::get_bsc();
    let bsc_hash = helpers::compute_file_hash(&bsc_path).ok_or(anyhow!(
        "Failed to compute bsc hash for {}",
        bsc_path.to_string_lossy()
    ))?;
    let runtime_path = compile::get_runtime_path(&project_context.current_config, project_context)?;
    Ok(CompilerInfo {
        bsc_path,
        bsc_hash,
        runtime_path,
    })
}

#[allow(clippy::too_many_arguments)]
#[instrument(name = "build.initialize", skip_all)]
pub fn initialize_build(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    show_progress: bool,
    path: &Path,
    plain_output: bool,
    warn_error: Option<String>,
    prod: bool,
    features: Option<Vec<String>>,
    source_map_command: SourceMapCommand,
) -> Result<BuildCommandState> {
    let project_context = ProjectContext::new(path)?;
    let compiler = get_compiler_info(&project_context)?;

    let timing_clean_start = Instant::now();
    let packages = packages::make(filter, &project_context, show_progress, prod, features.as_ref())?;

    let source_map_args = project_context
        .get_root_config()
        .get_source_map_args(source_map_command);
    let compiler_check = verify_compiler_info(&packages, &compiler, &source_map_args);

    if !packages::validate_packages_dependencies(&packages) {
        return Err(anyhow!("Failed to validate package dependencies"));
    }

    let mut build_state = BuildCommandState::new(
        path.to_path_buf(),
        project_context,
        packages,
        compiler,
        warn_error,
        features,
        source_map_command,
    );
    packages::parse_packages(&mut build_state)?;

    let compile_assets_state = read_compile_state::read(&mut build_state)?;

    let (diff_cleanup, total_cleanup) = clean::cleanup_previous_build(&mut build_state, compile_assets_state);
    let timing_clean_total = timing_clean_start.elapsed();

    if show_progress {
        if plain_output {
            if let CompilerCheckResult::CleanedPackagesDueToCompiler = compiler_check {
                // Snapshot-friendly output (no progress prefixes or emojis)
                println!("Cleaned previous build due to compiler update");
            }
            println!("Cleaned {diff_cleanup}/{total_cleanup}")
        } else {
            if let CompilerCheckResult::CleanedPackagesDueToCompiler = compiler_check {
                println!(
                    "{}{} {}Cleaned previous build due to compiler update",
                    LINE_CLEAR,
                    style("[1/3]").bold().dim(),
                    SWEEP
                );
            }
            println!(
                "{}{} {}Cleaned {}/{} in {:.2}s",
                LINE_CLEAR,
                style("[1/3]").bold().dim(),
                SWEEP,
                diff_cleanup,
                total_cleanup,
                default_timing.unwrap_or(timing_clean_total).as_secs_f64()
            );
        }
    }

    Ok(build_state)
}

fn format_step(current: usize, total: usize) -> console::StyledObject<String> {
    style(format!("[{current}/{total}]")).bold().dim()
}

#[derive(Debug, Clone)]
pub enum IncrementalBuildErrorKind {
    SourceFileParseError,
    CompileError(Option<String>),
}

#[derive(Debug, Clone)]
pub struct IncrementalBuildError {
    pub plain_output: bool,
    pub kind: IncrementalBuildErrorKind,
}

impl fmt::Display for IncrementalBuildError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IncrementalBuildErrorKind::SourceFileParseError => {
                if self.plain_output {
                    write!(f, "{LINE_CLEAR}  Could not parse Source Files",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Could not parse Source Files",)
                }
            }
            IncrementalBuildErrorKind::CompileError(Some(e)) => {
                if self.plain_output {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. Error: {e}",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. Error: {e}",)
                }
            }
            IncrementalBuildErrorKind::CompileError(None) => {
                if self.plain_output {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. See Errors Above",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. See Errors Above",)
                }
            }
        }
    }
}

pub fn with_build_lock<T>(path: &Path, f: impl FnOnce() -> T) -> T {
    let build_folder = path.to_string_lossy().to_string();
    let _lock = get_lock_or_exit(LockKind::Build, &build_folder);
    let result = f();
    let _lock = drop_lock(LockKind::Build, &build_folder);
    result
}

#[instrument(name = "build.incremental", skip_all, fields(module_count = build_state.modules.len()))]
pub fn incremental_build(
    build_state: &mut BuildCommandState,
    default_timing: Option<Duration>,
    initial_build: bool,
    show_progress: bool,
    only_incremental: bool,
    create_sourcedirs: bool,
    plain_output: bool,
) -> Result<CompilationOutcome, IncrementalBuildError> {
    let build_folder = build_state.root_folder.clone();
    with_build_lock(&build_folder, || {
        incremental_build_without_lock(
            build_state,
            default_timing,
            initial_build,
            show_progress,
            only_incremental,
            create_sourcedirs,
            plain_output,
        )
    })
}

// `build` needs to lock before initialization because initialization mutates previous
// build artifacts. Keep the compile body separate so it can run under that wider lock.
#[instrument(name = "build.incremental.unlocked", skip_all, fields(module_count = build_state.modules.len()))]
pub fn incremental_build_without_lock(
    build_state: &mut BuildCommandState,
    default_timing: Option<Duration>,
    initial_build: bool,
    show_progress: bool,
    only_incremental: bool,
    create_sourcedirs: bool,
    plain_output: bool,
) -> Result<CompilationOutcome, IncrementalBuildError> {
    logs::initialize(&build_state.packages);
    let num_dirty_modules = build_state.modules.values().filter(|m| is_dirty(m)).count() as u64;
    let pb = if !plain_output && show_progress {
        ProgressBar::new(num_dirty_modules)
    } else {
        ProgressBar::hidden()
    };
    let mut current_step = if only_incremental { 1 } else { 2 };
    let total_steps = if only_incremental { 2 } else { 3 };
    pb.set_style(
        ProgressStyle::with_template(&format!(
            "{} {}Parsing... {{spinner}} {{pos}}/{{len}} {{msg}}",
            format_step(current_step, total_steps),
            PARSE
        ))
        .unwrap(),
    );

    let timing_parse_start = Instant::now();
    let timing_ast = Instant::now();
    let result_asts = parse::generate_asts(build_state, || pb.inc(1));
    let timing_ast_elapsed = timing_ast.elapsed();

    let parse_warnings = match result_asts {
        Ok(warnings) => {
            pb.finish();
            warnings
        }
        Err(err) => {
            let _error_span = info_span!("build.parse_error").entered();
            logs::finalize(&build_state.packages);

            if !plain_output && show_progress {
                eprintln!(
                    "{}{} {}Error parsing source files in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    CROSS,
                    default_timing.unwrap_or(timing_ast_elapsed).as_secs_f64()
                );
                pb.finish();
            }

            eprintln!("{}", &err);

            return Err(IncrementalBuildError {
                kind: IncrementalBuildErrorKind::SourceFileParseError,
                plain_output,
            });
        }
    };
    let deleted_modules = build_state.deleted_modules.clone();
    deps::get_deps(build_state, &deleted_modules);
    let timing_parse_total = timing_parse_start.elapsed();

    if show_progress {
        if plain_output {
            println!("Parsed {num_dirty_modules} source files")
        } else {
            println!(
                "{}{} {}Parsed {} source files in {:.2}s",
                LINE_CLEAR,
                format_step(current_step, total_steps),
                PARSE,
                num_dirty_modules,
                default_timing.unwrap_or(timing_parse_total).as_secs_f64()
            );
        }
    }
    let has_parse_warnings = has_output(&parse_warnings);
    if has_parse_warnings {
        eprintln!("{}", &parse_warnings);
    }

    mark_modules_with_expired_deps_dirty(build_state);
    mark_modules_with_deleted_deps_dirty(&mut build_state.build_state);
    current_step += 1;

    //print all the compile_dirty modules
    if log_enabled!(log::Level::Trace) {
        for (module_name, module) in build_state.modules.iter() {
            if module.compile_dirty {
                println!("compile dirty: {module_name}");
            }
        }
    };

    let start_compiling = Instant::now();
    let pb = if !plain_output && show_progress {
        ProgressBar::new(build_state.modules.len().try_into().unwrap())
    } else {
        ProgressBar::hidden()
    };
    pb.set_style(
        ProgressStyle::with_template(&format!(
            "{} {}Compiling... {{spinner}} {{pos}}/{{len}} {{msg}}",
            format_step(current_step, total_steps),
            SWORDS
        ))
        .unwrap(),
    );

    let (compile_errors, compile_warnings, num_compiled_modules) = compile::compile(
        build_state,
        show_progress,
        || pb.inc(1),
        |size| pb.set_length(size),
    )
    .map_err(|e| IncrementalBuildError {
        kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
        plain_output,
    })?;

    let compile_duration = start_compiling.elapsed();

    logs::finalize(&build_state.packages);
    if create_sourcedirs {
        sourcedirs::print(build_state);
    }
    pb.finish();
    if !compile_errors.is_empty() {
        let _error_span = info_span!("build.compile_error").entered();
        if show_progress {
            if plain_output {
                eprintln!("Compiled {num_compiled_modules} modules")
            } else {
                eprintln!(
                    "{}{} {}Compiled {} modules in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    CROSS,
                    num_compiled_modules,
                    default_timing.unwrap_or(compile_duration).as_secs_f64()
                );
            }
        }
        if has_output(&compile_warnings) {
            let _warning_span = info_span!("build.compile_warning").entered();
            eprintln!("{}", &compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }
        if has_output(&compile_errors) {
            eprintln!("{}", &compile_errors);
        }

        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            plain_output,
        })
    } else {
        let has_compile_warnings = has_output(&compile_warnings);
        let has_config_warning_output = initial_build && has_config_warnings(build_state);
        let outcome = if has_parse_warnings || has_compile_warnings || has_config_warning_output {
            CompilationOutcome::Warnings
        } else {
            CompilationOutcome::Clean
        };

        if show_progress {
            if plain_output {
                println!("Compiled {num_compiled_modules} modules")
            } else {
                println!(
                    "{}{} {}Compiled {} modules in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    SWORDS,
                    num_compiled_modules,
                    default_timing.unwrap_or(compile_duration).as_secs_f64()
                );
            }
        }

        if has_compile_warnings {
            eprintln!("{}", &compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }

        // Write per-package compiler metadata to `lib/bs/compiler-info.json` (idempotent)
        write_compiler_info(build_state);

        Ok(outcome)
    }
}

fn log_config_warnings(build_state: &BuildCommandState) {
    let mut packages: Vec<_> = build_state.packages.values().collect();
    packages.sort_by(|a, b| a.name.cmp(&b.name));
    packages.iter().for_each(|package| {
        let deprecations = package.config.get_deprecations();
        if !deprecations.is_empty() {
            // External consumers can't fix deprecations themselves, so we
            // point them at the package's issue tracker when we can find one.
            let issue_tracker_url = if package.is_local_dep {
                None
            } else {
                crate::build::packages::read_issue_tracker_url(&package.path)
            };
            log_deprecations(&package.name, deprecations, issue_tracker_url.as_deref());
        }

        if package.is_local_dep {
            package
                .config
                .get_unsupported_fields()
                .iter()
                .for_each(|field| log_unsupported_config_field(&package.name, field));

            package
                .config
                .get_unknown_fields()
                .iter()
                .for_each(|field| log_unknown_config_field(&package.name, field));
        }
    });
}

fn log_deprecations(
    package_name: &str,
    deprecations: &[crate::config::DeprecationWarning],
    issue_tracker_url: Option<&str>,
) {
    let mut message = format!(
        "Package '{package_name}' uses deprecated config (support will be removed in a future version):"
    );
    for deprecation in deprecations {
        let line = match deprecation {
            crate::config::DeprecationWarning::BsconfigJson => {
                "  - filename 'bsconfig.json' — rename to 'rescript.json'"
            }
            crate::config::DeprecationWarning::BsDependencies => {
                "  - field 'bs-dependencies' — use 'dependencies' instead"
            }
            crate::config::DeprecationWarning::BsDevDependencies => {
                "  - field 'bs-dev-dependencies' — use 'dev-dependencies' instead"
            }
            crate::config::DeprecationWarning::BscFlags => {
                "  - field 'bsc-flags' — use 'compiler-flags' instead"
            }
            crate::config::DeprecationWarning::CjsModule => {
                "  - module 'cjs' in package-specs — use 'commonjs' instead"
            }
            crate::config::DeprecationWarning::Es6Module => {
                "  - module 'es6' in package-specs — use 'esmodule' instead"
            }
        };
        message.push('\n');
        message.push_str(line);
    }
    if let Some(url) = issue_tracker_url {
        message.push_str(&format!("\nPlease report this to the package maintainer: {url}"));
    }
    eprintln!("\n{}", style(message).yellow());
}

fn log_unsupported_config_field(package_name: &str, field_name: &str) {
    let warning = format!(
        "The field '{field_name}' found in the package config of '{package_name}' is not supported by ReScript 12's new build system."
    );
    eprintln!("\n{}", style(warning).yellow());
}

fn log_unknown_config_field(package_name: &str, field_name: &str) {
    let warning = format!(
        "Unknown field '{field_name}' found in the package config of '{package_name}'. This option will be ignored."
    );
    eprintln!("\n{}", style(warning).yellow());
}

// write build.ninja files in the packages after a non-incremental build
// this is necessary to bust the editor tooling cache. The editor tooling
// is watching this file.
// we don't need to do this in an incremental build because there are no file
// changes (deletes / additions)
pub fn write_build_ninja(build_state: &BuildCommandState) {
    for package in build_state.packages.values() {
        // write empty file:
        let mut f = File::create(package.get_build_path().join("build.ninja")).expect("Unable to write file");
        f.write_all(b"").expect("unable to write to ninja file");
    }
}

#[allow(clippy::too_many_arguments)]
#[instrument(name = "rewatch.build", skip_all, fields(working_dir = %path.display()))]
pub fn build(
    filter: &Option<regex::Regex>,
    path: &Path,
    show_progress: bool,
    no_timing: bool,
    create_sourcedirs: bool,
    plain_output: bool,
    warn_error: Option<String>,
    prod: bool,
    features: Option<Vec<String>>,
) -> Result<BuildCommandState> {
    let default_timing: Option<std::time::Duration> = if no_timing {
        Some(std::time::Duration::new(0.0 as u64, 0.0 as u32))
    } else {
        None
    };
    let timing_total = Instant::now();
    with_build_lock(path, || {
        let mut build_state = initialize_build(
            default_timing,
            filter,
            show_progress,
            path,
            plain_output,
            warn_error,
            prod,
            features,
            SourceMapCommand::Build,
        )
        .with_context(|| "Could not initialize build")?;

        match incremental_build_without_lock(
            &mut build_state,
            default_timing,
            true,
            show_progress,
            false,
            create_sourcedirs,
            plain_output,
        ) {
            Ok(result) => {
                if !plain_output && show_progress {
                    let timing_total_elapsed = timing_total.elapsed();
                    println!(
                        "\n{}",
                        format_finished_compilation_message(
                            None,
                            result,
                            default_timing.unwrap_or(timing_total_elapsed),
                        )
                    );
                }
                clean::cleanup_after_build(&build_state);
                write_build_ninja(&build_state);
                Ok(build_state)
            }
            Err(e) => {
                clean::cleanup_after_build(&build_state);
                write_build_ninja(&build_state);
                Err(anyhow!("Incremental build failed. Error: {e}"))
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process;
    use std::sync::mpsc;
    use std::thread;
    use tempfile::TempDir;

    #[test]
    fn with_build_lock_holds_lock_while_running_work() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let project_folder = temp_dir.path();
        let build_lock_path = project_folder.join("lib").join(LockKind::Build.file_name());

        let result = with_build_lock(project_folder, || {
            assert!(
                build_lock_path.exists(),
                "build lock should be held while guarded work runs"
            );
            42
        });

        assert_eq!(result, 42);
        assert!(
            !build_lock_path.exists(),
            "build lock should be removed after guarded work finishes"
        );
    }

    #[test]
    fn with_build_lock_drops_lock_after_error_result() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let project_folder = temp_dir.path();
        let build_lock_path = project_folder.join("lib").join(LockKind::Build.file_name());

        let result: Result<(), &str> = with_build_lock(project_folder, || Err("failed"));

        assert_eq!(result, Err("failed"));
        assert!(
            !build_lock_path.exists(),
            "build lock should be removed after guarded work returns an error"
        );
    }

    #[test]
    fn build_waits_for_lock_before_initializing() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let project_folder = temp_dir.path().to_path_buf();
        let lib_dir = project_folder.join("lib");
        let build_lock_path = lib_dir.join(LockKind::Build.file_name());
        fs::create_dir_all(&lib_dir).expect("lib directory should be created");
        fs::write(&build_lock_path, process::id().to_string()).expect("lockfile should be written");

        let (sender, receiver) = mpsc::channel();
        let build_project_folder = project_folder.clone();
        let build_thread = thread::spawn(move || {
            // This temp project has no config, so initialization would fail immediately
            // if `build` did not wait for the lock first.
            let result = build(
                &None,
                &build_project_folder,
                false,
                true,
                false,
                true,
                None,
                false,
                None,
            );
            sender.send(result.is_err()).expect("result should be sent");
        });

        assert!(
            receiver.recv_timeout(Duration::from_millis(250)).is_err(),
            "build should wait for build.lock before running initialization"
        );

        fs::remove_file(&build_lock_path).expect("lockfile should be removed");
        assert!(
            receiver
                .recv_timeout(Duration::from_secs(5))
                .expect("build should finish after lock is removed")
        );
        build_thread.join().expect("build thread should complete");
    }

    #[test]
    fn formats_successful_completion_message() {
        assert_eq!(
            format_finished_compilation_message(None, CompilationOutcome::Clean, Duration::from_millis(1500),),
            format!("{LINE_CLEAR}{}Finished compilation in 1.50s", CHECKMARK)
        );
    }

    #[test]
    fn formats_warning_completion_message() {
        assert_eq!(
            format_finished_compilation_message(
                Some("incremental"),
                CompilationOutcome::Warnings,
                Duration::from_millis(1500),
            ),
            format!(
                "{LINE_CLEAR}{}Finished incremental compilation with warnings in 1.50s",
                WARNING
            )
        );
    }
}
