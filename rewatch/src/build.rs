pub mod build_types;
pub mod clean;
pub mod compile;
pub mod compiler_info;
pub mod deps;
pub mod format;
pub mod logs;
pub mod namespaces;
pub mod packages;
pub mod parse;
pub mod read_compile_state;

use self::parse::parser_args;
use crate::build::compile::{mark_modules_with_deleted_deps_dirty, mark_modules_with_expired_deps_dirty};
use crate::build::compiler_info::{CompilerCheckResult, verify_compiler_info, write_compiler_info};
use crate::helpers;
use crate::project_context::ProjectContext;
use crate::sourcedirs;
use ahash::AHashSet;
use anyhow::{Result, anyhow};
use build_types::*;
use serde::Serialize;
use std::fmt;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::{Duration, Instant};
use tracing::info_span;

/// Progress events emitted during build
#[derive(Debug, Clone)]
pub enum BuildProgress {
    /// Build artifacts cleaned (during build)
    Cleaned {
        cleaned_count: i32,
        total_count: i32,
        duration_seconds: f64,
        due_to_compiler_update: bool,
    },
    /// Source files parsed
    Parsed {
        parsed_count: i32,
        duration_seconds: f64,
    },
    /// Incremental compile progress (sent as each module compiles)
    Compiling { current_count: i32, total_count: i32 },
    /// AST generation progress
    GeneratingAst { module_name: String },
    /// Modules compiled (final)
    Compiled {
        compiled_count: i32,
        duration_seconds: f64,
    },
    /// Compiler warning (pass-through from bsc, already formatted)
    CompilerWarning(String),
    /// Compiler error (pass-through from bsc, already formatted)
    CompilerError(String),
    /// Compiler assets cleaned (during clean command)
    CleanedCompilerAssets { duration_seconds: f64 },
    /// JS files cleaned (during clean command)
    CleanedJsFiles { suffix: String, duration_seconds: f64 },
    /// Circular dependency detected in the codebase
    CircularDependency { cycle_description: String },
    /// Unallowed dependency detected in package configuration
    UnallowedDependency {
        package_name: String,
        groups: Vec<UnallowedDependencyGroup>,
    },
    /// Error building the package tree
    PackageTreeError { package_name: String, error: String },
    /// Module not found during build
    ModuleNotFound { module_name: String },
    /// Config warning (unsupported or unknown field)
    ConfigWarning {
        package_name: String,
        field_name: String,
        kind: ConfigWarningKind,
    },
    /// Duplicated package warning
    DuplicatedPackage {
        package_name: String,
        chosen_path: String,
        duplicate_path: String,
        parent_path: String,
    },
    /// Interface file without implementation
    MissingImplementation { interface_file: String },
    /// Error during build initialization (e.g., duplicate modules, config errors)
    InitializationError(String),
    /// Package name mismatch between package.json and rescript.json
    PackageNameMismatch {
        package_path: String,
        package_json_name: String,
        rescript_json_name: String,
    },
    /// Output from js-post-build command
    JsPostBuildOutput {
        command: String,
        js_file: String,
        stdout: Option<String>,
        stderr: Option<String>,
    },
}

/// Kind of config warning
#[derive(Debug, Clone)]
pub enum ConfigWarningKind {
    /// Field not supported by ReScript 12's build system
    Unsupported,
    /// Unknown field, will be ignored
    Unknown,
}

/// A group of unallowed dependencies of a specific type
#[derive(Debug, Clone)]
pub struct UnallowedDependencyGroup {
    pub deps_type: String,
    pub deps: Vec<String>,
}

/// Trait for reporting build progress
pub trait BuildReporter: Send + Sync {
    fn report(&self, progress: BuildProgress);
}

/// A no-op reporter that discards all progress messages.
/// Used for standalone operations like `get_compiler_args` that don't need progress reporting.
pub struct NoopReporter;

impl BuildReporter for NoopReporter {
    fn report(&self, _progress: BuildProgress) {
        // Discard all progress
    }
}

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

/// Compute compiler arguments for a given file using a provided ProjectContext.
/// This is the core implementation used by both standalone and daemon modes.
pub fn get_compiler_args_with_context<R: BuildReporter>(
    rescript_file_path: &Path,
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<CompilerArgs> {
    let filename = &helpers::get_abs_path(rescript_file_path);
    let current_package_path = helpers::get_abs_path(
        &helpers::get_nearest_config(rescript_file_path).expect("Couldn't find package root"),
    );

    // Read the config for the file's package (may differ from project root in a monorepo)
    let file_package_config = packages::read_config(&current_package_path)?;

    let is_type_dev = match filename.strip_prefix(&current_package_path) {
        Err(_) => false,
        Ok(relative_path) => file_package_config.find_is_type_dev_for_path(relative_path),
    };

    // make PathBuf from package root and get the relative path for filename
    let relative_filename = filename
        .strip_prefix(PathBuf::from(&current_package_path))
        .unwrap();

    let file_path = PathBuf::from(&current_package_path).join(filename);
    let contents = helpers::read_file(&file_path)?;

    let (ast_path, parser_args) = parser_args(
        project_context,
        &file_package_config,
        relative_filename,
        &contents,
        /* is_local_dep */ true,
        /* warn_error_override */ None,
        reporter,
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
        &file_package_config,
        &ast_path,
        relative_filename,
        is_interface,
        has_interface,
        project_context,
        &None,
        is_type_dev,
        true,
        None, // No warn_error_override for compiler-args command
        reporter,
    )?;

    Ok(CompilerArgs {
        compiler_args,
        parser_args,
    })
}

/// Compute compiler args without a pre-existing ProjectContext.
/// This creates a fresh ProjectContext from scratch. Used when no daemon/cache is available.
/// Uses NoopReporter since this is a synchronous query for editor tooling - debug/trace
/// messages during package resolution are intentionally discarded.
pub fn get_compiler_args_standalone(rescript_file_path: &Path) -> Result<CompilerArgs> {
    let current_package = helpers::get_abs_path(
        &helpers::get_nearest_config(rescript_file_path)
            .ok_or_else(|| anyhow!("Couldn't find package root"))?,
    );
    let noop_reporter = NoopReporter;
    let project_context = ProjectContext::new(&current_package)?;

    get_compiler_args_with_context(rescript_file_path, &project_context, &noop_reporter)
}

/// Standalone entry point for getting compiler args as JSON (used when no daemon is running).
pub fn get_compiler_args(rescript_file_path: &Path) -> Result<String> {
    let result = get_compiler_args_standalone(rescript_file_path)?;
    let json = serde_json::to_string_pretty(&result)?;

    Ok(json)
}

pub fn get_compiler_info<R: BuildReporter>(
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<CompilerInfo> {
    let bsc_path = helpers::get_bsc();

    let bsc_hash = helpers::compute_file_hash(&bsc_path).ok_or(anyhow!(
        "Failed to compute bsc hash for {}",
        bsc_path.to_string_lossy()
    ))?;

    let runtime_path = compile::get_runtime_path(&project_context.current_config, project_context, reporter)?;

    Ok(CompilerInfo {
        bsc_path,
        bsc_hash,
        runtime_path,
    })
}

pub fn initialize_build<R: BuildReporter>(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    path: &Path,
    reporter: &R,
) -> Result<BuildState> {
    initialize_build_with_scope(default_timing, filter, path, None, reporter)
}

/// Initialize build with optional scoping to a specific package.
/// If `scope_to_package` is Some, only load sources for that package and its dependencies.
/// If None, load sources for all packages (full build).
pub fn initialize_build_with_scope<R: BuildReporter>(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    path: &Path,
    scope_to_package: Option<&str>,
    reporter: &R,
) -> Result<BuildState> {
    let project_context = ProjectContext::new(path)?;
    let compiler = get_compiler_info(&project_context, reporter)?;

    let timing_clean_start = Instant::now();

    // Compute which packages to load based on scope
    let packages_to_load: Option<AHashSet<String>> = match scope_to_package {
        Some(entry_pkg) => {
            // First discover all packages without loading sources to compute scope
            let packages_without_sources =
                packages::make_with_scope(filter, &project_context, Some(&AHashSet::new()), reporter)?;
            Some(packages::compute_build_scope(
                &packages_without_sources,
                entry_pkg,
            ))
        }
        None => None, // Load all packages
    };

    // Load packages with appropriate scope
    let packages = packages::make_with_scope(filter, &project_context, packages_to_load.as_ref(), reporter)?;

    let compiler_check = verify_compiler_info(&packages, &compiler, reporter);

    if !packages::validate_packages_dependencies(&packages, reporter) {
        return Err(anyhow!("Failed to validate package dependencies"));
    }

    let mut build_state = BuildState::new(project_context, packages, compiler);
    packages::parse_packages(&mut build_state, reporter)?;

    let compile_assets_state = read_compile_state::read(&mut build_state, reporter)?;

    let (diff_cleanup, total_cleanup) =
        clean::cleanup_previous_build(&mut build_state, compile_assets_state, reporter);
    let timing_clean_total = timing_clean_start.elapsed();

    let due_to_compiler_update = matches!(compiler_check, CompilerCheckResult::CleanedPackagesDueToCompiler);

    reporter.report(BuildProgress::Cleaned {
        cleaned_count: diff_cleanup as i32,
        total_count: total_cleanup as i32,
        duration_seconds: default_timing.unwrap_or(timing_clean_total).as_secs_f64(),
        due_to_compiler_update,
    });

    Ok(build_state)
}

#[derive(Debug, Clone)]
pub enum IncrementalBuildErrorKind {
    SourceFileParseError,
    CompileError(Option<String>),
    /// Error during build initialization (e.g., duplicate modules, config errors)
    InitializationError(String),
}

#[derive(Debug, Clone)]
pub struct IncrementalBuildError {
    pub kind: IncrementalBuildErrorKind,
}

impl fmt::Display for IncrementalBuildError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IncrementalBuildErrorKind::SourceFileParseError => {
                write!(f, "Could not parse Source Files")
            }
            IncrementalBuildErrorKind::CompileError(Some(e)) => {
                write!(f, "Failed to Compile. Error: {e}")
            }
            IncrementalBuildErrorKind::CompileError(None) => {
                write!(f, "Failed to Compile. See Errors Above")
            }
            IncrementalBuildErrorKind::InitializationError(e) => {
                write!(f, "{e}")
            }
        }
    }
}

pub fn incremental_build<R: BuildReporter>(
    build_state: &mut BuildState,
    warn_error: Option<String>,
    filter: Option<regex::Regex>,
    default_timing: Option<Duration>,
    create_sourcedirs: bool,
    reporter: &R,
) -> Result<(), IncrementalBuildError> {
    logs::initialize(&build_state.packages);
    let matches_filter = |name: &str| -> bool {
        match &filter {
            Some(re) => re.is_match(name),
            None => true,
        }
    };
    let dirty_module_names: Vec<&String> = build_state
        .modules
        .iter()
        .filter(|(name, m)| is_dirty(m) && matches_filter(name))
        .map(|(name, _)| name)
        .collect();
    let num_dirty_modules = dirty_module_names.len() as u64;

    // Debug: Show dirty modules
    tracing::debug!(
        total_modules = build_state.modules.len(),
        dirty_modules = num_dirty_modules,
        "incremental_build"
    );
    for (name, module) in build_state.modules.iter() {
        if is_dirty(module) && matches_filter(name) {
            tracing::debug!(module = %name, "dirty module");
        }
    }

    let timing_parse_start = Instant::now();
    let (parse_warnings, timing_parse_total) = {
        let dirty_modules_str = dirty_module_names
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(",");
        let _span = info_span!(
            "build.parse",
            dirty_modules = num_dirty_modules,
            dirty_module_names = dirty_modules_str.as_str()
        )
        .entered();

        let result_asts = parse::generate_asts(build_state, warn_error.clone(), || {}, reporter);

        let parse_warnings = match result_asts {
            Ok(warnings) => warnings,
            Err(err) => {
                let _error_span = info_span!("build.parse_error").entered();
                logs::finalize(&build_state.packages);
                reporter.report(BuildProgress::CompilerError(err.to_string()));
                return Err(IncrementalBuildError {
                    kind: IncrementalBuildErrorKind::SourceFileParseError,
                });
            }
        };

        {
            let _span = info_span!("build.get_deps").entered();
            let deleted_modules = build_state.deleted_modules.clone();
            deps::get_deps(build_state, &deleted_modules);
        }

        (parse_warnings, timing_parse_start.elapsed())
    };

    reporter.report(BuildProgress::Parsed {
        parsed_count: num_dirty_modules as i32,
        duration_seconds: default_timing.unwrap_or(timing_parse_total).as_secs_f64(),
    });

    if helpers::contains_ascii_characters(&parse_warnings) {
        reporter.report(BuildProgress::CompilerWarning(parse_warnings.clone()));
    }

    mark_modules_with_expired_deps_dirty(build_state);
    mark_modules_with_deleted_deps_dirty(build_state);

    // Report all compile_dirty modules at trace level
    for (module_name, module) in build_state.modules.iter() {
        if module.compile_dirty {
            tracing::trace!(module = %module_name, "compile dirty");
        }
    }

    let start_compiling = Instant::now();

    // Use atomics for progress tracking so closures can update and report
    let current_count = AtomicI32::new(0);
    let total_count = AtomicI32::new(0);

    let compile_result = {
        let _span = info_span!("build.compile").entered();
        compile::compile(
            build_state,
            warn_error,
            &filter,
            true,
            || {
                let current = current_count.fetch_add(1, Ordering::SeqCst) + 1;
                let total = total_count.load(Ordering::SeqCst);
                reporter.report(BuildProgress::Compiling {
                    current_count: current,
                    total_count: total,
                });
            },
            |size| {
                total_count.store(size as i32, Ordering::SeqCst);
            },
            reporter,
        )
        .map_err(|e| IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
        })?
    };

    let compile_duration = start_compiling.elapsed();

    logs::finalize(&build_state.packages);
    if create_sourcedirs {
        sourcedirs::print(build_state);
    }

    reporter.report(BuildProgress::Compiled {
        compiled_count: compile_result.num_compiled_modules as i32,
        duration_seconds: default_timing.unwrap_or(compile_duration).as_secs_f64(),
    });

    if helpers::contains_ascii_characters(&compile_result.compile_warnings) {
        let _warning_span = info_span!("build.compile_warning").entered();
        reporter.report(BuildProgress::CompilerWarning(
            compile_result.compile_warnings.clone(),
        ));
    }

    if compile_result.had_errors {
        let _error_span = info_span!("build.compile_error").entered();
        // Errors have already been reported via structured events (CompilerError, CircularDependency, etc.)
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
        })
    } else {
        // Write per-package compiler metadata to `lib/bs/compiler-info.json` (idempotent)
        write_compiler_info(build_state, reporter);
        Ok(())
    }
}

// write build.ninja files in the packages after a non-incremental build
// this is necessary to bust the editor tooling cache. The editor tooling
// is watching this file.
// we don't need to do this in an incremental build because there are no file
// changes (deletes / additions)
pub fn write_build_ninja(build_state: &BuildState) {
    for package in build_state.packages.values() {
        // write empty file:
        let path = package.get_build_path().join("build.ninja");
        if let Ok(mut f) = File::create(&path) {
            let _ = f.write_all(b"");
        }
    }
}
