pub mod build_types;
pub mod clean;
pub mod compile;
pub mod compiler_info;
pub mod deps;
pub mod diagnostics;
pub mod logs;
pub mod namespaces;
pub mod packages;
pub mod parse;
pub mod read_compile_state;

use crate::build::compile::{mark_modules_with_deleted_deps_dirty, mark_modules_with_expired_deps_dirty};
use crate::build::compiler_info::{CompilerCheckResult, verify_compiler_info, write_compiler_info};
use crate::helpers::emojis::*;
use crate::helpers::{self};
use crate::project_context::ProjectContext;
use crate::sourcedirs;
use ahash::AHashMap;
use anyhow::{Context, Result, anyhow};
use build_types::*;
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use log::log_enabled;
use std::fmt;
use std::fs::File;
use std::io::Write;
use std::path::Path;
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

/// Prepare a build from already-discovered packages.
///
/// Takes a `ProjectContext` and a fully-populated package map (with source files),
/// then performs compiler validation, dependency checking, cleanup of stale artifacts,
/// and directory creation — everything needed before `incremental_build`.
#[instrument(name = "prepare_build", skip_all)]
pub fn prepare_build(
    project_context: ProjectContext,
    packages: AHashMap<String, packages::Package>,
    default_timing: Option<Duration>,
    show_progress: bool,
    plain_output: bool,
    warn_error: Option<String>,
    build_profile: BuildProfile,
) -> Result<BuildCommandState> {
    let compiler = get_compiler_info(&project_context)?;

    let timing_clean_start = Instant::now();

    let compiler_check = verify_compiler_info(&packages, &compiler, build_profile);

    if !packages::validate_packages_dependencies(&packages) {
        return Err(anyhow!("Failed to validate package dependencies"));
    }

    let mut build_state = BuildCommandState::new(project_context, packages, compiler, warn_error);
    packages::parse_packages(&mut build_state, build_profile)?;

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

#[instrument(name = "initialize_build", skip_all)]
pub fn initialize_build(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    show_progress: bool,
    path: &Path,
    plain_output: bool,
    warn_error: Option<String>,
) -> Result<BuildCommandState> {
    let project_context = ProjectContext::new(path)?;
    let packages = packages::make(filter, &project_context, show_progress)?;

    prepare_build(
        project_context,
        packages,
        default_timing,
        show_progress,
        plain_output,
        warn_error,
        BuildProfile::Standard,
    )
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
    pub diagnostics: Vec<diagnostics::BscDiagnostic>,
}

#[derive(Debug, Clone)]
pub struct IncrementalBuildResult {
    pub diagnostics: Vec<diagnostics::BscDiagnostic>,
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

#[allow(clippy::too_many_arguments)]
#[instrument(name = "incremental_build", skip_all, fields(module_count = build_state.modules.len()))]
pub fn incremental_build(
    build_state: &mut BuildCommandState,
    build_profile: BuildProfile,
    default_timing: Option<Duration>,
    initial_build: bool,
    show_progress: bool,
    only_incremental: bool,
    create_sourcedirs: bool,
    plain_output: bool,
) -> Result<IncrementalBuildResult, IncrementalBuildError> {
    if build_profile == BuildProfile::Standard {
        logs::initialize(&build_state.packages);
    }
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
            CODE
        ))
        .unwrap(),
    );

    let timing_parse_start = Instant::now();
    let timing_ast = Instant::now();
    let result_asts = parse::generate_asts(build_state, build_profile, || pb.inc(1));
    let timing_ast_elapsed = timing_ast.elapsed();

    let parse_warnings = match result_asts {
        Ok(warnings) => {
            pb.finish();
            warnings
        }
        Err(err) => {
            let _error_span = info_span!("build.parse_error").entered();
            if build_profile == BuildProfile::Standard {
                logs::finalize(&build_state.packages);
            }

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

            let err_str = err.to_string();
            eprintln!("{}", &err_str);
            let parse_diagnostics = diagnostics::parse_compiler_output(&err_str);
            return Err(IncrementalBuildError {
                kind: IncrementalBuildErrorKind::SourceFileParseError,
                plain_output,
                diagnostics: parse_diagnostics,
            });
        }
    };
    let deleted_modules = build_state.deleted_modules.clone();
    deps::get_deps(build_state, &deleted_modules, build_profile);
    let timing_parse_total = timing_parse_start.elapsed();

    if show_progress {
        if plain_output {
            println!("Parsed {num_dirty_modules} source files")
        } else {
            println!(
                "{}{} {}Parsed {} source files in {:.2}s",
                LINE_CLEAR,
                format_step(current_step, total_steps),
                CODE,
                num_dirty_modules,
                default_timing.unwrap_or(timing_parse_total).as_secs_f64()
            );
        }
    }
    if helpers::contains_ascii_characters(&parse_warnings) {
        eprintln!("{}", &parse_warnings);
    }

    mark_modules_with_expired_deps_dirty(build_state);
    mark_modules_with_deleted_deps_dirty(&mut build_state.build_state);
    current_step += 1;

    //print all the modules that need compilation
    if log_enabled!(log::Level::Trace) {
        let target_stage = CompilationStage::target_for(build_profile);
        for (module_name, module) in build_state.modules.iter() {
            if module.compilation_stage.needs_compile(target_stage) {
                println!(
                    "needs compile: {module_name} (stage: {:?})",
                    module.compilation_stage
                );
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
        build_profile,
        show_progress,
        || pb.inc(1),
        |size| pb.set_length(size),
    )
    .map_err(|e| IncrementalBuildError {
        kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
        plain_output,
        diagnostics: vec![],
    })?;

    let compile_duration = start_compiling.elapsed();

    if build_profile == BuildProfile::Standard {
        logs::finalize(&build_state.packages);
    }
    if create_sourcedirs && build_profile == BuildProfile::Standard {
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
        if helpers::contains_ascii_characters(&compile_warnings) {
            let _warning_span = info_span!("build.compile_warning").entered();
            eprintln!("{}", &compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }
        if helpers::contains_ascii_characters(&compile_errors) {
            eprintln!("{}", &compile_errors);
        }
        let mut all_output = String::new();
        if helpers::contains_ascii_characters(&parse_warnings) {
            all_output.push_str(&parse_warnings);
        }
        all_output.push_str(&compile_warnings);
        all_output.push_str(&compile_errors);
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            plain_output,
            diagnostics: diagnostics::parse_compiler_output(&all_output),
        })
    } else {
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

        if helpers::contains_ascii_characters(&compile_warnings) {
            eprintln!("{}", &compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }

        // Write per-package compiler metadata to `lib/bs/compiler-info.json` (idempotent)
        write_compiler_info(build_state, build_profile);

        let mut all_output = String::new();
        if helpers::contains_ascii_characters(&parse_warnings) {
            all_output.push_str(&parse_warnings);
        }
        all_output.push_str(&compile_warnings);
        Ok(IncrementalBuildResult {
            diagnostics: diagnostics::parse_compiler_output(&all_output),
        })
    }
}

fn log_config_warnings(build_state: &BuildCommandState) {
    build_state.packages.iter().for_each(|(_, package)| {
        // Only warn for local dependencies, not external packages
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
pub fn build(
    filter: &Option<regex::Regex>,
    path: &Path,
    show_progress: bool,
    no_timing: bool,
    create_sourcedirs: bool,
    plain_output: bool,
    warn_error: Option<String>,
) -> Result<BuildCommandState> {
    let default_timing: Option<std::time::Duration> = if no_timing {
        Some(std::time::Duration::new(0.0 as u64, 0.0 as u32))
    } else {
        None
    };
    let timing_total = Instant::now();
    let mut build_state = initialize_build(
        default_timing,
        filter,
        show_progress,
        path,
        plain_output,
        warn_error,
    )
    .with_context(|| "Could not initialize build")?;

    match incremental_build(
        &mut build_state,
        BuildProfile::Standard,
        default_timing,
        true,
        show_progress,
        false,
        create_sourcedirs,
        plain_output,
    ) {
        Ok(_) => {
            if !plain_output && show_progress {
                let timing_total_elapsed = timing_total.elapsed();
                println!(
                    "\n{}{}Finished Compilation in {:.2}s",
                    LINE_CLEAR,
                    SPARKLES,
                    default_timing.unwrap_or(timing_total_elapsed).as_secs_f64()
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
}
