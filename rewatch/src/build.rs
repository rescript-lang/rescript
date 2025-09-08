pub mod build_types;
pub mod clean;
pub mod compile;
pub mod deps;
pub mod logs;
pub mod namespaces;
pub mod packages;
pub mod parse;
pub mod read_compile_state;

use self::parse::parser_args;
use crate::build::compile::{mark_modules_with_deleted_deps_dirty, mark_modules_with_expired_deps_dirty};
use crate::helpers::emojis::*;
use crate::helpers::{self};
use crate::project_context::ProjectContext;
use crate::{config, sourcedirs};
use anyhow::{Result, anyhow};
use build_types::*;
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use log::log_enabled;
use serde::Serialize;
use std::ffi::OsString;
use std::fmt;
use std::fs::File;
use std::io::{Write, stdout};
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::{Duration, Instant};

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
    );

    let result = serde_json::to_string_pretty(&CompilerArgs {
        compiler_args,
        parser_args,
    })?;

    Ok(result)
}

pub fn initialize_build(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    show_progress: bool,
    path: &Path,
    build_dev_deps: bool,
    snapshot_output: bool,
) -> Result<BuildState> {
    let bsc_path = helpers::get_bsc();
    let project_context = ProjectContext::new(path)?;

    if !snapshot_output && show_progress {
        print!("{} {}Building package tree...", style("[1/7]").bold().dim(), TREE);
        let _ = stdout().flush();
    }

    let timing_package_tree = Instant::now();
    let packages = packages::make(filter, &project_context, show_progress, build_dev_deps)?;
    let timing_package_tree_elapsed = timing_package_tree.elapsed();

    if !snapshot_output && show_progress {
        println!(
            "{}{} {}Built package tree in {:.2}s",
            LINE_CLEAR,
            style("[1/7]").bold().dim(),
            TREE,
            default_timing
                .unwrap_or(timing_package_tree_elapsed)
                .as_secs_f64()
        );
    }

    if !packages::validate_packages_dependencies(&packages) {
        return Err(anyhow!("Failed to validate package dependencies"));
    }

    let timing_source_files = Instant::now();

    if !snapshot_output && show_progress {
        print!(
            "{} {}Finding source files...",
            style("[2/7]").bold().dim(),
            LOOKING_GLASS
        );
        let _ = stdout().flush();
    }

    let mut build_state = BuildState::new(project_context, packages, bsc_path);
    packages::parse_packages(&mut build_state);
    let timing_source_files_elapsed = timing_source_files.elapsed();

    if !snapshot_output && show_progress {
        println!(
            "{}{} {}Found source files in {:.2}s",
            LINE_CLEAR,
            style("[2/7]").bold().dim(),
            LOOKING_GLASS,
            default_timing
                .unwrap_or(timing_source_files_elapsed)
                .as_secs_f64()
        );

        print!(
            "{} {}Reading compile state...",
            style("[3/7]").bold().dim(),
            COMPILE_STATE
        );
        let _ = stdout().flush();
    }
    let timing_compile_state = Instant::now();
    let compile_assets_state = read_compile_state::read(&mut build_state)?;
    let timing_compile_state_elapsed = timing_compile_state.elapsed();

    if !snapshot_output && show_progress {
        println!(
            "{}{} {}Read compile state {:.2}s",
            LINE_CLEAR,
            style("[3/7]").bold().dim(),
            COMPILE_STATE,
            default_timing
                .unwrap_or(timing_compile_state_elapsed)
                .as_secs_f64()
        );

        print!(
            "{} {}Cleaning up previous build...",
            style("[4/7]").bold().dim(),
            SWEEP
        );
    }
    let timing_cleanup = Instant::now();
    let (diff_cleanup, total_cleanup) = clean::cleanup_previous_build(&mut build_state, compile_assets_state);
    let timing_cleanup_elapsed = timing_cleanup.elapsed();

    if show_progress {
        if snapshot_output {
            println!("Cleaned {diff_cleanup}/{total_cleanup}")
        } else {
            println!(
                "{}{} {}Cleaned {}/{} {:.2}s",
                LINE_CLEAR,
                style("[4/7]").bold().dim(),
                SWEEP,
                diff_cleanup,
                total_cleanup,
                default_timing.unwrap_or(timing_cleanup_elapsed).as_secs_f64()
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
    pub snapshot_output: bool,
    pub kind: IncrementalBuildErrorKind,
}

impl fmt::Display for IncrementalBuildError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IncrementalBuildErrorKind::SourceFileParseError => {
                if self.snapshot_output {
                    write!(f, "{LINE_CLEAR}  Could not parse Source Files",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Could not parse Source Files",)
                }
            }
            IncrementalBuildErrorKind::CompileError(Some(e)) => {
                if self.snapshot_output {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. Error: {e}",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. Error: {e}",)
                }
            }
            IncrementalBuildErrorKind::CompileError(None) => {
                if self.snapshot_output {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. See Errors Above",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. See Errors Above",)
                }
            }
        }
    }
}

pub fn incremental_build(
    build_state: &mut BuildState,
    default_timing: Option<Duration>,
    initial_build: bool,
    show_progress: bool,
    only_incremental: bool,
    create_sourcedirs: bool,
    snapshot_output: bool,
) -> Result<(), IncrementalBuildError> {
    logs::initialize(&build_state.packages);
    let num_dirty_modules = build_state.modules.values().filter(|m| is_dirty(m)).count() as u64;
    let pb = if !snapshot_output && show_progress {
        ProgressBar::new(num_dirty_modules)
    } else {
        ProgressBar::hidden()
    };
    let mut current_step = if only_incremental { 1 } else { 5 };
    let total_steps = if only_incremental { 3 } else { 7 };
    pb.set_style(
        ProgressStyle::with_template(&format!(
            "{} {}Parsing... {{spinner}} {{pos}}/{{len}} {{msg}}",
            format_step(current_step, total_steps),
            CODE
        ))
        .unwrap(),
    );

    let timing_ast = Instant::now();
    let result_asts = parse::generate_asts(build_state, || pb.inc(1));
    let timing_ast_elapsed = timing_ast.elapsed();

    match result_asts {
        Ok(_ast) => {
            if show_progress {
                if snapshot_output {
                    println!("Parsed {num_dirty_modules} source files")
                } else {
                    println!(
                        "{}{} {}Parsed {} source files in {:.2}s",
                        LINE_CLEAR,
                        format_step(current_step, total_steps),
                        CODE,
                        num_dirty_modules,
                        default_timing.unwrap_or(timing_ast_elapsed).as_secs_f64()
                    );
                    pb.finish();
                }
            }
        }
        Err(err) => {
            logs::finalize(&build_state.packages);

            if !snapshot_output && show_progress {
                println!(
                    "{}{} {}Error parsing source files in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    CROSS,
                    default_timing.unwrap_or(timing_ast_elapsed).as_secs_f64()
                );
                pb.finish();
            }

            println!("{}", &err);
            return Err(IncrementalBuildError {
                kind: IncrementalBuildErrorKind::SourceFileParseError,
                snapshot_output,
            });
        }
    }
    let timing_deps = Instant::now();
    deps::get_deps(build_state, &build_state.deleted_modules.to_owned());
    let timing_deps_elapsed = timing_deps.elapsed();
    current_step += 1;

    if !snapshot_output && show_progress {
        println!(
            "{}{} {}Collected deps in {:.2}s",
            LINE_CLEAR,
            format_step(current_step, total_steps),
            DEPS,
            default_timing.unwrap_or(timing_deps_elapsed).as_secs_f64()
        );
    }

    mark_modules_with_expired_deps_dirty(build_state);
    mark_modules_with_deleted_deps_dirty(build_state);
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
    let pb = if !snapshot_output && show_progress {
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
        snapshot_output,
    })?;

    let compile_duration = start_compiling.elapsed();

    logs::finalize(&build_state.packages);
    if create_sourcedirs {
        sourcedirs::print(build_state);
    }
    pb.finish();
    if !compile_errors.is_empty() {
        if show_progress {
            if snapshot_output {
                println!("Compiled {num_compiled_modules} modules")
            } else {
                println!(
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
            println!("{}", &compile_warnings);
        }
        if initial_build {
            log_deprecations(build_state);
        }
        if helpers::contains_ascii_characters(&compile_errors) {
            println!("{}", &compile_errors);
        }
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            snapshot_output,
        })
    } else {
        if show_progress {
            if snapshot_output {
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
            println!("{}", &compile_warnings);
        }
        if initial_build {
            log_deprecations(build_state);
        }

        Ok(())
    }
}

fn log_deprecations(build_state: &BuildState) {
    build_state.packages.iter().for_each(|(_, package)| {
        // Only warn for local dependencies, not external packages
        if package.is_local_dep {
            package.config.get_deprecations().iter().for_each(
                |deprecation_warning| match deprecation_warning {
                    config::DeprecationWarning::BsDependencies => {
                        log_deprecated_config_field(&package.name, "bs-dependencies", "dependencies");
                    }
                    config::DeprecationWarning::BsDevDependencies => {
                        log_deprecated_config_field(&package.name, "bs-dev-dependencies", "dev-dependencies");
                    }
                    config::DeprecationWarning::BscFlags => {
                        log_deprecated_config_field(&package.name, "bsc-flags", "compiler-flags");
                    }
                },
            );
        }
    });
}

fn log_deprecated_config_field(package_name: &str, field_name: &str, new_field_name: &str) {
    let warning = format!(
        "The field '{field_name}' found in the package config of '{package_name}' is deprecated and will be removed in a future version.\n\
        Use '{new_field_name}' instead."
    );
    println!("\n{}", style(warning).yellow());
}

// write build.ninja files in the packages after a non-incremental build
// this is necessary to bust the editor tooling cache. The editor tooling
// is watching this file.
// we don't need to do this in an incremental build because there are no file
// changes (deletes / additions)
pub fn write_build_ninja(build_state: &BuildState) {
    for package in build_state.packages.values() {
        // write empty file:
        let mut f = File::create(package.get_build_path().join("build.ninja")).expect("Unable to write file");
        f.write_all(b"").expect("unable to write to ninja file");
    }
}

pub fn build(
    filter: &Option<regex::Regex>,
    path: &Path,
    show_progress: bool,
    no_timing: bool,
    create_sourcedirs: bool,
    build_dev_deps: bool,
    snapshot_output: bool,
) -> Result<BuildState> {
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
        build_dev_deps,
        snapshot_output,
    )
    .map_err(|e| anyhow!("Could not initialize build. Error: {e}"))?;

    match incremental_build(
        &mut build_state,
        default_timing,
        true,
        show_progress,
        false,
        create_sourcedirs,
        snapshot_output,
    ) {
        Ok(_) => {
            if !snapshot_output && show_progress {
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

pub fn pass_through_legacy(mut args: Vec<OsString>) -> i32 {
    let project_root = helpers::get_abs_path(Path::new("."));
    let project_context = ProjectContext::new(&project_root).unwrap();
    let rescript_legacy_path = helpers::get_rescript_legacy(&project_context);

    args.insert(0, rescript_legacy_path.into());
    let status = std::process::Command::new("node")
        .args(args)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status();

    match status {
        Ok(s) => s.code().unwrap_or(0),
        Err(err) => {
            eprintln!("Error running the legacy build system: {err}");
            1
        }
    }
}
