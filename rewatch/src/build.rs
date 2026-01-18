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
use crate::helpers::emojis::*;
use crate::helpers::{self};
use crate::project_context::ProjectContext;
use crate::{config, sourcedirs};
use ahash::AHashSet;
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
        compile::OutputMode::ToFile,
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

pub fn initialize_build(
    default_timing: Option<Duration>,
    filter: &Option<regex::Regex>,
    show_progress: bool,
    path: &Path,
    plain_output: bool,
    warn_error: Option<String>,
) -> Result<BuildCommandState> {
    let project_context = ProjectContext::new(path)?;
    let compiler = get_compiler_info(&project_context)?;

    let timing_clean_start = Instant::now();
    let packages = packages::make(filter, &project_context, show_progress)?;

    let compiler_check = verify_compiler_info(&packages, &compiler);

    if !packages::validate_packages_dependencies(&packages) {
        return Err(anyhow!("Failed to validate package dependencies"));
    }

    let mut build_state = BuildCommandState::new(project_context, packages, compiler, warn_error);
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

pub fn incremental_build(
    build_state: &mut BuildCommandState,
    default_timing: Option<Duration>,
    initial_build: bool,
    show_progress: bool,
    only_incremental: bool,
    create_sourcedirs: bool,
    plain_output: bool,
) -> Result<(), IncrementalBuildError> {
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
            CODE
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
            eprintln!("{}", &compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }
        if helpers::contains_ascii_characters(&compile_errors) {
            eprintln!("{}", &compile_errors);
        }
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            plain_output,
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
        write_compiler_info(build_state);

        Ok(())
    }
}

fn log_config_warnings(build_state: &BuildCommandState) {
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
                    config::DeprecationWarning::PackageSpecsEs6 => {
                        log_deprecated_package_specs_module("es6");
                    }
                    config::DeprecationWarning::PackageSpecsEs6Global => {
                        log_deprecated_package_specs_module("es6-global");
                    }
                },
            );

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

fn log_deprecated_config_field(package_name: &str, field_name: &str, new_field_name: &str) {
    let warning = format!(
        "The field '{field_name}' found in the package config of '{package_name}' is deprecated and will be removed in a future version.\n\
        Use '{new_field_name}' instead."
    );
    eprintln!("\n{}", style(warning).yellow());
}

fn log_deprecated_package_specs_module(module_name: &str) {
    let warning = format!("deprecated: Option \"{module_name}\" is deprecated. Use \"esmodule\" instead.");
    eprintln!("\n{}", style(warning).yellow());
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

/// Compile a single ReScript file and return its JavaScript output.
///
/// This function performs a targeted one-shot compilation:
/// 1. Initializes build state (reusing cached artifacts from previous builds)
/// 2. Finds the target module from the file path
/// 3. Calculates the dependency closure (all transitive dependencies)
/// 4. Marks dependencies (excluding target) as dirty for compilation
/// 5. Runs incremental build to compile dependencies (ensures .cmi files exist)
/// 6. Compiles target file directly to stdout (no file write)
///
/// # Workflow
/// Unlike the watch mode which expands UPWARD to dependents when a file changes,
/// this expands DOWNWARD to dependencies to ensure everything needed is compiled.
/// The target file itself is compiled separately with output to stdout.
///
/// # Example
/// If compiling `App.res` which imports `Component.res` which imports `Utils.res`:
/// - Dependency closure: {Utils, Component, App}
/// - Dependencies compiled to disk: Utils, Component (for .cmi files)
/// - Target compiled to stdout: App
///
/// # Errors
/// Returns error if:
/// - File doesn't exist or isn't part of the project
/// - Compilation fails (parse errors, type errors, etc.)
pub fn compile_one(
    target_file: &Path,
    project_root: &Path,
    plain_output: bool,
    warn_error: Option<String>,
    module_format: Option<String>,
) -> Result<String> {
    // Step 1: Initialize build state
    // This leverages any existing .ast/.cmi files from previous builds
    let mut build_state = initialize_build(
        None,
        &None, // no filter
        false, // no progress output (keep stderr clean)
        project_root,
        plain_output,
        warn_error.clone(),
    )?;

    // Determine the module format for output
    let root_config = build_state.get_root_config();
    let package_specs = root_config.get_package_specs();
    let module_format_str = get_module_format(&package_specs, &module_format)?;

    // Step 2: Find target module from file path
    let target_module_name = find_module_for_file(&build_state, target_file)
        .ok_or_else(|| anyhow!("File not found in project: {}", target_file.display()))?;

    // Step 3: Mark only the target file as parse_dirty
    // This ensures we parse the latest version of the target file
    if let Some(module) = build_state.modules.get_mut(&target_module_name)
        && let SourceType::SourceFile(source_file) = &mut module.source_type
    {
        source_file.implementation.parse_dirty = true;
        if let Some(interface) = &mut source_file.interface {
            interface.parse_dirty = true;
        }
    }

    // Step 4: Get dependency closure (downward traversal)
    // Unlike compile universe (upward to dependents), we need all dependencies
    let dependency_closure = get_dependency_closure(&target_module_name, &build_state);

    // Step 5: Mark dependencies (excluding target) as compile_dirty
    // The target will be compiled separately to stdout
    for module_name in &dependency_closure {
        if module_name != &target_module_name
            && let Some(module) = build_state.modules.get_mut(module_name)
        {
            module.compile_dirty = true;
        }
    }

    // Step 6: Run incremental build for dependencies only
    // This ensures all .cmi files exist for the target's imports
    incremental_build(
        &mut build_state,
        None,
        false, // not initial build
        false, // no progress output
        true,  // only incremental (no cleanup step)
        false, // no sourcedirs
        plain_output,
    )
    .map_err(|e| anyhow!("Compilation failed: {}", e))?;

    // Step 7: Compile the target file to stdout
    let module = build_state
        .get_module(&target_module_name)
        .ok_or_else(|| anyhow!("Module not found: {}", target_module_name))?;

    let package = build_state
        .get_package(&module.package_name)
        .ok_or_else(|| anyhow!("Package not found: {}", module.package_name))?;

    // Get the AST path for the target module
    let ast_path = get_ast_path(&build_state, &target_module_name)?;

    compile::compile_file_to_stdout(
        package,
        &ast_path,
        module,
        &build_state,
        warn_error,
        &module_format_str,
    )
}

/// Determine the module format to use for stdout output.
///
/// If module_format is specified via --module-format, it must match one of the
/// configured package-specs (since dependencies are compiled to those formats).
/// If not specified, use the first package-spec's format (warn if multiple exist).
fn get_module_format(
    package_specs: &[config::PackageSpec],
    module_format: &Option<String>,
) -> Result<String> {
    if package_specs.is_empty() {
        return Err(anyhow!("No package-specs configured in rescript.json"));
    }

    match module_format {
        Some(format) => {
            // Must match a configured package-spec (dependencies are compiled to those formats)
            if package_specs.iter().any(|spec| spec.module == *format) {
                Ok(format.clone())
            } else {
                let available: Vec<&str> = package_specs.iter().map(|s| s.module.as_str()).collect();
                Err(anyhow!(
                    "Module format '{}' not found in package-specs. Available: {}",
                    format,
                    available.join(", ")
                ))
            }
        }
        None => {
            // No format specified - use first package-spec, warn if multiple exist
            if package_specs.len() > 1 {
                let available: Vec<&str> = package_specs.iter().map(|s| s.module.as_str()).collect();
                eprintln!(
                    "Warning: Multiple package-specs configured ({}). Using '{}'. \
                     Specify --module-format to choose a different one.",
                    available.join(", "),
                    package_specs[0].module
                );
            }
            Ok(package_specs[0].module.clone())
        }
    }
}

/// Find the module name for a given file path by searching through all modules.
///
/// This performs a linear search through the build state's modules to match
/// the canonical file path. Returns the module name if found.
fn find_module_for_file(build_state: &BuildCommandState, target_file: &Path) -> Option<String> {
    let canonical_target = target_file.canonicalize().ok()?;

    for (module_name, module) in &build_state.modules {
        if let SourceType::SourceFile(source_file) = &module.source_type {
            let package = build_state.packages.get(&module.package_name)?;

            // Check implementation file
            let impl_path = package.path.join(&source_file.implementation.path);
            if impl_path.canonicalize().ok().as_ref() == Some(&canonical_target) {
                return Some(module_name.clone());
            }

            // Check interface file if present
            if let Some(interface) = &source_file.interface {
                let iface_path = package.path.join(&interface.path);
                if iface_path.canonicalize().ok().as_ref() == Some(&canonical_target) {
                    return Some(module_name.clone());
                }
            }
        }
    }

    None
}

/// Calculate the transitive closure of all dependencies for a given module.
///
/// This performs a downward traversal (dependencies, not dependents):
/// - Module A depends on B and C
/// - B depends on D
/// - Result: {A, B, C, D}
///
/// This is the opposite of the "compile universe" which expands upward to dependents.
fn get_dependency_closure(module_name: &str, build_state: &BuildState) -> AHashSet<String> {
    let mut closure = AHashSet::new();
    let mut to_process = vec![module_name.to_string()];

    while let Some(current) = to_process.pop() {
        if !closure.contains(&current) {
            closure.insert(current.clone());

            if let Some(module) = build_state.get_module(&current) {
                // Add all dependencies to process queue
                for dep in &module.deps {
                    if !closure.contains(dep) {
                        to_process.push(dep.clone());
                    }
                }
            }
        }
    }

    closure
}

/// Get the path to the AST file for a module.
///
/// The AST file is generated during the parse phase and is needed for compilation.
fn get_ast_path(build_state: &BuildCommandState, module_name: &str) -> Result<PathBuf> {
    let module = build_state
        .get_module(module_name)
        .ok_or_else(|| anyhow!("Module not found: {}", module_name))?;

    let package = build_state
        .get_package(&module.package_name)
        .ok_or_else(|| anyhow!("Package not found: {}", module.package_name))?;

    if let SourceType::SourceFile(source_file) = &module.source_type {
        let source_path = &source_file.implementation.path;
        let ast_file = source_path.with_extension("ast");
        Ok(package.get_build_path().join(ast_file))
    } else {
        Err(anyhow!("Cannot get AST path for non-source module"))
    }
}
