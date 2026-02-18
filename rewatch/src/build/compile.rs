#![allow(clippy::too_many_arguments)]

mod dependency_cycle;

use super::build_types::*;
use super::logs;
use super::packages;
use crate::config;
use crate::config::Config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::project_context::ProjectContext;
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use console::style;
use log::{debug, info, trace, warn};
use rayon::prelude::*;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;
use std::time::SystemTime;
use tracing::info_span;

/// The result of compiling a single file (implementation or interface).
enum CompileFileOutcome {
    /// Compilation succeeded with no warnings.
    Success,
    /// Compilation succeeded but produced warnings.
    Warning(String),
    /// Compilation failed.
    Error(String),
}

impl CompileFileOutcome {
    fn is_error(&self) -> bool {
        matches!(self, CompileFileOutcome::Error(_))
    }
}

impl From<Result<Option<String>>> for CompileFileOutcome {
    fn from(result: Result<Option<String>>) -> Self {
        match result {
            Ok(None) => CompileFileOutcome::Success,
            Ok(Some(warning)) => CompileFileOutcome::Warning(warning),
            Err(err) => CompileFileOutcome::Error(err.to_string()),
        }
    }
}

/// The result of processing one module in a compile wave.
struct CompileModuleResult {
    /// The name of the module that was processed.
    module_name: String,
    /// Result of compiling the implementation file.
    implementation: CompileFileOutcome,
    /// Result of compiling the interface file, if one exists.
    interface: Option<CompileFileOutcome>,
    /// Whether the .cmi file was unchanged after compilation.
    is_clean_cmi: bool,
    /// Whether the module was actually sent to the compiler.
    /// False for skipped modules and MlMap entries.
    was_compiled: bool,
}

/// Execute js-post-build command for a compiled JavaScript file.
/// The command runs in the directory containing the rescript.json that defines it.
/// The absolute path to the JS file is passed as an argument.
fn execute_post_build_command(cmd: &str, js_file_path: &Path, working_dir: &Path) -> Result<()> {
    let full_command = format!("{} {}", cmd, js_file_path.display());

    let span = info_span!(
        "build.js_post_build",
        command = %cmd,
        js_file = %js_file_path.display(),
        exit_code = tracing::field::Empty,
    )
    .entered();

    debug!(
        "Executing js-post-build: {} (in {})",
        full_command,
        working_dir.display()
    );

    let output = if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args(["/C", &full_command])
            .current_dir(working_dir)
            .output()
    } else {
        Command::new("sh")
            .args(["-c", &full_command])
            .current_dir(working_dir)
            .output()
    };

    match output {
        Ok(output) => {
            let exit_code = output.status.code().unwrap_or(-1);
            span.record("exit_code", exit_code);

            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);

            if !stdout.is_empty() {
                info!("{}", stdout.trim());
            }
            if !stderr.is_empty() {
                warn!("{}", stderr.trim());
            }

            if !output.status.success() {
                tracing::error!(
                    exit_code,
                    "js-post-build command failed for {}",
                    js_file_path.display()
                );
                Err(anyhow!(
                    "js-post-build command failed for {}",
                    js_file_path.display()
                ))
            } else {
                Ok(())
            }
        }
        Err(e) => Err(anyhow!("Failed to execute js-post-build command: {}", e)),
    }
}

/// Process modules in dependency-ordered parallel waves.
///
/// Repeatedly finds modules whose in-universe dependencies have all been
/// processed, then compiles or typechecks them in parallel via `rayon`.
/// Modules whose `.cmi` output is unchanged are marked "clean" so downstream
/// dependents can skip reprocessing.
///
pub fn process_in_waves(
    build_state: &mut BuildCommandState,
    compile_params: &CompileParams,
    inc: impl Fn() + std::marker::Sync,
    set_length: impl Fn(u64),
) -> anyhow::Result<ProcessResult> {
    let show_progress = compile_params.show_progress;
    let mode = compile_params.mode;
    let _compile_span = if mode.emits_js() {
        info_span!("build.compile").entered()
    } else {
        info_span!("build.typecheck").entered()
    };
    let mut compiled_modules = AHashSet::<String>::new();

    // Modules whose .cmi did not change after compilation.
    let mut clean_modules = AHashSet::<String>::new();
    // Modules that compiled successfully — stage/timestamps updated post-loop.
    let mut successfully_compiled = AHashSet::<String>::new();
    // Merged compile warning text per module (impl + interface warnings concatenated).
    let mut module_compile_warnings = AHashMap::<String, String>::new();
    // Modules that were compiled but failed — set to CompileError post-loop.
    let mut errored_modules = AHashSet::<String>::new();

    let mut loop_count = 0;
    let mut files_total_count = compiled_modules.len();
    let mut files_current_loop_count;
    let mut compile_errors = "".to_string();
    let mut compile_warnings = "".to_string();
    let mut num_compiled_modules = 0;
    let mut sorted_modules = build_state.module_names.iter().collect::<Vec<&String>>();
    sorted_modules.sort();

    let compile_universe_count = compile_params.modules.len();
    set_length(compile_universe_count as u64);

    // start off with all modules that have no deps in this compile universe
    let mut in_progress_modules = compile_params
        .modules
        .iter()
        .filter(|module_name| {
            let module = build_state.get_module(module_name).unwrap();
            module.deps().intersection(&compile_params.modules).count() == 0
        })
        .map(|module_name| module_name.to_string())
        .collect::<AHashSet<String>>();

    loop {
        files_current_loop_count = 0;
        loop_count += 1;

        trace!(
            "Compiled: {} out of {}. Compile loop: {}",
            files_total_count,
            compile_params.modules.len(),
            loop_count,
        );

        let wave_span = if mode.emits_js() {
            info_span!(
                "build.compile_wave",
                wave = loop_count,
                file_count = in_progress_modules.len(),
            )
        } else {
            info_span!(
                "build.typecheck_wave",
                wave = loop_count,
                file_count = in_progress_modules.len(),
            )
        };
        let _wave_entered = wave_span.enter();

        let current_in_progres_modules = in_progress_modules.clone();

        let results = current_in_progres_modules
            .par_iter()
            .filter_map(|module_name| {
                let module = build_state.get_module(module_name).unwrap();
                let package = build_state
                    .get_package(module.package_name())
                    .expect("Package not found");
                // Dependencies of this module that are part of the compile universe.
                let in_universe_deps: Vec<&String> =
                    module.deps().intersection(&compile_params.modules).collect();

                // all dependencies that we care about are compiled
                if in_universe_deps
                    .iter()
                    .all(|dep| compiled_modules.contains(*dep))
                {
                    if !compile_params.filter.needs_compile(module_name)
                        && !module.needs_compile_for_mode(mode)
                        && in_universe_deps
                            .iter()
                            .all(|dep| clean_modules.contains(*dep))
                    {
                        // This module doesn't need compilation itself, is already
                        // at the target stage, and all its in-universe
                        // dependencies had unchanged output (.cmi). Skip it.
                        return Some(CompileModuleResult {
                            module_name: module_name.to_string(),
                            implementation: CompileFileOutcome::Success,
                            interface: Some(CompileFileOutcome::Success),
                            is_clean_cmi: true,
                            was_compiled: false,
                        });
                    }
                    match module {
                        Module::MlMap(_) => {
                            // The mlmap is compiled during AST generation,
                            // so we just mark it as processed here.
                            Some(CompileModuleResult {
                                module_name: package.namespace.to_suffix().unwrap(),
                                implementation: CompileFileOutcome::Success,
                                interface: Some(CompileFileOutcome::Success),
                                is_clean_cmi: false,
                                was_compiled: false,
                            })
                        }
                        Module::SourceFile(source_file_module) => {
                            let source_file = source_file_module.source_file.clone();
                            let root_config = build_state.get_root_config();
                            let first_spec = root_config.get_package_specs().into_iter().next();
                            let suffix = first_spec.as_ref().map(|s| root_config.get_suffix(s)).unwrap_or_default();
                            let module_system = first_spec.as_ref().map(|s| s.module.as_str()).unwrap_or("esmodule");
                            let namespace = package.namespace.to_suffix().unwrap_or_default();
                            let _file_span = if mode.emits_js() {
                                info_span!(parent: &wave_span, "build.compile_file", module = %module_name, package = %package.name, suffix, module_system, namespace).entered()
                            } else {
                                info_span!(parent: &wave_span, "build.typecheck_file", module = %module_name, package = %package.name, namespace).entered()
                            };

                            let cmi_path = helpers::get_compiler_asset_in(
                                &package.get_ocaml_build_path_for_output(compile_params.output),
                                &package.namespace,
                                &source_file.implementation.path,
                                "cmi",
                            );

                            let cmi_digest = helpers::compute_file_hash(Path::new(&cmi_path));

                            let package = build_state
                                .get_package(module.package_name())
                                .expect("Package not found");

                            let interface_result = match source_file.interface.to_owned() {
                                Some(Interface { path, .. }) => {
                                    let result = compile_file(
                                        package,
                                        &helpers::get_ast_path(&path),
                                        module,
                                        true,
                                        build_state,
                                        build_state.get_warn_error_override(),
                                        compile_params.output,
                                        mode,
                                    );
                                    Some(result)
                                }
                                _ => None,
                            };
                            let result = compile_file(
                                package,
                                &helpers::get_ast_path(&source_file.implementation.path),
                                module,
                                false,
                                build_state,
                                build_state.get_warn_error_override(),
                                compile_params.output,
                                mode,
                            );
                            let cmi_digest_after = helpers::compute_file_hash(Path::new(&cmi_path));

                            // we want to compare both the hash of interface and the implementation
                            // compile assets to verify that nothing changed. We also need to checke the interface
                            // because we can include MyModule, so the modules that depend on this module might
                            // change when this modules interface does not change, but the implementation does
                            let is_clean_cmi = match (cmi_digest, cmi_digest_after) {
                                (Some(cmi_digest), Some(cmi_digest_after)) => {
                                    cmi_digest.eq(&cmi_digest_after)
                                }

                                _ => false,
                            };

                            Some(CompileModuleResult {
                                module_name: module_name.to_string(),
                                implementation: result.into(),
                                interface: interface_result.map(|r| r.into()),
                                is_clean_cmi,
                                was_compiled: true,
                            })
                        }
                    }
                } else {
                    None
                }
                .inspect(|_res| {
                    if show_progress {
                        inc();
                    }
                })
            })
            .collect::<Vec<_>>();

        for entry in results.iter() {
            in_progress_modules.remove(&entry.module_name);

            if entry.was_compiled {
                num_compiled_modules += 1;
            }

            files_current_loop_count += 1;
            compiled_modules.insert(entry.module_name.clone());

            if entry.is_clean_cmi {
                clean_modules.insert(entry.module_name.clone());
            }

            let module_dependents = build_state
                .get_module(&entry.module_name)
                .unwrap()
                .dependents()
                .clone();

            for dep in module_dependents.iter() {
                // For scoped builds, only process dependents within the universe.
                if compile_params.scoped && !compile_params.modules.contains(dep) {
                    continue;
                }
                if !compiled_modules.contains(dep) {
                    in_progress_modules.insert(dep.to_string());
                }
            }

            let package_name = {
                let module = build_state
                    .build_state
                    .modules
                    .get(&entry.module_name)
                    .ok_or(anyhow!("Module not found"))?;
                module.package_name().to_owned()
            };

            let package = build_state
                .build_state
                .packages
                .get(&package_name)
                .ok_or(anyhow!("Package name not found"))?;

            // Process results and update module state
            let (compile_warning, compile_error, interface_warning, interface_error) = {
                let module = build_state
                    .build_state
                    .modules
                    .get_mut(&entry.module_name)
                    .ok_or(anyhow!("Module not found"))?;

                let (compile_warning, compile_error) = match module {
                    Module::MlMap(mlmap) => {
                        mlmap.parse_dirty = false;
                        (None, None)
                    }
                    Module::SourceFile(_) => match &entry.implementation {
                        CompileFileOutcome::Warning(warning) => (Some(warning.clone()), None),
                        CompileFileOutcome::Success => (None, None),
                        CompileFileOutcome::Error(error) => (None, Some(error.clone())),
                    },
                };

                let (interface_warning, interface_error) = match &entry.interface {
                    Some(CompileFileOutcome::Warning(warning)) => (Some(warning.clone()), None),
                    Some(CompileFileOutcome::Success) => (None, None),
                    Some(CompileFileOutcome::Error(error)) => (None, Some(error.clone())),
                    None => (None, None),
                };

                if !entry.implementation.is_error() && entry.interface.as_ref().is_none_or(|r| !r.is_error())
                {
                    successfully_compiled.insert(entry.module_name.clone());
                } else if entry.was_compiled {
                    errored_modules.insert(entry.module_name.clone());
                }

                (compile_warning, compile_error, interface_warning, interface_error)
            };

            // Handle logging outside the mutable borrow
            if let Some(ref warning) = compile_warning {
                logs::append(package, warning);
                compile_warnings.push_str(warning);
            }
            if let Some(error) = compile_error {
                logs::append(package, &error);
                compile_errors.push_str(&error);
            }
            if let Some(ref warning) = interface_warning {
                logs::append(package, warning);
                compile_warnings.push_str(warning);
            }
            if let Some(error) = interface_error {
                logs::append(package, &error);
                compile_errors.push_str(&error);
            }

            // Merge impl + interface warnings and store per-module for stage construction
            let merged = match (compile_warning, interface_warning) {
                (Some(w1), Some(w2)) => Some(format!("{}{}", w1, w2)),
                (Some(w), None) | (None, Some(w)) => Some(w),
                (None, None) => None,
            };
            if let Some(text) = merged {
                module_compile_warnings.insert(entry.module_name.clone(), text);
            }
        }

        files_total_count += files_current_loop_count;

        if files_total_count == compile_universe_count {
            break;
        }
        if in_progress_modules.is_empty() || in_progress_modules.eq(&current_in_progres_modules) {
            // find the dependency cycle
            let cycle = dependency_cycle::find(
                &compile_params
                    .modules
                    .iter()
                    .map(|s| (s, build_state.get_module(s).unwrap()))
                    .collect::<Vec<(&String, &Module)>>(),
            );

            let guidance = "Possible solutions:\n- Extract shared code into a new module both depend on.\n";
            let message = format!(
                "\n{}\n{}\n{}",
                style("Can't continue... Found a circular dependency in your code:").red(),
                dependency_cycle::format(&cycle, build_state),
                guidance
            );

            // Append the error to the logs of all packages involved in the cycle,
            // so editor tooling can surface it from .compiler.log
            let mut touched_packages = AHashSet::<String>::new();
            for module_name in cycle.iter() {
                if let Some(module) = build_state.get_module(module_name)
                    && touched_packages.insert(module.package_name().to_owned())
                    && let Some(package) = build_state.get_package(module.package_name())
                {
                    logs::append(package, &message);
                }
            }

            compile_errors.push_str(&message)
        }
        if !compile_errors.is_empty() {
            break;
        };
    }

    // Update compilation stage and timestamps for all modules that compiled
    // successfully. This runs even when the loop broke early due to errors,
    // so modules that succeeded before the error are not unnecessarily
    // recompiled in the next build cycle.
    let now = SystemTime::now();
    for name in &successfully_compiled {
        let module = match build_state.build_state.modules.get(name) {
            Some(m) => m,
            None => continue,
        };

        // MlMap modules have no compilation stage to update — skip them.
        let Module::SourceFile(sf) = module else {
            continue;
        };

        let impl_path = sf.source_file.implementation.path.clone();
        let package_name = sf.package_name.clone();
        let prev_stage = sf.compilation_stage();

        let pkg = build_state.build_state.packages.get(&package_name).unwrap();
        let ocaml_path = pkg.get_ocaml_build_path_for_output(compile_params.output);

        // Get source + ast hashes and parse warnings from previous stage, or compute them
        let implementation_parse_warnings = prev_stage.implementation_parse_warnings().map(str::to_owned);
        let interface_parse_warnings = prev_stage.interface_parse_warnings().map(str::to_owned);
        let (implementation_source_hash, implementation_ast_hash, interface_source_hash, interface_ast_hash) =
            match prev_stage {
                CompilationStage::Parsed {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::CompileError {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::DependencyDirty {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::TypeChecked {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::Built {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                } => (
                    *implementation_source_hash,
                    *implementation_ast_hash,
                    *interface_source_hash,
                    *interface_ast_hash,
                ),
                CompilationStage::SourceDirty | CompilationStage::ParseError => {
                    // Fallback: compute from disk
                    let build_path = pkg.get_build_path_for_output(compile_params.output);
                    let sh = helpers::compute_file_hash(&pkg.path.join(&impl_path));
                    let ah = helpers::compute_file_hash(&build_path.join(helpers::get_ast_path(&impl_path)));
                    match (sh, ah) {
                        (Some(s), Some(a)) => (s, a, None, None),
                        _ => continue,
                    }
                }
            };

        let cmi_hash = helpers::compute_file_hash(&helpers::get_compiler_asset_in(
            &ocaml_path,
            &pkg.namespace,
            &impl_path,
            "cmi",
        ));
        let cmt_hash = helpers::compute_file_hash(&helpers::get_compiler_asset_in(
            &ocaml_path,
            &pkg.namespace,
            &impl_path,
            "cmt",
        ));

        if let Some(Module::SourceFile(sf)) = build_state.build_state.modules.get_mut(name) {
            let compile_warnings = module_compile_warnings.remove(name);
            match (mode.emits_js(), cmi_hash, cmt_hash) {
                (true, Some(cmi), Some(cmt)) => {
                    let cmj_hash = helpers::compute_file_hash(&helpers::get_compiler_asset_in(
                        &ocaml_path,
                        &pkg.namespace,
                        &impl_path,
                        "cmj",
                    ));
                    if let Some(cmj) = cmj_hash {
                        sf.set_compilation_stage(CompilationStage::TypeChecked {
                            implementation_source_hash,
                            implementation_ast_hash,
                            interface_source_hash,
                            interface_ast_hash,
                            cmi_hash: cmi,
                            cmt_hash: cmt,
                            compiled_at: now,
                            implementation_parse_warnings: None,
                            interface_parse_warnings: None,
                            compile_warnings: None,
                        });
                        sf.set_compilation_stage(CompilationStage::Built {
                            implementation_source_hash,
                            implementation_ast_hash,
                            interface_source_hash,
                            interface_ast_hash,
                            cmi_hash: cmi,
                            cmt_hash: cmt,
                            cmj_hash: cmj,
                            compiled_at: now,
                            implementation_parse_warnings,
                            interface_parse_warnings,
                            compile_warnings,
                        });
                    } else {
                        sf.set_compilation_stage(CompilationStage::TypeChecked {
                            implementation_source_hash,
                            implementation_ast_hash,
                            interface_source_hash,
                            interface_ast_hash,
                            cmi_hash: cmi,
                            cmt_hash: cmt,
                            compiled_at: now,
                            implementation_parse_warnings,
                            interface_parse_warnings,
                            compile_warnings,
                        });
                    }
                }
                (false, Some(cmi), Some(cmt)) => {
                    sf.set_compilation_stage(CompilationStage::TypeChecked {
                        implementation_source_hash,
                        implementation_ast_hash,
                        interface_source_hash,
                        interface_ast_hash,
                        cmi_hash: cmi,
                        cmt_hash: cmt,
                        compiled_at: now,
                        implementation_parse_warnings,
                        interface_parse_warnings,
                        compile_warnings,
                    });
                }
                _ => {
                    sf.set_compilation_stage(CompilationStage::SourceDirty);
                }
            }
        }
    }

    // Mark modules that failed compilation as CompileError, preserving
    // their AST hashes so they can be recompiled when the error is resolved.
    for name in &errored_modules {
        if let Some(Module::SourceFile(sf)) = build_state.build_state.modules.get(name) {
            let prev_stage = sf.compilation_stage();
            let implementation_parse_warnings = prev_stage.implementation_parse_warnings().map(str::to_owned);
            let interface_parse_warnings = prev_stage.interface_parse_warnings().map(str::to_owned);
            // Preserve FullCompile if the module was previously at
            // CompileError(FullCompile) — a TypecheckOnly pass must not
            // downgrade the user's intent to have JS emitted.
            let effective_mode = match prev_stage {
                CompilationStage::CompileError {
                    compile_mode: CompileMode::FullCompile,
                    ..
                } => CompileMode::FullCompile,
                _ => mode,
            };
            let hashes = match prev_stage {
                CompilationStage::Parsed {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::CompileError {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::DependencyDirty {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::TypeChecked {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                }
                | CompilationStage::Built {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    ..
                } => Some((
                    *implementation_source_hash,
                    *implementation_ast_hash,
                    *interface_source_hash,
                    *interface_ast_hash,
                )),
                CompilationStage::SourceDirty | CompilationStage::ParseError => None,
            };
            if let Some((
                implementation_source_hash,
                implementation_ast_hash,
                interface_source_hash,
                interface_ast_hash,
            )) = hashes
                && let Some(Module::SourceFile(sf)) = build_state.build_state.modules.get_mut(name)
            {
                sf.set_compilation_stage(CompilationStage::CompileError {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    implementation_parse_warnings,
                    interface_parse_warnings,
                    compile_mode: effective_mode,
                });
            }
        }
    }

    // Collect warnings from modules that were not recompiled in this build
    // but still have stored warnings from a previous compilation.
    // This ensures warnings are not lost during incremental builds in watch mode.
    for (module_name, module) in build_state.modules.iter() {
        if compile_params.modules.contains(module_name) {
            continue;
        }
        if let Module::SourceFile(sf_module) = module {
            let stage = sf_module.compilation_stage();
            if let Some(warning) = stage.implementation_parse_warnings() {
                if let Some(package) = build_state.get_package(&sf_module.package_name) {
                    logs::append(package, warning);
                }
                compile_warnings.push_str(warning);
            }
            if let Some(warning) = stage.interface_parse_warnings() {
                if let Some(package) = build_state.get_package(&sf_module.package_name) {
                    logs::append(package, warning);
                }
                compile_warnings.push_str(warning);
            }
            if let Some(warning) = stage.compile_warnings() {
                if let Some(package) = build_state.get_package(&sf_module.package_name) {
                    logs::append(package, warning);
                }
                compile_warnings.push_str(warning);
            }
        }
    }

    Ok(ProcessResult {
        compile_errors,
        compile_warnings,
        num_compiled_modules,
    })
}

static RUNTIME_PATH_MEMO: OnceLock<PathBuf> = OnceLock::new();

pub fn get_runtime_path(package_config: &Config, project_context: &ProjectContext) -> Result<PathBuf> {
    if let Some(p) = RUNTIME_PATH_MEMO.get() {
        return Ok(p.clone());
    }

    let resolved = match std::env::var("RESCRIPT_RUNTIME") {
        Ok(runtime_path) => Ok(PathBuf::from(runtime_path)),
        Err(_) => match helpers::try_package_path(package_config, project_context, "@rescript/runtime") {
            Ok(runtime_path) => Ok(runtime_path),
            Err(err) => Err(anyhow!(
                "The rescript runtime package could not be found.\nPlease set RESCRIPT_RUNTIME environment variable or make sure the runtime package is installed.\nError: {err}"
            )),
        },
    }?;

    let _ = RUNTIME_PATH_MEMO.set(resolved.clone());
    Ok(resolved)
}

pub fn get_runtime_path_args(
    package_config: &Config,
    project_context: &ProjectContext,
) -> Result<Vec<String>> {
    let runtime_path = get_runtime_path(package_config, project_context)?;
    Ok(vec![
        "-runtime-path".to_string(),
        runtime_path.to_string_lossy().to_string(),
    ])
}

pub fn compiler_args(
    config: &config::Config,
    ast_path: &Path,
    file_path: &Path,
    is_interface: bool,
    has_interface: bool,
    project_context: &ProjectContext,
    packages: &AHashMap<String, packages::Package>,
    // Is the file listed as "type":"dev"?
    is_type_dev: bool,
    is_local_dep: bool,
    // Command-line --warn-error flag override (takes precedence over rescript.json config)
    warn_error_override: Option<String>,
    output: OutputTarget,
    mode: CompileMode,
) -> Result<Vec<String>> {
    let bsc_flags = config::flatten_flags(&config.compiler_flags);
    let dependency_paths = get_dependency_paths(config, packages, is_type_dev, output);
    let module_name = helpers::file_path_to_module_name(file_path, &config.get_namespace());

    let namespace_args = match &config.get_namespace() {
        packages::Namespace::NamespaceWithEntry { namespace: _, entry } if &module_name == entry => {
            // if the module is the entry we just want to open the namespace
            vec![
                "-open".to_string(),
                config.get_namespace().to_suffix().unwrap().to_string(),
            ]
        }
        packages::Namespace::Namespace(_)
        | packages::Namespace::NamespaceWithEntry {
            namespace: _,
            entry: _,
        } => {
            vec![
                "-bs-ns".to_string(),
                config.get_namespace().to_suffix().unwrap().to_string(),
            ]
        }
        packages::Namespace::NoNamespace => vec![],
    };

    let root_config = project_context.get_root_config();
    let jsx_args = root_config.get_jsx_args();
    let jsx_module_args = root_config.get_jsx_module_args();
    let jsx_mode_args = root_config.get_jsx_mode_args();
    let jsx_preserve_args = root_config.get_jsx_preserve_args();
    let gentype_arg = config.get_gentype_arg();
    let experimental_args = root_config.get_experimental_features_args();
    let warning_args = config.get_warning_args(is_local_dep, warn_error_override);

    let read_cmi_args = match has_interface {
        true => {
            if is_interface {
                vec![]
            } else {
                vec!["-bs-read-cmi".to_string()]
            }
        }
        false => vec![],
    };

    let package_name_arg = vec!["-bs-package-name".to_string(), config.name.to_owned()];

    let implementation_args = if is_interface {
        debug!("Compiling interface file: {}", &module_name);
        vec![]
    } else {
        debug!("Compiling file: {}", &module_name);
        match mode {
            CompileMode::TypecheckOnly => {
                // Type-check only — skip JS generation entirely
                vec!["-bs-cmi-only".to_string()]
            }
            CompileMode::FullCompile => {
                let specs = root_config.get_package_specs();
                specs
                    .iter()
                    .flat_map(|spec| {
                        vec![
                            "-bs-package-output".to_string(),
                            format!(
                                "{}:{}:{}",
                                spec.module.as_str(),
                                if spec.in_source {
                                    file_path.parent().unwrap().to_str().unwrap().to_string()
                                } else {
                                    Path::new("lib")
                                        .join(Path::join(
                                            Path::new(&spec.get_out_of_source_dir()),
                                            file_path.parent().unwrap(),
                                        ))
                                        .to_str()
                                        .unwrap()
                                        .to_string()
                                },
                                root_config.get_suffix(spec),
                            ),
                        ]
                    })
                    .collect()
            }
        }
    };

    let runtime_path_args = get_runtime_path_args(config, project_context)?;

    Ok(vec![
        namespace_args,
        read_cmi_args,
        vec![
            "-I".to_string(),
            if output.is_lsp() {
                Path::new("..").join("lsp-ocaml").to_string_lossy().to_string()
            } else {
                Path::new("..").join("ocaml").to_string_lossy().to_string()
            },
        ],
        runtime_path_args,
        dependency_paths,
        jsx_args,
        jsx_module_args,
        jsx_mode_args,
        jsx_preserve_args,
        bsc_flags.to_owned(),
        warning_args,
        gentype_arg,
        experimental_args,
        // vec!["-warn-error".to_string(), "A".to_string()],
        // ^^ this one fails for bisect-ppx
        // this is the default
        // we should probably parse the right ones from the package config
        // vec!["-w".to_string(), "a".to_string()],
        package_name_arg,
        implementation_args,
        // vec![
        //     "-I".to_string(),
        //     abs_node_modules_path.to_string() + "/rescript/ocaml",
        // ],
        vec![ast_path.to_string_lossy().to_string()],
    ]
    .concat())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DependentPackage {
    Normal(String),
    Dev(String),
}

impl DependentPackage {
    fn name(&self) -> &str {
        match self {
            Self::Normal(name) => name,
            Self::Dev(name) => name,
        }
    }

    fn is_dev(&self) -> bool {
        match self {
            Self::Normal(_) => false,
            Self::Dev(_) => true,
        }
    }
}

fn get_dependency_paths(
    config: &config::Config,
    packages: &AHashMap<String, packages::Package>,
    is_file_type_dev: bool,
    output: OutputTarget,
) -> Vec<String> {
    let normal_deps = config
        .dependencies
        .clone()
        .unwrap_or_default()
        .into_iter()
        .map(DependentPackage::Normal)
        .collect();

    // We can only access dev dependencies for source_files that are marked as "type":"dev"
    let dev_deps = if is_file_type_dev {
        config
            .dev_dependencies
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(DependentPackage::Dev)
            .collect()
    } else {
        vec![]
    };

    [dev_deps, normal_deps]
        .concat()
        .par_iter()
        .filter_map(|dependent_package| {
            let package_name = dependent_package.name();
            let dependency_path = packages.get(package_name).map(|package| {
                vec![
                    "-I".to_string(),
                    package
                        .get_ocaml_build_path_for_output(output)
                        .to_string_lossy()
                        .to_string(),
                ]
            });

            if !dependent_package.is_dev() && dependency_path.is_none() {
                panic!(
                    "Expected to find dependent package {} of {}",
                    package_name, config.name
                );
            }

            dependency_path
        })
        .collect::<Vec<Vec<String>>>()
        .concat()
}

fn compile_file(
    package: &packages::Package,
    ast_path: &Path,
    module: &Module,
    is_interface: bool,
    build_state: &BuildState,
    warn_error_override: Option<String>,
    output: OutputTarget,
    mode: CompileMode,
) -> Result<Option<String>> {
    let BuildState {
        packages,
        project_context,
        compiler_info,
        ..
    } = build_state;
    let root_config = build_state.get_root_config();
    let ocaml_build_path_abs = package.get_ocaml_build_path_for_output(output);
    let build_path_abs = package.get_build_path_for_output(output);
    let sf_module = match module {
        Module::SourceFile(sf) => sf,
        Module::MlMap(_) => {
            return Err(anyhow!(
                "Tried to compile a file that is not a source file (MlMap). Path to AST: {}.",
                ast_path.to_string_lossy()
            ));
        }
    };
    let implementation_file_path = &sf_module.source_file.implementation.path;
    let basename =
        helpers::file_path_to_compiler_asset_basename(implementation_file_path, &package.namespace);
    let has_interface = sf_module.source_file.interface.is_some();
    let is_type_dev = sf_module.is_type_dev;
    let to_mjs_args = compiler_args(
        &package.config,
        ast_path,
        implementation_file_path,
        is_interface,
        has_interface,
        project_context,
        packages,
        is_type_dev,
        package.is_local_dep,
        warn_error_override,
        output,
        mode,
    )?;

    let to_mjs = Command::new(&compiler_info.bsc_path)
        .current_dir(
            build_path_abs
                .canonicalize()
                .map(StrippedVerbatimPath::to_stripped_verbatim_path)
                .ok()
                .unwrap(),
        )
        .args(to_mjs_args)
        .output();

    match to_mjs {
        Ok(x) if !x.status.success() => {
            let stderr = String::from_utf8_lossy(&x.stderr);
            let stdout = String::from_utf8_lossy(&x.stdout);
            Err(anyhow!(stderr.to_string() + &stdout))
        }
        Err(e) => Err(anyhow!(
            "Could not compile file. Error: {e}. Path to AST: {ast_path:?}"
        )),
        Ok(x) => {
            let err = std::str::from_utf8(&x.stderr)
                .expect("stdout should be non-null")
                .to_string();

            let dir = Path::new(implementation_file_path).parent().unwrap();

            // Copy type-checking artifacts to lib/ocaml (needed by both profiles)
            if !is_interface {
                let _ = std::fs::copy(
                    build_path_abs.join(dir).join(format!("{basename}.cmi")),
                    ocaml_build_path_abs.join(format!("{basename}.cmi")),
                );
                let _ = std::fs::copy(
                    build_path_abs.join(dir).join(format!("{basename}.cmt")),
                    ocaml_build_path_abs.join(format!("{basename}.cmt")),
                );
                // .cmj is only produced when JS is emitted (TypecheckOnly uses -bs-cmi-only)
                if mode.emits_js() {
                    let _ = std::fs::copy(
                        build_path_abs.join(dir).join(format!("{basename}.cmj")),
                        ocaml_build_path_abs.join(format!("{basename}.cmj")),
                    );
                }
            } else {
                let _ = std::fs::copy(
                    build_path_abs.join(dir).join(format!("{basename}.cmti")),
                    ocaml_build_path_abs.join(format!("{basename}.cmti")),
                );
                let _ = std::fs::copy(
                    build_path_abs.join(dir).join(format!("{basename}.cmi")),
                    ocaml_build_path_abs.join(format!("{basename}.cmi")),
                );
            }

            // Source file copies, JS output copies, and post-build commands
            // are needed when JS is emitted
            if mode.emits_js() {
                if let Some(Interface { path, .. }) = &sf_module.source_file.interface {
                    let _ = std::fs::copy(Path::new(&package.path).join(path), build_path_abs.join(path));

                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        ocaml_build_path_abs.join(std::path::Path::new(path).file_name().unwrap()),
                    );
                }
                {
                    let path = &sf_module.source_file.implementation.path;
                    let _ = std::fs::copy(Path::new(&package.path).join(path), build_path_abs.join(path));

                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        ocaml_build_path_abs.join(std::path::Path::new(path).file_name().unwrap()),
                    );
                }

                // copy js file
                root_config.get_package_specs().iter().for_each(|spec| {
                    if spec.in_source {
                        let path = &sf_module.source_file.implementation.path;
                        let source = helpers::get_source_file_from_rescript_file(
                            &Path::new(&package.path).join(path),
                            &root_config.get_suffix(spec),
                        );
                        let destination = helpers::get_source_file_from_rescript_file(
                            &build_path_abs.join(path),
                            &root_config.get_suffix(spec),
                        );

                        if source.exists() {
                            let _ = std::fs::copy(&source, &destination);
                        }
                    }
                });

                // Execute js-post-build command if configured
                if !is_interface && let Some(js_post_build) = &package.config.js_post_build {
                    let path = &sf_module.source_file.implementation.path;
                    for spec in root_config.get_package_specs() {
                        let js_file = if spec.in_source {
                            helpers::get_source_file_from_rescript_file(
                                &Path::new(&package.path).join(path),
                                &root_config.get_suffix(&spec),
                            )
                        } else {
                            helpers::get_source_file_from_rescript_file(
                                &Path::new(&package.path)
                                    .join("lib")
                                    .join(spec.get_out_of_source_dir())
                                    .join(path),
                                &root_config.get_suffix(&spec),
                            )
                        };

                        if js_file.exists() {
                            execute_post_build_command(&js_post_build.cmd, &js_file, &package.path)?;
                        }
                    }
                }
            }

            if helpers::contains_ascii_characters(&err) {
                if package.is_local_dep {
                    // suppress warnings of external deps
                    Ok(Some(err))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
    }
}

pub fn mark_modules_with_deleted_deps_source_dirty(build_state: &mut BuildState) {
    build_state.modules.iter_mut().for_each(|(_, module)| {
        if let Module::SourceFile(sf) = module
            && !sf.deps.is_disjoint(&build_state.deleted_modules)
        {
            sf.set_compilation_stage(CompilationStage::SourceDirty);
        }
    });
}

// this happens when a compile is not completed successfully in some way
// a dependent module could be compiled with a new interface, but the dependent
// modules have not finished compiling. This can cause a stale build.
// When the build is clean this doesn't happen. But when we interupt the build,
// such as force quitting the watcher, it can happen.
//
// If a build stops in the middle of errors, this will also happen, because
// basically we interrupt a build and we stop compiling somewhere in the middle.
//
// In watch mode, we retain the dirty state of the modules, so we don't need
// to do this, which will make it more efficient.
//
// We could clean up the build after errors. But I think we probably still need
// to do this, because people can also force quit the watcher of
pub fn mark_modules_with_expired_deps_for_recompile(build_state: &mut BuildCommandState) {
    let mut modules_with_expired_deps: AHashSet<String> = AHashSet::new();
    build_state
        .modules
        .iter()
        .filter_map(|(name, module)| match module {
            Module::SourceFile(sf) => Some((name, sf)),
            Module::MlMap(_) => None,
        })
        .for_each(|(module_name, sf_module)| {
            for dependent in sf_module.dependents.iter() {
                let dependent_module = build_state.modules.get(dependent).unwrap();
                match dependent_module {
                    Module::SourceFile(dep_sf) => {
                        let compiled_at = sf_module.compilation_stage().compiled_at();
                        if compiled_at.is_none() {
                            modules_with_expired_deps.insert(module_name.to_string());
                        }

                        // we compare the last compiled time of the dependent module with the last
                        // compile of the interface of the module it depends on, if the interface
                        // didn't change it doesn't matter
                        match (dep_sf.compilation_stage().compiled_at(), compiled_at) {
                            (Some(last_compiled_dependent), Some(last_compiled)) => {
                                if last_compiled_dependent < last_compiled {
                                    modules_with_expired_deps.insert(dependent.to_string());
                                }
                            }
                            (None, _) => {
                                modules_with_expired_deps.insert(dependent.to_string());
                            }
                            _ => (),
                        }
                    }
                    // a namespace is never a dependent of a module (it can be a dependency, but not the other
                    // way around)
                    Module::MlMap(mlmap_module) => {
                        for dependent_of_namespace in mlmap_module.dependents.iter() {
                            let dependent_module = build_state.modules.get(dependent_of_namespace).unwrap();

                            if let Module::SourceFile(dep_sf) = dependent_module
                                && let (Some(last_compiled_dependent), Some(last_compiled)) = (
                                    dep_sf.compilation_stage().compiled_at(),
                                    sf_module.compilation_stage().compiled_at(),
                                )
                                && last_compiled_dependent < last_compiled
                            {
                                modules_with_expired_deps.insert(dependent.to_string());
                            }
                        }
                    }
                }
            }
        });
    build_state.modules.iter_mut().for_each(|(module_name, module)| {
        if let Module::SourceFile(sf) = module
            && modules_with_expired_deps.contains(module_name)
            // Preserve CompileError and Parsed — both already signal "needs
            // compilation". CompileError carries semantic meaning for the LSP
            // flow (step 3 in file_build::build_batch uses it to detect
            // modules that need full compilation after a dependency fix).
            // Parsed modules were just parsed in this cycle and resetting them
            // loses their source/AST hashes.
            && !sf.compilation_stage().is_compile_error()
            && !matches!(sf.compilation_stage(), CompilationStage::Parsed { .. })
        {
            // Extract parse hashes from the current stage when available.
            // Modules at Built/TypeChecked/DependencyDirty have valid ASTs
            // and only need recompilation, not reparsing → DependencyDirty.
            // Modules at SourceDirty/ParseError have no valid hashes → SourceDirty.
            let hashes = match sf.compilation_stage() {
                CompilationStage::TypeChecked {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    implementation_parse_warnings,
                    interface_parse_warnings,
                    ..
                }
                | CompilationStage::Built {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    implementation_parse_warnings,
                    interface_parse_warnings,
                    ..
                }
                | CompilationStage::DependencyDirty {
                    implementation_source_hash,
                    implementation_ast_hash,
                    interface_source_hash,
                    interface_ast_hash,
                    implementation_parse_warnings,
                    interface_parse_warnings,
                } => Some((
                    *implementation_source_hash,
                    *implementation_ast_hash,
                    *interface_source_hash,
                    *interface_ast_hash,
                    implementation_parse_warnings.clone(),
                    interface_parse_warnings.clone(),
                )),
                _ => None,
            };
            match hashes {
                Some((ish, iah, ifs, ifa, ipw, ifpw)) => {
                    sf.set_compilation_stage(CompilationStage::DependencyDirty {
                        implementation_source_hash: ish,
                        implementation_ast_hash: iah,
                        interface_source_hash: ifs,
                        interface_ast_hash: ifa,
                        implementation_parse_warnings: ipw,
                        interface_parse_warnings: ifpw,
                    });
                }
                None => {
                    sf.set_compilation_stage(CompilationStage::SourceDirty);
                }
            }
        }
    });
}
