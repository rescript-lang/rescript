#![allow(clippy::too_many_arguments)]

mod dependency_cycle;

use super::BuildReporter;
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

use rayon::prelude::*;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;
use std::time::SystemTime;

/// Execute js-post-build command for a compiled JavaScript file.
/// The command runs in the directory containing the rescript.json that defines it.
/// The absolute path to the JS file is passed as an argument.
fn execute_post_build_command<R: BuildReporter>(
    cmd: &str,
    js_file_path: &Path,
    working_dir: &Path,
    reporter: &R,
) -> Result<()> {
    let full_command = format!("{} {}", cmd, js_file_path.display());

    let _span = tracing::info_span!(
        "build.js_post_build",
        command = %cmd,
        js_file = %js_file_path.display(),
    )
    .entered();

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
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Report stdout to client
            if !stdout.is_empty() {
                let _stdout_span = tracing::info_span!("build.js_post_build_stdout").entered();
                reporter.report(super::BuildProgress::JsPostBuildOutput {
                    command: cmd.to_string(),
                    js_file: js_file_path.display().to_string(),
                    stdout: Some(stdout.trim().to_string()),
                    stderr: None,
                });
            }

            // Report stderr to client
            if !stderr.is_empty() {
                let _stderr_span = tracing::info_span!("build.js_post_build_stderr").entered();
                reporter.report(super::BuildProgress::JsPostBuildOutput {
                    command: cmd.to_string(),
                    js_file: js_file_path.display().to_string(),
                    stdout: None,
                    stderr: Some(stderr.trim().to_string()),
                });
            }

            if !output.status.success() {
                let _error_span = tracing::info_span!("build.js_post_build_error").entered();
                Err(anyhow!(
                    "js-post-build command failed for {}",
                    js_file_path.display()
                ))
            } else {
                Ok(())
            }
        }
        Err(e) => {
            let _error_span = tracing::info_span!("build.js_post_build_error").entered();
            Err(anyhow!("Failed to execute js-post-build command: {}", e))
        }
    }
}

/// Result of compilation.
pub struct CompileResult {
    /// Compiler warnings (accumulated for logging)
    pub compile_warnings: String,
    /// Number of modules compiled
    pub num_compiled_modules: usize,
    /// Whether compilation had errors (circular dependency, compiler errors, etc.)
    pub had_errors: bool,
}

pub fn compile<R: BuildReporter>(
    build_state: &mut BuildState,
    warn_error: Option<String>,
    filter: &Option<regex::Regex>,
    show_progress: bool,
    inc: impl Fn() + std::marker::Sync,
    set_length: impl Fn(u64),
    reporter: &R,
) -> anyhow::Result<CompileResult> {
    let mut compiled_modules = AHashSet::<String>::new();
    let dirty_modules = build_state
        .modules
        .iter()
        .filter_map(|(module_name, module)| {
            let matches_filter = filter.as_ref().is_none_or(|re| re.is_match(module_name));
            (module.compile_dirty && matches_filter).then(|| module_name.to_owned())
        })
        .collect::<AHashSet<String>>();

    // dirty_modules.iter().for_each(|m| println!("dirty module: {}", m));
    // println!("{} dirty modules", dirty_modules.len());
    let mut sorted_dirty_modules = dirty_modules.iter().collect::<Vec<&String>>();
    sorted_dirty_modules.sort();
    // dirty_modules.iter().for_each(|m| println!("dirty module: {}", m));
    // sorted_dirty_modules
    //     .iter()
    //     .for_each(|m| println!("dirty module: {}", m));

    // for sure clean modules -- after checking the hash of the cmi
    let mut clean_modules = AHashSet::<String>::new();

    // TODO: calculate the real dirty modules from the original dirty modules in each iteration
    // taken into account the modules that we know are clean, so they don't propagate through the
    // deps graph
    // create a hashset of all clean modules from the file-hashes
    let mut loop_count = 0;
    let mut files_total_count = compiled_modules.len();
    let mut files_current_loop_count;
    let mut had_errors = false;
    let mut compile_warnings = "".to_string();
    let mut num_compiled_modules = 0;
    let mut sorted_modules = build_state.module_names.iter().collect::<Vec<&String>>();
    sorted_modules.sort();

    // this is the whole "compile universe" all modules that might be dirty
    // we get this by expanding the dependents from the dirty modules

    let mut compile_universe = dirty_modules.clone();
    let mut current_step_modules = compile_universe.clone();
    loop {
        let mut dependents: AHashSet<String> = AHashSet::new();
        for dirty_module in current_step_modules.iter() {
            if let Some(module) = build_state.get_module(dirty_module) {
                dependents.extend(module.dependents.clone());
            }
        }

        current_step_modules = dependents
            .difference(&compile_universe)
            .filter(|name| filter.as_ref().is_none_or(|re| re.is_match(name)))
            .map(|s| s.to_string())
            .collect::<AHashSet<String>>();

        compile_universe.extend(current_step_modules.to_owned());
        if current_step_modules.is_empty() {
            break;
        }
    }

    let compile_universe_count = compile_universe.len();
    set_length(compile_universe_count as u64);

    tracing::info!(
        dirty_modules = dirty_modules.len(),
        compile_universe = compile_universe_count,
        "build.compile_universe"
    );

    // start off with all modules that have no deps in this compile universe
    let mut in_progress_modules = compile_universe
        .iter()
        .filter(|module_name| {
            build_state
                .get_module(module_name)
                .is_some_and(|module| module.deps.intersection(&compile_universe).count() == 0)
        })
        .map(|module_name| module_name.to_string())
        .collect::<AHashSet<String>>();

    loop {
        files_current_loop_count = 0;
        loop_count += 1;

        let wave_span = tracing::info_span!(
            "build.compile_wave",
            wave = loop_count,
            file_count = in_progress_modules.len(),
        );
        let _wave_guard = wave_span.enter();

        let current_in_progres_modules = in_progress_modules.clone();

        let results = current_in_progres_modules
            .par_iter()
            .filter_map(|module_name| {
                let module = build_state.get_module(module_name)?;
                let package = build_state.get_package(&module.package_name)?;
                // all dependencies that we care about are compiled
                if module
                    .deps
                    .intersection(&compile_universe)
                    .all(|dep| compiled_modules.contains(dep))
                {
                    if !module.compile_dirty {
                        // we are sure we don't have to compile this, so we can mark it as compiled and clean
                        return Some((module_name.to_string(), Ok(None), Some(Ok(None)), true, false));
                    }
                    match module.source_type.to_owned() {
                        SourceType::MlMap(_) => {
                            // the mlmap needs to be compiled before the files are compiled
                            // in the same namespace, otherwise we get a compile error
                            // this is why mlmap is compiled in the AST generation stage
                            // compile_mlmap(&module.package, module_name, &project_root);
                            Some((
                                package.namespace.to_suffix().unwrap(),
                                Ok(None),
                                Some(Ok(None)),
                                false,
                                false,
                            ))
                        }
                        SourceType::SourceFile(source_file) => {
                            let _file_span = tracing::info_span!(
                                parent: &wave_span,
                                "build.compile_file",
                                module = %module_name,
                                package = %module.package_name,
                                has_interface = source_file.interface.is_some(),
                            )
                            .entered();

                            let cmi_path = helpers::get_compiler_asset(
                                package,
                                &package.namespace,
                                &source_file.implementation.path,
                                "cmi",
                            );

                            let cmi_digest = helpers::compute_file_hash(Path::new(&cmi_path));

                            let package = build_state.get_package(&module.package_name)?;

                            let interface_result = match source_file.interface.to_owned() {
                                Some(Interface { path, .. }) => {
                                    let result = compile_file(
                                        package,
                                        &helpers::get_ast_path(&path),
                                        module,
                                        true,
                                        build_state,
                                        warn_error.clone(),
                                        reporter,
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
                                warn_error.clone(),
                                reporter,
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

                            Some((
                                module_name.to_string(),
                                result,
                                interface_result,
                                is_clean_cmi,
                                true,
                            ))
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

        for result in results.iter() {
            let (module_name, result, interface_result, is_clean, is_compiled) = result;
            in_progress_modules.remove(module_name);

            if *is_compiled {
                num_compiled_modules += 1;
            }

            files_current_loop_count += 1;
            compiled_modules.insert(module_name.to_string());

            if *is_clean {
                // actually add it to a list of clean modules
                clean_modules.insert(module_name.to_string());
            }

            let module_dependents = build_state
                .get_module(module_name)
                .map(|m| m.dependents.clone())
                .unwrap_or_default();

            // if not clean -- compile modules that depend on this module
            for dep in module_dependents.iter() {
                //  mark the reverse dep as dirty when the source is not clean
                if !*is_clean && let Some(dep_module) = build_state.modules.get_mut(dep) {
                    dep_module.compile_dirty = true;
                }
                if !compiled_modules.contains(dep) {
                    in_progress_modules.insert(dep.to_string());
                }
            }

            let package_name = {
                let module = build_state
                    .modules
                    .get(module_name)
                    .ok_or(anyhow!("Module not found"))?;
                module.package_name.clone()
            };

            let package = build_state
                .packages
                .get(&package_name)
                .ok_or(anyhow!("Package name not found"))?;

            // Process results and update module state
            let (compile_warning, compile_error, interface_warning, interface_error) = {
                let module = build_state
                    .modules
                    .get_mut(module_name)
                    .ok_or(anyhow!("Module not found"))?;

                let (compile_warning, compile_error) = match module.source_type {
                    SourceType::MlMap(ref mut mlmap) => {
                        module.compile_dirty = false;
                        mlmap.parse_dirty = false;
                        (None, None)
                    }
                    SourceType::SourceFile(ref mut source_file) => match result {
                        Ok(Some(err)) => {
                            let warning_text = err.to_string();
                            source_file.implementation.compile_state = CompileState::Warning;
                            source_file.implementation.compile_warnings = Some(warning_text.clone());
                            (Some(warning_text), None)
                        }
                        Ok(None) => {
                            source_file.implementation.compile_state = CompileState::Success;
                            source_file.implementation.compile_warnings = None;
                            (None, None)
                        }
                        Err(err) => {
                            source_file.implementation.compile_state = CompileState::Error;
                            source_file.implementation.compile_warnings = None;
                            (None, Some(err.to_string()))
                        }
                    },
                };

                let (interface_warning, interface_error) =
                    if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                        match interface_result {
                            Some(Ok(Some(err))) => {
                                let warning_text = err.to_string();
                                source_file.interface.as_mut().unwrap().compile_state = CompileState::Warning;
                                source_file.interface.as_mut().unwrap().compile_warnings =
                                    Some(warning_text.clone());
                                (Some(warning_text), None)
                            }
                            Some(Ok(None)) => {
                                if let Some(interface) = source_file.interface.as_mut() {
                                    interface.compile_state = CompileState::Success;
                                    interface.compile_warnings = None;
                                }
                                (None, None)
                            }
                            Some(Err(err)) => {
                                source_file.interface.as_mut().unwrap().compile_state = CompileState::Error;
                                source_file.interface.as_mut().unwrap().compile_warnings = None;
                                (None, Some(err.to_string()))
                            }
                            _ => (None, None),
                        }
                    } else {
                        (None, None)
                    };

                // Update compilation timestamps for successful compilation
                if result.is_ok() && interface_result.as_ref().is_none_or(|r| r.is_ok()) {
                    module.compile_dirty = false;
                    module.last_compiled_cmi = Some(SystemTime::now());
                    module.last_compiled_cmt = Some(SystemTime::now());
                }

                (compile_warning, compile_error, interface_warning, interface_error)
            };

            // Handle logging outside the mutable borrow
            if let Some(warning) = compile_warning {
                logs::append(package, &warning);
                compile_warnings.push_str(&warning);
            }
            if let Some(error) = compile_error {
                logs::append(package, &error);
                reporter.report(crate::build::BuildProgress::CompilerError(error));
                had_errors = true;
            }
            if let Some(warning) = interface_warning {
                logs::append(package, &warning);
                compile_warnings.push_str(&warning);
            }
            if let Some(error) = interface_error {
                logs::append(package, &error);
                reporter.report(crate::build::BuildProgress::CompilerError(error));
                had_errors = true;
            }
        }

        files_total_count += files_current_loop_count;

        if files_total_count == compile_universe_count {
            break;
        }
        if in_progress_modules.is_empty() || in_progress_modules.eq(&current_in_progres_modules) {
            // find the dependency cycle
            let cycle = dependency_cycle::find(
                &compile_universe
                    .iter()
                    .filter_map(|s| build_state.get_module(s).map(|m| (s, m)))
                    .collect::<Vec<(&String, &Module)>>(),
            );

            let cycle_description = dependency_cycle::format(&cycle, build_state);

            // Report structured progress for client-side formatting
            reporter.report(crate::build::BuildProgress::CircularDependency {
                cycle_description: cycle_description.clone(),
            });

            // Build message for compiler logs (editor tooling reads these)
            let guidance = "Possible solutions:\n- Extract shared code into a new module both depend on.\n";
            let message = format!(
                "\nCan't continue... Found a circular dependency in your code:\n{}\n{}",
                cycle_description, guidance
            );

            // Append the error to the logs of all packages involved in the cycle,
            // so editor tooling can surface it from .compiler.log
            let mut touched_packages = AHashSet::<String>::new();
            for module_name in cycle.iter() {
                if let Some(module) = build_state.get_module(module_name)
                    && touched_packages.insert(module.package_name.clone())
                    && let Some(package) = build_state.get_package(&module.package_name)
                {
                    logs::append(package, &message);
                }
            }

            had_errors = true;
            break;
        }
        if had_errors {
            break;
        };
    }

    // Collect warnings from modules that were not recompiled in this build
    // but still have stored warnings from a previous compilation.
    // This ensures warnings are not lost during incremental builds in watch mode.
    for (module_name, module) in build_state.modules.iter() {
        if compile_universe.contains(module_name) {
            continue;
        }
        if let SourceType::SourceFile(ref source_file) = module.source_type {
            let package = build_state.get_package(&module.package_name);
            if let Some(ref warning) = source_file.implementation.compile_warnings {
                if let Some(package) = package {
                    logs::append(package, warning);
                }
                compile_warnings.push_str(warning);
            }
            if let Some(ref interface) = source_file.interface
                && let Some(ref warning) = interface.compile_warnings
            {
                if let Some(package) = package {
                    logs::append(package, warning);
                }
                compile_warnings.push_str(warning);
            }
        }
    }

    Ok(CompileResult {
        compile_warnings,
        num_compiled_modules,
        had_errors,
    })
}

static RUNTIME_PATH_MEMO: OnceLock<PathBuf> = OnceLock::new();

pub fn get_runtime_path<R: BuildReporter>(
    package_config: &Config,
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<PathBuf> {
    if let Some(p) = RUNTIME_PATH_MEMO.get() {
        return Ok(p.clone());
    }

    let resolved = match std::env::var("RESCRIPT_RUNTIME") {
        Ok(runtime_path) => Ok(PathBuf::from(runtime_path)),
        Err(_) => {
            match helpers::try_package_path(package_config, project_context, "@rescript/runtime", reporter) {
                Ok(runtime_path) => Ok(runtime_path),
                Err(err) => Err(anyhow!(
                    "The rescript runtime package could not be found.\nPlease set RESCRIPT_RUNTIME environment variable or make sure the runtime package is installed.\nError: {err}"
                )),
            }
        }
    }?;

    let _ = RUNTIME_PATH_MEMO.set(resolved.clone());
    Ok(resolved)
}

pub fn get_runtime_path_args<R: BuildReporter>(
    package_config: &Config,
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<Vec<String>> {
    let runtime_path = get_runtime_path(package_config, project_context, reporter)?;
    Ok(vec![
        "-runtime-path".to_string(),
        runtime_path.to_string_lossy().to_string(),
    ])
}

pub fn compiler_args<R: BuildReporter>(
    config: &config::Config,
    ast_path: &Path,
    file_path: &Path,
    is_interface: bool,
    has_interface: bool,
    project_context: &ProjectContext,
    // if packages are known, we pass a reference here
    // this saves us a scan to find their paths.
    // This is None when called by build::get_compiler_args
    packages: &Option<&AHashMap<String, packages::Package>>,
    // Is the file listed as "type":"dev"?
    is_type_dev: bool,
    is_local_dep: bool,
    // Command-line --warn-error flag override (takes precedence over rescript.json config)
    warn_error_override: Option<String>,
    reporter: &R,
) -> Result<Vec<String>> {
    let bsc_flags = config::flatten_flags(&config.compiler_flags);
    let dependency_paths = get_dependency_paths(config, project_context, packages, is_type_dev, reporter);
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
        tracing::debug!(module = %module_name, "Compiling interface file");
        vec![]
    } else {
        tracing::debug!(module = %module_name, "Compiling file");
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
    };

    let runtime_path_args = get_runtime_path_args(config, project_context, reporter)?;

    Ok(vec![
        namespace_args,
        read_cmi_args,
        vec![
            "-I".to_string(),
            Path::new("..").join("ocaml").to_string_lossy().to_string(),
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

fn get_dependency_paths<R: BuildReporter>(
    config: &config::Config,
    project_context: &ProjectContext,
    packages: &Option<&AHashMap<String, packages::Package>>,
    is_file_type_dev: bool,
    reporter: &R,
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
            let dependency_path = if let Some(packages) = packages {
                packages
                    .get(package_name)
                    .as_ref()
                    .map(|package| package.path.clone())
            } else {
                // packages will only be None when called by build::get_compiler_args
                // in that case we can safely pass config as the package config.
                packages::read_dependency(package_name, config, project_context, reporter).ok()
            }
            .map(|canonicalized_path| {
                vec![
                    "-I".to_string(),
                    packages::get_ocaml_build_path(&canonicalized_path)
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

fn compile_file<R: BuildReporter>(
    package: &packages::Package,
    ast_path: &Path,
    module: &Module,
    is_interface: bool,
    build_state: &BuildState,
    warn_error_override: Option<String>,
    reporter: &R,
) -> Result<Option<String>> {
    let BuildState {
        packages,
        project_context,
        compiler_info,
        ..
    } = build_state;
    let root_config = build_state.get_root_config();
    let ocaml_build_path_abs = package.get_ocaml_build_path();
    let build_path_abs = package.get_build_path();
    let implementation_file_path = match &module.source_type {
        SourceType::SourceFile(source_file) => Ok(&source_file.implementation.path),
        sourcetype => Err(format!(
            "Tried to compile a file that is not a source file ({}). Path to AST: {}. ",
            sourcetype,
            ast_path.to_string_lossy()
        )),
    }
    .map_err(|e| anyhow!(e))?;
    let basename =
        helpers::file_path_to_compiler_asset_basename(implementation_file_path, &package.namespace);
    let has_interface = module.get_interface().is_some();
    let is_type_dev = module.is_type_dev;
    let to_mjs_args = compiler_args(
        &package.config,
        ast_path,
        implementation_file_path,
        is_interface,
        has_interface,
        project_context,
        &Some(packages),
        is_type_dev,
        package.is_local,
        warn_error_override,
        reporter,
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

            // perhaps we can do this copying somewhere else
            if !is_interface {
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        // because editor tooling doesn't support namespace entries yet
                        // we just remove the @ for now. This makes sure the editor support
                        // doesn't break
                        .join(format!("{basename}.cmi")),
                    ocaml_build_path_abs.join(format!("{basename}.cmi")),
                );
                let _ = std::fs::copy(
                    package.get_build_path().join(dir).join(format!("{basename}.cmj")),
                    ocaml_build_path_abs.join(format!("{basename}.cmj")),
                );
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        // because editor tooling doesn't support namespace entries yet
                        // we just remove the @ for now. This makes sure the editor support
                        // doesn't break
                        .join(format!("{basename}.cmt")),
                    ocaml_build_path_abs.join(format!("{basename}.cmt")),
                );
            } else {
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        .join(format!("{basename}.cmti")),
                    ocaml_build_path_abs.join(format!("{basename}.cmti")),
                );
                let _ = std::fs::copy(
                    package.get_build_path().join(dir).join(format!("{basename}.cmi")),
                    ocaml_build_path_abs.join(format!("{basename}.cmi")),
                );
            }

            if let SourceType::SourceFile(SourceFile {
                interface: Some(Interface { path, .. }),
                ..
            }) = &module.source_type
            {
                // we need to copy the source file to the build directory.
                // editor tools expects the source file in lib/bs for finding the current package
                // and in lib/ocaml when referencing modules in other packages
                let src = Path::new(&package.path).join(path);
                let dest = package.get_build_path().join(path);
                if let Err(e) = std::fs::copy(&src, &dest) {
                    log::warn!(
                        "Failed to copy source file {} to {}: {}",
                        src.display(),
                        dest.display(),
                        e
                    );
                }

                if let Some(file_name) = std::path::Path::new(path).file_name() {
                    let dest = package.get_ocaml_build_path().join(file_name);
                    if let Err(e) = std::fs::copy(&src, &dest) {
                        log::warn!(
                            "Failed to copy source file {} to {}: {}",
                            src.display(),
                            dest.display(),
                            e
                        );
                    }
                }
            }
            if let SourceType::SourceFile(SourceFile {
                implementation: Implementation { path, .. },
                ..
            }) = &module.source_type
            {
                // we need to copy the source file to the build directory.
                // editor tools expects the source file in lib/bs for finding the current package
                // and in lib/ocaml when referencing modules in other packages
                let src = Path::new(&package.path).join(path);
                let dest = package.get_build_path().join(path);
                if let Err(e) = std::fs::copy(&src, &dest) {
                    log::warn!(
                        "Failed to copy source file {} to {}: {}",
                        src.display(),
                        dest.display(),
                        e
                    );
                }

                if let Some(file_name) = std::path::Path::new(path).file_name() {
                    let dest = package.get_ocaml_build_path().join(file_name);
                    if let Err(e) = std::fs::copy(&src, &dest) {
                        log::warn!(
                            "Failed to copy source file {} to {}: {}",
                            src.display(),
                            dest.display(),
                            e
                        );
                    }
                }
            }

            // copy js file
            root_config.get_package_specs().iter().for_each(|spec| {
                if spec.in_source
                    && let SourceType::SourceFile(SourceFile {
                        implementation: Implementation { path, .. },
                        ..
                    }) = &module.source_type
                {
                    let source = helpers::get_source_file_from_rescript_file(
                        &Path::new(&package.path).join(path),
                        &root_config.get_suffix(spec),
                    );
                    let destination = helpers::get_source_file_from_rescript_file(
                        &package.get_build_path().join(path),
                        &root_config.get_suffix(spec),
                    );

                    if source.exists()
                        && let Err(e) = std::fs::copy(&source, &destination)
                    {
                        log::warn!(
                            "Failed to copy JS file {} to {}: {}",
                            source.display(),
                            destination.display(),
                            e
                        );
                    }
                }
            });

            // Execute js-post-build command if configured
            // Only run for implementation files (not interfaces)
            if !is_interface
                && let Some(js_post_build) = &package.config.js_post_build
                && let SourceType::SourceFile(SourceFile {
                    implementation: Implementation { path, .. },
                    ..
                }) = &module.source_type
            {
                // Execute post-build command for each package spec (each output format)
                for spec in root_config.get_package_specs() {
                    // Determine the correct JS file path based on in-source setting:
                    // - in-source: true  -> next to the source file (e.g., src/Foo.js)
                    // - in-source: false -> in lib/<module>/ directory (e.g., lib/es6/src/Foo.js)
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
                        // Fail the build if post-build command fails (matches bsb behavior with &&)
                        // Run in the package's directory (where rescript.json is defined)
                        execute_post_build_command(&js_post_build.cmd, &js_file, &package.path, reporter)?;
                    }
                }
            }

            if helpers::contains_ascii_characters(&err) {
                if package.is_local {
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

pub fn mark_modules_with_deleted_deps_dirty(build_state: &mut BuildState) {
    build_state.modules.iter_mut().for_each(|(_, module)| {
        if !module.deps.is_disjoint(&build_state.deleted_modules) {
            module.compile_dirty = true;
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
pub fn mark_modules_with_expired_deps_dirty(build_state: &mut BuildState) {
    let mut modules_with_expired_deps: AHashSet<String> = AHashSet::new();
    build_state
        .modules
        .iter()
        .filter(|m| !m.1.is_mlmap())
        .for_each(|(module_name, module)| {
            for dependent in module.dependents.iter() {
                let Some(dependent_module) = build_state.modules.get(dependent) else {
                    continue;
                };
                match dependent_module.source_type {
                    SourceType::SourceFile(_) => {
                        match (module.last_compiled_cmt, module.last_compiled_cmt) {
                            (None, None) | (Some(_), None) | (None, Some(_)) => {
                                // println!(
                                //     "🛑 {} is a dependent of {} but has no cmt/cmi",
                                //     module_name, dependent
                                // );
                                modules_with_expired_deps.insert(module_name.to_string());
                            }
                            (Some(_), Some(_)) => (),
                        }

                        // we compare the last compiled time of the dependent module with the last
                        // compile of the interface of the module it depends on, if the interface
                        // didn't change it doesn't matter
                        match (dependent_module.last_compiled_cmt, module.last_compiled_cmt) {
                            (Some(last_compiled_dependent), Some(last_compiled)) => {
                                if last_compiled_dependent < last_compiled {
                                    // println!(
                                    //     "✅ {} is a dependent of {} ({:?} / {:?})",
                                    //     module_name, dependent, last_compiled_dependent, last_compiled
                                    // );

                                    modules_with_expired_deps.insert(dependent.to_string());
                                } else {
                                    // println!(
                                    //     "🛑 {} is a dependent of {} ({:?} / {:?})",
                                    //     module_name, dependent, last_compiled_dependent, last_compiled
                                    // );
                                }
                            }
                            (None, _) => {
                                // println!(
                                //     "🛑 {} is a dependent of {} (no last compiled time)",
                                //     module_name, dependent
                                // );
                                modules_with_expired_deps.insert(dependent.to_string());
                            }
                            _ => (),
                        }
                    }
                    // a namespace is never a dependent of a module (it can be a dependency, but not the other
                    // way around)
                    SourceType::MlMap(_) => {
                        for dependent_of_namespace in dependent_module.dependents.iter() {
                            let Some(dep_of_ns_module) = build_state.modules.get(dependent_of_namespace)
                            else {
                                continue;
                            };

                            if let (Some(last_compiled_dependent), Some(last_compiled)) =
                                (dep_of_ns_module.last_compiled_cmt, module.last_compiled_cmt)
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
        if modules_with_expired_deps.contains(module_name) {
            module.compile_dirty = true;
        }
    });
}
