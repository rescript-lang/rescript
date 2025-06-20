#![allow(clippy::too_many_arguments)]

mod dependency_cycle;

use super::build_types::*;
use super::logs;
use super::packages;
use crate::config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use ahash::{AHashMap, AHashSet};
use anyhow::anyhow;
use console::style;
use log::{debug, trace};
use rayon::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

pub fn compile(
    build_state: &mut BuildState,
    show_progress: bool,
    inc: impl Fn() + std::marker::Sync,
    set_length: impl Fn(u64),
    build_dev_deps: bool,
) -> anyhow::Result<(String, String, usize)> {
    let mut compiled_modules = AHashSet::<String>::new();
    let dirty_modules = build_state
        .modules
        .iter()
        .filter_map(|(module_name, module)| {
            if module.compile_dirty {
                Some(module_name.to_owned())
            } else {
                None
            }
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
    let mut compile_errors = "".to_string();
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
            dependents.extend(build_state.get_module(dirty_module).unwrap().dependents.clone());
        }

        current_step_modules = dependents
            .difference(&compile_universe)
            .map(|s| s.to_string())
            .collect::<AHashSet<String>>();

        compile_universe.extend(current_step_modules.to_owned());
        if current_step_modules.is_empty() {
            break;
        }
    }

    let compile_universe_count = compile_universe.len();
    set_length(compile_universe_count as u64);

    // start off with all modules that have no deps in this compile universe
    let mut in_progress_modules = compile_universe
        .iter()
        .filter(|module_name| {
            let module = build_state.get_module(module_name).unwrap();
            module.deps.intersection(&compile_universe).count() == 0
        })
        .map(|module_name| module_name.to_string())
        .collect::<AHashSet<String>>();

    loop {
        files_current_loop_count = 0;
        loop_count += 1;

        trace!(
            "Compiled: {} out of {}. Compile loop: {}",
            files_total_count,
            compile_universe.len(),
            loop_count,
        );

        let current_in_progres_modules = in_progress_modules.clone();

        let results = current_in_progres_modules
            .par_iter()
            .filter_map(|module_name| {
                let module = build_state.get_module(module_name).unwrap();
                let package = build_state
                    .get_package(&module.package_name)
                    .expect("Package not found");
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
                            let cmi_path = helpers::get_compiler_asset(
                                package,
                                &package.namespace,
                                &source_file.implementation.path,
                                "cmi",
                            );

                            let cmi_digest = helpers::compute_file_hash(Path::new(&cmi_path));

                            let package = build_state
                                .get_package(&module.package_name)
                                .expect("Package not found");

                            let root_package =
                                build_state.get_package(&build_state.root_config_name).unwrap();

                            let interface_result = match source_file.interface.to_owned() {
                                Some(Interface { path, .. }) => {
                                    let result = compile_file(
                                        package,
                                        root_package,
                                        &helpers::get_ast_path(&path),
                                        module,
                                        &build_state.rescript_version,
                                        true,
                                        &build_state.bsc_path,
                                        &build_state.packages,
                                        &build_state.project_root,
                                        &build_state.workspace_root,
                                        build_dev_deps,
                                    );
                                    Some(result)
                                }
                                _ => None,
                            };
                            let result = compile_file(
                                package,
                                root_package,
                                &helpers::get_ast_path(&source_file.implementation.path),
                                module,
                                &build_state.rescript_version,
                                false,
                                &build_state.bsc_path,
                                &build_state.packages,
                                &build_state.project_root,
                                &build_state.workspace_root,
                                build_dev_deps,
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

            let module_dependents = build_state.get_module(module_name).unwrap().dependents.clone();

            // if not clean -- compile modules that depend on this module
            for dep in module_dependents.iter() {
                //  mark the reverse dep as dirty when the source is not clean
                if !*is_clean {
                    let dep_module = build_state.modules.get_mut(dep).unwrap();
                    //  mark the reverse dep as dirty when the source is not clean
                    dep_module.compile_dirty = true;
                }
                if !compiled_modules.contains(dep) {
                    in_progress_modules.insert(dep.to_string());
                }
            }

            let module = build_state
                .modules
                .get_mut(module_name)
                .ok_or(anyhow!("Module not found"))?;

            let package = build_state
                .packages
                .get(&module.package_name)
                .ok_or(anyhow!("Package name not found"))?;

            match module.source_type {
                SourceType::MlMap(ref mut mlmap) => {
                    module.compile_dirty = false;
                    mlmap.parse_dirty = false;
                }
                SourceType::SourceFile(ref mut source_file) => {
                    match result {
                        Ok(Some(err)) => {
                            source_file.implementation.compile_state = CompileState::Warning;
                            logs::append(package, err);
                            compile_warnings.push_str(err);
                        }
                        Ok(None) => {
                            source_file.implementation.compile_state = CompileState::Success;
                        }
                        Err(err) => {
                            source_file.implementation.compile_state = CompileState::Error;
                            logs::append(package, &err.to_string());
                            compile_errors.push_str(&err.to_string());
                        }
                    };
                    match interface_result {
                        Some(Ok(Some(err))) => {
                            source_file.interface.as_mut().unwrap().compile_state = CompileState::Warning;
                            logs::append(package, &err.to_string());
                            compile_warnings.push_str(&err.to_string());
                        }
                        Some(Ok(None)) => {
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.compile_state = CompileState::Success;
                            }
                        }

                        Some(Err(err)) => {
                            source_file.interface.as_mut().unwrap().compile_state = CompileState::Error;
                            logs::append(package, &err.to_string());
                            compile_errors.push_str(&err.to_string());
                        }
                        _ => (),
                    };

                    match (result, interface_result) {
                        // successfull compilation
                        (Ok(None), Some(Ok(None))) | (Ok(None), None) => {
                            module.compile_dirty = false;
                            module.last_compiled_cmi = Some(SystemTime::now());
                            module.last_compiled_cmt = Some(SystemTime::now());
                        }
                        // some error or warning
                        (Err(_), _) | (_, Some(Err(_))) | (Ok(Some(_)), _) | (_, Some(Ok(Some(_)))) => {
                            module.compile_dirty = true;
                        }
                    }
                }
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
                    .map(|s| (s, build_state.get_module(s).unwrap()))
                    .collect::<Vec<(&String, &Module)>>(),
            );

            compile_errors.push_str(&format!(
                "\n{}\n{}\n",
                style("Can't continue... Found a circular dependency in your code:").red(),
                dependency_cycle::format(&cycle)
            ))
        }
        if !compile_errors.is_empty() {
            break;
        };
    }

    Ok((compile_errors, compile_warnings, num_compiled_modules))
}

pub fn compiler_args(
    config: &config::Config,
    root_config: &config::Config,
    ast_path: &Path,
    version: &str,
    file_path: &Path,
    is_interface: bool,
    has_interface: bool,
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
    // if packages are known, we pass a reference here
    // this saves us a scan to find their paths
    packages: &Option<&AHashMap<String, packages::Package>>,
    build_dev_deps: bool,
) -> Vec<String> {
    let bsc_flags = config::flatten_flags(&config.bsc_flags);

    let dependency_paths =
        get_dependency_paths(config, project_root, workspace_root, packages, build_dev_deps);

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

    let uncurried_args = root_config.get_uncurried_args(version);
    let gentype_arg = config.get_gentype_arg();

    let warning_args: Vec<String> = match config.warnings.to_owned() {
        None => vec![],
        Some(warnings) => {
            let warn_number = match warnings.number {
                None => vec![],
                Some(warnings) => {
                    vec!["-w".to_string(), warnings.to_string()]
                }
            };

            let warn_error = match warnings.error {
                Some(config::Error::Catchall(true)) => {
                    vec!["-warn-error".to_string(), "A".to_string()]
                }
                Some(config::Error::Qualified(errors)) => {
                    vec!["-warn-error".to_string(), errors.to_string()]
                }
                _ => vec![],
            };

            [warn_number, warn_error].concat()
        }
    };

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
        let specs = root_config.get_package_specs();

        specs
            .iter()
            .map(|spec| {
                return vec![
                    "-bs-package-output".to_string(),
                    format!(
                        "{}:{}:{}",
                        spec.module,
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
                ];
            })
            .flatten()
            .collect()
    };

    vec![
        namespace_args,
        read_cmi_args,
        vec![
            "-I".to_string(),
            Path::new("..").join("ocaml").to_string_lossy().to_string(),
        ],
        dependency_paths.concat(),
        uncurried_args,
        bsc_flags.to_owned(),
        warning_args,
        gentype_arg,
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
        vec!["-bs-v".to_string(), format!("{}", version)],
        vec![ast_path.to_string_lossy().to_string()],
    ]
    .concat()
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
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
    packages: &Option<&AHashMap<String, packages::Package>>,
    build_dev_deps: bool,
) -> Vec<Vec<String>> {
    let normal_deps = config
        .bs_dependencies
        .clone()
        .unwrap_or_default()
        .into_iter()
        .map(DependentPackage::Normal)
        .collect();
    let dev_deps = if build_dev_deps {
        config
            .bs_dev_dependencies
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
                packages::read_dependency(package_name, project_root, project_root, workspace_root).ok()
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
}

fn compile_file(
    package: &packages::Package,
    root_package: &packages::Package,
    ast_path: &Path,
    module: &Module,
    version: &str,
    is_interface: bool,
    bsc_path: &Path,
    packages: &AHashMap<String, packages::Package>,
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
    build_dev_deps: bool,
) -> Result<Option<String>, String> {
    let ocaml_build_path_abs = package.get_ocaml_build_path();
    let build_path_abs = package.get_build_path();
    let implementation_file_path = match &module.source_type {
        SourceType::SourceFile(ref source_file) => Ok(&source_file.implementation.path),
        sourcetype => Err(format!(
            "Tried to compile a file that is not a source file ({}). Path to AST: {}. ",
            sourcetype,
            ast_path.to_string_lossy()
        )),
    }?;
    let module_name = helpers::file_path_to_module_name(implementation_file_path, &package.namespace);
    let has_interface = module.get_interface().is_some();
    let to_mjs_args = compiler_args(
        &package.config,
        &root_package.config,
        ast_path,
        version,
        implementation_file_path,
        is_interface,
        has_interface,
        project_root,
        workspace_root,
        &Some(packages),
        build_dev_deps,
    );

    let to_mjs = Command::new(bsc_path)
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
            Err(stderr.to_string() + &stdout)
        }
        Err(e) => Err(format!(
            "Could not compile file. Error: {}. Path to AST: {:?}",
            e, ast_path
        )),
        Ok(x) => {
            let err = std::str::from_utf8(&x.stderr)
                .expect("stdout should be non-null")
                .to_string();

            let dir = std::path::Path::new(implementation_file_path).parent().unwrap();

            // perhaps we can do this copying somewhere else
            if !is_interface {
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        // because editor tooling doesn't support namespace entries yet
                        // we just remove the @ for now. This makes sure the editor support
                        // doesn't break
                        .join(format!("{}.cmi", module_name)),
                    ocaml_build_path_abs.join(format!("{}.cmi", module_name)),
                );
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        .join(format!("{}.cmj", module_name)),
                    ocaml_build_path_abs.join(format!("{}.cmj", module_name)),
                );
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        // because editor tooling doesn't support namespace entries yet
                        // we just remove the @ for now. This makes sure the editor support
                        // doesn't break
                        .join(format!("{}.cmt", module_name)),
                    ocaml_build_path_abs.join(format!("{}.cmt", module_name)),
                );
            } else {
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        .join(format!("{}.cmti", module_name)),
                    ocaml_build_path_abs.join(format!("{}.cmti", module_name)),
                );
                let _ = std::fs::copy(
                    package
                        .get_build_path()
                        .join(dir)
                        .join(format!("{}.cmi", module_name)),
                    ocaml_build_path_abs.join(format!("{}.cmi", module_name)),
                );
            }

            match &module.source_type {
                SourceType::SourceFile(SourceFile {
                    interface: Some(Interface { path, .. }),
                    ..
                }) => {
                    // we need to copy the source file to the build directory.
                    // editor tools expects the source file in lib/bs for finding the current package
                    // and in lib/ocaml when referencing modules in other packages
                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        package.get_build_path().join(path),
                    )
                    .expect("copying source file failed");

                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        package
                            .get_ocaml_build_path()
                            .join(std::path::Path::new(path).file_name().unwrap()),
                    )
                    .expect("copying source file failed");
                }
                _ => (),
            }
            match &module.source_type {
                SourceType::SourceFile(SourceFile {
                    implementation: Implementation { path, .. },
                    ..
                }) => {
                    // we need to copy the source file to the build directory.
                    // editor tools expects the source file in lib/bs for finding the current package
                    // and in lib/ocaml when referencing modules in other packages
                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        package.get_build_path().join(path),
                    )
                    .expect("copying source file failed");

                    let _ = std::fs::copy(
                        Path::new(&package.path).join(path),
                        package
                            .get_ocaml_build_path()
                            .join(std::path::Path::new(path).file_name().unwrap()),
                    )
                    .expect("copying source file failed");
                }
                _ => (),
            }

            // copy js file
            root_package.config.get_package_specs().iter().for_each(|spec| {
                if spec.in_source {
                    match &module.source_type {
                        SourceType::SourceFile(SourceFile {
                            implementation: Implementation { path, .. },
                            ..
                        }) => {
                            let source = helpers::get_source_file_from_rescript_file(
                                &Path::new(&package.path).join(path),
                                &root_package.config.get_suffix(spec),
                            );
                            let destination = helpers::get_source_file_from_rescript_file(
                                &package.get_build_path().join(path),
                                &root_package.config.get_suffix(spec),
                            );

                            if source.exists() {
                                let _ =
                                    std::fs::copy(&source, &destination).expect("copying source file failed");
                            }
                        }
                        _ => (),
                    }
                }
            });

            if helpers::contains_ascii_characters(&err) {
                if package.is_pinned_dep || package.is_local_dep {
                    // supress warnings of external deps
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
                let dependent_module = build_state.modules.get(dependent).unwrap();
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
                            let dependent_module = build_state.modules.get(dependent_of_namespace).unwrap();

                            if let (Some(last_compiled_dependent), Some(last_compiled)) =
                                (dependent_module.last_compiled_cmt, module.last_compiled_cmt)
                            {
                                if last_compiled_dependent < last_compiled {
                                    modules_with_expired_deps.insert(dependent.to_string());
                                }
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
