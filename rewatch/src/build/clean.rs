use super::build_types::*;
use super::packages;
use crate::build;
use crate::build::packages::Package;
use crate::config::Config;
use crate::helpers;
use crate::helpers::emojis::*;
use crate::project_context::ProjectContext;
use ahash::AHashSet;
use anyhow::Result;
use console::style;
use rayon::prelude::*;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::instrument;

fn remove_ast(ocaml_build_path: &Path, source_file: &Path) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset_in(
        ocaml_build_path,
        &packages::Namespace::NoNamespace,
        source_file,
        "ast",
    ));
}

fn remove_iast(ocaml_build_path: &Path, source_file: &Path) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset_in(
        ocaml_build_path,
        &packages::Namespace::NoNamespace,
        source_file,
        "iast",
    ));
}

fn remove_mjs_file(source_file: &Path, suffix: &str) {
    let _ = std::fs::remove_file(source_file.with_extension(
        // suffix.to_string includes the ., so we need to remove it
        &suffix[1..],
    ));
}

/// Remove stale source file copies (.res/.resi) from the flat build dir (e.g. lib/lsp-ocaml/)
/// and the nested build dir (e.g. lib/lsp/src/). These copies are created during
/// TypecheckAndEmit builds but are not tracked by the compile assets scan.
fn remove_source_copies(
    package: &packages::Package,
    ocaml_build_path: &Path,
    source_file: &Path,
    output: OutputTarget,
) {
    if let Some(filename) = source_file.file_name() {
        // Flat dir: lib/lsp-ocaml/App.res
        let _ = std::fs::remove_file(ocaml_build_path.join(filename));
    }
    // Nested dir: lib/lsp/src/App.res (relative to package root)
    if let Ok(relative) = source_file.strip_prefix(&package.path) {
        let _ = std::fs::remove_file(package.get_build_path_for_output(output).join(relative));
    }
}

fn remove_compile_asset(
    package: &packages::Package,
    ocaml_build_path: &Path,
    source_file: &Path,
    extension: &str,
) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset_in(
        ocaml_build_path,
        &package.namespace,
        source_file,
        extension,
    ));
    let _ = std::fs::remove_file(helpers::get_bs_compiler_asset(
        package,
        &package.namespace,
        source_file,
        extension,
    ));
}

pub fn remove_compile_assets(package: &packages::Package, ocaml_build_path: &Path, source_file: &Path) {
    // optimization
    // only issue cmti if there is an interfacce file
    for extension in &["cmj", "cmi", "cmt", "cmti"] {
        remove_compile_asset(package, ocaml_build_path, source_file, extension);
    }
}

fn clean_source_files(build_state: &BuildState, root_config: &Config) {
    // get all rescript file locations
    let rescript_file_locations = build_state
        .modules
        .values()
        .filter_map(|module| match module {
            Module::SourceFile(sf_module) => {
                build_state.packages.get(&sf_module.package_name).map(|package| {
                    let source_file = &sf_module.source_file;
                    root_config
                        .get_package_specs()
                        .into_iter()
                        .filter_map(|spec| {
                            if spec.in_source {
                                Some((
                                    package.path.join(&source_file.implementation.path),
                                    match spec.suffix {
                                        None => root_config.get_suffix(&spec),
                                        Some(suffix) => suffix,
                                    },
                                ))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<(PathBuf, String)>>()
                })
            }
            _ => None,
        })
        .flatten()
        .collect::<Vec<(PathBuf, String)>>();

    rescript_file_locations
        .par_iter()
        .for_each(|(rescript_file_location, suffix)| remove_mjs_file(rescript_file_location, suffix));
}

// TODO: change to scan_previous_build => CompileAssetsState
// and then do cleanup on that state (for instance remove all .mjs files that are not in the state)

#[instrument(name = "clean.cleanup_previous_build", skip_all)]
pub fn cleanup_previous_build(
    build_state: &mut BuildCommandState,
    compile_assets_state: CompileAssetsState,
    output: OutputTarget,
) -> (usize, usize) {
    // delete the .mjs file which appear in our previous compile assets
    // but does not exists anymore
    // delete the compiler assets for which modules we can't find a rescript file
    // location of rescript file is in the AST
    // delete the .mjs file for which we DO have a compiler asset, but don't have a
    // rescript file anymore (path is found in the .ast file)
    let diff = compile_assets_state
        .ast_rescript_file_locations
        .difference(&compile_assets_state.rescript_file_locations)
        .collect::<Vec<&PathBuf>>();

    let diff_len = diff.len();

    let deleted_interfaces = diff
        .par_iter()
        .map(|res_file_location| {
            let AstModule {
                module_name,
                package_name,
                ast_file_path,
                suffix,
                ..
            } = compile_assets_state
                .ast_modules
                .get(*res_file_location)
                .expect("Could not find module name for ast file");

            let package = build_state
                .packages
                .get(package_name)
                .expect("Could not find package");
            let ocaml_build_path = package.get_ocaml_build_path_for_output(output);
            remove_compile_assets(package, &ocaml_build_path, res_file_location);
            remove_mjs_file(res_file_location, suffix);
            remove_iast(&ocaml_build_path, res_file_location);
            remove_ast(&ocaml_build_path, res_file_location);
            remove_source_copies(package, &ocaml_build_path, res_file_location, output);
            match helpers::get_extension(ast_file_path).as_str() {
                "iast" => Some(module_name.to_owned()),
                "ast" => None,
                _ => None,
            }
        })
        .collect::<Vec<Option<String>>>()
        .iter()
        .filter_map(|module_name| module_name.to_owned())
        .collect::<AHashSet<String>>();

    compile_assets_state
        .ast_rescript_file_locations
        .intersection(&compile_assets_state.rescript_file_locations)
        .for_each(|res_file_location| {
            let AstModule {
                module_name,
                last_modified: ast_last_modified,
                ast_file_path,
                package_name: ast_package_name,
                ..
            } = compile_assets_state
                .ast_modules
                .get(res_file_location)
                .expect("Could not find module name for ast file");
            let package_path = build_state
                .build_state
                .packages
                .get(ast_package_name)
                .map(|pkg| pkg.path.clone());
            let module = build_state
                .build_state
                .modules
                .get_mut(module_name)
                .expect("Could not find module for ast file");

            let sf_module = match module {
                Module::SourceFile(sf) => sf,
                Module::MlMap(_) => unreachable!("MlMap is not matched with a ReScript file"),
            };

            // Determine source timestamp and whether the AST is fresh relative to source.
            // For interface AST files (.iast) we check the interface's timestamp;
            // for implementation AST files (.ast) we check the implementation's timestamp.
            let is_iast = helpers::is_interface_ast_file(ast_file_path);
            let ast_is_fresh = if is_iast {
                let iface = sf_module
                    .source_file
                    .interface
                    .as_ref()
                    .expect("Could not find interface for module");
                ast_last_modified > &iface.last_modified
            } else {
                let impl_modified = sf_module.source_file.implementation.last_modified;
                ast_last_modified > &impl_modified && !deleted_interfaces.contains(module_name)
            };

            if ast_is_fresh {
                // Promote from Dirty → Parsed since the AST on disk is fresh.
                // Guard on is_dirty() so a second .ast/.iast pass for the same
                // module doesn't overwrite a stage that was already restored.
                if sf_module.compilation_stage().is_dirty() && !deleted_interfaces.contains(module_name) {
                    // Compute implementation hashes.
                    // res_file_location is an absolute path. When we're processing the .iast,
                    // it points to the .resi — resolve the .res path via the package root.
                    let implementation_source_hash = if is_iast {
                        package_path.as_ref().and_then(|pp| {
                            helpers::compute_file_hash(&pp.join(&sf_module.source_file.implementation.path))
                        })
                    } else {
                        helpers::compute_file_hash(res_file_location)
                    };
                    let implementation_ast_hash = if is_iast {
                        // We're processing the .iast; find the .ast file path
                        compile_assets_state
                            .ast_modules
                            .iter()
                            .find(|(_, m)| {
                                &m.module_name == module_name
                                    && !helpers::is_interface_ast_file(&m.ast_file_path)
                            })
                            .and_then(|(_, m)| helpers::compute_file_hash(&m.ast_file_path))
                    } else {
                        helpers::compute_file_hash(ast_file_path)
                    };

                    // Compute interface hashes (if this module has an interface).
                    // When we're processing the .ast, resolve the .resi path via the package root.
                    let (interface_source_hash, interface_ast_hash) =
                        if let Some(interface) = &sf_module.source_file.interface {
                            let interface_source_hash = if is_iast {
                                helpers::compute_file_hash(res_file_location)
                            } else {
                                package_path
                                    .as_ref()
                                    .and_then(|pp| helpers::compute_file_hash(&pp.join(&interface.path)))
                            };
                            let interface_ast_hash = if is_iast {
                                helpers::compute_file_hash(ast_file_path)
                            } else {
                                compile_assets_state
                                    .ast_modules
                                    .iter()
                                    .find(|(_, m)| {
                                        &m.module_name == module_name
                                            && helpers::is_interface_ast_file(&m.ast_file_path)
                                    })
                                    .and_then(|(_, m)| helpers::compute_file_hash(&m.ast_file_path))
                            };
                            (interface_source_hash, interface_ast_hash)
                        } else {
                            (None, None)
                        };

                    if let (Some(ish), Some(iah)) = (implementation_source_hash, implementation_ast_hash) {
                        sf_module.set_compilation_stage(CompilationStage::Parsed {
                            implementation_source_hash: ish,
                            implementation_ast_hash: iah,
                            interface_source_hash,
                            interface_ast_hash,
                        });

                        // If compilation artifacts are also fresh (.cmt newer
                        // than .ast), restore through TypeChecked and Built.
                        let cmt_last_modified = compile_assets_state.cmt_modules.get(module_name);
                        let cmj_exists = compile_assets_state.cmj_modules.contains_key(module_name);
                        if let Some(cmt_last_modified) = cmt_last_modified
                            && cmt_last_modified > ast_last_modified
                        {
                            let (cmi_hash, cmt_hash, cmj_hash) = if let Some(pkg) =
                                build_state.build_state.packages.get(&sf_module.package_name)
                            {
                                let ocaml_path = pkg.get_ocaml_build_path_for_output(output);
                                let impl_path = &sf_module.source_file.implementation.path;
                                (
                                    helpers::compute_file_hash(&helpers::get_compiler_asset_in(
                                        &ocaml_path,
                                        &pkg.namespace,
                                        impl_path,
                                        "cmi",
                                    )),
                                    helpers::compute_file_hash(&helpers::get_compiler_asset_in(
                                        &ocaml_path,
                                        &pkg.namespace,
                                        impl_path,
                                        "cmt",
                                    )),
                                    if cmj_exists {
                                        helpers::compute_file_hash(&helpers::get_compiler_asset_in(
                                            &ocaml_path,
                                            &pkg.namespace,
                                            impl_path,
                                            "cmj",
                                        ))
                                    } else {
                                        None
                                    },
                                )
                            } else {
                                (None, None, None)
                            };

                            if let (Some(cmi), Some(cmt)) = (cmi_hash, cmt_hash) {
                                sf_module.set_compilation_stage(CompilationStage::TypeChecked {
                                    implementation_source_hash: ish,
                                    implementation_ast_hash: iah,
                                    interface_source_hash,
                                    interface_ast_hash,
                                    cmi_hash: cmi,
                                    cmt_hash: cmt,
                                    compiled_at: *cmt_last_modified,
                                });
                                if cmj_exists && let Some(cmj) = cmj_hash {
                                    sf_module.set_compilation_stage(CompilationStage::Built {
                                        implementation_source_hash: ish,
                                        implementation_ast_hash: iah,
                                        interface_source_hash,
                                        interface_ast_hash,
                                        cmi_hash: cmi,
                                        cmt_hash: cmt,
                                        cmj_hash: cmj,
                                        compiled_at: *cmt_last_modified,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        });

    let ast_module_names = compile_assets_state
        .ast_modules
        .values()
        .filter_map(
            |AstModule {
                 module_name,
                 ast_file_path,
                 ..
             }| {
                match helpers::get_extension(ast_file_path).as_str() {
                    "iast" => None,
                    "ast" => Some(module_name),
                    _ => None,
                }
            },
        )
        .collect::<AHashSet<&String>>();

    let all_module_names = build_state.modules.keys().collect::<AHashSet<&String>>();

    let deleted_module_names = ast_module_names
        .difference(&all_module_names)
        .flat_map(|module_name| {
            // if the module is a namespace, we need to mark the whole namespace as dirty when a module has been deleted
            if let Some(namespace) = helpers::get_namespace_from_module_name(module_name) {
                return vec![namespace, module_name.to_string()];
            }
            vec![module_name.to_string()]
        })
        .collect::<AHashSet<String>>();

    build_state.deleted_modules = deleted_module_names;

    (diff_len, compile_assets_state.ast_rescript_file_locations.len())
}

fn has_parse_warnings(sf_module: &SourceFileModule) -> bool {
    matches!(
        sf_module.source_file.implementation.parse_state,
        ParseState::Warning
    ) || sf_module
        .source_file
        .interface
        .as_ref()
        .is_some_and(|i| matches!(i.parse_state, ParseState::Warning))
}

fn has_compile_warnings(sf_module: &SourceFileModule) -> bool {
    matches!(
        sf_module.source_file.implementation.compile_state,
        CompileState::Warning
    ) || sf_module
        .source_file
        .interface
        .as_ref()
        .is_some_and(|i| matches!(i.compile_state, CompileState::Warning))
}

pub fn cleanup_after_build(build_state: &BuildCommandState, output: OutputTarget) {
    build_state.modules.par_iter().for_each(|(_module_name, module)| {
        let Module::SourceFile(sf_module) = module else {
            return;
        };
        let package = build_state.get_package(&sf_module.package_name).unwrap();
        let ocaml_build_path = package.get_ocaml_build_path_for_output(output);
        if has_parse_warnings(sf_module) {
            remove_iast(&ocaml_build_path, &sf_module.source_file.implementation.path);
            remove_ast(&ocaml_build_path, &sf_module.source_file.implementation.path);
        }
        if has_compile_warnings(sf_module) {
            // Only retain AST file if the compilation doesn't have warnings.
            // We remove the AST in favor of the CMI/CMT/CMJ files because if
            // we delete these, the editor tooling doesn't work anymore. If we
            // remove the intermediate AST file, the editor tooling will work,
            // and we have an indication that we need to recompile the file.
            //
            // Recompiling this takes a bit more time, because we have to parse
            // again, but if we have warnings it's usually not a lot of files
            // so the additional latency shouldn't be too bad.
            //
            // We only clean the ast here — this will cause the file to be
            // recompiled (and thus keep showing the warning), but it will keep
            // the cmi file so that we don't unnecessarily mark all the
            // dependents as dirty when there is no change in the interface.
            remove_ast(&ocaml_build_path, &sf_module.source_file.implementation.path);
            remove_iast(&ocaml_build_path, &sf_module.source_file.implementation.path);
        }
    });
}

#[instrument(name = "clean.clean", skip_all)]
pub fn clean(path: &Path, show_progress: bool, plain_output: bool) -> Result<()> {
    let project_context = ProjectContext::new(path)?;
    let compiler_info = build::get_compiler_info(&project_context)?;
    let packages = packages::make(&None, &project_context, show_progress)?;

    let timing_clean_compiler_assets = Instant::now();
    if !plain_output && show_progress {
        print!(
            "{} {}Cleaning compiler assets...",
            style("[1/2]").bold().dim(),
            SWEEP
        );
        let _ = std::io::stdout().flush();
    };

    for (_, package) in &packages {
        clean_package(show_progress, plain_output, package)
    }

    let timing_clean_compiler_assets_elapsed = timing_clean_compiler_assets.elapsed();

    if !plain_output && show_progress {
        println!(
            "{}{} {}Cleaned compiler assets in {:.2}s",
            LINE_CLEAR,
            style("[1/2]").bold().dim(),
            SWEEP,
            timing_clean_compiler_assets_elapsed.as_secs_f64()
        );
        let _ = std::io::stdout().flush();
    }

    let timing_clean_mjs = Instant::now();
    let mut build_state = BuildState::new(project_context, packages, compiler_info);
    packages::parse_packages(&mut build_state, OutputTarget::Standard, CompileMode::FullCompile)?;
    let root_config = build_state.get_root_config();
    let suffix_for_print = match root_config.package_specs {
        None => match &root_config.suffix {
            None => String::from(".js"),
            Some(suffix) => suffix.clone(),
        },
        Some(_) => root_config
            .get_package_specs()
            .into_iter()
            .filter_map(|spec| {
                if spec.in_source {
                    spec.suffix.or_else(|| root_config.suffix.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<String>>()
            .join(", "),
    };

    if !plain_output && show_progress {
        print!(
            "{} {}Cleaning {} files...",
            style("[2/2]").bold().dim(),
            SWEEP,
            suffix_for_print
        );
        let _ = std::io::stdout().flush();
    }

    clean_source_files(&build_state, root_config);
    let timing_clean_mjs_elapsed = timing_clean_mjs.elapsed();

    if !plain_output && show_progress {
        println!(
            "{}{} {}Cleaned {} files in {:.2}s",
            LINE_CLEAR,
            style("[2/2]").bold().dim(),
            SWEEP,
            suffix_for_print,
            timing_clean_mjs_elapsed.as_secs_f64()
        );
        let _ = std::io::stdout().flush();
    }

    Ok(())
}

pub fn clean_package(show_progress: bool, plain_output: bool, package: &Package) {
    if show_progress {
        if plain_output {
            println!("Cleaning {}", package.name)
        } else {
            print!(
                "{}{} {}Cleaning {}...",
                LINE_CLEAR,
                style("[1/2]").bold().dim(),
                SWEEP,
                package.name
            );
        }
        let _ = std::io::stdout().flush();
    }

    // Clean standard build artifacts (not LSP — a running LSP server
    // manages its own lib/lsp/ and lib/lsp-ocaml/ directories).
    let _ = std::fs::remove_dir_all(package.get_build_path());
    let _ = std::fs::remove_dir_all(package.get_ocaml_build_path());

    // remove the per-package compiler metadata file so that a subsequent build writes fresh metadata
    let _ = std::fs::remove_file(package.get_compiler_info_path());
}

/// Clean only the build artifacts for a specific output target.
/// Used by verify_compiler_info to avoid destroying the other target's artifacts.
pub fn clean_package_for_output(package: &Package, output: OutputTarget) {
    let _ = std::fs::remove_dir_all(package.get_build_path_for_output(output));
    let _ = std::fs::remove_dir_all(package.get_ocaml_build_path_for_output(output));
    let _ = std::fs::remove_file(package.get_compiler_info_path_for_output(output));
}
