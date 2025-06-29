use super::build_types::*;
use super::packages;
use crate::helpers;
use crate::helpers::emojis::*;
use ahash::AHashSet;
use anyhow::Result;
use console::style;
use rayon::prelude::*;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

fn remove_ast(package: &packages::Package, source_file: &Path) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset(
        package,
        &packages::Namespace::NoNamespace,
        source_file,
        "ast",
    ));
}

fn remove_iast(package: &packages::Package, source_file: &Path) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset(
        package,
        &packages::Namespace::NoNamespace,
        source_file,
        "iast",
    ));
}

fn remove_mjs_file(source_file: &Path, suffix: &String) {
    let _ = std::fs::remove_file(source_file.with_extension(
        // suffix.to_string includes the ., so we need to remove it
        &suffix.to_string()[1..],
    ));
}

fn remove_compile_asset(package: &packages::Package, source_file: &Path, extension: &str) {
    let _ = std::fs::remove_file(helpers::get_compiler_asset(
        package,
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

pub fn remove_compile_assets(package: &packages::Package, source_file: &Path) {
    // optimization
    // only issue cmti if there is an interfacce file
    for extension in &["cmj", "cmi", "cmt", "cmti"] {
        remove_compile_asset(package, source_file, extension);
    }
}

pub fn clean_mjs_files(build_state: &BuildState) {
    // get all rescript file locations
    let rescript_file_locations = build_state
        .modules
        .values()
        .filter_map(|module| match &module.source_type {
            SourceType::SourceFile(source_file) => {
                let package = build_state.packages.get(&module.package_name).unwrap();
                let root_package = build_state
                    .packages
                    .get(&build_state.root_config_name)
                    .expect("Could not find root package");

                Some(
                    root_package
                        .config
                        .get_package_specs()
                        .iter()
                        .filter_map(|spec| {
                            if spec.in_source {
                                Some((
                                    package.path.join(&source_file.implementation.path),
                                    root_package.config.get_suffix(spec),
                                ))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<(PathBuf, String)>>(),
                )
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

pub fn cleanup_previous_build(
    build_state: &mut BuildState,
    compile_assets_state: CompileAssetsState,
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
            remove_compile_assets(package, res_file_location);
            remove_mjs_file(res_file_location, suffix);
            remove_iast(package, res_file_location);
            remove_ast(package, res_file_location);
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
                ..
            } = compile_assets_state
                .ast_modules
                .get(res_file_location)
                .expect("Could not find module name for ast file");
            let module = build_state
                .modules
                .get_mut(module_name)
                .expect("Could not find module for ast file");

            let cmt_last_modified = compile_assets_state.cmt_modules.get(module_name);
            // if there is a new AST but it has not been compiled yet, we mark the module as compile dirty
            // we do this by checking if the cmt file is newer than the AST file. We always compile the
            // interface AND implementation. For some reason the CMI file is not always rewritten if it
            // doesn't have any changes, that's why we just look at the CMT file.
            if let Some(cmt_last_modified) = cmt_last_modified {
                if cmt_last_modified > ast_last_modified && !deleted_interfaces.contains(module_name) {
                    module.compile_dirty = false;
                }
            }

            match &mut module.source_type {
                SourceType::MlMap(_) => unreachable!("MlMap is not matched with a ReScript file"),
                SourceType::SourceFile(source_file) => {
                    if helpers::is_interface_ast_file(ast_file_path) {
                        let interface = source_file
                            .interface
                            .as_mut()
                            .expect("Could not find interface for module");

                        let source_last_modified = interface.last_modified;
                        if ast_last_modified > &source_last_modified {
                            interface.parse_dirty = false;
                        }
                    } else {
                        let implementation = &mut source_file.implementation;
                        let source_last_modified = implementation.last_modified;
                        if ast_last_modified > &source_last_modified
                            && !deleted_interfaces.contains(module_name)
                        {
                            implementation.parse_dirty = false;
                        }
                    }
                }
            }
        });

    compile_assets_state
        .cmi_modules
        .iter()
        .for_each(|(module_name, last_modified)| {
            if let Some(module) = build_state.modules.get_mut(module_name) {
                module.last_compiled_cmi = Some(*last_modified);
            }
        });

    compile_assets_state
        .cmt_modules
        .iter()
        .for_each(|(module_name, last_modified)| {
            if let Some(module) = build_state.modules.get_mut(module_name) {
                module.last_compiled_cmt = Some(*last_modified);
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

fn has_parse_warnings(module: &Module) -> bool {
    matches!(
        &module.source_type,
        SourceType::SourceFile(SourceFile {
            implementation: Implementation {
                parse_state: ParseState::Warning,
                ..
            },
            ..
        }) | SourceType::SourceFile(SourceFile {
            interface: Some(Interface {
                parse_state: ParseState::Warning,
                ..
            }),
            ..
        })
    )
}

fn has_compile_warnings(module: &Module) -> bool {
    matches!(
        &module.source_type,
        SourceType::SourceFile(SourceFile {
            implementation: Implementation {
                compile_state: CompileState::Warning,
                ..
            },
            ..
        }) | SourceType::SourceFile(SourceFile {
            interface: Some(Interface {
                compile_state: CompileState::Warning,
                ..
            }),
            ..
        })
    )
}

pub fn cleanup_after_build(build_state: &BuildState) {
    build_state.modules.par_iter().for_each(|(_module_name, module)| {
        let package = build_state.get_package(&module.package_name).unwrap();
        if has_parse_warnings(module) {
            if let SourceType::SourceFile(source_file) = &module.source_type {
                remove_iast(package, &source_file.implementation.path);
                remove_ast(package, &source_file.implementation.path);
            }
        }
        if has_compile_warnings(module) {
            // only retain AST file if the compilation doesn't have warnings, we remove the AST in favor
            // of the CMI/CMT/CMJ files because if we delete these, the editor tooling doesn't
            // work anymore. If we remove the intermediate AST file, the editor tooling will
            // work, and we have an indication that we need to recompile the file.
            //
            // Recompiling this takes a bit more time, because we have to parse again, but
            // if we have warnings it's usually not a lot of files so the additional
            // latency shouldn't be too bad
            match &module.source_type {
                SourceType::SourceFile(source_file) => {
                    // we only clean the ast here, this will cause the file to be recompiled
                    // (and thus keep showing the warning), but it will keep the cmi file, so that we don't
                    // unecessary mark all the dependents as dirty, when there is no change in the interface
                    remove_ast(package, &source_file.implementation.path);
                    remove_iast(package, &source_file.implementation.path);
                }
                SourceType::MlMap(_) => (),
            }
        }
    });
}

pub fn clean(
    path: &Path,
    show_progress: bool,
    bsc_path: Option<PathBuf>,
    snapshot_output: bool,
) -> Result<()> {
    let project_root = helpers::get_abs_path(path);
    let workspace_root = helpers::get_workspace_root(&project_root);
    let packages = packages::make(
        &None,
        &project_root,
        &workspace_root,
        show_progress,
        // Build the package tree with dev dependencies.
        // They should always be cleaned if they are there.
        true,
    )?;
    let root_config_name = packages::read_package_name(&project_root)?;
    let bsc_path = match bsc_path {
        Some(bsc_path) => helpers::get_abs_path(&bsc_path),
        None => helpers::get_bsc(&project_root, &workspace_root),
    };

    let rescript_version = helpers::get_rescript_version(&bsc_path);

    let timing_clean_compiler_assets = Instant::now();
    if !snapshot_output && show_progress {
        print!(
            "{} {}Cleaning compiler assets...",
            style("[1/2]").bold().dim(),
            SWEEP
        );
        let _ = std::io::stdout().flush();
    };
    packages.iter().for_each(|(_, package)| {
        if show_progress {
            if snapshot_output {
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

        let path_str = package.get_build_path();
        let path = std::path::Path::new(&path_str);
        let _ = std::fs::remove_dir_all(path);

        let path_str = package.get_ocaml_build_path();
        let path = std::path::Path::new(&path_str);
        let _ = std::fs::remove_dir_all(path);
    });
    let timing_clean_compiler_assets_elapsed = timing_clean_compiler_assets.elapsed();

    if !snapshot_output && show_progress {
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
    if !snapshot_output && show_progress {
        println!("{} {}Cleaning mjs files...", style("[2/2]").bold().dim(), SWEEP);
        let _ = std::io::stdout().flush();
    }
    let mut build_state = BuildState::new(
        project_root.to_owned(),
        root_config_name,
        packages,
        workspace_root,
        rescript_version,
        bsc_path,
    );
    packages::parse_packages(&mut build_state);
    clean_mjs_files(&build_state);
    let timing_clean_mjs_elapsed = timing_clean_mjs.elapsed();

    if !snapshot_output && show_progress {
        println!(
            "{}{} {}Cleaned mjs files in {:.2}s",
            LINE_CLEAR,
            style("[2/2]").bold().dim(),
            SWEEP,
            timing_clean_mjs_elapsed.as_secs_f64()
        );
        let _ = std::io::stdout().flush();
    }

    Ok(())
}
