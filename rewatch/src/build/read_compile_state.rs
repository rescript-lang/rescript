use super::build_types::*;
use super::packages;
use crate::helpers;
use ahash::{AHashMap, AHashSet};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

type CompileAsset = (
    PathBuf,
    SystemTime,
    String,
    String,
    PathBuf,
    bool,
    packages::Namespace,
    bool,
);

pub fn read(build_state: &mut BuildCommandState) -> anyhow::Result<CompileAssetsState> {
    let mut ast_modules: AHashMap<PathBuf, AstModule> = AHashMap::new();
    let mut cmi_modules: AHashMap<String, SystemTime> = AHashMap::new();
    let mut cmt_modules: AHashMap<String, SystemTime> = AHashMap::new();
    let mut ast_rescript_file_locations = AHashSet::new();

    let mut rescript_file_locations = build_state
        .modules
        .values()
        .filter_map(|module| match &module.source_type {
            SourceType::SourceFile(source_file) => {
                let package = build_state.packages.get(&module.package_name).unwrap();

                Some(PathBuf::from(&package.path).join(&source_file.implementation.path))
            }
            _ => None,
        })
        .collect::<AHashSet<PathBuf>>();

    rescript_file_locations.extend(
        build_state
            .modules
            .values()
            .filter_map(|module| {
                let package = build_state.packages.get(&module.package_name).unwrap();
                module
                    .get_interface()
                    .as_ref()
                    .map(|interface| package.path.join(&interface.path))
            })
            .collect::<AHashSet<PathBuf>>(),
    );

    // scan all ast files in all packages
    let compile_assets = build_state
        .packages
        .par_iter()
        .map(|(_, package)| {
            let mut package_assets = Vec::new();
            collect_compile_assets(package, &package.get_ocaml_build_path(), &mut package_assets);
            package_assets
        })
        .flatten()
        .collect::<Vec<CompileAsset>>();

    let root_config = build_state.get_root_config();

    compile_assets.iter().for_each(
        |(
            path,
            last_modified,
            extension,
            package_name,
            package_path,
            package_multi_entry,
            package_namespace,
            package_is_root,
        )| {
            match extension.as_str() {
                "iast" | "ast" => {
                    if let Some(res_file_path_buf) = get_res_path_from_ast(path) {
                        let source_path = res_file_path_buf
                            .strip_prefix(package_path)
                            .unwrap_or(&res_file_path_buf);
                        let module_name = helpers::file_path_to_module_key(
                            source_path,
                            package_namespace,
                            package_name,
                            *package_multi_entry,
                        );
                        let _ = ast_modules.insert(
                            res_file_path_buf.clone(),
                            AstModule {
                                module_name,
                                package_name: package_name.to_owned(),
                                namespace: package_namespace.to_owned(),
                                last_modified: last_modified.to_owned(),
                                ast_file_path: path.to_path_buf(),
                                is_root: *package_is_root,
                                suffix: root_config
                                    .get_suffix(root_config.get_package_specs().first().unwrap()),
                            },
                        );
                        let _ = ast_rescript_file_locations.insert(res_file_path_buf);
                    }
                }
                "cmi" => {
                    let module_name = helpers::file_path_to_module_name(
                        path,
                        // we don't want to include a namespace here because the CMI file
                        // already includes a namespace
                        &packages::Namespace::NoNamespace,
                    );
                    cmi_modules.insert(module_name, last_modified.to_owned());
                }
                "cmt" => {
                    let module_name = helpers::file_path_to_module_name(
                        path,
                        // we don't want to include a namespace here because the CMI file
                        // already includes a namespace
                        &packages::Namespace::NoNamespace,
                    );
                    cmt_modules.insert(module_name, last_modified.to_owned());
                }
                _ => {
                    // println!("other extension: {:?}", other);
                }
            }
        },
    );

    Ok(CompileAssetsState {
        ast_modules,
        cmi_modules,
        cmt_modules,
        ast_rescript_file_locations,
        rescript_file_locations,
    })
}

fn get_res_path_from_ast(ast_file: &Path) -> Option<PathBuf> {
    if let Ok(lines) = helpers::read_lines(ast_file) {
        // we skip the first line with is some null characters
        // the following lines in the AST are the dependency modules
        // we stop when we hit a line that is an absolute path, this is the path of the file.
        // this is the point where the dependencies end and the actual AST starts
        for line in lines.skip(1) {
            match line {
                Ok(line) if Path::new(&line).is_absolute() => return Some(PathBuf::from(line)),
                _ => (),
            }
        }
    }
    None
}

fn collect_compile_assets(package: &packages::Package, dir: &Path, assets: &mut Vec<CompileAsset>) {
    let Ok(read_dir) = fs::read_dir(dir) else {
        return;
    };

    for entry in read_dir.flatten() {
        let path = entry.path();
        let Ok(metadata) = entry.metadata() else {
            continue;
        };

        if metadata.is_dir() {
            collect_compile_assets(package, &path, assets);
            continue;
        }

        let Some(extension) = path.extension().and_then(|extension| extension.to_str()) else {
            continue;
        };
        let extension = extension.to_owned();

        match extension.as_str() {
            "iast" | "ast" | "cmi" | "cmt" => assets.push((
                path,
                metadata.modified().unwrap(),
                extension,
                package.name.to_owned(),
                package.path.to_owned(),
                package.config.is_multi_entry_enabled(),
                package.namespace.to_owned(),
                package.is_root,
            )),
            _ => {}
        }
    }
}
