use super::build_types::*;
use super::logs;
use super::namespaces;
use super::packages;
use crate::config;
use crate::config::OneOrMore;
use crate::helpers;
use ahash::AHashSet;
use log::debug;
use rayon::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn generate_asts(
    build_state: &mut BuildState,
    inc: impl Fn() + std::marker::Sync,
) -> Result<String, String> {
    let mut has_failure = false;
    let mut stderr = "".to_string();

    build_state
        .modules
        .par_iter()
        .map(|(module_name, module)| {
            debug!("Generating AST for module: {}", module_name);
            let package = build_state
                .get_package(&module.package_name)
                .expect("Package not found");
            match &module.source_type {
                SourceType::MlMap(_mlmap) => {
                    let path = package.get_mlmap_path();
                    (module_name.to_owned(), Ok((path, None)), Ok(None), false)
                }

                SourceType::SourceFile(source_file) => {
                    let root_package = build_state.get_package(&build_state.root_config_name).unwrap();

                    let (ast_result, iast_result, dirty) = if source_file.implementation.parse_dirty
                        || source_file
                            .interface
                            .as_ref()
                            .map(|i| i.parse_dirty)
                            .unwrap_or(false)
                    {
                        inc();
                        let ast_result = generate_ast(
                            package.to_owned(),
                            root_package.to_owned(),
                            &source_file.implementation.path.to_owned(),
                            &build_state.rescript_version,
                            &build_state.bsc_path,
                            &build_state.workspace_root,
                        );

                        let iast_result = match source_file.interface.as_ref().map(|i| i.path.to_owned()) {
                            Some(interface_file_path) => generate_ast(
                                package.to_owned(),
                                root_package.to_owned(),
                                &interface_file_path.to_owned(),
                                &build_state.rescript_version,
                                &build_state.bsc_path,
                                &build_state.workspace_root,
                            )
                            .map(Some),
                            _ => Ok(None),
                        };

                        (ast_result, iast_result, true)
                    } else {
                        (
                            Ok((
                                helpers::get_basename(&source_file.implementation.path).to_string() + ".ast",
                                None,
                            )),
                            Ok(source_file
                                .interface
                                .as_ref()
                                .map(|i| (helpers::get_basename(&i.path).to_string() + ".iast", None))),
                            false,
                        )
                    };

                    (module_name.to_owned(), ast_result, iast_result, dirty)
                }
            }
        })
        .collect::<Vec<(
            String,
            Result<(String, Option<helpers::StdErr>), String>,
            Result<Option<(String, Option<helpers::StdErr>)>, String>,
            bool,
        )>>()
        .into_iter()
        .for_each(|(module_name, ast_result, iast_result, is_dirty)| {
            if let Some(module) = build_state.modules.get_mut(&module_name) {
                // if the module is dirty, mark it also compile_dirty
                // do NOT set to false if the module is not parse_dirty, it needs to keep
                // the compile_dirty flag if it was set before
                if is_dirty {
                    module.compile_dirty = true;
                }
                let package = build_state
                    .packages
                    .get(&module.package_name)
                    .expect("Package not found");
                if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                    // We get Err(x) when there is a parse error. When it's Ok(_, Some(
                    // stderr_warnings )), the outputs are warnings
                    match ast_result {
                        // In case of a pinned (internal) dependency, we want to keep on
                        // propagating the warning with every compile. So we mark it as dirty for
                        // the next round
                        Ok((_path, Some(stderr_warnings))) if package.is_pinned_dep => {
                            source_file.implementation.parse_state = ParseState::Warning;
                            source_file.implementation.parse_dirty = true;
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.parse_dirty = false;
                            }
                            logs::append(package, &stderr_warnings);
                            stderr.push_str(&stderr_warnings);
                        }
                        Ok((_path, Some(_))) | Ok((_path, None)) => {
                            // If we do have stderr_warnings here, the file is not a pinned
                            // dependency (so some external dep). We can ignore those
                            source_file.implementation.parse_state = ParseState::Success;
                            source_file.implementation.parse_dirty = false;
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.parse_dirty = false;
                            }
                        }
                        Err(err) => {
                            // Some compilation error
                            source_file.implementation.parse_state = ParseState::ParseError;
                            source_file.implementation.parse_dirty = true;
                            logs::append(package, &err);
                            has_failure = true;
                            stderr.push_str(&err);
                        }
                    };

                    // We get Err(x) when there is a parse error. When it's Ok(_, Some(( _path,
                    // stderr_warnings ))), the outputs are warnings
                    match iast_result {
                        // In case of a pinned (internal) dependency, we want to keep on
                        // propagating the warning with every compile. So we mark it as dirty for
                        // the next round
                        Ok(Some((_path, Some(stderr_warnings)))) if package.is_pinned_dep => {
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.parse_state = ParseState::Warning;
                                interface.parse_dirty = true;
                            }
                            logs::append(package, &stderr_warnings);
                            stderr.push_str(&stderr_warnings);
                        }
                        Ok(Some((_, None))) | Ok(Some((_, Some(_)))) => {
                            // If we do have stderr_warnings here, the file is not a pinned
                            // dependency (so some external dep). We can ignore those
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.parse_state = ParseState::Success;
                                interface.parse_dirty = false;
                            }
                        }
                        Err(err) => {
                            // Some compilation error
                            if let Some(interface) = source_file.interface.as_mut() {
                                interface.parse_state = ParseState::ParseError;
                                interface.parse_dirty = true;
                            }
                            logs::append(package, &err);
                            has_failure = true;
                            stderr.push_str(&err);
                        }
                        Ok(None) => {
                            // The file had no interface file associated
                        }
                    }
                };
            }
        });

    // compile the mlmaps of dirty modules
    // first collect dirty packages
    let dirty_packages = build_state
        .modules
        .iter()
        .filter(|(_, module)| module.compile_dirty)
        .map(|(_, module)| module.package_name.clone())
        .collect::<AHashSet<String>>();

    build_state.modules.iter_mut().for_each(|(module_name, module)| {
        let is_dirty = match &module.source_type {
            SourceType::MlMap(_) => {
                if dirty_packages.contains(&module.package_name) {
                    let package = build_state
                        .packages
                        .get(&module.package_name)
                        .expect("Package not found");
                    // probably better to do this in a different function
                    // specific to compiling mlmaps
                    let compile_path = package.get_mlmap_compile_path();
                    let mlmap_hash = helpers::compute_file_hash(&compile_path);
                    namespaces::compile_mlmap(package, module_name, &build_state.bsc_path);
                    let mlmap_hash_after = helpers::compute_file_hash(&compile_path);

                    let suffix = package
                        .namespace
                        .to_suffix()
                        .expect("namespace should be set for mlmap module");
                    // copy the mlmap to the bs build path for editor tooling
                    let base_build_path = package.get_build_path() + "/" + &suffix;
                    let base_bs_build_path = package.get_bs_build_path() + "/" + &suffix;
                    let _ = std::fs::copy(
                        base_build_path.to_string() + ".cmi",
                        base_bs_build_path.to_string() + ".cmi",
                    );
                    let _ = std::fs::copy(
                        base_build_path.to_string() + ".cmt",
                        base_bs_build_path.to_string() + ".cmt",
                    );
                    let _ = std::fs::copy(
                        base_build_path.to_string() + ".cmj",
                        base_bs_build_path.to_string() + ".cmj",
                    );
                    let _ = std::fs::copy(
                        base_build_path.to_string() + ".mlmap",
                        base_bs_build_path.to_string() + ".mlmap",
                    );
                    match (mlmap_hash, mlmap_hash_after) {
                        (Some(digest), Some(digest_after)) => !digest.eq(&digest_after),
                        _ => true,
                    }
                } else {
                    false
                }
            }
            _ => false,
        };
        if is_dirty {
            module.compile_dirty = is_dirty;
        }
    });

    if has_failure {
        Err(stderr)
    } else {
        Ok(stderr)
    }
}

pub fn parser_args(
    config: &config::Config,
    root_config: &config::Config,
    filename: &str,
    version: &str,
    workspace_root: &Option<String>,
    root_path: &str,
    contents: &str,
) -> (String, Vec<String>) {
    let file = &filename.to_string();
    let path = PathBuf::from(filename);
    let ast_extension = path_to_ast_extension(&path);
    let ast_path = (helpers::get_basename(&file.to_string()).to_owned()) + ast_extension;
    let ppx_flags = config::flatten_ppx_flags(
        &if let Some(workspace_root) = workspace_root {
            format!("{}/node_modules", &workspace_root)
        } else {
            format!("{}/node_modules", &root_path)
        },
        &filter_ppx_flags(&config.ppx_flags, contents),
        &config.name,
    );
    let jsx_args = root_config.get_jsx_args();
    let jsx_module_args = root_config.get_jsx_module_args();
    let jsx_mode_args = root_config.get_jsx_mode_args();
    let uncurried_args = root_config.get_uncurried_args(version);
    let bsc_flags = config::flatten_flags(&config.bsc_flags);

    let file = "../../".to_string() + file;
    (
        ast_path.to_string(),
        [
            vec!["-bs-v".to_string(), format!("{}", version)],
            ppx_flags,
            jsx_args,
            jsx_module_args,
            jsx_mode_args,
            uncurried_args,
            bsc_flags,
            vec![
                "-absname".to_string(),
                "-bs-ast".to_string(),
                "-o".to_string(),
                ast_path.to_string(),
                file,
            ],
        ]
        .concat(),
    )
}

fn generate_ast(
    package: packages::Package,
    root_package: packages::Package,
    filename: &str,
    version: &str,
    bsc_path: &str,
    workspace_root: &Option<String>,
) -> Result<(String, Option<helpers::StdErr>), String> {
    let file_path = PathBuf::from(&package.path).join(filename);
    let contents = helpers::read_file(&file_path).expect("Error reading file");

    let build_path_abs = package.get_build_path();
    let (ast_path, parser_args) = parser_args(
        &package.config,
        &root_package.config,
        filename,
        version,
        workspace_root,
        &root_package.path,
        &contents,
    );

    /* Create .ast */
    let result = if let Some(res_to_ast) = Some(
        Command::new(bsc_path)
            .current_dir(&build_path_abs)
            .args(parser_args)
            .output()
            .expect("Error converting .res to .ast"),
    ) {
        let stderr = std::str::from_utf8(&res_to_ast.stderr).expect("Expect StdErr to be non-null");
        if helpers::contains_ascii_characters(stderr) {
            if res_to_ast.status.success() {
                Ok((ast_path, Some(stderr.to_string())))
            } else {
                Err(format!("Error in {}:\n{}", package.name, stderr))
            }
        } else {
            Ok((ast_path, None))
        }
    } else {
        log::info!("Parsing file {}...", filename);

        Err(format!(
            "Could not find canonicalize_string_path for file {} in package {}",
            filename, package.name
        ))
    };
    if let Ok((ast_path, _)) = &result {
        let dir = std::path::Path::new(filename).parent().unwrap();
        let _ = std::fs::copy(
            build_path_abs.to_string() + "/" + ast_path,
            std::path::Path::new(&package.get_bs_build_path())
                .join(dir)
                .join(ast_path),
        );
    }
    result
}

fn path_to_ast_extension(path: &Path) -> &str {
    let extension = path.extension().unwrap().to_str().unwrap();
    if helpers::is_interface_ast_file(extension) {
        ".iast"
    } else {
        ".ast"
    }
}

fn include_ppx(flag: &str, contents: &str) -> bool {
    if flag.contains("bisect") {
        return std::env::var("BISECT_ENABLE").is_ok();
    }

    if ((flag.contains("graphql-ppx") || flag.contains("graphql_ppx")) && !contents.contains("%graphql"))
        || (flag.contains("spice") && !contents.contains("@spice"))
        || (flag.contains("rescript-relay") && !contents.contains("%relay"))
        || (flag.contains("re-formality") && !contents.contains("%form"))
    {
        return false;
    };

    true
}

fn filter_ppx_flags(
    ppx_flags: &Option<Vec<OneOrMore<String>>>,
    contents: &str,
) -> Option<Vec<OneOrMore<String>>> {
    // get the environment variable "BISECT_ENABLE" if it exists set the filter to "bisect"
    ppx_flags.as_ref().map(|flags| {
        flags
            .iter()
            .filter(|flag| match flag {
                config::OneOrMore::Single(str) => include_ppx(str, contents),
                config::OneOrMore::Multiple(str) => include_ppx(str.first().unwrap(), contents),
            })
            .map(|x| x.to_owned())
            .collect::<Vec<OneOrMore<String>>>()
    })
}
