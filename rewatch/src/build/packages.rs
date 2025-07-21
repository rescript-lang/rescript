use super::build_types::*;
use super::namespaces;
use super::packages;
use crate::config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::helpers::emojis::*;
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use console::style;
use log::{debug, error};
use rayon::prelude::*;
use std::error;
use std::fs::{self};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

#[derive(Debug, Clone)]
pub struct SourceFileMeta {
    pub modified: SystemTime,
    pub is_type_dev: bool,
}

#[derive(Debug, Clone)]
pub enum Namespace {
    Namespace(String),
    NamespaceWithEntry { namespace: String, entry: String },
    NoNamespace,
}

impl Namespace {
    pub fn to_suffix(&self) -> Option<String> {
        match self {
            Namespace::Namespace(namespace) => Some(namespace.to_string()),
            Namespace::NamespaceWithEntry { namespace, entry: _ } => Some("@".to_string() + namespace),
            Namespace::NoNamespace => None,
        }
    }
}

#[derive(Debug, Clone)]
struct Dependency {
    name: String,
    config: config::Config,
    path: PathBuf,
    is_pinned: bool,
    dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub name: String,
    pub config: config::Config,
    pub source_folders: AHashSet<config::PackageSource>,
    // these are the relative file paths (relative to the package root)
    pub source_files: Option<AHashMap<PathBuf, SourceFileMeta>>,
    pub namespace: Namespace,
    pub modules: Option<AHashSet<String>>,
    // canonicalized dir of the package
    pub path: PathBuf,
    pub dirs: Option<AHashSet<PathBuf>>,
    pub is_pinned_dep: bool,
    pub is_local_dep: bool,
    pub is_root: bool,
}

pub fn get_build_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("bs")
}

pub fn get_js_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("js")
}

pub fn get_es6_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("es6")
}

pub fn get_ocaml_build_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("ocaml")
}

impl Package {
    pub fn get_ocaml_build_path(&self) -> PathBuf {
        get_ocaml_build_path(&self.path)
    }

    pub fn get_build_path(&self) -> PathBuf {
        get_build_path(&self.path)
    }

    pub fn get_js_path(&self) -> PathBuf {
        get_js_path(&self.path)
    }

    pub fn get_es6_path(&self) -> PathBuf {
        get_es6_path(&self.path)
    }

    pub fn get_mlmap_path(&self) -> PathBuf {
        let suffix = self
            .namespace
            .to_suffix()
            .expect("namespace should be set for mlmap module");
        self.get_build_path().join(format!("{suffix}.mlmap"))
    }

    pub fn get_mlmap_compile_path(&self) -> PathBuf {
        let suffix = self
            .namespace
            .to_suffix()
            .expect("namespace should be set for mlmap module");
        self.get_build_path().join(format!("{suffix}.cmi"))
    }

    pub fn is_source_file_type_dev(&self, path: &Path) -> bool {
        self.source_files
            .as_ref()
            .and_then(|sf| sf.get(path).map(|sfm| sfm.is_type_dev))
            .unwrap_or(false)
    }
}

impl PartialEq for Package {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Package {}
impl Hash for Package {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        blake3::hash(self.name.as_bytes());
    }
}

fn matches_filter(filter: &Option<regex::Regex>, path: &str) -> bool {
    match filter {
        Some(filter) => filter.is_match(path),
        None => true,
    }
}

pub fn read_folders(
    filter: &Option<regex::Regex>,
    package_dir: &Path,
    path: &Path,
    recurse: bool,
    is_type_dev: bool,
) -> Result<AHashMap<PathBuf, SourceFileMeta>, Box<dyn error::Error>> {
    let mut map: AHashMap<PathBuf, SourceFileMeta> = AHashMap::new();
    let path_buf = PathBuf::from(path);
    let meta = fs::metadata(package_dir.join(path));
    let path_with_meta = meta.map(|meta| {
        (
            path.to_owned(),
            SourceFileMeta {
                modified: meta.modified().unwrap(),
                is_type_dev,
            },
        )
    });

    for entry in fs::read_dir(package_dir.join(&path_buf))? {
        let entry_path_buf = entry.map(|entry| entry.path())?;
        let metadata = fs::metadata(&entry_path_buf)?;
        let name = entry_path_buf.file_name().unwrap().to_str().unwrap().to_string();

        let path_ext = entry_path_buf.extension().and_then(|x| x.to_str());
        let new_path = path_buf.join(&name);
        if metadata.file_type().is_dir() && recurse {
            match read_folders(filter, package_dir, &new_path, recurse, is_type_dev) {
                Ok(s) => map.extend(s),
                Err(e) => log::error!("Could not read directory: {e}"),
            }
        }

        match path_ext {
            Some(extension) if helpers::is_source_file(extension) => match path_with_meta {
                Ok((ref path, _)) if matches_filter(filter, &name) => {
                    let mut path = path.to_owned();
                    path.push(&name);
                    map.insert(
                        path,
                        SourceFileMeta {
                            modified: metadata.modified().unwrap(),
                            is_type_dev,
                        },
                    );
                }

                Ok(_) => log::info!("Filtered: {name:?}"),
                Err(ref e) => log::error!("Could not read directory: {e}"),
            },
            _ => (),
        }
    }

    Ok(map)
}

/// Given a projects' root folder and a `config::Source`, this recursively creates all the
/// sources in a flat list. In the process, it removes the children, as they are being resolved
/// because of the recursiveness. So you get a flat list of files back, retaining the type_ and
/// whether it needs to recurse into all structures
fn get_source_dirs(source: config::Source, sub_path: Option<PathBuf>) -> AHashSet<config::PackageSource> {
    let mut source_folders: AHashSet<config::PackageSource> = AHashSet::new();

    let source_folder = source.to_qualified_without_children(sub_path.to_owned());
    source_folders.insert(source_folder.to_owned());

    let (subdirs, full_recursive) = match source.to_owned() {
        config::Source::Shorthand(_)
        | config::Source::Qualified(config::PackageSource { subdirs: None, .. }) => (None, false),
        config::Source::Qualified(config::PackageSource {
            subdirs: Some(config::Subdirs::Recurse(recurse)),
            ..
        }) => (None, recurse),
        config::Source::Qualified(config::PackageSource {
            subdirs: Some(config::Subdirs::Qualified(subdirs)),
            ..
        }) => (Some(subdirs), false),
    };

    if !full_recursive {
        let sub_path = Path::new(&source_folder.dir).to_path_buf();
        subdirs
            .unwrap_or(vec![])
            .par_iter()
            .map(|subsource| {
                get_source_dirs(subsource.set_type(source.get_type()), Some(sub_path.to_owned()))
            })
            .collect::<Vec<AHashSet<config::PackageSource>>>()
            .into_iter()
            .for_each(|subdir| source_folders.extend(subdir))
    }

    source_folders
}

pub fn read_config(package_dir: &Path) -> Result<config::Config> {
    let rescript_json_path = package_dir.join("rescript.json");
    let bsconfig_json_path = package_dir.join("bsconfig.json");

    if Path::new(&rescript_json_path).exists() {
        config::Config::new(&rescript_json_path)
    } else {
        config::Config::new(&bsconfig_json_path)
    }
}

pub fn read_dependency(
    package_name: &str,
    parent_path: &Path,
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
) -> Result<PathBuf, String> {
    let path_from_parent = helpers::package_path(parent_path, package_name);
    let path_from_project_root = helpers::package_path(project_root, package_name);
    let maybe_path_from_workspace_root = workspace_root
        .as_ref()
        .map(|workspace_root| helpers::package_path(workspace_root, package_name));

    let path = match (
        path_from_parent,
        path_from_project_root,
        maybe_path_from_workspace_root,
    ) {
        (path_from_parent, _, _) if path_from_parent.exists() => Ok(path_from_parent),
        (_, path_from_project_root, _) if path_from_project_root.exists() => Ok(path_from_project_root),
        (_, _, Some(path_from_workspace_root)) if path_from_workspace_root.exists() => {
            Ok(path_from_workspace_root)
        }
        _ => Err(format!(
            "The package \"{package_name}\" is not found (are node_modules up-to-date?)..."
        )),
    }?;

    let canonical_path = match path
        .canonicalize()
        .map(StrippedVerbatimPath::to_stripped_verbatim_path)
    {
        Ok(canonical_path) => Ok(canonical_path),
        Err(e) => Err(format!(
            "Failed canonicalizing the package \"{}\" path \"{}\" (are node_modules up-to-date?)...\nMore details: {}",
            package_name,
            path.to_string_lossy(),
            e
        )),
    }?;

    Ok(canonical_path)
}

/// Given a config, recursively finds all dependencies.
/// 1. It starts with registering dependencies and
///    prevents the operation for the ones which are already
///    registered for the parent packages. Especially relevant for peerDependencies.
/// 2. In parallel performs IO to read the dependencies config and
///    recursively continues operation for their dependencies as well.
fn read_dependencies(
    registered_dependencies_set: &mut AHashSet<String>,
    parent_config: &config::Config,
    parent_path: &Path,
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
    show_progress: bool,
    build_dev_deps: bool,
) -> Vec<Dependency> {
    let mut dependencies = parent_config.dependencies.to_owned().unwrap_or_default();

    // Concatenate dev dependencies if build_dev_deps is true
    if build_dev_deps && let Some(dev_deps) = parent_config.dev_dependencies.to_owned() {
        dependencies.extend(dev_deps);
    }

    dependencies
        .iter()
        .filter_map(|package_name| {
            if registered_dependencies_set.contains(package_name) {
                None
            } else {
                registered_dependencies_set.insert(package_name.to_owned());
                Some(package_name.to_owned())
            }
        })
        .collect::<Vec<String>>()
        // Read all config files in parallel instead of blocking
        .par_iter()
        .map(|package_name| {
            let (config, canonical_path) =
                match read_dependency(package_name, parent_path, project_root, workspace_root) {
                    Err(error) => {
                        if show_progress {
                            println!(
                                "{} {} Error building package tree. {}",
                                style("[1/2]").bold().dim(),
                                CROSS,
                                error
                            );
                        }

                        let parent_path_str = parent_path.to_string_lossy();
                        log::error!(
                            "We could not build package tree reading dependency '{package_name}', at path '{parent_path_str}'. Error: {error}",
                        );

                        std::process::exit(2)
                    }
                    Ok(canonical_path) => {
                        match read_config(&canonical_path) {
                            Ok(config) => (config, canonical_path),
                            Err(error) => {
                                let parent_path_str = parent_path.to_string_lossy();
                                log::error!(
                                    "We could not build package tree  '{package_name}', at path '{parent_path_str}'. Error: {error}",
                                );
                                std::process::exit(2)
                            }
                        }
                    }
                };

            let is_pinned = parent_config
                .pinned_dependencies
                .as_ref()
                .map(|p| p.contains(&config.name))
                .unwrap_or(false);

            let dependencies = read_dependencies(
                &mut registered_dependencies_set.to_owned(),
                &config,
                &canonical_path,
                project_root,
                workspace_root,
                show_progress,
                build_dev_deps,
            );

            Dependency {
                name: package_name.to_owned(),
                config,
                path: canonical_path,
                is_pinned,
                dependencies,
            }
        })
        .collect()
}

fn flatten_dependencies(dependencies: Vec<Dependency>) -> Vec<Dependency> {
    let mut flattened: Vec<Dependency> = Vec::new();
    for dep in dependencies {
        flattened.push(dep.clone());
        let nested_flattened = flatten_dependencies(dep.dependencies);
        flattened.extend(nested_flattened);
    }
    flattened
}

pub fn read_package_name(package_dir: &Path) -> Result<String> {
    let package_json_path = package_dir.join("package.json");

    let package_json_contents =
        fs::read_to_string(&package_json_path).map_err(|e| anyhow!("Could not read package.json: {}", e))?;

    let package_json: serde_json::Value = serde_json::from_str(&package_json_contents)
        .map_err(|e| anyhow!("Could not parse package.json: {}", e))?;

    package_json["name"]
        .as_str()
        .map(|s| s.to_string())
        .ok_or_else(|| anyhow!("No name field found in package.json"))
}

fn make_package(
    config: config::Config,
    package_path: &Path,
    is_pinned_dep: bool,
    is_root: bool,
    project_root: &Path,
) -> Package {
    let source_folders = match config.sources.to_owned() {
        Some(config::OneOrMore::Single(source)) => get_source_dirs(source, None),
        Some(config::OneOrMore::Multiple(sources)) => {
            let mut source_folders: AHashSet<config::PackageSource> = AHashSet::new();
            sources
                .iter()
                .map(|source| get_source_dirs(source.to_owned(), None))
                .collect::<Vec<AHashSet<config::PackageSource>>>()
                .into_iter()
                .for_each(|source| source_folders.extend(source));
            source_folders
        }
        None => {
            if !is_root {
                let package_path_str = package_path.to_string_lossy();
                log::warn!(
                    "Package '{}' has not defined any sources, but is not the root package. This is likely a mistake. It is located: {}",
                    config.name,
                    package_path_str
                );
            }

            AHashSet::new()
        }
    };

    let package_name = read_package_name(package_path).expect("Could not read package name");
    if package_name != config.name {
        log::warn!(
            "\nPackage name mismatch for {}:\n\
The package.json name is \"{}\", while the rescript.json name is \"{}\"\n\
This inconsistency will cause issues with package resolution.\n",
            package_path.to_string_lossy(),
            package_name,
            config.name,
        );
    }

    let is_local_dep = {
        package_path.starts_with(project_root)
            && !package_path.components().any(|c| c.as_os_str() == "node_modules")
    };

    Package {
        name: package_name,
        config: config.to_owned(),
        source_folders,
        source_files: None,
        namespace: config.get_namespace(),
        modules: None,
        // we canonicalize the path name so it's always the same
        path: package_path
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
            .expect("Could not canonicalize"),
        dirs: None,
        is_pinned_dep,
        is_local_dep,
        is_root,
    }
}

fn read_packages(
    project_root: &Path,
    workspace_root: &Option<PathBuf>,
    show_progress: bool,
    build_dev_deps: bool,
) -> Result<AHashMap<String, Package>> {
    let root_config = read_config(project_root)?;

    // Store all packages and completely deduplicate them
    let mut map: AHashMap<String, Package> = AHashMap::new();
    let root_package = make_package(root_config.to_owned(), project_root, false, true, project_root);
    map.insert(root_package.name.to_string(), root_package);

    let mut registered_dependencies_set: AHashSet<String> = AHashSet::new();
    let dependencies = flatten_dependencies(read_dependencies(
        &mut registered_dependencies_set,
        &root_config,
        project_root,
        project_root,
        workspace_root,
        show_progress,
        build_dev_deps,
    ));
    dependencies.iter().for_each(|d| {
        if !map.contains_key(&d.name) {
            let package = make_package(d.config.to_owned(), &d.path, d.is_pinned, false, project_root);
            map.insert(d.name.to_string(), package);
        }
    });

    Ok(map)
}

/// `get_source_files` is essentially a wrapper around `read_structure`, which read a
/// list of files in a folder to a hashmap of `string` / `fs::Metadata` (file metadata). Reason for
/// this wrapper is the recursiveness of the `config.json` subfolders. Some sources in config
/// can be specified as being fully recursive (`{ subdirs: true }`). This wrapper pulls out that
/// data from the config and pushes it forwards. Another thing is the 'type_', some files / folders
/// can be marked with the type 'dev'. Which means that they may not be around in the distributed
/// NPM package. The file reader allows for this, just warns when this happens.
/// TODO -> Check whether we actually need the `fs::Metadata`
pub fn get_source_files(
    package_name: &String,
    package_dir: &Path,
    filter: &Option<regex::Regex>,
    source: &config::PackageSource,
    build_dev_deps: bool,
) -> AHashMap<PathBuf, SourceFileMeta> {
    let mut map: AHashMap<PathBuf, SourceFileMeta> = AHashMap::new();

    let recurse = match source {
        config::PackageSource {
            subdirs: Some(config::Subdirs::Recurse(subdirs)),
            ..
        } => *subdirs,
        _ => false,
    };

    let path_dir = Path::new(&source.dir);
    let is_type_dev = source.is_type_dev();

    if !build_dev_deps && is_type_dev {
        return map;
    }

    match read_folders(filter, package_dir, path_dir, recurse, is_type_dev) {
        Ok(files) => map.extend(files),

        Err(_e) => log::error!(
            "Could not read folder: {:?}. Specified in dependency: {}, located {:?}...",
            path_dir.to_path_buf().into_os_string(),
            package_name,
            package_dir
        ),
    };

    map
}

/// This takes the tree of packages, and finds all the source files for each, adding them to the
/// respective packages.
fn extend_with_children(
    filter: &Option<regex::Regex>,
    mut build: AHashMap<String, Package>,
    build_dev_deps: bool,
) -> AHashMap<String, Package> {
    for (_key, package) in build.iter_mut() {
        let mut map: AHashMap<PathBuf, SourceFileMeta> = AHashMap::new();
        package
            .source_folders
            .par_iter()
            .map(|source| {
                get_source_files(
                    &package.name,
                    Path::new(&package.path),
                    filter,
                    source,
                    package.is_local_dep && build_dev_deps,
                )
            })
            .collect::<Vec<AHashMap<PathBuf, SourceFileMeta>>>()
            .into_iter()
            .for_each(|source| map.extend(source));

        let mut modules = AHashSet::from_iter(
            map.keys()
                .map(|key| helpers::file_path_to_module_name(key, &package.namespace)),
        );
        match package.namespace.to_owned() {
            Namespace::Namespace(namespace) => {
                let _ = modules.insert(namespace);
            }
            Namespace::NamespaceWithEntry { namespace, entry: _ } => {
                let _ = modules.insert("@".to_string() + &namespace);
            }
            Namespace::NoNamespace => (),
        }
        package.modules = Some(modules);
        let mut dirs = AHashSet::new();
        map.keys().for_each(|path| {
            let dir = std::path::Path::new(&path).parent().unwrap();
            dirs.insert(dir.to_owned());
        });
        package.dirs = Some(dirs);
        package.source_files = Some(map);
    }
    build
}

/// Make turns a folder, that should contain a config, into a tree of Packages.
/// It does so in two steps:
/// 1. Get all the packages parsed, and take all the source folders from the config
/// 2. Take the (by then deduplicated) packages, and find all the '.re', '.res', '.ml' and
///    interface files.
///
/// The two step process is there to reduce IO overhead
pub fn make(
    filter: &Option<regex::Regex>,
    root_folder: &Path,
    workspace_root: &Option<PathBuf>,
    show_progress: bool,
    build_dev_deps: bool,
) -> Result<AHashMap<String, Package>> {
    let map = read_packages(root_folder, workspace_root, show_progress, build_dev_deps)?;

    /* Once we have the deduplicated packages, we can add the source files for each - to minimize
     * the IO */
    let result = extend_with_children(filter, map, build_dev_deps);

    Ok(result)
}

pub fn parse_packages(build_state: &mut BuildState) {
    build_state
        .packages
        .clone()
        .iter()
        .for_each(|(package_name, package)| {
            debug!("Parsing package: {package_name}");
            if let Some(package_modules) = package.modules.to_owned() {
                build_state.module_names.extend(package_modules)
            }
            let build_path_abs = package.get_build_path();
            let bs_build_path = package.get_ocaml_build_path();
            helpers::create_path(&build_path_abs);
            helpers::create_path(&bs_build_path);
            let root_config = build_state
                .get_package(&build_state.root_config_name)
                .expect("cannot find root config");

            root_config.config.get_package_specs().iter().for_each(|spec| {
                if !spec.in_source {
                    // we don't want to calculate this if we don't have out of source specs
                    // we do this twice, but we almost never have multiple package specs
                    // so this optimization is less important
                    let relative_dirs: AHashSet<PathBuf> = match &package.source_files {
                        Some(source_files) => source_files
                            .keys()
                            .map(|source_file| {
                                Path::new(source_file)
                                    .parent()
                                    .expect("parent dir not found")
                                    .to_owned()
                            })
                            .collect(),
                        _ => AHashSet::new(),
                    };
                    if spec.is_common_js() {
                        helpers::create_path(&package.get_js_path());
                        relative_dirs.iter().for_each(|path_buf| {
                            helpers::create_path_for_path(&Path::join(&package.get_js_path(), path_buf))
                        })
                    } else {
                        helpers::create_path(&package.get_es6_path());
                        relative_dirs.iter().for_each(|path_buf| {
                            helpers::create_path_for_path(&Path::join(&package.get_es6_path(), path_buf))
                        })
                    }
                }
            });

            package.namespace.to_suffix().iter().for_each(|namespace| {
                // generate the mlmap "AST" file for modules that have a namespace configured
                let source_files = match package.source_files.to_owned() {
                    Some(source_files) => source_files
                        .keys()
                        .map(|key| key.to_owned())
                        .collect::<Vec<PathBuf>>(),
                    None => unreachable!(),
                };
                let entry = match &package.namespace {
                    packages::Namespace::NamespaceWithEntry { entry, namespace: _ } => Some(entry),
                    _ => None,
                };

                let depending_modules = source_files
                    .iter()
                    .map(|path| helpers::file_path_to_module_name(path, &packages::Namespace::NoNamespace))
                    .filter(|module_name| {
                        if let Some(entry) = entry {
                            module_name != entry
                        } else {
                            true
                        }
                    })
                    .filter(|module_name| helpers::is_non_exotic_module_name(module_name))
                    .collect::<AHashSet<String>>();

                let mlmap = namespaces::gen_mlmap(package, namespace, &depending_modules);

                // mlmap will be compiled in the AST generation step
                // compile_mlmap(&package, namespace, &project_root);
                let deps = source_files
                    .iter()
                    .filter(|path| {
                        helpers::is_non_exotic_module_name(&helpers::file_path_to_module_name(
                            path,
                            &packages::Namespace::NoNamespace,
                        ))
                    })
                    .map(|path| helpers::file_path_to_module_name(path, &package.namespace))
                    .filter(|module_name| {
                        if let Some(entry) = entry {
                            module_name != entry
                        } else {
                            true
                        }
                    })
                    .collect::<AHashSet<String>>();

                build_state.insert_module(
                    &helpers::file_path_to_module_name(&mlmap.to_owned(), &packages::Namespace::NoNamespace),
                    Module {
                        deps_dirty: false,
                        source_type: SourceType::MlMap(MlMap { parse_dirty: false }),
                        deps,
                        dependents: AHashSet::new(),
                        package_name: package.name.to_owned(),
                        compile_dirty: false,
                        last_compiled_cmt: None,
                        last_compiled_cmi: None,
                        // Not sure if this is correct
                        is_type_dev: false,
                    },
                );
            });

            debug!("Building source file-tree for package: {}", package.name);
            match &package.source_files {
                None => (),
                Some(source_files) => source_files.iter().for_each(|(file, metadata)| {
                    let namespace = package.namespace.to_owned();

                    let extension = file.extension().unwrap().to_str().unwrap();
                    let module_name = helpers::file_path_to_module_name(file, &namespace);

                    if helpers::is_implementation_file(extension) {
                        build_state
                            .modules
                            .entry(module_name.to_string())
                            .and_modify(|module| {
                                if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                                    if &source_file.implementation.path != file {
                                        error!("Duplicate files found for module: {}", &module_name);
                                        error!(
                                            "file 1: {}",
                                            source_file.implementation.path.to_string_lossy()
                                        );
                                        error!("file 2: {}", file.to_string_lossy());

                                        panic!("Unable to continue... See log output above...");
                                    }
                                    source_file.implementation.path = file.to_owned();
                                    source_file.implementation.last_modified = metadata.modified;
                                    source_file.implementation.parse_dirty = true;
                                }
                            })
                            .or_insert(Module {
                                deps_dirty: true,
                                source_type: SourceType::SourceFile(SourceFile {
                                    implementation: Implementation {
                                        path: file.to_owned(),
                                        parse_state: ParseState::Pending,
                                        compile_state: CompileState::Pending,
                                        last_modified: metadata.modified,
                                        parse_dirty: true,
                                    },
                                    interface: None,
                                }),
                                deps: AHashSet::new(),
                                dependents: AHashSet::new(),
                                package_name: package.name.to_owned(),
                                compile_dirty: true,
                                last_compiled_cmt: None,
                                last_compiled_cmi: None,
                                is_type_dev: metadata.is_type_dev,
                            });
                    } else {
                        // remove last character of string: resi -> res, rei -> re, mli -> ml
                        let mut implementation_filename = file.to_owned();
                        let extension = implementation_filename.extension().unwrap().to_str().unwrap();
                        implementation_filename = match extension {
                            "resi" => implementation_filename.with_extension("res"),
                            "rei" => implementation_filename.with_extension("re"),
                            "mli" => implementation_filename.with_extension("ml"),
                            _ => implementation_filename,
                        };
                        match source_files.get(&implementation_filename) {
                            None => {
                                println!(
                                    "{} No implementation file found for interface file (skipping): {}",
                                    LINE_CLEAR,
                                    file.to_string_lossy()
                                )
                            }
                            Some(_) => {
                                build_state
                                    .modules
                                    .entry(module_name.to_string())
                                    .and_modify(|module| {
                                        if let SourceType::SourceFile(ref mut source_file) =
                                            module.source_type
                                        {
                                            source_file.interface = Some(Interface {
                                                path: file.to_owned(),
                                                parse_state: ParseState::Pending,
                                                compile_state: CompileState::Pending,
                                                last_modified: metadata.modified,
                                                parse_dirty: true,
                                            });
                                        }
                                    })
                                    .or_insert(Module {
                                        deps_dirty: true,
                                        source_type: SourceType::SourceFile(SourceFile {
                                            // this will be overwritten later
                                            implementation: Implementation {
                                                path: implementation_filename,
                                                parse_state: ParseState::Pending,
                                                compile_state: CompileState::Pending,
                                                last_modified: metadata.modified,
                                                parse_dirty: true,
                                            },
                                            interface: Some(Interface {
                                                path: file.to_owned(),
                                                parse_state: ParseState::Pending,
                                                compile_state: CompileState::Pending,
                                                last_modified: metadata.modified,
                                                parse_dirty: true,
                                            }),
                                        }),
                                        deps: AHashSet::new(),
                                        dependents: AHashSet::new(),
                                        package_name: package.name.to_owned(),
                                        compile_dirty: true,
                                        last_compiled_cmt: None,
                                        last_compiled_cmi: None,
                                        is_type_dev: metadata.is_type_dev,
                                    });
                            }
                        }
                    }
                }),
            }
        });
}

impl Package {
    pub fn get_jsx_args(&self) -> Vec<String> {
        self.config.get_jsx_args()
    }

    pub fn get_jsx_mode_args(&self) -> Vec<String> {
        self.config.get_jsx_mode_args()
    }

    pub fn get_jsx_module_args(&self) -> Vec<String> {
        self.config.get_jsx_module_args()
    }

    pub fn get_jsx_preserve_args(&self) -> Vec<String> {
        self.config.get_jsx_preserve_args()
    }
}

fn get_unallowed_dependents(
    packages: &AHashMap<String, Package>,
    package_name: &String,
    dependencies: &Vec<String>,
) -> Option<String> {
    for deps_package_name in dependencies {
        if let Some(deps_package) = packages.get(deps_package_name) {
            let deps_allowed_dependents = deps_package.config.allowed_dependents.to_owned();
            if let Some(allowed_dependents) = deps_allowed_dependents {
                if !allowed_dependents.contains(package_name) {
                    return Some(deps_package_name.to_string());
                }
            }
        }
    }
    None
}
#[derive(Debug, Clone)]
struct UnallowedDependency {
    bs_deps: Vec<String>,
    pinned_deps: Vec<String>,
    bs_build_dev_deps: Vec<String>,
}

pub fn validate_packages_dependencies(packages: &AHashMap<String, Package>) -> bool {
    let mut detected_unallowed_dependencies: AHashMap<String, UnallowedDependency> = AHashMap::new();

    for (package_name, package) in packages {
        let bs_dependencies = &package.config.dependencies.to_owned().unwrap_or(vec![]);
        let pinned_dependencies = &package.config.pinned_dependencies.to_owned().unwrap_or(vec![]);
        let dev_dependencies = &package.config.dev_dependencies.to_owned().unwrap_or(vec![]);

        [
            ("bs-dependencies", bs_dependencies),
            ("pinned-dependencies", pinned_dependencies),
            ("bs-dev-dependencies", dev_dependencies),
        ]
        .iter()
        .for_each(|(dependency_type, dependencies)| {
            if let Some(unallowed_dependency_name) =
                get_unallowed_dependents(packages, package_name, dependencies)
            {
                let empty_unallowed_deps = UnallowedDependency {
                    bs_deps: vec![],
                    pinned_deps: vec![],
                    bs_build_dev_deps: vec![],
                };

                let unallowed_dependency = detected_unallowed_dependencies.entry(String::from(package_name));
                let value = unallowed_dependency.or_insert_with(|| empty_unallowed_deps);
                match *dependency_type {
                    "bs-dependencies" => value.bs_deps.push(unallowed_dependency_name),
                    "pinned-dependencies" => value.pinned_deps.push(unallowed_dependency_name),
                    "bs-dev-dependencies" => value.bs_build_dev_deps.push(unallowed_dependency_name),
                    _ => (),
                }
            }
        });
    }
    for (package_name, unallowed_deps) in detected_unallowed_dependencies.iter() {
        log::error!(
            "\n{}: {} has the following unallowed dependencies:",
            console::style("Error").red(),
            console::style(package_name).bold()
        );

        [
            ("bs-dependencies", unallowed_deps.bs_deps.to_owned()),
            ("pinned-dependencies", unallowed_deps.pinned_deps.to_owned()),
            ("bs-dev-dependencies", unallowed_deps.bs_build_dev_deps.to_owned()),
        ]
        .iter()
        .for_each(|(deps_type, map)| {
            if !map.is_empty() {
                log::info!(
                    "{} dependencies: {}",
                    console::style(deps_type).bold().dim(),
                    console::style(map.join(" \n -")).bold().dim()
                );
            }
        });
    }
    let has_any_unallowed_dependent = !detected_unallowed_dependencies.is_empty();

    if has_any_unallowed_dependent {
        log::error!(
            "\nUpdate the {} value in the {} of the unallowed dependencies to solve the issue!",
            console::style("unallowed_dependents").bold().dim(),
            console::style("config.json").bold().dim()
        )
    }
    !has_any_unallowed_dependent
}

#[cfg(test)]
mod test {
    use crate::config;

    use super::{Namespace, Package};
    use ahash::{AHashMap, AHashSet};
    use std::path::PathBuf;

    pub struct CreatePackageArgs {
        name: String,
        bs_deps: Vec<String>,
        pinned_deps: Vec<String>,
        build_dev_deps: Vec<String>,
        allowed_dependents: Option<Vec<String>>,
    }

    fn create_package(args: CreatePackageArgs) -> Package {
        Package {
            name: args.name.clone(),
            config: config::tests::create_config(config::tests::CreateConfigArgs {
                name: args.name,
                bs_deps: args.bs_deps,
                pinned_deps: args.pinned_deps,
                build_dev_deps: args.build_dev_deps,
                allowed_dependents: args.allowed_dependents,
            }),
            source_folders: AHashSet::new(),
            source_files: None,
            namespace: Namespace::Namespace(String::from("Package1")),
            modules: None,
            path: PathBuf::from("./something"),
            dirs: None,
            is_pinned_dep: false,
            is_root: false,
            is_local_dep: false,
        }
    }
    #[test]
    fn should_return_false_with_invalid_parents_as_bs_dependencies() {
        let mut packages: AHashMap<String, Package> = AHashMap::new();
        packages.insert(
            String::from("Package1"),
            create_package(CreatePackageArgs {
                name: String::from("Package1"),
                bs_deps: vec![String::from("Package2")],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package3")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages);
        assert!(!is_valid)
    }

    #[test]
    fn should_return_false_with_invalid_parents_as_pinned_dependencies() {
        let mut packages: AHashMap<String, Package> = AHashMap::new();
        packages.insert(
            String::from("Package1"),
            create_package(CreatePackageArgs {
                name: String::from("Package1"),
                bs_deps: vec![],
                pinned_deps: vec![String::from("Package2")],
                build_dev_deps: vec![],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package3")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages);
        assert!(!is_valid)
    }

    #[test]
    fn should_return_false_with_invalid_parents_as_dev_dependencies() {
        let mut packages: AHashMap<String, Package> = AHashMap::new();
        packages.insert(
            String::from("Package1"),
            create_package(CreatePackageArgs {
                name: String::from("Package1"),
                bs_deps: vec![],
                pinned_deps: vec![],
                build_dev_deps: vec![String::from("Package2")],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package3")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages);
        assert!(!is_valid)
    }

    #[test]
    fn should_return_true_with_no_invalid_parent() {
        let mut packages: AHashMap<String, Package> = AHashMap::new();
        packages.insert(
            String::from("Package1"),
            create_package(CreatePackageArgs {
                name: String::from("Package1"),
                bs_deps: vec![String::from("Package2")],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                pinned_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package1")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages);
        assert!(is_valid)
    }
}
