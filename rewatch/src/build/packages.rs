use super::build_types::*;
use super::namespaces;
use super::packages;
use crate::build::{BuildProgress, BuildReporter, ConfigWarningKind};
use crate::config;
use crate::config::Config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::project_context::{MonorepoContext, ProjectContext};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use tracing::info_span;

use rayon::prelude::*;
use std::collections::hash_map::Entry;
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
    dependencies: Vec<Dependency>,
    is_local_dep: bool,
}

/// Loaded source information for a package.
/// None means sources haven't been scanned yet (lazy loading).
/// Some means sources have been scanned and are available.
#[derive(Debug, Clone)]
pub struct PackageSources {
    /// Relative file paths (relative to the package root) -> metadata
    pub files: AHashMap<PathBuf, SourceFileMeta>,
    /// Module names defined in this package
    pub modules: AHashSet<String>,
    /// Directories containing source files
    pub dirs: AHashSet<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub name: String,
    pub config: config::Config,
    pub source_folders: AHashSet<config::PackageSource>,
    pub namespace: Namespace,
    // canonicalized dir of the package
    pub path: PathBuf,
    /// Is this a local workspace package (vs external node_modules)?
    pub is_local: bool,
    pub is_root: bool,
    /// Loaded source information. None = not yet loaded, Some = sources scanned.
    pub sources: Option<PackageSources>,
}

pub fn get_build_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("bs")
}

pub fn get_js_path(canonical_path: &Path) -> PathBuf {
    canonical_path.join("lib").join("js")
}

pub fn get_esmodule_path(canonical_path: &Path) -> PathBuf {
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

    pub fn get_compiler_info_path(&self) -> PathBuf {
        self.get_build_path().join("compiler-info.json")
    }

    pub fn get_js_path(&self) -> PathBuf {
        get_js_path(&self.path)
    }

    pub fn get_esmodule_path(&self) -> PathBuf {
        get_esmodule_path(&self.path)
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
        self.sources
            .as_ref()
            .and_then(|s| s.files.get(path).map(|sfm| sfm.is_type_dev))
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

/// A source path with its recursive watching mode.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WatchSourcePath {
    pub path: PathBuf,
    pub recursive: bool,
}

/// Compute the paths a watch client should monitor, derived from the current build state.
/// Returns (source_paths, config_paths) where:
/// - source_paths: absolute paths to source directories with recursive flag
/// - config_paths: absolute paths to rescript.json files for all loaded packages
pub fn get_watch_paths(build_state: &BuildState) -> (AHashSet<WatchSourcePath>, AHashSet<PathBuf>) {
    let mut source_paths: AHashSet<WatchSourcePath> = AHashSet::new();
    let mut config_paths: AHashSet<PathBuf> = AHashSet::new();

    for package in build_state.packages.values() {
        // Only include packages whose sources have been loaded
        if package.sources.is_none() {
            continue;
        }

        // Only watch local packages — external packages' configs and sources
        // won't change during development
        if !package.is_local {
            continue;
        }

        for source_folder in &package.source_folders {
            let recursive = matches!(source_folder.subdirs, Some(config::Subdirs::Recurse(true)));
            source_paths.insert(WatchSourcePath {
                path: package.path.join(&source_folder.dir),
                recursive,
            });
        }

        config_paths.insert(package.path.join("rescript.json"));
    }

    (source_paths, config_paths)
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
                Err(e) => tracing::error!(error = %e, "Could not read directory"),
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

                Ok(_) => tracing::info!(name = ?name, "Filtered"),
                Err(ref e) => tracing::error!(error = %e, "Could not read directory"),
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
pub fn get_source_dirs(source: config::Source, sub_path: Option<PathBuf>) -> AHashSet<config::PackageSource> {
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

pub fn read_config(package_dir: &Path) -> Result<Config> {
    let rescript_json_path = package_dir.join("rescript.json");
    Config::new(&rescript_json_path)
}

pub fn read_dependency<R: BuildReporter>(
    package_name: &str,
    package_config: &Config,
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<PathBuf> {
    let path = helpers::try_package_path(package_config, project_context, package_name, reporter)?;

    let canonical_path = match path
        .canonicalize()
        .map(StrippedVerbatimPath::to_stripped_verbatim_path)
    {
        Ok(canonical_path) => Ok(canonical_path),
        Err(e) => Err(anyhow!(
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
/// 3. Detects and warns about duplicate packages (same name, different paths).
fn read_dependencies<R: BuildReporter>(
    registered_dependencies_set: &mut AHashSet<String>,
    project_context: &ProjectContext,
    package_config: &Config,
    is_local_dep: bool,
    reporter: &R,
) -> Result<Vec<Dependency>> {
    let mut dependencies = package_config.dependencies.to_owned().unwrap_or_default();

    // Concatenate dev dependencies if is_local_dep is true
    if is_local_dep && let Some(dev_deps) = package_config.dev_dependencies.to_owned() {
        dependencies.extend(dev_deps);
    }

    let packages_to_read: Vec<String> = dependencies
        .iter()
        .filter_map(|package_name| {
            if registered_dependencies_set.contains(package_name) {
                // Package already registered - check for duplicate (different path)
                // Re-resolve from current package and from root to compare paths
                if let Ok(current_path) =
                    read_dependency(package_name, package_config, project_context, reporter)
                    && let Ok(chosen_path) = read_dependency(
                        package_name,
                        &project_context.current_config,
                        project_context,
                        reporter,
                    )
                    && current_path != chosen_path
                {
                    // Different paths - this is a duplicate
                    let root_path = project_context.get_root_path();
                    let chosen_relative = chosen_path.strip_prefix(root_path).unwrap_or(&chosen_path);
                    let duplicate_relative = current_path.strip_prefix(root_path).unwrap_or(&current_path);
                    let current_package_path = package_config
                        .path
                        .parent()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_else(|| PathBuf::from("."));
                    let parent_relative = current_package_path
                        .strip_prefix(root_path)
                        .unwrap_or(&current_package_path);

                    reporter.report(BuildProgress::DuplicatedPackage {
                        package_name: package_name.to_string(),
                        chosen_path: chosen_relative.to_string_lossy().to_string(),
                        duplicate_path: duplicate_relative.to_string_lossy().to_string(),
                        parent_path: parent_relative.to_string_lossy().to_string(),
                    });
                }
                None
            } else {
                registered_dependencies_set.insert(package_name.to_owned());
                Some(package_name.to_owned())
            }
        })
        .collect();

    // Read all config files in parallel instead of blocking
    let results: Vec<Result<Dependency>> = packages_to_read
        .par_iter()
        .map(|package_name| {
            let (config, canonical_path) =
                match read_dependency(package_name, package_config, project_context, reporter) {
                    Err(error) => {
                        reporter.report(BuildProgress::PackageTreeError {
                            package_name: package_name.clone(),
                            error: error.to_string(),
                        });

                        let parent_path_str = project_context.get_root_path().to_string_lossy();
                        return Err(anyhow!(
                            "Could not build package tree reading dependency '{package_name}', at path '{parent_path_str}'. Error: {error}"
                        ));
                    }
                    Ok(canonical_path) => {
                        match read_config(&canonical_path) {
                            Ok(config) => (config, canonical_path),
                            Err(error) => {
                                reporter.report(BuildProgress::PackageTreeError {
                                    package_name: package_name.clone(),
                                    error: error.to_string(),
                                });

                                let parent_path_str = project_context.get_root_path().to_string_lossy();
                                return Err(anyhow!(
                                    "Could not build package tree for '{package_name}', at path '{parent_path_str}'. Error: {error}"
                                ));
                            }
                        }
                    }
                };

            let is_local_dep = {
                match &project_context.monorepo_context {
                    None => project_context.current_config.name.as_str() == package_name,
                    Some(MonorepoContext {
                             local_dependencies,
                             local_dev_dependencies,
                         }) => {
                        local_dependencies.contains(package_name) || local_dev_dependencies.contains(package_name)
                    },
                }
            };

            let dependencies = read_dependencies(
                &mut registered_dependencies_set.to_owned(),
                project_context,
                &config,
                is_local_dep,
                reporter,
            )?;

            Ok(Dependency {
                name: package_name.to_owned(),
                config,
                path: canonical_path,
                dependencies,
                is_local_dep,
            })
        })
        .collect();

    // Collect all dependencies, returning the first error if any
    results.into_iter().collect()
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
    let mut file_name = "package.json";
    let package_json_path = package_dir.join(file_name);

    let package_json_contents = if Path::exists(&package_json_path) {
        fs::read_to_string(&package_json_path).map_err(|e| anyhow!("Could not read package.json: {}", e))?
    } else {
        let rescript_json_path = package_dir.join("rescript.json");
        if Path::exists(&rescript_json_path) {
            file_name = "rescript.json";
            fs::read_to_string(&rescript_json_path)
                .map_err(|e| anyhow!("Could not read rescript.json: {}", e))?
        } else {
            return Err(anyhow!(
                "There is no package.json or rescript.json file in {}",
                package_dir.to_string_lossy()
            ));
        }
    };

    let package_json: serde_json::Value = serde_json::from_str(&package_json_contents)
        .map_err(|e| anyhow!("Could not parse {}: {}", file_name, e))?;

    package_json["name"]
        .as_str()
        .map(|s| s.to_string())
        .ok_or_else(|| anyhow!("No name field found in package.json"))
}

fn make_package<R: BuildReporter>(
    config: config::Config,
    package_path: &Path,
    is_root: bool,
    is_local: bool,
    reporter: &R,
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
                tracing::warn!(
                    package = %config.name,
                    path = %package_path_str,
                    "Package has not defined any sources, but is not the root package. This is likely a mistake."
                );
            }

            AHashSet::new()
        }
    };

    let package_name = read_package_name(package_path).expect("Could not read package name");
    if package_name != config.name {
        reporter.report(BuildProgress::PackageNameMismatch {
            package_path: package_path.to_string_lossy().to_string(),
            package_json_name: package_name.clone(),
            rescript_json_name: config.name.clone(),
        });
    }

    // Report config warnings for local packages
    if is_local {
        for field in config.get_unsupported_fields() {
            reporter.report(BuildProgress::ConfigWarning {
                package_name: package_name.clone(),
                field_name: field,
                kind: ConfigWarningKind::Unsupported,
            });
        }
        for field in config.get_unknown_fields() {
            reporter.report(BuildProgress::ConfigWarning {
                package_name: package_name.clone(),
                field_name: field,
                kind: ConfigWarningKind::Unknown,
            });
        }
    }

    Package {
        name: package_name,
        config: config.to_owned(),
        source_folders,
        namespace: config.get_namespace(),
        // we canonicalize the path name so it's always the same
        path: package_path
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
            .expect("Could not canonicalize"),
        is_local,
        is_root,
        sources: None,
    }
}

fn read_packages<R: BuildReporter>(
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<AHashMap<String, Package>> {
    // Store all packages and completely deduplicate them
    let mut map: AHashMap<String, Package> = AHashMap::new();

    let current_package = {
        let config = &project_context.current_config;
        let folder = config
            .path
            .parent()
            .ok_or_else(|| anyhow!("Could not the read parent folder or a rescript.json file"))?;
        make_package(config.to_owned(), folder, true, true, reporter)
    };

    map.insert(current_package.name.to_string(), current_package);

    let mut registered_dependencies_set: AHashSet<String> = AHashSet::new();
    let dependencies = flatten_dependencies(read_dependencies(
        &mut registered_dependencies_set,
        project_context,
        &project_context.current_config,
        /* is local dep */ true,
        reporter,
    )?);

    dependencies.iter().for_each(|d| {
        if !map.contains_key(&d.name) {
            let package = make_package(d.config.to_owned(), &d.path, false, d.is_local_dep, reporter);
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
    package_name: &str,
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

        Err(_e) => tracing::error!(
            folder = ?path_dir.to_path_buf().into_os_string(),
            dependency = %package_name,
            package_dir = ?package_dir,
            "Could not read folder"
        ),
    };

    map
}

/// Load sources for a single package. Idempotent - does nothing if already loaded.
pub fn load_package_sources(package: &mut Package, filter: &Option<regex::Regex>) {
    if package.sources.is_some() {
        return; // Already loaded
    }

    let mut files: AHashMap<PathBuf, SourceFileMeta> = AHashMap::new();
    package
        .source_folders
        .par_iter()
        .map(|source| {
            get_source_files(
                &package.name,
                Path::new(&package.path),
                filter,
                source,
                package.is_local,
            )
        })
        .collect::<Vec<AHashMap<PathBuf, SourceFileMeta>>>()
        .into_iter()
        .for_each(|source| files.extend(source));

    let mut modules = AHashSet::from_iter(
        files
            .keys()
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

    let mut dirs = AHashSet::new();
    files.keys().for_each(|path| {
        let dir = std::path::Path::new(&path).parent().unwrap();
        dirs.insert(dir.to_owned());
    });

    package.sources = Some(PackageSources { files, modules, dirs });
}

/// Compute the set of packages that need sources loaded for a given entry package.
/// This includes the entry package and all its transitive dependencies.
pub fn compute_build_scope(packages: &AHashMap<String, Package>, entry_package: &str) -> AHashSet<String> {
    let mut scope = AHashSet::new();
    let mut to_visit = vec![entry_package.to_string()];

    while let Some(pkg_name) = to_visit.pop() {
        if scope.contains(&pkg_name) {
            continue;
        }
        scope.insert(pkg_name.clone());

        if let Some(package) = packages.get(&pkg_name) {
            // Add dependencies
            if let Some(deps) = &package.config.dependencies {
                for dep in deps {
                    if packages.contains_key(dep) && !scope.contains(dep) {
                        to_visit.push(dep.clone());
                    }
                }
            }
            // Add dev dependencies for local packages
            if package.is_local
                && let Some(dev_deps) = &package.config.dev_dependencies
            {
                for dep in dev_deps {
                    if packages.contains_key(dep) && !scope.contains(dep) {
                        to_visit.push(dep.clone());
                    }
                }
            }
        }
    }

    scope
}

/// This takes the tree of packages, and finds all the source files for each, adding them to the
/// respective packages. If `packages_to_load` is Some, only load sources for those packages.
fn extend_with_children_scoped(
    filter: &Option<regex::Regex>,
    mut build: AHashMap<String, Package>,
    packages_to_load: Option<&AHashSet<String>>,
) -> AHashMap<String, Package> {
    let mut loaded_count = 0;
    let mut skipped_count = 0;
    for (name, package) in build.iter_mut() {
        // If scope is specified, only load sources for packages in scope
        if packages_to_load.is_none_or(|scope| scope.contains(name)) {
            load_package_sources(package, filter);
            loaded_count += 1;
        } else {
            skipped_count += 1;
        }
    }
    tracing::debug!(
        loaded = loaded_count,
        skipped = skipped_count,
        packages_to_load = ?packages_to_load.map(|s| s.len()),
        "extend_with_children_scoped"
    );
    build
}

/// Make turns a folder, that should contain a config, into a tree of Packages.
/// It does so in two steps:
/// 1. Get all the packages parsed, and take all the source folders from the config
/// 2. Take the (by then deduplicated) packages, and find all the '.res' and
///    interface files.
///
/// The two step process is there to reduce IO overhead.
///
/// If `packages_to_load` is Some, only load sources for those packages.
/// If None, load sources for all packages.
pub fn make<R: BuildReporter>(
    filter: &Option<regex::Regex>,
    project_context: &ProjectContext,
    reporter: &R,
) -> Result<AHashMap<String, Package>> {
    make_with_scope(filter, project_context, None, reporter)
}

/// Like `make`, but with optional scoping to only load sources for specific packages.
pub fn make_with_scope<R: BuildReporter>(
    filter: &Option<regex::Regex>,
    project_context: &ProjectContext,
    packages_to_load: Option<&AHashSet<String>>,
    reporter: &R,
) -> Result<AHashMap<String, Package>> {
    let map = read_packages(project_context, reporter)?;

    /* Once we have the deduplicated packages, we can add the source files for each - to minimize
     * the IO. If packages_to_load is specified, only load those. */
    let result = extend_with_children_scoped(filter, map, packages_to_load);

    Ok(result)
}

/// Load sources for packages that don't have them loaded yet.
/// This is used by the daemon to lazily expand the build state when requests
/// require additional packages.
///
/// If `packages_to_load` is None, loads sources for all packages that don't have them.
/// If `packages_to_load` is Some, only loads sources for the specified packages.
pub fn ensure_sources_loaded(
    packages: &mut AHashMap<String, Package>,
    packages_to_load: Option<&AHashSet<String>>,
) {
    let mut loaded_packages: Vec<String> = Vec::new();

    for (name, package) in packages.iter_mut() {
        // Skip if already loaded
        if package.sources.is_some() {
            continue;
        }
        // If scope is specified, only load sources for packages in scope
        if packages_to_load.is_none_or(|scope| scope.contains(name)) {
            let span = info_span!(
                "build.load_package_sources",
                package = %name
            );
            let _guard = span.enter();
            load_package_sources(package, &None);
            loaded_packages.push(name.clone());
        }
    }

    // Record which packages were loaded as a span event
    if !loaded_packages.is_empty() {
        tracing::info!(
            packages = ?loaded_packages,
            count = loaded_packages.len(),
            "loaded package sources"
        );
    }
}

pub fn parse_packages<R: BuildReporter>(build_state: &mut BuildState, reporter: &R) -> Result<()> {
    let packages = build_state.packages.clone();

    for (package_name, package) in packages.iter() {
        tracing::debug!(package = %package_name, "Parsing package");
        if let Some(sources) = &package.sources {
            build_state.module_names.extend(sources.modules.clone())
        }
        let build_path_abs = package.get_build_path();
        let bs_build_path = package.get_ocaml_build_path();
        helpers::create_path(&build_path_abs);
        helpers::create_path(&bs_build_path);
        let root_config = build_state.get_root_config();

        root_config.get_package_specs().iter().for_each(|spec| {
            if !spec.in_source {
                // we don't want to calculate this if we don't have out of source specs
                // we do this twice, but we almost never have multiple package specs
                // so this optimization is less important
                let relative_dirs: AHashSet<PathBuf> = match &package.sources {
                    Some(sources) => sources
                        .files
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
                    helpers::create_path(&package.get_esmodule_path());
                    relative_dirs.iter().for_each(|path_buf| {
                        helpers::create_path_for_path(&Path::join(&package.get_esmodule_path(), path_buf))
                    })
                }
            }
        });

        package.namespace.to_suffix().iter().for_each(|namespace| {
            // generate the mlmap "AST" file for modules that have a namespace configured
            // Skip packages that don't have sources loaded (not in build scope)
            let Some(sources) = &package.sources else {
                return;
            };
            let source_files: Vec<PathBuf> = sources.files.keys().cloned().collect();
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

            let Ok(mlmap) = namespaces::gen_mlmap(package, namespace, &depending_modules) else {
                log::error!(
                    "Failed to generate mlmap for namespace {namespace} in package {}",
                    package.name
                );
                return;
            };

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
                &helpers::file_path_to_module_name(&mlmap, &packages::Namespace::NoNamespace),
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

        tracing::debug!(package = %package.name, "Building source file-tree for package");
        if let Some(sources) = &package.sources {
            for (file, metadata) in sources.files.iter() {
                let namespace = package.namespace.to_owned();

                let extension = file.extension().unwrap().to_str().unwrap();
                let module_name = helpers::file_path_to_module_name(file, &namespace);

                if helpers::is_implementation_file(extension) {
                    // Store duplicate paths in an Option so we can build the error after the entry borrow ends.
                    let mut duplicate_paths: Option<(PathBuf, PathBuf)> = None;
                    match build_state.modules.entry(module_name.to_string()) {
                        Entry::Occupied(mut entry) => {
                            let module = entry.get_mut();
                            if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                                if &source_file.implementation.path != file {
                                    duplicate_paths = Some((
                                        Path::new(&package.path).join(&source_file.implementation.path),
                                        Path::new(&package.path).join(file),
                                    ));
                                }
                                // Only mark dirty if the file was actually modified since last seen
                                if metadata.modified > source_file.implementation.last_modified {
                                    source_file.implementation.parse_dirty = true;
                                }
                                source_file.implementation.path = file.to_owned();
                                source_file.implementation.last_modified = metadata.modified;
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Module {
                                deps_dirty: true,
                                source_type: SourceType::SourceFile(SourceFile {
                                    implementation: Implementation {
                                        path: file.to_owned(),
                                        parse_state: ParseState::Pending,
                                        compile_state: CompileState::Pending,
                                        last_modified: metadata.modified,
                                        parse_dirty: true,
                                        compile_warnings: None,
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
                        }
                    }
                    if let Some((existing_path, duplicate_path)) = duplicate_paths {
                        let root_path = build_state.get_root_config().path.clone();
                        let root = root_path.parent().map(PathBuf::from).unwrap_or(root_path);
                        let existing_display = existing_path.strip_prefix(&root).unwrap_or(&existing_path);
                        let duplicate_display = duplicate_path.strip_prefix(&root).unwrap_or(&duplicate_path);
                        let mut first = existing_display.to_string_lossy().to_string();
                        let mut second = duplicate_display.to_string_lossy().to_string();
                        if second < first {
                            std::mem::swap(&mut first, &mut second);
                        }
                        return Err(anyhow!(
                            "Duplicate module name: {module_name}. Found in {} and {}. Rename one of these files.",
                            first,
                            second
                        ));
                    }
                } else {
                    // remove last character of string: resi -> res
                    let mut implementation_filename = file.to_owned();
                    let extension = implementation_filename.extension().unwrap().to_str().unwrap();
                    implementation_filename = match extension {
                        "resi" => implementation_filename.with_extension("res"),
                        _ => implementation_filename,
                    };
                    match sources.files.get(&implementation_filename) {
                        None => {
                            if let Some(implementation_path) = sources.files.keys().find(|path| {
                                let extension = path.extension().and_then(|ext| ext.to_str());
                                matches!(extension, Some(ext) if helpers::is_implementation_file(ext))
                                    && helpers::file_path_to_module_name(path, &namespace) == module_name
                            }) {
                                let implementation_display =
                                    implementation_path.to_string_lossy().to_string();
                                let interface_display = file.to_string_lossy().to_string();
                                return Err(anyhow!(
                                    "Implementation and interface have different path names or different cases: `{}` vs `{}`",
                                    implementation_display,
                                    interface_display
                                ));
                            }
                            reporter.report(BuildProgress::MissingImplementation {
                                interface_file: file.to_string_lossy().to_string(),
                            })
                        }
                        Some(_) => {
                            build_state
                                .modules
                                .entry(module_name.to_string())
                                .and_modify(|module| {
                                    if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                                        match &mut source_file.interface {
                                            Some(existing_interface) => {
                                                // Only mark dirty if the file was actually modified since last seen
                                                if metadata.modified > existing_interface.last_modified {
                                                    existing_interface.parse_dirty = true;
                                                }
                                                existing_interface.path = file.to_owned();
                                                existing_interface.last_modified = metadata.modified;
                                            }
                                            None => {
                                                // New interface file
                                                source_file.interface = Some(Interface {
                                                    path: file.to_owned(),
                                                    parse_state: ParseState::Pending,
                                                    compile_state: CompileState::Pending,
                                                    last_modified: metadata.modified,
                                                    parse_dirty: true,
                                                    compile_warnings: None,
                                                });
                                            }
                                        }
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
                                            compile_warnings: None,
                                        },
                                        interface: Some(Interface {
                                            path: file.to_owned(),
                                            parse_state: ParseState::Pending,
                                            compile_state: CompileState::Pending,
                                            last_modified: metadata.modified,
                                            parse_dirty: true,
                                            compile_warnings: None,
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
            }
        }
    }

    Ok(())
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
            if let Some(allowed_dependents) = deps_allowed_dependents
                && !allowed_dependents.contains(package_name)
            {
                return Some(deps_package_name.to_string());
            }
        }
    }
    None
}
#[derive(Debug, Clone)]
struct UnallowedDependency {
    deps: Vec<String>,
    dev_deps: Vec<String>,
}

pub fn validate_packages_dependencies<R: BuildReporter>(
    packages: &AHashMap<String, Package>,
    reporter: &R,
) -> bool {
    let mut detected_unallowed_dependencies: AHashMap<String, UnallowedDependency> = AHashMap::new();

    for (package_name, package) in packages {
        let dependencies = &package.config.dependencies.to_owned().unwrap_or(vec![]);
        let dev_dependencies = &package.config.dev_dependencies.to_owned().unwrap_or(vec![]);

        [
            ("dependencies", dependencies),
            ("dev-dependencies", dev_dependencies),
        ]
        .iter()
        .for_each(|(dependency_type, dependencies)| {
            if let Some(unallowed_dependency_name) =
                get_unallowed_dependents(packages, package_name, dependencies)
            {
                let empty_unallowed_deps = UnallowedDependency {
                    deps: vec![],
                    dev_deps: vec![],
                };

                let unallowed_dependency = detected_unallowed_dependencies.entry(String::from(package_name));
                let value = unallowed_dependency.or_insert_with(|| empty_unallowed_deps);
                match *dependency_type {
                    "dependencies" => value.deps.push(unallowed_dependency_name),
                    "dev-dependencies" => value.dev_deps.push(unallowed_dependency_name),
                    _ => (),
                }
            }
        });
    }
    for (package_name, unallowed_deps) in detected_unallowed_dependencies.iter() {
        let groups: Vec<crate::build::UnallowedDependencyGroup> = [
            ("dependencies", unallowed_deps.deps.to_owned()),
            ("dev-dependencies", unallowed_deps.dev_deps.to_owned()),
        ]
        .iter()
        .filter(|(_, deps)| !deps.is_empty())
        .map(|(deps_type, deps)| crate::build::UnallowedDependencyGroup {
            deps_type: deps_type.to_string(),
            deps: deps.clone(),
        })
        .collect();

        reporter.report(BuildProgress::UnallowedDependency {
            package_name: package_name.clone(),
            groups,
        });
    }
    let has_any_unallowed_dependent = !detected_unallowed_dependencies.is_empty();
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
        build_dev_deps: Vec<String>,
        allowed_dependents: Option<Vec<String>>,
    }

    fn create_package(args: CreatePackageArgs) -> Package {
        Package {
            name: args.name.clone(),
            config: config::tests::create_config(config::tests::CreateConfigArgs {
                name: args.name,
                bs_deps: args.bs_deps,
                build_dev_deps: args.build_dev_deps,
                allowed_dependents: args.allowed_dependents,
                path: PathBuf::from("./something/rescript.json"),
            }),
            source_folders: AHashSet::new(),
            namespace: Namespace::Namespace(String::from("Package1")),
            path: PathBuf::from("./something"),
            is_local: false,
            is_root: false,
            sources: None,
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
                build_dev_deps: vec![],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package3")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages, &crate::build::NoopReporter);
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
                build_dev_deps: vec![String::from("Package2")],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package3")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages, &crate::build::NoopReporter);
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
                build_dev_deps: vec![],
                allowed_dependents: None,
            }),
        );
        packages.insert(
            String::from("Package2"),
            create_package(CreatePackageArgs {
                name: String::from("Package2"),
                bs_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: Some(vec![String::from("Package1")]),
            }),
        );

        let is_valid = super::validate_packages_dependencies(&packages, &crate::build::NoopReporter);
        assert!(is_valid)
    }
}
