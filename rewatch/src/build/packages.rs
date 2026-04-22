use super::build_types::*;
use super::namespaces;
use super::packages;
use crate::config;
use crate::config::Config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::helpers::emojis::*;
use crate::project_context::{MonoRepoContext, ProjectContext};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use console::style;
use log::debug;
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
    /// Every directory reachable from `source_folders` (honoring
    /// `subdirs: true`), relative to the package root — including ones that
    /// hold only `.ts` shim files and therefore are absent from `dirs`.
    /// Populated during package discovery only when gentype is enabled.
    pub gentype_dirs: Option<Vec<PathBuf>>,
    pub is_local_dep: bool,
    pub is_root: bool,
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
                get_source_dirs(
                    subsource
                        .set_type(source.get_type())
                        .set_feature(source.get_feature()),
                    Some(sub_path.to_owned()),
                )
            })
            .collect::<Vec<AHashSet<config::PackageSource>>>()
            .into_iter()
            .for_each(|subdir| source_folders.extend(subdir))
    }

    source_folders
}

pub fn read_config(package_dir: &Path) -> Result<Config> {
    let rescript_json_path = package_dir.join("rescript.json");
    let bsconfig_json_path = package_dir.join("bsconfig.json");

    if rescript_json_path.exists() {
        Config::new(&rescript_json_path)
    } else {
        Config::new(&bsconfig_json_path)
    }
}

pub fn read_dependency(
    package_name: &str,
    package_config: &Config,
    project_context: &ProjectContext,
) -> Result<PathBuf> {
    let path = helpers::try_package_path(package_config, project_context, package_name)?;

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
fn read_dependencies(
    registered_dependencies_set: &mut AHashSet<String>,
    project_context: &ProjectContext,
    package_config: &Config,
    show_progress: bool,
    is_local_dep: bool,
    prod: bool,
) -> Vec<Dependency> {
    let mut dependencies: Vec<String> = package_config.get_dependency_names();

    // Concatenate dev dependencies if is_local_dep is true and not in prod mode
    if is_local_dep && !prod {
        dependencies.extend(package_config.get_dev_dependency_names());
    }

    dependencies
        .iter()
        .filter_map(|package_name| {
            if registered_dependencies_set.contains(package_name) {
                // Package already registered - check for duplicate (different path)
                // Re-resolve from current package and from root to compare paths
                if let Ok(current_path) = read_dependency(package_name, package_config, project_context)
                    && let Ok(chosen_path) = read_dependency(package_name, &project_context.current_config, project_context)
                    && current_path != chosen_path
                {
                    // Different paths - this is a duplicate
                    let root_path = project_context.get_root_path();
                    let chosen_relative = chosen_path
                        .strip_prefix(root_path)
                        .unwrap_or(&chosen_path);
                    let duplicate_relative = current_path
                        .strip_prefix(root_path)
                        .unwrap_or(&current_path);
                    let current_package_path = package_config
                        .path
                        .parent()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_else(|| PathBuf::from("."));
                    let parent_relative = current_package_path
                        .strip_prefix(root_path)
                        .unwrap_or(&current_package_path);

                    eprintln!(
                        "Duplicated package: {} ./{} (chosen) vs ./{} in ./{}",
                        package_name,
                        chosen_relative.to_string_lossy(),
                        duplicate_relative.to_string_lossy(),
                        parent_relative.to_string_lossy()
                    );
                }
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
                match read_dependency(package_name, package_config, project_context) {
                    Err(error) => {
                        if show_progress {
                            println!(
                                "{} {} Error building package tree. {}",
                                style("[1/2]").bold().dim(),
                                CROSS,
                                error
                            );
                        }

                        let parent_path_str = project_context.get_root_path().to_string_lossy();
                        log::error!(
                            "Could not build package tree reading dependency '{package_name}' at path '{parent_path_str}'. Error: {error}",
                        );

                        std::process::exit(2)
                    }
                    Ok(canonical_path) => {
                        match read_config(&canonical_path) {
                            Ok(config) => (config, canonical_path),
                            Err(error) => {
                                let parent_path_str = project_context.get_root_path().to_string_lossy();
                                log::error!(
                                    "Could not build package tree for '{package_name}' at path '{parent_path_str}'. Error: {error}",
                                );
                                std::process::exit(2)
                            }
                        }
                    }
                };

            let is_local_dep = {
                match &project_context.monorepo_context {
                    None => project_context.current_config.name.as_str() == package_name
                    ,
                    Some(MonoRepoContext::MonorepoRoot {
                             local_dependencies,
                             local_dev_dependencies,
                         }) => {
                        local_dependencies.contains(package_name) || local_dev_dependencies.contains(package_name)
                    },
                    Some(MonoRepoContext::MonorepoPackage {
                             parent_config,
                         }) => {
                        helpers::is_local_package(&parent_config.path, &canonical_path)
                    }
                }
            };

            let dependencies = read_dependencies(
                &mut registered_dependencies_set.to_owned(),
                project_context,
                &config,
                show_progress,
                is_local_dep,
                prod,
            );

            Dependency {
                name: package_name.to_owned(),
                config,
                path: canonical_path,
                dependencies,
                is_local_dep,
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
    let read_name = |file_name: &str| -> Result<Option<String>> {
        let path = package_dir.join(file_name);
        if !Path::exists(&path) {
            return Ok(None);
        }

        let contents =
            fs::read_to_string(&path).map_err(|e| anyhow!("Could not read {}: {}", file_name, e))?;
        let json: serde_json::Value =
            serde_json::from_str(&contents).map_err(|e| anyhow!("Could not parse {}: {}", file_name, e))?;

        Ok(json["name"].as_str().map(|name| name.to_string()))
    };

    if let Some(name) = read_name("package.json")? {
        return Ok(name);
    }

    if let Some(name) = read_name("rescript.json")? {
        return Ok(name);
    }

    if let Some(name) = read_name("bsconfig.json")? {
        return Ok(name);
    }

    Err(anyhow!(
        "No name field found in package.json or rescript.json in {}",
        package_dir.to_string_lossy()
    ))
}

/// Looks up the best-effort issue tracker URL for a package by reading its
/// package.json, preferring `bugs.url`, then deriving from `repository`.
/// Returns None if no tracker could be inferred.
pub fn read_issue_tracker_url(package_dir: &Path) -> Option<String> {
    let contents = fs::read_to_string(package_dir.join("package.json")).ok()?;
    let json: serde_json::Value = serde_json::from_str(&contents).ok()?;

    let extract_url = |v: &serde_json::Value| -> Option<String> {
        match v {
            serde_json::Value::String(s) => Some(s.to_owned()),
            serde_json::Value::Object(o) => o.get("url").and_then(|u| u.as_str()).map(String::from),
            _ => None,
        }
    };

    if let Some(bugs_url) = json.get("bugs").and_then(extract_url) {
        return Some(bugs_url);
    }

    json.get("repository")
        .and_then(extract_url)
        .map(|repo| issues_url_from_repository(&repo))
}

fn issues_url_from_repository(repo: &str) -> String {
    let cleaned = repo.trim_start_matches("git+").trim_end_matches(".git");

    // npm shorthand (no scheme, no user@): treat as github-style "owner/repo"
    if !cleaned.contains("://") && !cleaned.contains('@') {
        let path = cleaned.trim_start_matches("github:");
        return format!("https://github.com/{path}/issues");
    }

    format!("{cleaned}/issues")
}

fn make_package(
    config: config::Config,
    package_path: &Path,
    is_root: bool,
    is_local_dep: bool,
) -> Result<Package> {
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

    let package_name = read_package_name(package_path)?;
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

    Ok(Package {
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
        gentype_dirs: None,
        is_local_dep,
        is_root,
    })
}

fn read_packages(
    project_context: &ProjectContext,
    show_progress: bool,
    prod: bool,
) -> Result<AHashMap<String, Package>> {
    // Store all packages and completely deduplicate them
    let mut map: AHashMap<String, Package> = AHashMap::new();

    let current_package = {
        let config = &project_context.current_config;
        let folder = config
            .path
            .parent()
            .ok_or_else(|| anyhow!("Could not the read parent folder or a rescript.json file"))?;
        make_package(config.to_owned(), folder, true, true)?
    };

    map.insert(current_package.name.to_string(), current_package);

    let mut registered_dependencies_set: AHashSet<String> = AHashSet::new();
    let dependencies = flatten_dependencies(read_dependencies(
        &mut registered_dependencies_set,
        project_context,
        &project_context.current_config,
        show_progress,
        /* is local dep */ true,
        prod,
    ));

    for d in dependencies.iter() {
        if !map.contains_key(&d.name) {
            let package = make_package(d.config.to_owned(), &d.path, false, d.is_local_dep)?;
            map.insert(d.name.to_string(), package);
        }
    }

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
    prod: bool,
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
                    package.is_local_dep && !prod,
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
        if package.config.gentype_config.is_some() {
            package.gentype_dirs = Some(collect_gentype_source_dirs(package));
        }
        package.source_files = Some(map);
    }
    build
}

/// Walks a package's declared source folders and returns every directory
/// reachable under them (honoring `subdirs: true`), relative to the package
/// root. Gentype needs every such directory — including ones containing only
/// `.ts` shims — to resolve cross-file imports, so `package.dirs` (which
/// tracks only dirs with `.res` source files) isn't enough.
fn collect_gentype_source_dirs(package: &Package) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = Vec::new();
    let root = &package.path;

    fn walk_recursive(root: &Path, rel: &Path, out: &mut Vec<PathBuf>) {
        let abs = if rel.as_os_str().is_empty() {
            root.to_path_buf()
        } else {
            root.join(rel)
        };
        let Ok(meta) = std::fs::metadata(&abs) else {
            return;
        };
        if !meta.is_dir() {
            return;
        }
        out.push(rel.to_path_buf());
        let Ok(entries) = std::fs::read_dir(&abs) else {
            return;
        };
        for entry in entries.flatten() {
            let Ok(child_meta) = entry.metadata() else {
                continue;
            };
            if child_meta.is_dir() {
                walk_recursive(root, &rel.join(entry.file_name()), out);
            }
        }
    }

    for source in &package.source_folders {
        let rel = PathBuf::from(&source.dir);
        match &source.subdirs {
            Some(config::Subdirs::Recurse(true)) => walk_recursive(root, &rel, &mut out),
            _ => {
                if root.join(&rel).is_dir() {
                    out.push(rel);
                }
            }
        }
    }
    out
}

/// Computes the active feature set for every package. Rules:
///
/// - The root package's active set comes from the `--features` CLI flag. When the flag is
///   absent, all features declared in the root config are active (default = all).
/// - For every other package, we scan every consumer (any package that lists this one in its
///   `dependencies` or `dev-dependencies`). A shorthand entry (or an object entry without a
///   `features` field) means the consumer wants all features of the dep. An entry with a
///   `features` list means the consumer wants exactly those. When any consumer requests "all"
///   or no consumer requests anything, the dep builds with all of its declared features.
///   Otherwise we take the union of specific feature requests and pass it through the dep's
///   own `features` map for transitive expansion.
///
/// `dev-dependencies` only contribute when that edge is actually traversed by
/// `read_dependencies` — i.e. when the consumer is a local dep and we're not in `--prod`. This
/// keeps dev-only feature requests (e.g. a shorthand `dev-dependencies` entry that would flip
/// everything to "all features") from leaking into production builds.
pub fn compute_active_features(
    packages: &AHashMap<String, Package>,
    cli_features: Option<&Vec<String>>,
    prod: bool,
) -> Result<AHashMap<String, AHashSet<String>>> {
    let mut result: AHashMap<String, AHashSet<String>> = AHashMap::new();

    for (package_name, package) in packages {
        let mut any_all_request = false;
        let mut saw_consumer_entry = false;
        let mut requested: AHashSet<String> = AHashSet::new();

        if package.is_root {
            match cli_features {
                None => any_all_request = true,
                Some(list) => requested.extend(list.iter().cloned()),
            }
        } else {
            for consumer in packages.values() {
                // `dependencies` always contribute.
                if let Some(deps) = consumer.config.dependencies.as_ref() {
                    for dep in deps {
                        if dep.name() != package_name {
                            continue;
                        }
                        saw_consumer_entry = true;
                        match dep.features() {
                            None => any_all_request = true,
                            Some(list) => requested.extend(list.iter().cloned()),
                        }
                    }
                }
                // `dev-dependencies` only contribute when that edge is actually traversed:
                // local consumer, not `--prod`. Matches `read_dependencies`.
                if consumer.is_local_dep
                    && !prod
                    && let Some(dev_deps) = consumer.config.dev_dependencies.as_ref()
                {
                    for dep in dev_deps {
                        if dep.name() != package_name {
                            continue;
                        }
                        saw_consumer_entry = true;
                        match dep.features() {
                            None => any_all_request = true,
                            Some(list) => requested.extend(list.iter().cloned()),
                        }
                    }
                }
            }

            // Defensive: if no consumer edge was found at all, keep all features. An empty
            // `requested` set by itself is a *valid* request (`"features": []` means
            // "untagged only"), so only fall back when we truly observed no entries.
            if !saw_consumer_entry {
                any_all_request = true;
            }
        }

        let closure = if any_all_request {
            package.config.collect_declared_features()
        } else {
            config::resolve_active_features(&requested, package.config.features.as_ref())
                .map_err(|e| anyhow!("Invalid features for package '{}': {}", package_name, e))?
        };

        result.insert(package_name.clone(), closure);
    }

    Ok(result)
}

/// Make turns a folder, that should contain a config, into a tree of Packages.
/// It does so in two steps:
/// 1. Get all the packages parsed, and take all the source folders from the config
/// 2. Take the (by then deduplicated) packages, and find all the '.res' and
///    interface files.
///
/// The two step process is there to reduce IO overhead.
pub fn make(
    filter: &Option<regex::Regex>,
    project_context: &ProjectContext,
    show_progress: bool,
    prod: bool,
    cli_features: Option<&Vec<String>>,
) -> Result<AHashMap<String, Package>> {
    let mut map = read_packages(project_context, show_progress, prod)?;

    let active_features = compute_active_features(&map, cli_features, prod)?;

    // Drop source directories whose feature tag is not in the package's active set.
    // Untagged source dirs remain; they're included regardless of the feature selection.
    for (package_name, package) in map.iter_mut() {
        if let Some(active) = active_features.get(package_name) {
            package
                .source_folders
                .retain(|source| source.is_feature_enabled(active));
        }
    }

    /* Once we have the deduplicated packages, we can add the source files for each - to minimize
     * the IO */
    let result = extend_with_children(filter, map, prod);

    Ok(result)
}

pub fn parse_packages(build_state: &mut BuildState) -> Result<()> {
    let packages = build_state.packages.clone();
    for (package_name, package) in packages.iter() {
        debug!("Parsing package: {package_name}");
        if let Some(package_modules) = package.modules.to_owned() {
            build_state.module_names.extend(package_modules)
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
                    helpers::create_path(&package.get_esmodule_path());
                    relative_dirs.iter().for_each(|path_buf| {
                        helpers::create_path_for_path(&Path::join(&package.get_esmodule_path(), path_buf))
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
        if let Some(source_files) = &package.source_files {
            for (file, metadata) in source_files.iter() {
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
                                source_file.implementation.path = file.to_owned();
                                source_file.implementation.last_modified = metadata.modified;
                                source_file.implementation.parse_dirty = true;
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
                    match source_files.get(&implementation_filename) {
                        None => {
                            if let Some(implementation_path) = source_files.keys().find(|path| {
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
                                    if let SourceType::SourceFile(ref mut source_file) = module.source_type {
                                        source_file.interface = Some(Interface {
                                            path: file.to_owned(),
                                            parse_state: ParseState::Pending,
                                            compile_state: CompileState::Pending,
                                            last_modified: metadata.modified,
                                            parse_dirty: true,
                                            compile_warnings: None,
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

pub fn validate_packages_dependencies(packages: &AHashMap<String, Package>) -> bool {
    let mut detected_unallowed_dependencies: AHashMap<String, UnallowedDependency> = AHashMap::new();

    for (package_name, package) in packages {
        let dependencies = &package.config.get_dependency_names();
        let dev_dependencies = &package.config.get_dev_dependency_names();

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
        log::error!(
            "\n{}: {} has the following unallowed dependencies:",
            console::style("Error").red(),
            console::style(package_name).bold()
        );

        [
            ("dependencies", unallowed_deps.deps.to_owned()),
            ("dev-dependencies", unallowed_deps.dev_deps.to_owned()),
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

    use super::{Namespace, Package, read_issue_tracker_url, read_package_name};
    use ahash::{AHashMap, AHashSet};
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

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
            source_files: None,
            namespace: Namespace::Namespace(String::from("Package1")),
            modules: None,
            path: PathBuf::from("./something"),
            dirs: None,
            gentype_dirs: None,
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

        let is_valid = super::validate_packages_dependencies(&packages);
        assert!(is_valid)
    }

    #[test]
    fn should_report_missing_name_when_package_and_rescript_json_lack_it() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let package_dir = temp_dir.path();

        fs::write(package_dir.join("package.json"), "{\n  \"private\": true\n}\n")
            .expect("package.json should be written");
        fs::write(
            package_dir.join("rescript.json"),
            "{\n  \"suffix\": \".mjs\"\n}\n",
        )
        .expect("rescript.json should be written");

        let error = read_package_name(package_dir).expect_err("missing names should fail");

        assert_eq!(
            error.to_string(),
            format!(
                "No name field found in package.json or rescript.json in {}",
                package_dir.to_string_lossy()
            )
        );
    }

    fn write_pkg_json(dir: &std::path::Path, body: &str) {
        fs::write(dir.join("package.json"), body).expect("package.json should be written");
    }

    #[test]
    fn issue_tracker_url_prefers_bugs_url_string() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(
            temp_dir.path(),
            r#"{"name":"x","bugs":"https://example.com/issues"}"#,
        );
        assert_eq!(
            read_issue_tracker_url(temp_dir.path()),
            Some("https://example.com/issues".to_string())
        );
    }

    #[test]
    fn issue_tracker_url_prefers_bugs_object_url() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(
            temp_dir.path(),
            r#"{"name":"x","bugs":{"url":"https://example.com/report"}}"#,
        );
        assert_eq!(
            read_issue_tracker_url(temp_dir.path()),
            Some("https://example.com/report".to_string())
        );
    }

    #[test]
    fn issue_tracker_url_derives_from_repository_git_url() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(
            temp_dir.path(),
            r#"{"name":"x","repository":"git+https://github.com/owner/repo.git"}"#,
        );
        assert_eq!(
            read_issue_tracker_url(temp_dir.path()),
            Some("https://github.com/owner/repo/issues".to_string())
        );
    }

    #[test]
    fn issue_tracker_url_derives_from_repository_object() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(
            temp_dir.path(),
            r#"{"name":"x","repository":{"type":"git","url":"https://github.com/owner/repo.git"}}"#,
        );
        assert_eq!(
            read_issue_tracker_url(temp_dir.path()),
            Some("https://github.com/owner/repo/issues".to_string())
        );
    }

    #[test]
    fn issue_tracker_url_handles_shorthand() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(temp_dir.path(), r#"{"name":"x","repository":"owner/repo"}"#);
        assert_eq!(
            read_issue_tracker_url(temp_dir.path()),
            Some("https://github.com/owner/repo/issues".to_string())
        );
    }

    #[test]
    fn issue_tracker_url_returns_none_without_hints() {
        let temp_dir = TempDir::new().unwrap();
        write_pkg_json(temp_dir.path(), r#"{"name":"x"}"#);
        assert_eq!(read_issue_tracker_url(temp_dir.path()), None);
    }

    fn root_package_with_features(
        features_map: Option<std::collections::HashMap<String, Vec<String>>>,
        tagged_sources: Vec<(&str, Option<&str>)>,
    ) -> super::Package {
        let sources: Vec<config::Source> = tagged_sources
            .into_iter()
            .map(|(dir, feature)| {
                config::Source::Qualified(config::PackageSource {
                    dir: dir.to_string(),
                    subdirs: None,
                    type_: None,
                    feature: feature.map(|s| s.to_string()),
                })
            })
            .collect();
        let mut config = config::tests::create_config(config::tests::CreateConfigArgs {
            name: "root".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });
        config.sources = Some(config::OneOrMore::Multiple(sources));
        config.features = features_map;
        super::Package {
            name: "root".to_string(),
            config,
            source_folders: AHashSet::new(),
            source_files: None,
            namespace: super::Namespace::NoNamespace,
            modules: None,
            path: PathBuf::from("."),
            dirs: None,
            gentype_dirs: None,
            is_local_dep: true,
            is_root: true,
        }
    }

    #[test]
    fn compute_active_features_returns_all_when_cli_absent() {
        let mut packages: AHashMap<String, super::Package> = AHashMap::new();
        let mut features_map = std::collections::HashMap::new();
        features_map.insert("full".to_string(), vec!["native".to_string()]);
        packages.insert(
            "root".to_string(),
            root_package_with_features(
                Some(features_map),
                vec![("src", None), ("src-native", Some("native"))],
            ),
        );

        let active = super::compute_active_features(&packages, None, false).unwrap();
        let root = active.get("root").unwrap();
        assert!(root.contains("native"));
        assert!(root.contains("full"));
    }

    #[test]
    fn compute_active_features_honours_cli_restriction_and_expands_transitive() {
        let mut packages: AHashMap<String, super::Package> = AHashMap::new();
        let mut features_map = std::collections::HashMap::new();
        features_map.insert(
            "full".to_string(),
            vec!["native".to_string(), "experimental".to_string()],
        );
        packages.insert(
            "root".to_string(),
            root_package_with_features(
                Some(features_map),
                vec![
                    ("src", None),
                    ("src-native", Some("native")),
                    ("src-experimental", Some("experimental")),
                ],
            ),
        );

        let cli = vec!["full".to_string()];
        let active = super::compute_active_features(&packages, Some(&cli), false).unwrap();
        let root = active.get("root").unwrap();
        assert!(root.contains("full"));
        assert!(root.contains("native"));
        assert!(root.contains("experimental"));
    }

    #[test]
    fn compute_active_features_dep_uses_consumer_restriction() {
        // Root consumes a dep with a restricted feature set.
        let mut packages: AHashMap<String, super::Package> = AHashMap::new();

        let mut root_config = config::tests::create_config(config::tests::CreateConfigArgs {
            name: "root".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });
        root_config.sources = Some(config::OneOrMore::Single(config::Source::Shorthand(
            "src".to_string(),
        )));
        root_config.dependencies = Some(vec![config::Dependency::Qualified(config::QualifiedDependency {
            name: "dep".to_string(),
            features: Some(vec!["native".to_string()]),
        })]);
        packages.insert(
            "root".to_string(),
            super::Package {
                name: "root".to_string(),
                config: root_config,
                source_folders: AHashSet::new(),
                source_files: None,
                namespace: super::Namespace::NoNamespace,
                modules: None,
                path: PathBuf::from("."),
                dirs: None,
                gentype_dirs: None,
                is_local_dep: true,
                is_root: true,
            },
        );

        packages.insert(
            "dep".to_string(),
            root_package_with_features(None, vec![("src", None), ("src-native", Some("native"))]),
        );
        // Flip the dep's is_root flag to false since root_package_with_features sets it to true.
        packages.get_mut("dep").unwrap().is_root = false;

        let active = super::compute_active_features(&packages, None, false).unwrap();
        let dep_active = active.get("dep").unwrap();
        assert!(dep_active.contains("native"));
    }

    #[test]
    fn compute_active_features_prod_ignores_dev_dependency_feature_requests() {
        // Regression: a dep that appears in both `dependencies` (restricted to `native`) and
        // `dev-dependencies` (shorthand = all features). In `--prod` the dev edge isn't
        // traversed by `read_dependencies`, so its broader feature request must not flip the
        // dep's active set to "all features". Only the non-dev restriction applies.
        let mut packages: AHashMap<String, super::Package> = AHashMap::new();

        let mut root_config = config::tests::create_config(config::tests::CreateConfigArgs {
            name: "root".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });
        root_config.sources = Some(config::OneOrMore::Single(config::Source::Shorthand(
            "src".to_string(),
        )));
        root_config.dependencies = Some(vec![config::Dependency::Qualified(config::QualifiedDependency {
            name: "dep".to_string(),
            features: Some(vec!["native".to_string()]),
        })]);
        root_config.dev_dependencies = Some(vec![config::Dependency::Shorthand("dep".to_string())]);
        packages.insert(
            "root".to_string(),
            super::Package {
                name: "root".to_string(),
                config: root_config,
                source_folders: AHashSet::new(),
                source_files: None,
                namespace: super::Namespace::NoNamespace,
                modules: None,
                path: PathBuf::from("."),
                dirs: None,
                gentype_dirs: None,
                is_local_dep: true,
                is_root: true,
            },
        );

        packages.insert(
            "dep".to_string(),
            root_package_with_features(
                None,
                vec![
                    ("src", None),
                    ("src-native", Some("native")),
                    ("src-experimental", Some("experimental")),
                ],
            ),
        );
        packages.get_mut("dep").unwrap().is_root = false;

        // Non-prod: dev edge IS traversed → all features active.
        let active = super::compute_active_features(&packages, None, /* prod */ false).unwrap();
        let dep_active = active.get("dep").unwrap();
        assert!(dep_active.contains("native"));
        assert!(
            dep_active.contains("experimental"),
            "non-prod should honour dev-dependency shorthand = all features"
        );

        // Prod: dev edge is NOT traversed → only the `dependencies` restriction applies.
        let active_prod = super::compute_active_features(&packages, None, /* prod */ true).unwrap();
        let dep_active_prod = active_prod.get("dep").unwrap();
        assert!(dep_active_prod.contains("native"));
        assert!(
            !dep_active_prod.contains("experimental"),
            "prod must not inherit the dev-dependency shorthand's all-features request"
        );
    }

    #[test]
    fn compute_active_features_honours_explicit_empty_features_list() {
        // Regression: `{"name": "dep", "features": []}` must be honoured as "no features, only
        // untagged dirs". Previously the fallback for "no consumer edges" fired when `requested`
        // was empty, forcing all features on even though the consumer explicitly asked for none.
        let mut packages: AHashMap<String, super::Package> = AHashMap::new();

        let mut root_config = config::tests::create_config(config::tests::CreateConfigArgs {
            name: "root".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });
        root_config.sources = Some(config::OneOrMore::Single(config::Source::Shorthand(
            "src".to_string(),
        )));
        root_config.dependencies = Some(vec![config::Dependency::Qualified(config::QualifiedDependency {
            name: "dep".to_string(),
            features: Some(vec![]),
        })]);
        packages.insert(
            "root".to_string(),
            super::Package {
                name: "root".to_string(),
                config: root_config,
                source_folders: AHashSet::new(),
                source_files: None,
                namespace: super::Namespace::NoNamespace,
                modules: None,
                path: PathBuf::from("."),
                dirs: None,
                gentype_dirs: None,
                is_local_dep: true,
                is_root: true,
            },
        );

        packages.insert(
            "dep".to_string(),
            root_package_with_features(
                None,
                vec![
                    ("src", None),
                    ("src-native", Some("native")),
                    ("src-experimental", Some("experimental")),
                ],
            ),
        );
        packages.get_mut("dep").unwrap().is_root = false;

        let active = super::compute_active_features(&packages, None, false).unwrap();
        let dep_active = active.get("dep").unwrap();
        assert!(
            dep_active.is_empty(),
            "an explicit empty features list should activate no feature-tagged dirs, got {dep_active:?}"
        );
    }
}
