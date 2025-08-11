use crate::build::packages;
use crate::config::Config;
use crate::helpers;
use ahash::AHashSet;
use anyhow::Result;
use anyhow::anyhow;
use log::debug;
use std::fmt;
use std::path::{Path, PathBuf};

type RescriptJsonPath = PathBuf;

/// Represents the context of a command, categorizing in various ways:
/// - SingleProject: one rescript.json with no parent or children.
/// - MonorepoRoot: a rescript.json with (dev-)dependencies found in subfolders.
/// - MonorepoPackage: a rescript.json with a parent rescript.json.
///   The current package name is listed in the parent (dev-)dependencies.
// I don't think this linting rule matters very much as we only create a single instance of this enum.
// See https://rust-lang.github.io/rust-clippy/master/index.html#large_enum_variant
#[allow(clippy::large_enum_variant)]
pub enum ProjectContext {
    /// Single project - no workspace relationship
    SingleProject { config: Config, path: RescriptJsonPath },
    /// Monorepo root - contains local dependencies (symlinked in node_modules)
    MonorepoRoot {
        config: Config,
        path: RescriptJsonPath,
        local_dependencies: AHashSet<String>, // names of local deps
    },
    /// Package within a monorepo - has a parent workspace
    MonorepoPackage {
        config: Config,
        // This field is only used for debugging purposes, maybe it could be removed in the future.
        path: RescriptJsonPath,
        parent_config: Config,
        parent_path: RescriptJsonPath,
    },
}

impl fmt::Debug for ProjectContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ProjectContext::SingleProject { config, path } => {
                write!(
                    f,
                    "SingleProject: \"{}\" at \"{}\"",
                    config.name,
                    path.to_string_lossy()
                )
            }
            ProjectContext::MonorepoRoot {
                config,
                path,
                local_dependencies,
            } => {
                let deps = if local_dependencies.is_empty() {
                    String::from("[]")
                } else {
                    format!(
                        "[\n{}\n]",
                        local_dependencies
                            .iter()
                            .map(|dep| format!("  \"{dep}\""))
                            .collect::<Vec<String>>()
                            .join(",\n")
                    )
                };

                write!(
                    f,
                    "MonorepoRoot: \"{}\" at \"{}\" with {}",
                    config.name,
                    path.to_string_lossy(),
                    deps
                )
            }
            ProjectContext::MonorepoPackage {
                config,
                path,
                parent_config,
                parent_path,
            } => {
                write!(
                    f,
                    "MonorepoPackage:\n  \"{}\" at \"{}\"\n  with parent \"{}\" at \"{}\"",
                    config.name,
                    path.to_string_lossy(),
                    parent_config.name,
                    parent_path.to_string_lossy()
                )
            }
        }
    }
}
fn read_local_packages(folder_path: &Path, config: &Config) -> Result<AHashSet<String>> {
    let mut local_dependencies = AHashSet::<String>::new();
    let mut dependencies = AHashSet::<String>::new();
    for dep in config.dependencies.as_ref().unwrap_or(&vec![]) {
        dependencies.insert(dep.clone());
    }
    for dev_dep in config.dev_dependencies.as_ref().unwrap_or(&vec![]) {
        dependencies.insert(dev_dep.clone());
    }

    for dep in dependencies {
        // Monorepo packages are expected to be symlinked in node_modules.
        if let Ok(dep_path) = folder_path
            .join("node_modules")
            .join(&dep)
            .canonicalize()
            .map(helpers::StrippedVerbatimPath::to_stripped_verbatim_path)
        {
            debug!(
                "Checking if dependency \"{}\" is a local package at \"{}\"",
                dep,
                dep_path.display()
            );
            if helpers::is_local_package(folder_path, &dep_path) {
                local_dependencies.insert(dep.to_string());
            }
        }
    }

    Ok(local_dependencies)
}

fn monorepo_or_single_project(
    path: &Path,
    current_rescript_json: RescriptJsonPath,
    current_config: Config,
) -> Result<ProjectContext> {
    let local_dependencies = read_local_packages(path, &current_config)?;
    if local_dependencies.is_empty() {
        Ok(ProjectContext::SingleProject {
            config: current_config,
            path: current_rescript_json.clone(),
        })
    } else {
        Ok(ProjectContext::MonorepoRoot {
            config: current_config,
            path: current_rescript_json.clone(),
            local_dependencies,
        })
    }
}

impl ProjectContext {
    pub fn new(path: &Path) -> Result<ProjectContext> {
        let path = helpers::get_abs_path(path);
        let (current_config, current_rescript_json) = packages::read_config(&path)
            .map_err(|_| anyhow!("Could not read rescript.json at {}", path.to_string_lossy()))?;
        let nearest_parent_config_path = match path.parent() {
            None => Err(anyhow!(
                "The current path \"{}\" does not have a parent folder",
                path.to_string_lossy()
            )),
            Some(parent) => Ok(helpers::get_nearest_config(parent)),
        }?;
        let context = match nearest_parent_config_path {
            None => monorepo_or_single_project(&path, current_rescript_json, current_config),
            Some(parent_config_path) => {
                match packages::read_config(parent_config_path.as_path()) {
                    Err(e) => Err(anyhow!(
                        "Could not read the parent config at {}: {}",
                        parent_config_path.to_string_lossy(),
                        e
                    )),
                    Ok((workspace_config, workspace_rescript_json)) => {
                        let is_current_config_listed_in_workspace = workspace_config
                            .dependencies
                            .to_owned()
                            .unwrap_or_default()
                            .iter()
                            .any(|dep| dep == &current_config.name)
                            || workspace_config
                                .dev_dependencies
                                .to_owned()
                                .unwrap_or_default()
                                .iter()
                                .any(|dep| dep == &current_config.name);

                        if is_current_config_listed_in_workspace {
                            // There is a parent rescript.json, and it has a reference to the current package.
                            Ok(ProjectContext::MonorepoPackage {
                                config: current_config,
                                path: path.join("rescript.json"),
                                parent_config: workspace_config,
                                parent_path: workspace_rescript_json.clone(),
                            })
                        } else {
                            // There is a parent rescript.json, but it has no reference to the current package.
                            // However, the current package could still be a monorepo root!
                            monorepo_or_single_project(&path, current_rescript_json, current_config)
                        }
                    }
                }
            }
        };
        context.iter().for_each(|pc| {
            debug!("Created project context {:#?} for \"{}\"", pc, path.display());
        });
        context
    }

    pub fn get_root_config(&self) -> &Config {
        match self {
            ProjectContext::SingleProject { config, .. } => config,
            ProjectContext::MonorepoRoot { config, .. } => config,
            ProjectContext::MonorepoPackage {
                parent_config: config,
                ..
            } => config,
        }
    }

    pub fn get_root_path(&self) -> &Path {
        let rescript_json = match self {
            ProjectContext::SingleProject { path, .. } => path,
            ProjectContext::MonorepoRoot { path, .. } => path,
            ProjectContext::MonorepoPackage {
                parent_path: path, ..
            } => path,
        };
        rescript_json.parent().unwrap()
    }

    pub fn get_suffix(&self) -> String {
        match self {
            ProjectContext::SingleProject { config, .. }
            | ProjectContext::MonorepoRoot { config, .. }
            | ProjectContext::MonorepoPackage {
                parent_config: config,
                ..
            } => config.suffix.clone().unwrap_or(String::from(".res.mjs")),
        }
    }

    /// Returns the local packages relevant for the current context.
    /// Either a single project, all projects from a monorepo or a single package inside a monorepo.
    pub fn get_scoped_local_packages(&self) -> AHashSet<String> {
        let mut local_packages = AHashSet::<String>::new();
        match &self {
            ProjectContext::SingleProject { config, .. } => {
                local_packages.insert(config.name.clone());
            }
            ProjectContext::MonorepoRoot {
                config,
                local_dependencies,
                ..
            } => {
                local_packages.insert(config.name.clone());
                for dep in local_dependencies {
                    local_packages.insert(dep.clone());
                }
            }
            ProjectContext::MonorepoPackage { config, .. } => {
                local_packages.insert(config.name.clone());
            }
        };
        local_packages
    }

    pub fn get_current_rescript_config(&self) -> &Config {
        match &self {
            ProjectContext::SingleProject { config, .. }
            | ProjectContext::MonorepoRoot { config, .. }
            | ProjectContext::MonorepoPackage { config, .. } => config,
        }
    }
}
