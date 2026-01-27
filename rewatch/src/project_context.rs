use crate::build::packages;
use crate::config::Config;
use crate::helpers;
use ahash::{AHashMap, AHashSet};
use anyhow::anyhow;
use anyhow::{Context, Result};
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

/// Monorepo context - contains local dependencies (symlinked in node_modules)
pub struct MonorepoContext {
    pub local_dependencies: AHashSet<String>,
    pub local_dev_dependencies: AHashSet<String>,
}

pub struct ProjectContext {
    pub current_config: Config,
    pub monorepo_context: Option<MonorepoContext>,
    pub node_modules_exist_cache: RwLock<AHashMap<PathBuf, bool>>, // caches whether a directory contains a node_modules subfolder
    pub packages_cache: RwLock<AHashMap<(PathBuf, String), PathBuf>>, // caches full results of helpers::try_package_path per (package_dir, package_name)
}

fn format_dependencies(dependencies: &AHashSet<String>) -> String {
    if dependencies.is_empty() {
        "[]".to_string()
    } else {
        let mut out = String::from("[\n");
        let mut first = true;

        for dep in dependencies {
            if !first {
                out.push_str(",\n");
            } else {
                first = false;
            }
            out.push_str("  \"");
            out.push_str(dep);
            out.push('"');
        }

        out.push_str("\n]");
        out
    }
}

impl fmt::Debug for ProjectContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.monorepo_context {
            None => {
                write!(
                    f,
                    "Single project: \"{}\" at \"{}\"",
                    &self.current_config.name,
                    &self.current_config.path.to_string_lossy()
                )
            }
            Some(MonorepoContext {
                local_dependencies,
                local_dev_dependencies,
            }) => {
                let deps = format_dependencies(local_dependencies);
                let dev_deps = format_dependencies(local_dev_dependencies);

                write!(
                    f,
                    "Monorepo root: \"{}\" at \"{}\" with dependencies:\n  {}\n  and devDependencies:\n {}",
                    &self.current_config.name,
                    &self.current_config.path.to_string_lossy(),
                    deps,
                    dev_deps
                )
            }
        }
    }
}
fn read_local_packages(
    folder_path: &Path,
    dependencies_from_config: &Vec<String>,
) -> Result<AHashSet<String>> {
    let mut local_dependencies = AHashSet::<String>::new();

    for dep in dependencies_from_config {
        // Monorepo packages are expected to be symlinked in node_modules.
        if let Ok(dep_path) = folder_path
            .join("node_modules")
            .join(dep)
            .canonicalize()
            .map(helpers::StrippedVerbatimPath::to_stripped_verbatim_path)
        {
            let is_local = helpers::is_local_package(folder_path, &dep_path);
            if is_local {
                local_dependencies.insert(dep.to_string());
            }

            tracing::debug!(
                dependency = %dep,
                is_local = is_local,
                path = %dep_path.display(),
                "Dependency resolved"
            );
        }
    }

    Ok(local_dependencies)
}

fn monorepo_or_single_project(path: &Path, current_config: Config) -> Result<ProjectContext> {
    let local_dependencies = match &current_config.dependencies {
        None => AHashSet::<String>::new(),
        Some(deps) => read_local_packages(path, deps)?,
    };
    let local_dev_dependencies = match &current_config.dev_dependencies {
        None => AHashSet::<String>::new(),
        Some(deps) => read_local_packages(path, deps)?,
    };
    if local_dependencies.is_empty() && local_dev_dependencies.is_empty() {
        Ok(ProjectContext {
            current_config,
            monorepo_context: None,
            node_modules_exist_cache: RwLock::new(AHashMap::new()),
            packages_cache: RwLock::new(AHashMap::new()),
        })
    } else {
        Ok(ProjectContext {
            current_config,
            monorepo_context: Some(MonorepoContext {
                local_dependencies,
                local_dev_dependencies,
            }),
            node_modules_exist_cache: RwLock::new(AHashMap::new()),
            packages_cache: RwLock::new(AHashMap::new()),
        })
    }
}

impl ProjectContext {
    pub fn new(path: &Path) -> Result<ProjectContext> {
        let path = helpers::get_abs_path(path);
        let current_config = packages::read_config(&path)
            .with_context(|| format!("Could not read rescript.json at {}", path.to_string_lossy()))?;
        let nearest_parent_config_path = match path.parent() {
            None => Err(anyhow!(
                "The current path \"{}\" does not have a parent folder",
                path.to_string_lossy()
            )),
            Some(parent) => Ok(helpers::get_nearest_config(parent)),
        }?;
        let context = match nearest_parent_config_path {
            None => monorepo_or_single_project(&path, current_config),
            Some(parent_config_path) => {
                match packages::read_config(parent_config_path.as_path()) {
                    Err(e) => Err(anyhow!(
                        "Could not read the parent config at {}: {}",
                        parent_config_path.to_string_lossy(),
                        e
                    )),
                    Ok(workspace_config)
                        if helpers::is_config_listed_in_workspace(&current_config, &workspace_config) =>
                    {
                        // There is a parent rescript.json, and it has a reference to the current package.
                        // Always create context at the parent (root) level - scoping is passed as a parameter to commands.
                        let parent_path = parent_config_path.parent().ok_or_else(|| {
                            anyhow!(
                                "Parent config path \"{}\" does not have a parent folder",
                                parent_config_path.to_string_lossy()
                            )
                        })?;
                        monorepo_or_single_project(parent_path, workspace_config)
                    }
                    Ok(_) => {
                        // There is a parent rescript.json, but it has no reference to the current package.
                        // However, the current package could still be a monorepo root!
                        monorepo_or_single_project(&path, current_config)
                    }
                }
            }
        };
        context.iter().for_each(|pc| {
            tracing::debug!(
                context = ?pc,
                path = %path.display(),
                "Created project context"
            );
        });
        context
    }

    pub fn get_root_config(&self) -> &Config {
        // With MonorepoPackage removed, current_config is always the root config
        &self.current_config
    }

    pub fn get_root_path(&self) -> &Path {
        self.get_root_config().path.parent().unwrap()
    }

    /// Returns all local packages in the project.
    /// For a single project, returns just that project.
    /// For a monorepo root, returns the root package plus all local dependencies.
    /// Scoping to specific packages is now handled by passing scope as a parameter to commands.
    pub fn get_scoped_local_packages(&self) -> AHashSet<String> {
        let mut local_packages = AHashSet::<String>::new();
        match &self.monorepo_context {
            None => {
                local_packages.insert(self.current_config.name.clone());
            }
            Some(MonorepoContext {
                local_dependencies,
                local_dev_dependencies,
            }) => {
                local_packages.insert(self.current_config.name.clone());
                local_packages.extend(local_dependencies.iter().cloned());
                local_packages.extend(local_dev_dependencies.iter().cloned());
            }
        };
        local_packages
    }
}
