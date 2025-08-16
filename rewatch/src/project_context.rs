use crate::build::packages;
use crate::config::Config;
use crate::helpers;
use ahash::AHashSet;
use anyhow::Result;
use anyhow::anyhow;
use log::debug;
use std::fmt;
use std::path::Path;

pub enum MonoRepoContext {
    /// Monorepo root - contains local dependencies (symlinked in node_modules)
    MonorepoRoot {
        local_dependencies: AHashSet<String>, // names of local deps
        local_dev_dependencies: AHashSet<String>,
    },
    /// Package within a monorepo - has a parent workspace
    MonorepoPackage { parent_config: Box<Config> },
}

pub struct ProjectContext {
    pub current_config: Config,
    pub monorepo_context: Option<MonoRepoContext>,
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
            Some(MonoRepoContext::MonorepoRoot {
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
            Some(MonoRepoContext::MonorepoPackage { parent_config }) => {
                write!(
                    f,
                    "MonorepoPackage:\n  \"{}\" at \"{}\"\n  with parent \"{}\" at \"{}\"",
                    &self.current_config.name,
                    &self.current_config.path.to_string_lossy(),
                    parent_config.name,
                    parent_config.path.to_string_lossy()
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

            debug!(
                "Dependency \"{}\" is a {}local package at \"{}\"",
                dep,
                if is_local { "" } else { "non-" },
                dep_path.display()
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
        })
    } else {
        Ok(ProjectContext {
            current_config,
            monorepo_context: Some(MonoRepoContext::MonorepoRoot {
                local_dependencies,
                local_dev_dependencies,
            }),
        })
    }
}

fn is_config_listed_in_workspace(current_config: &Config, workspace_config: &Config) -> bool {
    workspace_config
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
            .any(|dep| dep == &current_config.name)
}

impl ProjectContext {
    pub fn new(path: &Path) -> Result<ProjectContext> {
        let path = helpers::get_abs_path(path);
        let current_config = packages::read_config(&path)
            .map_err(|_| anyhow!("Could not read rescript.json at {}", path.to_string_lossy()))?;
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
                        if is_config_listed_in_workspace(&current_config, &workspace_config) =>
                    {
                        // There is a parent rescript.json, and it has a reference to the current package.
                        Ok(ProjectContext {
                            current_config,
                            monorepo_context: Some(MonoRepoContext::MonorepoPackage {
                                parent_config: Box::new(workspace_config),
                            }),
                        })
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
            debug!("Created project context {:#?} for \"{}\"", pc, path.display());
        });
        context
    }

    pub fn get_root_config(&self) -> &Config {
        match &self.monorepo_context {
            None => &self.current_config,
            Some(MonoRepoContext::MonorepoRoot { .. }) => &self.current_config,
            Some(MonoRepoContext::MonorepoPackage { parent_config }) => parent_config,
        }
    }

    pub fn get_root_path(&self) -> &Path {
        self.get_root_config().path.parent().unwrap()
    }

    /// Returns the local packages relevant for the current context.
    /// Either a single project, all projects from a monorepo or a single package inside a monorepo.
    pub fn get_scoped_local_packages(&self, include_dev_deps: bool) -> AHashSet<String> {
        let mut local_packages = AHashSet::<String>::new();
        match &self.monorepo_context {
            None => {
                local_packages.insert(self.current_config.name.clone());
            }
            Some(MonoRepoContext::MonorepoRoot {
                local_dependencies,
                local_dev_dependencies,
            }) => {
                local_packages.insert(self.current_config.name.clone());
                local_packages.extend(local_dependencies.iter().cloned());
                if include_dev_deps {
                    local_packages.extend(local_dev_dependencies.iter().cloned());
                }
            }
            Some(MonoRepoContext::MonorepoPackage { .. }) => {
                local_packages.insert(self.current_config.name.clone());
            }
        };
        local_packages
    }
}
