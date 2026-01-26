//! Package resolution helpers that require a BuildReporter.
//!
//! These functions run in the daemon context and use tracing for debugging.

use crate::build::BuildReporter;
use crate::config::Config;
use crate::helpers;
use crate::project_context::ProjectContext;
use anyhow::anyhow;
use std::path::{Path, PathBuf};

/// Cached check: does the given directory contain a node_modules subfolder?
fn has_node_modules_cached<R: BuildReporter>(
    project_context: &ProjectContext,
    dir: &Path,
    _reporter: &R,
) -> bool {
    match project_context.node_modules_exist_cache.read() {
        Ok(cache) => {
            if let Some(exists) = cache.get(dir) {
                return *exists;
            }
        }
        Err(poisoned) => {
            tracing::warn!("node_modules_exist_cache read lock poisoned; recovering");
            let cache = poisoned.into_inner();
            if let Some(exists) = cache.get(dir) {
                return *exists;
            }
        }
    }
    let exists = dir.join("node_modules").exists();
    match project_context.node_modules_exist_cache.write() {
        Ok(mut cache) => {
            cache.insert(dir.to_path_buf(), exists);
        }
        Err(poisoned) => {
            tracing::warn!("node_modules_exist_cache write lock poisoned; recovering");
            let mut cache = poisoned.into_inner();
            cache.insert(dir.to_path_buf(), exists);
        }
    }
    exists
}

/// Tap-style helper: cache and return the value (single clone for cache insert)
fn cache_package_tap<R: BuildReporter>(
    project_context: &ProjectContext,
    key: &(PathBuf, String),
    value: PathBuf,
    _reporter: &R,
) -> anyhow::Result<PathBuf> {
    match project_context.packages_cache.write() {
        Ok(mut cache) => {
            cache.insert(key.clone(), value.clone());
        }
        Err(poisoned) => {
            tracing::warn!("packages_cache write lock poisoned; recovering");
            let mut cache = poisoned.into_inner();
            cache.insert(key.clone(), value.clone());
        }
    }
    Ok(value)
}

/// Tries to find a path for input package_name.
/// The node_modules folder may be found at different levels in the case of a monorepo.
/// This helper tries a variety of paths.
pub fn try_package_path<R: BuildReporter>(
    package_config: &Config,
    project_context: &ProjectContext,
    package_name: &str,
    reporter: &R,
) -> anyhow::Result<PathBuf> {
    // try cached result first, keyed by (package_dir, package_name)
    let pkg_name = package_name.to_string();
    let package_dir = package_config
        .path
        .parent()
        .ok_or_else(|| {
            anyhow!(
                "Expected {} to have a parent folder",
                package_config.path.to_string_lossy()
            )
        })?
        .to_path_buf();

    let cache_key = (package_dir.clone(), pkg_name.clone());
    match project_context.packages_cache.read() {
        Ok(cache) => {
            if let Some(cached) = cache.get(&cache_key) {
                return Ok(cached.clone());
            }
        }
        Err(poisoned) => {
            tracing::warn!("packages_cache read lock poisoned; recovering");
            let cache = poisoned.into_inner();
            if let Some(cached) = cache.get(&cache_key) {
                return Ok(cached.clone());
            }
        }
    }

    // package folder + node_modules + package_name
    // This can happen in the following scenario:
    // The ProjectContext has a MonoRepoContext::MonorepoRoot.
    // We are reading a dependency from the root package.
    // And that local dependency has a hoisted dependency.
    // Example, we need to find package_name `foo` in the following scenario:
    // root/packages/a/node_modules/foo
    let path_from_current_package = helpers::package_path(&package_dir, package_name);

    // current folder + node_modules + package_name
    let path_from_current_config = project_context
        .current_config
        .path
        .parent()
        .ok_or_else(|| {
            anyhow!(
                "Expected {} to have a parent folder",
                project_context.current_config.path.to_string_lossy()
            )
        })
        .map(|parent_path| helpers::package_path(parent_path, package_name))?;

    // root folder + node_modules + package_name
    let path_from_root = helpers::package_path(project_context.get_root_path(), package_name);
    if path_from_current_package.exists() {
        cache_package_tap(project_context, &cache_key, path_from_current_package, reporter)
    } else if path_from_current_config.exists() {
        cache_package_tap(project_context, &cache_key, path_from_current_config, reporter)
    } else if path_from_root.exists() {
        cache_package_tap(project_context, &cache_key, path_from_root, reporter)
    } else {
        // As a last resort, when we're in a Single project context, traverse upwards
        // starting from the parent of the package root (package_config.path.parent().parent())
        // and probe each ancestor's node_modules for the dependency. This covers hoisted
        // workspace setups when building a package standalone.
        if project_context.monorepo_context.is_none() {
            match package_config.path.parent().and_then(|p| p.parent()) {
                Some(start_dir) => {
                    return find_dep_in_upward_node_modules(
                        project_context,
                        start_dir,
                        package_name,
                        reporter,
                    )
                    .and_then(|p| cache_package_tap(project_context, &cache_key, p, reporter));
                }
                None => {
                    tracing::debug!(
                        path = %package_config.path.to_string_lossy(),
                        "try_package_path: cannot compute start directory for upward traversal"
                    );
                }
            }
        }

        Err(anyhow!(
            "The package \"{package_name}\" is not found (are node_modules up-to-date?)..."
        ))
    }
}

fn find_dep_in_upward_node_modules<R: BuildReporter>(
    project_context: &ProjectContext,
    start_dir: &Path,
    package_name: &str,
    reporter: &R,
) -> anyhow::Result<PathBuf> {
    tracing::debug!(
        package_name = %package_name,
        start_dir = %start_dir.to_string_lossy(),
        "try_package_path: falling back to upward traversal"
    );

    let mut current = Some(start_dir);
    while let Some(dir) = current {
        if has_node_modules_cached(project_context, dir, reporter) {
            let candidate = helpers::package_path(dir, package_name);
            tracing::debug!(
                candidate = %candidate.to_string_lossy(),
                "try_package_path: checking"
            );
            if candidate.exists() {
                tracing::debug!(
                    package_name = %package_name,
                    path = %candidate.to_string_lossy(),
                    "try_package_path: found via upward traversal"
                );
                return Ok(candidate);
            }
        }
        current = dir.parent();
    }
    tracing::debug!(
        package_name = %package_name,
        start_dir = %start_dir.to_string_lossy(),
        "try_package_path: no package found during upward traversal"
    );
    Err(anyhow!(
        "try_package_path: upward traversal did not find '{}' starting at '{}'",
        package_name,
        start_dir.to_string_lossy()
    ))
}
