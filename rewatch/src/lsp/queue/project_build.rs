use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::instrument;

use super::super::{ProjectMap, group_by_file, publish_and_store};
use super::PendingState;
use crate::build;
use crate::build::build_types::{
    BuildCommandState, BuildConfig, CompilationStage, CompileScope, OutputTarget, SourceType,
};
use crate::build::compile;
use crate::build::diagnostics::BscDiagnostic;
use crate::build::packages;
use crate::helpers;
use crate::lsp::diagnostic_store::DiagnosticStore;
use crate::project_context::ProjectContext;

/// Collect all source file URIs from a build state, for diagnostic clearing.
fn collect_source_uris(build_state: &BuildCommandState) -> HashSet<Url> {
    let mut uris = HashSet::new();
    for module in build_state.build_state.modules.values() {
        let SourceType::SourceFile(ref source_file) = module.source_type else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };
        let impl_path = package.path.join(&source_file.implementation.path);
        if let Ok(uri) = Url::from_file_path(&impl_path) {
            uris.insert(uri);
        }
        if let Some(ref interface) = source_file.interface {
            let iface_path = package.path.join(&interface.path);
            if let Ok(uri) = Url::from_file_path(&iface_path) {
                uris.insert(uri);
            }
        }
    }
    uris
}

/// Re-initialize a project from scratch (re-read packages, re-scan sources, rebuild).
/// Mirrors `initial_build::run()`.
#[instrument(name = "lsp.full_build", skip_all, fields(project = tracing::field::Empty))]
fn reinitialize_project(
    project_root: &Path,
    old_warn_error: Option<String>,
) -> Result<(BuildCommandState, Vec<BscDiagnostic>), String> {
    let project_context =
        ProjectContext::new(project_root).map_err(|e| format!("ProjectContext::new failed: {e}"))?;

    tracing::Span::current().record("project", project_context.get_root_config().name.as_str());

    let discovered_packages =
        packages::read_packages(&project_context, false).map_err(|e| format!("read_packages failed: {e}"))?;
    let packages_with_sources = packages::extend_with_children(&None, discovered_packages);

    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::FullTypecheck,
    };

    let mut build_state = build::prepare_build(
        project_context,
        packages_with_sources,
        Some(std::time::Duration::ZERO),
        false,
        true,
        old_warn_error,
        &build_config,
    )
    .map_err(|e| format!("prepare_build failed: {e}"))?;

    // TypecheckOnly doesn't produce .cmj files. Downgrade any Built modules.
    for module in build_state.build_state.modules.values_mut() {
        if module.compilation_stage == CompilationStage::Built {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    let parse_warnings = match build::parse_and_resolve(
        &mut build_state,
        build_config.output,
        build_config.scope.mode(),
        false,
        true,
        Some(std::time::Duration::ZERO),
        false,
    ) {
        Ok(warnings) => warnings,
        Err(e) => {
            tracing::warn!("Project build parse failed: {e}");
            return Ok((build_state, e.diagnostics));
        }
    };

    let compile_universe =
        compile::compute_compile_universe(&build_state, build_config.scope.mode().target_stage());

    let diagnostics = match build::incremental_build(
        &mut build_state,
        build_config,
        compile_universe,
        parse_warnings,
        Some(std::time::Duration::ZERO),
        true,
        false,
        false,
        false,
        true,
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Full build completed with errors: {e}");
            e.diagnostics
        }
    };

    Ok((build_state, diagnostics))
}

/// Result of a full build for one project.
struct FullBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    old_uris: HashSet<Url>,
    new_uris: HashSet<Url>,
}

/// Full project rebuild triggered by file creation or deletion. Groups
/// changed paths by project root, cleans up associated artifacts for
/// deleted files, reinitializes each affected project from scratch, and
/// publishes diagnostics (clearing stale ones for removed files).
/// Per-project grouping of created and deleted files.
struct ProjectBuildGroup {
    created_files: HashSet<PathBuf>,
    deleted_files: HashSet<PathBuf>,
    config_changed: bool,
}

impl ProjectBuildGroup {
    fn new() -> Self {
        ProjectBuildGroup {
            created_files: HashSet::new(),
            deleted_files: HashSet::new(),
            config_changed: false,
        }
    }

    /// All changed paths (union of created + deleted).
    fn all_paths(&self) -> HashSet<PathBuf> {
        self.created_files
            .iter()
            .chain(&self.deleted_files)
            .cloned()
            .collect()
    }
}

pub(super) async fn run(
    state: &mut PendingState,
    projects: &Arc<Mutex<ProjectMap>>,
    client: &Client,
    diagnostic_store: Option<&DiagnosticStore>,
) {
    let pending_builds = std::mem::take(&mut state.build_projects);
    let projects_clone = Arc::clone(projects);

    let parent_span = tracing::Span::current();
    let results: Vec<FullBuildResult> = tokio::task::spawn_blocking(move || {
        let _entered = parent_span.enter();
        let mut results = Vec::new();

        // Group paths by project root, resolving roots under lock.
        let grouped: HashMap<PathBuf, ProjectBuildGroup> = {
            let guard = match projects_clone.lock() {
                Ok(g) => g,
                Err(e) => {
                    tracing::error!("projects mutex poisoned in full build flush: {e}");
                    return results;
                }
            };
            let mut map: HashMap<PathBuf, ProjectBuildGroup> = HashMap::new();
            for path in pending_builds.created_files {
                if let Some(root) = guard.project_root_for_path(&path) {
                    map.entry(root)
                        .or_insert_with(ProjectBuildGroup::new)
                        .created_files
                        .insert(path);
                } else {
                    tracing::debug!(
                        file = %path.display(),
                        "File does not belong to any known project, ignoring"
                    );
                }
            }
            for path in pending_builds.deleted_files {
                if let Some(root) = guard.project_root_for_path(&path) {
                    map.entry(root)
                        .or_insert_with(ProjectBuildGroup::new)
                        .deleted_files
                        .insert(path);
                } else {
                    tracing::debug!(
                        file = %path.display(),
                        "File does not belong to any known project, ignoring"
                    );
                }
            }
            // Config changes: the rescript.json's parent directory is the project root.
            for path in pending_builds.config_changed {
                if let Some(root) = path.parent() {
                    let root = root.to_path_buf();
                    if guard.states.contains_key(&root) {
                        map.entry(root)
                            .or_insert_with(ProjectBuildGroup::new)
                            .config_changed = true;
                    } else {
                        tracing::debug!(
                            file = %path.display(),
                            "Config file does not belong to any known project, ignoring"
                        );
                    }
                }
            }
            map
        };

        for (project_root, group) in grouped {
            let changed_paths = group.all_paths();
            // Collect old URIs and warn_error under lock, then release.
            // Also check whether any of the triggering files are still
            // relevant to the current build state — if none are, a prior
            // full build already handled them and we can skip.
            // While we hold the lock, also determine which associated files
            // to clean up for deleted .res files (needs old build state).
            let (old_uris, old_warn_error, files_to_cleanup) = {
                let guard = match projects_clone.lock() {
                    Ok(g) => g,
                    Err(e) => {
                        tracing::error!("projects mutex poisoned in full build flush: {e}");
                        continue;
                    }
                };
                match guard.states.get(&project_root) {
                    Some(old_state) => {
                        let any_relevant = group.config_changed
                            || changed_paths.iter().any(|path| {
                                // Created files won't be in the state yet,
                                // so they are always relevant.
                                if path.exists() {
                                    return true;
                                }
                                // Deleted files: relevant only if still known.
                                find_module_by_path(old_state, path).is_some()
                            });
                        if !any_relevant {
                            tracing::debug!(
                                project = %project_root.display(),
                                "All triggering files already handled, skipping full build"
                            );
                            continue;
                        }

                        // Collect files to clean up for deleted .res files.
                        let cleanup: Vec<PathBuf> = changed_paths
                            .iter()
                            .filter(|p| !p.exists())
                            .flat_map(|p| get_files_to_cleanup_on_delete(old_state, p))
                            .collect();

                        (
                            collect_source_uris(old_state),
                            old_state.get_warn_error_override().clone(),
                            cleanup,
                        )
                    }
                    None => {
                        tracing::warn!(
                            "No existing state for project root {}, skipping full build",
                            project_root.display()
                        );
                        continue;
                    }
                }
            };

            // Delete associated files from disk.
            for file in &files_to_cleanup {
                let _ = std::fs::remove_file(file);
                tracing::debug!("Cleaned up: {}", file.display());
            }

            match reinitialize_project(&project_root, old_warn_error) {
                Ok((new_state, diagnostics)) => {
                    let new_uris = collect_source_uris(&new_state);

                    // Replace state under lock
                    if let Ok(mut guard) = projects_clone.lock() {
                        guard.states.insert(project_root.clone(), new_state);
                        // Invalidate uri_cache entries for this project
                        guard.uri_cache.retain(|_, root| root != &project_root);
                    }

                    results.push(FullBuildResult {
                        diagnostics,
                        old_uris,
                        new_uris,
                    });
                }
                Err(e) => {
                    tracing::error!("Full build failed for {}: {e}", project_root.display());
                }
            }
        }

        results
    })
    .await
    .unwrap_or_else(|e| {
        tracing::error!("full build flush task panicked: {e}");
        Vec::new()
    });

    // Publish diagnostics and clear stale ones.
    // Files that were deleted (or their associated .resi cleaned up) will
    // appear in old_uris but not new_uris, so their diagnostics get cleared
    // by the old/new diff below.
    for result in &results {
        let by_file = group_by_file(&result.diagnostics);

        // Publish diagnostics for all files in the new state
        for uri in &result.new_uris {
            let diags = by_file.get(uri).cloned().unwrap_or_default();
            publish_and_store(client, diagnostic_store, uri.clone(), diags).await;
        }

        // Clear diagnostics for files that existed in the old state but not the new
        for uri in &result.old_uris {
            if !result.new_uris.contains(uri) {
                publish_and_store(client, diagnostic_store, uri.clone(), vec![]).await;
            }
        }
    }
}

/// Like `find_module_for_file` but does NOT canonicalize the input path.
/// Useful for deleted files where `canonicalize()` would fail.
/// Compares `package.path.join(source_path)` directly against `file_path`.
///
/// Returns `(module_name, package_name, is_interface)`.
fn find_module_by_path(state: &BuildCommandState, file_path: &Path) -> Option<(String, String, bool)> {
    for (module_name, module) in &state.build_state.modules {
        let package = state.build_state.packages.get(&module.package_name)?;

        if let SourceType::SourceFile(source_file) = &module.source_type {
            let impl_path = package.path.join(&source_file.implementation.path);
            if file_path == impl_path {
                return Some((module_name.clone(), module.package_name.clone(), false));
            }

            if let Some(interface) = &source_file.interface {
                let iface_path = package.path.join(&interface.path);
                if file_path == iface_path {
                    return Some((module_name.clone(), module.package_name.clone(), true));
                }
            }
        }
    }

    None
}

/// Given a deleted file path, determine which associated files should be cleaned up.
///
/// Rules:
/// - Deleting a `.res` file → also delete its `.resi` (if exists) and compiled JS for both
/// - Deleting a `.resi` file → do nothing (don't delete `.res` or JS)
///
/// Returns the list of files to remove from disk.
fn get_files_to_cleanup_on_delete(state: &BuildCommandState, file_path: &Path) -> Vec<PathBuf> {
    let Some((module_name, _package_name, is_interface)) = find_module_by_path(state, file_path) else {
        return Vec::new();
    };

    // Only clean up when the implementation (.res) is deleted, not the interface (.resi).
    if is_interface {
        return Vec::new();
    }

    let Some(module) = state.build_state.modules.get(&module_name) else {
        return Vec::new();
    };
    let SourceType::SourceFile(ref source_file) = module.source_type else {
        return Vec::new();
    };
    let Some(package) = state.build_state.packages.get(&module.package_name) else {
        return Vec::new();
    };

    let root_config = state.build_state.get_root_config();
    let mut files = Vec::new();

    // Include the .resi file if one exists.
    if let Some(ref interface) = source_file.interface {
        files.push(package.path.join(&interface.path));
    }

    // Include compiled JS output for each package spec.
    for spec in root_config.get_package_specs() {
        let suffix = root_config.get_suffix(&spec);

        // JS for the implementation file.
        let impl_js = if spec.in_source {
            helpers::get_source_file_from_rescript_file(
                &package.path.join(&source_file.implementation.path),
                &suffix,
            )
        } else {
            helpers::get_source_file_from_rescript_file(
                &package
                    .path
                    .join("lib")
                    .join(spec.get_out_of_source_dir())
                    .join(&source_file.implementation.path),
                &suffix,
            )
        };
        files.push(impl_js);

        // JS for the interface file (if it exists).
        if let Some(ref interface) = source_file.interface {
            let iface_js = if spec.in_source {
                helpers::get_source_file_from_rescript_file(&package.path.join(&interface.path), &suffix)
            } else {
                helpers::get_source_file_from_rescript_file(
                    &package
                        .path
                        .join("lib")
                        .join(spec.get_out_of_source_dir())
                        .join(&interface.path),
                    &suffix,
                )
            };
            files.push(iface_js);
        }
    }

    files
}
