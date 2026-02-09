use std::path::{Path, PathBuf};

use ahash::AHashMap;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tracing::Instrument;

use crate::build::packages;
use crate::config;
use crate::project_context::ProjectContext;

/// Normalize a path to always use forward slashes (LSP glob patterns use forward slashes).
fn to_forward_slashes(s: &str) -> String {
    s.replace('\\', "/")
}

/// For a single package, compute watcher glob patterns from its source_folders.
/// Emits an OTEL span per source directory entry.
fn package_watcher_patterns(package: &packages::Package, project_root: &Path) -> Vec<String> {
    let rel_prefix = package.path.strip_prefix(project_root).unwrap_or(&package.path);

    let mut patterns = Vec::new();

    for source in &package.source_folders {
        let dir = if rel_prefix == Path::new("") {
            source.dir.clone()
        } else {
            to_forward_slashes(&format!("{}/{}", rel_prefix.display(), source.dir))
        };

        // Mirror watcher.rs logic: subdirs with Recurse(true) get recursive globs,
        // everything else (including explicitly listed subdirs, which get_source_dirs
        // already flattened) gets a flat glob.
        let is_recursive = matches!(source.subdirs, Some(config::Subdirs::Recurse(true)));
        let glob = if is_recursive {
            format!("{dir}/**")
        } else {
            dir.clone()
        };
        patterns.push(format!("{glob}/*.res"));
        patterns.push(format!("{glob}/*.resi"));

        tracing::info_span!("lsp.source_dir", dir = %dir, recursive = %is_recursive).in_scope(|| {});
    }

    patterns.sort();
    patterns
}

/// A discovered workspace: a project context and its packages.
pub struct DiscoveredWorkspace {
    pub project_context: ProjectContext,
    pub packages: AHashMap<String, packages::Package>,
}

/// Discover all packages in the workspace and build scoped watcher patterns.
///
/// Emits OTEL spans per package and per source directory so the snapshot
/// shows exactly what was found.
fn discover_workspace(project_root: &Path) -> (Vec<String>, Option<DiscoveredWorkspace>) {
    let mut all_patterns = vec!["**/rescript.json".to_string()];

    let project_context = match ProjectContext::new(project_root) {
        Ok(ctx) => ctx,
        Err(e) => {
            tracing::warn!(
                "Could not create project context for {}: {e}",
                project_root.display()
            );
            return (all_patterns, None);
        }
    };

    let packages = match packages::read_packages(&project_context, false) {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("Could not read packages in {}: {e}", project_root.display());
            return (all_patterns, None);
        }
    };

    // Sort by name for deterministic output
    let mut sorted_packages: Vec<_> = packages.values().collect();
    sorted_packages.sort_by_key(|p| &p.name);

    for package in sorted_packages {
        let patterns = tracing::info_span!("lsp.discover_package", name = %package.name)
            .in_scope(|| package_watcher_patterns(package, project_root));
        all_patterns.extend(patterns);
    }

    all_patterns.sort();
    all_patterns.dedup();
    (
        all_patterns,
        Some(DiscoveredWorkspace {
            project_context,
            packages,
        }),
    )
}

/// Find independent `rescript.json` projects under a workspace folder that are NOT
/// already covered by the root workspace's packages.
///
/// Walks the directory tree (skipping `node_modules/` and `lib/`) looking for
/// `rescript.json` files. Any project whose directory is not already in `covered_paths`
/// is returned as an independent project root.
fn find_independent_projects(
    workspace_root: &Path,
    covered_paths: &std::collections::HashSet<PathBuf>,
) -> Vec<PathBuf> {
    let mut independent = Vec::new();

    fn walk(dir: &Path, covered: &std::collections::HashSet<PathBuf>, results: &mut Vec<PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let dir_name = entry.file_name();
            if dir_name == "node_modules" || dir_name == "lib" || dir_name == ".git" {
                continue;
            }
            if path.join("rescript.json").exists() {
                let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
                if !covered.contains(&canonical) {
                    results.push(canonical);
                }
                // Don't recurse into discovered projects — their subdirectories
                // are handled by their own package discovery.
                continue;
            }
            walk(&path, covered, results);
        }
    }

    walk(workspace_root, covered_paths, &mut independent);
    independent.sort();
    independent
}

/// Build watcher patterns from workspace folders and register them with the client.
///
/// File watchers cover changes that happen *outside* the editor's open buffers.
/// Files open in editor tabs are covered by didOpen/didChange/didSave — those notifications
/// only fire for documents the editor is actively tracking. But many mutations happen
/// externally: git checkout/rebase, LLM agents writing files, terminal commands (mv, cp),
/// or other tools editing .res files. The editor's built-in watcher picks these up and
/// forwards them via workspace/didChangeWatchedFiles, but only for patterns we register here.
///
/// We scope watchers to the actual source directories declared in rescript.json rather than
/// using a blanket **/*.res — that would match files in node_modules/ and lib/bs/, putting
/// unnecessary load on the editor's file watcher and triggering spurious events for copied
/// build artifacts.
pub async fn register_file_watchers(
    client: &tower_lsp::Client,
    workspace_folders: &[String],
) -> Vec<DiscoveredWorkspace> {
    let mut workspaces = Vec::new();

    async {
        async {
            let mut watcher_patterns: Vec<String> = Vec::new();

            for folder in workspace_folders {
                let folder_path = Path::new(folder);
                let (patterns, workspace) = discover_workspace(folder_path);
                watcher_patterns.extend(patterns);

                // Collect paths already covered by the root workspace's packages
                let mut covered_paths = std::collections::HashSet::new();
                if let Some(ref ws) = workspace {
                    for pkg in ws.packages.values() {
                        covered_paths.insert(pkg.path.clone());
                    }
                }

                if let Some(ws) = workspace {
                    workspaces.push(ws);
                }

                // Discover independent projects not covered by the root
                let independent = find_independent_projects(folder_path, &covered_paths);
                for project_root in independent {
                    tracing::info!("Discovered independent project: {}", project_root.display());
                    let (ind_patterns, ind_workspace) = discover_workspace(&project_root);
                    watcher_patterns.extend(ind_patterns);
                    if let Some(ws) = ind_workspace {
                        workspaces.push(ws);
                    }
                }
            }

            let watcher_count = watcher_patterns.len();
            tracing::Span::current().record("watcher_count", watcher_count);

            let watchers = watcher_patterns
                .into_iter()
                .map(|pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(pattern),
                    kind: Some(WatchKind::all()),
                })
                .collect();

            let register_options =
                match serde_json::to_value(DidChangeWatchedFilesRegistrationOptions { watchers }) {
                    Ok(v) => Some(v),
                    Err(e) => {
                        tracing::warn!("Failed to serialize watcher registration options: {e}");
                        return;
                    }
                };

            let registration = Registration {
                id: "rescript-file-watcher".to_string(),
                method: notification::DidChangeWatchedFiles::METHOD.to_string(),
                register_options,
            };

            if let Err(e) = client.register_capability(vec![registration]).await {
                tracing::warn!("Failed to register file watchers: {e}");
            }
        }
        .instrument(tracing::info_span!(
            "lsp.register_watchers",
            watcher_count = tracing::field::Empty
        ))
        .await;
    }
    .instrument(tracing::info_span!("lsp.initialized"))
    .await;

    workspaces
}
