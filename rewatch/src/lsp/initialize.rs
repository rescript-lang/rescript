use std::path::Path;

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

/// Discover all packages in the workspace and build scoped watcher patterns.
///
/// Emits OTEL spans per package and per source directory so the snapshot
/// shows exactly what was found.
fn discover_workspace(project_root: &Path) -> (Vec<String>, AHashMap<String, packages::Package>) {
    let mut all_patterns = vec!["**/rescript.json".to_string()];

    let project_context = match ProjectContext::new(project_root) {
        Ok(ctx) => ctx,
        Err(e) => {
            tracing::warn!(
                "Could not create project context for {}: {e}",
                project_root.display()
            );
            return (all_patterns, AHashMap::new());
        }
    };

    let packages = match packages::read_packages(&project_context, false) {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("Could not read packages in {}: {e}", project_root.display());
            return (all_patterns, AHashMap::new());
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
    (all_patterns, packages)
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
) -> AHashMap<String, packages::Package> {
    let mut all_packages = AHashMap::new();

    async {
        async {
            let mut watcher_patterns: Vec<String> = Vec::new();

            for folder in workspace_folders {
                let (patterns, packages) = discover_workspace(Path::new(folder));
                watcher_patterns.extend(patterns);
                all_packages.extend(packages);
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

    all_packages
}
