//! File change handling for incremental builds.
//!
//! **Important:** The daemon does NOT watch the filesystem directly. Watch clients
//! (running `notify-rs` locally) send file change events to the daemon via the
//! `NotifyFileChange` RPC. This module debounces those notifications and submits
//! them as `WorkItem::FileChangeBuild` to the serialized work queue.
//!
//! This design keeps filesystem watching on the client side, allowing the daemon
//! to remain a pure build coordinator without platform-specific file watching concerns.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use tokio::sync::mpsc;

use crate::build::build_types::SourceType;
use crate::helpers::StrippedVerbatimPath;

use super::state::{CompileType, DaemonState, InternalFileChange};
use super::work_queue::WorkItem;

/// Returns a priority for CompileType - higher value means more significant change.
fn compile_type_priority(ct: CompileType) -> u8 {
    match ct {
        CompileType::Incremental => 0,
        CompileType::SourceCreated => 1,
        CompileType::SourceRenamed => 2,
        CompileType::SourceDeleted => 3,
        CompileType::ConfigChange => 4,
    }
}

/// Deduplicate file changes by keeping only one entry per file path,
/// with the most significant change type. Results are sorted by path
/// for deterministic ordering across platforms.
fn deduplicate_file_changes(changes: Vec<InternalFileChange>) -> Vec<InternalFileChange> {
    let mut by_path: HashMap<String, InternalFileChange> = HashMap::new();

    for change in changes {
        by_path
            .entry(change.path.clone())
            .and_modify(|existing| {
                if compile_type_priority(change.compile_type) > compile_type_priority(existing.compile_type) {
                    existing.compile_type = change.compile_type;
                }
            })
            .or_insert(change);
    }

    let mut result: Vec<_> = by_path.into_values().collect();
    result.sort_by(|a, b| a.path.cmp(&b.path));
    result
}

/// Mark a specific file as dirty in the build state for incremental compilation.
/// Returns true if the file was found and marked dirty.
pub fn mark_file_dirty(build_state: &mut crate::build::build_types::BuildState, file_path: &Path) -> bool {
    let canonicalized_path = match file_path.canonicalize().map(|p| p.to_stripped_verbatim_path()) {
        Ok(p) => p,
        Err(_) => return false,
    };

    // First, collect module names and their package paths to avoid borrow conflicts
    let module_package_paths: Vec<(String, PathBuf)> = build_state
        .module_name_package_pairs()
        .into_iter()
        .filter_map(|(module_name, package_name)| {
            build_state
                .packages
                .get(&package_name)
                .map(|p| (module_name, p.path.clone()))
        })
        .collect();

    for (module_name, package_path) in module_package_paths {
        let module = match build_state.modules.get_mut(&module_name) {
            Some(m) => m,
            None => continue,
        };

        match &mut module.source_type {
            SourceType::SourceFile(source_file) => {
                let canonicalized_impl = package_path.join(&source_file.implementation.path);
                if canonicalized_path == canonicalized_impl {
                    if let Ok(modified) = canonicalized_path.metadata().and_then(|x| x.modified()) {
                        source_file.implementation.last_modified = modified;
                    }
                    source_file.implementation.parse_dirty = true;
                    return true;
                }

                if let Some(ref mut interface) = source_file.interface {
                    let canonicalized_iface = package_path.join(&interface.path);
                    if canonicalized_path == canonicalized_iface {
                        if let Ok(modified) = canonicalized_path.metadata().and_then(|x| x.modified()) {
                            interface.last_modified = modified;
                        }
                        interface.parse_dirty = true;
                        return true;
                    }
                }
            }
            SourceType::MlMap(_) => (),
        }
    }

    false
}

/// Background task that receives file change notifications from watch clients,
/// debounces them (50ms), and submits batched changes to the work queue.
pub async fn process_file_change_notifications(
    state: Arc<DaemonState>,
    mut file_change_rx: mpsc::Receiver<InternalFileChange>,
) {
    let mut pending_changes: Vec<InternalFileChange> = Vec::new();

    loop {
        // Wait for file changes with a small timeout to batch them
        let change = tokio::time::timeout(std::time::Duration::from_millis(50), file_change_rx.recv()).await;

        match change {
            Ok(Some(file_change)) => {
                pending_changes.push(file_change);
            }
            Ok(None) => {
                // Channel closed, exit the loop
                break;
            }
            Err(_) => {
                // Timeout - process any pending changes
                if pending_changes.is_empty() {
                    continue;
                }

                // Check if watch client is still connected
                let watch_client_id = state.clients.get_watch_client_id().await;
                if watch_client_id.is_none() {
                    pending_changes.clear();
                    continue;
                }
                let watch_client_id = watch_client_id.unwrap();

                let changes = std::mem::take(&mut pending_changes);

                // Deduplicate file change events per path, keeping the most significant change type.
                // Priority: ConfigChange > SourceDeleted > SourceRenamed > SourceCreated > Incremental
                let changes = deduplicate_file_changes(changes);

                // Get the watch span for tracing file change builds
                let parent_span = state.get_watch_span();

                // Submit the batched changes to the work queue
                let _ = state
                    .work_tx
                    .send(WorkItem::FileChangeBuild {
                        client_id: watch_client_id,
                        changes,
                        parent_span,
                    })
                    .await;
            }
        }
    }
}
