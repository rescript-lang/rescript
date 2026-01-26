use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Result;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;
use tokio_stream::StreamExt;

#[cfg(unix)]
use tokio::signal::unix::{SignalKind, signal};

use super::{connection, output};
use crate::cmd;
use crate::daemon::proto::{
    ConfigChangeNotification, DisconnectRequest, FileChangeNotification, FileChangeType, WatchRequest,
    daemon_event::Event as DaemonEventVariant,
};

/// Collect the current mtime of all .res/.resi files under `folder`.
/// This snapshot lets us distinguish genuine file changes from phantom
/// FSEvents notifications that arrive after the watcher starts.
fn snapshot_mtimes(folders: &[PathBuf]) -> HashMap<PathBuf, SystemTime> {
    let mut map = HashMap::new();
    for folder in folders {
        if let Ok(canonical) = folder.canonicalize() {
            collect_mtimes_recursive(&canonical, &mut map);
        }
    }
    map
}

fn collect_mtimes_recursive(dir: &Path, map: &mut HashMap<PathBuf, SystemTime>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(_) => return,
    };
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_dir() {
            collect_mtimes_recursive(&path, map);
        } else if is_rescript_file(&path)
            && !is_in_build_path(&path)
            && let Ok(mtime) = path.metadata().and_then(|m| m.modified())
        {
            map.insert(path, mtime);
        }
    }
}

/// Tracks which paths are being watched and the set of config file paths.
struct WatchState {
    /// The notify-rs watcher instance. Replaced when watch paths change.
    watcher: RecommendedWatcher,
    /// The set of config file paths being watched (canonicalized).
    config_paths: HashSet<PathBuf>,
    /// Mtime snapshot for deduplication.
    last_mtime: HashMap<PathBuf, SystemTime>,
}

/// Set up notify-rs watchers for the given source and config paths.
/// Returns a new WatchState with fresh watchers.
fn setup_watchers(
    source_paths: &[(PathBuf, bool)],
    config_paths: &[PathBuf],
    tx: &Arc<mpsc::Sender<Event>>,
) -> WatchState {
    let tx_clone = tx.clone();
    let watcher = RecommendedWatcher::new(
        move |res: Result<Event, notify::Error>| {
            if let Ok(event) = res {
                let _ = tx_clone.blocking_send(event);
            }
        },
        Config::default(),
    )
    .expect("Could not create watcher");

    let mut state = WatchState {
        watcher,
        config_paths: HashSet::new(),
        last_mtime: HashMap::new(),
    };

    // Watch source directories
    let mut source_dirs_for_snapshot = Vec::new();
    for (path, recursive) in source_paths {
        let mode = if *recursive {
            RecursiveMode::Recursive
        } else {
            RecursiveMode::NonRecursive
        };
        if let Err(e) = state.watcher.watch(path, mode) {
            eprintln!("Warning: could not watch {}: {}", path.display(), e);
        } else {
            source_dirs_for_snapshot.push(path.clone());
        }
    }

    // Watch config files (watch their parent directory non-recursively since
    // notify-rs doesn't support watching individual files on all platforms)
    for config_path in config_paths {
        let canonical = config_path.canonicalize().unwrap_or_else(|_| config_path.clone());
        state.config_paths.insert(canonical.clone());

        if let Some(parent) = config_path.parent() {
            // Watch the parent dir non-recursively; we'll filter for rescript.json in the event loop
            let _ = state.watcher.watch(parent, RecursiveMode::NonRecursive);
        }
    }

    // Snapshot mtimes for source files
    state.last_mtime = snapshot_mtimes(&source_dirs_for_snapshot);

    state
}

/// Run watch mode via the daemon
pub async fn run(folder: &str, filter: Option<String>, after_build: Option<String>) -> Result<()> {
    let root = connection::find_project_root(Path::new(folder))?;
    let working_dir = Path::new(folder).canonicalize()?;

    let mut client = connection::connect_or_start(&root).await?;

    // Send watch request to daemon
    let request = WatchRequest {
        working_directory: working_dir.to_string_lossy().to_string(),
        filter,
    };

    let mut build_stream = client.watch(request).await?.into_inner();

    // Clone client for file/config change notifications
    let mut notify_client = connection::connect(&root).await?;

    // Set up signal handlers for graceful shutdown
    #[cfg(unix)]
    let mut sigterm = signal(SignalKind::terminate()).ok();
    #[cfg(unix)]
    let mut sigint = signal(SignalKind::interrupt()).ok();

    // Channel for filesystem events from notify-rs
    let (tx, mut rx) = mpsc::channel::<Event>(100);
    let tx = Arc::new(tx);

    // Watch state is initialized when we receive the first WatchPaths event from the daemon.
    // Until then, no filesystem watching happens.
    let mut watch_state: Option<WatchState> = None;

    // Track client_id for graceful disconnect
    let mut client_id: Option<u64> = None;

    loop {
        tokio::select! {
            // Handle SIGTERM for graceful shutdown
            _ = async {
                if let Some(ref mut s) = sigterm {
                    s.recv().await
                } else {
                    std::future::pending::<Option<()>>().await
                }
            } => {
                // Send disconnect to daemon so it can clean up
                if let Some(id) = client_id {
                    let _ = notify_client.disconnect(DisconnectRequest { client_id: id }).await;
                }
                break;
            }

            // Handle SIGINT (Ctrl-C) for graceful shutdown
            _ = async {
                if let Some(ref mut s) = sigint {
                    s.recv().await
                } else {
                    std::future::pending::<Option<()>>().await
                }
            } => {
                // Send disconnect to daemon so it can clean up
                if let Some(id) = client_id {
                    let _ = notify_client.disconnect(DisconnectRequest { client_id: id }).await;
                }
                break;
            }

            // Handle file system events
            Some(event) = rx.recv() => {
                let Some(ref mut ws) = watch_state else {
                    continue;
                };

                for path in &event.paths {
                    // Check if this is a config file change
                    if is_config_file(path, &ws.config_paths) {
                        // Only forward if the file was actually modified (not a phantom event)
                        if let Ok(mtime) = path.metadata().and_then(|m| m.modified()) {
                            if ws.last_mtime.get(path) == Some(&mtime) {
                                continue;
                            }
                            ws.last_mtime.insert(path.to_path_buf(), mtime);
                        }

                        let _ = notify_client
                            .notify_config_change(ConfigChangeNotification {
                                path: path.to_string_lossy().to_string(),
                            })
                            .await;
                        continue;
                    }

                    // Source file change
                    if is_rescript_file(path) && !is_in_build_path(path) {
                        let change_type = match event.kind {
                            notify::EventKind::Create(_) => {
                                // Atomic writes (e.g. Node.js writeFile) show up as Create
                                // events even though the file already existed. If it's in our
                                // mtime cache, the file was there before — treat as Modified.
                                if ws.last_mtime.contains_key(path) {
                                    FileChangeType::Modified
                                } else {
                                    FileChangeType::Created
                                }
                            }
                            notify::EventKind::Remove(_) => {
                                ws.last_mtime.remove(path);
                                FileChangeType::Deleted
                            }
                            notify::EventKind::Modify(notify::event::ModifyKind::Name(_)) => FileChangeType::Renamed,
                            notify::EventKind::Modify(_) => FileChangeType::Modified,
                            _ => continue,
                        };

                        // For non-delete events, only forward if the mtime actually changed.
                        if change_type != FileChangeType::Deleted
                            && let Ok(mtime) = path.metadata().and_then(|m| m.modified())
                        {
                            if ws.last_mtime.get(path) == Some(&mtime) {
                                continue;
                            }
                            ws.last_mtime.insert(path.to_path_buf(), mtime);
                        }

                        let _ = notify_client
                            .notify_file_change(FileChangeNotification {
                                path: path.to_string_lossy().to_string(),
                                change_type: change_type.into(),
                            })
                            .await;
                    }
                }
            }

            // Handle build output from daemon
            Some(result) = build_stream.next() => {
                match result {
                    Ok(event) => {
                        // Check if this is a WatchPaths event — update our watchers
                        if let Some(DaemonEventVariant::WatchPaths(ref watch_paths)) = event.event {
                            let source_paths: Vec<(PathBuf, bool)> = watch_paths
                                .source_paths
                                .iter()
                                .map(|sp| (PathBuf::from(&sp.path), sp.recursive))
                                .collect();
                            let config_paths: Vec<PathBuf> = watch_paths
                                .config_paths
                                .iter()
                                .map(PathBuf::from)
                                .collect();

                            let is_first_setup = watch_state.is_none();
                            watch_state = Some(setup_watchers(&source_paths, &config_paths, &tx));
                            if is_first_setup {
                                println!("Watching for file changes... (Ctrl-C to exit)\n");
                            }
                            continue;
                        }

                        // Extract client_id from events (BuildStarted is the first event with it)
                        if client_id.is_none()
                            && let Some(DaemonEventVariant::BuildStarted(ref started)) = event.event
                        {
                            client_id = Some(started.client_id);
                        }

                        // Check if this is a BuildFinished with success before rendering
                        let is_success = if let Some(DaemonEventVariant::BuildFinished(ref finished)) = event.event {
                            finished.success
                        } else {
                            false
                        };

                        // Check if this is an initialization error - we should exit
                        let is_init_error = matches!(event.event, Some(DaemonEventVariant::InitializationError(_)));

                        output::render_event(&event, false, false);

                        if is_init_error {
                            return Err(anyhow::anyhow!("Watch failed due to initialization error"));
                        }

                        // Run after_build command on successful completion
                        if is_success
                            && let Some(ref cmd_str) = after_build
                        {
                            cmd::run(cmd_str.clone());
                        }
                    }
                    Err(e) => {
                        eprintln!("Stream error: {}", e);
                        // Send disconnect to daemon so it can clean up
                        if let Some(id) = client_id {
                            let _ = notify_client.disconnect(DisconnectRequest { client_id: id }).await;
                        }
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

/// Check if a path is a config file (rescript.json) that we're watching.
fn is_config_file(path: &Path, config_paths: &HashSet<PathBuf>) -> bool {
    if path.file_name().and_then(|f| f.to_str()) != Some("rescript.json") {
        return false;
    }
    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    config_paths.contains(&canonical)
}

fn is_rescript_file(path: &Path) -> bool {
    let extension = path.extension().and_then(|ext| ext.to_str());
    matches!(extension, Some("res") | Some("resi"))
}

fn is_in_build_path(path: &Path) -> bool {
    let mut prev_component: Option<&std::ffi::OsStr> = None;
    for component in path.components() {
        let comp_os = component.as_os_str();
        if let Some(prev) = prev_component
            && prev == "lib"
            && (comp_os == "bs" || comp_os == "ocaml")
        {
            return true;
        }
        prev_component = Some(comp_os);
    }
    false
}
