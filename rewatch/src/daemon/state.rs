//! Shared state for the daemon service.
//!
//! Contains the `DaemonState` struct which holds the project root, client manager,
//! build state, and provides methods to emit daemon events.

use std::path::PathBuf;
use std::sync::Mutex;

use chrono::Local;
use tokio::sync::mpsc;

use crate::build::build_types::BuildState;

use super::clients::{ClientCountChange, ClientId, ClientManager, ClientType};
use super::proto::{
    BuildFinished, BuildStarted, BuildType, ClientConnected, ClientDisconnected, DaemonEvent, FileChangeType,
    FileChanged, FormatFinished, FormatStarted, WatchPaths, WatchSourcePath as ProtoWatchSourcePath,
    daemon_event::Event,
};

/// Describes what happened to a source file, determining how the daemon should rebuild.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileType {
    /// A source file was modified in place. Mark it dirty, incremental build.
    Incremental,
    /// A new source file was created. Sources must be re-scanned from disk.
    SourceCreated,
    /// A source file was deleted. Sources must be re-scanned from disk.
    SourceDeleted,
    /// A source file was renamed. Sources must be re-scanned from disk.
    SourceRenamed,
    /// Config changed — all modules must be re-parsed and re-compiled,
    /// ignoring on-disk timestamps since artifacts are stale.
    ConfigChange,
}

impl CompileType {
    /// Whether this change type requires re-scanning source directories from disk.
    pub fn needs_source_rescan(self) -> bool {
        matches!(
            self,
            Self::SourceCreated | Self::SourceDeleted | Self::SourceRenamed
        )
    }
}

/// Internal file change event for the build queue.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalFileChange {
    pub path: String,
    pub compile_type: CompileType,
}

/// Shared state for the daemon service.
pub struct DaemonState {
    pub root: PathBuf,
    /// Manages connected clients.
    pub clients: ClientManager,
    /// Channel to send file change events to the file change handler for debouncing.
    pub file_change_tx: mpsc::Sender<InternalFileChange>,
    /// Channel to submit work items to the serialized work loop.
    /// All state-mutating operations go through this queue.
    pub work_tx: mpsc::Sender<super::work_queue::WorkItem>,
    /// The build state, owned by the daemon.
    /// Only the work loop should lock this mutex.
    /// Uses std::sync::Mutex so it can be locked from blocking tasks.
    pub build_state: Mutex<BuildState>,
    /// The watch session span, set when a watch client connects.
    /// File change builds are children of this span.
    watch_span: Mutex<Option<tracing::Span>>,
}

impl DaemonState {
    pub fn new(
        root: PathBuf,
        file_change_tx: mpsc::Sender<InternalFileChange>,
        work_tx: mpsc::Sender<super::work_queue::WorkItem>,
        client_count_tx: mpsc::Sender<ClientCountChange>,
        initial_build_state: BuildState,
    ) -> Self {
        Self {
            root,
            clients: ClientManager::new(client_count_tx),
            file_change_tx,
            work_tx,
            build_state: Mutex::new(initial_build_state),
            watch_span: Mutex::new(None),
        }
    }

    /// Set the watch session span (called when watch client connects).
    pub fn set_watch_span(&self, span: tracing::Span) {
        let mut guard = self.watch_span.lock().unwrap_or_else(|e| e.into_inner());
        *guard = Some(span);
    }

    /// Clear the watch session span (called when watch client disconnects).
    pub fn clear_watch_span(&self) {
        let mut guard = self.watch_span.lock().unwrap_or_else(|e| e.into_inner());
        *guard = None;
    }

    /// Get the current watch span for file change builds.
    /// Returns None span if no watch client is connected.
    pub fn get_watch_span(&self) -> tracing::Span {
        let guard = self.watch_span.lock().unwrap_or_else(|e| e.into_inner());
        guard.clone().unwrap_or_else(tracing::Span::none)
    }

    /// Helper to create a timestamp string.
    fn timestamp(&self) -> String {
        Local::now().format("%H:%M:%S%.3f").to_string()
    }

    /// Helper to create a DaemonEvent with timestamp.
    fn make_event(&self, event: Event) -> DaemonEvent {
        DaemonEvent {
            timestamp: self.timestamp(),
            event: Some(event),
        }
    }

    /// Emit a daemon event to all clients.
    pub fn emit(&self, event: Event) {
        self.clients.send(self.make_event(event));
    }

    /// Emit client connected event.
    pub fn emit_client_connected(
        &self,
        client_id: ClientId,
        client_type: ClientType,
        working_directory: String,
    ) {
        let proto_type = match client_type {
            ClientType::Build => super::proto::ClientType::ClientBuild,
            ClientType::Watch => super::proto::ClientType::ClientWatch,
            ClientType::Clean => super::proto::ClientType::ClientClean,
            ClientType::Debug => super::proto::ClientType::ClientDebug,
            ClientType::Format => super::proto::ClientType::ClientFormat,
        };
        self.emit(Event::ClientConnected(ClientConnected {
            client_id,
            client_type: proto_type.into(),
            working_directory,
        }));
    }

    /// Emit client disconnected event.
    pub fn emit_client_disconnected(&self, client_id: ClientId, client_type: ClientType) {
        let proto_type = match client_type {
            ClientType::Build => super::proto::ClientType::ClientBuild,
            ClientType::Watch => super::proto::ClientType::ClientWatch,
            ClientType::Clean => super::proto::ClientType::ClientClean,
            ClientType::Debug => super::proto::ClientType::ClientDebug,
            ClientType::Format => super::proto::ClientType::ClientFormat,
        };
        self.emit(Event::ClientDisconnected(ClientDisconnected {
            client_id,
            client_type: proto_type.into(),
        }));
    }

    /// Emit build started event.
    pub fn emit_build_started(
        &self,
        client_id: ClientId,
        build_type: BuildType,
        file_change_count: Option<i32>,
        is_initial: bool,
    ) {
        self.emit(Event::BuildStarted(BuildStarted {
            client_id,
            build_type: build_type.into(),
            file_change_count,
            is_initial,
        }));
    }

    /// Emit build finished event.
    pub fn emit_build_finished(
        &self,
        client_id: ClientId,
        success: bool,
        duration_seconds: f64,
        module_count: Option<i32>,
        error: Option<String>,
        is_clean: bool,
    ) {
        self.emit(Event::BuildFinished(BuildFinished {
            client_id,
            success,
            duration_seconds,
            module_count,
            error,
            is_clean,
        }));
    }

    /// Emit file changed event.
    pub fn emit_file_changed(&self, path: String, change_type: FileChangeType) {
        self.emit(Event::FileChanged(FileChanged {
            path,
            change_type: change_type.into(),
        }));
    }

    /// Emit format started event.
    pub fn emit_format_started(&self, client_id: ClientId, is_check: bool, is_stdin: bool, file_count: i32) {
        self.emit(Event::FormatStarted(FormatStarted {
            client_id,
            is_check,
            is_stdin,
            file_count,
        }));
    }

    /// Emit format finished event.
    pub fn emit_format_finished(
        &self,
        client_id: ClientId,
        success: bool,
        duration_seconds: f64,
        formatted_count: i32,
        failed_count: i32,
    ) {
        self.emit(Event::FormatFinished(FormatFinished {
            client_id,
            success,
            duration_seconds,
            formatted_count,
            failed_count,
        }));
    }

    /// Emit watch paths event, telling the watch client which directories to monitor.
    /// Derives source and config paths from the current build state.
    pub fn emit_watch_paths(&self, client_id: ClientId) {
        let build_state = self.build_state.lock().unwrap();
        let (source_paths, config_paths) = crate::build::packages::get_watch_paths(&build_state);

        tracing::debug!(
            source_paths_count = source_paths.len(),
            config_paths_count = config_paths.len(),
            "Emitting WatchPaths"
        );

        self.emit(Event::WatchPaths(WatchPaths {
            client_id,
            source_paths: source_paths
                .into_iter()
                .map(|sp| ProtoWatchSourcePath {
                    path: sp.path.to_string_lossy().to_string(),
                    recursive: sp.recursive,
                })
                .collect(),
            config_paths: config_paths
                .into_iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect(),
        }));
    }
}
