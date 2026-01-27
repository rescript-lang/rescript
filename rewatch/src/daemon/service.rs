//! The gRPC service implementation for the daemon.
//!
//! This module contains `DaemonService` and implements the `RescriptDaemon` trait
//! with handlers for build, clean, watch, format, and other RPC methods.

use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Instant;

use tokio::sync::mpsc;
use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::{Stream, StreamExt};
use tonic::{Request, Response, Status};
use tracing::Instrument;

use crate::build;

use super::clients::{ClientCountChange, ClientType};
use super::file_change_handler::process_file_change_notifications;
use super::proto::{
    BuildFinished, BuildRequest, BuildType, CleanRequest, CompilerArgsRequest, CompilerArgsResponse,
    ConfigChangeAck, ConfigChangeNotification, DaemonEvent, DebugRequest, FileChangeAck,
    FileChangeNotification, FileChangeType, FormatCheckFailed, FormatFinished, FormatRequest, FormattedStdin,
    GetClientsRequest, GetClientsResponse, PingRequest, PingResponse, RegisterRequest, RegisterResponse,
    ShutdownRequest, ShutdownResponse, WatchRequest, daemon_event::Event,
    rescript_daemon_server::RescriptDaemon,
};
use super::state::{CompileType, DaemonState, InternalFileChange};

type DaemonEventStream = Pin<Box<dyn Stream<Item = Result<DaemonEvent, Status>> + Send>>;

/// Helper to create a timestamp string for DaemonEvent.
fn timestamp() -> String {
    chrono::Local::now().format("%H:%M:%S%.3f").to_string()
}

/// Helper to create a BuildFinished DaemonEvent for streaming to clients.
/// Note: This is separate from state.emit_build_finished which broadcasts to all clients.
/// This is yielded directly to the requesting client's stream.
fn make_build_finished(
    client_id: u64,
    success: bool,
    duration_seconds: f64,
    module_count: Option<i32>,
) -> DaemonEvent {
    DaemonEvent {
        timestamp: timestamp(),
        event: Some(Event::BuildFinished(BuildFinished {
            client_id,
            success,
            duration_seconds,
            module_count,
            error: None,
            is_clean: false,
        })),
    }
}

/// Helper to create a FormatFinished DaemonEvent for streaming to clients.
fn make_format_finished(
    client_id: u64,
    success: bool,
    formatted_count: i32,
    failed_count: i32,
) -> DaemonEvent {
    DaemonEvent {
        timestamp: timestamp(),
        event: Some(Event::FormatFinished(FormatFinished {
            client_id,
            success,
            duration_seconds: 0.0, // Duration tracked separately
            formatted_count,
            failed_count,
        })),
    }
}

/// Helper to create a FormattedStdin DaemonEvent.
fn make_formatted_stdin(client_id: u64, content: String) -> DaemonEvent {
    DaemonEvent {
        timestamp: timestamp(),
        event: Some(Event::FormattedStdin(FormattedStdin { client_id, content })),
    }
}

/// Helper to create a FormatCheckFailed DaemonEvent.
fn make_format_check_failed(client_id: u64, file: String) -> DaemonEvent {
    DaemonEvent {
        timestamp: timestamp(),
        event: Some(Event::FormatCheckFailed(FormatCheckFailed { client_id, file })),
    }
}

/// Helper to create a CompilerError DaemonEvent.
fn make_compiler_error(client_id: u64, message: String) -> DaemonEvent {
    DaemonEvent {
        timestamp: timestamp(),
        event: Some(Event::CompilerError(super::proto::CompilerError {
            client_id,
            message,
        })),
    }
}

/// Check if an event matches the given client_id.
/// Events without client_id (like FileChanged) match all clients.
fn event_matches_client(event: &DaemonEvent, client_id: u64) -> bool {
    match &event.event {
        Some(Event::Cleaned(e)) => e.client_id == client_id,
        Some(Event::Parsed(e)) => e.client_id == client_id,
        Some(Event::Compiling(e)) => e.client_id == client_id,
        Some(Event::Compiled(e)) => e.client_id == client_id,
        Some(Event::CleanedCompilerAssets(e)) => e.client_id == client_id,
        Some(Event::CleanedJsFiles(e)) => e.client_id == client_id,
        Some(Event::CircularDependency(e)) => e.client_id == client_id,
        Some(Event::UnallowedDependency(e)) => e.client_id == client_id,
        Some(Event::PackageTreeError(e)) => e.client_id == client_id,
        Some(Event::ModuleNotFound(e)) => e.client_id == client_id,
        Some(Event::InitializationError(e)) => e.client_id == client_id,
        Some(Event::CompilerWarning(e)) => e.client_id == client_id,
        Some(Event::CompilerError(e)) => e.client_id == client_id,
        // ConfigWarning is project-wide, broadcast to all clients
        Some(Event::ConfigWarning(_)) => true,
        Some(Event::DuplicatedPackage(e)) => e.client_id == client_id,
        Some(Event::MissingImplementation(e)) => e.client_id == client_id,
        Some(Event::PackageNameMismatch(e)) => e.client_id == client_id,
        Some(Event::BuildStarted(e)) => e.client_id == client_id,
        Some(Event::BuildFinished(e)) => e.client_id == client_id,
        Some(Event::FormatStarted(e)) => e.client_id == client_id,
        Some(Event::FormatFinished(e)) => e.client_id == client_id,
        Some(Event::FormatProgress(e)) => e.client_id == client_id,
        Some(Event::FormatCheckFailed(e)) => e.client_id == client_id,
        Some(Event::FormattedStdin(e)) => e.client_id == client_id,
        Some(Event::JsPostBuildOutput(e)) => e.client_id == client_id,
        // Client lifecycle events match all clients (for debug visibility)
        Some(Event::ClientConnected(_)) => true,
        Some(Event::ClientDisconnected(_)) => true,
        // FileChanged has no client_id - it's a broadcast event
        Some(Event::FileChanged(_)) => true,
        // WatchPaths is targeted at the watch client
        Some(Event::WatchPaths(e)) => e.client_id == client_id,
        None => false,
    }
}

pub struct DaemonService {
    state: Arc<DaemonState>,
}

/// Result of client setup - everything needed to start processing a request.
struct ClientSetup {
    client_id: u64,
    event_rx: tokio::sync::broadcast::Receiver<DaemonEvent>,
}

impl DaemonService {
    /// Common setup for all client requests (build, watch, clean, format).
    /// Registers the client, emits connected event, and subscribes to the broadcast.
    /// Package initialization is handled by the work loop as a precondition.
    async fn setup_client(&self, client_type: ClientType, working_dir: String) -> ClientSetup {
        let client_id = self
            .state
            .clients
            .register(client_type, working_dir.clone())
            .await;

        self.state
            .emit_client_connected(client_id, client_type, working_dir);

        // Subscribe BEFORE submitting work, so we don't miss events
        let event_rx = self.state.clients.subscribe();

        ClientSetup { client_id, event_rx }
    }

    /// Create a new daemon service. Returns the service and a receiver for client count changes.
    /// Returns an error if build state initialization fails - the daemon cannot operate without it.
    pub async fn new(root: PathBuf) -> anyhow::Result<(Self, mpsc::Receiver<ClientCountChange>)> {
        let (file_change_tx, file_change_rx) = mpsc::channel::<InternalFileChange>(100);
        let (work_tx, work_rx) = mpsc::channel::<super::work_queue::WorkItem>(32);
        let (client_count_tx, client_count_rx) = mpsc::channel::<ClientCountChange>(10);

        // Initialize minimal build state - just project context and compiler info.
        // Package discovery is deferred until the first client request, so that:
        // 1. Config warnings are emitted to the client's reporter (not lost)
        // 2. Working directory from the request can be considered for scoping
        let root_for_init = root.clone();
        let init_result = tokio::task::spawn_blocking(move || {
            let reporter = build::NoopReporter;
            let project_context = crate::project_context::ProjectContext::new(&root_for_init)?;
            let compiler_info = build::get_compiler_info(&project_context, &reporter)?;
            Ok::<_, anyhow::Error>(build::build_types::BuildState::empty(
                project_context,
                compiler_info,
            ))
        })
        .await;

        let build_state = match init_result {
            Ok(Ok(state)) => state,
            Ok(Err(e)) => return Err(anyhow::anyhow!("Failed to initialize build state: {:#}", e)),
            Err(e) => return Err(anyhow::anyhow!("Build state init panicked: {}", e)),
        };

        let state = Arc::new(DaemonState::new(
            root.clone(),
            file_change_tx,
            work_tx,
            client_count_tx,
            build_state,
        ));

        tracing::info!(pid = std::process::id(), "Daemon started");

        // Start the serialized work loop — all state-mutating operations go through here
        // Use .instrument() to propagate the current span context into the spawned task
        let state_for_work_loop = state.clone();
        tokio::spawn(
            async move {
                super::work_queue::run_work_loop(state_for_work_loop, work_rx).await;
            }
            .instrument(tracing::Span::current()),
        );

        // Start background task to process file change notifications from watch clients.
        // This debounces changes and submits WorkItems to the work queue.
        let state_for_file_changes = state.clone();
        tokio::spawn(
            async move {
                process_file_change_notifications(state_for_file_changes, file_change_rx).await;
            }
            .instrument(tracing::Span::current()),
        );

        Ok((Self { state }, client_count_rx))
    }
}

#[tonic::async_trait]
impl RescriptDaemon for DaemonService {
    type BuildStream = DaemonEventStream;
    type CleanStream = DaemonEventStream;
    type WatchStream = DaemonEventStream;
    type DebugStream = DaemonEventStream;
    type FormatStream = DaemonEventStream;

    async fn register(
        &self,
        request: Request<RegisterRequest>,
    ) -> Result<Response<RegisterResponse>, Status> {
        let req = request.into_inner();

        let client_type = match super::proto::ClientType::try_from(req.client_type) {
            Ok(super::proto::ClientType::ClientBuild) => ClientType::Build,
            Ok(super::proto::ClientType::ClientWatch) => ClientType::Watch,
            Ok(super::proto::ClientType::ClientClean) => ClientType::Clean,
            Ok(super::proto::ClientType::ClientDebug) => ClientType::Debug,
            Ok(super::proto::ClientType::ClientFormat) => ClientType::Format,
            Err(_) => return Err(Status::invalid_argument("Invalid client type")),
        };

        let client_id = self
            .state
            .clients
            .register(client_type, req.working_directory.clone())
            .await;

        self.state
            .emit_client_connected(client_id, client_type, req.working_directory);

        Ok(Response::new(RegisterResponse { client_id }))
    }

    async fn build(&self, request: Request<BuildRequest>) -> Result<Response<Self::BuildStream>, Status> {
        let req = request.into_inner();
        let working_dir = req.working_directory.clone();

        let ClientSetup { client_id, event_rx } =
            self.setup_client(ClientType::Build, working_dir.clone()).await;

        // Create span for this build RPC.
        let filter_str = req.filter.as_deref().unwrap_or("");
        let warn_error_str = req.warn_error.as_deref().unwrap_or("");
        let rpc_span = tracing::info_span!(
            "rpc.build",
            client_id = client_id,
            working_dir = %working_dir,
            filter = filter_str,
            warn_error = warn_error_str,
        );

        let state = self.state.clone();

        let output = async_stream::stream! {
            state.emit_build_started(client_id, BuildType::BuildFull, None, false);

            // Submit work item with oneshot for completion
            let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
            let filter = req.filter.as_ref().and_then(|f| regex::Regex::new(f).ok());
            let work_item = super::work_queue::WorkItem::Build {
                client_id,
                working_directory: req.working_directory.clone(),
                warn_error: req.warn_error.clone(),
                filter,
                completion_tx,
                parent_span: rpc_span,
            };
            let _ = state.work_tx.send(work_item).await;

            // Stream broadcast events filtered by client_id until work completes
            let mut event_stream = BroadcastStream::new(event_rx);
            let mut completion_rx = completion_rx;
            let work_result = loop {
                tokio::select! {
                    biased;
                    result = &mut completion_rx => {
                        break result.ok();
                    }
                    event = event_stream.next() => {
                        match event {
                            Some(Ok(ev)) => {
                                if event_matches_client(&ev, client_id) {
                                    yield Ok(ev);
                                }
                            }
                            Some(Err(_)) => continue,
                            None => { break None; }
                        }
                    }
                }
            };

            // Yield final BuildFinished to the requesting client's stream
            match work_result {
                Some(super::work_queue::WorkResult::Success { duration_seconds, module_count }) => {
                    yield Ok(make_build_finished(client_id, true, duration_seconds, module_count.map(|n| n as i32)));
                }
                Some(super::work_queue::WorkResult::Failure { duration_seconds, .. }) => {
                    yield Ok(make_build_finished(client_id, false, duration_seconds, None));
                }
                None => {
                    yield Ok(make_build_finished(client_id, false, 0.0, None));
                }
            }

            state.clients.unregister(client_id).await;
            state.emit_client_disconnected(client_id, ClientType::Build);
        };

        Ok(Response::new(Box::pin(output)))
    }

    async fn clean(&self, request: Request<CleanRequest>) -> Result<Response<Self::CleanStream>, Status> {
        let req = request.into_inner();
        let working_dir = req.working_directory.clone();

        let ClientSetup { client_id, event_rx } =
            self.setup_client(ClientType::Clean, working_dir.clone()).await;

        // Create span for this clean RPC.
        let rpc_span = tracing::info_span!(
            "rpc.clean",
            client_id = client_id,
            working_dir = %working_dir,
        );

        let state = self.state.clone();

        let output = async_stream::stream! {
            // Submit work item with oneshot for completion
            let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
            let work_item = super::work_queue::WorkItem::Clean {
                client_id,
                working_directory: working_dir,
                completion_tx,
                parent_span: rpc_span,
            };
            let _ = state.work_tx.send(work_item).await;

            // Stream broadcast events filtered by client_id until work completes
            let mut event_stream = BroadcastStream::new(event_rx);
            let mut completion_rx = completion_rx;
            let work_result = loop {
                tokio::select! {
                    biased;
                    result = &mut completion_rx => {
                        break result.ok();
                    }
                    event = event_stream.next() => {
                        match event {
                            Some(Ok(ev)) => {
                                if event_matches_client(&ev, client_id) {
                                    yield Ok(ev);
                                }
                            }
                            Some(Err(_)) => continue,
                            None => { break None; }
                        }
                    }
                }
            };

            // Yield final BuildFinished to the requesting client's stream
            match work_result {
                Some(super::work_queue::WorkResult::Success { duration_seconds, .. }) => {
                    yield Ok(make_build_finished(client_id, true, duration_seconds, None));
                }
                Some(super::work_queue::WorkResult::Failure { duration_seconds, .. }) => {
                    yield Ok(make_build_finished(client_id, false, duration_seconds, None));
                }
                None => {
                    yield Ok(make_build_finished(client_id, false, 0.0, None));
                }
            }

            state.clients.unregister(client_id).await;
            state.emit_client_disconnected(client_id, ClientType::Clean);
        };

        Ok(Response::new(Box::pin(output)))
    }

    async fn watch(&self, request: Request<WatchRequest>) -> Result<Response<Self::WatchStream>, Status> {
        let req = request.into_inner();
        let working_dir = req.working_directory.clone();

        // Check if another watch client is already connected
        if self.state.clients.has_watch_client().await {
            return Err(Status::already_exists(
                "Another watch client is already connected",
            ));
        }

        let ClientSetup { client_id, event_rx } =
            self.setup_client(ClientType::Watch, working_dir.clone()).await;

        // Create span for the watch session.
        let watch_span = tracing::info_span!(
            "rpc.watch",
            client_id = client_id,
            working_dir = %working_dir,
        );

        // Store a clone in state for file change builds to use as parent
        self.state.set_watch_span(watch_span.clone());

        let state = self.state.clone();

        let output = async_stream::stream! {
            state.emit_build_started(client_id, BuildType::BuildFull, None, true);

            // Submit initial build via work queue
            let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
            let work_item = super::work_queue::WorkItem::WatchInitialBuild {
                client_id,
                completion_tx,
                parent_span: watch_span,
            };
            let _ = state.work_tx.send(work_item).await;

            // Stream broadcast events until initial build completes
            let mut event_stream = BroadcastStream::new(event_rx);
            let mut completion_rx = completion_rx;
            let work_result = loop {
                tokio::select! {
                    biased;
                    result = &mut completion_rx => {
                        break result.ok();
                    }
                    event = event_stream.next() => {
                        match event {
                            Some(Ok(ev)) => {
                                if event_matches_client(&ev, client_id) {
                                    yield Ok(ev);
                                }
                            }
                            Some(Err(_)) => continue,
                            None => { break None; }
                        }
                    }
                }
            };

            // Yield BuildFinished for the initial build
            match work_result {
                Some(super::work_queue::WorkResult::Success { duration_seconds, module_count }) => {
                    yield Ok(make_build_finished(client_id, true, duration_seconds, module_count.map(|n| n as i32)));
                }
                Some(super::work_queue::WorkResult::Failure { duration_seconds, .. }) => {
                    yield Ok(make_build_finished(client_id, false, duration_seconds, None));
                }
                None => {
                    yield Ok(make_build_finished(client_id, false, 0.0, None));
                }
            }

            // Continue streaming events from file changes indefinitely.
            // Use scopeguard to ensure disconnect is emitted even if the client
            // drops the connection (which causes the stream to end).
            let state_for_cleanup = state.clone();
            let _cleanup = scopeguard::guard((), move |_| {
                let state = state_for_cleanup;
                // Clear the watch span since the watch client is disconnecting
                state.clear_watch_span();
                tokio::spawn(async move {
                    state.clients.unregister(client_id).await;
                    state.emit_client_disconnected(client_id, ClientType::Watch);
                });
            });

            while let Some(result) = event_stream.next().await {
                match result {
                    Ok(event) => {
                        if event_matches_client(&event, client_id) {
                            yield Ok(event);
                        }
                    }
                    Err(_) => continue,
                }
            }
        };

        Ok(Response::new(Box::pin(output)))
    }

    async fn notify_file_change(
        &self,
        request: Request<FileChangeNotification>,
    ) -> Result<Response<FileChangeAck>, Status> {
        let req = request.into_inner();

        let change_type = FileChangeType::try_from(req.change_type).unwrap_or(FileChangeType::Modified);
        let compile_type = match change_type {
            FileChangeType::Modified => CompileType::Incremental,
            FileChangeType::Created => CompileType::SourceCreated,
            FileChangeType::Deleted => CompileType::SourceDeleted,
            FileChangeType::Renamed => CompileType::SourceRenamed,
        };

        self.state.emit_file_changed(req.path.clone(), change_type);

        let _ = self
            .state
            .file_change_tx
            .send(InternalFileChange {
                path: req.path,
                compile_type,
            })
            .await;

        Ok(Response::new(FileChangeAck { accepted: true }))
    }

    async fn notify_config_change(
        &self,
        request: Request<ConfigChangeNotification>,
    ) -> Result<Response<ConfigChangeAck>, Status> {
        let req = request.into_inner();

        tracing::debug!(path = %req.path, "Config change notification received");

        // Submit config change to work queue (re-reads config and invalidates state),
        // then trigger a full rebuild via the file change channel.
        let config_path = PathBuf::from(&req.path);
        let work_item = super::work_queue::WorkItem::ConfigChange { config_path };
        let _ = self.state.work_tx.send(work_item).await;

        // Trigger rebuild via the file change handler (which debounces and submits FileChangeBuild)
        let _ = self
            .state
            .file_change_tx
            .send(InternalFileChange {
                path: req.path,
                compile_type: CompileType::ConfigChange,
            })
            .await;

        Ok(Response::new(ConfigChangeAck { accepted: true }))
    }

    async fn debug(&self, _request: Request<DebugRequest>) -> Result<Response<Self::DebugStream>, Status> {
        let client_id = self
            .state
            .clients
            .register(ClientType::Debug, "debug".to_string())
            .await;

        self.state
            .emit_client_connected(client_id, ClientType::Debug, "debug".to_string());

        let rx = self.state.clients.subscribe();
        let state = self.state.clone();

        let output = async_stream::stream! {
            // Use scopeguard to ensure cleanup happens when the stream is dropped
            let state_for_cleanup = state.clone();
            let _cleanup = scopeguard::guard((), move |_| {
                let state = state_for_cleanup;
                tokio::spawn(async move {
                    state.clients.unregister(client_id).await;
                    state.emit_client_disconnected(client_id, ClientType::Debug);
                });
            });

            let mut event_stream = BroadcastStream::new(rx);
            while let Some(result) = event_stream.next().await {
                match result {
                    Ok(event) => yield Ok(event),
                    Err(_) => continue,
                }
            }
        };

        Ok(Response::new(Box::pin(output)))
    }

    async fn get_clients(
        &self,
        _request: Request<GetClientsRequest>,
    ) -> Result<Response<GetClientsResponse>, Status> {
        let clients = self.state.clients.get_all_clients().await;
        let _span = tracing::info_span!("rpc.get_clients", client_count = clients.len()).entered();

        let proto_clients: Vec<super::proto::ConnectedClient> = clients
            .into_iter()
            .map(|(id, info)| {
                let client_type = match info.client_type {
                    ClientType::Build => super::proto::ClientType::ClientBuild,
                    ClientType::Watch => super::proto::ClientType::ClientWatch,
                    ClientType::Clean => super::proto::ClientType::ClientClean,
                    ClientType::Debug => super::proto::ClientType::ClientDebug,
                    ClientType::Format => super::proto::ClientType::ClientFormat,
                };
                super::proto::ConnectedClient {
                    id,
                    client_type: client_type.into(),
                    working_directory: info.working_directory,
                    connected_at: info.connected_at,
                }
            })
            .collect();

        Ok(Response::new(GetClientsResponse {
            clients: proto_clients,
        }))
    }

    async fn ping(&self, _request: Request<PingRequest>) -> Result<Response<PingResponse>, Status> {
        Ok(Response::new(PingResponse { ready: true }))
    }

    async fn shutdown(
        &self,
        _request: Request<ShutdownRequest>,
    ) -> Result<Response<ShutdownResponse>, Status> {
        tracing::info!("Shutdown request received");
        Ok(Response::new(ShutdownResponse {}))
    }

    async fn disconnect(
        &self,
        request: Request<super::proto::DisconnectRequest>,
    ) -> Result<Response<super::proto::DisconnectResponse>, Status> {
        let req = request.into_inner();
        let client_id = req.client_id;

        // Look up the client type before unregistering
        let client_info = self.state.clients.get_all_clients().await;
        let client_type = client_info
            .iter()
            .find(|(id, _)| *id == client_id)
            .map(|(_, info)| info.client_type);

        // Unregister the client
        self.state.clients.unregister(client_id).await;

        // Emit disconnect event if we found the client
        if let Some(ct) = client_type {
            self.state.emit_client_disconnected(client_id, ct);
        }

        Ok(Response::new(super::proto::DisconnectResponse {}))
    }

    async fn format(&self, request: Request<FormatRequest>) -> Result<Response<Self::FormatStream>, Status> {
        let req = request.into_inner();
        let working_dir_str = req.working_directory.clone();
        let working_dir = PathBuf::from(&req.working_directory);
        let check = req.check;
        let requested_files = req.files;
        let stdin_ext = req.stdin_ext.clone();
        let stdin_content = req.stdin_content.clone();
        let is_stdin = stdin_ext.is_some();

        let rpc_span = tracing::info_span!(
            "rpc.format",
            working_dir = %working_dir.display(),
            check = %check,
            is_stdin = %is_stdin,
        );

        let ClientSetup { client_id, .. } = self.setup_client(ClientType::Format, working_dir_str).await;

        let state = self.state.clone();

        let output = async_stream::stream! {
            let _rpc_guard = rpc_span.enter();
            let start = Instant::now();

            // Handle stdin formatting (no state access needed)
            if let Some(ext) = stdin_ext {
                state.emit_format_started(client_id, check, true, 0);
                let content = stdin_content.unwrap_or_default();
                let stdin_span = tracing::info_span!(
                    parent: &rpc_span,
                    "format.stdin",
                    ext = %ext,
                );
                let result = tokio::task::spawn_blocking(move || -> Result<String, String> {
                    stdin_span.in_scope(|| {
                        build::format::format_stdin(&content, &ext)
                    })
                })
                .await;

                let duration = start.elapsed();

                match result {
                    Ok(Ok(formatted)) => {
                        state.emit_format_finished(client_id, true, duration.as_secs_f64(), 1, 0);
                        yield Ok(make_formatted_stdin(client_id, formatted));
                        yield Ok(make_format_finished(client_id, true, 1, 0));
                    }
                    Ok(Err(e)) => {
                        state.emit_format_finished(client_id, false, duration.as_secs_f64(), 0, 1);
                        yield Ok(make_compiler_error(client_id, e));
                        yield Ok(make_format_finished(client_id, false, 0, 1));
                    }
                    Err(e) => {
                        state.emit_format_finished(client_id, false, duration.as_secs_f64(), 0, 1);
                        yield Ok(make_compiler_error(client_id, format!("Task panicked: {}", e)));
                        yield Ok(make_format_finished(client_id, false, 0, 1));
                    }
                }

                state.clients.unregister(client_id).await;
                state.emit_client_disconnected(client_id, ClientType::Format);
                return;
            }

            // Get files to format: either from the request, or via the work queue
            let files = if !requested_files.is_empty() {
                requested_files
            } else {
                // Submit to work queue to collect files (ensures packages are loaded)
                let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                let work_item = super::work_queue::WorkItem::FormatCollectFiles {
                    client_id,
                    working_directory: working_dir,
                    completion_tx,
                    parent_span: rpc_span.clone(),
                };
                let _ = state.work_tx.send(work_item).await;
                completion_rx.await.unwrap_or_default()
            };

            // Emit format started now that we know the file count
            state.emit_format_started(client_id, check, false, files.len() as i32);

            // Format files
            let (tx, mut rx) = mpsc::channel::<DaemonEvent>(100);

            let file_count = files.len();
            let format_span = tracing::info_span!(
                parent: &rpc_span,
                "format.execute",
                file_count = file_count,
                check = %check,
            );
            let format_span_clone = format_span.clone();

            let format_result = tokio::task::spawn_blocking(move || {
                format_span_clone.in_scope(|| {
                    build::format::format_files(&files, check, &|file| {
                        let _ = tx.blocking_send(make_format_check_failed(
                            client_id,
                            file.to_string(),
                        ));
                    })
                })
            });

            // Yield progress updates from the channel
            loop {
                tokio::select! {
                    msg = rx.recv() => {
                        match msg {
                            Some(event) => yield Ok(event),
                            None => break,
                        }
                    }
                    _ = tokio::time::sleep(std::time::Duration::from_millis(50)) => {
                        if format_result.is_finished() {
                            // Drain remaining messages
                            while let Ok(event) = rx.try_recv() {
                                yield Ok(event);
                            }
                            break;
                        }
                    }
                }
            }

            // Get the final result
            let duration = start.elapsed();

            match format_result.await {
                Ok((formatted, failed, None)) => {
                    let success = failed == 0;
                    state.emit_format_finished(client_id, success, duration.as_secs_f64(), formatted as i32, failed as i32);
                    if !success {
                        if failed == 1 {
                            yield Ok(make_compiler_error(client_id, "The file listed above needs formatting".to_string()));
                        } else {
                            yield Ok(make_compiler_error(client_id, format!("The {} files listed above need formatting", failed)));
                        }
                    }
                    yield Ok(make_format_finished(client_id, success, formatted as i32, failed as i32));
                }
                Ok((formatted, failed, Some(e))) => {
                    state.emit_format_finished(client_id, false, duration.as_secs_f64(), formatted as i32, failed as i32);
                    yield Ok(make_compiler_error(client_id, e));
                    yield Ok(make_format_finished(client_id, false, formatted as i32, failed as i32));
                }
                Err(e) => {
                    state.emit_format_finished(client_id, false, duration.as_secs_f64(), 0, 0);
                    yield Ok(make_compiler_error(client_id, format!("Task panicked: {}", e)));
                    yield Ok(make_format_finished(client_id, false, 0, 0));
                }
            }

            state.clients.unregister(client_id).await;
            state.emit_client_disconnected(client_id, ClientType::Format);
        };

        Ok(Response::new(Box::pin(output)))
    }

    async fn get_compiler_args(
        &self,
        request: Request<CompilerArgsRequest>,
    ) -> Result<Response<CompilerArgsResponse>, Status> {
        let req = request.into_inner();
        let file_path = PathBuf::from(&req.file_path);

        let span = tracing::info_span!(
            "rpc.get_compiler_args",
            file_path = %file_path.display(),
        );
        let _guard = span.enter();

        let start = Instant::now();

        // Read-only: bypasses work queue, locks mutex directly
        let build_state_guard = self.state.build_state.lock().unwrap_or_else(|e| e.into_inner());
        let build_state = &*build_state_guard;

        // NoopReporter is fine here since compiler-args is a quick query that
        // just returns JSON - no progress reporting needed.
        let noop_reporter = build::NoopReporter;
        let result =
            build::get_compiler_args_with_context(&file_path, &build_state.project_context, &noop_reporter);

        let duration = start.elapsed();

        match result {
            Ok(args) => {
                tracing::info!(
                    duration_ms = duration.as_secs_f64() * 1000.0,
                    "compiler_args completed"
                );
                Ok(Response::new(CompilerArgsResponse {
                    compiler_args: args.compiler_args,
                    parser_args: args.parser_args,
                }))
            }
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "compiler_args failed"
                );
                Err(Status::internal(format!("{:#}", e)))
            }
        }
    }
}
