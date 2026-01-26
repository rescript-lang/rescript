mod clients;
mod file_change_handler;
mod reporters;
mod service;
mod state;
pub mod telemetry;
mod work_queue;

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use anyhow::Result;
use tokio::net::UnixListener;
use tokio::sync::mpsc;
use tonic::transport::Server;

#[cfg(unix)]
use tokio::signal::unix::{SignalKind, signal};

use crate::daemon::clients::ClientCountChange;
use crate::daemon::service::DaemonService;

pub mod proto {
    tonic::include_proto!("rescript.daemon");
}

/// Returns the path to the daemon socket for a given project root.
/// Uses a hash-based path in the system temp directory to avoid Unix socket
/// path length limits (104 bytes on macOS, 108 on Linux).
/// The hash is deterministic, so all clients for the same project root
/// will compute the same socket path and connect to the same daemon.
pub fn socket_path(root: &Path) -> PathBuf {
    let mut hasher = DefaultHasher::new();
    root.hash(&mut hasher);
    let hash = hasher.finish();
    std::env::temp_dir().join(format!("rescript-{:016x}.sock", hash))
}

/// Returns the path to the socket path file for a given project root.
/// This file contains the actual socket path, allowing clients to find
/// the daemon socket without needing to compute the same hash.
pub fn socket_path_file(root: &Path) -> PathBuf {
    root.join("lib").join("bs").join("rescript.sock.path")
}

/// Returns the path to the daemon PID file for a given project root.
pub fn pid_path(root: &Path) -> PathBuf {
    root.join("lib").join("bs").join("rescript.pid")
}

/// Returns the path to the daemon error file for a given project root.
/// This file is written when the daemon fails to start, so the client can read the error.
pub fn error_path(root: &Path) -> PathBuf {
    root.join("lib").join("bs").join("rescript.daemon.error")
}

/// Start the daemon server, listening on the Unix socket at the given root.
pub async fn start(root: PathBuf) -> Result<()> {
    // Initialize OpenTelemetry tracing FIRST (opt-in via OTEL_EXPORTER_OTLP_ENDPOINT)
    // This must happen before creating any spans so the subscriber is ready.
    let _telemetry_guard = telemetry::init();

    let socket = socket_path(&root);
    let error_file = error_path(&root);
    let socket_path_file = socket_path_file(&root);

    // Ensure lib/bs directory exists (for PID file, socket path file, etc.)
    let lib_bs = root.join("lib").join("bs");
    tokio::fs::create_dir_all(&lib_bs).await?;

    // Remove stale error file if it exists
    let _ = tokio::fs::remove_file(&error_file).await;

    // Remove stale socket if it exists
    if socket.exists() {
        tokio::fs::remove_file(&socket).await?;
    }

    // Write PID file
    let pid = std::process::id();
    let pid_file = pid_path(&root);
    tokio::fs::write(&pid_file, pid.to_string()).await?;

    // Write socket path file so clients can find us
    tokio::fs::write(&socket_path_file, socket.to_string_lossy().as_bytes()).await?;

    log::info!("Starting daemon for project root: {}", root.display());
    log::info!("Listening on socket: {}", socket.display());
    log::info!("PID: {}", pid);

    tracing::info!(
        socket = %socket.display(),
        pid = pid,
        "Daemon started"
    );

    let uds = UnixListener::bind(&socket)?;
    let uds_stream = tokio_stream::wrappers::UnixListenerStream::new(uds);

    let service_result = DaemonService::new(root.clone()).await;
    let (service, shutdown_rx) = match service_result {
        Ok(result) => result,
        Err(e) => {
            // Write the error to the error file so the client can read it
            let error_msg = format!("{:#}", e);
            let _ = tokio::fs::write(&error_file, &error_msg).await;
            tracing::error!(error = %e, "Daemon initialization failed");
            return Err(e);
        }
    };

    Server::builder()
        .add_service(proto::rescript_daemon_server::RescriptDaemonServer::new(service))
        .serve_with_incoming_shutdown(uds_stream, shutdown_monitor(shutdown_rx))
        .await?;

    // Clean up socket, PID file, and socket path file on exit
    let _ = tokio::fs::remove_file(&socket).await;
    let _ = tokio::fs::remove_file(&pid_file).await;
    let _ = tokio::fs::remove_file(&socket_path_file).await;

    tracing::info!("Daemon shut down cleanly");
    log::info!("Daemon shut down cleanly");

    Ok(())
}

/// Monitor signals and client count, shutting down on SIGTERM/SIGINT or when all clients disconnect.
/// SIGHUP is explicitly ignored to prevent daemon death when terminal closes.
async fn shutdown_monitor(mut client_count_rx: mpsc::Receiver<ClientCountChange>) {
    // Set up signal handlers
    let mut sigterm = match signal(SignalKind::terminate()) {
        Ok(s) => s,
        Err(e) => {
            tracing::warn!("Failed to register SIGTERM handler: {}", e);
            // Continue without SIGTERM handling - fall back to client disconnect only
            return client_disconnect_only_monitor(client_count_rx).await;
        }
    };

    let mut sigint = match signal(SignalKind::interrupt()) {
        Ok(s) => s,
        Err(e) => {
            tracing::warn!("Failed to register SIGINT handler: {}", e);
            return client_disconnect_only_monitor(client_count_rx).await;
        }
    };

    // SIGHUP handler - we'll receive it but ignore it
    let mut sighup = match signal(SignalKind::hangup()) {
        Ok(s) => s,
        Err(e) => {
            tracing::warn!("Failed to register SIGHUP handler: {}", e);
            // Continue without SIGHUP handling - not critical
            return signal_monitor_without_sighup(client_count_rx, sigterm, sigint).await;
        }
    };

    loop {
        tokio::select! {
            // SIGTERM - graceful shutdown
            _ = sigterm.recv() => {
                tracing::info!("Received SIGTERM, shutting down gracefully");
                return;
            }

            // SIGINT (Ctrl-C) - graceful shutdown
            _ = sigint.recv() => {
                tracing::info!("Received SIGINT, shutting down gracefully");
                return;
            }

            // SIGHUP - ignore (daemon should survive terminal close)
            _ = sighup.recv() => {
                tracing::info!("Received SIGHUP, ignoring");
                continue;
            }

            // Client count changes
            msg = client_count_rx.recv() => {
                match msg {
                    None => {
                        tracing::info!("Client count channel closed");
                        return;
                    }
                    Some(ClientCountChange::Connected) => {
                        continue;
                    }
                    Some(ClientCountChange::AllDisconnected) => {
                        tracing::info!("No clients connected, shutting down");
                        return;
                    }
                }
            }
        }
    }
}

/// Fallback monitor when signal handlers couldn't be registered.
/// Only monitors client disconnect.
async fn client_disconnect_only_monitor(mut client_count_rx: mpsc::Receiver<ClientCountChange>) {
    loop {
        match client_count_rx.recv().await {
            None => {
                tracing::info!("Client count channel closed");
                return;
            }
            Some(ClientCountChange::Connected) => {
                continue;
            }
            Some(ClientCountChange::AllDisconnected) => {
                tracing::info!("No clients connected, shutting down");
                return;
            }
        }
    }
}

/// Monitor without SIGHUP handling (fallback if SIGHUP registration fails).
async fn signal_monitor_without_sighup(
    mut client_count_rx: mpsc::Receiver<ClientCountChange>,
    mut sigterm: tokio::signal::unix::Signal,
    mut sigint: tokio::signal::unix::Signal,
) {
    loop {
        tokio::select! {
            _ = sigterm.recv() => {
                tracing::info!("Received SIGTERM, shutting down gracefully");
                return;
            }

            _ = sigint.recv() => {
                tracing::info!("Received SIGINT, shutting down gracefully");
                return;
            }

            msg = client_count_rx.recv() => {
                match msg {
                    None => {
                        tracing::info!("Client count channel closed");
                        return;
                    }
                    Some(ClientCountChange::Connected) => {
                        continue;
                    }
                    Some(ClientCountChange::AllDisconnected) => {
                        tracing::info!("No clients connected, shutting down");
                        return;
                    }
                }
            }
        }
    }
}
