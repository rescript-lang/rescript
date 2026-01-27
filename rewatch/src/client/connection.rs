use std::path::{Path, PathBuf};
use std::time::Duration;

use anyhow::{Context, Result, bail};
use hyper_util::rt::TokioIo;
use tokio::net::UnixStream;
use tokio::time::sleep;
use tonic::transport::{Channel, Endpoint, Uri};
use tower::service_fn;

use crate::daemon::{self, proto::rescript_daemon_client::RescriptDaemonClient};
use crate::helpers;

/// Check if a process with the given PID is running.
/// Uses sysinfo to check for process existence.
fn is_process_running(pid: u32) -> bool {
    use sysinfo::{Pid, PidExt, ProcessRefreshKind, RefreshKind, System, SystemExt};
    let pid = Pid::from_u32(pid);
    let system =
        System::new_with_specifics(RefreshKind::new().with_processes(ProcessRefreshKind::everything()));
    system.process(pid).is_some()
}

/// Read the PID from the daemon's PID file.
/// Returns None if the file doesn't exist or can't be parsed.
fn read_daemon_pid(root: &Path) -> Option<u32> {
    let pid_path = daemon::pid_path(root);
    std::fs::read_to_string(&pid_path)
        .ok()
        .and_then(|s| s.trim().parse().ok())
}

/// Check if the daemon is alive by verifying the PID file points to a running process.
/// Returns true if:
/// - PID file exists
/// - PID can be parsed
/// - Process with that PID is running
fn is_daemon_process_alive(root: &Path) -> bool {
    match read_daemon_pid(root) {
        Some(pid) => is_process_running(pid),
        None => false,
    }
}

/// Clean up stale daemon files (PID file, socket path file, and socket).
/// Call this when we detect the daemon is not running but files still exist.
fn cleanup_stale_daemon_files(root: &Path) {
    let pid_path = daemon::pid_path(root);
    let socket_path_file = daemon::socket_path_file(root);
    let socket_path = daemon::socket_path(root);

    // Best-effort cleanup - ignore errors (files may already be removed)
    let _ = std::fs::remove_file(&pid_path);
    let _ = std::fs::remove_file(&socket_path_file);
    let _ = std::fs::remove_file(&socket_path);
}

/// Find the project root by looking for rescript.json, respecting monorepo structure.
/// If we're in a package that's listed in a parent workspace's dependencies,
/// the parent is the true root (where the daemon socket should live).
pub fn find_project_root(start: &Path) -> Result<PathBuf> {
    let current = start.canonicalize()?;

    // Check for legacy bsconfig.json before looking for rescript.json
    if current.join("bsconfig.json").exists() && !current.join("rescript.json").exists() {
        bail!(
            "Could not read rescript.json. bsconfig.json is no longer supported, please rename it to rescript.json"
        );
    }

    helpers::get_workspace_root(&current)
        .ok_or_else(|| anyhow::anyhow!("Could not find rescript.json in any parent directory"))
}

/// Get the socket path for a project root
pub fn get_socket_path(root: &Path) -> PathBuf {
    daemon::socket_path(root)
}

/// Check if the daemon is running by checking if the socket exists
pub fn is_daemon_running(root: &Path) -> bool {
    get_socket_path(root).exists()
}

/// Wait for an old daemon to clean up its socket file.
/// Returns when the socket no longer exists or max attempts reached.
async fn wait_for_socket_cleanup(socket_path: &Path) {
    for _ in 0..5 {
        if !socket_path.exists() {
            return;
        }
        sleep(Duration::from_millis(200)).await;
    }
}

/// Start the daemon if it's not already running.
/// Returns Ok(true) if daemon was started, Ok(false) if it was already running.
pub async fn start_daemon_if_needed(root: &Path) -> Result<bool> {
    let socket_path = get_socket_path(root);
    log::debug!(
        "start_daemon_if_needed: root={}, socket_path={}",
        root.display(),
        socket_path.display()
    );

    // First, check if the daemon process is actually alive using the PID file.
    // This catches the case where the daemon crashed or was killed without cleanup.
    // Only do this check if the socket doesn't exist - if socket exists, we'll
    // verify via ping anyway.
    if !socket_path.exists() && read_daemon_pid(root).is_some() && !is_daemon_process_alive(root) {
        log::debug!("Daemon process is not alive but stale files exist, cleaning up");
        cleanup_stale_daemon_files(root);
    }

    if is_daemon_running(root) {
        log::debug!("Socket exists, checking if daemon is responsive");
        // Try to ping the daemon to make sure it's actually responding
        match connect(root).await {
            Ok(mut client) => {
                if client.ping(crate::daemon::proto::PingRequest {}).await.is_ok() {
                    log::debug!("Daemon is already running and responsive");
                    return Ok(false); // Already running and responsive
                }
                log::debug!("Socket exists but daemon not responding, waiting for cleanup");
                // Socket exists but daemon not responding - it might be shutting down.
                // Wait for the old daemon to clean up its socket before starting a new one.
                // This avoids a race where we start a new daemon and the old one removes
                // the new daemon's socket during its cleanup.
                wait_for_socket_cleanup(&socket_path).await;

                // If socket still exists after waiting, forcefully remove it
                if socket_path.exists() {
                    log::debug!("Forcefully removing stale socket file");
                    cleanup_stale_daemon_files(root);
                }
            }
            Err(e) => {
                log::debug!("Socket exists but can't connect: {}", e);
                // Socket exists but can't connect - wait for cleanup then remove if needed
                wait_for_socket_cleanup(&socket_path).await;
                if socket_path.exists() {
                    log::debug!("Forcefully removing stale socket file after wait");
                    cleanup_stale_daemon_files(root);
                }
            }
        }
    } else {
        log::debug!("No existing daemon socket found");
    }

    // Start the daemon
    let exe = std::env::current_exe().context("Failed to get current executable path")?;
    let root_str = root.to_string_lossy().to_string();

    log::debug!("Starting daemon: exe={}, root={}", exe.display(), root_str);

    // Create a log file for daemon output to help debug startup failures
    let daemon_log_path = root.join("lib").join("bs").join("daemon.log");
    // Ensure parent directory exists before creating the log file
    if let Some(parent) = daemon_log_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let daemon_log = std::fs::File::create(&daemon_log_path).ok();

    let (stdout_cfg, stderr_cfg) = match daemon_log {
        Some(file) => match file.try_clone() {
            Ok(file_clone) => {
                log::debug!("Daemon output will be logged to: {}", daemon_log_path.display());
                (
                    std::process::Stdio::from(file),
                    std::process::Stdio::from(file_clone),
                )
            }
            Err(_) => {
                log::debug!("Could not clone daemon log file, output will be discarded");
                (std::process::Stdio::null(), std::process::Stdio::null())
            }
        },
        None => {
            log::debug!("Could not create daemon log file, output will be discarded");
            (std::process::Stdio::null(), std::process::Stdio::null())
        }
    };

    // Inherit current environment so RESCRIPT_BSC_EXE and other vars are passed to daemon
    std::process::Command::new(&exe)
        .arg("daemon")
        .arg(&root_str)
        .envs(std::env::vars())
        .stdin(std::process::Stdio::null())
        .stdout(stdout_cfg)
        .stderr(stderr_cfg)
        .spawn()
        .context("Failed to spawn daemon process")?;

    // Wait for the daemon to be ready
    let socket_path = get_socket_path(root);
    let error_path = daemon::error_path(root);
    log::debug!(
        "Waiting for daemon to be ready: socket_path={}, error_path={}",
        socket_path.display(),
        error_path.display()
    );

    for attempt in 0..50 {
        // Try for up to 5 seconds
        sleep(Duration::from_millis(100)).await;

        // Check if daemon wrote an error file (startup failed)
        if error_path.exists() {
            log::debug!("Daemon error file exists at attempt {}", attempt);
            if let Ok(error_msg) = std::fs::read_to_string(&error_path) {
                let _ = std::fs::remove_file(&error_path);
                bail!("{}", error_msg);
            }
        }

        let socket_exists = socket_path.exists();
        if socket_exists {
            match connect(root).await {
                Ok(mut client) => {
                    if client.ping(crate::daemon::proto::PingRequest {}).await.is_ok() {
                        log::debug!("Daemon started successfully at attempt {}", attempt);
                        return Ok(true);
                    }
                    log::debug!("Attempt {}: socket exists, connected, but ping failed", attempt);
                }
                Err(e) => {
                    log::debug!("Attempt {}: socket exists but connect failed: {}", attempt, e);
                }
            }
        } else if attempt % 10 == 0 {
            log::debug!("Attempt {}: socket does not exist yet", attempt);
        }
    }

    // Check one more time for error file before giving up
    if error_path.exists() {
        log::debug!("Final check: daemon error file exists");
        if let Ok(error_msg) = std::fs::read_to_string(&error_path) {
            let _ = std::fs::remove_file(&error_path);
            bail!("{}", error_msg);
        }
    }

    // Provide more diagnostic information in the error message
    let socket_exists = socket_path.exists();
    let error_file_exists = error_path.exists();

    // Try to read daemon log file for additional diagnostics
    let daemon_log_content = std::fs::read_to_string(&daemon_log_path)
        .ok()
        .filter(|s| !s.is_empty());

    let mut error_msg = format!(
        "Daemon failed to start within timeout (5s). socket_path={}, socket_exists={}, error_file_exists={}",
        socket_path.display(),
        socket_exists,
        error_file_exists
    );

    if let Some(log_content) = daemon_log_content {
        error_msg.push_str("\n--- daemon.log ---\n");
        error_msg.push_str(&log_content);
    }

    bail!("{}", error_msg)
}

/// Connect to the daemon, returning a gRPC client
pub async fn connect(root: &Path) -> Result<RescriptDaemonClient<Channel>> {
    let socket_path = get_socket_path(root);

    if !socket_path.exists() {
        bail!(
            "Daemon is not running (socket not found at {})",
            socket_path.display()
        );
    }

    // Create a channel that connects via Unix socket
    // The URL is a dummy - we use connect_with_connector to override the connection
    let channel = Endpoint::try_from("http://[::]:50051")?
        .connect_with_connector(service_fn(move |_: Uri| {
            let path = socket_path.clone();
            async move {
                let stream = UnixStream::connect(path).await?;
                Ok::<_, std::io::Error>(TokioIo::new(stream))
            }
        }))
        .await
        .context("Failed to connect to daemon")?;

    Ok(RescriptDaemonClient::new(channel))
}

/// Connect to daemon, starting it if necessary.
/// Includes retry logic to handle race conditions when a daemon is shutting down
/// while a new client is trying to connect.
pub async fn connect_or_start(root: &Path) -> Result<RescriptDaemonClient<Channel>> {
    // Retry a few times to handle the race condition where:
    // 1. Previous command finishes, daemon starts shutdown grace period
    // 2. New command starts, tries to connect to dying daemon
    // 3. Connection fails, we start new daemon
    // 4. Old daemon might still be cleaning up
    const MAX_RETRIES: u32 = 3;
    const RETRY_DELAY: Duration = Duration::from_millis(200);

    let mut last_error = None;

    for attempt in 0..MAX_RETRIES {
        if attempt > 0 {
            log::debug!(
                "Retrying daemon connection (attempt {}/{})",
                attempt + 1,
                MAX_RETRIES
            );
            sleep(RETRY_DELAY).await;
        }

        match start_daemon_if_needed(root).await {
            Ok(_) => match connect(root).await {
                Ok(client) => return Ok(client),
                Err(e) => {
                    log::debug!("Failed to connect after starting daemon: {}", e);
                    last_error = Some(e);
                }
            },
            Err(e) => {
                log::debug!("Failed to start daemon: {}", e);
                last_error = Some(e);
            }
        }
    }

    Err(last_error.unwrap_or_else(|| anyhow::anyhow!("Failed to connect to daemon")))
}
