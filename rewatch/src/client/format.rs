use std::io::{self, Read, Write};

use anyhow::Result;
use tokio_stream::StreamExt;

use super::connection;
use crate::build;
use crate::daemon::proto::{FormatRequest, daemon_event::Event};

/// Format content from stdin with the given extension (.res or .resi).
/// Writes formatted output to stdout.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_stdin(stdin_ext: String) -> Result<bool> {
    let current_dir = std::env::current_dir()?;

    // Read stdin content locally — the daemon's stdin is /dev/null
    let mut stdin_content = String::new();
    io::stdin().read_to_string(&mut stdin_content)?;

    // Try daemon if running
    if let Ok(root) = connection::find_project_root(&current_dir)
        && connection::is_daemon_running(&root)
        && let Ok(mut client) = connection::connect(&root).await
    {
        let request = FormatRequest {
            working_directory: current_dir.to_string_lossy().to_string(),
            check: false,
            files: vec![],
            stdin_ext: Some(stdin_ext.clone()),
            stdin_content: Some(stdin_content.clone()),
        };
        return run_format_request(&mut client, request).await;
    }

    // Standalone: just run bsc -format directly
    match build::format::format_stdin(&stdin_content, &stdin_ext) {
        Ok(formatted) => {
            io::stdout().write_all(formatted.as_bytes())?;
            Ok(true)
        }
        Err(e) => {
            eprintln!("{}", e);
            Ok(false)
        }
    }
}

/// Format all files in the project/monorepo scope.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_project() -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    // Try daemon if running
    if connection::is_daemon_running(&root)
        && let Ok(mut client) = connection::connect(&root).await
    {
        let request = FormatRequest {
            working_directory: current_dir.to_string_lossy().to_string(),
            check: false,
            files: vec![],
            stdin_ext: None,
            stdin_content: None,
        };
        return run_format_request(&mut client, request).await;
    }

    // Standalone: collect files then format
    let files = build::format::collect_format_files(&current_dir, &root);
    Ok(run_standalone_format(&files, false))
}

/// Format specific files.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_files(files: Vec<String>) -> Result<bool> {
    let current_dir = std::env::current_dir()?;

    // Try daemon if running
    if let Ok(root) = connection::find_project_root(&current_dir)
        && connection::is_daemon_running(&root)
        && let Ok(mut client) = connection::connect(&root).await
    {
        let request = FormatRequest {
            working_directory: current_dir.to_string_lossy().to_string(),
            check: false,
            files: files.clone(),
            stdin_ext: None,
            stdin_content: None,
        };
        return run_format_request(&mut client, request).await;
    }

    // Standalone: format the specified files directly
    Ok(run_standalone_format(&files, false))
}

/// Check formatting of all files in the project/monorepo scope.
/// Returns Ok(true) if all files are formatted correctly, Ok(false) otherwise.
pub async fn check_format_project() -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    // Try daemon if running
    if connection::is_daemon_running(&root)
        && let Ok(mut client) = connection::connect(&root).await
    {
        let request = FormatRequest {
            working_directory: current_dir.to_string_lossy().to_string(),
            check: true,
            files: vec![],
            stdin_ext: None,
            stdin_content: None,
        };
        return run_format_request(&mut client, request).await;
    }

    // Standalone: collect files then check format
    let files = build::format::collect_format_files(&current_dir, &root);
    Ok(run_standalone_format(&files, true))
}

/// Check formatting of specific files.
/// Returns Ok(true) if all files are formatted correctly, Ok(false) otherwise.
pub async fn check_format_files(files: Vec<String>) -> Result<bool> {
    let current_dir = std::env::current_dir()?;

    // Try daemon if running
    if let Ok(root) = connection::find_project_root(&current_dir)
        && connection::is_daemon_running(&root)
        && let Ok(mut client) = connection::connect(&root).await
    {
        let request = FormatRequest {
            working_directory: current_dir.to_string_lossy().to_string(),
            check: true,
            files: files.clone(),
            stdin_ext: None,
            stdin_content: None,
        };
        return run_format_request(&mut client, request).await;
    }

    // Standalone: check the specified files directly
    Ok(run_standalone_format(&files, true))
}

/// Run format in standalone mode (no daemon).
/// Prints errors/check failures to stderr, matching the daemon client output format.
/// Returns true on success.
fn run_standalone_format(files: &[String], check: bool) -> bool {
    let (_, failed_count, error) = build::format::format_files(files, check, &|file| {
        eprintln!("[format check] {}", file);
    });

    if let Some(e) = error {
        eprintln!("{}", e);
        return false;
    }

    if check && failed_count > 0 {
        if failed_count == 1 {
            eprintln!("The file listed above needs formatting");
        } else {
            eprintln!("The {} files listed above need formatting", failed_count);
        }
        return false;
    }

    true
}

async fn run_format_request(
    client: &mut crate::daemon::proto::rescript_daemon_client::RescriptDaemonClient<
        tonic::transport::Channel,
    >,
    request: FormatRequest,
) -> Result<bool> {
    let mut stream = client.format(request).await?.into_inner();
    let mut success = false;

    while let Some(result) = stream.next().await {
        match result {
            Ok(event) => {
                if let Some(ref evt) = event.event {
                    match evt {
                        Event::FormattedStdin(content) => {
                            io::stdout().write_all(content.content.as_bytes())?;
                        }
                        Event::FormatProgress(_) => {
                            // Silent like the original
                        }
                        Event::FormatCheckFailed(check_failed) => {
                            eprintln!("[format check] {}", check_failed.file);
                        }
                        Event::CompilerError(err) => {
                            eprintln!("{}", err.message);
                        }
                        Event::FormatFinished(finished) => {
                            success = finished.success;
                        }
                        // Ignore other events (client lifecycle, build events, etc.)
                        _ => {}
                    }
                }
            }
            Err(e) => {
                eprintln!("Stream error: {}", e);
                return Ok(false);
            }
        }
    }

    Ok(success)
}
