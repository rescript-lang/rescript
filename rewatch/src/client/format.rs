use std::io::{self, Read, Write};

use anyhow::Result;
use tokio_stream::StreamExt;

use super::connection;
use crate::daemon::proto::{FormatRequest, daemon_event::Event};

/// Format content from stdin with the given extension (.res or .resi).
/// Writes formatted output to stdout.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_stdin(stdin_ext: String) -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    // Read stdin content locally — the daemon's stdin is /dev/null
    let mut stdin_content = String::new();
    io::stdin().read_to_string(&mut stdin_content)?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = FormatRequest {
        working_directory: current_dir.to_string_lossy().to_string(),
        check: false,
        files: vec![],
        stdin_ext: Some(stdin_ext),
        stdin_content: Some(stdin_content),
    };

    run_format_request(&mut client, request).await
}

/// Format all files in the project/monorepo scope.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_project() -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = FormatRequest {
        working_directory: current_dir.to_string_lossy().to_string(),
        check: false,
        files: vec![],
        stdin_ext: None,
        stdin_content: None,
    };

    run_format_request(&mut client, request).await
}

/// Format specific files.
/// Returns Ok(true) on success, Ok(false) on failure.
pub async fn format_files(files: Vec<String>) -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = FormatRequest {
        working_directory: current_dir.to_string_lossy().to_string(),
        check: false,
        files,
        stdin_ext: None,
        stdin_content: None,
    };

    run_format_request(&mut client, request).await
}

/// Check formatting of all files in the project/monorepo scope.
/// Returns Ok(true) if all files are formatted correctly, Ok(false) otherwise.
pub async fn check_format_project() -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = FormatRequest {
        working_directory: current_dir.to_string_lossy().to_string(),
        check: true,
        files: vec![],
        stdin_ext: None,
        stdin_content: None,
    };

    run_format_request(&mut client, request).await
}

/// Check formatting of specific files.
/// Returns Ok(true) if all files are formatted correctly, Ok(false) otherwise.
pub async fn check_format_files(files: Vec<String>) -> Result<bool> {
    let current_dir = std::env::current_dir()?;
    let root = connection::find_project_root(&current_dir)?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = FormatRequest {
        working_directory: current_dir.to_string_lossy().to_string(),
        check: true,
        files,
        stdin_ext: None,
        stdin_content: None,
    };

    run_format_request(&mut client, request).await
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
