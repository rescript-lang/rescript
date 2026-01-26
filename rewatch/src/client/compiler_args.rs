use std::path::Path;

use anyhow::Result;

use super::connection;
use crate::build;
use crate::daemon::proto::CompilerArgsRequest;

/// Get compiler arguments for a file.
///
/// This function tries to use the daemon if it's already running (for faster response),
/// but falls back to computing the args directly if no daemon is available.
/// Unlike other commands, this does NOT start a daemon because it must provide fast answers.
pub async fn get_compiler_args(file_path: &Path) -> Result<String> {
    let abs_path = file_path.canonicalize()?;

    // Try to find project root and connect to daemon if running
    if let Ok(root) = connection::find_project_root(&abs_path)
        && connection::is_daemon_running(&root)
    {
        // Try to connect to the running daemon
        if let Ok(mut client) = connection::connect(&root).await {
            let request = CompilerArgsRequest {
                file_path: abs_path.to_string_lossy().to_string(),
            };

            match client.get_compiler_args(request).await {
                Ok(response) => {
                    let resp = response.into_inner();
                    // Format as JSON to match the standalone output
                    let args = build::CompilerArgs {
                        compiler_args: resp.compiler_args,
                        parser_args: resp.parser_args,
                    };
                    return serde_json::to_string_pretty(&args)
                        .map_err(|e| anyhow::anyhow!("Failed to serialize: {}", e));
                }
                Err(_) => {
                    // Daemon returned an error, fall through to standalone mode
                }
            }
        }
    }

    // Fall back to standalone computation (no daemon or daemon failed)
    build::get_compiler_args(&abs_path)
}
