use std::path::Path;

use anyhow::Result;
use tokio_stream::StreamExt;

use super::{connection, output};
use crate::daemon::proto::BuildRequest;

/// Run a build via the daemon and render output
pub async fn run(
    folder: &str,
    filter: Option<String>,
    warn_error: Option<String>,
    no_timing: bool,
) -> Result<bool> {
    let root = connection::find_project_root(Path::new(folder))?;
    let working_dir = Path::new(folder).canonicalize()?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = BuildRequest {
        working_directory: working_dir.to_string_lossy().to_string(),
        filter,
        warn_error,
    };

    let mut stream = client.build(request).await?.into_inner();
    let mut success = false;

    while let Some(result) = stream.next().await {
        match result {
            Ok(event) => {
                success = output::render_event(&event, no_timing, success);
            }
            Err(e) => {
                eprintln!("Stream error: {}", e);
                return Ok(false);
            }
        }
    }

    Ok(success)
}
