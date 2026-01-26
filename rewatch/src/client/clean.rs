use std::path::Path;

use anyhow::Result;
use tokio_stream::StreamExt;

use super::{connection, output};
use crate::daemon::proto::CleanRequest;

/// Run a clean via the daemon and render output
pub async fn run(folder: &str) -> Result<bool> {
    let root = connection::find_project_root(Path::new(folder))?;
    let working_dir = Path::new(folder).canonicalize()?;

    let mut client = connection::connect_or_start(&root).await?;

    let request = CleanRequest {
        working_directory: working_dir.to_string_lossy().to_string(),
    };

    let mut stream = client.clean(request).await?.into_inner();
    let mut success = false;

    while let Some(result) = stream.next().await {
        match result {
            Ok(event) => {
                success = output::render_event(&event, false, success);
            }
            Err(e) => {
                eprintln!("Stream error: {}", e);
                return Ok(false);
            }
        }
    }

    Ok(success)
}
