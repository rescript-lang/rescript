use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::Mutex;

use tower_lsp::lsp_types::{Position, Range, TextEdit, Url};
use tracing::instrument;

/// Handle a formatting request: get the source buffer, run `bsc -format -bs-read-stdin`,
/// and return a full-document TextEdit with the formatted output.
#[instrument(name = "lsp.formatting", skip_all, fields(file = %file_path.display()))]
pub fn run(
    bsc_path: &Path,
    open_buffers: &Mutex<HashMap<Url, String>>,
    file_path: &Path,
    uri: &Url,
) -> Option<Vec<TextEdit>> {
    let source = open_buffers
        .lock()
        .ok()
        .and_then(|buffers| buffers.get(uri).cloned())
        .or_else(|| std::fs::read_to_string(file_path).ok());

    let source = match source {
        Some(s) => s,
        None => {
            tracing::warn!("formatting: no buffer content available");
            return None;
        }
    };

    let mut child = match Command::new(bsc_path)
        .args(["-bs-read-stdin", "-format", &file_path.to_string_lossy()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::warn!("formatting: failed to spawn bsc: {e}");
            return None;
        }
    };

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(source.as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("formatting: bsc invocation failed: {e}");
            return None;
        }
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        tracing::debug!(stderr = %stderr, "formatting: bsc -format failed");
        return None;
    }

    let formatted = String::from_utf8_lossy(&output.stdout).into_owned();

    Some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: 0,
            },
        },
        new_text: formatted,
    }])
}
