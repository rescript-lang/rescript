use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Position, Range, TextEdit, Url};
use tracing::instrument;

use crate::format::format_source;
use crate::lsp::analysis;

/// Handle a formatting request: get the source buffer, run `bsc -format -bs-read-stdin`,
/// and return a full-document TextEdit with the formatted output.
#[instrument(name = "lsp.formatting", skip_all, fields(file = %file_path.display()))]
pub fn run(
    bsc_path: &Path,
    open_buffers: &Mutex<HashMap<Url, String>>,
    file_path: &Path,
    uri: &Url,
) -> Option<Vec<TextEdit>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "formatting")?;

    let formatted = match format_source(bsc_path, &source, &file_path.to_string_lossy()) {
        Ok(f) => f,
        Err(e) => {
            tracing::debug!("formatting: {e}");
            return None;
        }
    };

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
