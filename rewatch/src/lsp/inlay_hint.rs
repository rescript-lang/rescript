use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{InlayHint, Position, Range, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle an inlay hint request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    range: Range,
) -> Option<Vec<InlayHint>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "inlay_hint")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        // Pass the line range as pos: [start_line, end_line].
        // The OCaml side interprets pos as (start_line, end_line) for inlay hints.
        guard.build_analysis_context(
            uri,
            file_path,
            &source,
            Position {
                line: range.start.line,
                character: range.end.line,
            },
            true,
            Some(&|map: &mut serde_json::Map<String, serde_json::Value>| {
                map.insert("maxLength".to_string(), serde_json::Value::Null);
            }),
        )?
    };

    let _span = tracing::info_span!(
        "lsp.inlay_hint",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "inlayHint"])?;
    let hints: Vec<InlayHint> = serde_json::from_str(&stdout).ok()?;
    if hints.is_empty() { None } else { Some(hints) }
}
