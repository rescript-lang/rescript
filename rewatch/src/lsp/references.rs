use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Location, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a references request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<Vec<Location>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "references")?;
    handle_with_source(projects, file_path, uri, &source, position)
}

/// Core references logic that takes source directly, bypassing open_buffers.
/// Used by both the LSP handler and the HTTP endpoint.
pub fn handle_with_source(
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    source: &str,
    position: Position,
) -> Option<Vec<Location>> {
    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.references",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "references"])?;
    let locations: Vec<Location> = serde_json::from_str(&stdout).ok()?;
    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}
