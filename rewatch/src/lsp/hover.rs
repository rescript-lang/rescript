use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Hover, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a hover request: resolve the source buffer, build analysis context
/// under lock, then spawn the analysis binary after releasing the lock.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<Hover> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "hover")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.hover",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "hover"])?;
    serde_json::from_str(&stdout).ok()
}
