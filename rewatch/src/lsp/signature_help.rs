use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Position, SignatureHelp, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a signature help request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<SignatureHelp> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "signature_help")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.signature_help",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "signatureHelp"])?;
    serde_json::from_str::<SignatureHelp>(&stdout).ok()
}
