use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CodeLens, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a code lens request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
) -> Option<Vec<CodeLens>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "code_lens")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(
            uri,
            file_path,
            &source,
            Position {
                line: 0,
                character: 0,
            },
            true,
            None,
        )?
    };

    let _span = tracing::info_span!(
        "lsp.code_lens",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "codeLens"])?;
    let lenses: Vec<CodeLens> = serde_json::from_str(&stdout).ok()?;
    if lenses.is_empty() { None } else { Some(lenses) }
}
