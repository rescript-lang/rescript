use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a definition request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "definition")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.definition",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "definition"])?;
    parse_definition_response(&stdout)
}

/// Parse the JSON output from the analysis binary into a GotoDefinitionResponse.
///
/// Shared with type_definition which uses the same response format.
pub fn parse_definition_response(stdout: &str) -> Option<GotoDefinitionResponse> {
    let location: Location = serde_json::from_str(stdout).ok()?;
    Some(GotoDefinitionResponse::Scalar(location))
}
