use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{GotoDefinitionResponse, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis::{self, AnalysisContext};
use crate::lsp::definition;

/// Handle a type definition request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "type_definition")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        let build_state = guard.get_for_uri(uri)?;
        AnalysisContext::new(build_state, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.type_definition",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "typeDefinition"])?;
    definition::parse_definition_response(&stdout)
}
