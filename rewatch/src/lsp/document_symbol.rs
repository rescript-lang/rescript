use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{DocumentSymbolResponse, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis::{self, AnalysisContext};

/// Handle a document symbol request.
///
/// Document symbols are purely syntactic — the analysis binary parses the
/// source to extract symbols, so no `.cmt` is needed.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
) -> Option<DocumentSymbolResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "document_symbol")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        let build_state = guard.get_for_uri(uri)?;
        AnalysisContext::new(
            build_state,
            file_path,
            &source,
            Position {
                line: 0,
                character: 0,
            },
            false,
            None,
        )?
    };

    let _span = tracing::info_span!(
        "lsp.document_symbol",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "documentSymbol"])?;
    let symbols: Vec<tower_lsp::lsp_types::DocumentSymbol> = serde_json::from_str(&stdout).ok()?;
    if symbols.is_empty() {
        None
    } else {
        Some(DocumentSymbolResponse::Nested(symbols))
    }
}
