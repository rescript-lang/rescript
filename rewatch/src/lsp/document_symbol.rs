use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{DocumentSymbol, DocumentSymbolResponse, Position};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a document symbol request: lock build state and delegate to the analysis binary.
///
/// Unlike other endpoints, documentSymbol is purely syntactic — it only needs
/// the file path, not source content or cursor position.
pub fn handle(
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
) -> Option<DocumentSymbolResponse> {
    let source = std::fs::read_to_string(file_path).ok()?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source)
}

/// Run a document symbol request by shelling out to
/// `rescript-editor-analysis.exe rewatch documentSymbol`.
///
/// The analysis binary parses the `.res` file directly and returns a
/// hierarchical list of symbols (types, values, modules, etc.).
#[instrument(name = "lsp.document_symbol", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(build_state: &BuildCommandState, file_path: &Path, source: &str) -> Option<DocumentSymbolResponse> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.document_symbol.build_context").entered();
        analysis::build_context_json(
            build_state,
            source,
            &path_str,
            Position {
                line: 0,
                character: 0,
            },
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        )
    };

    let _guard = tracing::info_span!("lsp.document_symbol.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "documentSymbol"], &json_blob)?;

    parse_document_symbol_response(&stdout)
}

/// Parse the JSON output from the analysis binary into a DocumentSymbolResponse.
///
/// The analysis binary outputs LSP-conformant JSON:
/// `[{"name": "...", "kind": N, "range": {...}, "selectionRange": {...}, "children": [...]}, ...]`
fn parse_document_symbol_response(stdout: &str) -> Option<DocumentSymbolResponse> {
    let symbols: Vec<DocumentSymbol> = serde_json::from_str(stdout).ok()?;
    if symbols.is_empty() {
        None
    } else {
        Some(DocumentSymbolResponse::Nested(symbols))
    }
}
