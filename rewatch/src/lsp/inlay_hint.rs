use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{InlayHint, Position, Range, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle an inlay hint request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    range: Range,
) -> Option<Vec<InlayHint>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "inlay_hint")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, range)
}

/// Run an inlay hint request by shelling out to
/// `rescript-editor-analysis.exe rewatch inlayHint`.
///
/// The analysis binary parses the `.res` file and returns inlay hints
/// showing type annotations for value bindings within the visible range.
#[instrument(name = "lsp.inlay_hint", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    range: Range,
) -> Option<Vec<InlayHint>> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.inlay_hint.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.inlay_hint.build_context").entered();
        // Pass the line range as pos: [start_line, end_line].
        // The OCaml side interprets pos as (start_line, end_line) for inlay hints.
        let mut blob = analysis::build_context_json(
            build_state,
            source,
            &path_str,
            Position {
                line: range.start.line,
                character: range.end.line,
            },
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        );

        // Add maxLength field (empty string = no limit)
        if let serde_json::Value::Object(ref mut map) = blob {
            map.insert("maxLength".to_string(), serde_json::Value::String(String::new()));
        }

        blob
    };

    let _guard = tracing::info_span!("lsp.inlay_hint.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "inlayHint"], &json_blob)?;

    parse_inlay_hint_response(&stdout)
}

/// Parse the JSON output from the analysis binary into a Vec<InlayHint>.
///
/// The analysis binary outputs LSP-conformant JSON:
/// `[{"position": {...}, "label": "...", "kind": 1, "paddingLeft": true, "paddingRight": false}, ...]`
fn parse_inlay_hint_response(stdout: &str) -> Option<Vec<InlayHint>> {
    let hints: Vec<InlayHint> = serde_json::from_str(stdout).ok()?;
    if hints.is_empty() { None } else { Some(hints) }
}
