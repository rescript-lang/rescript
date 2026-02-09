use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CodeActionOrCommand, CodeActionResponse, Position, Range, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a code action request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    range: Range,
) -> Option<CodeActionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "code_action")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, range)
}

/// Run a code action request by shelling out to
/// `rescript-editor-analysis.exe rewatch codeAction`.
///
/// The analysis binary parses the source, walks the AST at the given range,
/// and returns refactoring suggestions (e.g. "Replace with switch",
/// "Add type annotation", "Extract module to file").
#[instrument(name = "lsp.code_action", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    range: Range,
) -> Option<CodeActionResponse> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.code_action.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.code_action.build_context").entered();
        let mut blob = analysis::build_context_json(
            build_state,
            source,
            &path_str,
            Position {
                line: range.start.line,
                character: range.start.character,
            },
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        );

        // Add endPos for the range end
        if let serde_json::Value::Object(ref mut map) = blob {
            map.insert(
                "endPos".to_string(),
                serde_json::json!([range.end.line, range.end.character]),
            );
        }

        blob
    };

    let _guard = tracing::info_span!("lsp.code_action.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "codeAction"], &json_blob)?;

    parse_code_action_response(&stdout)
}

/// Parse the JSON output from the analysis binary into a CodeActionResponse.
///
/// The analysis binary outputs LSP-conformant JSON:
/// `[{"title": "...", "kind": "refactor.rewrite", "edit": {"documentChanges": [...]}}]`
fn parse_code_action_response(stdout: &str) -> Option<CodeActionResponse> {
    let actions: Vec<CodeActionOrCommand> = serde_json::from_str(stdout).ok()?;
    if actions.is_empty() { None } else { Some(actions) }
}
