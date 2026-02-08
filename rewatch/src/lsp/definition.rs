use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Range, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a definition request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "definition")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, position)
}

/// Run a definition request by shelling out to `rescript-editor-analysis.exe rewatch definition`.
///
/// Builds a JSON blob with all the package/module context the analysis binary needs,
/// pipes it to stdin, and parses the JSON location response from stdout.
#[instrument(name = "lsp.definition", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.definition.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.definition.build_context").entered();
        analysis::build_context_json(
            build_state,
            source,
            &path_str,
            position,
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        )
    };

    let _guard = tracing::info_span!("lsp.definition.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "definition"], &json_blob)?;

    parse_definition_response(&stdout)
}

/// Parse the JSON output from the analysis binary into an LSP GotoDefinitionResponse.
///
/// The analysis binary outputs: `{"uri": "...", "range": {"start": {...}, "end": {...}}}`
/// or `"null"` when no definition is found.
fn parse_definition_response(stdout: &str) -> Option<GotoDefinitionResponse> {
    let json: serde_json::Value = serde_json::from_str(stdout).ok()?;

    let uri_str = json.get("uri")?.as_str()?;
    let uri = Url::parse(uri_str).ok()?;

    let range = json.get("range")?;
    let start = range.get("start")?;
    let end = range.get("end")?;

    let start_line = start.get("line")?.as_u64()?;
    let start_char = start.get("character")?.as_u64()?;
    let end_line = end.get("line")?.as_u64()?;
    let end_char = end.get("character")?.as_u64()?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri,
        range: Range {
            start: Position {
                line: start_line as u32,
                character: start_char as u32,
            },
            end: Position {
                line: end_line as u32,
                character: end_char as u32,
            },
        },
    }))
}
