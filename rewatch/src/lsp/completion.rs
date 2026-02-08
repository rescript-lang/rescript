use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CompletionItem, CompletionResponse, Position, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a completion request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<CompletionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "completion")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, position).map(CompletionResponse::Array)
}

/// Run a completion request by shelling out to `rescript-editor-analysis.exe rewatch completion`.
///
/// Builds a JSON blob with all the package/module context the analysis binary needs,
/// pipes it to stdin, and parses the JSON completion items from stdout.
#[instrument(name = "lsp.completion", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
    items_count = tracing::field::Empty,
))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
) -> Option<Vec<CompletionItem>> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.completion.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.completion.build_context").entered();
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

    let items = {
        let _guard = tracing::info_span!("lsp.completion.analysis_binary").entered();

        let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "completion"], &json_blob)?;

        parse_completion_response(&stdout)
    };

    span.record("items_count", items.as_ref().map_or(0, |v| v.len()));
    items
}

/// Parse the JSON output from the analysis binary into CompletionItems.
///
/// The analysis binary already outputs LSP-conformant completion items,
/// so we deserialize directly.
fn parse_completion_response(stdout: &str) -> Option<Vec<CompletionItem>> {
    serde_json::from_str(stdout).ok()
}
