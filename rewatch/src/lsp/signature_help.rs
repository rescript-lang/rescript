use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Position, SignatureHelp, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a signatureHelp request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<SignatureHelp> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "signature_help")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, position)
}

/// Run a signatureHelp request by shelling out to `rescript-editor-analysis.exe rewatch signatureHelp`.
#[instrument(name = "lsp.signature_help", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
) -> Option<SignatureHelp> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.signature_help.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.signature_help.build_context").entered();
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

    let _guard = tracing::info_span!("lsp.signature_help.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "signatureHelp"], &json_blob)?;

    serde_json::from_str::<SignatureHelp>(&stdout).ok()
}
