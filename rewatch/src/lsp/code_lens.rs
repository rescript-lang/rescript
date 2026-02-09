use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CodeLens, Position, Url};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a code lens request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
) -> Option<Vec<CodeLens>> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "code_lens")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source)
}

/// Run a code lens request by shelling out to
/// `rescript-editor-analysis.exe rewatch codeLens`.
///
/// The analysis binary parses the `.res` file and returns code lenses
/// showing function type signatures.
#[instrument(name = "lsp.code_lens", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(build_state: &BuildCommandState, file_path: &Path, source: &str) -> Option<Vec<CodeLens>> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.code_lens.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.code_lens.build_context").entered();
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

    let _guard = tracing::info_span!("lsp.code_lens.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "codeLens"], &json_blob)?;

    parse_code_lens_response(&stdout)
}

/// Parse the JSON output from the analysis binary into a Vec<CodeLens>.
///
/// The analysis binary outputs LSP-conformant JSON:
/// `[{"range": {...}, "command": {"title": "...", "command": ""}}, ...]`
fn parse_code_lens_response(stdout: &str) -> Option<Vec<CodeLens>> {
    let lenses: Vec<CodeLens> = serde_json::from_str(stdout).ok()?;
    if lenses.is_empty() { None } else { Some(lenses) }
}
