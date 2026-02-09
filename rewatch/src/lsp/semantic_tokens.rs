use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Position, SemanticTokens, SemanticTokensResult, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis::{self, AnalysisContext};

/// Handle a semantic tokens request.
///
/// Semantic tokens are purely syntactic — no `.cmt` is needed.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
) -> Option<SemanticTokensResult> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "semantic_tokens")?;

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
        "lsp.semantic_tokens",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "semanticTokens"])?;
    let tokens: SemanticTokens = serde_json::from_str(&stdout).ok()?;
    if tokens.data.is_empty() {
        None
    } else {
        Some(SemanticTokensResult::Tokens(tokens))
    }
}
