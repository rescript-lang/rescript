use super::build_types::*;
use super::compile;
use crate::build::compiler_info::write_compiler_info;
use tracing::{info_span, instrument};

/// Full typecheck: all dirty modules + dependents, typecheck only (no JS).
///
/// Used by LSP initial build and project rebuild. No progress bars or logging.
#[instrument(name = "full_typecheck", skip_all, fields(
    module_count = build_state.modules.len(),
    output = "lsp",
))]
pub fn full_typecheck(
    build_state: &mut BuildCommandState,
) -> Result<IncrementalBuildResult, IncrementalBuildError> {
    let (all, needs_compile) = super::dirty_modules_for_typecheck(build_state);

    let params = CompileParams {
        modules: all,
        filter: CompileFilter::DirtyOnly(needs_compile),
        mode: CompileMode::TypecheckOnly,
        scoped: false,
        output: OutputTarget::Lsp,
        show_progress: false,
    };

    let result = match compile::process_in_waves(build_state, &params, || {}, |_| {}) {
        Ok(result) => result,
        Err(e) => {
            return Err(IncrementalBuildError {
                kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
                output_mode: OutputMode::Silent,
                diagnostics: vec![],
                modules: Box::new(params.modules),
                skipped_modules: Box::default(),
            });
        }
    };

    if !result.compile_errors.is_empty() {
        let _error_span = info_span!("build.compile_error", error = %result.compile_errors).entered();
        if !result.skipped_modules.is_empty() {
            tracing::debug!(
                skipped = ?result.skipped_modules.iter().collect::<Vec<_>>(),
                "full_typecheck: modules skipped due to errors"
            );
        }
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            output_mode: OutputMode::Silent,
            diagnostics: result.to_diagnostics(),
            modules: Box::new(params.modules),
            skipped_modules: Box::new(result.skipped_modules),
        })
    } else {
        write_compiler_info(build_state, OutputTarget::Lsp);
        Ok(IncrementalBuildResult {
            diagnostics: result.to_diagnostics(),
            modules: params.modules,
        })
    }
}
