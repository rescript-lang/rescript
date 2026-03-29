use super::build_types::*;
use super::compile;
use super::dependency_closure;
use ahash::AHashSet;
use tracing::{info_span, instrument};

/// Compile the dependency closure of the given modules to JS.
///
/// Used by LSP file save step 1 (and step 3 for previously-errored modules).
#[instrument(name = "compile_dependencies", skip_all, fields(
    module_count = build_state.modules.len(),
    output = "lsp",
))]
pub fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &AHashSet<String>,
) -> Result<IncrementalBuildResult, IncrementalBuildError> {
    let closure = dependency_closure::get_dependency_closure(&build_state.modules, module_names.clone());

    let needs_compile: AHashSet<String> = closure
        .iter()
        .filter(|name| {
            build_state
                .get_module(name)
                .is_some_and(|m| m.needs_compile_for_mode(CompileMode::FullCompile))
        })
        .cloned()
        .collect();

    tracing::debug!(
        starts = ?module_names.iter().collect::<Vec<_>>(),
        closure_size = closure.len(),
        needs_compile_count = needs_compile.len(),
        "compile_dependencies: closure computed"
    );

    let params = CompileParams {
        modules: closure,
        filter: CompileFilter::DirtyOnly(needs_compile),
        mode: CompileMode::FullCompile,
        scoped: true,
        output: OutputTarget::Lsp,
        show_progress: false,
    };

    let result = match compile::process_in_waves(build_state, &params, || {}, |_| {}) {
        Ok(result) => result,
        Err(e) => {
            // process_in_waves itself failed — the entire closure is skipped.
            let skipped_modules = Box::new(params.modules.clone());
            return Err(IncrementalBuildError {
                kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
                output_mode: OutputMode::Silent,
                diagnostics: vec![],
                modules: Box::new(params.modules),
                skipped_modules,
            });
        }
    };

    if !result.compile_errors.is_empty() {
        let _error_span = info_span!("build.compile_error", error = %result.compile_errors).entered();
        if !result.skipped_modules.is_empty() {
            tracing::debug!(
                skipped = ?result.skipped_modules.iter().collect::<Vec<_>>(),
                "compile_dependencies: modules skipped due to errors"
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
        Ok(IncrementalBuildResult {
            diagnostics: result.to_diagnostics(),
            modules: params.modules,
        })
    }
}
