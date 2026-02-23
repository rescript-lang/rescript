use super::build_types::*;
use super::compile;
use super::dependency_closure;
use ahash::AHashSet;
use tracing::{info_span, instrument};

/// Typecheck the dependent closure of the given modules (no JS).
///
/// Used by LSP file save step 2.
#[instrument(name = "typecheck_dependents", skip_all, fields(
    module_count = build_state.modules.len(),
    output = "lsp",
))]
pub fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &AHashSet<String>,
) -> Result<IncrementalBuildResult, IncrementalBuildError> {
    let all_dependents =
        dependency_closure::get_dependent_closure(&build_state.modules, module_names.clone());

    // Exclude modules at SourceDirty or ParseError — they need
    // parse_and_resolve before they can be compiled (their AST is
    // invalid or missing). DependencyDirty modules pass through
    // because their AST is still valid — they only need recompilation.
    let typecheckable: AHashSet<String> = all_dependents
        .into_iter()
        .filter(|name| {
            build_state.get_module(name).is_some_and(|m| match m {
                Module::SourceFile(sf) => {
                    !sf.compilation_stage().is_source_dirty() && !sf.compilation_stage().is_parse_error()
                }
                Module::MlMap(_) => true,
            })
        })
        .collect();

    let params = CompileParams {
        modules: typecheckable,
        filter: CompileFilter::All,
        mode: CompileMode::TypecheckOnly,
        scoped: true,
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
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            output_mode: OutputMode::Silent,
            diagnostics: result.to_diagnostics(),
            modules: Box::new(params.modules),
            skipped_modules: Box::default(),
        })
    } else {
        Ok(IncrementalBuildResult {
            diagnostics: result.to_diagnostics(),
            modules: params.modules,
        })
    }
}
