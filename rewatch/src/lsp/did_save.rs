use std::path::Path;

use tracing::instrument;

use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage};
use crate::build::diagnostics::BscDiagnostic;

use super::dependency_closure;

/// Run an incremental build after a file was saved.
///
/// This performs two phases:
///
/// 1. **Compile dependencies** (`TypecheckAndEmit`): Compile the saved file and
///    its transitive imports to produce JS output. Only the dependency closure
///    is compiled — modules outside it are temporarily promoted to `Built`.
///
/// 2. **Typecheck dependents** (`TypecheckOnly`): Re-typecheck modules that
///    transitively import the saved file to surface errors caused by API
///    changes. No JS is emitted for dependents — they get JS when they are
///    themselves saved.
#[instrument(name = "lsp.did_save", skip_all, fields(file = %file_path.display()))]
pub fn run(build_state: &mut BuildCommandState, file_path: &Path) -> Vec<BscDiagnostic> {
    let module_name = match build_state.mark_file_parse_dirty(file_path) {
        Some(name) => name,
        None => {
            tracing::warn!(
                path = %file_path.display(),
                "didSave: no module found for file"
            );
            return Vec::new();
        }
    };

    let mut diagnostics = compile_dependencies(build_state, &module_name);
    diagnostics.extend(typecheck_dependents(build_state, &module_name));
    diagnostics
}

/// Phase 1: Compile the saved file and its transitive dependencies to JS.
///
/// After the initial LSP build (`TypecheckOnly`), every module sits at
/// `CompilationStage::TypeChecked`. A `TypecheckAndEmit` build targets
/// `CompilationStage::Built`, so every module would satisfy `needs_compile`.
///
/// To restrict compilation to only the dependency closure:
/// 1. Compute the closure (saved file + transitive imports)
/// 2. Temporarily promote modules **outside** the closure to `Built`
/// 3. Run the incremental build — only closure modules compile
/// 4. Restore promoted modules back to `TypeChecked`
#[instrument(name = "lsp.did_save.compile_dependencies", skip_all, fields(module = module_name))]
fn compile_dependencies(build_state: &mut BuildCommandState, module_name: &str) -> Vec<BscDiagnostic> {
    let closure = dependency_closure::get_dependency_closure(&build_state.build_state.modules, module_name);

    // Temporarily promote modules outside the closure to `Built` so they
    // are excluded from the compile universe.
    let mut promoted: Vec<String> = Vec::new();
    for (name, module) in build_state.build_state.modules.iter_mut() {
        if !closure.contains(name) && module.compilation_stage == CompilationStage::TypeChecked {
            module.compilation_stage = CompilationStage::Built;
            promoted.push(name.clone());
        }
    }

    let result = match build::incremental_build(
        build_state,
        BuildProfile::TypecheckAndEmit,
        Some(std::time::Duration::ZERO),
        false, // initial_build
        false, // show_progress
        true,  // only_incremental
        false, // create_sourcedirs
        true,  // plain_output
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Incremental build completed with errors: {e}");
            e.diagnostics
        }
    };

    // Restore promoted modules back to `TypeChecked`. Their type-check
    // artifacts are still valid; they just haven't emitted JS yet.
    for name in &promoted {
        if let Some(module) = build_state.build_state.modules.get_mut(name)
            && module.compilation_stage == CompilationStage::Built
        {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    result
}

/// Phase 2: Re-typecheck modules that transitively depend on the saved file.
///
/// After phase 1, the saved file's `.cmi` may have changed. Dependents need
/// to be re-typechecked against the updated type information to surface errors
/// (e.g. "unbound value" if an export was removed). No JS output is produced —
/// dependents get JS when they are themselves saved.
///
/// To scope the typecheck to only dependents:
/// 1. Compute the dependent closure (everything that transitively imports the saved file)
/// 2. Mark each dependent as parse-dirty so it enters the compile universe
/// 3. Temporarily promote modules **outside** the dependent closure to their
///    current stage's target (so they are excluded)
/// 4. Run an incremental build with `TypecheckOnly`
/// 5. Restore promoted modules
#[instrument(name = "lsp.did_save.typecheck_dependents", skip_all, fields(module = module_name, dependent_count = tracing::field::Empty))]
fn typecheck_dependents(build_state: &mut BuildCommandState, module_name: &str) -> Vec<BscDiagnostic> {
    let dependents = dependency_closure::get_dependent_closure(&build_state.build_state.modules, module_name);

    if dependents.is_empty() {
        return Vec::new();
    }

    tracing::Span::current().record("dependent_count", dependents.len());

    // Mark each dependent as parse-dirty so it enters the compile universe
    // for the TypecheckOnly build.
    for name in &dependents {
        if let Some(module) = build_state.build_state.modules.get_mut(name) {
            module.compilation_stage = CompilationStage::Dirty;
        }
    }

    // Temporarily promote modules outside the dependent closure so they are
    // excluded from the compile universe. Modules already at `Built` stay
    // there; we only promote `TypeChecked` → `Built` temporarily.
    let mut promoted: Vec<String> = Vec::new();
    for (name, module) in build_state.build_state.modules.iter_mut() {
        if !dependents.contains(name) && module.compilation_stage == CompilationStage::TypeChecked {
            module.compilation_stage = CompilationStage::Built;
            promoted.push(name.clone());
        }
    }

    let result = match build::incremental_build(
        build_state,
        BuildProfile::TypecheckOnly,
        Some(std::time::Duration::ZERO),
        false, // initial_build
        false, // show_progress
        true,  // only_incremental
        false, // create_sourcedirs
        true,  // plain_output
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Typecheck of dependents completed with errors: {e}");
            e.diagnostics
        }
    };

    // Restore promoted modules back to `TypeChecked`.
    for name in &promoted {
        if let Some(module) = build_state.build_state.modules.get_mut(name)
            && module.compilation_stage == CompilationStage::Built
        {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    result
}
