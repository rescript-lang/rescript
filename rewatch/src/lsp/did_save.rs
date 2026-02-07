use std::path::Path;

use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage};
use crate::build::diagnostics::BscDiagnostic;

use super::dependency_closure;

/// Run an incremental build after a file was saved.
///
/// Marks the saved file as parse-dirty, then compiles only the saved file and
/// its transitive dependencies (the "dependency closure"). This avoids
/// compiling the entire project on the first save after LSP startup.
///
/// ## How scoping works
///
/// After the initial LSP build (`TypecheckOnly`), every module sits at
/// `CompilationStage::TypeChecked`. A `TypecheckAndEmit` build targets
/// `CompilationStage::Built`, so every module would satisfy `needs_compile`.
///
/// To restrict compilation to only the modules needed for the saved file:
/// 1. Compute the dependency closure (saved file + transitive imports)
/// 2. Temporarily promote modules **outside** the closure to `Built`
/// 3. Run the incremental build — only closure modules enter the compile universe
/// 4. Restore promoted modules back to `TypeChecked`
///
/// ## Caveat: dependents are NOT compiled
///
/// Modules that **import** the saved file (its dependents) are intentionally
/// excluded. Their type-check artifacts (.cmi/.cmt) remain valid from the
/// initial build. They will get JS output when they are themselves saved.
/// This is the right trade-off for LSP: compile the minimum needed for the
/// file the user is actively editing, not the entire reverse dependency graph.
pub fn run(build_state: &mut BuildCommandState, file_path: &Path) -> Vec<BscDiagnostic> {
    let _span = tracing::info_span!(
        "lsp.did_save",
        file = %file_path.display(),
    )
    .entered();

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

    // Compute the dependency closure: the saved module + everything it
    // transitively imports. This is the minimal set needed to produce
    // correct JS for the saved file.
    let closure = dependency_closure::get_dependency_closure(&build_state.build_state.modules, &module_name);

    // Temporarily promote modules outside the closure to `Built` so they
    // are excluded from the compile universe. We collect their names to
    // restore them afterwards.
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
