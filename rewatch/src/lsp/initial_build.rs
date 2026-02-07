use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage};
use crate::build::diagnostics::BscDiagnostic;
use crate::build::packages;

use super::initialize::DiscoveredWorkspace;

/// Run the initial build for a discovered workspace.
///
/// Takes already-discovered packages and project context (from initialization),
/// populates source files, then runs the full build pipeline.
/// Returns the build state (for reuse in subsequent incremental builds)
/// and structured diagnostics from the build (errors and warnings).
pub fn run(workspace: DiscoveredWorkspace) -> Result<(BuildCommandState, Vec<BscDiagnostic>), String> {
    let _span = tracing::info_span!("lsp.initial_build").entered();

    let DiscoveredWorkspace {
        project_context,
        packages: discovered_packages,
    } = workspace;

    // Populate source files, modules, and dirs for each package
    let packages = packages::extend_with_children(&None, discovered_packages);

    // Prepare the build state (compiler info, validation, cleanup)
    let mut build_state = build::prepare_build(
        project_context,
        packages,
        Some(std::time::Duration::ZERO), // no timing
        false,                           // no progress output
        true,                            // plain output
        None,                            // no warn_error override
        BuildProfile::TypecheckOnly,
    )
    .map_err(|e| e.to_string())?;

    // TypecheckOnly doesn't produce .cmj files. Downgrade any modules marked
    // Built by cleanup_previous_build to TypeChecked, so the first
    // TypecheckAndEmit build (on save) will emit JS for them.
    for module in build_state.build_state.modules.values_mut() {
        if module.compilation_stage == CompilationStage::Built {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    // Run the actual build (parse, deps, compile)
    let diagnostics = match build::incremental_build(
        &mut build_state,
        BuildProfile::TypecheckOnly,
        Some(std::time::Duration::ZERO),
        true,  // initial_build
        false, // show_progress
        false, // only_incremental
        false, // create_sourcedirs
        true,  // plain_output
    ) {
        Ok(result) => {
            tracing::info!("Initial build succeeded");
            result.diagnostics
        }
        Err(e) => {
            tracing::warn!("Initial build completed with errors: {e}");
            // Build errors are expected (e.g. type errors in user code).
            // We still consider this a successful build cycle — return
            // diagnostics so they can be published to the client.
            e.diagnostics
        }
    };

    Ok((build_state, diagnostics))
}
