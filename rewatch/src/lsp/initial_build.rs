use rayon::prelude::*;

use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage};
use crate::build::diagnostics::BscDiagnostic;
use crate::build::packages;

use super::initialize::{self, DiscoveredWorkspace};

/// Run initial builds for all discovered workspaces in parallel.
///
/// Partitions workspaces into conflict groups (workspaces sharing a package
/// path must build sequentially), then builds groups in parallel via rayon.
pub fn run_all(workspaces: Vec<DiscoveredWorkspace>) -> Vec<(BuildCommandState, Vec<BscDiagnostic>)> {
    let groups = initialize::partition_workspaces(workspaces);
    let parent_span = tracing::Span::current();

    groups
        .into_par_iter()
        .flat_map(|group| {
            group
                .into_iter()
                .filter_map(|workspace| match run(workspace, &parent_span) {
                    Ok(result) => Some(result),
                    Err(e) => {
                        tracing::error!("Initial build failed: {e}");
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

/// Run the initial build for a discovered workspace.
///
/// Takes already-discovered packages and project context (from initialization),
/// populates source files, then runs the full build pipeline.
/// Returns the build state (for reuse in subsequent incremental builds)
/// and structured diagnostics from the build (errors and warnings).
pub fn run(
    workspace: DiscoveredWorkspace,
    parent_span: &tracing::Span,
) -> Result<(BuildCommandState, Vec<BscDiagnostic>), String> {
    let DiscoveredWorkspace {
        project_context,
        packages: discovered_packages,
    } = workspace;

    // Explicit parent prevents inheriting an ambient span from the rayon
    // thread pool.  Without this, rayon work-stealing can cause one
    // workspace's initial_build span to be recorded as a child of another
    // workspace's span that happened to run on the same thread.
    let _span = tracing::info_span!(
        parent: parent_span,
        "lsp.initial_build",
        project = %project_context.get_root_config().name,
    )
    .entered();

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
