use crate::build;
use crate::build::build_types::{BuildCommandState, BuildConfig, CompileScope, OutputMode, OutputTarget};
use crate::build::clean;
use crate::build::diagnostics::BscDiagnostic;
use crate::build::packages;

use super::initialize::{self, DiscoveredWorkspace};

/// Run initial builds for all discovered workspaces, parallelising across
/// independent conflict groups.
///
/// Workspaces that share a package path (conflict group) must build
/// sequentially, but independent groups can safely overlap. We use
/// `std::thread::scope` to spawn one OS thread per group instead of
/// rayon's `par_iter`, because each build already uses rayon internally
/// for file-level parallelism (parse, compile). Nesting `par_iter` inside
/// `par_iter` on the same global thread pool risks starvation: the outer
/// tasks occupy rayon threads while inner tasks block on `bsc` subprocess
/// I/O, leaving no threads to make progress. Dedicated OS threads for
/// the outer level avoid this — they block independently while rayon's
/// work-stealing pool stays fully available for the inner parallelism.
pub fn run_all(workspaces: Vec<DiscoveredWorkspace>) -> Vec<(BuildCommandState, Vec<BscDiagnostic>)> {
    let groups = initialize::partition_workspaces(workspaces);
    let parent_span = tracing::Span::current();

    std::thread::scope(|s| {
        let handles: Vec<_> = groups
            .into_iter()
            .map(|group| {
                let span = &parent_span;
                s.spawn(move || {
                    group
                        .into_iter()
                        .filter_map(|workspace| match run(workspace, span) {
                            Ok(result) => Some(result),
                            Err(e) => {
                                tracing::error!("Initial build failed: {e}");
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                })
            })
            .collect();

        handles
            .into_iter()
            .flat_map(|h| h.join().unwrap_or_default())
            .collect()
    })
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

    // Clean stale LSP build artifacts before building. This avoids problems
    // where orphaned .cmt files from deleted modules make dependents appear
    // up-to-date when they actually need recompilation.
    for package in packages.values() {
        clean::clean_package_for_output(package, OutputTarget::Lsp);
    }

    // Prepare the build state (compiler info, validation, cleanup)
    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::FullTypecheck,
        output_mode: OutputMode::Silent,
    };

    let mut build_state = build::prepare_build(
        project_context,
        packages,
        Some(std::time::Duration::ZERO), // no timing
        None,                            // no warn_error override
        &build_config,
    )
    .map_err(|e| e.to_string())?;

    // Parse source files and resolve dependencies
    let parse_warnings =
        match build::parse_and_resolve(&mut build_state, &build_config, Some(std::time::Duration::ZERO)) {
            Ok(warnings) => warnings,
            Err(e) => {
                tracing::warn!("Initial build parse failed: {e}");
                return Ok((build_state, e.diagnostics));
            }
        };

    let diagnostics = match build::incremental_build(
        &mut build_state,
        build_config,
        parse_warnings,
        Some(std::time::Duration::ZERO),
    ) {
        Ok(result) => {
            tracing::info!("Initial build succeeded");
            result.diagnostics
        }
        Err(e) => {
            tracing::warn!("Initial build completed with errors: {e}");
            e.diagnostics
        }
    };

    Ok((build_state, diagnostics))
}
