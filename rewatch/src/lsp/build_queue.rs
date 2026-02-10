//! Debounced build queue for LSP save events.
//!
//! When the editor fires rapid saves ("Save All", format-on-save), the
//! `didSave` handler sends each file path into an unbounded channel and
//! returns immediately. A single background consumer task collects paths
//! into a `HashSet`, resets a 150ms debounce timer on each arrival, and
//! flushes one batched build per project once the timer expires.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::sync::mpsc;
use tokio::time::Instant;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::instrument;

use ahash::AHashSet;

use super::{ProjectMap, group_by_file, notifications};
use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage, SourceType};
use crate::build::diagnostics::BscDiagnostic;

/// Result of a batched build for one project.
struct BatchBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    /// Absolute paths of all source files that were compiled or typechecked.
    touched_files: HashSet<PathBuf>,
}

pub struct BuildQueue {
    tx: mpsc::UnboundedSender<PathBuf>,
}

impl BuildQueue {
    /// Create the queue and spawn the background consumer task.
    pub fn new(projects: Arc<Mutex<ProjectMap>>, client: Client) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        tokio::spawn(build_consumer(rx, projects, client));
        BuildQueue { tx }
    }

    /// Queue a file path for the next batched build.
    pub fn queue_file(&self, path: PathBuf) {
        let _ = self.tx.send(path);
    }
}

async fn build_consumer(
    mut rx: mpsc::UnboundedReceiver<PathBuf>,
    projects: Arc<Mutex<ProjectMap>>,
    client: Client,
) {
    const DEBOUNCE: Duration = Duration::from_millis(150);
    let mut pending: HashSet<PathBuf> = HashSet::new();

    loop {
        // Phase 1: wait for at least one event
        match rx.recv().await {
            Some(path) => {
                pending.insert(path);
            }
            None => break,
        }

        // Phase 2: debounce — collect more events until 150ms of silence
        let deadline = tokio::time::sleep(DEBOUNCE);
        tokio::pin!(deadline);
        loop {
            tokio::select! {
                event = rx.recv() => {
                    match event {
                        Some(path) => {
                            pending.insert(path);
                            deadline.as_mut().reset(Instant::now() + DEBOUNCE);
                        }
                        None => {
                            flush_build(&mut pending, &projects, &client).await;
                            return;
                        }
                    }
                }
                _ = &mut deadline => break,
            }
        }

        // Phase 3: flush
        flush_build(&mut pending, &projects, &client).await;
    }
}

async fn flush_build(pending: &mut HashSet<PathBuf>, projects: &Arc<Mutex<ProjectMap>>, client: &Client) {
    if pending.is_empty() {
        return;
    }
    let file_paths: Vec<PathBuf> = pending.drain().collect();
    let projects = Arc::clone(projects);

    let results = tokio::task::spawn_blocking(move || {
        let mut guard = match projects.lock() {
            Ok(g) => g,
            Err(_) => return Vec::new(),
        };

        // Group files by project root
        let mut by_project: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();
        for path in &file_paths {
            if let Some(root) = guard.project_root_for_path(path) {
                by_project.entry(root).or_default().push(path.clone());
            }
        }

        // Mark dirty + build per project
        let mut results = Vec::new();
        for (root, paths) in &by_project {
            if let Some(build_state) = guard.states.get_mut(root) {
                let module_names: Vec<String> = paths
                    .iter()
                    .filter_map(|p| build_state.mark_file_parse_dirty(p))
                    .collect();
                if !module_names.is_empty() {
                    results.push(build_batch(build_state, module_names));
                }
            }
        }
        results
    })
    .await
    .unwrap_or_default();

    // Publish diagnostics and buildFinished outside blocking context
    for result in &results {
        let by_file = group_by_file(&result.diagnostics);
        for touched in &result.touched_files {
            if let Ok(uri) = Url::from_file_path(touched) {
                let diags = by_file.get(&uri).cloned().unwrap_or_default();
                client.publish_diagnostics(uri, diags, None).await;
            }
        }
    }

    client
        .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
        .await;
}

/// Map a set of module names to their absolute source file paths.
fn module_names_to_paths(build_state: &BuildCommandState, names: &AHashSet<String>) -> HashSet<PathBuf> {
    let mut paths = HashSet::new();
    for name in names {
        let Some(module) = build_state.build_state.modules.get(name) else {
            continue;
        };
        let SourceType::SourceFile(source_file) = &module.source_type else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };
        paths.insert(package.path.join(&source_file.implementation.path));
        if let Some(interface) = &source_file.interface {
            paths.insert(package.path.join(&interface.path));
        }
    }
    paths
}

/// Run a batched incremental build for one or more dirty modules.
///
/// This performs two phases:
///
/// 1. **Compile dependencies** (`TypecheckAndEmit`): Compile the dirty modules
///    and their transitive imports to produce JS output.
///
/// 2. **Typecheck dependents** (`TypecheckOnly`): Re-typecheck modules that
///    transitively import the dirty modules to surface errors caused by API
///    changes. No JS is emitted for dependents.
#[instrument(name = "lsp.build_batch", skip_all, fields(file_count = module_names.len()))]
fn build_batch(build_state: &mut BuildCommandState, module_names: Vec<String>) -> BatchBuildResult {
    let (mut diagnostics, mut touched_files) = compile_dependencies(build_state, &module_names);
    let (dep_diagnostics, dep_touched) = typecheck_dependents(build_state, &module_names);
    diagnostics.extend(dep_diagnostics);
    touched_files.extend(dep_touched);

    BatchBuildResult {
        diagnostics,
        touched_files,
    }
}

/// Phase 1: Compile the dirty modules and their transitive dependencies to JS.
///
/// After the initial LSP build (`TypecheckOnly`), every module sits at
/// `CompilationStage::TypeChecked`. A `TypecheckAndEmit` build targets
/// `CompilationStage::Built`, so every module would satisfy `needs_compile`.
///
/// To restrict compilation to only the dependency closure:
/// 1. Compute the closure (dirty modules + transitive imports)
/// 2. Temporarily promote modules **outside** the closure to `Built`
/// 3. Run the incremental build — only closure modules compile
/// 4. Restore promoted modules back to `TypeChecked`
#[instrument(name = "lsp.build_batch.compile_dependencies", skip_all)]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    use super::dependency_closure;

    let closure =
        dependency_closure::get_dependency_closure(&build_state.build_state.modules, module_names.to_vec());
    let touched_files = module_names_to_paths(build_state, &closure);

    // Temporarily promote modules outside the closure to `Built` so they
    // are excluded from the compile universe.
    let mut promoted: Vec<String> = Vec::new();
    for (name, module) in build_state.build_state.modules.iter_mut() {
        if !closure.contains(name) && module.compilation_stage == CompilationStage::TypeChecked {
            module.compilation_stage = CompilationStage::Built;
            promoted.push(name.clone());
        }
    }

    let diagnostics = match build::incremental_build(
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

    (diagnostics, touched_files)
}

/// Phase 2: Re-typecheck modules that transitively depend on the dirty modules.
///
/// After phase 1, the dirty modules' `.cmi` may have changed. Dependents need
/// to be re-typechecked against the updated type information to surface errors
/// (e.g. "unbound value" if an export was removed). No JS output is produced —
/// dependents get JS when they are themselves saved.
///
/// To scope the typecheck to only dependents:
/// 1. Compute the dependent closure (everything that transitively imports the dirty modules)
/// 2. Mark each dependent as parse-dirty so it enters the compile universe
/// 3. Temporarily promote modules **outside** the dependent closure to their
///    current stage's target (so they are excluded)
/// 4. Run an incremental build with `TypecheckOnly`
/// 5. Restore promoted modules
#[instrument(name = "lsp.build_batch.typecheck_dependents", skip_all, fields(dependent_count = tracing::field::Empty))]
fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    use super::dependency_closure;

    let dependents =
        dependency_closure::get_dependent_closure(&build_state.build_state.modules, module_names.to_vec());

    if dependents.is_empty() {
        return (Vec::new(), HashSet::new());
    }

    let touched_files = module_names_to_paths(build_state, &dependents);

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

    let diagnostics = match build::incremental_build(
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

    (diagnostics, touched_files)
}
