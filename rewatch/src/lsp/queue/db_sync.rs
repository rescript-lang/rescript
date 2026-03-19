//! Background sync queue for keeping `rescript.db` fully up to date.
//!
//! Receives self-contained payloads from the main build queue after each flush
//! (including the initial build). Debounces and coalesces events, then shells
//! out to the analysis binary to extract module data and cross-module usage
//! data, writing it to the SQLite database.
//!
//! **Scope**: This queue updates all module data tables: `types`, `values`,
//! `fields`, `constructors`, `aliases`, nested `modules` rows, and `usages`.
//! Top-level module rows are preserved (their `module_id` is referenced by
//! other modules' usages); child data is deleted and re-inserted from fresh
//! analysis output. New modules not yet in the database are fully inserted.
//! If the database does not exist (user hasn't run `rescript sync`), sync
//! events are silently skipped.

use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use rusqlite::Connection;
use serde_json::{Value, json};
use tokio::sync::mpsc;
use tokio::time::Instant;

use tracing::Instrument;

use crate::llm_index;

/// Structured path info for a single module's cmt/cmti files.
#[derive(Debug, Clone)]
pub struct ModuleFileEntry {
    pub module_name: String,
    pub cmt: String,
    pub cmti: String,
    /// blake3 hash of the .cmt/.cmti file from the build state.
    /// Used to skip re-extraction when the typed tree didn't change.
    pub cmt_hash: String,
    /// Package name from the build state, used to look up `package_id`
    /// when inserting new modules that don't yet exist in the DB.
    pub package_name: String,
}

/// Everything the sync queue needs to know about a package.
/// Extracted from `BuildCommandState` while the build state is available.
#[derive(Debug, Clone)]
pub struct SyncPackageContext {
    pub root_path: PathBuf,
    pub suffix: String,
    pub rescript_version: (i32, i32),
    pub opens: Value,
    pub paths_for_module: Value,
    pub project_files: Value,
    pub dependencies_files: Value,
}

/// Self-contained payload with structured Rust data.
/// The sync queue consumer serializes to JSON when shelling out.
pub struct DbSyncEvent {
    /// Project root — used to locate `rescript.db`.
    pub project_root: PathBuf,
    /// Path to `rescript-editor-analysis.exe`.
    pub analysis_binary_path: PathBuf,
    /// Package context for building the analysis binary input.
    pub package_context: SyncPackageContext,
    /// Per-module cmt/cmti file paths for the touched modules.
    pub files: Vec<ModuleFileEntry>,
}

/// Background queue that keeps `rescript.db` in sync.
#[derive(Clone)]
pub struct DbSyncQueue {
    tx: mpsc::UnboundedSender<DbSyncEvent>,
}

impl DbSyncQueue {
    /// Create the queue and spawn the background consumer task.
    /// `parent_span` should be the LSP root span so the consumer appears
    /// as a sibling of `lsp.flush` in traces.
    pub fn new(debounce: Duration, parent_span: tracing::Span) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        tokio::spawn(consumer(rx, debounce).instrument(parent_span));
        DbSyncQueue { tx }
    }

    /// Send a sync event (non-blocking, fire-and-forget).
    pub fn send(&self, event: DbSyncEvent) {
        let _ = self.tx.send(event);
    }
}

/// Build the stdin JSON for the analysis binary from a `SyncPackageContext`.
fn build_stdin_json(ctx: &SyncPackageContext, files: &[&ModuleFileEntry]) -> Value {
    let file_entries: Vec<Value> = files
        .iter()
        .map(|f| {
            json!({
                "moduleName": f.module_name,
                "cmt": f.cmt,
                "cmti": f.cmti,
            })
        })
        .collect();

    json!({
        "packages": [{
            "rootPath": ctx.root_path.to_string_lossy(),
            "suffix": ctx.suffix,
            "rescriptVersion": [ctx.rescript_version.0, ctx.rescript_version.1],
            "genericJsxModule": Value::Null,
            "opens": ctx.opens,
            "pathsForModule": ctx.paths_for_module,
            "projectFiles": ctx.project_files,
            "dependenciesFiles": ctx.dependencies_files,
            "files": file_entries,
        }]
    })
}

/// Open the existing database if it has the expected schema.
/// Returns `None` if the database does not exist or has a stale schema —
/// the caller should skip the sync (a full `rescript sync` is needed first).
fn open_db(db_path: &std::path::Path) -> Option<Connection> {
    if !db_path.exists() {
        return None;
    }
    let conn = match Connection::open(db_path) {
        Ok(conn) => conn,
        Err(e) => {
            tracing::warn!("db_sync: failed to open {}: {e}", db_path.display());
            return None;
        }
    };

    // Match the pragmas used by the full `rescript sync` path so that
    // concurrent readers (e.g. Claude querying the DB) don't hit SQLITE_BUSY.
    conn.execute_batch("PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000; PRAGMA foreign_keys=ON;")
        .ok()?;

    // Check that the usages table exists (implies up-to-date schema)
    let has_usages: bool = conn
        .query_row(
            "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='usages'",
            [],
            |row| row.get::<_, i64>(0),
        )
        .map(|count| count > 0)
        .unwrap_or(false);

    if has_usages { Some(conn) } else { None }
}

/// Process a coalesced batch: shell out to analysis binary and update all module data in the DB.
fn process_batch(
    parent_span: &tracing::Span,
    project_root: &std::path::Path,
    analysis_binary_path: &std::path::Path,
    ctx: &SyncPackageContext,
    files: &[ModuleFileEntry],
) {
    let span = tracing::debug_span!(
        parent: parent_span,
        "db_sync.process_batch",
        project = %project_root.display(),
        module_count = files.len(),
        changed_count = tracing::field::Empty,
        skipped_count = tracing::field::Empty,
    );
    let _guard = span.enter();
    let db_path = project_root.join("rescript.db");
    let Some(conn) = open_db(&db_path) else {
        // No DB or stale schema — user needs to run `rescript sync` first.
        tracing::debug!("db_sync: no valid rescript.db at {}, skipping", db_path.display());
        return;
    };

    // Filter out modules whose cmt_hash matches what's already in the DB.
    // This avoids shelling out to the analysis binary for reverse-dependency
    // modules whose typed tree didn't actually change.
    let stored_hashes: HashMap<String, String> = conn
        .prepare("SELECT name, cmt_hash FROM modules WHERE cmt_hash IS NOT NULL AND cmt_hash != ''")
        .and_then(|mut stmt| {
            let rows = stmt.query_map([], |row| Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?)))?;
            rows.collect::<Result<HashMap<_, _>, _>>()
        })
        .unwrap_or_default();

    let mut no_hash = 0usize;
    let mut not_in_db = 0usize;
    let mut hash_mismatch = 0usize;
    let mut hash_match = 0usize;

    let changed_files: Vec<&ModuleFileEntry> = files
        .iter()
        .filter(|f| {
            if f.cmt_hash.is_empty() {
                no_hash += 1;
                return true; // no hash available — include to be safe
            }
            match stored_hashes.get(&f.module_name) {
                Some(stored) if stored == &f.cmt_hash => {
                    hash_match += 1;
                    false
                }
                Some(_) => {
                    hash_mismatch += 1;
                    true
                }
                None => {
                    not_in_db += 1;
                    true
                }
            }
        })
        .collect();

    // `span` is the manually created db_sync.process_batch span entered above.
    span.record("changed_count", changed_files.len());
    span.record("skipped_count", hash_match);
    tracing::debug!(
        sent_to_analysis = changed_files.len(),
        hash_match,
        hash_mismatch,
        not_in_db,
        no_hash,
        "db_sync: hash filter results"
    );

    if changed_files.is_empty() {
        return;
    }

    let stdin_json = build_stdin_json(ctx, &changed_files);

    let analysis_output = match llm_index::spawn_analysis(analysis_binary_path, &stdin_json) {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("db_sync: analysis binary failed: {e}");
            return;
        }
    };

    // Run the full module data refresh inside a transaction. If anything fails,
    // the RAII Transaction guard automatically rolls back on drop.
    let result = (|| -> Result<(), rusqlite::Error> {
        let tx = conn.unchecked_transaction()?;

        // Build lookup maps for existing top-level modules and packages.
        let module_info: HashMap<String, (i64, i64)> = {
            let mut stmt = tx.prepare(
                "SELECT id, qualified_name, package_id FROM modules WHERE parent_module_id IS NULL",
            )?;
            let rows = stmt.query_map([], |row| {
                Ok((
                    row.get::<_, String>(1)?,
                    (row.get::<_, i64>(0)?, row.get::<_, i64>(2)?),
                ))
            })?;
            rows.collect::<Result<HashMap<_, _>, _>>()?
        };

        let package_id_map: HashMap<String, i64> = {
            let mut stmt = tx.prepare("SELECT id, name FROM packages")?;
            let rows = stmt.query_map([], |row| Ok((row.get::<_, String>(1)?, row.get::<_, i64>(0)?)))?;
            rows.collect::<Result<HashMap<_, _>, _>>()?
        };

        // Map module_name → file entry for compiled path / hash / package_name lookup.
        let file_entry_map: HashMap<&str, &ModuleFileEntry> = changed_files
            .iter()
            .map(|f| (f.module_name.as_str(), *f))
            .collect();

        // Phase 1: For each analysis module, delete old data and insert fresh data.
        for analysis_module in &analysis_output {
            let Some(file_entry) = file_entry_map.get(analysis_module.module_name.as_str()) else {
                continue;
            };
            let compiled_file_path = if file_entry.cmti.is_empty() {
                &file_entry.cmt
            } else {
                &file_entry.cmti
            };
            let source_file_path = &analysis_module.source_file_path;
            let cmt_hash = &file_entry.cmt_hash;

            if let Some(&(module_id, package_id)) = module_info.get(&analysis_module.qualified_name) {
                // Existing module: refresh child data + delete old usages
                llm_index::refresh_module_data(
                    &tx,
                    analysis_module,
                    module_id,
                    package_id,
                    compiled_file_path,
                    cmt_hash,
                    source_file_path,
                )?;
                tx.execute(
                    "DELETE FROM usages WHERE source_module_id = ?1",
                    rusqlite::params![module_id],
                )?;
            } else {
                // New module: look up package_id and do full insert
                let Some(&pkg_id) = package_id_map.get(&file_entry.package_name) else {
                    tracing::warn!(
                        module = %analysis_module.qualified_name,
                        package = %file_entry.package_name,
                        "db_sync: package not in DB, skipping new module"
                    );
                    continue;
                };
                match llm_index::insert_module_recursive(
                    &tx,
                    analysis_module,
                    pkg_id,
                    None,
                    compiled_file_path,
                    cmt_hash,
                    source_file_path,
                ) {
                    Ok(_) => {}
                    Err(e) => {
                        tracing::warn!(
                            module = %analysis_module.qualified_name,
                            "db_sync: failed to insert new module: {e}"
                        );
                    }
                }
            }
        }

        // Phase 2: Rebuild module_id_map (now includes new + nested modules)
        // and insert fresh usages for all changed modules.
        let module_id_map = llm_index::build_module_id_map(&tx)?;
        for analysis_module in &analysis_output {
            llm_index::insert_usages(&tx, analysis_module, &module_id_map)?;
        }

        tx.commit()?;
        Ok(())
    })();

    if let Err(e) = result {
        tracing::warn!("db_sync: transaction failed: {e}");
    }
}

/// Per-project coalesced state accumulated during the debounce window.
struct CoalescedProject {
    analysis_binary_path: PathBuf,
    package_context: SyncPackageContext,
    files_map: HashMap<String, ModuleFileEntry>,
}

impl CoalescedProject {
    fn from_event(event: DbSyncEvent) -> (PathBuf, Self) {
        let files_map = event
            .files
            .into_iter()
            .map(|f| (f.module_name.clone(), f))
            .collect();
        (
            event.project_root,
            CoalescedProject {
                analysis_binary_path: event.analysis_binary_path,
                package_context: event.package_context,
                files_map,
            },
        )
    }

    fn merge(&mut self, event: DbSyncEvent) {
        self.analysis_binary_path = event.analysis_binary_path;
        self.package_context = event.package_context;
        for f in event.files {
            self.files_map.insert(f.module_name.clone(), f);
        }
    }
}

fn merge_event(projects: &mut HashMap<PathBuf, CoalescedProject>, event: DbSyncEvent) {
    let project_root = event.project_root.clone();
    match projects.entry(project_root) {
        std::collections::hash_map::Entry::Occupied(mut entry) => {
            entry.get_mut().merge(event);
        }
        std::collections::hash_map::Entry::Vacant(entry) => {
            let (_root, coalesced) = CoalescedProject::from_event(event);
            entry.insert(coalesced);
        }
    }
}

/// Background consumer: debounces events and processes them in batches.
/// Events are keyed by `project_root` so that different projects in a
/// monorepo are processed independently with their own package context.
async fn consumer(mut rx: mpsc::UnboundedReceiver<DbSyncEvent>, debounce: Duration) {
    loop {
        // Wait for at least one event
        let Some(first) = rx.recv().await else {
            return; // Channel closed
        };

        // Coalesce events keyed by project_root
        let mut pending: HashMap<PathBuf, CoalescedProject> = HashMap::new();
        merge_event(&mut pending, first);

        // Debounce: drain any events that arrive within the window
        let deadline = Instant::now() + debounce;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                break;
            }
            match tokio::time::timeout(remaining, rx.recv()).await {
                Ok(Some(event)) => {
                    merge_event(&mut pending, event);
                }
                Ok(None) => return, // Channel closed
                Err(_) => break,    // Timeout — flush
            }
        }

        // Process each project in parallel.
        // Pass the current span so process_batch can parent itself to
        // db_sync.consumer (spawn_blocking doesn't propagate context).
        let parent_span = tracing::Span::current();
        let handles: Vec<_> = pending
            .into_iter()
            .map(|(project_root, coalesced)| {
                let files: Vec<ModuleFileEntry> = coalesced.files_map.into_values().collect();
                let analysis_binary_path = coalesced.analysis_binary_path;
                let package_context = coalesced.package_context;
                let span = parent_span.clone();

                tokio::task::spawn_blocking(move || {
                    process_batch(
                        &span,
                        &project_root,
                        &analysis_binary_path,
                        &package_context,
                        &files,
                    );
                })
            })
            .collect();
        for handle in handles {
            handle.await.unwrap_or_else(|e| {
                tracing::error!("db_sync: task panicked: {e}");
            });
        }
    }
}
