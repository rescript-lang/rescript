use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Instant;

use rusqlite::Connection;
use serde::Deserialize;
use serde_json::{Value, json};
use tracing::instrument;

use crate::build;
use crate::build::build_types::{BuildCommandState, Module, OutputTarget};
use crate::config::flatten_flags;
use crate::helpers;
use crate::lsp::analysis;

/// Parse the major and minor version from `CARGO_PKG_VERSION` at compile time.
/// Returns e.g. `(13, 0)` for version `"13.0.0-alpha.3"`.
pub fn cargo_pkg_version() -> (i32, i32) {
    (
        env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap_or(13),
        env!("CARGO_PKG_VERSION_MINOR").parse().unwrap_or(0),
    )
}
use crate::lsp::analysis::{RuntimeModuleData, scan_runtime_modules};

// ---------------------------------------------------------------------------
// Types for the analysis binary's llmIndex JSON output
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisModule {
    pub module_name: String,
    pub qualified_name: String,
    pub source_file_path: String,
    #[serde(default)]
    pub records: Vec<AnalysisRecord>,
    #[serde(default)]
    pub variants: Vec<AnalysisVariant>,
    #[serde(default)]
    pub type_aliases: Vec<AnalysisTypeAlias>,
    #[serde(default)]
    pub values: Vec<AnalysisValue>,
    #[serde(default)]
    pub module_aliases: Vec<AnalysisModuleAlias>,
    #[serde(default)]
    pub nested_modules: Vec<AnalysisModule>,
    #[serde(default)]
    pub usages: Vec<AnalysisUsage>,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisRecord {
    pub name: String,
    pub signature: Option<String>,
    #[serde(default)]
    pub fields: Vec<AnalysisField>,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisField {
    pub name: String,
    pub signature: String,
    #[serde(default)]
    pub optional: bool,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisVariant {
    pub name: String,
    pub signature: Option<String>,
    #[serde(default)]
    pub constructors: Vec<AnalysisConstructor>,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisConstructor {
    pub name: String,
    pub signature: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisTypeAlias {
    pub name: String,
    pub signature: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisValue {
    pub name: String,
    pub signature: Option<String>,
    #[serde(default)]
    pub param_count: i64,
    pub return_type: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct AnalysisModuleAlias {
    pub name: String,
    pub target: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisUsage {
    pub target_module: String,
    #[serde(default)]
    pub path: Vec<String>,
    pub tip: String,
    pub line: Option<i64>,
    pub col: Option<i64>,
}

pub const SCHEMA_DDL: &str = "
CREATE TABLE IF NOT EXISTS packages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL,
    path TEXT NOT NULL,
    rescript_json TEXT NOT NULL,
    config_hash TEXT,
    is_local INTEGER DEFAULT 0
);

CREATE TABLE IF NOT EXISTS modules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_id INTEGER NOT NULL,
    parent_module_id INTEGER,
    name TEXT NOT NULL,
    qualified_name TEXT NOT NULL UNIQUE,
    source_file_path TEXT NOT NULL,
    compiled_file_path TEXT NOT NULL,
    cmt_hash TEXT,
    is_auto_opened INTEGER DEFAULT 0,
    FOREIGN KEY (package_id) REFERENCES packages(id) ON DELETE CASCADE,
    FOREIGN KEY (parent_module_id) REFERENCES modules(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    kind TEXT NOT NULL,
    signature TEXT,
    FOREIGN KEY (module_id) REFERENCES modules(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    signature TEXT NOT NULL,
    optional INTEGER DEFAULT 0,
    FOREIGN KEY (type_id) REFERENCES types(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS constructors (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    signature TEXT,
    FOREIGN KEY (type_id) REFERENCES types(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS \"values\" (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    return_type TEXT,
    param_count INTEGER DEFAULT 0,
    signature TEXT,
    FOREIGN KEY (module_id) REFERENCES modules(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aliases (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_module_id INTEGER NOT NULL,
    alias_name TEXT NOT NULL,
    alias_kind TEXT NOT NULL,
    target_qualified_name TEXT NOT NULL,
    docstrings TEXT,
    FOREIGN KEY (source_module_id) REFERENCES modules(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_modules_package ON modules(package_id);
CREATE INDEX IF NOT EXISTS idx_modules_parent ON modules(parent_module_id);
CREATE INDEX IF NOT EXISTS idx_modules_qualified ON modules(qualified_name);
CREATE INDEX IF NOT EXISTS idx_modules_compiled_path ON modules(compiled_file_path);
CREATE INDEX IF NOT EXISTS idx_modules_auto_opened ON modules(is_auto_opened);
CREATE INDEX IF NOT EXISTS idx_types_module ON types(module_id);
CREATE INDEX IF NOT EXISTS idx_fields_type ON fields(type_id);
CREATE INDEX IF NOT EXISTS idx_constructors_type ON constructors(type_id);
CREATE INDEX IF NOT EXISTS idx_values_module ON \"values\"(module_id);
CREATE INDEX IF NOT EXISTS idx_aliases_name ON aliases(alias_name);
CREATE INDEX IF NOT EXISTS idx_aliases_source ON aliases(source_module_id);

CREATE TABLE IF NOT EXISTS usages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_module_id INTEGER NOT NULL,
    target_module_id INTEGER NOT NULL,
    target_path TEXT,
    tip TEXT NOT NULL,
    source_line INTEGER,
    source_col INTEGER,
    FOREIGN KEY (source_module_id) REFERENCES modules(id) ON DELETE CASCADE,
    FOREIGN KEY (target_module_id) REFERENCES modules(id) ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_usages_source ON usages(source_module_id);
CREATE INDEX IF NOT EXISTS idx_usages_target ON usages(target_module_id);
CREATE INDEX IF NOT EXISTS idx_usages_source_target ON usages(source_module_id, target_module_id);
CREATE INDEX IF NOT EXISTS idx_usages_target_path ON usages(target_module_id, target_path);
";

/// Collect the list of files to index (for the `files` array in the stdin JSON).
/// Each entry has `moduleName` and either `cmt` or `cmti`.
pub fn collect_files_for_indexing(build_state: &BuildCommandState) -> Vec<Value> {
    let mut files = Vec::new();

    for (module_name, module) in &build_state.build_state.modules {
        let Module::SourceFile(sf_module) = module else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&sf_module.package_name) else {
            continue;
        };
        let build_path = package.get_build_path_for_output(OutputTarget::Standard);
        let impl_path = &sf_module.source_file.implementation.path;
        let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);
        let dir = impl_path.parent().unwrap_or(Path::new(""));

        let cmt = build_path.join(dir).join(format!("{basename}.cmt"));
        if let Some(interface) = &sf_module.source_file.interface {
            let iface_path = &interface.path;
            let iface_basename =
                helpers::file_path_to_compiler_asset_basename(iface_path, &package.namespace);
            let iface_dir = iface_path.parent().unwrap_or(Path::new(""));
            let cmti = build_path.join(iface_dir).join(format!("{iface_basename}.cmti"));
            // Send both: cmti for module structure, cmt for usages
            files.push(json!({
                "moduleName": module_name,
                "cmt": cmt.to_string_lossy(),
                "cmti": cmti.to_string_lossy(),
            }));
        } else {
            files.push(json!({
                "moduleName": module_name,
                "cmt": cmt.to_string_lossy(),
                "cmti": "",
            }));
        }
    }

    // Runtime modules are handled separately via build_runtime_stdin_json
    files
}

/// Build the combined stdin JSON for a single `llmIndex` invocation.
/// Contains a `"packages"` array — each entry has its own context and files.
fn build_combined_stdin_json(
    build_state: &BuildCommandState,
    runtime: &RuntimeModuleData,
    root_package: &crate::build::packages::Package,
) -> Value {
    let root_config = build_state.build_state.get_root_config();
    let suffix = root_config.suffix.clone().unwrap_or_else(|| ".js".to_string());
    let opens = analysis::build_opens(&root_package.namespace, &root_package.config);
    let paths_for_module = analysis::build_paths_for_module(build_state, runtime, OutputTarget::Standard);
    let (project_files, dependencies_files) = analysis::build_file_sets(build_state, runtime);
    let files = collect_files_for_indexing(build_state);

    let (major, minor) = cargo_pkg_version();

    // Project + dependency packages context
    let project_entry = json!({
        "rootPath": root_package.path.to_string_lossy(),
        "suffix": suffix,
        "rescriptVersion": [major, minor],
        "genericJsxModule": Value::Null,
        "opens": opens,
        "pathsForModule": paths_for_module,
        "projectFiles": project_files,
        "dependenciesFiles": dependencies_files,
        "files": files,
    });

    // Runtime context
    let runtime_path = &build_state.build_state.compiler_info.runtime_path;
    let ocaml_dir = runtime_path.join("lib").join("ocaml");
    let runtime_files: Vec<Value> = runtime
        .module_names
        .iter()
        .filter(|name| runtime.paths.contains_key(name.as_str()))
        .map(|name| {
            let cmti_path = ocaml_dir.join(format!("{name}.cmti"));
            if cmti_path.exists() {
                json!({
                    "moduleName": name,
                    "cmt": "",
                    "cmti": cmti_path.to_string_lossy(),
                })
            } else {
                json!({
                    "moduleName": name,
                    "cmt": ocaml_dir.join(format!("{name}.cmt")).to_string_lossy(),
                    "cmti": "",
                })
            }
        })
        .collect();
    let runtime_project_files: Vec<Value> = runtime
        .module_names
        .iter()
        .filter(|name| runtime.paths.contains_key(name.as_str()))
        .map(|n| Value::String(n.clone()))
        .collect();
    let runtime_entry = json!({
        "rootPath": runtime_path.to_string_lossy(),
        "suffix": ".js",
        "rescriptVersion": [major, minor],
        "genericJsxModule": Value::Null,
        "opens": [],
        "pathsForModule": Value::Object(runtime.paths.clone()),
        "projectFiles": runtime_project_files,
        "dependenciesFiles": [],
        "files": runtime_files,
    });

    json!({
        "packages": [project_entry, runtime_entry]
    })
}

/// Spawn the analysis binary with `rewatch llmIndex` and return parsed output.
pub fn spawn_analysis(
    analysis_binary_path: &Path,
    stdin_json: &Value,
) -> anyhow::Result<Vec<AnalysisModule>> {
    let mut child = Command::new(analysis_binary_path)
        .args(["rewatch", "llmIndex"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| anyhow::anyhow!("Failed to spawn {}: {e}", analysis_binary_path.display()))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(stdin_json.to_string().as_bytes())
            .map_err(|e| anyhow::anyhow!("Failed to write stdin to analysis binary: {e}"))?;
    }

    let output = child.wait_with_output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.is_empty() {
        tracing::warn!(stderr = %stderr, "analysis binary stderr");
    }

    if !output.status.success() {
        return Err(anyhow::anyhow!(
            "Analysis binary exited with status {}",
            output.status
        ));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    serde_json::from_str(&stdout).map_err(|e| {
        anyhow::anyhow!(
            "Failed to parse analysis output as JSON: {e}\nstdout (first 500 chars): {}",
            &stdout[..stdout.len().min(500)]
        )
    })
}

/// Insert a module and its children into the database recursively.
/// Returns the number of modules inserted (including nested).
pub fn insert_module_recursive(
    conn: &Connection,
    module: &AnalysisModule,
    package_id: i64,
    parent_module_id: Option<i64>,
    compiled_file_path: &str,
    cmt_hash: &str,
    source_file_path: &str,
) -> rusqlite::Result<usize> {
    let module_id: i64 = conn.query_row(
        "INSERT INTO modules (package_id, parent_module_id, name, qualified_name, \
         source_file_path, compiled_file_path, cmt_hash, is_auto_opened) \
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, 0) \
         RETURNING id",
        rusqlite::params![
            package_id,
            parent_module_id,
            &module.module_name,
            &module.qualified_name,
            source_file_path,
            compiled_file_path,
            cmt_hash,
        ],
        |row| row.get(0),
    )?;

    let nested_count = insert_module_children(
        conn,
        module,
        module_id,
        package_id,
        compiled_file_path,
        cmt_hash,
        source_file_path,
    )?;

    Ok(1 + nested_count)
}

/// Insert all child data (types, values, aliases, nested modules) for a module
/// that already has a row in the DB. Returns the count of nested modules inserted.
pub fn insert_module_children(
    conn: &Connection,
    module: &AnalysisModule,
    module_id: i64,
    package_id: i64,
    compiled_file_path: &str,
    cmt_hash: &str,
    source_file_path: &str,
) -> rusqlite::Result<usize> {
    // Insert records → types table with kind='record' + fields table
    for record in &module.records {
        let type_id: i64 = conn.query_row(
            "INSERT INTO types (module_id, name, kind, signature) VALUES (?1, ?2, 'record', ?3) RETURNING id",
            rusqlite::params![module_id, &record.name, &record.signature],
            |row| row.get(0),
        )?;
        for field in &record.fields {
            conn.execute(
                "INSERT INTO fields (type_id, name, signature, optional) VALUES (?1, ?2, ?3, ?4)",
                rusqlite::params![type_id, &field.name, &field.signature, field.optional as i32],
            )?;
        }
    }

    // Insert variants → types table with kind='variant' + constructors table
    for variant in &module.variants {
        let type_id: i64 = conn.query_row(
            "INSERT INTO types (module_id, name, kind, signature) VALUES (?1, ?2, 'variant', ?3) RETURNING id",
            rusqlite::params![module_id, &variant.name, &variant.signature],
            |row| row.get(0),
        )?;
        for ctor in &variant.constructors {
            conn.execute(
                "INSERT INTO constructors (type_id, name, signature) VALUES (?1, ?2, ?3)",
                rusqlite::params![type_id, &ctor.name, &ctor.signature],
            )?;
        }
    }

    // Insert type aliases → types table with kind='alias'
    for alias in &module.type_aliases {
        conn.execute(
            "INSERT INTO types (module_id, name, kind, signature) VALUES (?1, ?2, 'alias', ?3)",
            rusqlite::params![module_id, &alias.name, &alias.signature],
        )?;
    }

    // Insert values
    for val in &module.values {
        conn.execute(
            "INSERT INTO \"values\" (module_id, name, return_type, param_count, signature) \
             VALUES (?1, ?2, ?3, ?4, ?5)",
            rusqlite::params![
                module_id,
                &val.name,
                &val.return_type,
                val.param_count,
                &val.signature,
            ],
        )?;
    }

    // Insert module aliases
    for alias in &module.module_aliases {
        conn.execute(
            "INSERT INTO aliases (source_module_id, alias_name, alias_kind, target_qualified_name, docstrings) \
             VALUES (?1, ?2, 'module', ?3, '')",
            rusqlite::params![module_id, &alias.name, &alias.target],
        )?;
    }

    // Note: usages are NOT inserted here because target modules may not exist
    // in the DB yet. Usages are inserted in a second pass after all modules
    // have been inserted (see `insert_usages`).

    // Recurse into nested modules
    let mut count = 0usize;
    for child in &module.nested_modules {
        count += insert_module_recursive(
            conn,
            child,
            package_id,
            Some(module_id),
            compiled_file_path,
            cmt_hash,
            source_file_path,
        )?;
    }

    Ok(count)
}

/// Delete all child data (types, values, aliases, nested modules) for a module.
/// Does NOT delete the module row itself, nor its usages.
/// Requires `PRAGMA foreign_keys = ON` for cascading nested module deletes.
pub fn delete_module_children(conn: &Connection, module_id: i64) -> rusqlite::Result<()> {
    // Delete types (with FK cascade, this also deletes fields + constructors)
    conn.execute(
        "DELETE FROM types WHERE module_id = ?1",
        rusqlite::params![module_id],
    )?;
    // Delete values
    conn.execute(
        "DELETE FROM \"values\" WHERE module_id = ?1",
        rusqlite::params![module_id],
    )?;
    // Delete aliases
    conn.execute(
        "DELETE FROM aliases WHERE source_module_id = ?1",
        rusqlite::params![module_id],
    )?;
    // Delete nested modules (with FK cascade, this recursively deletes their
    // types/values/aliases/further-nested modules)
    conn.execute(
        "DELETE FROM modules WHERE parent_module_id = ?1",
        rusqlite::params![module_id],
    )?;
    Ok(())
}

/// Refresh all data for an existing module: delete old children, update the
/// module row metadata, and re-insert fresh children from analysis output.
/// Returns the count of nested modules inserted.
pub fn refresh_module_data(
    conn: &Connection,
    module: &AnalysisModule,
    module_id: i64,
    package_id: i64,
    compiled_file_path: &str,
    cmt_hash: &str,
    source_file_path: &str,
) -> rusqlite::Result<usize> {
    delete_module_children(conn, module_id)?;

    conn.execute(
        "UPDATE modules SET cmt_hash = ?1, source_file_path = ?2, compiled_file_path = ?3 \
         WHERE id = ?4",
        rusqlite::params![cmt_hash, source_file_path, compiled_file_path, module_id],
    )?;

    insert_module_children(
        conn,
        module,
        module_id,
        package_id,
        compiled_file_path,
        cmt_hash,
        source_file_path,
    )
}

/// Build a qualified_name → module_id lookup map from the database.
/// Call this once after all modules have been inserted, then pass to `insert_usages`.
pub fn build_module_id_map(conn: &Connection) -> rusqlite::Result<std::collections::HashMap<String, i64>> {
    let mut stmt = conn.prepare("SELECT id, qualified_name FROM modules")?;
    let rows = stmt.query_map([], |row| Ok((row.get::<_, String>(1)?, row.get::<_, i64>(0)?)))?;
    let mut map = std::collections::HashMap::new();
    for row in rows {
        let (name, id) = row?;
        map.insert(name, id);
    }
    Ok(map)
}

/// Insert usages for a module (and its nested modules) into the DB.
/// Must be called after ALL modules have been inserted into the DB.
/// Uses a pre-built `module_id_map` for O(1) lookups instead of per-usage queries.
///
/// Currently only top-level modules carry usages — the OCaml side extracts them
/// from `extra.externalReferences` which is per-file. Nested modules should have
/// `usages: []`, but we recurse defensively in case this changes.
pub fn insert_usages(
    conn: &Connection,
    module: &AnalysisModule,
    module_id_map: &std::collections::HashMap<String, i64>,
) -> rusqlite::Result<()> {
    if !module.usages.is_empty() {
        let Some(&source_id) = module_id_map.get(&module.qualified_name) else {
            tracing::debug!(
                module = %module.qualified_name,
                "insert_usages: source module not in DB, skipping all usages"
            );
            return Ok(());
        };

        for usage in &module.usages {
            let Some(&target_id) = module_id_map.get(&usage.target_module) else {
                tracing::debug!(
                    source = %module.qualified_name,
                    target = %usage.target_module,
                    "insert_usages: target module not in DB, skipping"
                );
                continue;
            };
            let target_path = if usage.path.is_empty() {
                None
            } else {
                Some(usage.path.join("."))
            };
            conn.execute(
                "INSERT INTO usages (source_module_id, target_module_id, target_path, tip, source_line, source_col) \
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                rusqlite::params![source_id, target_id, &target_path, &usage.tip, usage.line, usage.col],
            )?;
        }
    }

    for child in &module.nested_modules {
        debug_assert!(
            child.usages.is_empty(),
            "nested module {} has non-empty usages — OCaml extraction changed?",
            child.qualified_name
        );
        insert_usages(conn, child, module_id_map)?;
    }

    Ok(())
}

/// Mark auto-opened modules in the database.
fn mark_auto_opened(conn: &Connection, build_state: &BuildCommandState) -> rusqlite::Result<()> {
    // Stdlib and Pervasives from runtime
    conn.execute(
        "UPDATE modules SET is_auto_opened = 1 WHERE qualified_name IN ('Stdlib', 'Pervasives')",
        [],
    )?;

    // Modules from -open flags in compiler config
    for (_pkg_name, package) in &build_state.build_state.packages {
        let flags = flatten_flags(&package.config.compiler_flags);
        let mut i = 0;
        while i < flags.len() {
            if flags[i] == "-open"
                && let Some(name) = flags.get(i + 1)
            {
                conn.execute(
                    "UPDATE modules SET is_auto_opened = 1 WHERE qualified_name = ?1",
                    rusqlite::params![name],
                )?;
                i += 2;
                continue;
            }
            i += 1;
        }
    }

    Ok(())
}

/// Run the sync command: build, index, and write rescript.db.
#[instrument(name = "rewatch.sync", skip_all, fields(working_dir = %folder))]
pub fn run_sync(folder: &str, show_progress: bool, plain_output: bool) -> i32 {
    let timing = Instant::now();
    let path = Path::new(folder);

    // Step 1: Typecheck-only build (produces .cmi/.cmt but no JS)
    if show_progress {
        eprintln!("Typechecking project...");
    }
    let build_config = build::build_types::BuildConfig {
        output: OutputTarget::Standard,
        mode: build::build_types::CompileMode::TypecheckOnly,
        output_mode: build::build_types::OutputMode::Standard {
            show_progress,
            plain_output,
            initial_build: true,
        },
    };
    let mut build_state = match build::initialize_build(None, &None, path, &build_config, None) {
        Ok(state) => state,
        Err(e) => {
            eprintln!("Build initialization failed: {e:#}");
            return 1;
        }
    };
    if let Err(e) = build::parse_and_resolve(&mut build_state, &build_config, None) {
        eprintln!("Parsing failed: {e}");
        return 1;
    }
    let (all_modules, needs_compile) = build::dirty_modules_for_typecheck(&build_state);
    let typecheck_params = build::build_types::CompileParams {
        modules: all_modules,
        filter: build::build_types::CompileFilter::DirtyOnly(needs_compile),
        mode: build::build_types::CompileMode::TypecheckOnly,
        scoped: false,
        output: OutputTarget::Standard,
        show_progress,
    };
    if let Err(e) = build::compile::process_in_waves(&mut build_state, &typecheck_params, || {}, |_| {}) {
        eprintln!("Typecheck failed: {e}");
        return 1;
    }

    // Step 2: Resolve analysis binary and runtime
    let analysis_binary_path = match build_state.build_state.compiler_info.bsc_path.parent() {
        Some(dir) => dir.join("rescript-editor-analysis.exe"),
        None => {
            eprintln!("Could not resolve analysis binary path");
            return 1;
        }
    };

    let runtime = scan_runtime_modules(&build_state.build_state.compiler_info.runtime_path);

    let db_path = path.join("rescript.db");
    if db_path.exists()
        && let Err(e) = std::fs::remove_file(&db_path)
    {
        eprintln!("Could not remove existing {}: {e}", db_path.display());
        return 1;
    }
    let conn = match Connection::open(&db_path) {
        Ok(conn) => conn,
        Err(e) => {
            eprintln!("Could not open database: {e}");
            return 1;
        }
    };

    if let Err(e) = conn.execute_batch("PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000;") {
        eprintln!("Could not set database pragmas: {e}");
        return 1;
    }
    if let Err(e) = conn.execute_batch(SCHEMA_DDL) {
        eprintln!("Could not apply database schema: {e}");
        return 1;
    }

    // Insert all packages into the database and build a name→id map
    let mut package_ids: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
    for (pkg_name, package) in &build_state.build_state.packages {
        let rescript_json_path = package.path.join("rescript.json");
        let rescript_json = std::fs::read_to_string(&rescript_json_path).unwrap_or_default();
        let is_local = (package.is_root || package.is_local_dep) as i32;
        match conn.query_row(
            "INSERT INTO packages (name, path, rescript_json, config_hash, is_local) \
             VALUES (?1, ?2, ?3, '', ?4) RETURNING id",
            rusqlite::params![pkg_name, package.path.to_string_lossy(), rescript_json, is_local],
            |row| row.get::<_, i64>(0),
        ) {
            Ok(id) => {
                package_ids.insert(pkg_name.clone(), id);
            }
            Err(e) => {
                eprintln!("Could not insert package {pkg_name}: {e}");
            }
        }
    }

    // Insert @rescript/runtime as a package
    let runtime_path = &build_state.build_state.compiler_info.runtime_path;
    let runtime_json = std::fs::read_to_string(runtime_path.join("package.json")).unwrap_or_default();
    match conn.query_row(
        "INSERT INTO packages (name, path, rescript_json, config_hash) \
         VALUES (?1, ?2, ?3, '') RETURNING id",
        rusqlite::params!["@rescript/runtime", runtime_path.to_string_lossy(), runtime_json],
        |row| row.get::<_, i64>(0),
    ) {
        Ok(id) => {
            package_ids.insert("@rescript/runtime".to_string(), id);
        }
        Err(e) => {
            eprintln!("Could not insert @rescript/runtime package: {e}");
        }
    }

    // Call analysis binary once with all packages (project + runtime)
    let Some(root_package) = build_state.build_state.packages.values().find(|p| p.is_root) else {
        eprintln!(
            "No root package found. Is there a rescript.json in {}?",
            path.display()
        );
        return 1;
    };
    let stdin_json = build_combined_stdin_json(&build_state, &runtime, root_package);

    if show_progress {
        eprintln!("Indexing modules...");
    }

    let runtime_ocaml_dir = runtime_path.join("lib").join("ocaml");
    let runtime_package_id = package_ids.get("@rescript/runtime").copied().unwrap_or(0);
    let runtime_names: std::collections::HashSet<&str> =
        runtime.module_names.iter().map(|s| s.as_str()).collect();

    let mut total_modules = 0;
    match spawn_analysis(&analysis_binary_path, &stdin_json) {
        Ok(analysis_output) => {
            let tx = match conn.unchecked_transaction() {
                Ok(tx) => tx,
                Err(e) => {
                    eprintln!("Failed to begin transaction: {e}");
                    return 1;
                }
            };
            for analysis_module in &analysis_output {
                let module_name = &analysis_module.module_name;

                // Look up which package this module belongs to and resolve paths.
                // Hash the .cmt/.cmti (not .cmi) so the incremental sync can
                // skip modules whose typed tree didn't change.
                let (package_id, source_file_path, cmt_hash, compiled) =
                    match build_state.build_state.modules.get(module_name.as_str()) {
                        Some(Module::SourceFile(sf)) => {
                            let pkg_id = package_ids.get(&sf.package_name).copied().unwrap_or(0);
                            let src = sf.source_file.implementation.path.to_string_lossy().to_string();
                            let Some(pkg) = build_state.build_state.packages.get(&sf.package_name) else {
                                continue;
                            };
                            let build_path = pkg.get_build_path_for_output(OutputTarget::Standard);
                            let impl_path = &sf.source_file.implementation.path;
                            let basename =
                                helpers::file_path_to_compiler_asset_basename(impl_path, &pkg.namespace);
                            let dir = impl_path.parent().unwrap_or(Path::new(""));
                            let (cmt_path, compiled_path) = if let Some(iface) = &sf.source_file.interface {
                                let iface_basename = helpers::file_path_to_compiler_asset_basename(
                                    &iface.path,
                                    &pkg.namespace,
                                );
                                let iface_dir = iface.path.parent().unwrap_or(Path::new(""));
                                let cmti = build_path.join(iface_dir).join(format!("{iface_basename}.cmti"));
                                let compiled = cmti.to_string_lossy().to_string();
                                (cmti, compiled)
                            } else {
                                let cmt = build_path.join(dir).join(format!("{basename}.cmt"));
                                let compiled = cmt.to_string_lossy().to_string();
                                (cmt, compiled)
                            };
                            let hash = helpers::compute_file_hash(&cmt_path)
                                .map(|h| h.to_string())
                                .unwrap_or_default();
                            (pkg_id, src, hash, compiled_path)
                        }
                        _ => {
                            // Not in build state — likely a runtime module
                            if runtime_package_id > 0 && runtime_names.contains(module_name.as_str()) {
                                let src = analysis_module.source_file_path.clone();
                                let cmti = runtime_ocaml_dir.join(format!("{module_name}.cmti"));
                                let cmt_path = if cmti.exists() {
                                    cmti
                                } else {
                                    runtime_ocaml_dir.join(format!("{module_name}.cmt"))
                                };
                                let hash = helpers::compute_file_hash(&cmt_path)
                                    .map(|h| h.to_string())
                                    .unwrap_or_default();
                                let compiled_path = cmt_path.to_string_lossy().to_string();
                                (runtime_package_id, src, hash, compiled_path)
                            } else {
                                continue;
                            }
                        }
                    };

                if package_id == 0 {
                    continue;
                }

                match insert_module_recursive(
                    &tx,
                    analysis_module,
                    package_id,
                    None,
                    &compiled,
                    &cmt_hash,
                    &source_file_path,
                ) {
                    Ok(count) => total_modules += count,
                    Err(e) => eprintln!("Failed to insert module {module_name}: {e}"),
                }
            }
            // Second pass: insert usages now that all modules exist in the DB
            let module_id_map = match build_module_id_map(&tx) {
                Ok(map) => map,
                Err(e) => {
                    eprintln!("Failed to build module ID map: {e}");
                    // tx drops here and auto-rolls back
                    return 1;
                }
            };
            for analysis_module in &analysis_output {
                if let Err(e) = insert_usages(&tx, analysis_module, &module_id_map) {
                    eprintln!("Failed to insert usages for {}: {e}", analysis_module.module_name);
                }
            }
            if let Err(e) = mark_auto_opened(&tx, &build_state) {
                eprintln!("Failed to mark auto-opened modules: {e}");
            }
            if let Err(e) = tx.commit() {
                eprintln!("Failed to commit transaction: {e}");
                return 1;
            }
        }
        Err(e) => {
            eprintln!("Analysis binary failed: {e}");
            return 1;
        }
    }

    let elapsed = timing.elapsed();
    if show_progress {
        eprintln!(
            "\nSync completed in {:.2}s — {total_modules} modules indexed to {}",
            elapsed.as_secs_f64(),
            db_path.display()
        );
    }

    0
}
