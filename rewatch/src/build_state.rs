use crate::build::build_types::BuildCommandState;
use anyhow::{Context, Result, anyhow};
use sqlx::sqlite::SqliteConnectOptions;
use sqlx::{Connection, Executor, SqliteConnection};
use std::collections::BTreeSet;
use std::fs;
use std::future::Future;
use std::path::{Path, PathBuf};
use tokio::runtime::Builder;

const BUILD_STATE_DB: &str = "build.sqlite";
const MODULES_TABLE: &str = "modules";
const MODULES_NAME_INDEX: &str = "idx_modules_name";
const MODULE_EDGES_TABLE: &str = "module_edges";
const MODULE_EDGES_PAIR_INDEX: &str = "idx_module_edges_from_to";
const MODULE_EDGES_TO_INDEX: &str = "idx_module_edges_to_name";

fn block_on<F, T>(future: F) -> Result<T>
where
    F: Future<Output = Result<T>>,
{
    let runtime = Builder::new_current_thread()
        .enable_all()
        .build()
        .context("Failed to create SQLite runtime")?;

    runtime.block_on(future)
}

fn get_db_path(project_folder: &Path) -> PathBuf {
    project_folder.join("lib").join(BUILD_STATE_DB)
}

fn collect_module_names(build_state: &BuildCommandState) -> BTreeSet<String> {
    let mut module_names = BTreeSet::new();

    for (module_name, module) in &build_state.build_state.modules {
        module_names.insert(module_name.clone());
        module_names.extend(module.deps.iter().cloned());
        module_names.extend(module.dependents.iter().cloned());
    }

    module_names
}

fn collect_edges(build_state: &BuildCommandState) -> BTreeSet<(String, String)> {
    let mut edges = BTreeSet::new();

    for (module_name, module) in &build_state.build_state.modules {
        for dep in &module.deps {
            edges.insert((module_name.clone(), dep.clone()));
        }
    }

    edges
}

async fn flush_sqlite(build_state: &BuildCommandState, db_path: &Path) -> Result<()> {
    let module_names = collect_module_names(build_state);
    let edges = collect_edges(build_state);
    let options = SqliteConnectOptions::new()
        .filename(db_path)
        .create_if_missing(true)
        .foreign_keys(true);
    let mut connection = SqliteConnection::connect_with(&options)
        .await
        .with_context(|| format!("Failed to open build state database at {}", db_path.display()))?;
    let mut transaction = connection.begin().await?;

    transaction
        .execute(format!("CREATE TABLE {MODULES_TABLE} (name TEXT NOT NULL)").as_str())
        .await?;
    transaction
        .execute(format!("CREATE UNIQUE INDEX {MODULES_NAME_INDEX} ON {MODULES_TABLE} (name)").as_str())
        .await?;
    transaction
        .execute(
            format!(
                "CREATE TABLE {MODULE_EDGES_TABLE} (
                    from_name TEXT NOT NULL,
                    to_name TEXT NOT NULL,
                    FOREIGN KEY (from_name) REFERENCES {MODULES_TABLE}(name) ON DELETE CASCADE,
                    FOREIGN KEY (to_name) REFERENCES {MODULES_TABLE}(name) ON DELETE CASCADE
                )"
            )
            .as_str(),
        )
        .await?;
    transaction
        .execute(
            format!(
                "CREATE UNIQUE INDEX {MODULE_EDGES_PAIR_INDEX} ON {MODULE_EDGES_TABLE} (from_name, to_name)"
            )
            .as_str(),
        )
        .await?;
    transaction
        .execute(format!("CREATE INDEX {MODULE_EDGES_TO_INDEX} ON {MODULE_EDGES_TABLE} (to_name)").as_str())
        .await?;

    for module_name in module_names {
        sqlx::query(&format!("INSERT INTO {MODULES_TABLE} (name) VALUES (?)"))
            .bind(module_name)
            .execute(&mut *transaction)
            .await?;
    }

    for (from_name, to_name) in edges {
        sqlx::query(&format!(
            "INSERT INTO {MODULE_EDGES_TABLE} (from_name, to_name) VALUES (?, ?)"
        ))
        .bind(from_name)
        .bind(to_name)
        .execute(&mut *transaction)
        .await?;
    }

    transaction.commit().await?;

    Ok(())
}

pub fn flush(build_state: &BuildCommandState, folder: &str) -> Result<()> {
    let project_folder = Path::new(folder);
    if !project_folder.exists() {
        return Err(anyhow!("Build folder does not exist"));
    }

    let lib_dir = project_folder.join("lib");
    fs::create_dir_all(&lib_dir)
        .with_context(|| format!("Failed to create build output directory at {}", lib_dir.display()))?;

    let db_path = get_db_path(project_folder);
    if db_path.exists() {
        fs::remove_file(&db_path).with_context(|| {
            format!(
                "Failed to remove previous build state database at {}",
                db_path.display()
            )
        })?;
    }

    block_on(flush_sqlite(build_state, &db_path))
}

#[cfg(test)]
mod tests {
    use super::{
        BUILD_STATE_DB, MODULE_EDGES_PAIR_INDEX, MODULE_EDGES_TABLE, MODULE_EDGES_TO_INDEX,
        MODULES_NAME_INDEX, MODULES_TABLE, block_on, flush, get_db_path,
    };
    use crate::build::build_types::{
        BuildCommandState, CompileState, CompilerInfo, Implementation, Module, ParseState, SourceFile,
        SourceType,
    };
    use crate::project_context::ProjectContext;
    use ahash::{AHashMap, AHashSet};
    use anyhow::{Context, Result};
    use blake3::hash;
    use sqlx::{Connection, Row, SqliteConnection};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::SystemTime;
    use tempfile::TempDir;

    fn write_rescript_config(root: &Path) -> Result<()> {
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("rescript.json"),
            r#"{
  "name": "test-project",
  "sources": ["src"]
}"#,
        )?;
        Ok(())
    }

    fn make_module(name: &str, deps: &[&str], dependents: &[&str]) -> Module {
        Module {
            source_type: SourceType::SourceFile(SourceFile {
                implementation: Implementation {
                    path: PathBuf::from(format!("src/{name}.res")),
                    parse_state: ParseState::Success,
                    compile_state: CompileState::Success,
                    last_modified: SystemTime::UNIX_EPOCH,
                    parse_dirty: false,
                    compile_warnings: None,
                },
                interface: None,
            }),
            deps: deps.iter().map(|dep| dep.to_string()).collect(),
            dependents: dependents.iter().map(|dependent| dependent.to_string()).collect(),
            package_name: "test-project".to_string(),
            compile_dirty: false,
            last_compiled_cmi: None,
            last_compiled_cmt: None,
            deps_dirty: false,
            is_type_dev: false,
        }
    }

    fn make_build_state(root: &Path) -> Result<BuildCommandState> {
        let project_context = ProjectContext::new(root)?;
        Ok(BuildCommandState::new(
            root.to_path_buf(),
            project_context,
            AHashMap::new(),
            CompilerInfo {
                bsc_path: root.join("bsc.exe"),
                bsc_hash: hash(b"bsc"),
                runtime_path: root.join("runtime"),
            },
            None,
        ))
    }

    async fn read_string_column(db_path: &Path, query: &str, column: &str) -> Result<Vec<String>> {
        let mut connection = SqliteConnection::connect(db_path.to_string_lossy().as_ref()).await?;
        let rows = sqlx::query(query).fetch_all(&mut connection).await?;

        Ok(rows.into_iter().map(|row| row.get(column)).collect())
    }

    async fn read_edge_rows(db_path: &Path) -> Result<Vec<(String, String)>> {
        let mut connection = SqliteConnection::connect(db_path.to_string_lossy().as_ref()).await?;
        let rows = sqlx::query(&format!(
            "SELECT from_name, to_name FROM {MODULE_EDGES_TABLE} ORDER BY from_name, to_name"
        ))
        .fetch_all(&mut connection)
        .await?;

        Ok(rows
            .into_iter()
            .map(|row| (row.get("from_name"), row.get("to_name")))
            .collect())
    }

    #[test]
    fn flush_writes_modules_edges_and_indexes() -> Result<()> {
        let temp_dir = TempDir::new().context("temp dir should be created")?;
        write_rescript_config(temp_dir.path())?;

        let mut build_state = make_build_state(temp_dir.path())?;
        build_state.insert_module("A", make_module("A", &["B", "C"], &[]));
        build_state.insert_module("B", make_module("B", &[], &["A", "C"]));
        build_state.insert_module("C", make_module("C", &["B"], &["A"]));

        flush(&build_state, temp_dir.path().to_string_lossy().as_ref())?;

        let db_path = get_db_path(temp_dir.path());
        assert_eq!(
            db_path.file_name().and_then(|name| name.to_str()),
            Some(BUILD_STATE_DB)
        );
        assert!(
            db_path.exists(),
            "expected SQLite snapshot at {}",
            db_path.display()
        );

        let modules = block_on(read_string_column(
            &db_path,
            &format!("SELECT name FROM {MODULES_TABLE} ORDER BY name"),
            "name",
        ))?;
        assert_eq!(modules, vec!["A", "B", "C"]);

        let edges = block_on(read_edge_rows(&db_path))?;
        assert_eq!(
            edges,
            vec![
                ("A".to_string(), "B".to_string()),
                ("A".to_string(), "C".to_string()),
                ("C".to_string(), "B".to_string()),
            ]
        );

        let dependents_of_b = block_on(read_string_column(
            &db_path,
            &format!("SELECT from_name FROM {MODULE_EDGES_TABLE} WHERE to_name = 'B' ORDER BY from_name"),
            "from_name",
        ))?;
        assert_eq!(dependents_of_b, vec!["A", "C"]);

        let module_indexes = block_on(read_string_column(
            &db_path,
            "PRAGMA index_list('modules')",
            "name",
        ))?;
        assert!(
            module_indexes.contains(&MODULES_NAME_INDEX.to_string()),
            "expected module name index, found {module_indexes:?}"
        );

        let edge_indexes = block_on(read_string_column(
            &db_path,
            "PRAGMA index_list('module_edges')",
            "name",
        ))?;
        assert!(
            edge_indexes.contains(&MODULE_EDGES_PAIR_INDEX.to_string()),
            "expected dependency edge index, found {edge_indexes:?}"
        );
        assert!(
            edge_indexes.contains(&MODULE_EDGES_TO_INDEX.to_string()),
            "expected dependent lookup index, found {edge_indexes:?}"
        );

        Ok(())
    }

    #[test]
    fn flush_recreates_the_database_for_new_state() -> Result<()> {
        let temp_dir = TempDir::new().context("temp dir should be created")?;
        write_rescript_config(temp_dir.path())?;

        let mut build_state = make_build_state(temp_dir.path())?;
        build_state.insert_module("A", make_module("A", &["B"], &[]));
        build_state.insert_module("B", make_module("B", &[], &["A"]));

        flush(&build_state, temp_dir.path().to_string_lossy().as_ref())?;

        build_state.build_state.modules = AHashMap::new();
        build_state.build_state.module_names = AHashSet::new();
        build_state.insert_module("X", make_module("X", &["Y"], &[]));
        build_state.insert_module("Y", make_module("Y", &[], &["X"]));

        flush(&build_state, temp_dir.path().to_string_lossy().as_ref())?;

        let db_path = get_db_path(temp_dir.path());
        let modules = block_on(read_string_column(
            &db_path,
            &format!("SELECT name FROM {MODULES_TABLE} ORDER BY name"),
            "name",
        ))?;
        assert_eq!(modules, vec!["X", "Y"]);

        let edges = block_on(read_edge_rows(&db_path))?;
        assert_eq!(edges, vec![("X".to_string(), "Y".to_string())]);

        Ok(())
    }
}
