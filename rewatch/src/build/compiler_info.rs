use crate::helpers;

use super::build_types::{BuildCommandState, CompilerInfo};
use super::packages;
use super::{clean, logs};
use ahash::AHashMap;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::Write;

// In order to have a loose coupling with the compiler, we don't want to have a hard dependency on the compiler's structs
// We can use this struct to parse the compiler-info.json file
// If something is not there, that is fine, we will treat it as a mismatch
#[derive(Serialize, Deserialize)]
struct CompilerInfoFile {
    version: String,
    bsc_path: String,
    bsc_hash: String,
    rescript_config_hash: String,
    source_map_args: Vec<String>,
    runtime_path: String,
    generated_at: String,
}

pub enum CompilerCheckResult {
    SameCompilerAsLastRun,
    CleanedPackagesDueToCompiler,
}

fn get_rescript_config_hash(package: &packages::Package) -> Option<String> {
    helpers::compute_file_hash(&package.config.path).map(|hash| hash.to_hex().to_string())
}

pub fn verify_compiler_info(
    packages: &AHashMap<String, packages::Package>,
    compiler: &CompilerInfo,
    source_map_args: &[String],
) -> CompilerCheckResult {
    let mismatched_packages = packages
        .values()
        .filter(|package| {
            let info_path = package.get_compiler_info_path();
            let Ok(contents) = std::fs::read_to_string(&info_path) else {
                // Can't read the compiler-info.json file, maybe there is no current build.
                // We check if the ocaml build folder exists, if not, we assume the compiler is not installed
                return logs::does_ocaml_build_compiler_log_exist(package);
            };

            let parsed: Result<CompilerInfoFile, _> = serde_json::from_str(&contents);
            let parsed = match parsed {
                Ok(p) => p,
                Err(_) => return true, // unknown or invalid format -> treat as mismatch
            };

            let current_bsc_path_str = compiler.bsc_path.to_string_lossy();
            let current_bsc_hash_hex = compiler.bsc_hash.to_hex().to_string();
            let current_runtime_path_str = compiler.runtime_path.to_string_lossy();
            let current_rescript_config_hash = match get_rescript_config_hash(package) {
                Some(hash) => hash,
                None => return true, // can't compute hash -> treat as mismatch
            };

            let mut mismatch = false;
            if parsed.bsc_path != current_bsc_path_str {
                log::debug!(
                    "compiler-info mismatch for {}: bsc_path changed (stored='{}', current='{}')",
                    package.name,
                    parsed.bsc_path,
                    current_bsc_path_str
                );
                mismatch = true;
            }
            if parsed.bsc_hash != current_bsc_hash_hex {
                log::debug!(
                    "compiler-info mismatch for {}: bsc_hash changed (stored='{}', current='{}')",
                    package.name,
                    parsed.bsc_hash,
                    current_bsc_hash_hex
                );
                mismatch = true;
            }
            if parsed.runtime_path != current_runtime_path_str {
                log::debug!(
                    "compiler-info mismatch for {}: runtime_path changed (stored='{}', current='{}')",
                    package.name,
                    parsed.runtime_path,
                    current_runtime_path_str
                );
                mismatch = true;
            }
            if parsed.rescript_config_hash != current_rescript_config_hash {
                log::debug!(
                    "compiler-info mismatch for {}: rescript_config_hash changed (stored='{}', current='{}')",
                    package.name,
                    parsed.rescript_config_hash,
                    current_rescript_config_hash
                );
                mismatch = true;
            }
            if parsed.source_map_args != source_map_args {
                log::debug!(
                    "compiler-info mismatch for {}: source_map_args changed (stored={:?}, current={:?})",
                    package.name,
                    parsed.source_map_args,
                    source_map_args
                );
                mismatch = true;
            }

            mismatch
        })
        .collect::<Vec<_>>();

    let cleaned_count = mismatched_packages.len();
    mismatched_packages.par_iter().for_each(|package| {
        // suppress progress printing during init to avoid breaking step output
        clean::clean_package(false, true, package);
    });
    if cleaned_count == 0 {
        CompilerCheckResult::SameCompilerAsLastRun
    } else {
        CompilerCheckResult::CleanedPackagesDueToCompiler
    }
}

pub fn write_compiler_info(build_state: &BuildCommandState) {
    let bsc_path = build_state.compiler_info.bsc_path.to_string_lossy().to_string();
    let bsc_hash = build_state.compiler_info.bsc_hash.to_hex().to_string();
    let runtime_path = build_state
        .compiler_info
        .runtime_path
        .to_string_lossy()
        .to_string();
    let source_map_args = build_state
        .get_root_config()
        .get_source_map_args(build_state.source_map_command);
    // derive version from the crate version
    let version = env!("CARGO_PKG_VERSION").to_string();
    let generated_at = crate::helpers::get_system_time().to_string();

    // Borrowing serializer to avoid cloning the constant fields for every package
    #[derive(Serialize)]
    struct CompilerInfoFileRef<'a> {
        version: &'a str,
        bsc_path: &'a str,
        bsc_hash: &'a str,
        rescript_config_hash: String,
        source_map_args: &'a [String],
        runtime_path: &'a str,
        generated_at: &'a str,
    }

    build_state.packages.values().par_bridge().for_each(|package| {
        if let Some(rescript_config_hash) = helpers::compute_file_hash(&package.config.path) {
            let out = CompilerInfoFileRef {
                version: &version,
                bsc_path: &bsc_path,
                bsc_hash: &bsc_hash,
                rescript_config_hash: rescript_config_hash.to_hex().to_string(),
                source_map_args: &source_map_args,
                runtime_path: &runtime_path,
                generated_at: &generated_at,
            };
            let contents = match serde_json::to_string_pretty(&out) {
                Ok(s) => s,
                Err(err) => {
                    log::error!(
                        "Failed to serialize compiler-info for package {}: {}. Skipping write.",
                        package.name,
                        err
                    );
                    return;
                }
            };
            let info_path = package.get_compiler_info_path();
            let should_write = match std::fs::read_to_string(&info_path) {
                Ok(existing) => existing != contents,
                Err(_) => true,
            };

            if should_write {
                if let Some(parent) = info_path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }
                // We write atomically to avoid leaving a partially written JSON file
                // (e.g. process interruption) that would be read on the next init as an
                // invalid/mismatched compiler-info, causing unnecessary cleans. The
                // rename within the same directory is atomic on common platforms.
                let tmp = info_path.with_extension("json.tmp");
                if let Ok(mut f) = File::create(&tmp) {
                    if let Err(err) = f.write_all(contents.as_bytes()) {
                        log::error!(
                            "Failed to write compiler-info for package {} to temporary file {}: {}. Skipping rename.",
                            package.name,
                            tmp.display(),
                            err
                        );
                        let _ = std::fs::remove_file(&tmp);
                        return;
                    }
                    if let Err(err) = f.sync_all() {
                        log::error!(
                            "Failed to flush compiler-info for package {}: {}. Skipping rename.",
                            package.name,
                            err
                        );
                        let _ = std::fs::remove_file(&tmp);
                        return;
                    }
                    if let Err(err) = std::fs::rename(&tmp, &info_path) {
                        log::error!(
                            "Failed to atomically replace compiler-info for package {}: {}.",
                            package.name,
                            err
                        );
                        let _ = std::fs::remove_file(&tmp);
                    }
                }
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build::packages::{Namespace, Package};
    use crate::config;
    use ahash::{AHashMap, AHashSet};
    use serde_json::json;
    use std::fs;
    use std::path::Path;
    use tempfile::TempDir;

    fn test_compiler(root: &Path) -> CompilerInfo {
        CompilerInfo {
            bsc_path: root.join("bsc"),
            bsc_hash: blake3::hash(b"test-bsc"),
            runtime_path: root.join("runtime"),
        }
    }

    fn test_package(root: &Path, name: &str) -> Package {
        let package_path = root.join(name);
        fs::create_dir_all(&package_path).expect("package directory should be created");
        let config_path = package_path.join("rescript.json");
        fs::write(&config_path, format!(r#"{{"name":"{name}"}}"#)).expect("rescript.json should be written");

        Package {
            name: name.to_string(),
            config: config::tests::create_config(config::tests::CreateConfigArgs {
                name: name.to_string(),
                bs_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: None,
                path: config_path,
            }),
            source_folders: AHashSet::new(),
            source_files: None,
            namespace: Namespace::NoNamespace,
            modules: None,
            path: package_path,
            dirs: None,
            gentype_dirs: None,
            is_local_dep: true,
            is_root: false,
        }
    }

    fn write_test_compiler_info(package: &Package, compiler: &CompilerInfo, source_map_args: Vec<&str>) {
        fs::create_dir_all(package.get_build_path()).expect("build directory should be created");
        fs::create_dir_all(package.get_ocaml_build_path()).expect("ocaml build directory should be created");

        let rescript_config_hash =
            get_rescript_config_hash(package).expect("rescript config hash should be available");
        let contents = json!({
            "version": env!("CARGO_PKG_VERSION"),
            "bsc_path": compiler.bsc_path.to_string_lossy().to_string(),
            "bsc_hash": compiler.bsc_hash.to_hex().to_string(),
            "rescript_config_hash": rescript_config_hash,
            "source_map_args": source_map_args,
            "runtime_path": compiler.runtime_path.to_string_lossy().to_string(),
            "generated_at": "test",
        });

        fs::write(
            package.get_compiler_info_path(),
            serde_json::to_string_pretty(&contents).expect("compiler info should serialize"),
        )
        .expect("compiler info should be written");
    }

    fn packages_map(package: Package) -> AHashMap<String, Package> {
        let mut packages = AHashMap::new();
        packages.insert(package.name.clone(), package);
        packages
    }

    #[test]
    fn verify_compiler_info_keeps_package_when_source_map_args_match() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let compiler = test_compiler(temp_dir.path());
        let package = test_package(temp_dir.path(), "dep");
        let build_path = package.get_build_path();
        let source_map_args = vec!["-bs-source-map".to_string(), "linked".to_string()];
        write_test_compiler_info(&package, &compiler, vec!["-bs-source-map", "linked"]);

        let result = verify_compiler_info(&packages_map(package), &compiler, &source_map_args);

        assert!(matches!(result, CompilerCheckResult::SameCompilerAsLastRun));
        assert!(build_path.exists());
    }

    #[test]
    fn verify_compiler_info_cleans_package_when_source_map_args_change() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let compiler = test_compiler(temp_dir.path());
        let package = test_package(temp_dir.path(), "dep");
        let build_path = package.get_build_path();
        let source_map_args = vec!["-bs-source-map".to_string(), "linked".to_string()];
        write_test_compiler_info(&package, &compiler, vec!["-bs-source-map", "false"]);

        let result = verify_compiler_info(&packages_map(package), &compiler, &source_map_args);

        assert!(matches!(
            result,
            CompilerCheckResult::CleanedPackagesDueToCompiler
        ));
        assert!(!build_path.exists());
    }
}
