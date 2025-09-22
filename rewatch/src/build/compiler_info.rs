use crate::helpers;

use super::build_types::{BuildState, CompilerInfo};
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

pub fn write_compiler_info(build_state: &BuildState) {
    let bsc_path = build_state.compiler_info.bsc_path.to_string_lossy().to_string();
    let bsc_hash = build_state.compiler_info.bsc_hash.to_hex().to_string();
    let runtime_path = build_state
        .compiler_info
        .runtime_path
        .to_string_lossy()
        .to_string();
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
