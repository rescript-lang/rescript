use std::path::Path;

use ahash::AHashSet;
use rayon::prelude::*;
use std::fs;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::config;
use crate::helpers;
use crate::project_context::ProjectContext;

use super::packages;

/// Format content from stdin with the given extension (.res or .resi).
/// Returns the formatted content or an error message.
pub fn format_stdin(stdin_content: &str, ext: &str) -> Result<String, String> {
    use std::io::Write;

    let bsc_path = helpers::get_bsc();

    let mut temp_file = tempfile::Builder::new()
        .suffix(ext)
        .tempfile()
        .map_err(|e| format!("Failed to create temp file: {}", e))?;

    temp_file
        .write_all(stdin_content.as_bytes())
        .map_err(|e| format!("Failed to write to temp file: {}", e))?;

    let temp_path = temp_file.path();

    let output = Command::new(&bsc_path)
        .arg("-format")
        .arg(temp_path)
        .output()
        .map_err(|e| format!("Failed to run bsc: {}", e))?;

    if output.status.success() {
        String::from_utf8(output.stdout).map_err(|e| format!("Invalid UTF-8 in output: {}", e))
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

/// Format a list of files in parallel.
///
/// - `files`: absolute paths to .res/.resi files
/// - `check`: if true, only check (don't write), report mismatches
/// - `on_check_failed`: called when a file fails the format check (check mode only)
///
/// Returns `(formatted_count, failed_count, Option<error_string>)`.
/// `error_string` is set if `bsc -format` fails on a file (terminal error).
pub fn format_files(
    files: &[String],
    check: bool,
    on_check_failed: &(dyn Fn(&str) + Send + Sync),
) -> (usize, usize, Option<String>) {
    let bsc_path = helpers::get_bsc();
    let batch_size = 4 * num_cpus::get();
    let formatted_count = AtomicUsize::new(0);
    let failed_count = AtomicUsize::new(0);

    let result: Result<(), String> = files.par_chunks(batch_size).try_for_each(|batch| {
        batch.iter().try_for_each(|file| {
            let output = Command::new(&bsc_path)
                .arg("-format")
                .arg(file)
                .output()
                .map_err(|e| format!("Failed to run bsc: {}", e))?;

            if output.status.success() {
                let original_content =
                    fs::read_to_string(file).map_err(|e| format!("Failed to read {}: {}", file, e))?;
                let formatted_content = String::from_utf8_lossy(&output.stdout);
                if original_content != formatted_content {
                    if check {
                        failed_count.fetch_add(1, Ordering::SeqCst);
                        on_check_failed(file);
                    } else {
                        let _file_span = tracing::info_span!("format.write_file", file = %file,).entered();

                        fs::write(file, &*formatted_content)
                            .map_err(|e| format!("Failed to write {}: {}", file, e))?;
                    }
                }
                formatted_count.fetch_add(1, Ordering::SeqCst);
            } else {
                let stderr_str = String::from_utf8_lossy(&output.stderr);
                return Err(format!("Error formatting {}: {}", file, stderr_str));
            }
            Ok(())
        })
    });

    (
        formatted_count.load(Ordering::SeqCst),
        failed_count.load(Ordering::SeqCst),
        result.err(),
    )
}

/// Collect all .res/.resi files for formatting without requiring a BuildState.
/// This is the lightweight standalone alternative to FormatCollectFiles in the work queue.
///
/// - `working_directory`: the cwd from which format was invoked
/// - `root`: the project/workspace root (from find_project_root)
pub fn collect_format_files(working_directory: &Path, root: &Path) -> Vec<String> {
    let project_context = match ProjectContext::new(root) {
        Ok(ctx) => ctx,
        Err(e) => {
            tracing::debug!(error = %e, "Failed to create project context for standalone format");
            return Vec::new();
        }
    };

    // Determine which packages to format
    let local_packages = project_context.get_scoped_local_packages();

    let scope_package =
        helpers::get_scope_package_from_working_dir(working_directory, project_context.get_root_path());
    let packages_to_format: AHashSet<String> = match scope_package {
        Some(ref pkg) if local_packages.contains(pkg) => {
            let mut set = AHashSet::new();
            set.insert(pkg.clone());
            set
        }
        _ => local_packages,
    };

    let mut files: Vec<String> = Vec::new();

    for package_name in &packages_to_format {
        // Resolve the package path
        let package_path = if *package_name == project_context.current_config.name {
            project_context.get_root_path().to_path_buf()
        } else {
            // Local dependency: resolve via node_modules symlink
            let candidate = project_context
                .get_root_path()
                .join("node_modules")
                .join(package_name);
            match candidate.canonicalize() {
                Ok(p) => p,
                Err(_) => continue,
            }
        };

        let pkg_config = match packages::read_config(&package_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        // Get source dirs from config
        let source_folders: AHashSet<config::PackageSource> = match pkg_config.sources {
            Some(config::OneOrMore::Single(source)) => packages::get_source_dirs(source, None),
            Some(config::OneOrMore::Multiple(sources)) => {
                let mut folders = AHashSet::new();
                for source in sources {
                    folders.extend(packages::get_source_dirs(source, None));
                }
                folders
            }
            None => continue,
        };

        // Walk each source dir for .res/.resi files
        for source in &source_folders {
            let source_files = packages::get_source_files(
                package_name,
                &package_path,
                &None, // no filter
                source,
                true, // include dev sources for formatting
            );
            for (relative_path, _meta) in source_files {
                if let Some(ext) = relative_path.extension()
                    && (ext == "res" || ext == "resi")
                {
                    files.push(package_path.join(&relative_path).to_string_lossy().into_owned());
                }
            }
        }
    }

    files
}
