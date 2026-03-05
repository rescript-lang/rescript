use crate::{helpers, project_context};
use anyhow::{Result, bail};
use num_cpus;
use rayon::prelude::*;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use tracing::{info_span, instrument};

use crate::build::packages;
use crate::cli::FileExtension;
use clap::ValueEnum;

/// On Windows, bsc writes CRLF to stdout (text mode).
/// Match the line ending style of the original source to avoid unwanted conversions.
fn normalize_line_endings(source: &str, formatted: &[u8]) -> String {
    let raw = String::from_utf8_lossy(formatted);
    if !source.contains("\r\n") && raw.contains("\r\n") {
        raw.replace("\r\n", "\n")
    } else {
        raw.into_owned()
    }
}

/// Format source code by piping it to `bsc -bs-read-stdin -format`.
///
/// `filename` is used by bsc for extension detection (`.res` vs `.resi`)
/// and error locations — the file doesn't need to exist on disk.
///
/// On Windows, bsc writes CRLF to stdout. This function matches the line
/// ending style of `source` to avoid unwanted conversions.
pub fn format_source(bsc_exe: &Path, source: &str, filename: &str) -> Result<String> {
    let mut child = Command::new(bsc_exe)
        .args(["-bs-read-stdin", "-format", filename])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(source.as_bytes())?;
    }

    let output = child.wait_with_output()?;

    if !output.status.success() {
        let stderr_str = String::from_utf8_lossy(&output.stderr);
        bail!("Error formatting {filename}: {stderr_str}");
    }

    Ok(normalize_line_endings(source, &output.stdout))
}

#[instrument(name = "format.format", skip_all)]
pub fn format(stdin_extension: Option<FileExtension>, check: bool, files: Vec<String>) -> Result<()> {
    let bsc_path = helpers::get_bsc();

    match stdin_extension {
        Some(extension) => {
            format_stdin(&bsc_path, extension)?;
        }
        None => {
            let files = if files.is_empty() {
                get_files_in_scope()?
            } else {
                files
            };
            format_files(&bsc_path, files, check)?;
        }
    }

    Ok(())
}

fn get_files_in_scope() -> Result<Vec<String>> {
    let current_dir = std::env::current_dir()?;
    let project_context = project_context::ProjectContext::new(&current_dir)?;

    let packages = packages::make(&None, &project_context, false)?;
    let mut files: Vec<String> = Vec::new();
    let packages_to_format = project_context.get_scoped_local_packages();

    for (_package_name, package) in packages {
        if packages_to_format.contains(&package.name)
            && let Some(source_files) = &package.source_files
        {
            for (path, _metadata) in source_files {
                if let Some(extension) = path.extension()
                    && (extension == "res" || extension == "resi")
                {
                    files.push(package.path.join(path).to_string_lossy().into_owned());
                }
            }
        }
    }
    Ok(files)
}

fn format_stdin(bsc_exe: &Path, extension: FileExtension) -> Result<()> {
    let extension_value = extension
        .to_possible_value()
        .ok_or(anyhow::anyhow!("Could not get extension arg value"))?;

    let dummy_filename = format!("stdin{}", extension_value.get_name());

    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;

    let formatted = format_source(bsc_exe, &source, &dummy_filename)?;
    io::stdout().write_all(formatted.as_bytes())?;

    Ok(())
}

fn format_files(bsc_exe: &Path, files: Vec<String>, check: bool) -> Result<()> {
    let batch_size = 4 * num_cpus::get();
    let incorrectly_formatted_files = AtomicUsize::new(0);

    files.par_chunks(batch_size).try_for_each(|batch| {
        batch.iter().try_for_each(|file| -> Result<()> {
            let original_content = fs::read_to_string(file)?;

            let output = Command::new(bsc_exe).args(["-format", file]).output()?;

            if !output.status.success() {
                let stderr_str = String::from_utf8_lossy(&output.stderr);
                bail!("Error formatting {file}: {stderr_str}");
            }

            let formatted_content = normalize_line_endings(&original_content, &output.stdout);

            if original_content != formatted_content {
                if check {
                    eprintln!("[format check] {file}");
                    incorrectly_formatted_files.fetch_add(1, Ordering::SeqCst);
                } else {
                    let _file_span = info_span!("format.write_file", file = %file).entered();
                    fs::write(file, &formatted_content)?;
                }
            }
            Ok(())
        })
    })?;

    let count = incorrectly_formatted_files.load(Ordering::SeqCst);
    if count > 0 {
        if count == 1 {
            eprintln!("The file listed above needs formatting");
        } else {
            eprintln!("The {count} files listed above need formatting");
        }
        bail!("Formatting check failed");
    }

    Ok(())
}
