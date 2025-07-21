use crate::helpers;
use anyhow::{Result, bail};
use num_cpus;
use rayon::prelude::*;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::build::packages;
use crate::cli::FileExtension;
use clap::ValueEnum;

pub fn format(
    stdin_extension: Option<FileExtension>,
    all: bool,
    check: bool,
    files: Vec<String>,
) -> Result<()> {
    let bsc_path = helpers::get_bsc();

    match stdin_extension {
        Some(extension) => {
            format_stdin(&bsc_path, extension)?;
        }
        None => {
            let files = if all { get_all_files()? } else { files };
            format_files(&bsc_path, files, check)?;
        }
    }

    Ok(())
}

fn get_all_files() -> Result<Vec<String>> {
    let current_dir = std::env::current_dir()?;
    let project_root = helpers::get_abs_path(&current_dir);
    let workspace_root_option = helpers::get_workspace_root(&project_root);

    let build_state = packages::make(&None, &project_root, &workspace_root_option, false, false)?;
    let mut files: Vec<String> = Vec::new();

    for (_package_name, package) in build_state {
        if let Some(source_files) = package.source_files {
            for (path, _metadata) in source_files {
                if let Some(extension) = path.extension() {
                    if extension == "res" || extension == "resi" {
                        files.push(package.path.join(path).to_string_lossy().into_owned());
                    }
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

    let mut temp_file = tempfile::Builder::new()
        .suffix(extension_value.get_name())
        .tempfile()?;
    io::copy(&mut io::stdin(), &mut temp_file)?;
    let temp_path = temp_file.path();

    let mut cmd = Command::new(bsc_exe);
    cmd.arg("-format").arg(temp_path);

    let output = cmd.output()?;

    if output.status.success() {
        io::stdout().write_all(&output.stdout)?;
    } else {
        let stderr_str = String::from_utf8_lossy(&output.stderr);
        bail!("Error formatting stdin: {}", stderr_str);
    }

    Ok(())
}

fn format_files(bsc_exe: &Path, files: Vec<String>, check: bool) -> Result<()> {
    let batch_size = 4 * num_cpus::get();
    let incorrectly_formatted_files = AtomicUsize::new(0);

    files.par_chunks(batch_size).try_for_each(|batch| {
        batch.iter().try_for_each(|file| {
            let mut cmd = Command::new(bsc_exe);
            if check {
                cmd.arg("-format").arg(file);
            } else {
                cmd.arg("-o").arg(file).arg("-format").arg(file);
            }

            let output = cmd.output()?;

            if output.status.success() {
                if check {
                    let original_content = fs::read_to_string(file)?;
                    let formatted_content = String::from_utf8_lossy(&output.stdout);
                    if original_content != formatted_content {
                        eprintln!("[format check] {file}");
                        incorrectly_formatted_files.fetch_add(1, Ordering::SeqCst);
                    }
                }
            } else {
                let stderr_str = String::from_utf8_lossy(&output.stderr);
                bail!("Error formatting {}: {}", file, stderr_str);
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
