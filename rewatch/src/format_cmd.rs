use std::sync::atomic::{AtomicUsize, Ordering};
use anyhow::{bail, Result};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::io::{self, Read, Write};
use std::fs;
use rayon::prelude::*;
use num_cpus;

pub fn run(
    stdin_path: Option<String>,
    all: bool,
    check: bool,
    files: Vec<String>,
    bsc_path_arg: Option<PathBuf>,
) -> Result<()> {
    let bsc_exe = match bsc_path_arg {
        Some(path) => path,
        None => find_bsc_exe()?,
    };

    if check && stdin_path.is_some() {
        bail!("format -stdin cannot be used with -check flag");
    }

    if all {
        if stdin_path.is_some() || !files.is_empty() {
            bail!("format -all can not be in use with other flags");
        }
        format_all(&bsc_exe, check)?;
    } else if stdin_path.is_some() {
        format_stdin(&bsc_exe, stdin_path.unwrap())?;
    } else {
        format_files(&bsc_exe, files, check)?;
    }

    Ok(())
}

fn find_bsc_exe() -> Result<PathBuf> {
    let current_exe = std::env::current_exe()?;
    let mut current_dir = current_exe.parent().unwrap_or_else(|| Path::new("/"));

    // Traverse up to find node_modules
    let node_modules_path = loop {
        let potential_path = current_dir.join("node_modules");
        if potential_path.exists() {
            break Some(potential_path);
        }
        if current_dir.parent().is_none() {
            break None;
        }
        current_dir = current_dir.parent().unwrap();
    }
    .ok_or_else(|| anyhow::anyhow!("Could not find node_modules directory"))?;

    let target = format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH);
    let bsc_path = node_modules_path
        .join("@rescript")
        .join(target)
        .join("bsc.exe");

    if !bsc_path.exists() {
        bail!("bsc executable not found at {}", bsc_path.display());
    }
    Ok(bsc_path)
}

fn format_all(bsc_exe: &Path, check: bool) -> Result<()> {
    let output = Command::new(std::env::current_exe()?)
        .arg("info")
        .arg("-list-files")
        .output()?;

    if !output.status.success() {
        io::stderr().write_all(&output.stderr)?;
        bail!("Failed to list files");
    }

    let files_str = String::from_utf8_lossy(&output.stdout);
    let files: Vec<String> = files_str
        .split('\n')
        .filter(|s| !s.trim().is_empty())
        .map(|s| s.trim().to_string())
        .collect();

    format_files(bsc_exe, files, check)?;
    Ok(())
}

fn format_stdin(bsc_exe: &Path, stdin_path: String) -> Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut cmd = Command::new(bsc_exe);
    cmd.arg("-format").arg(&stdin_path);
    cmd.stdin(std::process::Stdio::piped());
    cmd.stdout(std::process::Stdio::piped());
    cmd.stderr(std::process::Stdio::piped());

    let mut child = cmd.spawn()?;
    let mut stdin = child.stdin.take().unwrap();
    std::thread::spawn(move || {
        stdin.write_all(input.as_bytes()).unwrap();
    });

    let output = child.wait_with_output()?;

    if output.status.success() {
        io::stdout().write_all(&output.stdout)?;
    }
    else {
        io::stderr().write_all(&output.stderr)?;
        bail!("bsc exited with an error");
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
            }
            else {
                cmd.arg("-o").arg(file).arg("-format").arg(file);
            }

            let output = cmd.output()?;

            if output.status.success() {
                if check {
                    let original_content = fs::read_to_string(file)?;
                    let formatted_content = String::from_utf8_lossy(&output.stdout);
                    if original_content != formatted_content {
                        eprintln!("[format check] {}", file);
                        incorrectly_formatted_files.fetch_add(1, Ordering::SeqCst);
                    }
                }
            }
            else {
                io::stderr().write_all(&output.stderr)?;
                bail!("bsc exited with an error for file {}", file);
            }
            Ok(()) as Result<()>
        })
    })?;

    let count = incorrectly_formatted_files.load(Ordering::SeqCst);
    if count > 0 {
        if count == 1 {
            eprintln!("The file listed above needs formatting");
        }
        else {
            eprintln!(
                "The {} files listed above need formatting",
                count
            );
        }
        bail!("Formatting check failed");
    }

    Ok(())
}