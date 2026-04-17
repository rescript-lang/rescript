use anyhow::Result;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use sysinfo::{PidExt, ProcessExt, System, SystemExt};

use crate::queue::FifoQueue;
use crate::queue::*;

/* This locking mechanism is meant to never be deleted. Instead, it stores the PID of the process
 * that's running, when trying to aquire a lock, it checks wether that process is still running. If
 * not, it rewrites the lockfile to have its own PID instead. */

pub enum AwaitLockError {
    Watcher(notify::Error),
    Timeout(String),
}

impl std::fmt::Display for AwaitLockError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            AwaitLockError::Watcher(error) => format!("Error starting file watcher {}", error),
            AwaitLockError::Timeout(path) => format!("Timeout awaiting lockfile {}", path),
        };
        write!(f, "{msg}")
    }
}

pub enum Error {
    Locked(u32),
    AwaitingLockFile(AwaitLockError),
    ParsingLockfile(std::num::ParseIntError),
    ReadingLockfile(LockKind, std::io::Error),
    WritingLockfile(std::io::Error),
    ProjectFolderMissing(std::path::PathBuf),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            Error::Locked(pid) => {
                format!("A ReScript build is already running. The process ID (PID) is {pid}")
            }
            Error::ParsingLockfile(e) => format!(
                "Could not parse lockfile: \n {e} \n  (try removing it and running the command again)"
            ),
            Error::ReadingLockfile(kind, e) => {
                format!(
                    "Could not read lockfile: {}, \n {e} \n  (try removing it and running the command again)",
                    kind.file_name()
                )
            }
            Error::WritingLockfile(e) => format!("Could not write lockfile: \n {e}"),
            Error::ProjectFolderMissing(path) => format!(
                "Could not write lockfile because the specified project folder does not exist: {}",
                path.to_string_lossy()
            ),
            Error::AwaitingLockFile(await_lock_error) => {
                format!("Error awaiting lockfile: {await_lock_error}")
            }
        };
        write!(f, "{msg}")
    }
}

pub enum Lock {
    Aquired(u32),
    Error(Error),
}

fn matching_process_name() -> Option<String> {
    std::env::current_exe()
        .ok()
        .and_then(|path| path.file_name().map(|name| name.to_string_lossy().into_owned()))
}

fn pid_matches_current_process(to_check_pid: u32) -> bool {
    let system = System::new_all();
    let current_process_name = matching_process_name();

    system.processes().iter().any(|(pid, process)| {
        if pid.as_u32() != to_check_pid {
            return false;
        }

        match &current_process_name {
            Some(current_process_name) => process
                .exe()
                .file_name()
                .map(|name| name.to_string_lossy() == current_process_name.as_str())
                .unwrap_or_else(|| process.name() == current_process_name.as_str()),
            None => true,
        }
    })
}

#[derive(Clone, Copy)]
pub enum LockKind {
    Watch,
    Build,
}

impl LockKind {
    pub fn file_name(&self) -> String {
        String::from(match self {
            LockKind::Watch => "watch.lock",
            LockKind::Build => "build.lock",
        })
    }
}

pub const TIMEOUT_SECONDS: u64 = 60;

pub fn await_lock_deletion(location: &Path, kind: LockKind) -> Result<(), Error> {
    let now = SystemTime::now();
    let lock_path = location.join(kind.file_name());
    let queue = Arc::new(FifoQueue::<Result<Event, notify::Error>>::new());
    let producer = queue.clone();

    let mut watcher = RecommendedWatcher::new(move |res| producer.push(res), Config::default())
        .map_err(|e| Error::AwaitingLockFile(AwaitLockError::Watcher(e)))?;

    watcher
        .watch(location, RecursiveMode::NonRecursive)
        .map_err(|e| Error::AwaitingLockFile(AwaitLockError::Watcher(e)))?;

    loop {
        if !lock_path.exists() {
            return Ok(());
        }

        while !queue.is_empty() {
            match queue.pop() {
                Ok(Event {
                    kind: EventKind::Remove(_),
                    paths,
                    ..
                }) if paths
                    .iter()
                    .any(|path| path == &lock_path || path.ends_with(kind.file_name())) =>
                {
                    return Ok(());
                }
                Ok(_) | Err(_) => (),
            }
        }

        match now.elapsed() {
            Ok(elapsed) if elapsed < Duration::from_secs(TIMEOUT_SECONDS) => {
                std::thread::sleep(Duration::from_millis(50));
            }
            Ok(_) | Err(_) => {
                return Err(Error::AwaitingLockFile(AwaitLockError::Timeout(
                    lock_path.to_string_lossy().to_string(),
                )));
            }
        }
    }
}

pub fn get(kind: LockKind, folder: &str) -> Lock {
    let project_folder = Path::new(folder);
    if !project_folder.exists() {
        return Lock::Error(Error::ProjectFolderMissing(project_folder.to_path_buf()));
    }

    let lib_dir = project_folder.join("lib");
    let location = lib_dir.join(kind.file_name());
    let pid = process::id();

    // When a lockfile already exists we parse its PID: if the process is still alive we refuse to
    // proceed, otherwise we will overwrite the stale lock with our own PID.
    loop {
        match fs::read_to_string(&location) {
            Ok(contents) => match contents.parse::<u32>() {
                Ok(parsed_pid) if pid_matches_current_process(parsed_pid) => match kind {
                    LockKind::Build => {
                        println!("Waiting for other build to finish...");
                        match await_lock_deletion(&lib_dir, kind) {
                            Ok(_) => {
                                continue;
                            }
                            Err(_) => return Lock::Error(Error::Locked(parsed_pid)),
                        };
                    }
                    LockKind::Watch => return Lock::Error(Error::Locked(parsed_pid)),
                },
                Ok(_) => break,
                Err(e) => return Lock::Error(Error::ParsingLockfile(e)),
            },
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => break,
            Err(e) => return Lock::Error(Error::ReadingLockfile(kind, e)),
        }
    }

    if let Err(e) = fs::create_dir_all(&lib_dir) {
        return Lock::Error(Error::WritingLockfile(e));
    }

    // Rewrite the lockfile with our own PID.
    match File::create(&location) {
        Ok(mut file) => match file.write(pid.to_string().as_bytes()) {
            Ok(_) => Lock::Aquired(pid),
            Err(e) => Lock::Error(Error::WritingLockfile(e)),
        },
        Err(e) => Lock::Error(Error::WritingLockfile(e)),
    }
}

pub fn get_lock_or_exit(kind: LockKind, folder: &str) -> Lock {
    match get(kind, folder) {
        Lock::Error(error) => {
            eprintln!("Could not start Rescript build: {error}");
            std::process::exit(1);
        }

        acquired_lock => acquired_lock,
    }
}

pub fn drop_lock(kind: LockKind, folder: &str) -> Result<()> {
    let project_folder = Path::new(folder);
    if !project_folder.exists() {
        return Ok(());
    }

    let lib_dir = project_folder.join("lib");
    let location = lib_dir.join(kind.file_name());

    fs::remove_file(&location)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn returns_error_when_project_folder_missing() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let missing_folder = temp_dir.path().join("missing_project");

        match get(
            LockKind::Watch,
            missing_folder.to_str().expect("path should be valid"),
        ) {
            Lock::Error(Error::ProjectFolderMissing(path)) => {
                assert_eq!(path, missing_folder);
            }
            _ => panic!("expected ProjectFolderMissing error"),
        }

        assert!(
            !missing_folder.exists(),
            "missing project folder should not be created"
        );
    }

    #[test]
    fn creates_lock_when_project_folder_exists() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let project_folder = temp_dir.path().join("project");
        fs::create_dir(&project_folder).expect("project folder should be created");

        match get(
            LockKind::Watch,
            project_folder.to_str().expect("path should be valid"),
        ) {
            Lock::Aquired(_) => {}
            _ => panic!("expected lock to be acquired"),
        }

        assert!(
            project_folder.join("lib").exists(),
            "lib directory should be created"
        );
        assert!(
            project_folder
                .join("lib")
                .join(LockKind::Watch.file_name())
                .exists(),
            "lockfile should be created"
        );
    }

    #[test]
    fn ignores_stale_lock_for_unrelated_process_name() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let project_folder = temp_dir.path().join("project");
        let lib_dir = project_folder.join("lib");
        fs::create_dir_all(&lib_dir).expect("lib directory should be created");
        fs::write(lib_dir.join(LockKind::Watch.file_name()), "1").expect("lockfile should be written");

        match get(
            LockKind::Watch,
            project_folder.to_str().expect("path should be valid"),
        ) {
            Lock::Aquired(_) => {}
            _ => panic!("expected stale lock from unrelated process to be ignored"),
        }
    }
}
