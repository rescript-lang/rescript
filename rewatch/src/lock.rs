use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process;
use sysinfo::{PidExt, System, SystemExt};

/* This locking mechanism is meant to never be deleted. Instead, it stores the PID of the process
 * that's running, when trying to aquire a lock, it checks wether that process is still running. If
 * not, it rewrites the lockfile to have its own PID instead. */

pub static LOCKFILE: &str = "rescript.lock";

pub enum Error {
    Locked(u32),
    ParsingLockfile(std::num::ParseIntError),
    ReadingLockfile(std::io::Error),
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
            Error::ReadingLockfile(e) => {
                format!("Could not read lockfile: \n {e} \n  (try removing it and running the command again)")
            }
            Error::WritingLockfile(e) => format!("Could not write lockfile: \n {e}"),
            Error::ProjectFolderMissing(path) => format!(
                "Could not write lockfile because the specified project folder does not exist: {}",
                path.to_string_lossy()
            ),
        };
        write!(f, "{msg}")
    }
}

pub enum Lock {
    Aquired(u32),
    Error(Error),
}

fn pid_exists(to_check_pid: u32) -> bool {
    System::new_all()
        .processes()
        .iter()
        .any(|(pid, _process)| pid.as_u32() == to_check_pid)
}

pub fn get(folder: &str) -> Lock {
    let project_folder = Path::new(folder);
    if !project_folder.exists() {
        return Lock::Error(Error::ProjectFolderMissing(project_folder.to_path_buf()));
    }

    let lib_dir = project_folder.join("lib");
    let location = lib_dir.join(LOCKFILE);
    let pid = process::id();

    // When a lockfile already exists we parse its PID: if the process is still alive we refuse to
    // proceed, otherwise we will overwrite the stale lock with our own PID.
    match fs::read_to_string(&location) {
        Ok(contents) => match contents.parse::<u32>() {
            Ok(parsed_pid) if pid_exists(parsed_pid) => return Lock::Error(Error::Locked(parsed_pid)),
            Ok(_) => (),
            Err(e) => return Lock::Error(Error::ParsingLockfile(e)),
        },
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => (),
        Err(e) => return Lock::Error(Error::ReadingLockfile(e)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn returns_error_when_project_folder_missing() {
        let temp_dir = TempDir::new().expect("temp dir should be created");
        let missing_folder = temp_dir.path().join("missing_project");

        match get(missing_folder.to_str().expect("path should be valid")) {
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

        match get(project_folder.to_str().expect("path should be valid")) {
            Lock::Aquired(_) => {}
            _ => panic!("expected lock to be acquired"),
        }

        assert!(
            project_folder.join("lib").exists(),
            "lib directory should be created"
        );
        assert!(
            project_folder.join("lib").join(LOCKFILE).exists(),
            "lockfile should be created"
        );
    }
}
