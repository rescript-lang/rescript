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
        };
        write!(f, "{msg}")
    }
}

pub enum Lock {
    Aquired(u32),
    Error(Error),
}

fn exists(to_check_pid: u32) -> bool {
    System::new_all()
        .processes()
        .iter()
        .any(|(pid, _process)| pid.as_u32() == to_check_pid)
}

fn create(lockfile_location: &Path, pid: u32) -> Lock {
    // Create /lib if not exists
    if let Some(Err(e)) = lockfile_location.parent().map(fs::create_dir_all) {
        return Lock::Error(Error::WritingLockfile(e));
    };

    File::create(lockfile_location)
        .and_then(|mut file| file.write(pid.to_string().as_bytes()).map(|_| Lock::Aquired(pid)))
        .unwrap_or_else(|e| Lock::Error(Error::WritingLockfile(e)))
}

pub fn get(folder: &str) -> Lock {
    let location = Path::new(folder).join("lib").join(LOCKFILE);
    let pid = process::id();

    match fs::read_to_string(&location) {
        Err(e) if (e.kind() == std::io::ErrorKind::NotFound) => create(&location, pid),
        Err(e) => Lock::Error(Error::ReadingLockfile(e)),
        Ok(s) => match s.parse::<u32>() {
            Ok(parsed_pid) if !exists(parsed_pid) => create(&location, pid),
            Ok(parsed_pid) => Lock::Error(Error::Locked(parsed_pid)),
            Err(e) => Lock::Error(Error::ParsingLockfile(e)),
        },
    }
}
