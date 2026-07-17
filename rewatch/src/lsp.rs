use crate::helpers;
use anyhow::{Context, Result, bail};
use std::path::Path;
use std::process::{Command, Stdio};

const LANGUAGE_SERVER_EXE: &str = "rescript-language-server.exe";

pub fn run(stdio: bool) -> Result<()> {
    let language_server_path = helpers::get_bin_dir().join(LANGUAGE_SERVER_EXE);
    let status = language_server_command(&language_server_path, stdio)
        .status()
        .with_context(|| {
            format!(
                "Could not start language server at {}",
                language_server_path.display()
            )
        })?;

    if !status.success() {
        bail!("Language server exited with status {status}");
    }

    Ok(())
}

fn language_server_command(language_server_path: &Path, stdio: bool) -> Command {
    let mut command = Command::new(language_server_path);
    if stdio {
        command.arg("--stdio");
    }
    command
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    command
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;

    #[test]
    fn uses_language_server_executable_with_stdio() {
        let language_server_path = Path::new("bin").join(LANGUAGE_SERVER_EXE);
        let command = language_server_command(&language_server_path, true);

        assert_eq!(command.get_program(), language_server_path.as_os_str());
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            vec![OsStr::new("--stdio")]
        );
    }

    #[test]
    fn omits_stdio_when_not_requested() {
        let language_server_path = Path::new("bin").join(LANGUAGE_SERVER_EXE);
        let command = language_server_command(&language_server_path, false);

        assert!(command.get_args().next().is_none());
    }
}
