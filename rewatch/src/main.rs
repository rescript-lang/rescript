use anyhow::Result;
use std::path::Path;

use rescript::{cli, client, cmd, daemon};

fn main() -> Result<()> {
    let cli = cli::parse_with_default().unwrap_or_else(|err| err.exit());

    match cli.command {
        cli::Command::CompilerArgs { path } => {
            let runtime = tokio::runtime::Runtime::new()?;
            let result = runtime.block_on(client::get_compiler_args(Path::new(&path)));
            match result {
                Ok(json) => {
                    println!("{}", json);
                    std::process::exit(0);
                }
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1);
                }
            }
        }
        cli::Command::Build(build_args) => {
            let runtime = tokio::runtime::Runtime::new()?;
            let result = runtime.block_on(client::build(
                &build_args.folder,
                build_args.filter.as_ref().map(|r| r.as_str().to_string()),
                build_args.warn_error.warn_error.clone(),
                build_args.no_timing,
            ));

            match result {
                Ok(success) => {
                    if success {
                        if let Some(args_after_build) = (*build_args.after_build).clone() {
                            cmd::run(args_after_build)
                        }
                        std::process::exit(0)
                    } else {
                        std::process::exit(1)
                    }
                }
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1)
                }
            }
        }
        cli::Command::Watch(watch_args) => {
            let runtime = tokio::runtime::Runtime::new()?;
            let result = runtime.block_on(client::watch(
                &watch_args.folder,
                watch_args.filter.as_ref().map(|r| r.as_str().to_string()),
                (*watch_args.after_build).clone(),
            ));

            match result {
                Ok(()) => Ok(()),
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1)
                }
            }
        }
        cli::Command::Clean { folder } => {
            let runtime = tokio::runtime::Runtime::new()?;
            let result = runtime.block_on(client::clean(&folder));

            match result {
                Ok(success) => {
                    if success {
                        std::process::exit(0)
                    } else {
                        std::process::exit(1)
                    }
                }
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1)
                }
            }
        }
        cli::Command::Format { stdin, check, files } => {
            let runtime = tokio::runtime::Runtime::new()?;

            // Dispatch to the appropriate format function based on arguments
            let result = runtime.block_on(async {
                if let Some(ext) = stdin {
                    // Format stdin with the given extension
                    let ext_str = match ext {
                        cli::FileExtension::Res => ".res".to_string(),
                        cli::FileExtension::Resi => ".resi".to_string(),
                    };
                    client::format::format_stdin(ext_str).await
                } else if check {
                    // Check formatting
                    if files.is_empty() {
                        client::format::check_format_project().await
                    } else {
                        client::format::check_format_files(files).await
                    }
                } else {
                    // Format files
                    if files.is_empty() {
                        client::format::format_project().await
                    } else {
                        client::format::format_files(files).await
                    }
                }
            });

            match result {
                Ok(success) => {
                    if success {
                        std::process::exit(0)
                    } else {
                        std::process::exit(1)
                    }
                }
                Err(e) => {
                    eprintln!("{:#}", e);
                    std::process::exit(1)
                }
            }
        }
        // Hidden command used internally by client::start_daemon_if_needed() to spawn
        // the daemon as a background process. Not intended for direct user invocation.
        cli::Command::Daemon { folder } => {
            let root = Path::new(&folder as &str).canonicalize()?;
            let runtime = tokio::runtime::Runtime::new()?;
            runtime.block_on(daemon::start(root))
        }
        cli::Command::Debug { folder } => {
            let root = client::find_project_root(Path::new(&folder as &str))?;

            // Start daemon if not running (debug clients can also launch the daemon)
            let runtime = tokio::runtime::Runtime::new()?;
            runtime.block_on(async {
                client::start_daemon_if_needed(&root).await?;
                client::debug(&root).await
            })
        }
    }
}
