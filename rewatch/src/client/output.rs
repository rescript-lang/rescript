use console::{Term, style};

use crate::daemon::proto::{DaemonEvent, daemon_event::Event};
use crate::helpers::emojis::*;

/// Check if we're running in a TTY (interactive terminal)
fn is_tty() -> bool {
    Term::stdout().is_term() && Term::stderr().is_term()
}

/// Format timing string, returning empty string if no_timing is true
fn timing(duration_seconds: f64, no_timing: bool) -> String {
    if no_timing {
        String::new()
    } else {
        format!(" in {:.2}s", duration_seconds)
    }
}

/// Render a DaemonEvent with emojis and formatting.
/// Returns the updated success value (true if a successful BuildFinished was encountered).
/// When not in a TTY, uses plain output format (no emojis, no step prefixes).
pub fn render_event(event: &DaemonEvent, no_timing: bool, current_success: bool) -> bool {
    let plain_output = !is_tty();
    let mut success = current_success;

    if let Some(ref evt) = event.event {
        match evt {
            Event::CompilerWarning(warning) => {
                if plain_output {
                    eprint!("{}", warning.message);
                } else {
                    eprint!("{}", style(&warning.message).yellow());
                }
            }
            Event::CompilerError(error) => {
                if plain_output {
                    eprint!("{}", error.message);
                } else {
                    eprint!("{}", style(&error.message).red());
                }
            }
            Event::InitializationError(error) => {
                let message = format!("Could not initialize build: {}", error.message);
                if plain_output {
                    eprintln!("{}", message);
                } else {
                    eprintln!("{}", style(&message).red());
                }
            }
            Event::ConfigWarning(warning) => {
                let kind_name = crate::daemon::proto::ConfigWarningKind::try_from(warning.kind)
                    .unwrap_or(crate::daemon::proto::ConfigWarningKind::ConfigUnknown);
                let message = match kind_name {
                    crate::daemon::proto::ConfigWarningKind::ConfigUnsupported => format!(
                        "The field '{}' found in the package config of '{}' is not supported by ReScript 12's new build system.",
                        warning.field_name, warning.package_name
                    ),
                    crate::daemon::proto::ConfigWarningKind::ConfigUnknown => format!(
                        "Unknown field '{}' found in the package config of '{}'. This option will be ignored.",
                        warning.field_name, warning.package_name
                    ),
                };
                if plain_output {
                    eprintln!("\n{}", message);
                } else {
                    eprintln!("\n{}", style(&message).yellow());
                }
            }
            Event::DuplicatedPackage(warning) => {
                let message = format!(
                    "Duplicated package: {} ./{} (chosen) vs ./{} in ./{}",
                    warning.package_name, warning.chosen_path, warning.duplicate_path, warning.parent_path
                );
                if plain_output {
                    eprintln!("{}", message);
                } else {
                    eprintln!("{}", style(&message).yellow());
                }
            }
            Event::MissingImplementation(warning) => {
                let message = format!(
                    "No implementation file found for interface file (skipping): {}",
                    warning.interface_file
                );
                if plain_output {
                    eprintln!("{}", message);
                } else {
                    eprintln!("{}", style(&message).yellow());
                }
            }
            Event::ModuleNotFound(error) => {
                let message = format!("Module not found: {}", error.module_name);
                if plain_output {
                    eprintln!("{}", message);
                } else {
                    eprintln!("{}", style(&message).red());
                }
            }
            Event::PackageNameMismatch(warning) => {
                let message = format!(
                    "Package name mismatch for {}:\n\
The package.json name is \"{}\", while the rescript.json name is \"{}\"\n\
This inconsistency will cause issues with package resolution.",
                    warning.package_path, warning.package_json_name, warning.rescript_json_name
                );
                if plain_output {
                    eprintln!("{}", message);
                } else {
                    eprintln!("{}", style(&message).yellow());
                }
            }
            Event::Cleaned(cleaned) => {
                if plain_output {
                    if cleaned.due_to_compiler_update {
                        println!("Cleaned previous build due to compiler update");
                    }
                    println!("Cleaned {}/{}", cleaned.cleaned_count, cleaned.total_count);
                } else {
                    if cleaned.due_to_compiler_update {
                        println!(
                            "{}{} {}Cleaned previous build due to compiler update",
                            LINE_CLEAR,
                            style("[1/3]").bold().dim(),
                            SWEEP
                        );
                    }
                    println!(
                        "{}{} {}Cleaned {}/{}{}",
                        LINE_CLEAR,
                        style("[1/3]").bold().dim(),
                        SWEEP,
                        cleaned.cleaned_count,
                        cleaned.total_count,
                        timing(cleaned.duration_seconds, no_timing)
                    );
                }
            }
            Event::Parsed(parsed) => {
                if plain_output {
                    println!("Parsed {} source files", parsed.parsed_count);
                } else {
                    println!(
                        "{}{} {}Parsed {} source files{}",
                        LINE_CLEAR,
                        style("[2/3]").bold().dim(),
                        CODE,
                        parsed.parsed_count,
                        timing(parsed.duration_seconds, no_timing)
                    );
                }
            }
            Event::Compiling(compiling) => {
                // Only show live progress in TTY mode
                if !plain_output {
                    use std::io::Write;
                    print!(
                        "\r{}{} {}Compiling... {}/{}",
                        LINE_CLEAR,
                        style("[3/3]").bold().dim(),
                        SWORDS,
                        compiling.current_count,
                        compiling.total_count
                    );
                    let _ = std::io::stdout().flush();
                }
            }
            Event::Compiled(compiled) => {
                if plain_output {
                    println!("Compiled {} modules", compiled.compiled_count);
                } else {
                    println!(
                        "{}{} {}Compiled {} modules{}",
                        LINE_CLEAR,
                        style("[3/3]").bold().dim(),
                        SWORDS,
                        compiled.compiled_count,
                        timing(compiled.duration_seconds, no_timing)
                    );
                }
            }
            Event::CleanedCompilerAssets(cleaned) => {
                if plain_output {
                    println!("Cleaned compiler assets");
                } else {
                    println!(
                        "{}{} {}Cleaned compiler assets{}",
                        LINE_CLEAR,
                        style("[1/2]").bold().dim(),
                        SWEEP,
                        timing(cleaned.duration_seconds, no_timing)
                    );
                }
            }
            Event::CleanedJsFiles(cleaned) => {
                if plain_output {
                    println!("Cleaned {} files", cleaned.suffix);
                } else {
                    println!(
                        "{}{} {}Cleaned {} files{}",
                        LINE_CLEAR,
                        style("[2/2]").bold().dim(),
                        SWEEP,
                        cleaned.suffix,
                        timing(cleaned.duration_seconds, no_timing)
                    );
                }
            }
            Event::CircularDependency(err) => {
                let guidance =
                    "Possible solutions:\n- Extract shared code into a new module both depend on.\n";
                if plain_output {
                    eprintln!(
                        "\nCan't continue... Found a circular dependency in your code:\n{}\n{}",
                        err.cycle_description, guidance
                    );
                } else {
                    eprintln!(
                        "\n{}\n{}\n{}",
                        style("Can't continue... Found a circular dependency in your code:").red(),
                        err.cycle_description,
                        guidance
                    );
                }
            }
            Event::UnallowedDependency(err) => {
                if plain_output {
                    eprintln!(
                        "\nError: {} has the following unallowed dependencies:",
                        err.package_name
                    );
                    for group in &err.groups {
                        eprintln!("{} dependencies: {}", group.deps_type, group.deps.join(", "));
                    }
                    eprintln!(
                        "\nUpdate the unallowed_dependents value in the config.json of the unallowed dependencies to solve the issue!"
                    );
                } else {
                    eprintln!(
                        "\n{}: {} has the following unallowed dependencies:",
                        style("Error").red(),
                        style(&err.package_name).bold()
                    );
                    for group in &err.groups {
                        eprintln!(
                            "{} dependencies: {}",
                            style(&group.deps_type).bold().dim(),
                            style(group.deps.join(", ")).bold().dim()
                        );
                    }
                    eprintln!(
                        "\nUpdate the {} value in the {} of the unallowed dependencies to solve the issue!",
                        style("unallowed_dependents").bold().dim(),
                        style("config.json").bold().dim()
                    );
                }
            }
            Event::PackageTreeError(err) => {
                if plain_output {
                    eprintln!(
                        "Could not build package tree reading dependency '{}'. {}",
                        err.package_name, err.error
                    );
                } else {
                    eprintln!(
                        "{}{} {}Could not build package tree reading dependency '{}'. {}",
                        LINE_CLEAR,
                        style("[1/2]").bold().dim(),
                        CROSS,
                        err.package_name,
                        err.error
                    );
                }
            }
            Event::BuildFinished(finished) => {
                success = finished.success;
                if finished.success {
                    // Only show completion message in TTY mode on success
                    if !plain_output {
                        if no_timing {
                            println!("\n{}{}Finished Compilation", LINE_CLEAR, SPARKLES);
                        } else {
                            println!(
                                "\n{}{}Finished Compilation in {:.2}s",
                                LINE_CLEAR, SPARKLES, finished.duration_seconds
                            );
                        }
                    }
                } else {
                    // Show failure message
                    if plain_output {
                        eprintln!(
                            "\nIncremental build failed. Error: {}Failed to Compile. See Errors Above",
                            LINE_CLEAR
                        );
                    } else {
                        eprintln!(
                            "\nIncremental build failed. Error: {}{}Failed to Compile. See Errors Above",
                            LINE_CLEAR, CROSS
                        );
                    }
                }
            }
            Event::JsPostBuildOutput(output) => {
                // Show js-post-build output to the user
                if let Some(ref stdout) = output.stdout {
                    if plain_output {
                        println!("[js-post-build] {}", stdout);
                    } else {
                        println!("{}[js-post-build] {}", style("").dim(), stdout);
                    }
                }
                if let Some(ref stderr) = output.stderr {
                    if plain_output {
                        eprintln!("[js-post-build] {}", stderr);
                    } else {
                        eprintln!("{}", style(format!("[js-post-build] {}", stderr)).yellow());
                    }
                }
            }
            // Ignore events that don't need rendering for build/clean/watch clients
            Event::ClientConnected(_)
            | Event::ClientDisconnected(_)
            | Event::BuildStarted(_)
            | Event::FileChanged(_)
            | Event::WatchPaths(_)
            | Event::FormatStarted(_)
            | Event::FormatFinished(_)
            | Event::FormatProgress(_)
            | Event::FormatCheckFailed(_)
            | Event::FormattedStdin(_)
            | Event::GeneratingAst(_) => {}
        }
    }

    success
}
