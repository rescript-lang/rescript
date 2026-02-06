use crate::build;
use crate::build::build_types::{BuildCommandState, SourceType};
use crate::build::clean;
use crate::cmd;
use crate::config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::helpers::emojis::*;
use crate::lock::LOCKFILE;
use crate::queue::FifoQueue;
use crate::queue::*;
use anyhow::{Context, Result};
use futures_timer::Delay;
use notify::event::ModifyKind;
use notify::{Config, Error, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::Mutex;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum CompileType {
    Incremental,
    Full,
    None,
}

fn is_rescript_file(path_buf: &Path) -> bool {
    let extension = path_buf.extension().and_then(|ext| ext.to_str());

    if let Some(extension) = extension {
        helpers::is_implementation_file(extension) || helpers::is_interface_file(extension)
    } else {
        false
    }
}

fn is_in_build_path(path_buf: &Path) -> bool {
    let mut prev_component: Option<&std::ffi::OsStr> = None;
    for component in path_buf.components() {
        let comp_os = component.as_os_str();
        if let Some(prev) = prev_component
            && prev == "lib"
            && (comp_os == "bs" || comp_os == "ocaml")
        {
            return true;
        }
        prev_component = Some(comp_os);
    }
    false
}

fn matches_filter(path_buf: &Path, filter: &Option<regex::Regex>) -> bool {
    let name = path_buf
        .file_name()
        .map(|x| x.to_string_lossy().to_string())
        .unwrap_or("".to_string());
    filter.as_ref().map(|re| !re.is_match(&name)).unwrap_or(true)
}

/// Computes the list of paths to watch based on the build state.
/// Returns tuples of (path, recursive_mode) for each watch target.
fn compute_watch_paths(build_state: &BuildCommandState, root: &Path) -> Vec<(PathBuf, RecursiveMode)> {
    // Use a HashMap to deduplicate paths, giving precedence to Recursive mode
    // when the same path appears with different modes (e.g. package root watched
    // NonRecursively for rescript.json changes, but also as a source folder with
    // Recursive mode).
    let mut watch_paths: std::collections::HashMap<PathBuf, RecursiveMode> = std::collections::HashMap::new();

    let mut insert = |path: PathBuf, mode: RecursiveMode| {
        watch_paths
            .entry(path)
            .and_modify(|existing| {
                if mode == RecursiveMode::Recursive {
                    *existing = RecursiveMode::Recursive;
                }
            })
            .or_insert(mode);
    };

    for (_, package) in build_state.build_state.packages.iter() {
        if !package.is_local_dep {
            continue;
        }

        // Watch the package root non-recursively to detect rescript.json changes.
        // We watch the directory rather than the file directly because many editors
        // use atomic writes (delete + recreate or write to temp + rename) which would
        // cause a direct file watch to be lost after the first edit.
        insert(package.path.clone(), RecursiveMode::NonRecursive);

        // Watch each source folder
        for source in &package.source_folders {
            let dir = package.path.join(&source.dir);
            if !dir.exists() {
                log::error!(
                    "Could not read folder: {:?}. Specified in dependency: {}, located {:?}...",
                    source.dir,
                    package.name,
                    package.path
                );
                continue;
            }
            let mode = match &source.subdirs {
                Some(config::Subdirs::Recurse(true)) => RecursiveMode::Recursive,
                _ => RecursiveMode::NonRecursive,
            };
            insert(dir, mode);
        }
    }

    // Watch the lib/ directory for the lockfile (rescript.lock lives in lib/)
    let lib_dir = root.join("lib");
    if lib_dir.exists() {
        insert(lib_dir, RecursiveMode::NonRecursive);
    }

    watch_paths.into_iter().collect()
}

/// Registers all watch paths with the given watcher.
fn register_watches(watcher: &mut RecommendedWatcher, watch_paths: &[(PathBuf, RecursiveMode)]) {
    for (path, mode) in watch_paths {
        let mode_str = if *mode == RecursiveMode::Recursive {
            "recursive"
        } else {
            "non-recursive"
        };
        log::debug!("  watching ({mode_str}): {}", path.display());
        if let Err(e) = watcher.watch(path, *mode) {
            log::error!("Could not watch {}: {}", path.display(), e);
        }
    }
}

/// Unregisters all watch paths from the given watcher.
fn unregister_watches(watcher: &mut RecommendedWatcher, watch_paths: &[(PathBuf, RecursiveMode)]) {
    for (path, _) in watch_paths {
        let _ = watcher.unwatch(path);
    }
}

struct AsyncWatchArgs<'a> {
    watcher: &'a mut RecommendedWatcher,
    current_watch_paths: Vec<(PathBuf, RecursiveMode)>,
    initial_build_state: BuildCommandState,
    q: Arc<FifoQueue<Result<Event, Error>>>,
    path: &'a Path,
    show_progress: bool,
    filter: &'a Option<regex::Regex>,
    after_build: Option<String>,
    create_sourcedirs: bool,
    plain_output: bool,
}

async fn async_watch(
    AsyncWatchArgs {
        watcher,
        mut current_watch_paths,
        initial_build_state,
        q,
        path,
        show_progress,
        filter,
        after_build,
        create_sourcedirs,
        plain_output,
    }: AsyncWatchArgs<'_>,
) -> Result<()> {
    let mut build_state = initial_build_state;
    let mut needs_compile_type = CompileType::Incremental;
    // create a mutex to capture if ctrl-c was pressed
    let ctrlc_pressed = Arc::new(Mutex::new(false));
    let ctrlc_pressed_clone = Arc::clone(&ctrlc_pressed);

    ctrlc::set_handler(move || {
        let pressed = Arc::clone(&ctrlc_pressed);
        let mut pressed = pressed.lock().unwrap();
        *pressed = true;
    })
    .expect("Error setting Ctrl-C handler");

    let mut initial_build = true;

    loop {
        if *ctrlc_pressed_clone.lock().unwrap() {
            if show_progress {
                println!("\nExiting...");
            }
            clean::cleanup_after_build(&build_state);
            break Ok(());
        }
        let mut events: Vec<Event> = vec![];
        if !q.is_empty() {
            // Wait for events to settle
            Delay::new(Duration::from_millis(50)).await;
        }
        while !q.is_empty() {
            if let Ok(event) = q.pop() {
                events.push(event)
            }
        }

        for event in events {
            // if there is a file named rescript.lock in the events path, we can quit the watcher
            if event.paths.iter().any(|path| path.ends_with(LOCKFILE))
                && let EventKind::Remove(_) = event.kind
            {
                if show_progress {
                    println!("\nExiting... (lockfile removed)");
                }
                clean::cleanup_after_build(&build_state);
                return Ok(());
            }

            // Detect rescript.json changes and trigger a full rebuild
            if event
                .paths
                .iter()
                .any(|p| p.file_name().map(|name| name == "rescript.json").unwrap_or(false))
                && matches!(
                    event.kind,
                    EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_)
                )
            {
                log::debug!("rescript.json changed -> full compile");
                tracing::debug!(
                    reason = "rescript.json changed",
                    event_kind = ?event.kind,
                    paths = ?event.paths,
                    "watcher.full_compile_triggered"
                );
                needs_compile_type = CompileType::Full;
                continue;
            }

            let paths = event
                .paths
                .iter()
                .filter(|path| is_rescript_file(path))
                .filter(|path| !is_in_build_path(path))
                .filter(|path| matches_filter(path, filter));
            for path in paths {
                let path_buf = path.to_path_buf();

                match (needs_compile_type, event.kind) {
                    (
                        CompileType::Incremental | CompileType::None,
                        // when we have a name change, create or remove event we need to do a full compile
                        EventKind::Remove(_)
                        | EventKind::Any
                        | EventKind::Create(_)
                        | EventKind::Modify(ModifyKind::Name(_)),
                    ) => {
                        // if we are going to do a full compile, we don't need to bother marking
                        // files dirty because we do a full scan anyway
                        log::debug!("received {:?} while needs_compile_type was {needs_compile_type:?} -> full compile", event.kind);
                        needs_compile_type = CompileType::Full;
                    }

                    (
                        CompileType::None | CompileType::Incremental,
                        // when we have a data change event, we can do an incremental compile
                        EventKind::Modify(ModifyKind::Data(_)) |
                        // windows sends ModifyKind::Any on file content changes
                        EventKind::Modify(ModifyKind::Any),
                    ) => {
                        // if we are going to compile incrementally, we need to mark the exact files
                        // dirty
                        log::debug!("received {:?} while needs_compile_type was {needs_compile_type:?} -> incremental compile", event.kind);
                        if let Ok(canonicalized_path_buf) = path_buf
                            .canonicalize()
                            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
                        {
                            // Collect package names first to avoid borrow checker issues
                            let module_package_pairs = build_state.module_name_package_pairs();

                            for (module_name, package_name) in module_package_pairs {
                                let package = build_state
                                    .build_state
                                    .packages
                                    .get(&package_name)
                                    .expect("Package not found");

                                if let Some(module) = build_state.build_state.modules.get_mut(&module_name) {
                                    match module.source_type {
                                        SourceType::SourceFile(ref mut source_file) => {
                                        let canonicalized_implementation_file =
                                            package.path.join(&source_file.implementation.path);
                                        if canonicalized_path_buf == canonicalized_implementation_file {
                                            if let Ok(modified) =
                                                canonicalized_path_buf.metadata().and_then(|x| x.modified())
                                            {
                                                source_file.implementation.last_modified = modified;
                                            };
                                            source_file.implementation.parse_dirty = true;
                                            break;
                                        }

                                        // mark the interface file dirty
                                        if let Some(ref mut interface) = source_file.interface {
                                            let canonicalized_interface_file =
                                                package.path.join(&interface.path);
                                            if canonicalized_path_buf == canonicalized_interface_file {
                                                if let Ok(modified) = canonicalized_path_buf
                                                    .metadata()
                                                    .and_then(|x| x.modified())
                                                {
                                                    interface.last_modified = modified;
                                                }
                                                interface.parse_dirty = true;
                                                break;
                                            }
                                        }
                                        }
                                        SourceType::MlMap(_) => (),
                                    }
                                }
                            }
                            needs_compile_type = CompileType::Incremental;
                        }
                    }

                    (
                        CompileType::None | CompileType::Incremental,
                        // these are not relevant events for compilation
                        EventKind::Access(_)
                        | EventKind::Other
                        | EventKind::Modify(ModifyKind::Metadata(_))
                        | EventKind::Modify(ModifyKind::Other),
                    ) => (),
                    // if we already need a full compile, we don't need to check for other events
                    (CompileType::Full, _) => (),
                }
            }
        }

        if needs_compile_type != CompileType::None {
            log::debug!("doing {needs_compile_type:?}");
        }

        match needs_compile_type {
            CompileType::Incremental => {
                let timing_total = Instant::now();
                if build::incremental_build(
                    &mut build_state,
                    None,
                    initial_build,
                    show_progress,
                    !initial_build,
                    create_sourcedirs,
                    plain_output,
                )
                .is_ok()
                {
                    if let Some(a) = after_build.clone() {
                        cmd::run(a)
                    }
                    let timing_total_elapsed = timing_total.elapsed();
                    if show_progress {
                        let compilation_type = if initial_build { "initial" } else { "incremental" };
                        if plain_output {
                            println!("Finished {compilation_type} compilation")
                        } else {
                            println!(
                                "\n{}{}Finished {} compilation in {:.2}s\n",
                                LINE_CLEAR,
                                SPARKLES,
                                compilation_type,
                                timing_total_elapsed.as_secs_f64()
                            );
                        }
                    }
                }
                needs_compile_type = CompileType::None;
                initial_build = false;
            }
            CompileType::Full => {
                let timing_total = Instant::now();
                build_state = build::initialize_build(
                    None,
                    filter,
                    show_progress,
                    path,
                    plain_output,
                    build_state.get_warn_error_override(),
                )
                .expect("Could not initialize build");

                // Re-register watches based on the new build state
                unregister_watches(watcher, &current_watch_paths);
                current_watch_paths = compute_watch_paths(&build_state, path);
                register_watches(watcher, &current_watch_paths);

                let _ = build::incremental_build(
                    &mut build_state,
                    None,
                    initial_build,
                    show_progress,
                    false,
                    create_sourcedirs,
                    plain_output,
                );
                if let Some(a) = after_build.clone() {
                    cmd::run(a)
                }

                build::write_build_ninja(&build_state);

                let timing_total_elapsed = timing_total.elapsed();
                if show_progress {
                    if plain_output {
                        println!("Finished compilation")
                    } else {
                        println!(
                            "\n{}{}Finished compilation in {:.2}s\n",
                            LINE_CLEAR,
                            SPARKLES,
                            timing_total_elapsed.as_secs_f64()
                        );
                    }
                }
                needs_compile_type = CompileType::None;
                initial_build = false;
            }
            CompileType::None => {
                // We want to sleep for a little while so the CPU can schedule other work. That way we end
                // up not burning CPU cycles.
                Delay::new(Duration::from_millis(50)).await;
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn start(
    filter: &Option<regex::Regex>,
    show_progress: bool,
    folder: &str,
    after_build: Option<String>,
    create_sourcedirs: bool,
    plain_output: bool,
    warn_error: Option<String>,
) -> Result<()> {
    futures::executor::block_on(async {
        let queue = Arc::new(FifoQueue::<Result<Event, Error>>::new());
        let producer = queue.clone();
        let consumer = queue.clone();

        let mut watcher = RecommendedWatcher::new(move |res| producer.push(res), Config::default())
            .expect("Could not create watcher");

        let path = Path::new(folder);

        // Do an initial build to discover packages and source folders
        let build_state: BuildCommandState = build::initialize_build(
            None,
            filter,
            show_progress,
            path,
            plain_output,
            warn_error.clone(),
        )
        .with_context(|| "Could not initialize build")?;

        // Compute and register targeted watches based on source folders
        let current_watch_paths = compute_watch_paths(&build_state, path);
        register_watches(&mut watcher, &current_watch_paths);

        async_watch(AsyncWatchArgs {
            watcher: &mut watcher,
            current_watch_paths,
            initial_build_state: build_state,
            q: consumer,
            path,
            show_progress,
            filter,
            after_build,
            create_sourcedirs,
            plain_output,
        })
        .await
    })
}
