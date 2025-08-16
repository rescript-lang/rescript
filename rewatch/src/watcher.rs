use crate::build;
use crate::build::build_types::SourceType;
use crate::build::clean;
use crate::cmd;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::helpers::emojis::*;
use crate::lock::LOCKFILE;
use crate::queue::FifoQueue;
use crate::queue::*;
use futures_timer::Delay;
use notify::event::ModifyKind;
use notify::{Config, Error, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;
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
        if let Some(prev) = prev_component {
            if prev == "lib" && (comp_os == "bs" || comp_os == "ocaml") {
                return true;
            }
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

struct AsyncWatchArgs<'a> {
    q: Arc<FifoQueue<Result<Event, Error>>>,
    path: &'a Path,
    show_progress: bool,
    filter: &'a Option<regex::Regex>,
    after_build: Option<String>,
    create_sourcedirs: bool,
    build_dev_deps: bool,
    snapshot_output: bool,
}

async fn async_watch(
    AsyncWatchArgs {
        q,
        path,
        show_progress,
        filter,
        after_build,
        create_sourcedirs,
        build_dev_deps,
        snapshot_output,
    }: AsyncWatchArgs<'_>,
) -> notify::Result<()> {
    let mut build_state =
        build::initialize_build(None, filter, show_progress, path, build_dev_deps, snapshot_output)
            .expect("Can't initialize build");
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
            if event.paths.iter().any(|path| path.ends_with(LOCKFILE)) {
                if let EventKind::Remove(_) = event.kind {
                    if show_progress {
                        println!("\nExiting... (lockfile removed)");
                    }
                    clean::cleanup_after_build(&build_state);
                    return Ok(());
                }
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
                            for module in build_state.modules.values_mut() {
                                match module.source_type {
                                    SourceType::SourceFile(ref mut source_file) => {
                                        // mark the implementation file dirty
                                        let package = build_state
                                            .packages
                                            .get(&module.package_name)
                                            .expect("Package not found");
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
                    snapshot_output,
                )
                .is_ok()
                {
                    if let Some(a) = after_build.clone() {
                        cmd::run(a)
                    }
                    let timing_total_elapsed = timing_total.elapsed();
                    if show_progress {
                        let compilation_type = if initial_build { "initial" } else { "incremental" };
                        if snapshot_output {
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
                    build_dev_deps,
                    snapshot_output,
                )
                .expect("Can't initialize build");
                let _ = build::incremental_build(
                    &mut build_state,
                    None,
                    initial_build,
                    show_progress,
                    false,
                    create_sourcedirs,
                    snapshot_output,
                );
                if let Some(a) = after_build.clone() {
                    cmd::run(a)
                }

                build::write_build_ninja(&build_state);

                let timing_total_elapsed = timing_total.elapsed();
                if !snapshot_output && show_progress {
                    println!(
                        "\n{}{}Finished compilation in {:.2}s\n",
                        LINE_CLEAR,
                        SPARKLES,
                        timing_total_elapsed.as_secs_f64()
                    );
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

pub fn start(
    filter: &Option<regex::Regex>,
    show_progress: bool,
    folder: &str,
    after_build: Option<String>,
    create_sourcedirs: bool,
    build_dev_deps: bool,
    snapshot_output: bool,
) {
    futures::executor::block_on(async {
        let queue = Arc::new(FifoQueue::<Result<Event, Error>>::new());
        let producer = queue.clone();
        let consumer = queue.clone();

        let mut watcher = RecommendedWatcher::new(move |res| producer.push(res), Config::default())
            .expect("Could not create watcher");

        log::debug!("watching {folder}");

        watcher
            .watch(folder.as_ref(), RecursiveMode::Recursive)
            .expect("Could not start watcher");

        let path = Path::new(folder);

        if let Err(e) = async_watch(AsyncWatchArgs {
            q: consumer,
            path,
            show_progress,
            filter,
            after_build,
            create_sourcedirs,
            build_dev_deps,
            snapshot_output,
        })
        .await
        {
            println!("{e:?}")
        }
    })
}
