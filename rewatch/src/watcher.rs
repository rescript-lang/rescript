use crate::build;
use crate::build::build_types::{BuildCommandState, SourceType};
use crate::build::clean;
use crate::cmd;
use crate::config;
use crate::helpers;
use crate::helpers::StrippedVerbatimPath;
use crate::lock::LockKind;
use crate::queue::FifoQueue;
use crate::queue::*;
use anyhow::{Context, Result};
use console::Term;
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

    // Watch the lib/ directory for the watcher lockfile (watch.lock lives in lib/)
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

fn carry_forward_compile_warnings(previous: &BuildCommandState, next: &mut BuildCommandState) {
    for (module_name, next_module) in next.build_state.modules.iter_mut() {
        let Some(previous_module) = previous.build_state.modules.get(module_name) else {
            continue;
        };
        if previous_module.package_name != next_module.package_name {
            continue;
        }

        match (&previous_module.source_type, &mut next_module.source_type) {
            (SourceType::SourceFile(previous_source), SourceType::SourceFile(next_source)) => {
                if previous_source.implementation.path == next_source.implementation.path {
                    next_source.implementation.compile_warnings =
                        previous_source.implementation.compile_warnings.clone();

                    if next_source.implementation.compile_warnings.is_some() {
                        next_source.implementation.compile_state =
                            previous_source.implementation.compile_state.clone();
                    }
                }

                if let (Some(previous_interface), Some(next_interface)) =
                    (&previous_source.interface, &mut next_source.interface)
                    && previous_interface.path == next_interface.path
                {
                    next_interface.compile_warnings = previous_interface.compile_warnings.clone();

                    if next_interface.compile_warnings.is_some() {
                        next_interface.compile_state = previous_interface.compile_state.clone();
                    }
                }
            }
            (SourceType::MlMap(_), SourceType::MlMap(_)) => (),
            _ => (),
        }
    }
}

fn should_clear_screen(
    clear_screen: bool,
    show_progress: bool,
    plain_output: bool,
    initial_build: bool,
) -> bool {
    clear_screen && show_progress && !plain_output && !initial_build
}

fn clear_terminal_screen() {
    let _ = Term::stdout().clear_screen();
}

fn print_rebuild_header(compile_type: CompileType) {
    match compile_type {
        CompileType::Incremental => println!("Change detected. Rebuilding..."),
        CompileType::Full => println!("Change detected. Full rebuild..."),
        CompileType::None => (),
    }
}

fn print_build_failed_footer() {
    println!("\nBuild failed. Watching for changes...");
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
    clear_screen: bool,
    prod: bool,
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
        clear_screen,
        prod,
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
            // If watch.lock is removed, we can quit the watcher.
            if event
                .paths
                .iter()
                .any(|path| path.ends_with(LockKind::Watch.file_name()))
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
                if should_clear_screen(clear_screen, show_progress, plain_output, initial_build) {
                    clear_terminal_screen();
                    print_rebuild_header(CompileType::Incremental);
                }

                let timing_total = Instant::now();
                let result = build::incremental_build(
                    &mut build_state,
                    None,
                    initial_build,
                    show_progress,
                    !initial_build,
                    create_sourcedirs,
                    plain_output,
                );

                match result {
                    Ok(result) => {
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
                                    "\n{}\n",
                                    build::format_finished_compilation_message(
                                        Some(compilation_type),
                                        result,
                                        timing_total_elapsed,
                                    )
                                );
                            }
                        }
                    }
                    Err(_) => {
                        if should_clear_screen(clear_screen, show_progress, plain_output, initial_build) {
                            print_build_failed_footer();
                        }
                    }
                }

                needs_compile_type = CompileType::None;
                initial_build = false;
            }
            CompileType::Full => {
                if should_clear_screen(clear_screen, show_progress, plain_output, initial_build) {
                    clear_terminal_screen();
                    print_rebuild_header(CompileType::Full);
                }

                let timing_total = Instant::now();
                let mut next_build_state = build::initialize_build(
                    None,
                    filter,
                    show_progress,
                    path,
                    plain_output,
                    build_state.get_warn_error_override(),
                    prod,
                )
                .expect("Could not initialize build");

                // Full rebuilds can be triggered by editor atomic saves that surface as rename events.
                // Preserve warning state for unchanged modules so their warnings are re-emitted after the
                // fresh build state replaces the previous one.
                carry_forward_compile_warnings(&build_state, &mut next_build_state);
                build_state = next_build_state;

                // Re-register watches based on the new build state
                unregister_watches(watcher, &current_watch_paths);
                current_watch_paths = compute_watch_paths(&build_state, path);
                register_watches(watcher, &current_watch_paths);

                let result = build::incremental_build(
                    &mut build_state,
                    None,
                    initial_build,
                    show_progress,
                    false,
                    create_sourcedirs,
                    plain_output,
                );
                match result {
                    Ok(result) => {
                        if let Some(a) = after_build.clone() {
                            cmd::run(a)
                        }

                        let timing_total_elapsed = timing_total.elapsed();
                        if !plain_output && show_progress {
                            println!(
                                "\n{}\n",
                                build::format_finished_compilation_message(
                                    None,
                                    result,
                                    timing_total_elapsed,
                                )
                            );
                        }
                    }
                    Err(_) => {
                        if should_clear_screen(clear_screen, show_progress, plain_output, initial_build) {
                            print_build_failed_footer();
                        }
                    }
                }

                build::write_build_ninja(&build_state);
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
    clear_screen: bool,
    prod: bool,
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
            prod,
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
            clear_screen,
            prod,
        })
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build::build_types::{
        CompileState, CompilerInfo, Implementation, Interface, Module, ParseState, SourceFile, SourceType,
    };
    use crate::build::packages::{Namespace, Package};
    use crate::config;
    use crate::project_context::ProjectContext;
    use ahash::{AHashMap, AHashSet};
    use std::path::PathBuf;
    use std::sync::RwLock;
    use std::time::SystemTime;

    fn test_project_context(root: &str) -> ProjectContext {
        let root_path = PathBuf::from(root);
        let config = config::tests::create_config(config::tests::CreateConfigArgs {
            name: "test-root".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: root_path.clone(),
        });

        ProjectContext {
            current_config: config,
            monorepo_context: None,
            node_modules_exist_cache: RwLock::new(AHashMap::new()),
            packages_cache: RwLock::new(AHashMap::new()),
        }
    }

    fn test_package(name: &str, path: &str) -> Package {
        let package_path = PathBuf::from(path);
        Package {
            name: name.to_string(),
            config: config::tests::create_config(config::tests::CreateConfigArgs {
                name: name.to_string(),
                bs_deps: vec![],
                build_dev_deps: vec![],
                allowed_dependents: None,
                path: package_path.clone(),
            }),
            source_folders: AHashSet::new(),
            source_files: None,
            namespace: Namespace::NoNamespace,
            modules: None,
            path: package_path,
            dirs: None,
            gentype_dirs: None,
            is_local_dep: true,
            is_root: true,
        }
    }

    fn test_build_state(module_name: &str, module: Module) -> BuildCommandState {
        let root = "/tmp/rewatch-warning-carry-forward";
        let package = test_package("test-package", root);
        let mut packages = AHashMap::new();
        packages.insert(package.name.clone(), package);

        let compiler = CompilerInfo {
            bsc_path: PathBuf::from("/tmp/bsc"),
            bsc_hash: blake3::hash(b"test-bsc"),
            runtime_path: PathBuf::from("/tmp/runtime"),
        };

        let mut build_state = BuildCommandState::new(
            PathBuf::from(root),
            test_project_context(root),
            packages,
            compiler,
            None,
        );
        build_state.insert_module(module_name, module);
        build_state
    }

    fn test_module(
        implementation_path: &str,
        implementation_warning: Option<&str>,
        interface_path: Option<&str>,
        interface_warning: Option<&str>,
    ) -> Module {
        let implementation_compile_state = if implementation_warning.is_some() {
            CompileState::Warning
        } else {
            CompileState::Success
        };
        let interface_compile_state = if interface_warning.is_some() {
            CompileState::Warning
        } else {
            CompileState::Success
        };

        Module {
            source_type: SourceType::SourceFile(SourceFile {
                implementation: Implementation {
                    path: PathBuf::from(implementation_path),
                    parse_state: ParseState::Success,
                    compile_state: implementation_compile_state,
                    last_modified: SystemTime::UNIX_EPOCH,
                    parse_dirty: false,
                    compile_warnings: implementation_warning.map(str::to_string),
                },
                interface: interface_path.map(|interface_path| Interface {
                    path: PathBuf::from(interface_path),
                    parse_state: ParseState::Success,
                    compile_state: interface_compile_state,
                    last_modified: SystemTime::UNIX_EPOCH,
                    parse_dirty: false,
                    compile_warnings: interface_warning.map(str::to_string),
                }),
            }),
            deps: AHashSet::new(),
            dependents: AHashSet::new(),
            package_name: "test-package".to_string(),
            compile_dirty: false,
            last_compiled_cmi: None,
            last_compiled_cmt: None,
            deps_dirty: false,
            is_type_dev: false,
        }
    }

    #[test]
    fn clears_screen_only_for_non_initial_interactive_rebuilds() {
        assert!(should_clear_screen(true, true, false, false));
        assert!(!should_clear_screen(true, true, false, true));
        assert!(!should_clear_screen(true, true, true, false));
        assert!(!should_clear_screen(true, false, false, false));
        assert!(!should_clear_screen(false, true, false, false));
    }

    #[test]
    fn carries_forward_implementation_warnings_for_matching_module_paths() {
        let previous = test_build_state(
            "ModuleA",
            test_module("src/ModuleA.res", Some("warning: impl"), None, None),
        );
        let mut next = test_build_state("ModuleA", test_module("src/ModuleA.res", None, None, None));

        carry_forward_compile_warnings(&previous, &mut next);

        let module = next.get_module("ModuleA").expect("module should exist");
        let SourceType::SourceFile(source_file) = &module.source_type else {
            panic!("expected source file module");
        };

        assert_eq!(
            source_file.implementation.compile_warnings.as_deref(),
            Some("warning: impl")
        );
        assert_eq!(source_file.implementation.compile_state, CompileState::Warning);
    }

    #[test]
    fn does_not_carry_forward_warnings_when_module_paths_change() {
        let previous = test_build_state(
            "ModuleA",
            test_module("src/ModuleA.res", Some("warning: impl"), None, None),
        );
        let mut next = test_build_state("ModuleA", test_module("src/ModuleARenamed.res", None, None, None));

        carry_forward_compile_warnings(&previous, &mut next);

        let module = next.get_module("ModuleA").expect("module should exist");
        let SourceType::SourceFile(source_file) = &module.source_type else {
            panic!("expected source file module");
        };

        assert_eq!(source_file.implementation.compile_warnings, None);
        assert_eq!(source_file.implementation.compile_state, CompileState::Success);
    }

    #[test]
    fn carries_forward_interface_warnings_for_matching_interface_paths() {
        let previous = test_build_state(
            "ModuleA",
            test_module(
                "src/ModuleA.res",
                None,
                Some("src/ModuleA.resi"),
                Some("warning: interface"),
            ),
        );
        let mut next = test_build_state(
            "ModuleA",
            test_module("src/ModuleA.res", None, Some("src/ModuleA.resi"), None),
        );

        carry_forward_compile_warnings(&previous, &mut next);

        let module = next.get_module("ModuleA").expect("module should exist");
        let SourceType::SourceFile(source_file) = &module.source_type else {
            panic!("expected source file module");
        };
        let interface = source_file.interface.as_ref().expect("interface should exist");

        assert_eq!(interface.compile_warnings.as_deref(), Some("warning: interface"));
        assert_eq!(interface.compile_state, CompileState::Warning);
    }
}
