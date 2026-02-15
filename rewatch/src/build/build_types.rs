use crate::build::packages::{Namespace, Package};
use crate::config::Config;
use crate::helpers::StrippedVerbatimPath;
use crate::project_context::ProjectContext;
use ahash::{AHashMap, AHashSet};
use blake3::Hash;
use std::{fmt::Display, ops::Deref, path::Path, path::PathBuf, time::SystemTime};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseState {
    Pending,
    ParseError,
    Warning,
    Success,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileState {
    Pending,
    Error,
    Warning,
    Success,
}
/// Tracks how far a module has progressed through compilation.
///
/// The ordering matters: `Dirty < TypeChecked < Built`. A module needs
/// compilation when its stage is below the target stage for the current
/// build mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompilationStage {
    /// Not yet compiled, or source changed — needs full pipeline.
    Dirty,
    /// Type-checked only (.cmi/.cmt produced, no .cmj).
    TypeChecked,
    /// Fully compiled (.cmi/.cmt/.cmj + JS produced).
    Built,
}

impl CompilationStage {
    /// Whether this module needs compilation to reach the given target stage.
    pub fn needs_compile(self, target: CompilationStage) -> bool {
        self < target
    }
}

/// Where build artifacts are written.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputTarget {
    /// CLI builds: lib/bs, lib/ocaml
    Standard,
    /// LSP builds: lib/lsp, lib/lsp-ocaml
    Lsp,
}

impl OutputTarget {
    /// Whether this output target uses the LSP build path (lib/lsp).
    pub fn is_lsp(self) -> bool {
        matches!(self, OutputTarget::Lsp)
    }
}

impl std::fmt::Display for OutputTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputTarget::Standard => write!(f, "standard"),
            OutputTarget::Lsp => write!(f, "lsp"),
        }
    }
}

/// What the compiler produces.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompileMode {
    /// Typecheck only: .cmi/.cmt, uses -bs-cmi-only, no JS output.
    TypecheckOnly,
    /// Full compile: .cmi/.cmt/.cmj + JS output.
    FullCompile,
}

impl CompileMode {
    /// Whether this mode emits JavaScript output.
    pub fn emits_js(self) -> bool {
        matches!(self, CompileMode::FullCompile)
    }

    /// The compilation stage a module reaches under this mode.
    pub fn target_stage(self) -> CompilationStage {
        match self {
            CompileMode::FullCompile => CompilationStage::Built,
            CompileMode::TypecheckOnly => CompilationStage::TypeChecked,
        }
    }
}

/// Which modules participate in compilation and how.
///
/// Each variant encodes the compile mode and scoping behavior.
/// For full builds, the compile universe is derived from all dirty modules.
/// For scoped builds, the anchor module names are carried in the variant and
/// the universe is derived from their dependency/dependent closure.
#[derive(Debug, Clone, PartialEq)]
pub enum CompileScope {
    /// CLI/watcher: all dirty modules with dependent expansion, full compile with JS output.
    FullBuild,
    /// LSP initial/project build: all dirty modules with dependent expansion, typecheck only.
    FullTypecheck,
    /// LSP save (step 1): compile the dependency closure of these modules to JS.
    CompileDependencies(AHashSet<String>),
    /// LSP save (step 2): typecheck the dependent closure of these modules, no JS.
    TypecheckDependents(AHashSet<String>),
}

impl CompileScope {
    /// Short label for tracing attributes.
    pub fn label(&self) -> &'static str {
        match self {
            CompileScope::FullBuild => "full_build",
            CompileScope::FullTypecheck => "full_typecheck",
            CompileScope::CompileDependencies(_) => "compile_dependencies",
            CompileScope::TypecheckDependents(_) => "typecheck_dependents",
        }
    }

    /// The compile mode implied by this scope variant.
    pub fn mode(&self) -> CompileMode {
        match self {
            CompileScope::FullBuild | CompileScope::CompileDependencies(_) => CompileMode::FullCompile,
            CompileScope::FullTypecheck | CompileScope::TypecheckDependents(_) => CompileMode::TypecheckOnly,
        }
    }

    /// Whether this is a scoped build (restricted to a specific set of modules).
    pub fn is_scoped(&self) -> bool {
        matches!(
            self,
            CompileScope::CompileDependencies(_) | CompileScope::TypecheckDependents(_)
        )
    }
}

/// The set of modules participating in a compile cycle.
///
/// `originally_dirty` are modules whose source changed (parse-dirty,
/// expired deps, deleted deps). `all` is the full set: originally dirty
/// modules plus their transitive dependents. A module in `all` but not
/// in `originally_dirty` only needs recompilation if one of its
/// in-universe dependencies produced a changed `.cmi`.
#[derive(Debug)]
pub struct CompileUniverse {
    /// All modules that participate in this compile cycle
    /// (dirty modules + their transitive dependents).
    pub all: AHashSet<String>,
    /// The subset of `all` that were directly dirty
    /// (source changed, deps expired/deleted).
    pub originally_dirty: AHashSet<String>,
}

/// How the build reports progress to the user.
#[derive(Debug, Clone)]
pub enum OutputMode {
    /// CLI/watcher: show progress bars and compilation summaries.
    Standard {
        show_progress: bool,
        plain_output: bool,
        /// Show config warnings (unsupported fields, etc.) on this build.
        /// Typically true for the first build, false for subsequent rebuilds.
        initial_build: bool,
    },
    /// LSP: no user-facing output.
    Silent,
}

impl OutputMode {
    pub fn is_silent(&self) -> bool {
        matches!(self, OutputMode::Silent)
    }

    pub fn show_progress(&self) -> bool {
        match self {
            OutputMode::Standard { show_progress, .. } => *show_progress,
            OutputMode::Silent => false,
        }
    }

    pub fn plain_output(&self) -> bool {
        match self {
            OutputMode::Standard { plain_output, .. } => *plain_output,
            OutputMode::Silent => true,
        }
    }

    pub fn initial_build(&self) -> bool {
        match self {
            OutputMode::Standard { initial_build, .. } => *initial_build,
            OutputMode::Silent => false,
        }
    }
}

/// Bundles the build concerns: where artifacts go, which modules
/// participate, and how progress is reported.
#[derive(Debug, Clone)]
pub struct BuildConfig {
    pub output: OutputTarget,
    pub scope: CompileScope,
    pub output_mode: OutputMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub path: PathBuf,
    pub parse_state: ParseState,
    pub compile_state: CompileState,
    pub last_modified: SystemTime,
    pub parse_dirty: bool,
    /// Compiler warning output (from bsc stderr) stored for re-emission
    /// during incremental builds when this module is not recompiled.
    /// Written to `.compiler.log` on each build cycle.
    pub compile_warnings: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub path: PathBuf,
    pub parse_state: ParseState,
    pub compile_state: CompileState,
    pub last_modified: SystemTime,
    pub parse_dirty: bool,
    /// Compiler warning output (from bsc stderr) stored for re-emission
    /// during incremental builds when this module is not recompiled.
    /// Written to `.compiler.log` on each build cycle.
    pub compile_warnings: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub implementation: Implementation,
    pub interface: Option<Interface>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MlMap {
    pub parse_dirty: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    SourceFile(SourceFile),
    MlMap(MlMap),
}

impl Display for SourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceType::SourceFile(_) => write!(f, "SourceFile"),
            SourceType::MlMap(_) => write!(f, "MlMap"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    // -- Module identity (immutable after construction) --
    /// The package this module belongs to, used to look up package-specific
    /// configuration such as namespace, compiler flags, and source paths.
    pub package_name: String,
    /// Discriminates between regular source files (.res/.resi) and generated
    /// namespace map files (.mlmap). Drives branching in parsing, dependency
    /// scanning, compilation, and cleanup.
    pub source_type: SourceType,
    /// Whether this module lives under a `"type": "dev"` source directory.
    /// Dev modules are excluded from dependency include paths during compilation
    /// of non-dev consumers.
    pub is_type_dev: bool,

    // -- Dependency graph (mutated in `deps.rs` after each AST rescan) --
    /// Modules this module needs (forward dependencies). Reassigned in
    /// `deps.rs` when the AST is rescanned. Used for compilation ordering,
    /// cycle detection, and marking modules dirty when a dependency is removed.
    pub deps: AHashSet<String>,
    /// Modules that use this module (reverse dependencies). Grown via
    /// `insert` in `deps.rs` as the inverse of `deps`. Used to expand the
    /// compile universe: when this module changes, every dependent needs
    /// recompilation.
    pub dependents: AHashSet<String>,
    /// Whether this module's dependency list needs to be re-scanned from
    /// its AST. Set to `true` in `parse.rs` when the source changes,
    /// and reset to `false` in `deps.rs` after dependencies are resolved.
    pub needs_dependencies_rescan: bool,

    // -- Build status (mutated throughout the build pipeline) --
    /// How far this module has progressed through the build pipeline
    /// (Dirty → TypeChecked → Built). Set to `Dirty` in `parse.rs` and
    /// `compile.rs` (when dependencies are invalidated), advanced to the
    /// target stage in `compile.rs` after successful compilation, and
    /// restored to `Built` in `clean.rs` when existing artifacts are fresh.
    pub compilation_stage: CompilationStage,
    /// Timestamp of the last produced .cmi (compiled interface) file.
    /// Updated in `compile.rs` after compilation and in `clean.rs` from
    /// existing artifact timestamps.
    pub last_compiled_cmi: Option<SystemTime>,
    /// Timestamp of the last produced .cmt (compiled typed tree) file.
    /// Updated in `compile.rs` after compilation and in `clean.rs` from
    /// existing artifact timestamps. Used for incremental staleness checks:
    /// if a dependency's .cmt is newer than a dependent's .cmt, the
    /// dependent must be recompiled.
    pub last_compiled_cmt: Option<SystemTime>,
}

impl Module {
    pub fn is_mlmap(&self) -> bool {
        matches!(self.source_type, SourceType::MlMap(_))
    }

    pub fn get_interface(&self) -> &Option<Interface> {
        match &self.source_type {
            SourceType::SourceFile(source_file) => &source_file.interface,
            _ => &None,
        }
    }
}

/// Core build state containing all the essential data needed for compilation.
/// This is the minimal state required for basic build operations like cleaning.
/// Used by commands that don't need command-line specific overrides (e.g., `clean`).
#[derive(Debug)]
pub struct BuildState {
    pub project_context: ProjectContext,
    pub modules: AHashMap<String, Module>,
    pub packages: AHashMap<String, Package>,
    pub module_names: AHashSet<String>,
    pub deleted_modules: AHashSet<String>,
    pub compiler_info: CompilerInfo,
    pub deps_initialized: bool,
}

/// Extended build state that includes command-line specific overrides.
/// Wraps `BuildState` and adds command-specific data like warning overrides.
/// Used by commands that need to respect CLI flags (e.g., `build`, `watch`).
///
/// The separation exists because:
/// - `clean` command only needs core build data, no CLI overrides
/// - `build`/`watch` commands need both core data AND CLI overrides
/// - This prevents the "code smell" of optional fields that are None for some commands
#[derive(Debug)]
pub struct BuildCommandState {
    pub build_state: BuildState,
    // Command-line --warn-error flag override (takes precedence over rescript.json config)
    pub warn_error_override: Option<String>,
}

#[derive(Debug, Clone)]
pub struct CompilerInfo {
    pub bsc_path: PathBuf,
    pub bsc_hash: Hash,
    pub runtime_path: PathBuf,
}

impl BuildState {
    pub fn get_package(&self, package_name: &str) -> Option<&Package> {
        self.packages.get(package_name)
    }

    pub fn get_module(&self, module_name: &str) -> Option<&Module> {
        self.modules.get(module_name)
    }

    pub fn new(
        project_context: ProjectContext,
        packages: AHashMap<String, Package>,
        compiler: CompilerInfo,
    ) -> Self {
        Self {
            project_context,
            module_names: AHashSet::new(),
            modules: AHashMap::new(),
            packages,
            deleted_modules: AHashSet::new(),
            compiler_info: compiler,
            deps_initialized: false,
        }
    }

    pub fn insert_module(&mut self, module_name: &str, module: Module) {
        self.modules.insert(module_name.to_owned(), module);
        self.module_names.insert(module_name.to_owned());
    }

    pub fn get_root_config(&self) -> &Config {
        self.project_context.get_root_config()
    }
}

impl BuildCommandState {
    pub fn new(
        project_context: ProjectContext,
        packages: AHashMap<String, Package>,
        compiler: CompilerInfo,
        warn_error_override: Option<String>,
    ) -> Self {
        Self {
            build_state: BuildState::new(project_context, packages, compiler),
            warn_error_override,
        }
    }

    pub fn get_warn_error_override(&self) -> Option<String> {
        self.warn_error_override.clone()
    }

    pub fn module_name_package_pairs(&self) -> Vec<(String, String)> {
        self.build_state
            .modules
            .iter()
            .map(|(name, module)| (name.clone(), module.package_name.clone()))
            .collect()
    }

    /// Find the module and package name for a given file path.
    /// Returns `(module_name, package_name, is_interface)` if found.
    pub fn find_module_for_file(&self, file_path: &Path) -> Option<(String, String, bool)> {
        let canonicalized = file_path
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
            .ok()?;

        for (module_name, module) in &self.build_state.modules {
            let package = self.build_state.packages.get(&module.package_name)?;

            if let SourceType::SourceFile(source_file) = &module.source_type {
                let impl_path = package.path.join(&source_file.implementation.path);
                if canonicalized == impl_path {
                    return Some((module_name.clone(), module.package_name.clone(), false));
                }

                if let Some(interface) = &source_file.interface {
                    let iface_path = package.path.join(&interface.path);
                    if canonicalized == iface_path {
                        return Some((module_name.clone(), module.package_name.clone(), true));
                    }
                }
            }
        }

        None
    }

    /// Find the module matching the given file path and mark it as parse-dirty.
    /// Updates `last_modified` from the file's metadata.
    /// Returns the module name if found and marked, `None` otherwise.
    pub fn mark_file_parse_dirty(&mut self, file_path: &Path) -> Option<String> {
        let (module_name, _, _) = match self.find_module_for_file(file_path) {
            Some(result) => {
                tracing::debug!(
                    file = %file_path.display(),
                    module = %result.0,
                    "mark_file_parse_dirty: resolved file to module"
                );
                result
            }
            None => {
                tracing::warn!(
                    file = %file_path.display(),
                    "mark_file_parse_dirty: could not resolve file to any module"
                );
                return None;
            }
        };

        let canonicalized = file_path
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
            .ok()?;

        let module = self.build_state.modules.get_mut(&module_name)?;

        if let SourceType::SourceFile(ref mut source_file) = module.source_type {
            let package = self.build_state.packages.get(&module.package_name)?;
            let impl_path = package.path.join(&source_file.implementation.path);
            if canonicalized == impl_path {
                if let Ok(modified) = canonicalized.metadata().and_then(|m| m.modified()) {
                    source_file.implementation.last_modified = modified;
                }
                source_file.implementation.parse_dirty = true;
                return Some(module_name);
            }

            if let Some(ref mut interface) = source_file.interface {
                let iface_path = package.path.join(&interface.path);
                if canonicalized == iface_path {
                    if let Ok(modified) = canonicalized.metadata().and_then(|m| m.modified()) {
                        interface.last_modified = modified;
                    }
                    interface.parse_dirty = true;
                    return Some(module_name);
                }
            }
        }

        None
    }
}

// Implement Deref to automatically delegate method calls to the inner BuildState
impl Deref for BuildCommandState {
    type Target = BuildState;

    fn deref(&self) -> &Self::Target {
        &self.build_state
    }
}

// Implement DerefMut to allow mutable access to the inner BuildState
impl std::ops::DerefMut for BuildCommandState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.build_state
    }
}

#[derive(Debug)]
pub struct AstModule {
    pub module_name: String,
    pub package_name: String,
    pub namespace: Namespace,
    pub last_modified: SystemTime,
    pub ast_file_path: PathBuf,
    pub is_root: bool,
    pub suffix: String,
}

/// Snapshot of build artifacts on disk, read at the start of a build to determine
/// which modules need recompilation. Each map tracks the last-modified time of a
/// specific artifact kind, keyed by module name (or source path for AST files).
///
/// The distinction between `cmi_modules` (type interfaces) and `cmj_modules`
/// (compiled JS) is important: a TypecheckOnly build produces `.cmi`/`.cmt` but
/// no `.cmj`. When resuming from such a build, modules with `.cmi` but no `.cmj`
/// must be marked as `TypeChecked` rather than `Built` so they get fully compiled
/// on the next save.
#[derive(Debug)]
pub struct CompileAssetsState {
    pub ast_modules: AHashMap<PathBuf, AstModule>,
    pub cmi_modules: AHashMap<String, SystemTime>,
    pub cmj_modules: AHashMap<String, SystemTime>,
    pub cmt_modules: AHashMap<String, SystemTime>,
    pub ast_rescript_file_locations: AHashSet<PathBuf>,
    pub rescript_file_locations: AHashSet<PathBuf>,
}
