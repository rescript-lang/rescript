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
/// `BuildProfile`.
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
    /// The target stage for a given build profile.
    pub fn target_for(profile: BuildProfile) -> Self {
        match profile {
            BuildProfile::Standard | BuildProfile::TypecheckAndEmit => CompilationStage::Built,
            BuildProfile::TypecheckOnly => CompilationStage::TypeChecked,
        }
    }

    /// Whether this module needs compilation to reach the given target stage.
    pub fn needs_compile(self, target: CompilationStage) -> bool {
        self < target
    }
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
    pub source_type: SourceType,
    pub deps: AHashSet<String>,
    pub dependents: AHashSet<String>,
    pub package_name: String,
    pub compilation_stage: CompilationStage,
    pub last_compiled_cmi: Option<SystemTime>,
    pub last_compiled_cmt: Option<SystemTime>,
    pub deps_dirty: bool,
    pub is_type_dev: bool,
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

/// Controls which artifacts the build produces and where they go.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuildProfile {
    /// Normal build: emit JS to lib/bs, lib/js, lib/es6
    Standard,
    /// LSP: only type information (.cmi/.cmt) to lib/lsp, skip JS generation
    TypecheckOnly,
    /// LSP: type information + JS output, artifacts in lib/lsp
    TypecheckAndEmit,
}

impl BuildProfile {
    /// Whether this profile emits JavaScript output.
    pub fn emits_js(self) -> bool {
        matches!(self, BuildProfile::Standard | BuildProfile::TypecheckAndEmit)
    }

    /// Whether this profile uses the LSP build path (lib/lsp).
    pub fn is_lsp(self) -> bool {
        matches!(self, BuildProfile::TypecheckOnly | BuildProfile::TypecheckAndEmit)
    }
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

    /// Find the module matching the given file path and mark it as parse-dirty.
    /// Updates `last_modified` from the file's metadata.
    /// Returns the module name if found and marked, `None` otherwise.
    pub fn mark_file_parse_dirty(&mut self, file_path: &Path) -> Option<String> {
        let canonicalized = match file_path
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path)
        {
            Ok(p) => p,
            Err(_) => return None,
        };

        let module_package_pairs = self.module_name_package_pairs();

        for (module_name, package_name) in module_package_pairs {
            let package = match self.build_state.packages.get(&package_name) {
                Some(p) => p,
                None => continue,
            };

            let module = match self.build_state.modules.get_mut(&module_name) {
                Some(m) => m,
                None => continue,
            };

            if let SourceType::SourceFile(ref mut source_file) = module.source_type {
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

#[derive(Debug)]
pub struct CompileAssetsState {
    pub ast_modules: AHashMap<PathBuf, AstModule>,
    pub cmi_modules: AHashMap<String, SystemTime>,
    pub cmt_modules: AHashMap<String, SystemTime>,
    pub ast_rescript_file_locations: AHashSet<PathBuf>,
    pub rescript_file_locations: AHashSet<PathBuf>,
}
