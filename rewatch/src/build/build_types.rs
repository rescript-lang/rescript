use crate::build::packages::{Namespace, Package};
use crate::config::Config;
use crate::helpers::StrippedVerbatimPath;
use crate::helpers::emojis::*;
use crate::project_context::ProjectContext;
use ahash::{AHashMap, AHashSet};
use blake3::Hash;
use std::{fmt, fmt::Display, ops::Deref, path::Path, path::PathBuf, time::SystemTime};

/// Tracks how far a module has progressed through compilation.
///
/// Each variant accumulates blake3 hashes of all artifacts produced up to
/// that stage, enabling the compile loop to make data-driven decisions
/// about what work is needed and whether output will affect dependents.
///
/// Lifecycle: `Source*Dirty → Parsed → TypeChecked → Built`
///            `Built/TypeChecked → DependencyDirty → TypeChecked → Built`
///
/// Error paths:
/// - `Source*Dirty → ParseError` when parsing fails (syntax error, etc.).
///   The module stays here until its source changes.
/// - `Parsed → CompileError` when compilation fails (e.g. due
///   to a dependency's API not matching). The AST hashes are preserved so
///   that when a dependency fix makes the module valid again, we know
///   full recompilation is needed.
#[derive(Debug, PartialEq, Eq)]
pub enum CompilationStage {
    /// Only the implementation (.res) source changed — needs reparse + recompile.
    SourceImplementationDirty,
    /// Only the interface (.resi) source changed — needs reparse + recompile.
    SourceInterfaceDirty,
    /// Both sides changed, or structural change (new module, deleted dep, mlmap, unknown).
    /// Needs full pipeline (reparse + recompile).
    SourceBothDirty,
    /// Parsing failed (syntax error, etc.). The module is inert until
    /// its source changes.
    ParseError,
    /// AST generated. Carries hashes of implementation (and optional interface)
    /// source + AST artifacts.
    Parsed {
        implementation_source_hash: Hash,
        implementation_ast_hash: Hash,
        interface_source_hash: Option<Hash>,
        interface_ast_hash: Option<Hash>,
        implementation_parse_warnings: Option<String>,
        interface_parse_warnings: Option<String>,
    },
    /// Compilation was attempted but failed (type error, etc.).
    /// Preserves parse hashes so the module can be recompiled when the
    /// error is resolved (e.g. a dependency fix).
    /// `compile_mode` records whether the failure occurred during a full
    /// compile (JS emission) or typecheck-only pass, so the LSP knows
    /// what to do when the error is resolved.
    CompileError {
        implementation_source_hash: Hash,
        implementation_ast_hash: Hash,
        interface_source_hash: Option<Hash>,
        interface_ast_hash: Option<Hash>,
        implementation_parse_warnings: Option<String>,
        interface_parse_warnings: Option<String>,
        compile_mode: CompileMode,
    },
    /// A dependency's interface changed but this module's source and AST are
    /// still valid. Only needs recompilation, NOT reparsing. Carries the same
    /// parse hashes as `Parsed` so the compile loop can use them directly.
    DependencyDirty {
        implementation_source_hash: Hash,
        implementation_ast_hash: Hash,
        interface_source_hash: Option<Hash>,
        interface_ast_hash: Option<Hash>,
        implementation_parse_warnings: Option<String>,
        interface_parse_warnings: Option<String>,
    },
    /// Type-checked only (.cmi/.cmt produced, no .cmj).
    /// Accumulates parse hashes + typecheck artifact hashes.
    TypeChecked {
        implementation_source_hash: Hash,
        implementation_ast_hash: Hash,
        interface_source_hash: Option<Hash>,
        interface_ast_hash: Option<Hash>,
        cmi_hash: Hash,
        cmt_hash: Hash,
        compiled_at: SystemTime,
        implementation_parse_warnings: Option<String>,
        interface_parse_warnings: Option<String>,
        compile_warnings: Option<String>,
    },
    /// Fully compiled (.cmi/.cmt/.cmj + JS produced).
    /// Accumulates all prior hashes + build artifact hashes.
    Built(FileBuiltState),
}

/// All hashes and metadata for a fully compiled module.
/// Used both in `CompilationStage::Built` and for snapshots when
/// preserving Built status across project rebuilds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileBuiltState {
    pub implementation_source_hash: Hash,
    pub implementation_ast_hash: Hash,
    pub interface_source_hash: Option<Hash>,
    pub interface_ast_hash: Option<Hash>,
    pub cmi_hash: Hash,
    pub cmt_hash: Hash,
    pub cmj_hash: Hash,
    pub compiled_at: SystemTime,
    pub implementation_parse_warnings: Option<String>,
    pub interface_parse_warnings: Option<String>,
    pub compile_warnings: Option<String>,
}

impl CompilationStage {
    /// Whether this module's source has changed and needs reparsing + recompilation.
    pub fn is_source_dirty(&self) -> bool {
        matches!(
            self,
            CompilationStage::SourceImplementationDirty
                | CompilationStage::SourceInterfaceDirty
                | CompilationStage::SourceBothDirty
        )
    }

    /// Whether this module failed compilation (type error, etc.).
    pub fn is_compile_error(&self) -> bool {
        matches!(self, CompilationStage::CompileError { .. })
    }

    /// Whether this module failed parsing (syntax error, etc.).
    pub fn is_parse_error(&self) -> bool {
        matches!(self, CompilationStage::ParseError)
    }

    /// The implementation parse warning text, if any.
    pub fn implementation_parse_warnings(&self) -> Option<&str> {
        match self {
            CompilationStage::Parsed {
                implementation_parse_warnings,
                ..
            }
            | CompilationStage::CompileError {
                implementation_parse_warnings,
                ..
            }
            | CompilationStage::DependencyDirty {
                implementation_parse_warnings,
                ..
            }
            | CompilationStage::TypeChecked {
                implementation_parse_warnings,
                ..
            } => implementation_parse_warnings.as_deref(),
            CompilationStage::Built(b) => b.implementation_parse_warnings.as_deref(),
            _ => None,
        }
    }

    /// The interface parse warning text, if any.
    pub fn interface_parse_warnings(&self) -> Option<&str> {
        match self {
            CompilationStage::Parsed {
                interface_parse_warnings,
                ..
            }
            | CompilationStage::CompileError {
                interface_parse_warnings,
                ..
            }
            | CompilationStage::DependencyDirty {
                interface_parse_warnings,
                ..
            }
            | CompilationStage::TypeChecked {
                interface_parse_warnings,
                ..
            } => interface_parse_warnings.as_deref(),
            CompilationStage::Built(b) => b.interface_parse_warnings.as_deref(),
            _ => None,
        }
    }

    /// Whether compilation produced warnings.
    pub fn has_compile_warnings(&self) -> bool {
        match self {
            CompilationStage::TypeChecked { compile_warnings, .. } => compile_warnings.is_some(),
            CompilationStage::Built(b) => b.compile_warnings.is_some(),
            _ => false,
        }
    }

    /// The compile warning text, if any.
    pub fn compile_warnings(&self) -> Option<&str> {
        match self {
            CompilationStage::TypeChecked { compile_warnings, .. } => compile_warnings.as_deref(),
            CompilationStage::Built(b) => b.compile_warnings.as_deref(),
            _ => None,
        }
    }

    /// Whether `self → new_stage` is a valid compilation stage transition.
    ///
    /// Valid transitions:
    /// ```text
    /// Source*Dirty     → Source*Dirty, ParseError, Parsed
    /// ParseError       → Source*Dirty, ParseError
    /// Parsed           → Source*Dirty, Parsed, CompileError, TypeChecked
    /// CompileError     → Source*Dirty, CompileError, TypeChecked
    /// DependencyDirty  → Source*Dirty, DependencyDirty, CompileError, TypeChecked
    /// TypeChecked      → Source*Dirty, CompileError, TypeChecked, Built, DependencyDirty
    /// Built            → Source*Dirty, CompileError, TypeChecked, DependencyDirty
    /// ```
    pub fn can_transition_to(&self, new_stage: &CompilationStage) -> bool {
        use CompilationStage::*;
        matches!(
            (self, new_stage),
            // Any stage can reset to any source-dirty variant:
            //   parse.rs     — source changed on disk, or mlmap changed
            //   compile.rs   — deleted deps, or hash computation failed
            //   build_types  — mark_file_parse_dirty sets the specific variant
            (_, SourceBothDirty)
            | (_, SourceImplementationDirty)
            | (_, SourceInterfaceDirty)
            // Source*Dirty → ParseError: parse.rs — AST generation failed (syntax error)
            | (SourceBothDirty | SourceImplementationDirty | SourceInterfaceDirty, ParseError)
            // ParseError → ParseError: parse.rs — re-parsed but still fails
            | (ParseError, ParseError)
            // Source*Dirty → Parsed: parse.rs — AST successfully generated
            //                        clean.rs — AST on disk is fresh, restoring from artifacts
            | (SourceBothDirty | SourceImplementationDirty | SourceInterfaceDirty, Parsed { .. })
            // Parsed → Parsed: parse.rs — re-parsed (e.g. re-entered parse phase)
            | (Parsed { .. }, Parsed { .. })
            // Parsed → CompileError: compile.rs — compilation failed (type error)
            | (Parsed { .. }, CompileError { .. })
            // Parsed → TypeChecked: compile.rs — compilation succeeded (typecheck or full)
            //                       clean.rs — restoring from artifacts
            | (Parsed { .. }, TypeChecked { .. })
            // CompileError → CompileError: compile.rs — still failing after retry
            | (CompileError { .. }, CompileError { .. })
            // CompileError → TypeChecked: compile.rs — dependency fix resolved the error
            | (CompileError { .. }, TypeChecked { .. })
            // DependencyDirty → DependencyDirty: compile.rs — re-marking (idempotent)
            | (DependencyDirty { .. }, DependencyDirty { .. })
            // DependencyDirty → CompileError: compile.rs — compilation failed
            | (DependencyDirty { .. }, CompileError { .. })
            // DependencyDirty → TypeChecked: compile.rs — typecheck succeeded
            | (DependencyDirty { .. }, TypeChecked { .. })
            // TypeChecked → CompileError: compile.rs — dependency API change broke it
            | (TypeChecked { .. }, CompileError { .. })
            // TypeChecked → TypeChecked: compile.rs — re-typechecked successfully
            | (TypeChecked { .. }, TypeChecked { .. })
            // TypeChecked → Built: compile.rs — FullCompile after TypecheckOnly
            //                      clean.rs — restoring from artifacts
            | (TypeChecked { .. }, Built(..))
            // TypeChecked → DependencyDirty: compile.rs — dependency interface changed
            | (TypeChecked { .. }, DependencyDirty { .. })
            // Built → CompileError: compile.rs — LSP save recompile, dependency broke it
            | (Built(..), CompileError { .. })
            // Built → TypeChecked: compile.rs — LSP save recompile in TypecheckOnly mode
            | (Built(..), TypeChecked { .. })
            // Built → DependencyDirty: compile.rs — dependency interface changed
            | (Built(..), DependencyDirty { .. })
        )
    }

    /// Whether this module needs work for the given compile mode.
    /// `ParseError` returns `false` — the module can't compile without a successful parse.
    /// `Source*Dirty` returns `true` — should not normally appear in a compile universe
    /// (it needs parsing first), but is kept for safety.
    pub fn needs_compile_for_mode(&self, mode: CompileMode) -> bool {
        match (self, mode) {
            (CompilationStage::Built(..), _) => false,
            (CompilationStage::ParseError, _) => false,
            (CompilationStage::TypeChecked { .. }, CompileMode::FullCompile) => true,
            (CompilationStage::TypeChecked { .. }, CompileMode::TypecheckOnly) => false,
            (
                CompilationStage::Parsed { .. }
                | CompilationStage::CompileError { .. }
                | CompilationStage::DependencyDirty { .. },
                _,
            ) => true,
            (
                CompilationStage::SourceImplementationDirty
                | CompilationStage::SourceInterfaceDirty
                | CompilationStage::SourceBothDirty,
                _,
            ) => true,
        }
    }

    /// When this module was last compiled, if typechecked or built.
    /// Used for incremental staleness checks: if a dependency was compiled
    /// more recently than a dependent, the dependent must be recompiled.
    pub fn compiled_at(&self) -> Option<SystemTime> {
        match self {
            CompilationStage::TypeChecked { compiled_at, .. } => Some(*compiled_at),
            CompilationStage::Built(b) => Some(b.compiled_at),
            _ => None,
        }
    }

    /// The compiled module interface hash, if available.
    /// Present only after successful typecheck or full build.
    pub fn cmi_hash(&self) -> Option<Hash> {
        match self {
            CompilationStage::TypeChecked { cmi_hash, .. } => Some(*cmi_hash),
            CompilationStage::Built(b) => Some(b.cmi_hash),
            _ => None,
        }
    }

    /// Whether this module has a separate interface (.resi) file,
    /// based on the presence of `interface_source_hash` in the compilation stage.
    pub fn has_interface(&self) -> bool {
        match self {
            CompilationStage::Parsed {
                interface_source_hash,
                ..
            }
            | CompilationStage::CompileError {
                interface_source_hash,
                ..
            }
            | CompilationStage::DependencyDirty {
                interface_source_hash,
                ..
            }
            | CompilationStage::TypeChecked {
                interface_source_hash,
                ..
            } => interface_source_hash.is_some(),
            CompilationStage::Built(b) => b.interface_source_hash.is_some(),
            CompilationStage::SourceImplementationDirty
            | CompilationStage::SourceInterfaceDirty
            | CompilationStage::SourceBothDirty
            | CompilationStage::ParseError => false,
        }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileMode {
    /// Typecheck only: .cmi/.cmt, uses -bs-cmi-only, no JS output.
    TypecheckOnly,
    /// Full compile: .cmi/.cmt/.cmj + JS output.
    FullCompile,
}

impl std::fmt::Display for CompileMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileMode::TypecheckOnly => write!(f, "typecheck_only"),
            CompileMode::FullCompile => write!(f, "full_compile"),
        }
    }
}

impl CompileMode {
    /// Whether this mode emits JavaScript output.
    pub fn emits_js(self) -> bool {
        matches!(self, CompileMode::FullCompile)
    }
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

/// Bundles the build concerns: where artifacts go, what compile mode,
/// and how progress is reported.
#[derive(Debug, Clone)]
pub struct BuildConfig {
    pub output: OutputTarget,
    pub mode: CompileMode,
    pub output_mode: OutputMode,
}

/// Which modules in the compile universe actually need recompilation.
#[derive(Debug)]
pub enum CompileFilter {
    /// Every module in the universe must be compiled unconditionally.
    All,
    /// Only the modules in this set definitely need compilation.
    /// Modules outside this set may be skipped when all their
    /// in-universe dependencies produced unchanged `.cmi` files.
    DirtyOnly(AHashSet<String>),
}

impl CompileFilter {
    /// Whether a specific module definitely needs compilation.
    pub fn needs_compile(&self, name: &str) -> bool {
        match self {
            CompileFilter::All => true,
            CompileFilter::DirtyOnly(set) => set.contains(name),
        }
    }
}

/// Parameters for `process_in_waves`: the module set, how to filter them,
/// and output configuration. Every field is consumed directly by the
/// compile loop — no scenario dispatch happens on these values.
#[derive(Debug)]
pub struct CompileParams {
    /// All modules in this compile cycle.
    pub modules: AHashSet<String>,
    /// Which modules need compilation vs. which may be skippable.
    pub filter: CompileFilter,
    /// TypecheckOnly vs FullCompile.
    pub mode: CompileMode,
    /// Whether to restrict dependent expansion to within the universe.
    pub scoped: bool,
    /// Where artifacts are written.
    pub output: OutputTarget,
    /// Whether to show progress bar ticks.
    pub show_progress: bool,
}

/// The raw output of a single `process_in_waves` run.
pub struct ProcessResult {
    pub compile_errors: String,
    pub compile_warnings: String,
    pub num_compiled_modules: usize,
    /// Modules that were in the compile universe but never attempted because
    /// the compile loop broke early (e.g. a dependency error aborted the loop
    /// before these modules' dependencies were satisfied).
    pub skipped_modules: AHashSet<String>,
    /// Diagnostics created directly by the build system (not parsed from bsc
    /// output). Used for errors that rewatch detects itself before bsc runs.
    pub raw_diagnostics: Vec<super::diagnostics::BscDiagnostic>,
    /// Source files to attach fallback diagnostics to when compile_errors is
    /// non-empty but produces no parsed or raw diagnostics.
    pub compile_error_files: Vec<std::path::PathBuf>,
}

impl ProcessResult {
    pub fn to_diagnostics(&self) -> Vec<super::diagnostics::BscDiagnostic> {
        use super::diagnostics::*;

        let mut diagnostics = self.raw_diagnostics.clone();

        let mut output = String::new();
        output.push_str(&self.compile_warnings);
        output.push_str(&self.compile_errors);
        let parsed = parse_compiler_output(&output);

        // If compile_errors is non-empty but neither raw nor parsed diagnostics
        // were produced, the error format was not recognised. Emit a fallback
        // diagnostic so it does not fail silently in the LSP.
        if !self.compile_errors.is_empty() && diagnostics.is_empty() && parsed.is_empty() {
            let cleaned = strip_ansi(&self.compile_errors);
            let message = cleaned.trim().to_owned();
            if !message.is_empty() {
                log::warn!("Compile error produced no diagnostics — emitting fallback: {message}");
                for file in &self.compile_error_files {
                    diagnostics.push(BscDiagnostic {
                        file: file.clone(),
                        range: SourceRange {
                            start: SourcePosition {
                                line: 1,
                                character: 1,
                            },
                            end: SourcePosition {
                                line: 1,
                                character: 1,
                            },
                        },
                        severity: Severity::Error,
                        message: message.clone(),
                    });
                }
            }
        }

        diagnostics.extend(parsed);
        diagnostics
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub path: PathBuf,
    pub last_modified: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub path: PathBuf,
    pub last_modified: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub implementation: Implementation,
    pub interface: Option<Interface>,
}

/// A regular source file module (.res/.resi) that goes through the full
/// compilation pipeline: Source*Dirty → Parsed → TypeChecked → Built.
#[derive(Debug)]
pub struct SourceFileModule {
    /// The module name (HashMap key), stored here so that methods like
    /// [`set_compilation_stage`] can include it in diagnostics and tracing
    /// without needing it passed in at every call site.
    pub module_name: String,
    /// The package this module belongs to, used to look up package-specific
    /// configuration such as namespace, compiler flags, and source paths.
    pub package_name: String,
    pub source_file: SourceFile,
    /// Whether this module lives under a `"type": "dev"` source directory.
    /// Dev modules are excluded from dependency include paths during compilation
    /// of non-dev consumers.
    pub is_type_dev: bool,
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
    /// How far this module has progressed through the build pipeline
    /// (Source*Dirty → Parsed → CompileError / TypeChecked → Built).
    ///
    /// Private — use [`compilation_stage()`] to read and
    /// [`set_compilation_stage()`] to write, which logs every transition
    /// via `tracing::debug!` for OTEL observability.
    compilation_stage: CompilationStage,
}

impl SourceFileModule {
    /// Create a new module in the `SourceBothDirty` stage.
    pub fn new(
        module_name: String,
        package_name: String,
        source_file: SourceFile,
        is_type_dev: bool,
    ) -> Self {
        Self {
            module_name,
            package_name,
            source_file,
            is_type_dev,
            deps: AHashSet::new(),
            dependents: AHashSet::new(),
            needs_dependencies_rescan: true,
            compilation_stage: CompilationStage::SourceBothDirty,
        }
    }

    /// Current compilation stage.
    pub fn compilation_stage(&self) -> &CompilationStage {
        &self.compilation_stage
    }

    /// Advance (or reset) the compilation stage, logging the transition.
    ///
    /// Panics in debug builds if the transition is invalid (see
    /// [`CompilationStage::can_transition_to`]).
    ///
    /// **WARNING**: In the LSP, `file_build` uses a take-remove-reinsert
    /// pattern for `BuildCommandState`. If this assertion fires inside
    /// `spawn_blocking`, the panic prevents the state from being
    /// re-inserted into `ProjectMap.states`, permanently losing the
    /// project and breaking all subsequent builds for that project.
    pub fn set_compilation_stage(&mut self, new_stage: CompilationStage) {
        debug_assert!(
            self.compilation_stage.can_transition_to(&new_stage),
            "invalid compilation_stage transition for {}: {:?} → {:?}",
            self.module_name,
            self.compilation_stage,
            new_stage,
        );
        tracing::debug!(
            module = %self.module_name,
            from = ?self.compilation_stage,
            to = ?new_stage,
            "compilation_stage transition"
        );
        self.compilation_stage = new_stage;
    }
}

/// A synthetic namespace map module (.mlmap). Compiled during the parse phase,
/// not through the regular compilation pipeline.
#[derive(Debug)]
pub struct MlMapModule {
    /// The package this module belongs to.
    pub package_name: String,
    /// Whether the mlmap file needs to be regenerated.
    pub parse_dirty: bool,
    /// Forward dependencies (modules listed in the namespace map).
    pub deps: AHashSet<String>,
    /// Modules that depend on this namespace map.
    pub dependents: AHashSet<String>,
}

/// A module in the build graph — either a real source file or a synthetic
/// namespace map. The enum-level split ensures that compilation-stage fields
/// only exist on source file modules, making it a compile error to access
/// them on mlmap modules.
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Module {
    SourceFile(SourceFileModule),
    MlMap(MlMapModule),
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Module::SourceFile(_) => write!(f, "SourceFile"),
            Module::MlMap(_) => write!(f, "MlMap"),
        }
    }
}

impl Module {
    pub fn is_mlmap(&self) -> bool {
        matches!(self, Module::MlMap(_))
    }

    pub fn package_name(&self) -> &str {
        match self {
            Module::SourceFile(m) => &m.package_name,
            Module::MlMap(m) => &m.package_name,
        }
    }

    pub fn deps(&self) -> &AHashSet<String> {
        match self {
            Module::SourceFile(m) => &m.deps,
            Module::MlMap(m) => &m.deps,
        }
    }

    pub fn deps_mut(&mut self) -> &mut AHashSet<String> {
        match self {
            Module::SourceFile(m) => &mut m.deps,
            Module::MlMap(m) => &mut m.deps,
        }
    }

    pub fn dependents(&self) -> &AHashSet<String> {
        match self {
            Module::SourceFile(m) => &m.dependents,
            Module::MlMap(m) => &m.dependents,
        }
    }

    pub fn dependents_mut(&mut self) -> &mut AHashSet<String> {
        match self {
            Module::SourceFile(m) => &mut m.dependents,
            Module::MlMap(m) => &mut m.dependents,
        }
    }

    pub fn get_interface(&self) -> Option<&Interface> {
        match self {
            Module::SourceFile(m) => m.source_file.interface.as_ref(),
            Module::MlMap(_) => None,
        }
    }

    /// Whether this module needs work for the given compile mode.
    /// MlMap modules never need compilation (they're handled in the parse phase).
    pub fn needs_compile_for_mode(&self, mode: CompileMode) -> bool {
        match self {
            Module::SourceFile(m) => m.compilation_stage().needs_compile_for_mode(mode),
            Module::MlMap(_) => false,
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
            .map(|(name, module)| (name.clone(), module.package_name().to_owned()))
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
            if let Module::SourceFile(m) = module {
                let package = self.build_state.packages.get(&m.package_name)?;

                let impl_path = package.path.join(&m.source_file.implementation.path);
                if canonicalized == impl_path {
                    return Some((module_name.clone(), m.package_name.clone(), false));
                }

                if let Some(interface) = &m.source_file.interface {
                    let iface_path = package.path.join(&interface.path);
                    if canonicalized == iface_path {
                        return Some((module_name.clone(), m.package_name.clone(), true));
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
        let (module_name, _, is_interface) = match self.find_module_for_file(file_path) {
            Some(result) => {
                tracing::debug!(
                    file = %file_path.display(),
                    module = %result.0,
                    is_interface = result.2,
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

        if let Module::SourceFile(m) = module {
            let package = self.build_state.packages.get(&m.package_name)?;
            let impl_path = package.path.join(&m.source_file.implementation.path);
            let mut matched = false;
            if canonicalized == impl_path {
                if let Ok(modified) = canonicalized.metadata().and_then(|md| md.modified()) {
                    m.source_file.implementation.last_modified = modified;
                }
                matched = true;
            } else if let Some(ref mut interface) = m.source_file.interface {
                let iface_path = package.path.join(&interface.path);
                if canonicalized == iface_path {
                    if let Ok(modified) = canonicalized.metadata().and_then(|md| md.modified()) {
                        interface.last_modified = modified;
                    }
                    matched = true;
                }
            }
            if matched {
                let new_stage = match m.compilation_stage() {
                    // Already partially dirty — if the other side now changed too, upgrade
                    CompilationStage::SourceImplementationDirty if is_interface => {
                        CompilationStage::SourceBothDirty
                    }
                    CompilationStage::SourceInterfaceDirty if !is_interface => {
                        CompilationStage::SourceBothDirty
                    }
                    // Already at the right dirty state or SourceBothDirty — no change needed
                    s if s.is_source_dirty() => return Some(module_name),
                    // Not dirty yet — set the specific variant
                    _ => {
                        if is_interface {
                            CompilationStage::SourceInterfaceDirty
                        } else {
                            CompilationStage::SourceImplementationDirty
                        }
                    }
                };
                m.set_compilation_stage(new_stage);
                return Some(module_name);
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
/// The distinction between `cmj_modules` (compiled JS) and `cmt_modules`
/// (typed trees) is important: a TypecheckOnly build produces `.cmi`/`.cmt` but
/// no `.cmj`. When resuming from such a build, modules with `.cmi` but no `.cmj`
/// must be marked as `TypeChecked` rather than `Built` so they get fully compiled
/// on the next save.
#[derive(Debug)]
pub struct CompileAssetsState {
    pub ast_modules: AHashMap<PathBuf, AstModule>,
    pub cmj_modules: AHashMap<String, SystemTime>,
    pub cmt_modules: AHashMap<String, SystemTime>,
    pub ast_rescript_file_locations: AHashSet<PathBuf>,
    pub rescript_file_locations: AHashSet<PathBuf>,
}

#[derive(Debug, Clone)]
pub enum IncrementalBuildErrorKind {
    SourceFileParseError,
    CompileError(Option<String>),
}

#[derive(Debug, Clone)]
pub struct IncrementalBuildError {
    pub output_mode: OutputMode,
    pub kind: IncrementalBuildErrorKind,
    pub diagnostics: Vec<super::diagnostics::BscDiagnostic>,
    /// The set of module names that participated in this compile cycle.
    /// Boxed (along with `skipped_modules`) to keep `IncrementalBuildError`
    /// under clippy's `result_large_err` size threshold.
    pub modules: Box<AHashSet<String>>,
    /// Modules that were in the closure but never attempted by bsc because
    /// the compile loop broke early due to a dependency error.
    pub skipped_modules: Box<AHashSet<String>>,
}

#[derive(Debug, Clone)]
pub struct IncrementalBuildResult {
    pub diagnostics: Vec<super::diagnostics::BscDiagnostic>,
    /// The set of module names that participated in this compile cycle.
    pub modules: AHashSet<String>,
}

impl fmt::Display for IncrementalBuildError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let plain = self.output_mode.plain_output();
        match &self.kind {
            IncrementalBuildErrorKind::SourceFileParseError => {
                if plain {
                    write!(f, "{LINE_CLEAR}  Could not parse Source Files",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Could not parse Source Files",)
                }
            }
            IncrementalBuildErrorKind::CompileError(Some(e)) => {
                if plain {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. Error: {e}",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. Error: {e}",)
                }
            }
            IncrementalBuildErrorKind::CompileError(None) => {
                if plain {
                    write!(f, "{LINE_CLEAR}  Failed to Compile. See Errors Above",)
                } else {
                    write!(f, "{LINE_CLEAR}  {CROSS}Failed to Compile. See Errors Above",)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_hash() -> Hash {
        blake3::hash(b"test")
    }

    fn make_parsed() -> CompilationStage {
        CompilationStage::Parsed {
            implementation_source_hash: dummy_hash(),
            implementation_ast_hash: dummy_hash(),
            interface_source_hash: None,
            interface_ast_hash: None,
            implementation_parse_warnings: None,
            interface_parse_warnings: None,
        }
    }

    #[test]
    fn is_source_dirty_matches_all_three_variants() {
        assert!(CompilationStage::SourceImplementationDirty.is_source_dirty());
        assert!(CompilationStage::SourceInterfaceDirty.is_source_dirty());
        assert!(CompilationStage::SourceBothDirty.is_source_dirty());
        assert!(!CompilationStage::ParseError.is_source_dirty());
        assert!(!make_parsed().is_source_dirty());
    }

    #[test]
    fn transition_any_stage_to_any_dirty_variant() {
        let stages = vec![
            CompilationStage::SourceImplementationDirty,
            CompilationStage::SourceInterfaceDirty,
            CompilationStage::SourceBothDirty,
            CompilationStage::ParseError,
            make_parsed(),
        ];
        let dirty_targets = vec![
            CompilationStage::SourceImplementationDirty,
            CompilationStage::SourceInterfaceDirty,
            CompilationStage::SourceBothDirty,
        ];
        for from in &stages {
            for to in &dirty_targets {
                assert!(
                    from.can_transition_to(to),
                    "{from:?} should be able to transition to {to:?}"
                );
            }
        }
    }

    #[test]
    fn transition_dirty_to_parsed_and_parse_error() {
        let dirty_variants = vec![
            CompilationStage::SourceImplementationDirty,
            CompilationStage::SourceInterfaceDirty,
            CompilationStage::SourceBothDirty,
        ];
        let parsed = make_parsed();
        let parse_error = CompilationStage::ParseError;
        for dirty in &dirty_variants {
            assert!(
                dirty.can_transition_to(&parsed),
                "{dirty:?} → Parsed should be valid"
            );
            assert!(
                dirty.can_transition_to(&parse_error),
                "{dirty:?} → ParseError should be valid"
            );
        }
    }

    #[test]
    fn transition_partial_dirty_to_both_dirty_is_valid() {
        assert!(
            CompilationStage::SourceImplementationDirty.can_transition_to(&CompilationStage::SourceBothDirty)
        );
        assert!(CompilationStage::SourceInterfaceDirty.can_transition_to(&CompilationStage::SourceBothDirty));
    }

    #[test]
    fn needs_compile_for_all_dirty_variants() {
        let dirty_variants = vec![
            CompilationStage::SourceImplementationDirty,
            CompilationStage::SourceInterfaceDirty,
            CompilationStage::SourceBothDirty,
        ];
        for dirty in &dirty_variants {
            assert!(
                dirty.needs_compile_for_mode(CompileMode::FullCompile),
                "{dirty:?} should need compile for FullCompile"
            );
            assert!(
                dirty.needs_compile_for_mode(CompileMode::TypecheckOnly),
                "{dirty:?} should need compile for TypecheckOnly"
            );
        }
    }
}
