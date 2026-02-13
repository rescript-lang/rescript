use crate::build::build_types::{BuildCommandState, CompileMode, OutputTarget, SourceType};
use crate::build::compile;
use crate::build::parse;
use std::path::{Path, PathBuf};

/// Returns `true` for `.res` or `.resi` source files.
pub fn is_rescript_source(path: &Path) -> bool {
    matches!(path.extension().and_then(|e| e.to_str()), Some("res" | "resi"))
}

/// Returns `true` for `rescript.json` config files.
pub fn is_rescript_config(path: &Path) -> bool {
    path.file_name()
        .and_then(|f| f.to_str())
        .is_some_and(|f| f == "rescript.json")
}

/// Returns `true` when the path points to a file the LSP should process:
/// `.res`, `.resi` sources or `rescript.json` config.
pub fn is_rescript_file(path: &Path) -> bool {
    is_rescript_source(path) || is_rescript_config(path)
}

/// All bsc arguments needed to typecheck a file.
///
/// Contains pre-computed compiler and parser args so callers don't need to
/// look up module metadata or call `compile::compiler_args` /
/// `parse::parser_args` themselves.
pub struct TypecheckArgs {
    pub module_name: String,
    /// Relative source path (e.g. `src/Library.res`).
    pub source_path: PathBuf,
    /// Compiler args with the source path as last arg (for single-file typecheck).
    pub compiler_args: Vec<String>,
    /// Parser args for `bsc -bs-ast` (produces `.ast` file).
    pub parser_args: Vec<String>,
    /// Expected `.ast` output path (relative to build dir).
    pub ast_path: PathBuf,
    /// Compiler args with the `.ast` path as last arg (for typecheck-from-ast).
    pub ast_compiler_args: Vec<String>,
    /// Absolute path to bsc.
    pub bsc_path: PathBuf,
    /// Absolute path to the build directory (e.g. `lib/lsp/`).
    pub build_path_abs: PathBuf,
    /// Absolute path to the package root (for diagnostic path remapping).
    pub package_path: PathBuf,
}

/// Extension trait adding typecheck argument computation to `BuildCommandState`.
pub trait BuildCommandStateExt {
    fn get_typecheck_args(
        &self,
        file_path: &Path,
        content: &str,
        output: OutputTarget,
        mode: CompileMode,
    ) -> Option<TypecheckArgs>;
}

impl BuildCommandStateExt for BuildCommandState {
    /// Compute all bsc arguments needed to typecheck a file.
    ///
    /// This is the single source of truth for computing parser and compiler args
    /// from an existing `BuildCommandState`. Used by LSP handlers that need to
    /// invoke bsc without holding the projects lock.
    fn get_typecheck_args(
        &self,
        file_path: &Path,
        content: &str,
        output: OutputTarget,
        mode: CompileMode,
    ) -> Option<TypecheckArgs> {
        let (module_name, package_name, is_interface) = self.find_module_for_file(file_path)?;
        let module = self.build_state.modules.get(&module_name)?;
        let package = self.build_state.packages.get(&package_name)?;

        let source_file = match &module.source_type {
            SourceType::SourceFile(sf) => sf,
            _ => return None,
        };

        let source_path = if is_interface {
            source_file.interface.as_ref().map(|i| i.path.clone())?
        } else {
            source_file.implementation.path.clone()
        };

        let has_interface = source_file.interface.is_some();
        let build_path = package.get_build_path_for_output(output);
        let build_path_abs = build_path.canonicalize().ok()?;

        let compiler_args = compile::compiler_args(
            &package.config,
            &source_path,
            &source_file.implementation.path,
            is_interface,
            has_interface,
            &self.build_state.project_context,
            &self.build_state.packages,
            module.is_type_dev,
            package.is_local_dep,
            self.get_warn_error_override(),
            output,
            mode,
        )
        .ok()?;

        let (ast_path, parser_args) = parse::parser_args(
            &self.build_state.project_context,
            &package.config,
            &source_path,
            content,
            package.is_local_dep,
            self.get_warn_error_override(),
        )
        .ok()?;

        let ast_compiler_args = compile::compiler_args(
            &package.config,
            &ast_path,
            &source_file.implementation.path,
            is_interface,
            has_interface,
            &self.build_state.project_context,
            &self.build_state.packages,
            module.is_type_dev,
            package.is_local_dep,
            self.get_warn_error_override(),
            output,
            mode,
        )
        .ok()?;

        Some(TypecheckArgs {
            module_name,
            source_path,
            compiler_args,
            parser_args,
            ast_path,
            ast_compiler_args,
            bsc_path: self.build_state.compiler_info.bsc_path.clone(),
            build_path_abs,
            package_path: package.path.clone(),
        })
    }
}
