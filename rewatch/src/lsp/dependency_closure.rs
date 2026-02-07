use ahash::{AHashMap, AHashSet};

use crate::build::build_types::Module;

/// Calculate the transitive closure of all **dependencies** for a given module.
///
/// This performs a downward traversal through `module.deps`:
/// - Module A depends on B and C
/// - B depends on D
/// - Result: {A, B, C, D}
///
/// This is the opposite of the "compile universe" expansion in `compile.rs`,
/// which walks **upward** through `module.dependents` (reverse dependencies).
///
/// ## Why this matters for LSP `did_save`
///
/// After the initial LSP build (`TypecheckOnly`), every module sits at
/// `CompilationStage::TypeChecked`. When a file is saved, we switch to
/// `TypecheckAndEmit` (target = `Built`), which means every `TypeChecked`
/// module would satisfy `needs_compile(Built)` and enter the compile universe.
/// In a large project, this compiles the **entire** codebase on the first save.
///
/// By computing the dependency closure, we can limit compilation to only the
/// modules that the saved file transitively imports — the minimal set needed
/// to produce correct JS output for that file.
pub fn get_dependency_closure(modules: &AHashMap<String, Module>, start: &str) -> AHashSet<String> {
    let mut closure = AHashSet::new();
    let mut stack = vec![start.to_string()];
    while let Some(name) = stack.pop() {
        if closure.insert(name.clone())
            && let Some(module) = modules.get(&name)
        {
            for dep in &module.deps {
                if !closure.contains(dep) {
                    stack.push(dep.clone());
                }
            }
        }
    }
    closure
}
