use ahash::{AHashMap, AHashSet};

use crate::build::build_types::Module;

/// Calculate the transitive closure of all **dependencies** for the given modules.
///
/// This performs a downward traversal through `module.deps` from each start module:
/// - Module A depends on B and C
/// - B depends on D
/// - Result: {A, B, C, D}
///
/// When multiple start modules are given, the result is the union of their
/// individual dependency closures. This is used for batched saves where
/// multiple files change at once.
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
/// modules that the saved files transitively import — the minimal set needed
/// to produce correct JS output for those files.
pub fn get_dependency_closure(
    modules: &AHashMap<String, Module>,
    starts: AHashSet<String>,
) -> AHashSet<String> {
    let mut closure = AHashSet::new();
    let mut stack: Vec<String> = starts.into_iter().collect();
    while let Some(name) = stack.pop() {
        if closure.insert(name.clone())
            && let Some(module) = modules.get(&name)
        {
            for dep in module.deps() {
                if !closure.contains(dep) {
                    stack.push(dep.clone());
                }
            }
        }
    }
    closure
}

/// Calculate the transitive closure of all **dependents** for the given modules.
///
/// This performs an upward traversal through `module.dependents` from each
/// start module:
/// - Module C is imported by B
/// - B is imported by A
/// - Result: {B, A}
///
/// The starting modules are **excluded** from the result — they are handled
/// separately by the caller (e.g. compiled with `TypecheckAndEmit`).
///
/// When multiple start modules are given, the result is the union of their
/// individual dependent closures (minus all start modules).
///
/// ## Why this matters for LSP `did_save`
///
/// When files are saved, their public APIs (.cmi) may have changed. Modules
/// that import the saved files need to be re-typechecked to surface errors
/// caused by API changes. However, they do NOT need JS output — only
/// diagnostics matter. JS emission happens when those files are themselves
/// saved.
pub fn get_dependent_closure(
    modules: &AHashMap<String, Module>,
    starts: AHashSet<String>,
) -> AHashSet<String> {
    let mut closure = AHashSet::new();
    let mut stack: Vec<String> = starts.iter().cloned().collect();
    while let Some(name) = stack.pop() {
        if let Some(module) = modules.get(&name) {
            for dep in module.dependents() {
                if !starts.contains(dep) && closure.insert(dep.clone()) {
                    stack.push(dep.clone());
                }
            }
        }
    }
    closure
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build::build_types::{MlMapModule, Module};

    /// Create a minimal Module with given deps and dependents.
    fn make_module(deps: &[&str], dependents: &[&str]) -> Module {
        Module::MlMap(MlMapModule {
            package_name: "test".to_string(),
            parse_dirty: false,
            deps: deps.iter().map(|s| s.to_string()).collect(),
            dependents: dependents.iter().map(|s| s.to_string()).collect(),
        })
    }

    /// Build a module graph:
    ///
    /// ```text
    ///   A → B → D
    ///   A → C
    ///   E → B
    /// ```
    ///
    /// Arrows mean "depends on" (A imports B and C).
    fn sample_graph() -> AHashMap<String, Module> {
        let mut modules = AHashMap::new();
        //            deps          dependents
        modules.insert("A".into(), make_module(&["B", "C"], &[]));
        modules.insert("B".into(), make_module(&["D"], &["A", "E"]));
        modules.insert("C".into(), make_module(&[], &["A"]));
        modules.insert("D".into(), make_module(&[], &["B"]));
        modules.insert("E".into(), make_module(&["B"], &[]));
        modules
    }

    // ── get_dependency_closure ───────────────────────────────────────

    #[test]
    fn dependency_single_start() {
        let modules = sample_graph();
        let closure = get_dependency_closure(&modules, AHashSet::from_iter(["A".into()]));
        assert_eq!(
            closure,
            ["A", "B", "C", "D"].iter().map(|s| s.to_string()).collect()
        );
    }

    #[test]
    fn dependency_leaf_module() {
        let modules = sample_graph();
        let closure = get_dependency_closure(&modules, AHashSet::from_iter(["D".into()]));
        assert_eq!(closure, ["D"].iter().map(|s| s.to_string()).collect());
    }

    #[test]
    fn dependency_multiple_starts_union() {
        let modules = sample_graph();
        // A's closure = {A, B, C, D}, E's closure = {E, B, D}
        // Union = {A, B, C, D, E}
        let closure = get_dependency_closure(&modules, AHashSet::from_iter(["A".into(), "E".into()]));
        assert_eq!(
            closure,
            ["A", "B", "C", "D", "E"].iter().map(|s| s.to_string()).collect()
        );
    }

    #[test]
    fn dependency_empty_starts() {
        let modules = sample_graph();
        let closure = get_dependency_closure(&modules, AHashSet::new());
        assert!(closure.is_empty());
    }

    #[test]
    fn dependency_unknown_start_module() {
        let modules = sample_graph();
        // Unknown module is included in the closure but has no deps to follow
        let closure = get_dependency_closure(&modules, AHashSet::from_iter(["Unknown".into()]));
        assert_eq!(closure, ["Unknown"].iter().map(|s| s.to_string()).collect());
    }

    // ── get_dependent_closure ────────────────────────────────────────

    #[test]
    fn dependent_single_start() {
        let modules = sample_graph();
        // D's dependents: B, and B's dependents: A, E
        let closure = get_dependent_closure(&modules, AHashSet::from_iter(["D".into()]));
        assert_eq!(closure, ["B", "A", "E"].iter().map(|s| s.to_string()).collect());
    }

    #[test]
    fn dependent_excludes_start_modules() {
        let modules = sample_graph();
        // B's dependents: A, E. Neither A nor E should include B itself.
        let closure = get_dependent_closure(&modules, AHashSet::from_iter(["B".into()]));
        assert_eq!(closure, ["A", "E"].iter().map(|s| s.to_string()).collect());
        assert!(!closure.contains("B"));
    }

    #[test]
    fn dependent_multiple_starts_excludes_all_starts() {
        let modules = sample_graph();
        // Start with both B and D.
        // D's dependents = {B}, but B is a start → excluded from closure.
        // B's dependents = {A, E}.
        // Result should be {A, E} — neither B nor D appears.
        let closure = get_dependent_closure(&modules, AHashSet::from_iter(["B".into(), "D".into()]));
        assert_eq!(closure, ["A", "E"].iter().map(|s| s.to_string()).collect());
        assert!(!closure.contains("B"));
        assert!(!closure.contains("D"));
    }

    #[test]
    fn dependent_root_module_has_no_dependents() {
        let modules = sample_graph();
        let closure = get_dependent_closure(&modules, AHashSet::from_iter(["A".into()]));
        assert!(closure.is_empty());
    }

    #[test]
    fn dependent_empty_starts() {
        let modules = sample_graph();
        let closure = get_dependent_closure(&modules, AHashSet::new());
        assert!(closure.is_empty());
    }

    // ── overlap between dependency and dependent closures ────────────

    #[test]
    fn module_in_both_dependency_and_dependent_closures() {
        let modules = sample_graph();
        // Save A and D simultaneously.
        // Dependency closure of {A, D} = {A, B, C, D}
        // Dependent closure of {A, D} = {B (from D), A (from B) — but A is start → skip}
        //   then B's dependents: A (start, skip), E → {B, E}
        let starts = AHashSet::from_iter(["A".into(), "D".into()]);
        let dep_closure = get_dependency_closure(&modules, starts.clone());
        let dependent_closure = get_dependent_closure(&modules, starts);

        // B appears in BOTH closures — this is the key scenario for batched saves
        assert!(dep_closure.contains("B"));
        assert!(dependent_closure.contains("B"));

        // E appears only in the dependent closure
        assert!(!dep_closure.contains("E"));
        assert!(dependent_closure.contains("E"));
    }
}
