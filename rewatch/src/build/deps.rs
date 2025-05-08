use super::build_types::*;
use super::packages;
use crate::helpers;
use ahash::AHashSet;
use rayon::prelude::*;

fn get_dep_modules(
    ast_file: &str,
    namespace: Option<String>,
    package_modules: &AHashSet<String>,
    valid_modules: &AHashSet<String>,
    package: &packages::Package,
) -> AHashSet<String> {
    let mut deps = AHashSet::new();
    let ast_file = package.get_build_path() + "/" + ast_file;
    if let Ok(lines) = helpers::read_lines(ast_file.to_string()) {
        // we skip the first line with is some null characters
        // the following lines in the AST are the dependency modules
        // we stop when we hit a line that starts with a "/", this is the path of the file.
        // this is the point where the dependencies end and the actual AST starts
        for line in lines.skip(1).flatten() {
            let line = line.trim().to_string();
            if line.starts_with('/') {
                break;
            } else if !line.is_empty() {
                deps.insert(line);
            }
        }
    } else {
        panic!("Could not read file {}", ast_file);
    }

    return deps
        .iter()
        .map(|dep| {
            let dep_first = dep.split('.').next().unwrap();
            let dep_second = dep.split('.').nth(1);
            match &namespace {
                Some(namespace) => {
                    // if the module is in the own namespace, take the submodule -- so:
                    // if the module is TeamwalnutApp.MyModule inside of the namespace TeamwalnutApp
                    // we need the dependency to be MyModule in the same namespace
                    let dep = match dep_second {
                        Some(dep_second) if dep_first == namespace => dep_second,
                        _ => dep_first,
                    };
                    let namespaced_name = dep.to_owned() + "-" + namespace;
                    if package_modules.contains(&namespaced_name) || valid_modules.contains(&namespaced_name)
                    {
                        namespaced_name
                    } else {
                        dep.to_string()
                    }
                }
                None => dep_first.to_string(),
            }
        })
        .filter(|dep| {
            valid_modules.contains(dep)
                && match namespace.to_owned() {
                    Some(namespace) => !dep.eq(&namespace),
                    None => true,
                }
        })
        .collect::<AHashSet<String>>();
}

pub fn get_deps(build_state: &mut BuildState, deleted_modules: &AHashSet<String>) {
    let all_mod = &build_state.module_names.union(deleted_modules).cloned().collect();
    build_state
        .modules
        .par_iter()
        .map(|(module_name, module)| match &module.source_type {
            SourceType::MlMap(_) => (module_name.to_string(), module.deps.to_owned()),
            SourceType::SourceFile(source_file) => {
                let package = build_state
                    .get_package(&module.package_name)
                    .expect("Package not found");
                let ast_path = helpers::get_ast_path(&source_file.implementation.path);
                if module.deps_dirty || !build_state.deps_initialized {
                    let mut deps = get_dep_modules(
                        &ast_path.to_string_lossy(),
                        package.namespace.to_suffix(),
                        package.modules.as_ref().unwrap(),
                        all_mod,
                        &package,
                    );

                    if let Some(interface) = &source_file.interface {
                        let iast_path = helpers::get_ast_path(&interface.path);

                        deps.extend(get_dep_modules(
                            &iast_path.to_string_lossy(),
                            package.namespace.to_suffix(),
                            package.modules.as_ref().unwrap(),
                            all_mod,
                            &package,
                        ))
                    }
                    match &package.namespace {
                        packages::Namespace::NamespaceWithEntry { namespace: _, entry }
                            if entry == module_name =>
                        {
                            deps.insert(package.namespace.to_suffix().unwrap());
                        }
                        _ => (),
                    }
                    deps.remove(module_name);
                    (module_name.to_string(), deps)
                } else {
                    (module_name.to_string(), module.deps.to_owned())
                }
            }
        })
        .collect::<Vec<(String, AHashSet<String>)>>()
        .into_iter()
        .for_each(|(module_name, deps)| {
            if let Some(module) = build_state.modules.get_mut(&module_name) {
                module.deps = deps.clone();
                module.deps_dirty = false;
            }
            deps.iter().for_each(|dep_name| {
                if let Some(module) = build_state.modules.get_mut(dep_name) {
                    module.dependents.insert(module_name.to_string());
                }
            });
        });
    build_state.deps_initialized = true;
}
