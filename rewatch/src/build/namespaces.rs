use crate::build::packages;
use crate::helpers::StrippedVerbatimPath;
use ahash::AHashSet;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

// Namespaces work like the following: The build system will generate a file
// called `MyModule.mlmap` which contains all modules that are in the namespace
//
// Not sure what the first line of this file is, but the next lines are names of
// the modules in the namespace you can call bsc with this file, and it will
// produce compiler assets for this file basically a module with all aliases.
// Given that this is just aliases, it doesn not need to create a mjs file.
//
// Internal modules are not accessible with the following trick, they are
// compiled to a module name such as `MyModule-MyNameSpace`.  A dash in a module
// name is not possible to make in a source file, but it's possible when
// constructing the AST, so these modules are hidden from compilation.
// in the top namespace however, we alias with the proper names

pub fn gen_mlmap(
    package: &packages::Package,
    namespace: &str,
    depending_modules: &AHashSet<String>,
) -> PathBuf {
    let build_path_abs = package.get_build_path();
    // we don't really need to create a digest, because we track if we need to
    // recompile in a different way but we need to put it in the file for it to
    // be readable.

    let path = build_path_abs.join(format!("{}.mlmap", namespace));
    let mut file = File::create(&path).expect("Unable to create mlmap");

    file.write_all(b"randjbuildsystem\n")
        .expect("Unable to write mlmap");

    let mut modules = Vec::from_iter(depending_modules.to_owned());
    modules.sort();
    for module in modules {
        // check if the module names is referencible in code (no exotic module names)
        // (only contains A-Z a-z 0-9 and _ and only starts with a capital letter)
        // if not, it does not make sense to export as part of the name space
        // this helps compile times of exotic modules such as MyModule.test
        file.write_all(module.as_bytes()).unwrap();
        file.write_all(b"\n").unwrap();
    }

    path
}

pub fn compile_mlmap(package: &packages::Package, namespace: &str, bsc_path: &Path) {
    let build_path_abs = package.get_build_path();
    let mlmap_name = format!("{}.mlmap", namespace);
    let args = vec!["-w", "-49", "-color", "always", "-no-alias-deps", &mlmap_name];

    let _ = Command::new(bsc_path)
        .current_dir(
            build_path_abs
                .canonicalize()
                .map(StrippedVerbatimPath::to_stripped_verbatim_path)
                .ok()
                .unwrap(),
        )
        .args(args)
        .output()
        .expect("err");
}
