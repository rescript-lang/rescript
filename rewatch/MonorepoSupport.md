# Monorepo Support in `rewatch` (ReScript Build System)

This document describes **how `rewatch` infers monorepo structure**, **what invariants are required**, and **how build scope changes** depending on where you invoke the build.

All statements are derived from:
- `rewatch/src/project_context.rs` – monorepo context detection
- `rewatch/src/build/packages.rs` – package discovery and traversal
- `rewatch/src/helpers.rs` – path resolution utilities
- `rewatch/src/build/{parse.rs,deps.rs,compile.rs}` – build phases

---

## Terminology

| Term | Definition |
|------|------------|
| **Package** | A folder containing `rescript.json` (or legacy `bsconfig.json`). Usually also has `package.json`. |
| **Root config** | The `rescript.json` used for global build settings (JSX, output format, etc.). |
| **Local package** | A package whose canonical path is inside the workspace AND not under any `node_modules` path component. |
| **Current package** | The package where you ran `rescript build` or `rescript watch`. |

---

## Build Modes

`rewatch` does **not** read package manager workspace definitions (e.g., `pnpm-workspace.yaml`). Instead, it infers monorepo structure from:

- `rescript.json` `dependencies` / `dev-dependencies` lists
- `node_modules/<packageName>` resolution (typically workspace symlinks)
- Parent `rescript.json` that lists the current package as a dependency

There are **three effective modes**:

### 1. Single Project
- No parent config references this package
- No dependencies resolve to local packages
- The current package is both "root" and only package in scope

### 2. Monorepo Root
- At least one dependency or dev-dependency resolves via `./node_modules/<dep>` to a **local package**
- The root `rescript.json` should list workspace packages by name in `dependencies`/`dev-dependencies`

### 3. Monorepo Leaf Package
- A parent directory contains a `rescript.json` (or `bsconfig.json`)
- That parent config lists this package's name in its `dependencies` or `dev-dependencies`

---

## Local Package Detection

A resolved dependency path is considered "local" if both conditions are met:

1. Its **canonical path** is within the workspace root path
2. The canonical path contains **no** `node_modules` segment

This is why workspace symlinks work: `node_modules/<name>` → real path in repo.

```rust
// From helpers.rs
pub fn is_local_package(workspace_path: &Path, canonical_package_path: &Path) -> bool {
    canonical_package_path.starts_with(workspace_path)
        && !canonical_package_path
            .components()
            .any(|c| c.as_os_str() == "node_modules")
}
```

---

## Dependency Resolution

All dependencies are resolved via `try_package_path`, which probes in order:

| Priority | Path Probed | When Used |
|----------|-------------|-----------|
| 1 | `<packageDir>/node_modules/<dep>` | Always (handles hoisted deps in nested packages) |
| 2 | `<currentConfigDir>/node_modules/<dep>` | Always (current build context) |
| 3 | `<rootDir>/node_modules/<dep>` | Always (monorepo root) |
| 4 | Upward traversal through ancestors | **Only in single-project mode** |

If no path exists, the build fails with: *"are node_modules up-to-date?"*

---

## Build Scope ("Package Graph")

Starting from the current package, `rewatch` builds a **package graph**:

1. **Always includes** the current package (marked as `is_root=true`)
2. **Recursively includes** transitive `dependencies`
3. **Includes `dev-dependencies`** only for **local packages**

> External package dev-dependencies are **never** included.

### Practical Effect

| Invocation Location | Packages Built |
|---------------------|----------------|
| Monorepo root | All packages reachable from root's `dependencies` + `dev-dependencies` |
| Leaf package | Only that package + its transitive deps (not unrelated siblings) |

---

## Root Config vs Per-Package Config

Even when building from a leaf package, some settings are inherited from the **root config**:

### From Root Config
| Setting | Notes |
|---------|-------|
| `jsx`, `jsx.mode`, `jsx.module`, `jsx.preserve` | JSX configuration is global |
| `package-specs`, `suffix` | Output format must be consistent |
| Experimental features | Runtime feature flags |

### From Per-Package Config
| Setting | Notes |
|---------|-------|
| `namespace`, `namespace-entry` | Each package can have its own namespace |
| `compiler-flags` (`bsc-flags`) | Package-specific compiler options |
| `ppx-flags` | PPX transformations are per-package |
| `warnings` | Warning configuration is per-package |
| `sources` | Obviously per-package |

---

## Cross-Package Compilation

### Directory Structure

Each package compiles into:
```
<package>/
├── lib/
│   ├── bs/         # Build working directory (AST files, intermediate outputs)
│   └── ocaml/      # Published artifacts (.cmi, .cmj, .cmt, .cmti)
```

### Compilation Process

For package `A` depending on package `B`:

1. **bsc runs with CWD** = `<A>/lib/bs`
2. **Include path** = `-I <B>/lib/ocaml` for each declared dependency
3. **Own artifacts** = `-I ../ocaml` (relative path to own lib/ocaml)

### Dependency Filtering

Module dependencies discovered from `.ast` files are **filtered**:
- A module in another package is only valid if that package is declared in `dependencies` or `dev-dependencies`
- "It compiles locally because the module exists" is **not** sufficient

---

## Algorithm (Pseudo-code)

```text
function BUILD(entryFolder):
  entryFolderAbs = canonicalize(entryFolder)
  currentConfig = read_config(entryFolderAbs / "rescript.json")
  
  # Step 1: Determine monorepo context
  parentConfigDir = nearest_ancestor_with_config(parent(entryFolderAbs))
  
  if parentConfigDir exists:
    parentConfig = read_config(parentConfigDir / "rescript.json")
    if currentConfig.name ∈ (parentConfig.dependencies ∪ parentConfig.dev_dependencies):
      context = MonorepoPackage(parentConfig)
    else:
      context = infer_root_or_single(entryFolderAbs, currentConfig)
  else:
    context = infer_root_or_single(entryFolderAbs, currentConfig)
  
  rootConfig = context.get_root_config()  # parent for MonorepoPackage, else current
  
  # Step 2: Build package closure
  packages = {currentConfig.name: Package(is_root=true, is_local_dep=true)}
  walk(currentConfig, is_local_dep=true)
  
  function walk(config, is_local):
    deps = config.dependencies
    if is_local:
      deps = deps ∪ config.dev_dependencies
    
    for depName in deps:
      if depName already in packages: continue
      
      depFolder = canonicalize(resolve_node_modules(config, depName))
      depConfig = read_config(depFolder)
      
      depIsLocal = match context:
        | SingleProject    → (currentConfig.name == depName)
        | MonorepoRoot     → depName ∈ context.local_deps
        | MonorepoPackage  → is_local_package(parentConfig.folder, depFolder)
      
      packages[depName] = Package(is_root=false, is_local_dep=depIsLocal)
      walk(depConfig, depIsLocal)
  
  # Step 3: Scan sources
  for package in packages:
    scan sources (type:dev only included if package.is_local_dep)
    compute module names (apply namespace suffix rules)
    ensure lib/bs + lib/ocaml exist
    enforce global unique module names
  
  # Step 4: Build loop
  1. Parse dirty sources: bsc -bs-ast, cwd=<pkg>/lib/bs
  2. Compute module deps from AST; filter by declared package deps
  3. Compile in dependency-order waves:
     - bsc cwd=<pkg>/lib/bs
     - include: -I ../ocaml -I <dep>/lib/ocaml for each declared dep
     - runtime: -runtime-path <@rescript/runtime resolved>
     - package specs: from rootConfig
  4. Copy artifacts to <pkg>/lib/ocaml
```

---

## Practical Guidance

### Structuring a Monorepo

Each workspace package should have:
- `rescript.json` with `"name"` matching `package.json` `"name"` (mismatch is warned)
- Correct `dependencies` for every other ReScript package it imports from

A monorepo root that wants to "build everything" should:
- Have its own `rescript.json` (can have no sources)
- List each workspace package in `dependencies` / `dev-dependencies`
- Ensure package manager creates `node_modules/<pkgName>` symlinks to workspace packages

### Where to Build From

| Goal | Run From |
|------|----------|
| Build one leaf package + its deps | That leaf package's folder |
| Build entire monorepo | Root folder with `rescript.json` listing all packages |

### Common Issues

| Symptom | Likely Cause |
|---------|--------------|
| "Package X not found" | Missing from `dependencies` or `node_modules` not linked |
| Module from sibling package not visible | Sibling not in current package's `dependencies` |
| Dev sources not compiled | Package is not detected as "local" |
| Wrong JSX settings | JSX comes from root config, not per-package |
