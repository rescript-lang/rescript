# Dead Code Analysis Architecture

This document describes the architecture of the reanalyze dead code analysis pipeline.

## Overview

The DCE (Dead Code Elimination) analysis is structured as a **pure pipeline** with four phases:

1. **MAP** - Process each `.cmt` file independently → per-file data
2. **MERGE** - Combine all per-file data → immutable project-wide view
3. **SOLVE** - Compute dead/live status → immutable result with issues
4. **REPORT** - Output issues (side effects only here)

This design enables:
- **Order independence** - Processing files in any order gives identical results
- **Incremental updates** - Replace one file's data without reprocessing others
- **Testability** - Each phase is independently testable with pure functions
- **Parallelization potential** - Phases 1-3 work on immutable data

---

## Pipeline Diagram

> **Source**: [`diagrams/batch-pipeline.mmd`](diagrams/batch-pipeline.mmd)

![Batch Pipeline](diagrams/batch-pipeline.svg)

---

## Key Data Types

| Type | Purpose | Mutability |
|------|---------|------------|
| `DceFileProcessing.file_data` | Per-file collected data | Builders (mutable during AST walk) |
| `FileAnnotations.t` | Source annotations (`@dead`, `@live`) | Immutable after merge |
| `Declarations.t` | All exported declarations (pos → Decl.t) | Immutable after merge |
| `References.t` | Value/type references (pos → PosSet.t) | Immutable after merge |
| `FileDeps.t` | Cross-file dependencies (file → FileSet.t) | Immutable after merge |
| `OptionalArgsState.t` | Computed optional arg state per-decl | Immutable |
| `AnalysisResult.t` | Solver output with Issue.t list | Immutable |
| `DceConfig.t` | Analysis configuration | Immutable (passed explicitly) |

---

## Phase Details

### Phase 1: MAP (Per-File Processing)

**Entry point**: `DceFileProcessing.process_cmt_file`

**Input**: `.cmt` file path + `DceConfig.t`

**Output**: `file_data` containing builders for:
- `annotations` - `@dead`, `@live` annotations from source
- `decls` - Exported value/type/exception declarations
- `refs` - References to other declarations
- `file_deps` - Which files this file depends on
- `cross_file` - Items needing cross-file resolution (optional args, exceptions)

**Key property**: Local mutable state is OK here (performance). Each file is processed independently.

### Phase 2: MERGE (Combine Builders)

**Entry point**: `Reanalyze.runAnalysis` (merge section)

**Input**: `file_data list`

**Output**: Immutable project-wide data structures

**Operations**:
```ocaml
let annotations = FileAnnotations.merge_all (file_data_list |> List.map (fun fd -> fd.annotations))
let decls = Declarations.merge_all (file_data_list |> List.map (fun fd -> fd.decls))
let refs = References.merge_all (file_data_list |> List.map (fun fd -> fd.refs))
let file_deps = FileDeps.merge_all (file_data_list |> List.map (fun fd -> fd.file_deps))
```

**Key property**: Merge operations are commutative - order of `file_data_list` doesn't matter.

### Phase 3: SOLVE (Deadness Computation)

**Entry point**: `DeadCommon.solveDead` + optional args second pass in `Reanalyze.runAnalysis`

**Input**: All merged data + config

**Output**: `AnalysisResult.t` containing `Issue.t list`

**Algorithm** (two-pass for liveness-aware optional args):

**Pass 1: Core deadness resolution**
1. Build file dependency order (roots to leaves)
2. Sort declarations by dependency order
3. For each declaration, resolve references recursively
4. Determine dead/live status based on reference count
5. Collect issues for dead declarations

**Pass 2: Liveness-aware optional args analysis**
1. Use `Decl.isLive` to build an `is_live` predicate from Pass 1 results
2. Compute optional args state via `CrossFileItems.compute_optional_args_state`, filtering out calls from dead code
3. Collect optional args issues only for live declarations
4. Merge optional args issues into the final result

This two-pass approach ensures that optional argument warnings (e.g., "argument X is never used") only consider calls from live code, preventing false positives when a function is only called from dead code.

**Key property**: Pure functions - immutable in, immutable out. No side effects.

### Phase 4: REPORT (Output)

**Entry point**: `Reanalyze.runAnalysis` (report section)

**Input**: `AnalysisResult.t`

**Output**: Logging / JSON to stdout

**Operations**:
```ocaml
AnalysisResult.get_issues analysis_result
|> List.iter (fun issue -> Log_.warning ~loc:issue.loc issue.description)
```

**Key property**: All side effects live here at the edge. The solver never logs directly.

---

## Incremental Updates (Future)

The architecture enables incremental updates when a file changes:

1. Re-run Phase 1 for changed file only → new `file_data`
2. Replace in `file_data` map (keyed by filename)
3. Re-run Phase 2 (merge) - fast, pure function
4. Re-run Phase 3 (solve) - fast, pure function

The key insight: **immutable data structures enable safe incremental updates** - you can swap one file's data without affecting others.

---

## Reactive Pipelines

The reactive layer (`analysis/reactive/`) provides delta-based incremental updates. Instead of re-running entire phases, changes propagate automatically through derived collections.

### Core Reactive Primitives

| Primitive | Description |
|-----------|-------------|
| `Reactive.t ('k, 'v)` | Universal reactive collection interface |
| `subscribe` | Register for delta notifications |
| `iter` | Iterate current entries |
| `get` | Lookup by key |
| `delta` | Change notification: `Set (key, value)` or `Remove key` |
| `flatMap` | Transform collection, optionally merge same-key values |
| `join` | Hash join two collections with automatic updates |
| `lookup` | Single-key subscription |
| `ReactiveFileCollection` | File-backed collection with change detection |

### Reactive Analysis Pipeline

> **Source**: [`diagrams/reactive-pipeline.mmd`](diagrams/reactive-pipeline.mmd)

![Reactive Pipeline](diagrams/reactive-pipeline.svg)

**Legend:**

| Symbol | Collection | Type |
|--------|-----------|------|
| **RFC** | `ReactiveFileCollection` | File change detection |
| **FD** | `file_data` | `path → file_data option` |
| **D** | `decls` | `pos → Decl.t` |
| **A** | `annotations` | `pos → annotation` |
| **VR** | `value_refs` | `pos → PosSet` (per-file) |
| **TR** | `type_refs` | `pos → PosSet` (per-file) |
| **CFI** | `cross_file_items` | `path → CrossFileItems.t` |
| **DBP** | `decl_by_path` | `path → decl_info list` |
| **SPR** | `same_path_refs` | Same-path duplicates |
| **I2I** | `impl_to_intf_refs` | Impl → Interface links |
| **I2I₂** | `impl_to_intf_refs_path2` | Impl → Interface (path2) |
| **I→I** | `intf_to_impl_refs` | Interface → Impl links |
| **ER** | `exception_refs` | Exception references |
| **ED** | `exception_decls` | Exception declarations |
| **RR** | `resolved_refs` | Resolved exception refs |
| **REFS** | Output | Combined `References.t` |

### Delta Propagation

> **Source**: [`diagrams/delta-propagation.mmd`](diagrams/delta-propagation.mmd)

![Delta Propagation](diagrams/delta-propagation.svg)

### Key Benefits

| Aspect | Batch Pipeline | Reactive Pipeline |
|--------|----------------|-------------------|
| File change | Re-process all files | Re-process changed file only |
| Merge | Re-merge all data | Update affected entries only |
| Type deps | Rebuild entire index | Update affected paths only |
| Exception refs | Re-resolve all | Re-resolve affected only |
| Memory | O(N) per phase | O(N) total, shared |

### Reactive Modules

| Module | Responsibility |
|--------|---------------|
| `Reactive` | Core primitives: `flatMap`, `join`, `lookup`, delta types |
| `ReactiveFileCollection` | File-backed collection with change detection |
| `ReactiveAnalysis` | CMT processing with file caching |
| `ReactiveMerge` | Derives decls, annotations, refs from file_data |
| `ReactiveTypeDeps` | Type-label dependency resolution via join |
| `ReactiveExceptionRefs` | Exception ref resolution via join |

---

## Testing

**Order-independence test**: Run with `-test-shuffle` flag to randomize file processing order. The test (`make test-reanalyze-order-independence`) verifies that shuffled runs produce identical output.

**Unit testing**: Each phase can be tested independently:
- Phase 1: Process a single `.cmt` file, verify `file_data`
- Phase 2: Merge known builders, verify merged result
- Phase 3: Call solver with known inputs, verify issues

---

## Key Modules

| Module | Responsibility |
|--------|---------------|
| `Reanalyze` | Entry point, orchestrates pipeline |
| `DceFileProcessing` | Phase 1: Per-file AST processing |
| `DceConfig` | Configuration (CLI flags + run config) |
| `DeadCommon` | Phase 3: Solver (`solveDead`) |
| `Declarations` | Declaration storage (builder/immutable) |
| `References` | Reference tracking (builder/immutable) |
| `FileAnnotations` | Source annotation tracking |
| `FileDeps` | Cross-file dependency graph |
| `CrossFileItems` | Cross-file optional args and exceptions |
| `AnalysisResult` | Immutable solver output |
| `Issue` | Issue type definitions |
| `Log_` | Phase 4: Logging output |

