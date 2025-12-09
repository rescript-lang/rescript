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

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         DCE ANALYSIS PIPELINE                               │
└─────────────────────────────────────────────────────────────────────────────┘

                              ┌─────────────┐
                              │ DceConfig.t │ (explicit configuration)
                              └──────┬──────┘
                                     │
    ╔════════════════════════════════╪════════════════════════════════════════╗
    ║  PHASE 1: MAP (per-file)       │                                        ║
    ╠════════════════════════════════╪════════════════════════════════════════╣
    ║                                ▼                                        ║
    ║  ┌──────────┐   process_cmt_file    ┌───────────────────────────────┐   ║
    ║  │ file1.cmt├──────────────────────►│ file_data {                   │   ║
    ║  └──────────┘                       │   annotations: builder        │   ║
    ║  ┌──────────┐   process_cmt_file    │   decls: builder              │   ║
    ║  │ file2.cmt├──────────────────────►│   refs: builder               │   ║
    ║  └──────────┘                       │   file_deps: builder          │   ║
    ║  ┌──────────┐   process_cmt_file    │   cross_file: builder         │   ║
    ║  │ file3.cmt├──────────────────────►│ }                             │   ║
    ║  └──────────┘                       └───────────────────────────────┘   ║
    ║                                                  │                      ║
    ║  Local mutable state OK                          │ file_data list       ║
    ╚══════════════════════════════════════════════════╪══════════════════════╝
                                                       │
    ╔══════════════════════════════════════════════════╪══════════════════════╗
    ║  PHASE 2: MERGE (combine builders)               │                      ║
    ╠══════════════════════════════════════════════════╪══════════════════════╣
    ║                                                  ▼                      ║
    ║  ┌─────────────────────────────────────────────────────────────────┐   ║
    ║  │ FileAnnotations.merge_all  → annotations: FileAnnotations.t     │   ║
    ║  │ Declarations.merge_all     → decls: Declarations.t              │   ║
    ║  │ References.merge_all       → refs: References.t                 │   ║
    ║  │ FileDeps.merge_all         → file_deps: FileDeps.t              │   ║
    ║  │ CrossFileItems.merge_all   → cross_file: CrossFileItems.t       │   ║
    ║  │                                                                  │   ║
    ║  │ CrossFileItems.compute_optional_args_state                       │   ║
    ║  │                            → optional_args_state: State.t        │   ║
    ║  └─────────────────────────────────────────────────────────────────┘   ║
    ║                                                  │                      ║
    ║  Pure functions, immutable output                │ merged data          ║
    ╚══════════════════════════════════════════════════╪══════════════════════╝
                                                       │
    ╔══════════════════════════════════════════════════╪══════════════════════╗
    ║  PHASE 3: SOLVE (pure deadness computation)      │                      ║
    ╠══════════════════════════════════════════════════╪══════════════════════╣
    ║                                                  ▼                      ║
    ║  ┌─────────────────────────────────────────────────────────────────┐   ║
    ║  │ DeadCommon.solveDead                                            │   ║
    ║  │   ~annotations ~decls ~refs ~file_deps                          │   ║
    ║  │   ~optional_args_state ~config ~checkOptionalArg                │   ║
    ║  │                                                                  │   ║
    ║  │   → AnalysisResult.t { issues: Issue.t list }                   │   ║
    ║  └─────────────────────────────────────────────────────────────────┘   ║
    ║                                                  │                      ║
    ║  Pure function: immutable in → immutable out     │ issues               ║
    ╚══════════════════════════════════════════════════╪══════════════════════╝
                                                       │
    ╔══════════════════════════════════════════════════╪══════════════════════╗
    ║  PHASE 4: REPORT (side effects at the edge)      │                      ║
    ╠══════════════════════════════════════════════════╪══════════════════════╣
    ║                                                  ▼                      ║
    ║  ┌─────────────────────────────────────────────────────────────────┐   ║
    ║  │ AnalysisResult.get_issues                                       │   ║
    ║  │ |> List.iter (fun issue -> Log_.warning ~loc issue.description) │   ║
    ║  │                                                                  │   ║
    ║  │ (Optional: EmitJson for JSON output)                            │   ║
    ║  └─────────────────────────────────────────────────────────────────┘   ║
    ║                                                                        ║
    ║  Side effects only here: logging, JSON output                          ║
    ╚════════════════════════════════════════════════════════════════════════╝
```

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

**Entry point**: `DeadCommon.solveDead`

**Input**: All merged data + config

**Output**: `AnalysisResult.t` containing `Issue.t list`

**Algorithm**:
1. Build file dependency order (roots to leaves)
2. Sort declarations by dependency order
3. For each declaration, resolve references recursively
4. Determine dead/live status based on reference count
5. Collect issues for dead declarations

**Key property**: Pure function - immutable in, immutable out. No side effects.

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

