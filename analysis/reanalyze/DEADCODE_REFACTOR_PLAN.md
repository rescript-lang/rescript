## Dead Code Analysis – Pure Pipeline Refactor Plan

**Goal**: Turn the reanalyze dead code analysis into a transparent, effect-free pipeline where:
- Analysis is a pure function from inputs → results
- Global mutable state is eliminated
- Side effects (logging, file I/O) live at the edges
- Processing files in different orders gives the same results
- **Incremental analysis is possible** - can reprocess one file without redoing everything

**Why?** The current architecture makes:
- Incremental/reactive analysis impossible (can't reprocess one file)
- Testing hard (global state persists between tests)
- Parallelization impossible (shared mutable state)
- Reasoning difficult (order-dependent hidden mutations)

---

## Key Design Principles

### 1. Separate per-file input from project-wide analysis

**Per-file source data** (can be incrementally updated):
- Source annotations (`@dead`, `@live`, `@genType` from AST)
- Declarations defined in that file
- References made from that file
- Keyed by filename so we can replace one file's data

**Project-wide analysis** (computed from merged per-file data):
- Deadness solver operates on merged view of all files
- Results are **immutable** - returned as data, not mutated

### 2. Analysis results are immutable

The solver should:
- Take source data as **read-only input**
- Return results as **new immutable data**
- Never mutate input state during analysis

```ocaml
(* WRONG - current design mutates state during analysis *)
let resolveRecursiveRefs ~state ... =
  ...
  AnnotationState.annotate_dead state decl.pos  (* mutation! *)

(* RIGHT - return results as data *)
let solve_deadness ~source_annotations ~decls ~refs =
  ... compute ...
  { dead_positions; issues; annotations_to_write }  (* return, don't mutate *)
```

### 3. Enable incremental updates

When file F changes:
1. Replace `per_file_data[F]` with new data from re-processing F
2. Re-merge into project-wide view
3. Re-run solver (returns new results)

This requires per-file data to be **keyed by filename**.

---

## Current Problems (What We're Fixing)

### P1: Global "current file" context
**Problem**: `Common.currentSrc`, `currentModule`, `currentModuleName` are global refs set before processing each file. Every function implicitly depends on "which file are we processing right now?". This makes it impossible to process multiple files concurrently or incrementally.

**Used by**: `DeadCommon.addDeclaration_`, `DeadType.addTypeDependenciesAcrossFiles`, `DeadValue` path construction.

**Status**: ✅ FIXED in Task 1 - explicit `file_context` now threaded through all analysis functions.

### P2: Global analysis tables
**Problem**: All analysis results accumulate in global hashtables:
- `DeadCommon.decls` - all declarations
- `ValueReferences.table` - all value references  
- `TypeReferences.table` - all type references
- `FileReferences.table` - cross-file dependencies

**Impact**: Can't analyze a subset of files without reanalyzing everything. Can't clear state between test runs without module reloading.

### P3: Delayed/deferred processing queues
**Problem**: Several analyses use global queues that get "flushed" later:
- `DeadOptionalArgs.delayedItems` - deferred optional arg analysis
- `DeadException.delayedItems` - deferred exception checks
- `DeadType.TypeDependencies.delayedItems` - deferred type deps
- `ProcessDeadAnnotations.positionsAnnotated` - annotation tracking

**Additional problem**: `positionsAnnotated` mixes **input** (source annotations from AST) with **output** (positions the solver determines are dead). The solver mutates this during analysis, violating purity.

**Impact**: Order-dependent. Processing files in different orders can give different results because queue processing happens at arbitrary times. Mixing input/output prevents incremental analysis.

### P4: Global configuration reads
**Problem**: Analysis code directly reads `!Common.Cli.debug`, `RunConfig.runConfig.transitive`, etc. scattered throughout. Can't run analysis with different configs without mutating globals.

**Status**: ✅ FIXED in Task 2 - explicit `config` now threaded through all analysis functions.

### P5: Side effects mixed with analysis
**Problem**: Analysis functions directly call:
- `Log_.warning` - logging
- `EmitJson` - JSON output  
- `WriteDeadAnnotations` - file I/O
- Direct mutation of result data structures

**Impact**: Can't get analysis results as data. Can't test without capturing I/O. Can't reuse analysis logic for different output formats.

### P6: Binding/reporting state
**Problem**: `DeadCommon.Current.bindings`, `lastBinding`, `maxValuePosEnd` are per-file state stored globally.

**Status**: ✅ ALREADY FIXED in previous work - now explicit state threaded through traversals.

---

## End State

```ocaml
(* Configuration: immutable *)
type config = {
  run : RunConfig.t;
  debug : bool;
  write_annotations : bool;
  live_names : string list;
  live_paths : string list;
  exclude_paths : string list;
}

(* Per-file source data - extracted from one file's AST *)
type file_data = {
  source_path : string;
  module_name : Name.t;
  is_interface : bool;
  source_annotations : annotated_as PosHash.t;  (* @dead/@live/@genType in source *)
  decls : decl list;                            (* declarations defined here *)
  value_refs : (pos * pos) list;                (* references made from here *)
  type_refs : (pos * pos) list;
  file_refs : string list;                      (* files this file depends on *)
}

(* Per-file data keyed by filename - enables incremental updates *)
type per_file_state = file_data StringMap.t

(* Project-wide merged view - computed from per_file_state *)
type merged_state = {
  all_annotations : annotated_as PosHash.t;     (* merged from all files *)
  all_decls : decl PosHash.t;                   (* merged from all files *)
  all_value_refs : PosSet.t PosHash.t;          (* merged from all files *)
  all_type_refs : PosSet.t PosHash.t;
  all_file_refs : FileSet.t StringMap.t;
}

(* Analysis results - IMMUTABLE, returned by solver *)
type analysis_result = {
  dead_positions : PosSet.t;
  issues : issue list;
  annotations_to_write : (string * line_annotation list) list;
}

(* Pure: extract data from one file *)
val process_file : config -> Cmt_format.cmt_infos -> file_data

(* Pure: merge per-file data into project-wide view *)
val merge_file_data : per_file_state -> merged_state

(* Pure: solve deadness - takes READ-ONLY input, returns IMMUTABLE result *)
val solve_deadness : config -> merged_state -> analysis_result

(* Orchestration with side effects at edges *)
let run_analysis ~config ~cmt_files =
  (* Pure: process each file independently *)
  let per_file = 
    cmt_files 
    |> List.map (fun path -> (path, process_file config (load_cmt path)))
    |> StringMap.of_list
  in
  (* Pure: merge into project-wide view *)
  let merged = merge_file_data per_file in
  (* Pure: solve deadness - NO MUTATION *)
  let result = solve_deadness config merged in
  (* Impure: report results *)
  result.issues |> List.iter report_issue;
  if config.write_annotations then 
    result.annotations_to_write |> List.iter write_to_file

(* Incremental update when file F changes *)
let update_file ~config ~per_file ~changed_file =
  let new_file_data = process_file config (load_cmt changed_file) in
  let per_file = StringMap.add changed_file new_file_data per_file in
  let merged = merge_file_data per_file in
  solve_deadness config merged
```

---

## Refactor Tasks

Each task should:
- ✅ Fix a real problem listed above
- ✅ Leave the code in a measurably better state
- ✅ Be testable (behavior preserved, but architecture improved)
- ❌ NOT add scaffolding that isn't immediately used

### Task 1: Remove global "current file" context (P1)

**Value**: Makes it possible to process files concurrently or out of order.

**Changes**:
- [x] Create `DeadCommon.FileContext.t` type with `source_path`, `module_name`, `is_interface` fields
- [x] Thread through `DeadCode.processCmt`, `DeadValue`, `DeadType`, `DeadCommon.addDeclaration_`
- [x] Thread through `Exception.processCmt`, `Arnold.processCmt`
- [x] Remove all reads of `Common.currentSrc`, `currentModule`, `currentModuleName` from DCE code
- [x] Delete the globals `currentSrc`, `currentModule`, `currentModuleName` from `Common.ml`

**Status**: Complete ✅

**Test**: Run analysis on same files but vary the order - should get identical results.

**Estimated effort**: Medium (touches ~10 functions, mostly mechanical)

### Task 2: Extract configuration into explicit value (P4)

**Value**: Can run analysis with different configs without mutating globals. Can test with different configs.

**Changes**:
- [x] ~~Use the `DceConfig.t` already created, thread it through DCE analysis functions~~
- [x] ~~Replace all DCE code's `!Common.Cli.debug`, `runConfig.transitive`, etc. reads with `config.debug`, `config.run.transitive`~~
- [x] ~~Make all config parameters required (not optional) - no `config option` anywhere~~
- [x] Thread config through Exception and Arnold analyses (no `DceConfig.current()` in analysis code)
- [x] Single entry point: only the CLI/entry wrappers (`runAnalysisAndReport`, `DceCommand`) call `DceConfig.current()` once, then pass explicit config everywhere

**Status**: Complete ✅ (DCE + Exception + Arnold).

**Test**: Create two configs with different settings, run analysis with each - should respect the config, not read globals.

**Estimated effort**: Medium (done)

### Task 3: Make `ProcessDeadAnnotations` state explicit (P3)

**Value**: Removes hidden global state. Makes annotation tracking testable.

**Changes**:
- [x] Create `AnnotationState.t` module with explicit state type and accessor functions
- [x] Change `ProcessDeadAnnotations` functions to take explicit `~state:AnnotationState.t`
- [x] Thread `annotation_state` through `DeadCode.processCmt` and `Reanalyze.loadCmtFile`
- [x] Update `declIsDead`, `doReportDead`, `resolveRecursiveRefs`, `reportDead` to use explicit state
- [x] Update `DeadOptionalArgs.check` to take explicit state
- [x] Delete the global `positionsAnnotated`

**Status**: Partially complete ⚠️

**Known limitation**: Current implementation still mixes concerns:
- Source annotations (from `@dead`/`@live`/`@genType` in files) - INPUT
- Analysis results (positions solver determined are dead) - OUTPUT

The solver currently **mutates** `AnnotationState` via `annotate_dead` during `resolveRecursiveRefs`.
This violates the principle that analysis results should be immutable and returned.

**TODO** (in later task):
- [ ] Separate `SourceAnnotations.t` (per-file, read-only input) from analysis results
- [ ] Make `SourceAnnotations` keyed by filename for incremental updates
- [ ] Solver should return dead positions as part of `analysis_result`, not mutate state

**Test**: Process two files "simultaneously" (two separate state values) - should not interfere.

**Estimated effort**: Small (well-scoped module)

### Task 4: Localize analysis tables (P2) - Part 1: Declarations

**Value**: First step toward incremental analysis. Per-file declaration data enables replacing one file's contributions.

**Changes**:
- [ ] Create `FileDecls.t` type for per-file declarations (keyed by filename)
- [ ] `process_file` returns declarations for that file only
- [ ] Store as `file_decls : decl list StringMap.t` (per-file, keyed by filename)
- [ ] Create `merge_decls : file_decls -> decl PosHash.t` for project-wide view
- [ ] Delete global `DeadCommon.decls`

**Incremental benefit**: When file F changes, just replace `file_decls[F]` and re-merge.

**Test**: Analyze files with separate decl tables - should not interfere.

**Estimated effort**: Medium (core data structure, many call sites)

### Task 5: Localize analysis tables (P2) - Part 2: References

**Value**: Completes per-file reference tracking for incremental analysis.

**Changes**:
- [ ] Create `FileRefs.t` for per-file references (keyed by filename)
- [ ] `process_file` returns references made from that file
- [ ] Store as `file_value_refs : (pos * pos) list StringMap.t`
- [ ] Create `merge_refs` for project-wide view
- [ ] Delete global `ValueReferences.table` and `TypeReferences.table`

**Incremental benefit**: When file F changes, replace `file_refs[F]` and re-merge.

**Test**: Same as Task 4.

**Estimated effort**: Medium (similar to Task 4)

### Task 6: Localize delayed processing queues (P3)

**Value**: Removes order dependence. Makes analysis deterministic.

**Changes**:
- [ ] `DeadOptionalArgs`: Return delayed items from file processing, merge later
- [ ] `DeadException`: Return delayed items from file processing, merge later
- [ ] `DeadType.TypeDependencies`: Return delayed items from file processing, merge later
- [ ] `forceDelayedItems` operates on merged delayed items (pure function)
- [ ] Delete global refs

**Key insight**: Delayed items should be **returned** from file processing, not accumulated in globals.
This makes them per-file and enables incremental updates.

**Test**: Process files in different orders - delayed items should be processed consistently.

**Estimated effort**: Medium (3 modules, each similar to Task 3)

### Task 7: Localize file/module tracking (P2 + P3)

**Value**: Per-file dependency tracking enables incremental dependency graph updates.

**Changes**:
- [ ] `FileReferences`: Store per-file as `file_deps : string list StringMap.t`
- [ ] Create `merge_file_refs` for project-wide dependency graph
- [ ] `DeadModules`: Track per-file module usage, merge for project-wide view
- [ ] `iterFilesFromRootsToLeaves`: pure function on merged file refs, returns ordered list

**Incremental benefit**: When file F changes, update `file_deps[F]` and re-merge graph.

**Test**: Build file reference graph in isolation, verify topological ordering is correct.

**Estimated effort**: Medium (cross-file logic, but well-contained)

### Task 8: Separate analysis from reporting (P5) - Immutable Results

**Value**: Solver returns immutable results. No mutation during analysis. Pure function.

**Changes**:
- [ ] Create `AnalysisResult.t` type with `dead_positions`, `issues`, `annotations_to_write`
- [ ] `solve_deadness`: Return `AnalysisResult.t` instead of mutating state
- [ ] Remove `AnnotationState.annotate_dead` call from `resolveRecursiveRefs`
- [ ] Dead positions are part of returned result, not mutated into input state
- [ ] `Decl.report`: Return `issue` instead of logging
- [ ] Remove all `Log_.warning`, `Log_.item`, `EmitJson` calls from `Dead*.ml` modules
- [ ] `Reanalyze.runAnalysis`: Call pure solver, then separately report from result

**Key principle**: The solver takes **read-only** merged state and returns **new immutable** results.
No mutation of input state during analysis.

```ocaml
(* Before - WRONG *)
let solve ~state = 
  ... AnnotationState.annotate_dead state pos ...  (* mutates input! *)

(* After - RIGHT *)
let solve ~merged_state =
  let dead_positions = ... compute ... in
  { dead_positions; issues; annotations_to_write }  (* return new data *)
```

**Test**: Run analysis, capture result, verify input state unchanged.

**Estimated effort**: Medium (many logging call sites, but mechanical)

### Task 9: Separate annotation computation from file writing (P5)

**Value**: Can compute what to write without actually writing. Testable.

**Changes**:
- [ ] `WriteDeadAnnotations`: Split into pure `compute_annotations` and impure `write_to_files`
- [ ] Pure function takes deadness results, returns `(filepath * line_annotation list) list`
- [ ] Impure function takes that list and does file I/O
- [ ] Remove file I/O from analysis path

**Test**: Compute annotations, verify correct without touching filesystem.

**Estimated effort**: Small (single module)

### Task 10: Verify zero `DceConfig.current()` calls in analysis code

**Value**: Enforce purity - no hidden global reads.

**Changes**:
- [x] Verify `DceConfig.current()` only called in entry wrappers (CLI / `runAnalysisAndReport`)
- [x] Verify no calls to `DceConfig.current()` in `Dead*.ml`, `Exception.ml`, `Arnold.ml` analysis code
- [x] All analysis functions take explicit `~config` parameter

**Test**: `grep -r "DceConfig.current" analysis/reanalyze/src/{Dead,Exception,Arnold}.ml` returns zero results. ✅

**Estimated effort**: Trivial (done)

### Task 11: Integration and order-independence verification

**Value**: Verify the refactor achieved its goals.

**Changes**:
- [ ] Write property test: process files in random orders, verify identical results
- [ ] Write test: analyze with different configs, verify each is respected
- [ ] Write test: analyze subset of files without initializing globals
- [ ] Document the new architecture and API

**Test**: The tests are the task.

**Estimated effort**: Small (mostly writing tests)

---

## Execution Strategy

**Completed**: Task 1 ✅, Task 2 ✅, Task 10 ✅
**Partially complete**: Task 3 ⚠️ (state explicit but still mixes input/output)

**Remaining order**: 4 → 5 → 6 → 7 → 8 → 9 → 11 (test)

**Why this order?**
- Tasks 1-2 remove implicit dependencies (file context, config) - ✅ DONE
- Task 3 makes annotation tracking explicit - ⚠️ PARTIAL (needs input/output separation in Task 8)
- Tasks 4-7 make state **per-file** for incremental updates
- Task 8 makes solver **pure** with immutable results (also fixes Task 3's input/output mixing)
- Task 9 separates annotation computation from file writing
- Task 10 verifies no global config reads remain - ✅ DONE
- Task 11 validates everything including incremental updates

**Key architectural milestones**:
1. **After Task 7**: All state is per-file, keyed by filename
2. **After Task 8**: Solver is pure, returns immutable results
3. **After Task 11**: Incremental updates verified working

**Time estimate**: 
- Best case (everything goes smoothly): 2-3 days
- Realistic (with bugs/complications): 1 week  
- Worst case (major architectural issues): 2 weeks

---

## Success Criteria

After all tasks:

✅ **No global mutable state in analysis path**
- No `ref` or mutable `Hashtbl` in `Dead*.ml` modules
- All state is local or explicitly threaded
- **Zero `DceConfig.current()` calls in analysis code** - only at entry point

✅ **Order independence**
- Processing files in any order gives identical results
- Property test verifies this

✅ **Pure analysis function**
- Can call analysis and get results as data
- No side effects (logging, file I/O) during analysis
- **Solver returns immutable results** - no mutation of input state

✅ **Per-file state enables incremental updates**
- All per-file data (annotations, decls, refs) keyed by filename
- Can replace one file's data: `per_file_state[F] = new_data`
- Re-merge and re-solve without reprocessing other files

✅ **Clear separation of input vs output**
- Source annotations (from AST) are **read-only input**
- Analysis results (dead positions, issues) are **immutable output**
- Solver takes input, returns output - no mixing

✅ **Testable**
- Can test analysis without mocking I/O
- Can test with different configs without mutating globals
- Can test with isolated state
- Can verify solver doesn't mutate its input
