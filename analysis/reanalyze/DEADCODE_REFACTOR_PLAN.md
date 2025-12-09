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

### 1. Local mutable state during AST processing, immutable after

**AST processing phase** (per-file):
- Uses local mutable state for performance (hashtables, etc.)
- Returns **immutable** `file_data` when done
- This phase is inherently sequential per-file

**Analysis phase** (project-wide):
- Works only with **immutable data structures**
- Must be parallelizable, reorderable
- Static guarantees from this point on

```ocaml
(* AST processing: local mutable state OK, returns immutable *)
let process_file config cmt_infos : file_data =
  let local_state = Hashtbl.create 256 in  (* local mutable *)
  ... traverse AST, mutate local_state ...
  freeze_to_file_data local_state  (* return immutable *)

(* Analysis: immutable in, immutable out - parallelizable *)
let solve_deadness config (files : file_data list) : analysis_result =
  ... pure computation on immutable data ...
```

### 2. Clear phase boundaries

| Phase | Input | Mutability | Output | Parallelizable? |
|-------|-------|------------|--------|-----------------|
| **AST processing** | cmt file | Local mutable OK | Immutable `file_data` | Per-file yes |
| **Merge** | `file_data list` | None | Immutable merged view | Yes |
| **Analysis** | Merged view | None | Immutable `result` | Yes |
| **Reporting** | `result` | I/O side effects | None | N/A |

### 3. Enable incremental updates

When file F changes:
1. Re-run AST processing for F only → new `file_data`
2. Replace in `file_data` map (keyed by filename)
3. Re-run merge and analysis (on immutable data)

The key is that **immutable data structures enable safe incremental updates** -
you can swap one file's data without affecting others.

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

### P3: Cross-file processing queues
**Problem**: Several analyses use global queues that get "flushed" later:
- `DeadOptionalArgs.delayedItems` - cross-file optional arg analysis → DELETED (now `CrossFileItems`)
- `DeadException.delayedItems` - cross-file exception checks → DELETED (now `CrossFileItems`)
- `DeadType.TypeDependencies.delayedItems` - per-file type deps (already handled per-file)
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
- ~~`WriteDeadAnnotations` - file I/O~~ (removed - added complexity with little value)
- Direct mutation of result data structures

**Impact**: Can't get analysis results as data. Can't test without capturing I/O. Can't reuse analysis logic for different output formats.

### P6: Binding/reporting state
**Problem**: `DeadCommon.Current.bindings`, `lastBinding`, `maxValuePosEnd` are per-file state stored globally.

**Status**: ✅ ALREADY FIXED in previous work - now explicit state threaded through traversals.

---

## End State

```ocaml
(* ===== IMMUTABLE DATA TYPES ===== *)

(* Configuration: immutable *)
type config = { ... }

(* Per-file data - IMMUTABLE, returned by AST processing *)
type file_data = {
  source_path : string;
  module_name : Name.t;
  is_interface : bool;
  source_annotations : AnnotationMap.t;  (* immutable map *)
  decls : DeclMap.t;                     (* immutable map *)
  value_refs : RefMap.t;                 (* immutable map *)
  type_refs : RefMap.t;
  file_deps : StringSet.t;               (* files this depends on *)
}

(* Project-wide merged view - IMMUTABLE *)
type merged_view = {
  all_annotations : AnnotationMap.t;
  all_decls : DeclMap.t;
  all_value_refs : RefMap.t;
  all_type_refs : RefMap.t;
  file_graph : FileGraph.t;
}

(* Analysis results - IMMUTABLE *)
type analysis_result = {
  dead_decls : decl list;
  issues : issue list;
  annotations_to_write : (string * line_annotation list) list;
}

(* ===== PHASE 1: AST PROCESSING (local mutable OK) ===== *)

(* Uses local mutable hashtables for performance, returns immutable *)
let process_file config cmt_infos : file_data =
  (* Local mutable state - not visible outside this function *)
  let annotations = Hashtbl.create 64 in
  let decls = Hashtbl.create 256 in
  let refs = Hashtbl.create 256 in
  
  (* Traverse AST, populate local tables *)
  traverse_ast ~annotations ~decls ~refs cmt_infos;
  
  (* Freeze into immutable data *)
  {
    source_annotations = AnnotationMap.of_hashtbl annotations;
    decls = DeclMap.of_hashtbl decls;
    value_refs = RefMap.of_hashtbl refs;
    ...
  }

(* ===== PHASE 2: MERGE (pure, parallelizable) ===== *)

let merge_files (files : file_data StringMap.t) : merged_view =
  (* Pure merge of immutable data - can parallelize *)
  ...

(* ===== PHASE 3: ANALYSIS (pure, parallelizable) ===== *)

let solve_deadness config (view : merged_view) : analysis_result =
  (* Pure computation on immutable data *)
  (* Can be parallelized, reordered, memoized *)
  ...

(* ===== ORCHESTRATION ===== *)

let run_analysis ~config ~cmt_files =
  (* Phase 1: Process files (can parallelize per-file) *)
  let files = 
    cmt_files 
    |> List.map (fun path -> (path, process_file config (load_cmt path)))
    |> StringMap.of_list
  in
  (* Phase 2: Merge *)
  let merged = merge_files files in
  (* Phase 3: Analyze *)
  let result = solve_deadness config merged in
  (* Phase 4: Report (side effects) *)
  report result

(* Incremental: only re-process changed file *)
let update_file ~config ~files ~changed_file =
  let new_data = process_file config (load_cmt changed_file) in
  let files = StringMap.add changed_file new_data files in
  let merged = merge_files files in
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

### Task 3: Source annotations use map → list → merge pattern (P3)

**Value**: Demonstrates the "local mutable → immutable" architecture for one data type.
Shows the reusable pattern: **map** (per-file) → **list** → **merge** → **immutable result**.

**Changes**:
- [x] Create `FileAnnotations` module with two types:
  - `builder` - mutable, for AST processing
  - `t` - immutable, for solver (read-only)
- [x] `DceFileProcessing.process_cmt_file` returns `builder` (local mutable state)
- [x] `processCmtFiles` collects builders into a list (order doesn't matter)
- [x] `FileAnnotations.merge_all : builder list -> t` combines all into immutable result
- [x] Solver receives `t` (read-only, no mutation functions available)
- [x] **Remove solver mutation**: `resolveRecursiveRefs` no longer calls `annotate_dead`
- [x] **Use `decl.resolvedDead` directly**: Already-resolved decls use their stored result

**Status**: Complete ✅

**The Pattern** (reusable for Tasks 4-7):
```ocaml
(* Two types: mutable builder, immutable result *)
type builder  (* mutable - for AST processing *)
type t        (* immutable - for solver *)

(* Builder API *)
val create_builder : unit -> builder
val annotate_* : builder -> ... -> unit

(* Merge: list of builders → immutable result *)
val merge_all : builder list -> t

(* Read-only API for t *)
val is_annotated_* : t -> ... -> bool
```

**Architecture achieved**:
```
┌─────────────────────────────────────────────────────────────┐
│ MAP: process each file (parallelizable)                     │
│   process_cmt_file → builder (local mutable)                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
                      [ builder list ]
                      (order doesn't matter)
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ MERGE: combine all (pure)                                   │
│   merge_all builders → t (immutable)                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ ANALYZE: use immutable data                                 │
│   reportDead ~annotations:t (read-only)                     │
└─────────────────────────────────────────────────────────────┘
```

**Key properties**:
- **Order independence**: builders collected in any order → same result
- **Parallelizable**: map phase can run concurrently
- **Incremental**: replace one builder in list, re-merge
- **Type-safe**: `t` has no mutation functions in API

**Test**: Process files in different orders - results should be identical.

**Estimated effort**: Small (well-scoped module)

### Task 4: Declarations use map → list → merge pattern (P2)

**Value**: Declarations become immutable after AST processing. Enables parallelizable analysis.

**Pattern**: Same as Task 3 - `builder` (mutable) → `builder list` → `merge_all` → `t` (immutable)

**Changes**:
- [x] Create `Declarations` module with `builder` and `t` types
- [x] `process_cmt_file` returns `DceFileProcessing.file_data` containing both `annotations` and `decls` builders
- [x] `processCmtFiles` collects into `file_data list`
- [x] `Declarations.merge_all : builder list -> t`
- [x] Solver uses immutable `Declarations.t`
- [x] Delete global `DeadCommon.decls`
- [x] Update `DeadOptionalArgs.forceDelayedItems` to take `~decls:Declarations.t`

**Status**: Complete ✅

**Test**: Process files in different orders - results should be identical.

**Estimated effort**: Medium (core data structure, many call sites)

### Task 5: References use map → list → merge pattern (P2)

**Value**: References become immutable after AST processing.

**Pattern**: Same as Task 3/4.

**Changes**:
- [x] Create `References` module with `builder` and `t` types
- [x] Thread `~refs:References.builder` through `addValueReference`, `addTypeReference`
- [x] `process_cmt_file` returns `References.builder` in `file_data`
- [x] Merge refs into builder, process delayed items, then freeze
- [x] Solver uses `References.t` via `find_value_refs` and `find_type_refs`
- [x] Delete global `ValueReferences.table` and `TypeReferences.table`

**Status**: Complete ✅

**Test**: Process files in different orders - results should be identical.

**Estimated effort**: Medium (similar to Task 4)

### Task 6: Cross-file items use map → list → merge pattern (P3)

**Value**: No global queues. Cross-file items are per-file immutable data.

**Pattern**: Same as Task 3/4/5.

**Changes**:
- [x] Create `CrossFileItems` module with `builder` and `t` types
- [x] Thread `~cross_file:CrossFileItems.builder` through AST processing
- [x] `process_cmt_file` returns `CrossFileItems.builder` in `file_data`
- [x] `CrossFileItems.merge_all : builder list -> t`
- [x] `process_exception_refs` and `process_optional_args` are pure functions on merged `t`
- [x] Delete global `delayedItems` refs from `DeadException` and `DeadOptionalArgs`

**Status**: Complete ✅

**Note**: `DeadType.TypeDependencies` was already per-file (processed within `process_cmt_file`),
so it didn't need to be included.

**Key insight**: Cross-file items are references that span file boundaries.
They should follow the same pattern as everything else.

**Test**: Process files in different orders - results should be identical.

**Estimated effort**: Medium (3 modules)

### Task 7: File dependencies use map → list → merge pattern (P2 + P3)

**Value**: File graph built from immutable per-file data.

**Pattern**: Same as Task 3/4/5/6.

**Changes**:
- [x] Create `FileDeps` module with `builder` and `t` types
- [x] `process_cmt_file` returns `FileDeps.builder`
- [x] `FileDeps.merge_all : builder list -> t`
- [x] Thread `~file_deps` through `addValueReference`
- [x] `iter_files_from_roots_to_leaves : t -> (string -> unit) -> unit` (pure function)
- [x] Delete global `FileReferences` from `Common.ml`

**Status**: Complete ✅

**Test**: Build file graph, verify topological ordering is correct.

**Estimated effort**: Medium (cross-file logic, but well-contained)

### Task 8: Analysis phase is pure (P5)

**Value**: Analysis phase works on immutable merged data, returns immutable results.
Can be parallelized, memoized, reordered.

**Architecture goal**:
```
merged_view (immutable) 
    │
    ▼
solve_deadness (pure function)
    │
    ▼
analysis_result (immutable)
    │
    ▼
report (side effects here only)
```

**Approach**: Break into small, behavior-preserving steps. Each step can be verified
before moving to the next. The key is: change return type, then immediately log at
the call site, so behavior stays identical.

---

#### Task 8.1: Create `AnalysisResult` module ✅

**Changes**:
- [x] Create `AnalysisResult.ml/mli` with `type t = { issues: Common.issue list }`
- [x] Add constructors: `empty`, `add_issue`, `get_issues`
- [x] Add issue constructors: `make_dead_issue`, `make_dead_module_issue`

**Verify**: Build succeeds. No behavior change (module not used yet).

---

#### Task 8.2: Make `emitWarning` return issue (behavior preserving) ✅

**Changes**:
- [x] Add `makeDeadIssue` (pure function)
- [x] `emitWarning` uses `makeDeadIssue` internally (later removed)

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.3: Make `Decl.report` return issue option (behavior preserving) ✅

**Changes**:
- [x] Change `Decl.report` signature to return `issue option`
- [x] Use `makeDeadIssue` internally
- [x] At call site in `reportDead`, log returned issue

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.4: Make `DeadOptionalArgs.check` return issues (behavior preserving) ✅

**Changes**:
- [x] Add `foldUnused`, `foldAlwaysUsed` to `OptionalArgs` module
- [x] Change `check` signature to return `issue list` instead of `unit`
- [x] At call site in `resolveRecursiveRefs`, immediately log returned issues

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.5: Collect incorrect annotation issues (behavior preserving) ✅

**Changes**:
- [x] Use `makeDeadIssue` for incorrect `@dead` annotation issues
- [x] Log immediately at call site
- [x] Remove `emitWarning` function (no longer needed)

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.6: Make `DeadModules.checkModuleDead` return issue (behavior preserving) ✅

**Changes**:
- [x] Change `DeadModules.checkModuleDead` to return `issue option`
- [x] At call sites, log returned issue immediately

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.7: Collect all issues in `reportDead` (behavior preserving) ✅

**Changes**:
- [x] Change `Decl.report` to return `issue list` (includes dead module issues)
- [x] Use `List.concat_map` to collect all issues
- [x] Log all issues at the end of `reportDead`

**Verify**: `make test-analysis` passes with identical output.

---

#### Task 8.8: Return `AnalysisResult.t` from `reportDead` ✅

**Changes**:
- [x] Change `reportDead` to return `AnalysisResult.t` instead of `unit`
- [x] Move logging from `reportDead` to caller in `Reanalyze.ml`

**Verify**: `make test-analysis` passes with identical output.

---

**Status**: Complete ✅

**Key guarantee**: The analysis phase (`reportDead`) now returns an immutable
`AnalysisResult.t` containing all dead code issues. Side effects (logging)
only happen in the caller (`Reanalyze.runAnalysis`).

**Note**: Optional args and incorrect annotation issues were logged inline
during `resolveRecursiveRefs`. Fixed in Task 8b.

### Task 8b: Collect all issues in AnalysisResult.t (P5) ✅

**Value**: Complete the pure analysis phase - all issues returned in result, no inline logging.

**Problem**: `resolveRecursiveRefs` was logging two types of issues inline:
1. Optional args issues (from `checkOptionalArgFn`)
2. Incorrect `@dead` annotation issues

These bypassed `AnalysisResult.t` and were logged directly via `Log_.warning`.

**Changes**:
- [x] Pass `~issues:(Common.issue list ref)` through `resolveRecursiveRefs`
- [x] Collect optional args issues instead of logging inline
- [x] Collect incorrect annotation issues instead of logging inline
- [x] Add collected issues to `AnalysisResult.t` in `reportDead`
- [x] Remove all `Log_.warning` calls from `resolveRecursiveRefs`

**Status**: Complete ✅

**Key guarantee**: No `Log_.warning` calls in `resolveRecursiveRefs`. All issues
are collected in `AnalysisResult.t` and logged by the caller.

### Task 9: ~~Separate annotation computation from file writing (P5)~~ REMOVED

**Status**: Removed ✅ - `WriteDeadAnnotations` feature was deleted entirely.

The `-write` flag that auto-inserted `@dead` annotations into source files was removed
as it added significant complexity (global state, file I/O during analysis, extra types)
for a rarely-used feature. Users who want to suppress dead code warnings can manually
add `@dead` annotations.

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

**Completed**: Task 1 ✅, Task 2 ✅, Task 3 ✅, Task 10 ✅

**Remaining order**: 4 → 5 → 6 → 7 → 8 → 9 → 11 (test)

**Why this order?**
- Tasks 1-2 remove implicit dependencies (file context, config) - ✅ DONE
- Task 3 makes source annotations read-only (solver no longer mutates) - ✅ DONE
- Tasks 4-7 make state **per-file** for incremental updates
- Task 8 makes reporting **pure** with immutable results
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

## Optional Future Tasks

### Optional Task: Make OptionalArgs tracking immutable ✅

**Value**: `OptionalArgs.t` is now fully immutable. No mutation of declarations.

**Changes made**:
- [x] Made `OptionalArgs.t` immutable (no mutable fields)
- [x] Added pure functions: `apply_call`, `combine_pair`
- [x] Created `OptionalArgsState` module in `Common.ml` for state map
- [x] `compute_optional_args_state` returns immutable state map
- [x] `DeadOptionalArgs.check` looks up state from map

**Architecture**:
- Declaration's `optionalArgs` = initial state (what args exist)
- `OptionalArgsState.t` = computed state (after all calls/combines)
- Solver uses `OptionalArgsState.find_opt` to get final state

**Status**: Complete ✅

---

## Success Criteria

After all tasks:

✅ **Local mutable → Immutable boundary**
- AST processing uses local mutable state (performance)
- Returns **immutable** `file_data`
- Analysis phase works **only** on immutable data

✅ **Pure analysis phase**
- `solve_deadness : merged_view -> analysis_result` is pure
- No side effects (logging, I/O) in analysis
- Can parallelize, memoize, reorder

✅ **Incremental updates**
- Replace one file's `file_data` without touching others
- Re-merge is pure function on immutable data
- Re-analyze is pure function on immutable data

✅ **Order independence**
- Processing files in any order → identical `file_data`
- Merging in any order → identical `merged_view`
- Property test verifies this

✅ **Static guarantees**
- Type system enforces immutability after AST processing
- No `ref` or mutable `Hashtbl` visible in analysis phase API
- Compiler catches violations

✅ **Testable**
- Test AST processing in isolation (per-file)
- Test merge function in isolation (pure)
- Test analysis in isolation (pure)
- No mocking needed - just pass immutable data
