## Dead Code Analysis – Pure Pipeline Refactor Plan

**Goal**: Turn the reanalyze dead code analysis into a transparent, effect-free pipeline where:
- Analysis is a pure function from inputs → results
- Global mutable state is eliminated
- Side effects (logging, file I/O) live at the edges
- Processing files in different orders gives the same results

**Why?** The current architecture makes:
- Incremental/reactive analysis impossible (can't reprocess one file)
- Testing hard (global state persists between tests)
- Parallelization impossible (shared mutable state)
- Reasoning difficult (order-dependent hidden mutations)

---

## Current Problems (What We're Fixing)

### P1: Global "current file" context
**Problem**: `Common.currentSrc`, `currentModule`, `currentModuleName` are global refs set before processing each file. Every function implicitly depends on "which file are we processing right now?". This makes it impossible to process multiple files concurrently or incrementally.

**Used by**: `DeadCommon.addDeclaration_`, `DeadType.addTypeDependenciesAcrossFiles`, `DeadValue` path construction.

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

**Impact**: Order-dependent. Processing files in different orders can give different results because queue processing happens at arbitrary times.

### P4: Global configuration reads
**Problem**: Analysis code directly reads `!Common.Cli.debug`, `RunConfig.runConfig.transitive`, etc. scattered throughout. Can't run analysis with different configs without mutating globals.

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
(* Configuration: all inputs as immutable data *)
type config = {
  run : RunConfig.t;          (* transitive, suppress lists, etc. *)
  debug : bool;
  write_annotations : bool;
  live_names : string list;
  live_paths : string list;
  exclude_paths : string list;
}

(* Per-file analysis state - everything needed to analyze one file *)
type file_state = {
  source_path : string;
  module_name : Name.t;
  is_interface : bool;
  annotations : annotation_state;
  (* ... other per-file state *)
}

(* Project-level analysis state - accumulated across all files *)
type project_state = {
  decls : decl PosHash.t;
  value_refs : PosSet.t PosHash.t;
  type_refs : PosSet.t PosHash.t;
  file_refs : FileSet.t FileHash.t;
  optional_args : optional_args_state;
  exceptions : exception_state;
  (* ... *)
}

(* Pure analysis function *)
val analyze_file : config -> file_state -> project_state -> Cmt_format.cmt_infos -> project_state

(* Pure deadness solver *)
val solve_deadness : config -> project_state -> analysis_result

type analysis_result = {
  dead_decls : decl list;
  issues : Common.issue list;
  annotations_to_write : (string * line_annotation list) list;
}

(* Side effects at the edge *)
let run_analysis ~config ~cmt_files =
  (* Pure: analyze all files *)
  let project_state = 
    cmt_files 
    |> List.fold_left (fun state file -> 
         analyze_file config (file_state_for file) state (load_cmt file)
       ) empty_project_state
  in
  (* Pure: solve deadness *)
  let result = solve_deadness config project_state in
  (* Impure: report results *)
  result.issues |> List.iter report_issue;
  if config.write_annotations then 
    result.annotations_to_write |> List.iter write_annotations_to_file
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
- [ ] Create `DeadFileContext.t` type with `source_path`, `module_name`, `is_interface` fields
- [ ] Thread through `DeadCode.processCmt`, `DeadValue`, `DeadType`, `DeadCommon.addDeclaration_`
- [ ] Remove all reads of `Common.currentSrc`, `currentModule`, `currentModuleName` from DCE code
- [ ] Delete the globals (or mark as deprecated if still used by Exception/Arnold)

**Test**: Run analysis on same files but vary the order - should get identical results.

**Estimated effort**: Medium (touches ~10 functions, mostly mechanical)

### Task 2: Extract configuration into explicit value (P4)

**Value**: Can run analysis with different configs without mutating globals. Can test with different configs.

**Changes**:
- [x] ~~Use the `DceConfig.t` already created, thread it through DCE analysis functions~~
- [x] ~~Replace all DCE code's `!Common.Cli.debug`, `runConfig.transitive`, etc. reads with `config.debug`, `config.run.transitive`~~
- [x] ~~Make all config parameters required (not optional) - no `config option` anywhere~~
- [ ] **Thread config through Exception and Arnold analyses** - they currently call `DceConfig.current()` at each use site
- [ ] **Single entry point**: Only `Reanalyze.runAnalysisAndReport` should call `DceConfig.current()` once, then pass explicit config everywhere

**Status**: DCE code complete ✅. Exception/Arnold still need threading.

**Test**: Create two configs with different settings, run analysis with each - should respect the config, not read globals.

**Estimated effort**: Medium (DCE done; Exception/Arnold similar effort)

### Task 3: Make `ProcessDeadAnnotations` state explicit (P3)

**Value**: Removes hidden global state. Makes annotation tracking testable.

**Changes**:
- [ ] Change `ProcessDeadAnnotations` functions to take/return explicit `state` instead of mutating `positionsAnnotated` ref
- [ ] Thread `annotation_state` through `DeadCode.processCmt`
- [ ] Delete the global `positionsAnnotated`

**Test**: Process two files "simultaneously" (two separate state values) - should not interfere.

**Estimated effort**: Small (well-scoped module)

### Task 4: Localize analysis tables (P2) - Part 1: Declarations

**Value**: First step toward incremental analysis. Can analyze a subset of files with isolated state.

**Changes**:
- [ ] Change `DeadCommon.addDeclaration_` and friends to take `decl_state : decl PosHash.t` parameter
- [ ] Thread through `DeadCode.processCmt` - allocate fresh state, pass through, return updated state
- [ ] Accumulate per-file states in `Reanalyze.processCmtFiles`
- [ ] Delete global `DeadCommon.decls`

**Test**: Analyze files with separate decl tables - should not interfere.

**Estimated effort**: Medium (core data structure, many call sites)

### Task 5: Localize analysis tables (P2) - Part 2: References

**Value**: Completes the localization of analysis state.

**Changes**:
- [ ] Same pattern as Task 4 but for `ValueReferences.table` and `TypeReferences.table`
- [ ] Thread explicit `value_refs` and `type_refs` parameters
- [ ] Delete global reference tables

**Test**: Same as Task 4.

**Estimated effort**: Medium (similar to Task 4)

### Task 6: Localize delayed processing queues (P3)

**Value**: Removes order dependence. Makes analysis deterministic.

**Changes**:
- [ ] `DeadOptionalArgs`: Thread explicit `state` with `delayed_items` and `function_refs`, delete global refs
- [ ] `DeadException`: Thread explicit `state` with `delayed_items` and `declarations`, delete global refs
- [ ] `DeadType.TypeDependencies`: Thread explicit `type_deps_state`, delete global ref
- [ ] Update `forceDelayedItems` calls to operate on explicit state

**Test**: Process files in different orders - delayed items should be processed consistently.

**Estimated effort**: Medium (3 modules, each similar to Task 3)

### Task 7: Localize file/module tracking (P2 + P3)

**Value**: Removes last major global state. Makes cross-file analysis explicit.

**Changes**:
- [ ] `FileReferences`: Replace global `table` with explicit `file_refs_state` parameter
- [ ] `DeadModules`: Replace global `table` with explicit `module_state` parameter  
- [ ] Thread both through analysis pipeline
- [ ] `iterFilesFromRootsToLeaves`: take explicit state, return ordered file list (pure)

**Test**: Build file reference graph in isolation, verify topological ordering is correct.

**Estimated effort**: Medium (cross-file logic, but well-contained)

### Task 8: Separate analysis from reporting (P5)

**Value**: Core analysis is now pure. Can get results as data. Can test without I/O.

**Changes**:
- [ ] `DeadCommon.reportDead`: Return `issue list` instead of calling `Log_.warning`
- [ ] `Decl.report`: Return `issue` instead of logging
- [ ] Remove all `Log_.warning`, `Log_.item`, `EmitJson` calls from `Dead*.ml` modules
- [ ] `Reanalyze.runAnalysis`: Call pure analysis, then separately report issues

**Test**: Run analysis, capture result list, verify no I/O side effects occurred.

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
- [ ] Verify `DceConfig.current()` only called in `Reanalyze.runAnalysisAndReport` (entry point)
- [ ] Verify no calls to `DceConfig.current()` in `Dead*.ml`, `Exception.ml`, `Arnold.ml` analysis code
- [ ] All analysis functions take explicit `~config` parameter

**Test**: `grep -r "DceConfig.current" analysis/reanalyze/src/{Dead,Exception,Arnold}.ml` returns zero results.

**Estimated effort**: Trivial (verification only, assuming Task 2 complete)

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

**Recommended order**: 1 → 2 (complete all analyses) → 3 → 4 → 5 → 6 → 7 → 8 → 9 → 10 (verify) → 11 (test)

**Why this order?**
- Tasks 1-2 remove implicit dependencies (file context, config) - these are foundational
- Task 2 must be **fully complete** (DCE + Exception + Arnold) before proceeding
- Tasks 3-7 localize global state - can be done incrementally once inputs are explicit
- Tasks 8-9 separate pure/impure - can only do this once state is local
- Task 10 verifies no global config reads remain
- Task 11 validates everything

**Alternative**: Could do 3-7 in any order (they're mostly independent).

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

✅ **Incremental analysis possible**
- Can create empty state and analyze just one file
- Can update state with new file without reanalyzing everything

✅ **Testable**
- Can test analysis without mocking I/O
- Can test with different configs without mutating globals
- Can test with isolated state
