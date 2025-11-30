## Dead Code Analysis – Pure Pipeline Refactor Plan

This document tracks the plan to turn the **reanalyze dead code analysis** into a transparent, effect‑free pipeline expressed as pure function composition. It is deliberately fine‑grained so each task can be done and checked off independently, while always keeping the system runnable and behaviour‑preserving.

Scope: only the **dead code / DCE** parts under `analysis/reanalyze/src`:
- `Reanalyze.ml` (DCE wiring)
- `DeadCode.ml`
- `DeadCommon.ml`
- `DeadValue.ml`
- `DeadType.ml`
- `DeadOptionalArgs.ml`
- `DeadException.ml`
- `DeadModules.ml`
- `SideEffects.ml`
- `WriteDeadAnnotations.ml` (only the pieces tied to DCE)
- Supporting shared state in `Common.ml`, `ModulePath.ml`, `Paths.ml`, `RunConfig.ml`, `Log_.ml`

Exception and termination analyses (`Exception.ml`, `Arnold.ml`, etc.) are out of scope except where they share state that must be disentangled.

---

## 1. Target Architecture: Pure Pipeline (End State)

This section describes the desired **end state**, not something to implement in one big change.

### 1.1 Top‑level inputs and outputs

**Inputs**
- CLI / configuration:
  - `RunConfig.t` (DCE flags, project root, transitive, suppression lists, etc.).
  - CLI flags from `Common.Cli` (`debug`, `ci`, `json`, `write`, `liveNames`, `livePaths`, `excludePaths`).
- Project context:
  - Root directory / `cmtRoot` or inferred `projectRoot`.
  - Discovered `cmt` / `cmti` files and their associated source files.
- Per‑file compiler artifacts:
  - `Cmt_format.cmt_infos` for each `*.cmt` / `*.cmti`.

**Outputs**
- Pure analysis results:
  - List of `Common.issue` values (dead values, dead types, dead exceptions, dead modules, dead/always‑supplied optional args, incorrect `@dead` annotations, circular dependency warnings).
  - Derived `@dead` line annotations per file (to be written back to source when enabled).
- Side‑effectful consumers (kept at the edges):
  - Terminal logging / JSON output (`Log_`, `EmitJson`).
  - File rewriting for `@dead` annotations (`WriteDeadAnnotations`).

### 1.2 File‑level pure API (end state)

Conceptual end‑state per‑file API:

```ocaml
type cli_config = {
  debug : bool;
  ci : bool;
  write_annotations : bool;
  live_names : string list;
  live_paths : string list;
  exclude_paths : string list;
}

type dce_config = {
  run : RunConfig.t;
  cli : cli_config;
}

type file_input = {
  cmt_path : string;
  source_path : string;
  cmt_infos : Cmt_format.cmt_infos;
}

type file_dce_result = {
  issues : Common.issue list;
  dead_annotations : WriteDeadAnnotations.line_annotation list;
}

val analyze_file_dce : dce_config -> file_input -> file_dce_result
```

The implementation of `analyze_file_dce` should be expressible as composition of small, pure steps (collect annotations, collect decls and refs, resolve dependencies, solve deadness, derive issues/annotations).

### 1.3 Project‑level pure API (end state)

End‑state project‑level API:

```ocaml
type project_input = {
  config : dce_config;
  files : file_input list;
}

type project_dce_result = {
  per_file : (string * file_dce_result) list; (* keyed by source path *)
  cross_file_issues : Common.issue list;      (* e.g. circular deps, dead modules *)
}

val analyze_project_dce : project_input -> project_dce_result
```

The actual implementation will be obtained incrementally by refactoring existing code; we do **not** introduce these types until they are immediately used in a small, behaviour‑preserving change.

---

## 2. Current Mutation and Order Dependencies (High‑Level)

This section summarises the main sources of mutation / order dependence that the tasks in §4 will address.

### 2.1 Global “current file” context

- `Common.currentSrc : string ref`
- `Common.currentModule : string ref`
- `Common.currentModuleName : Name.t ref`
- Set in `Reanalyze.loadCmtFile` before calling `DeadCode.processCmt`.
- Read by:
  - `DeadCommon.addDeclaration_` (filters declarations by `!currentSrc`).
  - `DeadType.addTypeDependenciesAcrossFiles` (decides interface vs implementation using `!currentSrc`).
  - `DeadValue` (builds paths using `!currentModuleName`).

### 2.2 Global declaration / reference tables and binding state

In `DeadCommon`:
- `decls : decl PosHash.t` – all declarations.
- `ValueReferences.table` – value references.
- `TypeReferences.table` – type references.
- `Current.bindings`, `Current.lastBinding`, `Current.maxValuePosEnd` – per‑file binding/reporting state.
- `ProcessDeadAnnotations.positionsAnnotated` – global annotation map.
- `FileReferences.table` / `iterFilesFromRootsToLeaves` – cross‑file graph and ordering using `Hashtbl`s.
- `reportDead` – mutates global state, constructs orderings, and logs warnings directly.

### 2.3 Per‑analysis mutable queues/sets

- `DeadOptionalArgs.delayedItems` / `functionReferences`.
- `DeadException.delayedItems` / `declarations`.
- `DeadType.TypeDependencies.delayedItems`.
- `DeadModules.table`.

All of these are refs or Hashtbls, updated during traversal and flushed later, with ordering mattering.

### 2.4 CLI/config globals and logging / annotation I/O

- `Common.Cli` refs, `RunConfig.runConfig` mutation.
- `Log_.warning`, `Log_.item`, `EmitJson` calls inside analysis modules.
- `WriteDeadAnnotations` holding refs to current file and lines, writing directly during analysis.

---

## 3. End‑State Summary

At the end of the refactor:

- All DCE computations are pure:
  - No `ref` / mutable `Hashtbl` in the core analysis path.
  - No writes to global state from `Dead*` modules.
  - No direct logging or file I/O from the dead‑code logic.
- Impure actions live only at the edges:
  - CLI parsing (`Reanalyze.cli`).
  - Discovering `cmt` / `cmti` files.
  - Logging / JSON (`Log_`, `EmitJson`).
  - Applying annotations to files (`WriteDeadAnnotations`).
- Results are order‑independent:
  - Processing files in different orders yields the same `project_dce_result`.

---

## 4. Refactor Tasks – From Mutable to Pure

This section lists **small, incremental changes**. Each checkbox is intended as a single PR/patch that:
- Starts from a clean, runnable state and returns to a clean, runnable state.
- Does **not** change user‑visible behaviour of DCE.
- Only introduces data structures that are immediately used to remove a specific mutation or implicit dependency.

Think “replace one wheel at a time while the car is moving”: every step should feel like a polished state, not a half‑converted architecture.

### 4.1 Make DCE configuration explicit (minimal surface)

Goal: introduce an explicit configuration value for DCE **without** changing how internals read it yet.

- [ ] Add a small `dce_config` record type (e.g. in `RunConfig.ml` or a new `DceConfig.ml`) that just wraps existing data, for example:
      `type dce_config = { run : RunConfig.t; cli_debug : bool; cli_json : bool; cli_write : bool }`
- [ ] Add a helper `DceConfig.current () : dce_config` that reads from `RunConfig.runConfig` and `Common.Cli` and returns a value.
- [ ] Change `Reanalyze.runAnalysis` to take a `dce_config` parameter, but initially always pass `DceConfig.current ()` and keep all existing global reads unchanged.

Result: a single, well‑typed configuration value is threaded at the top level, but internals still use the old globals. No behaviour change.

### 4.2 Encapsulate global “current file” state (one module at a time)

Goal: step‑wise removal of `Common.currentSrc`, `currentModule`, `currentModuleName` as implicit inputs.

- [ ] Define a lightweight `file_ctx` record (e.g. in a new `DeadFileContext` module):
      `type t = { source_path : string; module_name : Name.t; module_path : Name.t list; is_interface : bool }`
- [ ] In `Reanalyze.loadCmtFile`, build a `file_ctx` value *in addition to* updating `Common.current*` so behaviour stays identical.
- [ ] Update `DeadCommon.addDeclaration_` to take a `file_ctx` parameter and use it **only to replace** the check that currently uses `!currentSrc` / `!currentModule`. Call sites pass the new `file_ctx` while still relying on globals elsewhere.
- [ ] In a follow‑up patch, change `DeadType.addTypeDependenciesAcrossFiles` to take `is_interface` from `file_ctx` instead of reading `!Common.currentSrc`. Again, call sites pass `file_ctx`.
- [ ] Update `DeadValue` call sites that construct paths (using `!Common.currentModuleName`) to accept `file_ctx` and use its `module_name` instead.
- [ ] Once all reads of `Common.currentSrc`, `currentModule`, `currentModuleName` in DCE code are replaced by fields from `file_ctx`, remove or deprecate these globals from the DCE path (they may still exist for other analyses).

Each bullet above should be done as a separate patch touching only a small set of functions.

### 4.3 Localise `Current.*` binding state

Goal: remove `DeadCommon.Current.bindings`, `lastBinding`, and `maxValuePosEnd` as mutable globals by turning them into local state threaded through functions.

- [ ] In `DeadCommon`, define:
      ```ocaml
      type current_state = {
        bindings : PosSet.t;
        last_binding : Location.t;
        max_value_pos_end : Lexing.position;
      }

      let empty_current_state = {
        bindings = PosSet.empty;
        last_binding = Location.none;
        max_value_pos_end = Lexing.dummy_pos;
      }
      ```
- [ ] Change `addValueReference` to take a `current_state` and return an updated `current_state` instead of reading/writing `Current.*`. For the first patch, implement it by calling the existing global‑based logic and then mirroring the resulting values into a `current_state`, so behaviour is identical.
- [ ] Update the places that call `addValueReference` (mainly in `DeadValue`) to thread a `current_state` value through, starting from `empty_current_state`, and ignore `Current.*`.
- [ ] In a follow‑up patch, re‑implement `addValueReference` and any other helpers that touch `Current.*` purely in terms of `current_state` and delete the `Current.*` refs from DCE code.

At the end of this step, binding‑related state is explicit and confined to the call chains that need it.

### 4.4 Make `ProcessDeadAnnotations` state explicit

Goal: turn `ProcessDeadAnnotations.positionsAnnotated` into an explicit value rather than a hidden global.

- [ ] Introduce:
      ```ocaml
      module ProcessDeadAnnotations : sig
        type state
        val empty : state
        (* new, pure API; existing API kept temporarily *)
      end
      ```
- [ ] Add pure variants of the mutating functions:
      - `annotateGenType' : state -> Lexing.position -> state`
      - `annotateDead'   : state -> Lexing.position -> state`
      - `annotateLive'   : state -> Lexing.position -> state`
      - `isAnnotated*'   : state -> Lexing.position -> bool`
      leaving the old global‑based functions in place for now.
- [ ] Change `ProcessDeadAnnotations.structure` and `.signature` to:
      - Take an explicit `state`,
      - Call the `'` functions,
      - Return the updated `state` along with the original AST.
- [ ] Update `DeadCode.processCmt` to allocate a fresh `ProcessDeadAnnotations.state` per file, thread it through the structure/signature walkers, and store it alongside other per‑file information.
- [ ] Once all users have switched to the state‑passing API, delete or deprecate direct uses of `positionsAnnotated` and the old global helpers.

### 4.5 De‑globalize `DeadOptionalArgs` (minimal slice)

Goal: remove the `delayedItems` and `functionReferences` refs, one small step at a time.

- [ ] Introduce in `DeadOptionalArgs`:
      ```ocaml
      type state = {
        delayed_items : item list;
        function_refs : (Lexing.position * Lexing.position) list;
      }

      let empty_state = { delayed_items = []; function_refs = [] }
      ```
- [ ] Add pure variants:
      - `addReferences' : state -> ... -> state`
      - `addFunctionReference' : state -> ... -> state`
      - `forceDelayedItems' : state -> decls -> state * decls`
      and make the existing functions delegate to these, passing a hidden global `state` for now.
- [ ] Update `DeadValue` to allocate a `DeadOptionalArgs.state` per file and call the `'` variants, **without** changing behaviour (the old global still exists for other callers until fully migrated).
- [ ] Update `Reanalyze.runAnalysis` (or the relevant driver) to call `forceDelayedItems'` with an explicit state instead of `DeadOptionalArgs.forceDelayedItems`.
- [ ] When all call sites use the new API, remove `delayedItems` and `functionReferences` refs and the global wrapper.

### 4.6 De‑globalize `DeadException` (minimal slice)

Goal: make delayed exception uses explicit.

- [ ] Introduce:
      ```ocaml
      type state = {
        delayed_items : item list;
        declarations : (Path.t, Location.t) Hashtbl.t;
      }

      val empty_state : unit -> state
      ```
- [ ] Add state‑passing versions of `add`, `markAsUsed`, and `forceDelayedItems` that operate on a `state` value, with old variants delegating to them using a hidden global state.
- [ ] Update `DeadValue` and any other DCE callers to allocate a `DeadException.state` per file and use the state‑passing API.
- [ ] Replace the global `DeadException.forceDelayedItems` call in `Reanalyze.runAnalysis` with a call on the explicit state.
- [ ] Remove the old globals once all uses go through the new API.

### 4.7 Localise `decls`, `ValueReferences`, and `TypeReferences`

Goal: move the main declaration and reference tables out of global scope, **one structure at a time**.

- [ ] For `decls`:
      - Introduce `type decl_state = decl PosHash.t`.
      - Change `addDeclaration_` to take and return a `decl_state`, with an adapter that still passes the existing global `decls` to keep behaviour unchanged.
      - Thread `decl_state` through `DeadValue`, `DeadType`, and `DeadCode.processCmt`, returning the updated `decl_state` per file.
- [ ] For value references:
      - Introduce `type value_refs_state = PosSet.t PosHash.t`.
      - Parameterise `ValueReferences.add` / `find` over `value_refs_state`, with wrappers that still use the global table.
      - Thread `value_refs_state` through the same paths that currently use `ValueReferences.table`.
- [ ] For type references:
      - Introduce `type type_refs_state = PosSet.t PosHash.t`.
      - Parameterise `TypeReferences.add` / `find` over `type_refs_state` in the same way.
- [ ] Once all three structures are threaded explicitly per file, delete the global `decls`, `ValueReferences.table`, and `TypeReferences.table` in DCE code and construct fresh instances in `DeadCode.processCmt`.

Each of these bullets should be implemented as a separate patch (decls first, then value refs, then type refs).

### 4.8 Pure `TypeDependencies` in `DeadType`

Goal: make `DeadType.TypeDependencies` operate on explicit state rather than a ref.

- [ ] Introduce `type type_deps_state = (Location.t * Location.t) list` (or a small record) to represent delayed type dependency pairs.
- [ ] Change `TypeDependencies.add`, `clear`, and `forceDelayedItems` to take and return a `type_deps_state` instead of writing to a ref, keeping wrappers that still use the old global for the first patch.
- [ ] Update `DeadType.addDeclaration` and any other callers to thread a `type_deps_state` along with other per‑file state.
- [ ] Remove the global `delayedItems` ref once all calls have been migrated to the new API.

### 4.9 De‑globalize `DeadModules`

Goal: turn module deadness tracking into project‑level data passed explicitly.

- [ ] Introduce `type module_dead_state = (Name.t, (bool * Location.t)) Hashtbl.t` in `DeadModules` and keep the existing `table` as `module_dead_state` for the first patch.
- [ ] Change `markDead` and `markLive` to take a `module_dead_state` and operate on it, with wrappers that pass the global `table`.
- [ ] Update the calls in deadness resolution (in `DeadCommon.resolveRecursiveRefs`) to use a `module_dead_state` passed in from the caller.
- [ ] Replace `DeadModules.checkModuleDead` so that it:
      - Takes `module_dead_state` and file name,
      - Returns a list of `Common.issue` values, leaving logging to the caller.
- [ ] Once all uses go through explicit state, remove the global `table` and construct a `module_dead_state` in a project‑level driver.

### 4.10 Pure `FileReferences` and `iterFilesFromRootsToLeaves`

Goal: make file ordering and cross‑file references explicit and order‑independent.

- [ ] Extract `FileReferences.table` into a new type `file_refs_state` (e.g. `string -> FileSet.t`) and parameterise `add`, `addFile`, and `iter` over this state, with wrappers retaining the old global behaviour initially.
- [ ] Rewrite `iterFilesFromRootsToLeaves` to:
      - Take a `file_refs_state`,
      - Return an ordered list of file names (plus any diagnostics for circular dependencies),
      - Avoid any hidden mutation beyond local variables.
- [ ] Update `DeadCommon.reportDead` to:
      - Call the new pure `iterFilesFromRootsToLeaves`,
      - Use the returned ordering instead of relying on a global `orderedFiles` table.
- [ ] Remove the global `FileReferences.table` once the project‑level driver constructs and passes in a `file_refs_state`.

### 4.11 Separate deadness solving from reporting

Goal: compute which declarations are dead/live purely, then render/report in a separate step.

- [ ] Extract the recursive deadness logic (`resolveRecursiveRefs`, `declIsDead`, plus the bookkeeping that populates `deadDeclarations`) into a function that:
      - Takes a fully built project‑level state (decls, refs, annotations, module_dead_state),
      - Returns the same state augmented with dead/live flags and a list of “dead declaration” descriptors.
- [ ] Replace `Decl.report`’s direct calls to `Log_.warning` with construction of `Common.issue` values, collected into a list.
- [ ] Change `DeadCommon.reportDead` to:
      - Return the list of `issue`s instead of logging them,
      - Leave logging and JSON emission to the caller (`Reanalyze`).

This should only be done after the relevant state has been made explicit by earlier tasks.

### 4.12 Make CLI / configuration explicit internally

Goal: stop reading `Common.Cli.*` and `RunConfig.runConfig` directly inside DCE code.

- [ ] Replace direct reads in `DeadCommon`, `DeadValue`, `DeadType`, `DeadOptionalArgs`, `DeadModules` with fields from the `dce_config` value introduced in 4.1, passed down from `Reanalyze`.
- [ ] Ensure each function that previously reached into globals now takes the specific configuration flags it needs (or a narrowed config record), minimising the surface area.
- [ ] Once all reads have been converted, keep `DceConfig.current ()` as the only place that touches the global `RunConfig` and `Common.Cli` for DCE.

### 4.13 Isolate logging / JSON and annotation writing

Goal: keep the core analysis free of side‑effects and move all I/O into thin wrappers.

- [ ] Identify all calls to `Log_.warning`, `Log_.item`, and `EmitJson` in DCE modules and replace them with construction of `Common.issue` values (or similar purely data‑oriented records).
- [ ] Add a `DceReporter` (or reuse `Reanalyze`) that:
      - Takes `issue list`,
      - Emits logs / JSON using `Log_` and `EmitJson`.
- [ ] In `WriteDeadAnnotations`, introduce a pure function that, given per‑file deadness information, computes the textual updates to apply. Keep file I/O in a separate `apply_updates` wrapper.
- [ ] Update `Reanalyze.runAnalysis` to:
      - Call the pure analysis pipeline,
      - Then call `DceReporter` and `WriteDeadAnnotations.apply_updates` as needed.

### 4.14 Verify order independence

Goal: ensure the new pure pipeline is not order‑dependent.

- [ ] Add tests (or property checks) that:
      - Compare `project_dce_result` when files are processed in different orders,
      - Verify deadness decisions for declarations do not change with traversal order.
- [ ] If order dependence is discovered, treat it as a bug and introduce explicit data flow to remove it (document any necessary constraints in this plan).

---

## 5. Suggested Execution Order

Recommended rough order of tasks (each remains independent and small):

1. 4.1 – Introduce and thread `dce_config` at the top level.
2. 4.2 – Start passing explicit `file_ctx` and remove `current*` reads.
3. 4.3 / 4.4 – Localise binding state and annotation state.
4. 4.5 / 4.6 / 4.7 / 4.8 – De‑globalize optional args, exceptions, decls/refs, and type dependencies in small slices.
5. 4.9 / 4.10 – Make file/module state explicit and pure.
6. 4.11 – Separate deadness solving from reporting, returning issues instead of logging.
7. 4.12 / 4.13 – Remove remaining global config/logging/annotation side‑effects.
8. 4.14 – Add and maintain order‑independence tests.

Each checkbox above should be updated to `[x]` as the corresponding change lands, keeping the codebase runnable and behaviour‑preserving after every step.

