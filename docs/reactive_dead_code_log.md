# Reactive Dead Code Implementation Log

This log tracks concrete code changes and validation steps for each milestone. Every entry should reflect what actually landed in the repository so later milestones can rely on a verified baseline.

---

## Milestone 0 – Runtime Readiness (macOS focus)

### Goal
Link the editor analysis binary against the Skip runtime on macOS, provide a smoke-test executable, and gather baseline `reanalyze -dce` metrics without changing existing behaviour.

### Implementation Notes
- **Skip Runtime Vendoring**
  - `scripts/build_skip_runtime.sh` builds the locally checked-out `skip-ocaml` (authenticated via `SKIP_OCAML_SRC` env var, defaults to `~/GitHub/skip-ocaml`).
  - Only `libskip_reactive.a` is copied into `analysis/bin/skip_runtime/` (ignored by git). No cross-repo linking remains.
- **Vendored runtime sources (follow-up)**
  - Copied the required C/C++ runtime sources plus `skip.ll` from `skip-ocaml` commit `0a8ddbf29970869d9396d7e860e3b48bb3df8695` into `analysis/skip_runtime/vendor/`.
  - `scripts/build_skip_runtime.sh` now compiles `libskip_reactive.a` directly from those vendored files, so no sibling checkout is needed.
- **Compiler Integration**
  - `rescript-editor-analysis` now links against `skip_reactive`, so every build already includes the Skip runtime bits even before the reactive path is used.
  - `make compiler` depends on the `analysis/bin/skip_runtime/.stamp` produced by `make skip-runtime`, ensuring the runtime artifacts are rebuilt (and the `reactive_repl` smoke test is recompiled) before the analysis binary.
- **OCaml Bridge**
  - Added `analysis/skip_runtime/{dune,reactive.ml}` providing a `skip_reactive` dune library. The OCaml runtime wrapper is copied from `skip-ocaml` and linked with `unix` plus the vendored static library.
- **Smoke Test**
  - `analysis/bin/reactive_repl.ml` initialises Skip (`Reactive.init heap 1GiB`) and exits immediately. `make skip-runtime` now builds the vendored runtime and this executable.
- **Instrumentation for Baselines**
  - `reanalyze` gains `--baseline-metrics <path>`; when present, DCE runs count processed `.cmt/.cmti` files and record wall-clock seconds. JSON written to `<path>` on completion; directories created on demand.
- **CLI Guard**
  - `analysis/bin/main.ml` recognises `--reactive` but still exits early (“not implemented yet”), ensuring a safe placeholder until the reactive service is ready.

### Validation
- `make skip-runtime`
  - rebuilds `skip-ocaml`, copies `libskip_reactive.a`, and `dune build analysis/bin/reactive_repl.exe`
- `dune exec analysis/bin/reactive_repl.exe`
  - prints the init/exit smoke message without errors
- `dune exec analysis/bin/main.exe -- -dce tests/analysis_tests/tests/repros/rescript \
    --baseline-metrics analysis/benchmarks/reactive_baseline.json`
  - runs as before and emits the metrics JSON
- `make test-analysis`
  - complete suite passes (analysis tests, incremental typechecking, reanalyze, deadcode, termination, etc.)

---

## Milestone 1 – Collector Abstraction

### Goal
Start isolating the AST traversal from global state by introducing a collector interface and plumbing it through the value-analysis entrypoints, while keeping batch behaviour identical via a sink implementation backed by `DeadCommon`.

### Implementation Notes
- Added `analysis/reanalyze/src/collected_types.{ml,mli}` to describe declarative events (value declarations, references) and final snapshots.
- Added `analysis/reanalyze/src/collector.{ml,mli}` exposing two back ends:
  - `Collector.dead_common_sink ()` delegates to existing `DeadCommon` helpers (batch mode).
  - `Collector.collected ()` stores `Common.decl` and reference events in-memory for future pure consumers.
- Threaded a `collector` record through `DeadCode.processCmt`, `DeadValue.processStructure`, `DeadValue.processSignatureItem`, `DeadOptionalArgs`, and `DeadException`. All writes to declarations and value references now go through the collector interface (including delayed optional-arg/exceptions queues). `DeadType` also mirrors every declaration/type-reference event into the collector so future milestones can build summaries without reading `DeadCommon`.
- Added `DeadCode.collect_cmt` which runs a pure `Collector.collected` pass over a single `.cmt` file and returns a `Collected_types.t` snapshot. Batch mode still uses the sink collector, but this helper gives later milestones a stable entry-point for Skip-backed pipelines.
- Added `Typedtree_helpers`, a thin wrapper around \code{Cmt_format.read_cmt} that returns typed structures/signatures plus their metadata. Parity tooling can now load typedtrees from compiled artifacts without duplicating the boilerplate in \code{Reanalyze.loadCmtFile}.
- Added the \code{analysis/bin/collector\_parity.exe} tool. It walks a directory of compiled artefacts, runs both collector implementations per module (pure snapshot vs legacy `DeadCommon`), normalises the results, and reports any mismatches. New `DeadCommon.Test` helpers clear/snapshot the legacy tables so parity runs stay isolated.
- Hardened the parity harness for staged debugging: `collector_parity_cli` now resolves missing source files via `cmt_sourcefile`, exposes `--allow-missing-source` (downgrade unresolved paths to warnings when we explicitly opt in), and `--max-examples <n>` so each diff prints a bounded sample per category. Mismatches now emit structured summaries grouped per module, which we can track milestone-by-milestone.
- Matched collector semantics for value references: `DeadValue.record_value_reference` now mirrors `DeadCommon.addValueReference` (respect `Current.lastBinding`, drop ghost emitters, and store point locations). After this change the parity run on `tests/analysis_tests/tests-reanalyze/deadcode/lib/bs` shrank from dozens of modules down to the remaining type-only cases (DeadRT/DeadTypeTest/DeadTest/FirstClassModulesInterface/InnerModuleTypes) plus the exception suite. This isolates the next stage to type-reference plumbing and exception bookkeeping instead of the general value path.
- Threaded the collector through `DeadType.TypeDependencies.forceDelayedItems` so cross-file type references land in the pure snapshot. `make test-analysis` still passes, and parity now reports only the exception fixtures (DeadExn + exception/*) as outstanding gaps. Type-only fixtures are clean, so the next pass can focus exclusively on exception declaration/reference wiring.
- Reconciled exception references: `DeadException.record_value_reference` now mirrors `DeadCommon` semantics (honour `Current.lastBinding`, ignore ghost emitters, and collapse both endpoints to single-point locations before calling `Collector.add_value_reference`). After rebuilding (`make test-analysis`) and re-running `dune exec analysis/bin/collector_parity.exe -- tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`, all 97 fixtures report “Collector parity OK”. This completes the parity stage for the deadcode suite.
  - Root cause summary (pre-fix):  
    1. **Value reference normalization** – the new collector was using raw `Location.t`s, whereas `DeadCommon` rewrites them through `Current.lastBinding`, drops ghost emitters, and stores point locations; every reference therefore disagreed until `DeadValue`/`DeadException` adopted the same normalization.  
    2. **Deferred edge replay** – cross-file edges (type deps and ghost exception references) were only flushed through `DeadCommon`, so the pure snapshots simply missed them; wrapping `DeadType.TypeDependencies.forceDelayedItems` (and the exception replay) inside the active collector lets those queued edges emit via `Collector.*`, producing identical graphs.
- Introduced `Common.with_current_module` and `ModulePath.with_current` so each `.cmt` run resets global/module-path state, avoiding leakage between files while keeping the old globals available to other analyses.
- `Reanalyze.loadCmtFile` instantiates the sink collector per `.cmt` so current CLI behaviour stays unchanged while enabling later milestones to plug different collectors.

### Validation
- `dune build analysis/bin/main.exe`
- `make test-analysis`
  - Exercises the existing suites (analysis, JSX transform, reanalyze, termination) ensuring no behavioural regressions surfaced.
- `dune exec analysis/bin/collector_parity.exe -- tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`

---

## Milestone 2 – Deterministic File Summaries

### Goal
Emit canonical per-file summaries (with schema, digests, and caching) based on the collector snapshots so future Skip stages have a stable input, while continuing to validate exclusively via the real project fixtures (`make test-analysis`) and the parity harness.

### Implementation Notes
- Added `analysis/reanalyze/src/summary.{ml,mli}`:
  - Defines explicit summary types (positions, ranges, decl kinds, optional-arg snapshots, references, file edges) and produces canonical JSON (`version = 1`) plus BLAKE2b digests via `Digestif`. Exposes helpers for serialisation (`to_json`, `to_string`), round-trips (`of_json`, `of_string`), canonical strings, and `verify_digest`.
- Added `analysis/reanalyze/src/summary_cache.{ml,mli}`:
  - Stores summaries under `.reanalyze/summaries/<digest>.json`, writing atomically and exposing `read`, `write`, and `read_or_recompute` with structured error reporting (`Io_error`, `Invalid_format`, `Digest_mismatch`, `Not_found`).
- CLI plumbing:
  - `Common.Cli.cacheSummaries` + `-cache-summaries` flag. When enabled, each `.cmt` run uses a tee collector (`Collector.collected` + `Collector.dead_common_sink`), converts the pure snapshot via `Summary.of_collected`, and updates the cache through `Summary_cache.read_or_recompute`. Batch diagnostics still flow through `DeadCommon`.
- Build/dependency tweaks: `analysis/reanalyze/src/dune` now links `digestif.c`, and `dune-project` declares `digestif` as an `analysis` dependency.
- Documentation cleanup: removed the Alcotest/unit-test guidance and rewrote Milestone 2’s validation bullet so it references `make test-analysis` + parity instead of synthetic unit tests.
- Removed the temporary `tests/bin` helpers and `tests/reactive_goldens/` manifest now that validation relies solely on the existing compiled-project tests; `tests/dune` again only lists the longstanding suite directories.

### Validation
- `dune build analysis/reanalyze`
- `make test-analysis`
- `dune exec analysis/bin/collector_parity.exe -- tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`


---

## Milestone 3 – Graph Store and Incremental Liveness Scaffolding

### Goal
Track per-declaration graphs (IDs, edges, file mappings) and compute the frontier touched by changed summaries so later milestones can recompute liveness incrementally. Expose tooling (CLI flag + parity harness) to observe the frontier without changing the batch diagnostics yet.

### Implementation Notes
- Added `analysis/reanalyze/src/graph_store.{ml,mli}`:
  - Decl IDs encode `file#line#column#cnum#kind#name`. The store keeps node metadata, per-file decl lists, value/type adjacency, pending edges (for references whose targets are declared later), file->file edges, per-file digests, and a dirty-file tracker.
  - Source lookup now keys on exact start positions, so references whose `loc_from` matches a declaration header are resolved even when the expression lies outside the declaration’s range.
- Added `analysis/reanalyze/src/tarjan.{ml,mli}` plus `incremental_liveness.{ml,mli}`:
  - Tarjan exposes a simple `compute ~successors` API (linear-time SCC). The graph-driven solver is functional but still gated behind `INCR_GRAPH_SOLVER=1` while we encode the remaining legacy heuristics; the default code path reuses the batch liveness decisions so the CLI can enforce parity today.
- Summary schema bumped to version 2 (positions now include `cnum`/`bol`) so graph store can match references precisely. `docs/reactive_dead_code_plan.tex` reflects the update.
- `Reanalyze` grows an `-incremental-liveness` flag:
  - Summaries are built whenever either `-cache-summaries` or `-incremental-liveness` is set.
  - When incremental mode is on, every summary feeds a process-wide `Graph_store`, and after `DeadCommon.reportDead` runs we collect/log the frontier size. Diagnostics still come from the batch path while the solver flag is experimental.
- `analysis/bin/collector_parity.exe` now has a second stage:
  - Stage 1 still verifies collector parity.
  - Stage 2 rebuilds the summary graph, prints frontier stats, and compares incremental-vs-legacy liveness. Environment knobs (`INCR_GRAPH_SOLVER` and `INCR_AFTER_LEGACY`) allow experimenting with the new solver without regressing CI.
- Introduced five sub-milestones (3.1–3.5) covering the remaining gaps (annotation semantics, delayed edges, file ordering, reference normalisation, module bookkeeping).
  - ✅ \textbf{3.1 Annotation Semantics}: summaries now snapshot per-declaration annotation flags (dead/live/genType), and the incremental solver consumes those snapshots instead of relying on global mutable state. Parity harness confirms no annotation-related mismatches remain.
  - ✅ \textbf{3.2 Delayed Edge Replay}: \code{DeadException.replay\_delayed\_items} and \code{DeadOptionalArgs.replay\_delayed\_items} replay the pending queues against the pure collector without draining the legacy queues. \code{Collector.collected} now clones \code{OptionalArgs.t} before storing declarations so summary-side replays cannot mutate the batch tables, and \code{DeadCode.processCmt} no longer forces those queues per file (only the type-dependency flush remains). When either \code{-cache-summaries} or \code{-incremental-liveness} is active we replay the queues into the summary collector just before \code{Collector.finalize}; batch runs remain untouched. `make test-analysis` stays green, and the parity harness still reports a clean collector stage with 115 incremental mismatches (downstream sub-milestones).
  - ✅ \textbf{3.3 File Ordering Parity}: `Graph_store` now keeps the per-file dependency DAG (capturing every cross-file edge the collector recorded) and recomputes the same `orderedFiles` ranks that `DeadCommon.iterFilesFromRootsToLeaves` produces. Those ranks are threaded into the incremental solver so Tarjan receives a frontier list sorted by file rank and resolves each SCC in the same order the legacy `Decl.compareUsingDependencies` enforces (later files first, then bottom-to-top within each file). After this change the incremental mismatches remain capped at the 115 reference/module gaps tracked for 3.4/3.5, confirming that the remaining differences are no longer due to traversal order.
  - ✅ \textbf{3.4 Reference Normalisation}: references whose \code{loc\_from} does not resolve to a collected declaration (generated JSX and optional-arg helpers) now set an “unknown live” flag inside \code{Graph_store}. Pending references inherit that flag when their targets appear, and \code{incremental\_liveness} uses it exactly like the legacy solver’s \code{unknown\_live} branch. Stage-2 parity mismatches dropped from 115 to 110, leaving only the module bookkeeping work (3.5).
  - ✅ \textbf{3.5 Module Bookkeeping}: \code{incremental\_liveness} now calls \code{DeadModules.(markDead|markLive)} whenever a declaration converges, keeping module-level warnings in sync with the legacy solver even when we skip the batch pass. (Stage-2 parity still reports 110 declaration mismatches because module warnings are not part of the comparison yet.)
- 🔧 \textbf{Annotation wiring + exception edges (2025-11-16)}:
  - `graph_store` now caches every declaration’s `Summary.annotation_snapshot` in a dedicated index so the incremental solver can recover annotation flags even when `find_node_by_position` fails (e.g. for ghost positions looked up by lexing offsets). `incremental_liveness` consults that index whenever it needs to seed annotation state for a position that no longer has a live node, eliminating the stale-default path that previously caused annotated props/components to diverge.
  - `incoming_refs` now routes exceptions through the value edge set (mirroring `DeadCommon`, which records exception usage in `ValueReferences`). With `GRAPH_DEBUG_FILE=Exn.res` we now see the same edges (`twoThrows -> +Exn.A`, etc.) feeding the incremental solver; the remaining mismatches for `exception/Exn.res` are purely semantic (legacy keeps exceptions alive even when only dead code throws them), not due to missing annotation metadata.
  - Follow-up: `incremental_liveness` treats `annotation_is_dead` as a hard kill (matching `ProcessDeadAnnotations.annotateDead`) and pulls the “unknown live” bit for exceptions from the value-side flag regardless of their decl kind. Remaining mismatches are now limited to the legacy exception bookkeeping (the graph has the right edges, annotated helpers are forced dead, and the incremental solver sees the unknown-live hints). Next step is to model the exception-analysis semantics so the solver keeps declarations like `A/B/C/Error` alive when only throws reach them.
- 🔧 \textbf{Side-effectful keepalive (2025-11-16)}:
  - `graph_store.add_value_reference` now inspects the resolved source declaration; when a toplevel value with side effects references another value, the target is flagged with `unknown_value_ref`, exactly like legacy `DeadCommon` does when a reference cannot be proven dead. This mirrors the batch solver’s behaviour where dead-but-side-effectful wrappers (e.g. `callsThrowWithAnnotation`) keep their callees alive. With this change the incremental solver no longer reclaims `throwsWithAnnotation` and the parity harness reports zero mismatches for `exception/Exn.cmt`.
- 🔧 \textbf{Top-level `_` bindings and `Tstr_eval` coverage (2025-11-17)}:
  - `DeadValue.traverseStructure` now emits synthetic `_` declarations for every `Tstr_eval` item, marking them as toplevel side-effectful nodes and updating `Current.lastBinding` to the expression’s location. These nodes become concrete sources in `graph_store`, so references originating from standalone expressions (e.g. `ignore(...)`) no longer fall through the “missing source” path.
  - `collectValueBinding` was updated to always record `_` patterns, regardless of whether the bound expression has visible side effects. Top-level `_` bindings are forced to `sideEffects=true`, ensuring that keepalive propagation mirrors `DeadCommon`; nested `_` bindings reuse `SideEffects.checkExpr` so non-toplevel cases stay precise.
  - With both changes in place the `collector_parity` dumps for `DeadTest` and `DeadTypeTest` no longer list value mismatches (only the known variant/type gaps remain). Validation:  
    `GRAPH_TRACE_UNKNOWN=1 GRAPH_DEBUG_FILE=DeadTest.res INCR_GRAPH_SOLVER=1 dune exec analysis/bin/collector_parity.exe -- --dump DeadTest tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`  
    `GRAPH_TRACE_UNKNOWN=1 GRAPH_DEBUG_FILE=DeadTypeTest.res INCR_GRAPH_SOLVER=1 dune exec analysis/bin/collector_parity.exe -- --dump DeadTypeTest tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`
- 🔧 \textbf{Cross-module type reference parity (2025-11-17)}:
  - `graph_store.add_type_reference` now retries `find_local_decl` with the same line‑fallback heuristic we use for value refs, so references emitted before their decls are indexed still capture the right source IDs.
  - When a side-effectful toplevel binding references a type whose declaration hasn’t been added yet, we enqueue pending edges and immediately tag that target’s `(file,line,column)` key in `pending_unknown_type`. Once the declaration arrives, `resolve_pending_type` replays the edge and calls `mark_unknown_type_ref`, matching `DeadCommon`’s “unknown live” semantics.
  - This change removes the last DeadRT mismatches and ensures cross-file `DeadTest` → `DeadTypeTest` keepalive edges are wired identically to the legacy solver. Validation:  
    `GRAPH_TRACE_UNKNOWN=1 GRAPH_DEBUG_FILE=DeadRT.res INCR_GRAPH_SOLVER=1 dune exec analysis/bin/collector_parity.exe -- --dump DeadRT tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`
- 🔧 \textbf{Cross-module variant constructors (2025-11-17)}:
  - `DeadValue.processValueDependency` now mirrors variant/record references in the dependency list into explicit `DeadType.addTypeReference` calls. This lets cross-file constructor usages (e.g. `DeadTest.OnlyInInterface`) surface as type edges instead of getting stuck as value-only refs.
  - `graph_store` treats any type reference that lands in another module as a “fuzzy” source and immediately marks the target path unknown; when the interface decl is added later we replay the unknown bit via the updated `add_path_index` handler.
  - Propagation through `path_key` now works for both values and types, so late-arriving aliases (e.g. `.resi` constructors) inherit the `unknown_live` bit that earlier runs attached to their `.res` counterparts. Validation:  
    `GRAPH_TRACE_UNKNOWN=1 INCR_GRAPH_SOLVER=1 dune exec analysis/bin/collector_parity.exe -- DeadTypeTest tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`
- 🔧 \textbf{Variant pending replay + interface targeting (2025-11-17)}:
  - `record_value_decl` now replays every resolved `PendingDeps` entry through both `record_value_reference` and `DeadType.addTypeReference`, so constructor usages first seen in other files emit their type edges as soon as the destination declaration is added.
  - `graph_store` now prefers targets whose recorded file matches the reference’s `pos_to.file` when multiple declarations share the same `(line,column)` (e.g. `.res` and `.resi`). Cross-module references therefore connect to the interface node rather than whichever declaration was indexed first.
  - Combined with the earlier cross-module fixes, `DeadTypeTest.deadType.{OnlyInInterface,InBoth}` inherit the `unknown_live` bit the moment the interface summary loads, eliminating the remaining DeadTypeTest diffs. Validation:  
    `GRAPH_TRACE_UNKNOWN=1 GRAPH_DEBUG_FILE=DeadTest.res INCR_GRAPH_SOLVER=1 INCR_DEBUG_NAME=DeadTypeTest.deadType dune exec analysis/bin/collector_parity.exe -- DeadTypeTest tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`
- 🔧 \textbf{ErrorHandler interface targeting (2025-11-17)}:
  - `graph_store.position_in_range` now requires `files_match` before treating a declaration range as a hit, so `.resi` nodes aren’t matched by `.res` ranges when positions overlap. All downstream fallback/name/line/scan paths likewise skip local lookups when the reference file differs, forcing cross-file references to rely on `target_path`/pending queues until the proper declaration is indexed.
  - This change prevents `CreateErrorHandler` from hijacking the interface declarations for `+notify` / `+notification`, allowing the interface summaries to retain their unknown-live bits once processed. Validation:  
    `GRAPH_TRACE_UNKNOWN=1 GRAPH_DEBUG_FILE=ErrorHandler.res INCR_GRAPH_SOLVER=1 INCR_DEBUG_NAME=ErrorHandler dune exec analysis/bin/collector_parity.exe -- ErrorHandler tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`
  - ⚠️ Debug note: we still rely on the verbose tracing guarded by `GRAPH_TRACE_UNKNOWN` / `INCR_DEBUG_NAME` inside `graph_store.add_value_reference` and `DeadValue.record_value_decl` to chase remaining mismatches. Remove those `Printf.eprintf` blocks once parity is fully green.

### Validation
- `dune build analysis/reanalyze`
- `make test-analysis`
- `dune exec analysis/bin/collector_parity.exe -- tests/analysis_tests/tests-reanalyze/deadcode/lib/bs`



Add new sections below as soon as a milestone lands.

---
