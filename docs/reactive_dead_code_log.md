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

Add new sections below as soon as a milestone lands.

