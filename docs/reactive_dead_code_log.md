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

## Milestone 1 – Collector Abstraction (partial)

### Goal
Start isolating the AST traversal from global state by introducing a collector interface and plumbing it through the value-analysis entrypoints, while keeping batch behaviour identical via a sink implementation backed by `DeadCommon`.

### Implementation Notes
- Added `analysis/reanalyze/src/collected_types.{ml,mli}` to describe declarative events (value declarations, references) and final snapshots.
- Added `analysis/reanalyze/src/collector.{ml,mli}` exposing two back ends:
  - `Collector.dead_common_sink ()` delegates to existing `DeadCommon` helpers (batch mode).
  - `Collector.collected ()` stores `Common.decl` and reference events in-memory for future pure consumers.
- Threaded a `collector` record through `DeadCode.processCmt`, `DeadValue.processStructure`, `DeadValue.processSignatureItem`, and `DeadException.add`. All writes to declarations and value references now go through the collector interface. `DeadType` also mirrors every declaration/type-reference event into the collector so future milestones can build summaries without reading `DeadCommon`.
- Introduced `Common.with_current_module` and `ModulePath.with_current` so each `.cmt` run resets global/module-path state, avoiding leakage between files while keeping the old globals available to other analyses.
- `Reanalyze.loadCmtFile` instantiates the sink collector per `.cmt` so current CLI behaviour stays unchanged while enabling later milestones to plug different collectors.

### Validation
- `dune build analysis/bin/main.exe`
- `make test-analysis`
  - Exercises the existing suites (analysis, JSX transform, reanalyze, termination) ensuring no behavioural regressions surfaced.

---

Add new sections below as soon as a milestone lands.

