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

Add new sections below as soon as a milestone lands.

