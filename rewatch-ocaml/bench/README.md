# OCaml rewatch performance checks

These scripts compare build time, compiler work, generated artifacts, and
resource use. Run them on Linux; the performance and watch gates require
`/proc` and GNU utilities. Build both implementations with the same compiler,
runtime, Dune profile, and installed testrepo dependencies. Keep executable
locations comparable: launching `bsc` from a different filesystem has changed
the measured Rust baseline substantially.

## Embedded compiler baseline

Run the fixed-setting baseline for clean, no-op, implementation edit,
shared-interface edit, GenType, an external identity PPX, and build-after-restart
requests. It also measures a 201-module project for clean, no-op, leaf edit, and
shared-interface edit. Each scenario runs with `REWATCH_BIN_ANNOT` unset, `0`,
and `1`, in alternating order. The runner records elapsed time, `wait4` CPU,
embedded OCaml allocation, peak RSS, and generated artifact size.

```sh
opam exec -- dune build rewatch-ocaml/rescript_ocaml.exe \
  compiler/bsc/rescript_compiler_main.exe
python3 rewatch-ocaml/bench/embedded_baseline.py \
  /tmp/embedded-baseline --runs 5 --domains 4
```

The output directory must not exist. It contains raw CSV samples, executable
hashes and host settings in `metadata.json`, and per-request logs. The recorded
starting point for this plan is [the 2026-09-30 baseline](baseline-2026-09-30.md).
The identity PPX measures the external protocol and subprocess boundary; use a
real PPX fixture when validating transformation behavior.

The [checkpoint 2 comparison](checkpoint-2-2026-09-30.md) records typed-request
option caching, interleaved measurements, and the one-domain artifact gate.
The [checkpoint 3 comparison](checkpoint-3-2026-09-30.md) records independent
compiler and editor policies and GenType artifact parity.
The [checkpoint 4 comparison](checkpoint-4-2026-09-30.md) records owned GenType
semantic inputs, reduced CMT reads, and interface precedence checks.
The [checkpoint 5 comparison](checkpoint-5-2026-09-30.md) records versioned
dependency summaries, recursive-read traces, TypeScript parity, and fallback
behavior. `gentype_summary_parity.py` checks clean, edit, and restart builds in
all three annotation modes against the previous executable. The focused
`gentype_summary_benchmark.py` measures clean, unchanged, and restarted edit
builds of its recursive-import fixture. Pass both runners the old executable,
new executable, and a new output directory, in that order.

## Build and compare

From the repository root:

```sh
yarn --cwd rewatch/testrepo install --immutable
opam exec -- make lib
cargo build --manifest-path rewatch/Cargo.toml --release
opam exec -- dune build --profile release \
  compiler/bsc/rescript_compiler_main.exe \
  rewatch-ocaml/rescript_ocaml.exe
export RESCRIPT_BSC_EXE="$PWD/_build/default/compiler/bsc/rescript_compiler_main.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
rewatch-ocaml/bench/performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe 5
rewatch-ocaml/bench/watch_performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe 7
```

The build gate interleaves clean, unchanged, and one-file-edit runs in isolated
copies of the testrepo fixture. It compares logical compiler requests, complete
generated file sets, and byte-stable artifacts, then reports elapsed time and
sampled process-tree RSS. The watch gate checks edit latency, generated
JavaScript, compiler work, and retained file descriptors, tasks, and RSS.
Run counts must be odd and at least five. The build gate's default clean-build
time and memory limit is 125% of the reference; the watch gate's median latency
limit is 150%.

To compare two OCaml revisions, put both executables in the same directory and
set `REWATCH_FIRST_EMBEDDED=1`. The first executable is still labeled `Rust`
in the gate output. Use the same standalone compiler and runtime for both.
Set `REWATCH_COMPILER_DOMAINS` or `REWATCH_WATCH_COMPILER_DOMAINS` to hold the
worker count fixed. `KEEP_REWATCH_BENCHMARK_WORKDIR=1` and
`KEEP_REWATCH_WATCH_PERFORMANCE=1` retain raw results.

Report host architecture, filesystem, compiler and executable hashes, Dune
profile, worker count, run count, elapsed medians, sampled RSS, compiler work,
and artifact comparison. The gates print most of these. Compare medians only
within one interleaved run: historical checkpoints used different code,
fixtures, compiler placement, and measurement methods. A worker-time trace
sums concurrent requests; its total is not elapsed build time.

## Compiler probes

Set `REWATCH_TYPECHECK_TRACE` to an absolute TSV path to collect exclusive
compiler-phase timings, then analyze the trace with:

```sh
node rewatch-ocaml/bench/analyze_typecheck_trace.js /tmp/typecheck.trace.tsv
```

The trace is opt-in and adds instrumentation. The
`rewatch-ocaml/bench/typecheck_breakdown.sh` runner compares traced and plain
builds and archives their work, artifacts, and timings. The
`REWATCH_COMPILER_TIMING_LOG` setting and
`rewatch-ocaml/bench/analyze_compiler_timing.js` provide a lighter request
timeline.

For repeated imports of a large synthetic interface, generate a 201-module
project with:

```sh
python3 rewatch-ocaml/bench/make_immutable_interface_fixture.py /tmp/rewatch-interface-probe
```

The generator also accepts `--values-only`, `--types-only`,
`--variants-only`, `--modules-only`, or `--open-only`. Compare builds with
`REWATCH_FROZEN_VALUES=0` and the default setting while holding binary
annotations and worker count fixed. The type-graph microprobe is
`rewatch-ocaml/bench/frozen_type_graph_probe.exe`; build it with Dune and pass
a CMI path and iteration count.

## Filesystem and source-size audits

```sh
rewatch-ocaml/bench/filesystem_audit.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
rewatch-ocaml/bench/watch_filesystem_audit.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
```

These `strace`-based audits normalize project-local operations for clean,
unchanged, edit, and retained-watch work. Inspect repeated accesses to the
same project paths; process-wide syscall totals also include runtime and
toolchain differences. Set `KEEP_REWATCH_FILESYSTEM_AUDIT=1` or
`KEEP_REWATCH_WATCH_AUDIT=1` to retain raw traces.

Run `rewatch-ocaml/bench/source_size.sh` with `cloc` to compare production,
test, and benchmark source sizes. Line counts are descriptive; use the
behavioral, artifact, resource, and performance gates to assess a change.
