# OCaml rewatch performance measurements

The OCaml rewatch now uses parallel in-process compiler domains by default.
`REWATCH_COMPILER_DOMAINS` overrides the CPU-based worker count; without it,
the worker count is `min(8, max(1, available CPUs - 1))`. Rust rewatch is the
build-level reference for compiler work, generated file sets, and stable
artifact bytes. Keep both implementations on the same compiler and runtime
build and the same Dune profile when comparing them. The profile affects
serialized AST and CMI bytes for at least one testrepo dependency, even when
compiler request arguments are identical.

## Domain baseline

On a ten-CPU Linux ARM64 host, clean builds of the 472-module testrepo fixture
at the same absolute path measured the following directional medians. The host
had light unrelated work. Each domain build's selected AST, IAST, CMI, CMJ,
JavaScript, source-map, and namespace-map artifacts matched a sequential
embedded baseline in the repeated checks performed before the mode cleanup.

| compiler workers | samples | median wall |
| ---: | ---: | ---: |
| 1 | 3 | 5.49s |
| 2 | 3 | 4.56s |
| 4 | 3 | 2.40s |
| 8 | 10 | 1.94s |
| 12 | 10 | 2.17s |

In separate samples, Rust rewatch took 4.62s, sequential embedded OCaml took
5.22s, one persistent process worker took 10.37s, and sequential external
compiler processes took 27.25s. Those last three execution modes have since
been removed. A later five-run comparison of eight, ten, and twelve domains
measured clean-build medians of 2.19s, 2.58s, and 2.37s, with peak RSS of
336, 430, and 499 MiB respectively. All fifteen selected-artifact comparisons
matched. Eight workers gave the best result on this host; the heuristic remains
provisional.

One earlier twelve-domain build differed in two generated copies of `Net.mjs`.
Forty focused repeats and ten full selected-artifact comparisons did not
reproduce it. The cause remains unknown; compare stable artifacts as well as
wall time while testing parallel changes.

The retained-watch gate with eight workers passed seven ordinary edits: OCaml
median 82 ms versus Rust 132 ms, equal seven-parse/seven-compile work, and
byte-identical edited JavaScript. The watcher held 12 file descriptors and
four tasks; RSS rose from 26,680 to 27,688 KiB. That small fixture dirties
only one module per edit and does not establish watch-time scaling.

## Direct before-and-after comparison

Revision `95467b2bf` is the parent of the parallel in-process compiler change.
It still launches standalone `bsc` requests. On the same 12-CPU Linux ARM64
host, five interleaved testrepo runs compared its release executable with
revision `7494d76ece1f33e8a808c26206194b7a20158766`. Both used the
current lockfile-pinned fixture, release-profile `bsc`, and local runtime.
Compiler source files did not change between these revisions:

| scenario | before median wall | after median wall | before peak tree RSS | after peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean | 4,045 ms | 1,420 ms | 302,984 KiB | 365,916 KiB |
| Unchanged | 187 ms | 71 ms | 42,336 KiB | 26,080 KiB |
| One source edit | 185 ms | 70 ms | 53,052 KiB | 26,152 KiB |

The clean build became 2.85x faster with 1.21x sampled peak tree RSS. Both
revisions made the same 1,031 clean compiler requests and the same unchanged
and edit requests. Complete post-build file sets and selected stable artifact
bytes matched. The before/after gate passed its 125% clean RSS limit in this
comparison. The first executable is labeled `Rust` by the reusable benchmark
script below, but it is the older OCaml build system; its external compiler
calls are observed by the same `bsc` proxy. This comparison measures the
revision range, including the in-process compiler and parallel scheduling,
not an isolated compiler micro-optimization.

In a separate seven-edit retained-watch comparison, median latency fell from
161 to 83 ms. Both revisions made seven parser and seven compiler requests
and produced identical edited JavaScript. The older watcher's RSS rose from
10,960 to 11,736 KiB and the current watcher's from 26,800 to 27,932 KiB;
both held stable file descriptor and task counts.

To reproduce the before/after gate after preparing the dependencies and
release compiler/runtime as described below:

```sh
git worktree add --detach /tmp/rewatch-before-954 95467b2bf
(cd /tmp/rewatch-before-954 && \
  opam exec -- dune build --profile release rewatch-ocaml/rescript_ocaml.exe)
export RESCRIPT_BSC_EXE="$PWD/_build/default/compiler/bsc/rescript_compiler_main.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
REWATCH_COMPILER_DOMAINS=8 rewatch-ocaml/bench/performance_gate.sh \
  /tmp/rewatch-before-954/_build/default/rewatch-ocaml/rescript_ocaml.exe \
  _build/default/rewatch-ocaml/rescript_ocaml.exe 5
REWATCH_WATCH_COMPILER_DOMAINS=8 \
  rewatch-ocaml/bench/watch_performance_gate.sh \
  /tmp/rewatch-before-954/_build/default/rewatch-ocaml/rescript_ocaml.exe \
  _build/default/rewatch-ocaml/rescript_ocaml.exe 7
```

## Native Linux testrepo checkpoint

At revision `7688129cd0bd6d8b66fec797327db3455398a4a1`, five
interleaved runs on a 12-CPU Linux ARM64 host with OCaml 5.5.1 measured the
472-module testrepo fixture. Standalone `bsc` and the embedded compiler were
built with the same Dune `release` profile; Rust Rewatch was a Cargo release
build. Both implementations used the same local runtime. The fixture includes
installed, lockfile-pinned dependencies; the benchmark copies them into
isolated roots and applies the canonical test-suite Belt-dependency correction
to those copies. No company-project source was used. Earlier measurements at
`49951ac49f78be57f8f7ae347bf577a2839f90ca` did not pin both compiler
executables to the same Dune profile and are superseded by this checkpoint.

| scenario | Rust median wall | OCaml median wall | Rust median peak tree RSS | OCaml median peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean, 8 OCaml workers | 3,585 ms | 1,413 ms | 252,464 KiB | 368,032 KiB |
| Unchanged after clean, 8 workers | 142 ms | 71 ms | 30,056 KiB | 26,072 KiB |
| One source edit, 8 workers | 140 ms | 72 ms | 28,648 KiB | 26,156 KiB |
| Clean, 7 OCaml workers | 3,712 ms | 1,527 ms | 260,280 KiB | 334,112 KiB |
| Clean, 6 OCaml workers | 3,617 ms | 1,574 ms | 275,592 KiB | 289,640 KiB |

Eight workers gave a 2.54x clean-build wall-time gain and a 1.46x sampled
peak-tree-RSS ratio relative to Rust. Seven workers gave a 2.43x gain but
still failed the existing 125% clean-memory gate in its comparison. Six
workers passed the complete gate, at a 2.30x gain and a 1.05x sampled RSS
ratio. These are separate runs, so compare ratios within a row. Peak tree RSS
is sampled every 20 ms and can miss short-lived compiler-child peaks; Rust's
sampled clean peak varied substantially across runs. The memory gate gives a
directional constraint, especially near its threshold, rather than a precise
cross-architecture memory ratio.

In one clean build, GNU `/usr/bin/time -v` reported 49,644 KiB maximum RSS
for Rust and 385,944 KiB for OCaml. Rust launches many compiler children,
whereas OCaml compiles mostly in-process. GNU time's maximum RSS does not sum
the concurrent process tree, so those two values are not a total-build memory
comparison. Keep the sampled tree figure alongside any per-executable GNU
time reading.

All three worker counts matched Rust's clean, unchanged, and edit compiler
work. The clean build made 1,031 logical compiler requests in each
implementation: 512 parse, seven namespace, and 512 compile requests. The
unchanged build made four requests because the warning in the testrepo's
`ModuleA` deliberately invalidates its AST for diagnostic replay. Every
comparison matched the complete post-build file set and the selected stable
artifact bytes.

In a separate seven-edit retained-watch run at the default eight workers,
Rust's median edit-to-hook latency was 142 ms and OCaml's was 83 ms. Both made
seven parser and seven compiler requests, produced identical edited JavaScript,
and held stable file descriptor, task, and RSS counts. Watch-mode samples use
a small one-module fixture, so they do not establish scaling on a large
dependency graph.

The later per-request timing trace measured an OCaml compile span of 1.17 s.
A 5x clean gain over the aligned Rust median would require the entire build
to finish in about 0.72 s. The compiler work alone exceeds that budget on
this fixture; faster parsing or orchestration alone cannot reach it.

The aligned filesystem audit counted 6,604 Rust versus 9,042 OCaml project-local
metadata calls on clean builds, with nearly equal open counts (17,636 and
17,633). The high-count missing CMI lookups, including 313 opens of the
fixture's `Pervasives.cmi` path, were identical in both implementations. The
extra metadata checks merit investigation on slower filesystems, but the
shared CMI lookup pattern does not identify an OCaml-specific optimization.
Exploratory lower GC space-overhead settings reduced OCaml RSS in individual
runs, but did not establish a validated advantage over six default-GC
workers. The default GC setting is unchanged.

An opt-in per-request timing trace resolves the OCaml compile phase further.
On one eight-worker clean build of the same fixture, 512 parse requests
spanned 95 ms and 512 implementation/interface requests spanned 1,172 ms.
The compiler workers were active for virtually the entire compile span, with
7.39 of eight workers active on average and all eight active at peak. The compile
requests summed to 8,666 ms of worker time; the 95th percentile request took
48 ms. `DOMAPI.ast`, `Net.ast`, and `Http.ast` were among the slowest requests.
This is a single diagnostic run with logging enabled, not a benchmark median.
It indicates that the clean-build limit is largely compiler work rather than
idle scheduler time on this fixture. At eight workers, the summed work alone
has a 1.08 s lower bound without faster individual requests.

To collect another trace, set `REWATCH_COMPILER_TIMING_LOG` to an absolute
path for an OCaml build and analyze the resulting tab-separated file:

```sh
export RESCRIPT_BSC_EXE="$PWD/_build/default/compiler/bsc/rescript_compiler_main.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
_build/default/rewatch-ocaml/rescript_ocaml.exe clean rewatch/testrepo
rm -f /tmp/rewatch-compiler-timing.tsv
REWATCH_COMPILER_TIMING_LOG=/tmp/rewatch-compiler-timing.tsv \
  _build/default/rewatch-ocaml/rescript_ocaml.exe build rewatch/testrepo
node rewatch-ocaml/bench/analyze_compiler_timing.js \
  /tmp/rewatch-compiler-timing.tsv
```

Each row records phase, working directory, input, start time, and end time.
Request time includes any PPX command that the compiler invokes.
The analyzer reports elapsed phase span, summed compiler time, average and
peak active requests, idle time inside each phase, and the longest compile
requests. Remove an old trace before a new run; the compiler appends rows.

A separate temporary compiler-core trace split 479 implementation requests
from one instrumented clean build. These are summed concurrent-worker times,
not elapsed build time:

| compiler-core phase | summed worker time |
| --- | ---: |
| Initial environment setup | 1,837 ms |
| Type checking, including CMI/CMT work | 6,736 ms |
| Lambda translation | 30 ms |
| Lambda compilation | 135 ms |
| JavaScript emission | 46 ms |

The remaining 33 interface requests and outer request setup are outside this
split. The instrumented build's 512 compile requests spanned 1,253 ms, so do
not compare that span directly with the uninstrumented benchmark median.
Environment setup and type checking account for nearly all measured
implementation work. Reusing a prepared environment across requests would
have to preserve the compiler's fresh per-request type and identifier state;
it is an architectural change with correctness and memory risks. Faster
JavaScript emission alone has little headroom on this fixture. The temporary
compiler-core instrumentation was removed after the measurement.

A second temporary trace split `Typemod.type_implementation_more` on the same
fixture. Among 479 implementation requests, `type_structure` used 5,253 ms
of summed worker time, inclusion and delayed checks 820 ms, CMI saving 267 ms,
and CMT saving 443 ms. Signature simplification and reset took under 3 ms
combined. This is a separate instrumented run, so its totals differ slightly
from the compiler-core split above. The type-structure work is the main part
of the remaining compiler cost; eliminating artifact writes alone has a
limited bound. The temporary type-checker instrumentation was removed and
the normal release binary rebuilt afterward.

## AST I/O checkpoint

Temporary counters on the same host and eight-domain fixture measured 917
sequential scheduler AST-header reads totaling 123 kB in 5.5–7.8 ms (5.9 ms
median) per clean build. Compiler domains read and deserialized 512 full ASTs
totaling 6.93 MB in 71–120 ms summed across domains (75 ms median). Two
additional builds spent 97–98 ms summed across domains on the AST write path,
including dependency extraction and serialization. Three interleaved clean
builds with counters enabled and three with counters disabled measured median
wall times of 2.19s and 2.35s; unrelated work makes that difference noise,
not an optimization result. The counters were removed afterward.

Avoiding scheduler header reads alone has little headroom on this fixture.
A parsed-tree handoff could avoid compiler reads, but summed concurrent-worker
time is not wall time saved. Preserve published AST/IAST files and freshness
behavior, then measure RSS, work, and artifact parity on a larger project.

## Rust comparison gates

Build the local runtime, lockfile-pinned testrepo dependencies, and both
release executables. Then run the Linux clean, unchanged, edit, work, resource,
and artifact comparison:

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
```

The harness isolates dependency trees, interleaves timed clean, unchanged,
and single-edit builds, and samples process-tree RSS and task counts. It then
traces Rust `bsc` requests and OCaml's logical compiler-request log, compares
work in all three scenarios, and compares complete file sets and stable
artifact bytes at the same absolute path. The timed source edits add unique
comments to `packages/watch-warnings/src/B.res` in both isolated fixtures.
The unchanged workload replays the fixture's local `ModuleA` warning, so four
compiler requests there are expected. `KEEP_REWATCH_BENCHMARK_WORKDIR=1`
retains raw outputs and `results.csv`. The 125% wall-time and memory limits
currently apply to the clean scenario; the other scenarios are measured and
checked for equivalent work and artifacts.

## Filesystem-work audit

Pipe-based subprocess capture has removed the intentional temporary capture
files. Run the second orchestration audit on Linux with:

```sh
rewatch-ocaml/bench/filesystem_audit.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
```

It traces isolated clean, unchanged, and single-edit builds with `strace`,
normalizes each fixture root, retains operations whose target is inside that
root, and reports per-path operation multisets plus metadata, open,
directory-scan, create, rename, remove, and execute categories. Set
`KEEP_REWATCH_FILESYSTEM_AUDIT=1` to retain normalized manifests and raw traces
for investigation.

Do not gate on the raw process-wide syscall total: Rust, OCaml, libc, the
dynamic loader, and subprocess startup legitimately perform different
toolchain-level accesses. Report those separately, and treat repeated accesses
to the same project artifact or discovery path as the primary evidence of
superfluous orchestration work. The existing compiler-work and artifact checks
must remain enabled so fewer filesystem calls cannot conceal skipped work.

The older process-attributed syscall measurements used the removed external
compiler mode. Repeat the audit with domains before using it to guide I/O
changes.

For the ordinary-edit path inside one long-lived watcher, run:

```sh
rewatch-ocaml/bench/watch_filesystem_audit.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
```

This starts each implementation on an isolated small project, waits for its
initial build hook, traces one dependency-preserving source edit, and stops the
watcher through its lock file. Only calls timestamped between the edit and the
successful incremental-build hook enter the normalized reports, so initial
discovery and shutdown do not obscure retained-state work. Set
`KEEP_REWATCH_WATCH_AUDIT=1` to retain raw traces, normalized path/category
tables, process attribution, and command output. As with the short-lived audit,
project-local repeated paths and compiler work are the useful comparison; raw
runtime-wide syscall totals are diagnostic rather than an acceptance limit.

On the small `basic` fixture, one retained-watch edit produced 19 Rust versus
26 OCaml project-local metadata calls and 43 versus 42 opens. The most
repeated source and compiler-artifact opens were similar in both versions.
This one-edit trace does not indicate a large OCaml-specific filesystem cost
on the watch path; it says little about larger dependency graphs.

The retained-watch performance and resource gate exercises several ordinary
edits through the same long-lived watcher:

```sh
rewatch-ocaml/bench/watch_performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe \
  7
```

Set `REWATCH_WATCH_COMPILER_DOMAINS` to measure a specific worker count;
otherwise the compiler uses its CPU-based heuristic.

It warms both implementations, interleaves an odd number of timed edits,
requires byte-identical generated JavaScript and equal logical parser/compiler
work counts, and samples file descriptors, tasks, and RSS after every build.
Rust work is observed through the counting `bsc` proxy; embedded OCaml work is
observed at the shared logical compiler-request boundary.
This catches retained-state implementations that appear fast by skipping work,
as well as resource growth that a one-event syscall trace cannot show. The
default median-latency limit is 150% of Rust because individual watch events
include operating-system notification and 50 ms polling intervals; override it
with `REWATCH_WATCH_PERFORMANCE_THRESHOLD_PERCENT` only for investigation.
The build gate's 125% clean-build wall-time limit remains the general latency
criterion; unchanged and single-edit builds are measured separately. Set
`KEEP_REWATCH_WATCH_PERFORMANCE=1` to retain output, compiler-call logs,
latencies, and fixtures. This gate requires Linux `/proc`, GNU-compatible
millisecond `date`, and `setsid`.

## Source-size snapshot

Run `bench/source_size.sh` with `cloc` installed to record a reproducible
maintainability snapshot. The production comparison excludes Rust's explicitly
out-of-scope telemetry module and reports its inline `#[cfg(test)]` sections as
tests rather than implementation. Both OCaml platform backends count because
both remain maintained production source. All tracked OCaml test harnesses,
fixtures, and configuration files are reported together but separately from
implementation; benchmark tooling includes every executable shell/JavaScript
file in `bench`, including this counting script itself. The report also lists
the ten largest production modules and test/tooling files so growth and mixed
responsibilities are visible without treating line count as a target. Record
the `cloc` version with the result and rerun this at the final maintainability
review.

Source lines are an observation, not an acceptance threshold. A smaller port
can indicate less machinery, but missing compatibility, weak tests, compressed
code, or too few explanatory comments can also reduce the number. Behavioral
and work equivalence, platform support, performance, module size, and review
findings remain the actual quality gates.

Rerun the snapshot after the domain cleanup before quoting source counts.
