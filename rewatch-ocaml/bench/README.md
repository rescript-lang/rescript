# OCaml rewatch performance measurements

The OCaml rewatch now uses parallel in-process compiler domains by default.
`REWATCH_COMPILER_DOMAINS` overrides the CPU-based worker count; without it,
the worker count is `min(8, max(1, available CPUs - 1))`. Rust rewatch is the
build-level reference for compiler work, generated file sets, and stable
artifact bytes. Keep both implementations on the same compiler and runtime
build when comparing them.

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

## Native Linux testrepo checkpoint

At compiler revision `49951ac49f78be57f8f7ae347bf577a2839f90ca`, five
interleaved runs on a 12-CPU Linux ARM64 host with OCaml 5.5.1 measured the
472-module testrepo fixture. Both implementations used the same local compiler
and runtime. The fixture includes installed, lockfile-pinned dependencies;
the benchmark copies them into isolated roots and applies the canonical
test-suite Belt-dependency correction to those copies. No company-project
source was used.

| scenario | Rust median wall | OCaml median wall | Rust median peak tree RSS | OCaml median peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean, 8 OCaml workers | 3,625 ms | 1,511 ms | 264,644 KiB | 358,256 KiB |
| Unchanged after clean, 8 workers | 142 ms | 71 ms | 29,156 KiB | 25,764 KiB |
| One source edit, 8 workers | 163 ms | 72 ms | 45,120 KiB | 25,520 KiB |
| Clean, 6 OCaml workers | 3,724 ms | 1,673 ms | 252,272 KiB | 303,536 KiB |
| Clean, 4 OCaml workers | 3,642 ms | 2,020 ms | 250,644 KiB | 219,364 KiB |

Eight workers gave a 2.40x clean-build wall-time gain and a 1.35x peak-RSS
ratio relative to Rust in the complete clean/unchanged/edit run. The existing
125% clean-memory gate therefore failed at eight workers. Six workers passed
the complete gate, at a 2.23x clean-build gain and 1.20x peak-RSS ratio. Four
workers also passed, with a 1.80x clean-build gain. These are separate runs,
so compare ratios within a row rather than treating small cross-run differences
as an effect of worker count. Peak tree RSS is sampled every 20 ms and can
miss short-lived child peaks.

All three worker counts matched Rust's clean, unchanged, and edit compiler
work. The clean build made 1,031 logical compiler requests in each
implementation: 512 parse, seven namespace, and 512 compile requests. The
unchanged build made four requests because the warning in the testrepo's
`ModuleA` deliberately invalidates its AST for diagnostic replay. Every
comparison matched the complete post-build file set and the selected stable
artifact bytes.

In a separate seven-edit retained-watch run at the default eight workers,
Rust's median edit-to-hook latency was 128 ms and OCaml's was 82 ms. Both made
seven parser and seven compiler requests, produced identical edited JavaScript,
and held stable file descriptor, task, and RSS counts. Watch-mode samples use
a small one-module fixture, so they do not establish scaling on a large
dependency graph.

One interactive clean build reported Rust parse/compile times of 1.35/2.09 s
and OCaml times of 0.16/1.24 s. These single-run phase timings are diagnostic,
not medians. The remaining OCaml compile phase alone exceeds the roughly
0.73 s total required for a 5x gain over the measured Rust clean median.
Further work toward 5x therefore needs substantial compilation or scheduling
improvement; eliminating the already short parse phase cannot reach it alone.

The filesystem audit counted 6,604 Rust versus 9,056 OCaml project-local
metadata calls on clean builds, with nearly equal open counts (17,636 and
17,633). The high-count missing CMI lookups, including 313 opens of the
fixture's `Pervasives.cmi` path, were identical in both implementations. The
extra metadata checks merit investigation on slower filesystems, but the
shared CMI lookup pattern does not identify an OCaml-specific optimization.
An exploratory `OCAMLRUNPARAM=o=70` run reduced the eight-worker median peak
tree RSS to 330,232 KiB with a 1,505 ms clean median; it still missed the
125% memory gate in that interleaved comparison. The default GC setting is
unchanged pending broader workload evidence.

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
opam exec -- dune build --profile release rewatch-ocaml/rescript_ocaml.exe
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
The build gate's lower-noise 125% clean/incremental threshold remains the
authoritative general performance criterion. Set
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
