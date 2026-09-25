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
That older watch gate timed the external-compiler side through its counting
proxy, which added process launches to its latency samples.

A filesystem-controlled follow-up at revision
`9fa158aee436b0804ae7f6d0bb5d72144e038053` put both release Rewatch
executables and the byte-identical release `bsc` on `/tmp` (`overlay`) instead
of launching `bsc` from the workspace's `virtiofs` mount. The older executable
was rebuilt from `95467b2bfab9d6edd8fd00a8897a5308794a211d`; both used
the same current `bsc` and runtime. Five interleaved runs after one warm-up
each measured:

| scenario | before median wall | after median wall | before peak tree RSS | after peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean | 1,717 ms | 1,374 ms | 365,952 KiB | 361,164 KiB |
| Unchanged | 145 ms | 45 ms | 29,480 KiB | 25,848 KiB |
| One source edit | 148 ms | 47 ms | 52,456 KiB | 25,220 KiB |

The clean gain was 1.25x under this compiler placement, compared with 2.85x
when `bsc` launched from the slower workspace mount. This supports compiler
launch location as a large part of the earlier measured gain on this host.
The unchanged and single-edit medians still fell by about 3x. Each timed edit
started from the same restored source and compiled baseline. The gate passed
its clean time and memory limits; both versions made the same 1,031 clean,
four unchanged, and six edit compiler requests, and produced identical
complete file sets and stable artifact bytes. The first executable is the
older OCaml Rewatch, despite the gate's `Rust` label.

The revised retained-watch gate times both executables with the real `bsc`
and replays the external side through a counting proxy only after timing. In
a separate seven-edit run with the same `/tmp` compiler, the older OCaml
watcher measured 143 ms median versus 83 ms for current OCaml. Both made seven
parse and seven compile requests and generated identical edited JavaScript;
file descriptors, task counts, and retained RSS stayed within the gate's
limits. Each edit changed the generated JavaScript, proving that the timed
watchers rebuilt the edited module. These medians replace the proxy-influenced
161 versus 83 ms comparison above for latency purposes.

After creating the older worktree and building both release executables as
shown below, reproduce this placement with:

```sh
mkdir -p /tmp/rewatch-before-after-fast
cp /tmp/rewatch-before-954/_build/default/rewatch-ocaml/rescript_ocaml.exe \
  /tmp/rewatch-before-after-fast/before
cp _build/default/rewatch-ocaml/rescript_ocaml.exe \
  /tmp/rewatch-before-after-fast/after
cp _build/default/compiler/bsc/rescript_compiler_main.exe \
  /tmp/rewatch-before-after-fast/bsc
export RESCRIPT_BSC_EXE=/tmp/rewatch-before-after-fast/bsc
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
REWATCH_COMPILER_DOMAINS=8 rewatch-ocaml/bench/performance_gate.sh \
  /tmp/rewatch-before-after-fast/before \
  /tmp/rewatch-before-after-fast/after 5
REWATCH_WATCH_COMPILER_DOMAINS=8 \
  rewatch-ocaml/bench/watch_performance_gate.sh \
  /tmp/rewatch-before-after-fast/before \
  /tmp/rewatch-before-after-fast/after 7
```

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

A temporary compiler-core trace split 479 implementation requests from one
instrumented clean build. These are summed concurrent-worker times, not elapsed
build time:

| compiler-core phase | summed worker time |
| --- | ---: |
| Initial environment setup | 1,837 ms |
| Implementation typing and persistence (split below) | 6,736 ms |
| Lambda translation | 30 ms |
| Lambda compilation | 135 ms |
| JavaScript emission | 46 ms |

The remaining 33 interface requests and outer request setup are outside this
pipeline split. The instrumented build's 512 compile requests spanned 1,253
ms, so do not compare that span directly with the uninstrumented benchmark
median.

A second instrumented clean build split the implementation typing and
persistence phase for the same 479 implementation requests:

| work inside `Typemod.type_implementation_more` | summed worker time |
| --- | ---: |
| Structure typing, including imported CMI and signature work (`type_structure`) | 5,253 ms |
| Interface inclusion and delayed checks | 820 ms |
| CMI saving | 267 ms |
| CMT saving | 443 ms |
| Signature simplification and reset | under 3 ms |

This second run totals about 6,783 ms, rather than the first run's 6,736 ms;
the rows are a breakdown of the same phase on a different run, not an exact
subtraction from 6,736 ms. `type_structure` includes import loading and
signature expansion, so its 5,253 ms is not a measurement of source typing
alone. Eliminating CMI/CMT saves alone has a limited bound.

A third instrumented clean build measured imported CMI work across **all 512
compile requests**, including the 33 interface requests excluded from the
tables above:

| imported CMI operation | summed worker time |
| --- | ---: |
| 2,952 successful persistent-module lookups, including path search and failed candidate opens | 2,711 ms |
| 2,992 CMI decodes, including 40 through other CMI call sites | 1,511 ms |

CMI decoding overlaps the lookup row, and CMI loading overlaps structure
typing and possibly other phases. These rows came from a separate run and
cover more requests, so 2,711 / 6,736 (about 40%) is only a numerical ratio,
not the measured lookup share of the typing phase. The remaining source typing
time was not isolated. A disjoint split requires nested timers in the same
run. The CMI trace decoded 194 MB of repeated input; its measurements include
tracing overhead and are not wall-time savings. Even eliminating all 2,711 ms
of lookup work would have an ideal eight-worker bound of about 0.34 s, short
of closing the 5x gap. A raw-byte cache would still pay most decoding cost,
while reusing decoded type graphs across fresh compiler requests would need
safe copying and CMI invalidation to preserve dependency correctness.

Environment setup and implementation typing account for nearly all measured
implementation work. Reusing a prepared environment across requests would
have to preserve the compiler's fresh per-request type and identifier state;
it is an architectural change with correctness and memory risks. Faster
JavaScript emission alone has little headroom on this fixture. The temporary
instrumentation was removed after each measurement.

An exploratory change deferred construction of `Env.initial_safe_string` for
`-bs-ast` requests while keeping it eager for type-checking requests. Fifteen
interleaved clean testrepo builds of matched release executables on `/tmp`
measured 1,374 ms before versus 1,372 ms after; unchanged and edit medians
were 46 ms in both versions. Sampled clean peak tree RSS was 364,392 versus
368,052 KiB. Compiler requests, complete file sets, and stable artifact bytes
matched. The extra lazy-state handling had no useful measured gain, so it was
reverted. This probe does not split the cost of opening the implicit modules
from the rest of initial-environment setup.

A further temporary single-worker trace separated module lookup from opening
the resolved signature. Across 1,897 opens, lookup used about 360 ms of
process CPU time and signature opening about 2,063 ms. The 137 opens of
`WebAPI.DOMAPI` alone used about 230 ms for lookup and 1,687 ms for signature
opening. Its CMI is roughly 987 kB, and expanding its large signature is
repeated in fresh compiler requests. A separate structure-item trace counted
429 source `open` items using about 1,784 ms of process CPU time, including
those `DOMAPI` opens; these two traces are separate runs and their times are
not additive. The one-worker timings are diagnostic, include tracing overhead
and some build-system CPU activity, and cannot be read as eight-worker wall
time savings. Even an ideal eight-way division of all `DOMAPI` opening work
would save only about 0.21 s. Reusing expanded components would need to keep
the mutable type graphs isolated and invalidate them when a CMI changes. The
temporary instrumentation was removed after these measurements.

## Exclusive type-checking and artifact breakdown

An opt-in trace now times nested compiler phases in one clean build. Its rows
are exclusive, so they add to the request total. This resolves the overlap in
the temporary measurements above. Set `REWATCH_TYPECHECK_TRACE` to an absolute
TSV path to enable it; otherwise the trace is disabled.

Five interleaved pairs used eight workers, OCaml 5.5.1, and Dune's development
profile on Linux ARM64. Both executables were copied to the same `/tmp`
directory. The plain executable was built from `80eb95963` without tracing
hooks; the traced executable differed only by the diagnostic instrumentation.
Each sample cleaned Belt and testrepo in an isolated fixture. All traced
samples made 512 parse, 40 interface, 472 implementation, and seven namespace
implementation requests (1,031 total).
An additional clean baseline/traced comparison produced the same 10,029
paths and SHA-256 hashes for files ending in `.ast`, `.iast`, `.cmi`, `.cmj`,
`.cmt`, `.cmti`, `.mjs`, `.cjs`, `.js`, or `.map`.

`traced-3.trace.tsv` gives the following **summed worker times** in
milliseconds. The 519 compile requests sum to 8,106 ms. Parse requests add
678 ms, including 452 ms of outer setup. Elapsed build time was 1.37 s;
worker sums are not elapsed time or directly achievable wall-time savings.

| Exclusive phase | Interfaces (40) | Implementations (472) | Namespace (7) | Compile total |
| --- | ---: | ---: | ---: | ---: |
| Request setup and initial environment | 18.1 | 553.2 | 1.5 | 572.9 |
| Obtain dependency interfaces | 70.8 | 2,511.7 | 2.2 | 2,584.7 |
| Check source and open signatures | 23.9 | 3,885.6 | 1.9 | 3,911.4 |
| Prepare CMI/CMT data | 2.4 | 60.8 | 0.3 | 63.5 |
| Serialize, hash, and write artifacts | 30.4 | 647.5 | 1.0 | 678.8 |
| AST reading, backend, and other work | 5.5 | 287.9 | 1.3 | 294.7 |
| **Total** | **151.1** | **7,946.7** | **8.2** | **8,106.0** |

Source checking comprises 2,152 ms typing, inclusion, delayed checks, and
typed-tree construction, plus 1,759 ms opening signatures and making names
available. Nested CMI file work is charged only to dependencies. Setup
includes fresh request state, include paths, and 332 ms opening implicit and
configured modules, excluding CMI work. The outer-request timer also covers
argument parsing, output capture, and teardown.

The dependency row comprises 1,155 ms finding and opening CMI paths, 2 ms
finding and opening the current module's explicit interface, 1,406 ms in
buffered reading and Marshal decoding, 19 ms in CRC consistency checks,
and 3 ms registering decoded persistent structures. The trace counted 2,966
successful loader searches and 3,006 decodes; the extra 40 are explicit
interface reads. `Pervasives` and `Stdlib` were each loaded 519 times, once per
compile request, and `WebAPI` 312 times. Fresh request state repeats this work.
The current `input_value` reader interleaves I/O and decoding, so their costs
cannot be separated without changing that reader. Lazy expansion during
source `open` appears in the source row, separate from CMI file loading.

CMI preparation copies the exported signature into saved form and registers
it for later checking; it is needed to make dependency types available. CMT
and CMTI preparation clears typed-tree environments and packages metadata for
editor tooling. The 679 ms persistence row includes 106 ms standalone CMI
serialization, 53 ms CMI hashing, 85 ms remaining standalone CMI file work,
212 ms CMT serialization, 19 ms source hashing, and 203 ms remaining CMT/CMTI
file work. The last component includes the CMI prefix embedded in CMT files.
These subtimers are exclusive and do not double-count one another.

The compile requests allocated 4,218 MB in OCaml heaps: 2,848 MB during
source checking, 671 MB obtaining dependencies, 458 MB in setup, 75 MB in
artifact preparation, 22 MB in persistence, and 144 MB elsewhere. Parse
requests allocated another 234 MB. The largest sampled `Gc.quick_stat`
top heap was 34.5 million words (about 263 MiB). Summing request-boundary
GC counter deltas gave 3,437 minor and 355 major collections, with no
compactions; overlapping requests may observe the same global collection,
so these sums are diagnostic rather than exact build-wide counts. Allocation
counters cover OCaml allocations on worker domains, not native allocations or
retained memory.

Across the five pairs, median elapsed build time was 1.37 s for both plain
and traced; median process user-plus-system time was 6.59 s for both. Median
GNU `time` peak RSS was 368,196 KiB plain and 352,324 KiB traced, with broad
per-run overlap (338,644–386,256 KiB across both modes). The trace showed no
resolvable wall-time, CPU, or memory penalty here. GNU `time` records the
build process's maximum RSS, not the sum
of concurrently live process trees. Each nested timer reads a clock and an
allocation counter, so tiny phase timings remain directional.

Reproduce the comparison with the instrumented branch checked out:

```sh
opam exec -- dune build rewatch-ocaml/rescript_ocaml.exe compiler/bsc/rescript_compiler_main.exe
git -c "safe.directory=$PWD" worktree add --detach /tmp/rescript-typecheck-base 80eb95963
(cd /tmp/rescript-typecheck-base && opam exec -- dune build \
  rewatch-ocaml/rescript_ocaml.exe compiler/bsc/rescript_compiler_main.exe)
REWATCH_PLAIN_EXECUTABLE=/tmp/rescript-typecheck-base/_build/default/rewatch-ocaml/rescript_ocaml.exe \
REWATCH_PLAIN_BSC=/tmp/rescript-typecheck-base/_build/default/compiler/bsc/rescript_compiler_main.exe \
  bash rewatch-ocaml/bench/typecheck_breakdown.sh /tmp/rescript-typecheck-data 5
node rewatch-ocaml/bench/analyze_typecheck_trace.js \
  /tmp/rescript-typecheck-data/traced-1.trace.tsv
```

The runner records host details, binary hashes, GNU `time` results, build
output, and raw traces. Omit `REWATCH_PLAIN_*` to compare trace enabled and
disabled in the same binary. The analyzer checks that exclusive phases account
for every request. Keep worker count and fixture filesystem fixed when
comparing results.

A separate 64 MiB raw CMI byte-cache prototype kept file bytes across
requests, checked file metadata before reuse, and decoded a fresh signature
on every read. Three interleaved eight-worker clean-build pairs gave median
elapsed times of 1.46 s without the cache and 1.42 s with it; median peak
RSS was 363,916 and 355,588 KiB, respectively, within the run-to-run
spread. It left path lookup, Marshal decoding, and signature opening in place,
so this gain was too small to justify another cache and invalidation path.
The prototype was removed. The combined experiment below tested CMI loading
and expansion together while preserving fresh mutable type graphs.

### Expanded WebAPI component cache experiment

A later release-profile experiment tried caching the expanded
`WebAPI.DOMAPI` alias and its forced `DOMAPI-WebAPI` signature. The cache
serialized one expanded graph, then deserialized and relocated its generated
type and identifier IDs for each fresh compiler request. It did not share
mutable type nodes between requests. The snapshot was 1.74 MB. Tracing
`components_of_module_maker` with the
`dependency.expand_components:<path>:alias=<target>` phase found 137
`WebAPI.DOMAPI` expansions in the
isolated testrepo clean build, taking 1.724 s and allocating 1.034 GB of
summed worker work. Preparing the snapshot took about 29 ms once; 136 copies
took 0.780 s and allocated 785 MB. These phase totals include tracing and
do not predict elapsed build savings by themselves.

Three interleaved eight-worker clean builds with the same fixture and
release-profile executable gave median elapsed times of 1.57 s without the
cache and 1.52 s with it. Median peak process RSS rose from 344,176 to
466,916 KiB. Two single-worker pairs gave 4.30 and 4.27 s without the cache,
versus 3.73 and 3.75 s with it; peak RSS rose from about 91 to 118 MiB.
In one retained watcher edit, the cached compiler request took 8.78 ms versus
11.32 ms without the cache. The build-level gain with eight workers was too
small for the memory cost and the extra type-graph relocation machinery, so
the prototype was removed.

A follow-up replaced the graph-wide ID search with allocation capture while
expanding the alias. It prepared the snapshot in about 17 ms, but the 136
copies still took 885 ms of summed worker time. Three eight-worker pairs had
the same 1.46 s median elapsed time with and without this cache; median peak
RSS was 489,628 KiB with it and 347,792 KiB without it. Lowering the OCaml
major-heap space overhead to 10% reduced some peaks but did not produce a
consistent elapsed-time gain. Its 14,739 selected artifacts matched the
same-binary uncached build byte for byte. This simpler implementation was
also removed.

The experiment also checked correctness. An isolated compiler-driver fixture
confirmed that fresh requests received distinct mutable type nodes and that
rebuilding `DOMAPI-WebAPI.cmi` from an `int` signature to a `string` signature
invalidated the cache. All 14,739 selected generated artifacts in the full
clean build had identical SHA-256 hashes with and without caching. The target
CMI was already loaded before every alias expansion in this fixture, so this
cache did not avoid its per-request CMI decode.

### Combined CMI and expansion snapshot experiment

A further prototype stored the raw target CMI signature, its expanded
signature, and the target and `WebAPI.DOMAPI` component tables in one 2.58 MB
snapshot. Each request deserialized the snapshot into a fresh graph and
relocated generated IDs when each lazy stage was first forced. The target and
namespace CMI paths and file identity, size, modification time, and change
time guarded reuse. A separate prototype hashed both files on every hit, but
that validation alone cost 0.71 s of summed worker time across 134 hits; file
metadata checks took about 0.01 s. A focused compiler-driver test passed
request graph isolation and target-CMI invalidation, and all 14,739 selected
artifacts matched the same-binary uncached build byte for byte.

The snapshot clone itself took about 1.45 s and allocated 1.30 GB of summed
worker work across 134 hits. One traced eight-worker clean build took 1.48 s
and peaked at 578,340 KiB RSS, versus roughly 1.46 s and 350,000 KiB in the
uncached runs. Two interleaved single-worker pairs took 4.45 and 4.58 s
uncached versus 3.64 and 3.68 s cached; peak RSS rose from about 92 to 145
MiB. A retained watcher edit took 10.94 ms of compiler request time cached
versus 11.29 ms uncached. Splitting the target components into a separate
lazy clone raised eight-worker peak RSS to 765,876 KiB and elapsed time to
1.78 s, though its artifacts still matched. These are small samples and the
watcher comparison is one edit per mode.

The combined cache made the single-worker clean build about 19% faster, but
it did not improve the default eight-worker build and substantially increased
its peak memory. It was removed. Further work needs to reduce the allocation
cost of fresh mutable graphs or shorten the build's critical path, rather than
only eliminating summed worker work.

A later two-pair sweep of the same opt-in prototype across worker counts showed
where the gain disappears. Each pair cleaned the same fixture and interleaved
uncached and cached builds with the same release executable. Elapsed times and
peak RSS (KiB) were:

| workers | uncached elapsed | cached elapsed | uncached RSS | cached RSS |
| ---: | ---: | ---: | ---: | ---: |
| 2 | 3.90 / 3.74 s | 2.83 / 2.86 s | 134,436 / 135,312 | 214,708 / 240,496 |
| 4 | 2.16 / 2.15 s | 1.98 / 1.92 s | 203,464 / 205,520 | 328,904 / 346,148 |
| 6 | 1.65 / 1.66 s | 1.58 / 1.57 s | 281,580 / 280,672 | 469,440 / 476,992 |
| 8 | 1.61 / 1.44 s | 1.50 / 1.45 s | 344,468 / 356,152 | 633,676 / 600,740 |

The two-worker gain is substantial, but this implementation hard-codes one
WebAPI alias and is not suitable as a general compiler cache. The eight-worker
elapsed differences remain within the observed uncached spread.

### Per-domain in-memory graph reuse

The retained implementation keeps one expanded alias graph per compiler domain. A
domain never compiles two requests at once, so the graph is exclusive while a
request runs. The cache relocates generated type and identifier IDs when
the graph enters each request, then checks that mutable graph state is
restored before the next request. A full serialization check on one eight-worker
clean build found no retained mutation across 129 reuses. A cheaper typed
check covered type nodes, captured identifiers, abbreviation and object-field
references, row-field references, variant layouts, label arrays, and component
tables; an audit mode compared it with the full serialization check on every
reuse without a disagreement in that fixture. Cache entries still checked the
target and namespace CMI paths and file metadata before use.

Three interleaved eight-worker pairs with the typed check took 1.48, 1.49,
and 1.51 s uncached versus 1.31, 1.27, and 1.25 s cached. Peak RSS ranged from
347–357 MiB uncached and 520–541 MiB cached. All 14,739 selected artifacts
matched byte for byte in a same-binary cached/uncached comparison. An
upper-bound trial without the request-boundary check took 1.19–1.26 s cached
versus 1.45–1.52 s uncached. That unchecked mode was removed. The checked
cache is enabled by default for Rewatch compiler workers. It reuses mutable
nodes sequentially on one domain after verifying that the previous request
left the graph clean. A dirty graph is restored from the saved snapshot.

A first same-binary release-profile comparison used three interleaved
eight-worker clean testrepo pairs. Setting
`REWATCH_COMBINED_SIGNATURE_CACHE=0` disabled the cache for the baseline.
Elapsed times were 1.64, 1.54, and 1.51 s without the cache versus 1.35,
1.27, and 1.30 s with it: medians of 1.54 and 1.30 s. A cold two-module
incremental build was slower with eager snapshot preparation, however:
0.16 s cached versus 0.09 s uncached. It compiled one WebAPI source that
opened the large DOMAPI signature only once.

The retained cache waits until two distinct compiler requests have expanded
the same large alias before preparing a snapshot. A small process-wide table
tracks that first encounter; expanded graphs remain private to each domain.
Six further interleaved eight-worker clean pairs took 1.48, 1.43, 1.43,
1.46, 1.43, and 1.46 s uncached versus 1.23, 1.27, 1.21, 1.28, 1.27, and
1.26 s cached. Median elapsed time fell from 1.45 to 1.27 s, about 12%.
Median peak RSS rose from 349 to 523 MiB. Both selected-artifact comparisons
matched all 14,741 files byte for byte. Three cold incremental pairs after
this change took 0.08–0.09 s uncached and 0.09 s cached, rebuilt the same
two modules, and produced identical selected artifacts. The compiler test
suite, Rewatch integration suite, and a focused test for sequential reuse,
cross-domain separation, dirty-graph recovery, and CMI invalidation passed.
A final audit build checked 128 cached request boundaries against full graph
serialization without a disagreement.

### Reusing decoded runtime interfaces

An eight-worker release-profile trace with the expanded WebAPI cache enabled
showed that the 519 compile requests each decoded `Stdlib.cmi` and
`Pervasives.cmi`. Those two files cost 477 ms of summed worker time in CMI
reading and decoding, plus 695 ms searching and opening their paths. The
remaining implementation requests spent about 1,071 ms searching and opening
all CMIs and 849 ms reading and decoding them, including these two runtime
interfaces. Source checking took 1,183 ms; expanded `WebAPI.DOMAPI` work still
accounted for 434 ms. These are exclusive traced worker totals from one build,
not expected elapsed savings. The temporary per-file decode labels used for
this diagnosis were removed afterward.

The retained compiler cache keeps the decoded `Stdlib` and `Pervasives` CMI
graphs private to each worker domain. Each hit resolves the current load path
and checks the file's identity, size, modification time, and change time. At
the end of a request it compares the graph with a saved serialized image and
restores the image if the request changed it. The nested fresh request used to
prepare an expanded WebAPI snapshot bypasses this cache, so the two caches do
not share mutable input graphs. The cache is active only with the existing
Rewatch signature cache; `REWATCH_COMBINED_SIGNATURE_CACHE=0` disables both.

A follow-up traced build decoded each runtime CMI nine times rather than 519
times. Path re-resolution across 1,020 hits took 31 ms of summed worker time, and
1,038 request-boundary graph checks took 64 ms. The one-run trace includes
instrumentation overhead. A focused test changes and shadows `Stdlib.cmi`
between requests, mutates a loaded type, and loads it from another domain.
Another traced clean build found no changed cached runtime CMI graph among
1,038 request-boundary checks. This observation covers these two interfaces
in this fixture; it is not a proof that arbitrary imported type graphs can be
shared concurrently between compiler domains.

The same testrepo fixture, standalone `bsc`, runtime, and release-profile
toolchain were used for each pair of executables, placed in one directory.
After a warm-up, the gate interleaved the default eight-worker builds. Its
20 ms process-tree sampler measured memory; wall time stopped when each build
process exited. The first seven pairs had clean medians of 1,184 ms before
versus 1,118 ms after. Eleven further pairs, after the nested-request guard
and final code cleanup, had clean medians of 1,203 versus 1,154 ms. Ten of
those eleven paired runs favored the change. Median sampled peak tree RSS was
557,196 KiB before and 566,480 KiB after in the final run, within the
run-to-run spread. Unchanged medians were 44 ms in both versions; single-edit
medians were 45 and 44 ms. Both builds made the same 1,031 clean, four
unchanged, and six edit compiler requests. Complete post-build file sets and
stable generated artifact bytes matched exactly.

A broader prototype cached frequently loaded CMIs up to 64 KiB, with 32 entries
per domain. Seven pairs found a 26 ms clean median gain beyond the narrow
cache; fifteen further pairs found 19 ms, with overlapping samples and a 1 ms
slower single-edit median. That extra gain was within benchmark variation, so
the broader prototype was removed. The retained change avoids roughly 4–6%
of clean-build wall time on this host and fixture; it does not establish the
same gain for other projects. `make test`, `make test-rewatch`, the OCaml
Rewatch integration script, and the focused Rewatch OUnit suite passed.

A later project-session implementation retains tables of up to 32 decoded CMIs
of at most 64 KiB each and one expanded signature graph. A module request
borrows one table exclusively and returns it after graph verification, so a
later worker domain can reuse it across build phases and watch edits while
creating fresh inference state. On
this host, nine interleaved clean-build pairs of a synthetic 1,201-module
project measured 839.4 ms for the runtime-only cache and 843.4 ms for the
project cache after the table-lease change; that difference is within run
variation. A seven-edit retained-watch gate on a small fixture measured 77 ms
for each mode, with matching compiler work and output. The available testrepo
dependencies came from a Linux container, so the larger testrepo gate was not
run on macOS. The performance benefit of cross-edit decoded interface and
expanded-signature reuse on larger projects remains unmeasured.

The request driver now captures ordinary text output in memory, opening a
temporary file only if a job requests an output channel for binary ASTs or
channel-based printing. The same synthetic clean build's summed parse-request
setup time fell from 1,211 ms to 11 ms across 1,202 requests; a traced build
fell from about 0.74 s to 0.62 s. Typed integrity checks on decoded CMIs
replaced repeated full serialization, reducing summed CMI verification from
about 170 ms to 8 ms across 1,201 implementation jobs. An audit mode that also
serialized the CMIs found no missed mutation on this fixture. Nine untraced
interleaved clean-build pairs with both changes gave medians of 696.2 ms for
the runtime-only CMI cache and 695.5 ms for the broader project cache, still
within run variation. These fixtures establish the request-overhead reduction
but no separate wall-time win from caching project CMIs.

One more temporary trace split the remaining WebAPI cache-hit work. Across 128
hits, forcing the cached target signature took under 1 ms, alias-ID relocation
took 17 ms, and target-signature-ID relocation took 11 ms in summed worker
time. The larger `WebAPI.DOMAPI` phase also contains the uncached expansions
and snapshot preparation on each domain. Optimizing hit relocation alone has
little elapsed-time headroom. The temporary subtimers were removed.

To repeat the gate, build the parent revision and this revision with Dune's
`release` profile, copy both embedded executables into one directory, and set
`REWATCH_FIRST_EMBEDDED=1` when invoking `performance_gate.sh`. Use the same
`RESCRIPT_BSC_EXE` and `RESCRIPT_RUNTIME` for both and retain the default eight
compiler domains. The gate archives the same committed testrepo fixture for
each executable and compares work counts and all generated artifact bytes.

### Sharing a prepared signature image across workers

The expanded WebAPI snapshot previously had to be prepared separately by each
compiler domain. The compiler now publishes one immutable marshaled image after
the first preparation. Other domains decode private graphs from that image;
fresh inference variables and request-local type and identifier IDs remain
private to each compile job. A mutex serializes the first preparation, while
the existing per-domain graph verifier continues to check for mutations after
each job. The shared image is keyed by the alias, both CMI paths and file
metadata, and the resolved load path. The per-domain cache also checks the
resolved load path, so a job with different import resolution prepares its own
graph. `REWATCH_COMBINED_SIGNATURE_CACHE=0` disables this reuse.

One traced clean build restored the shared image on seven domains and captured
eight private graph integrity snapshots. It made the same 128 expanded-snapshot
cache hits as the previous implementation. Decoding the seven private graphs
took 72 ms of summed worker time in that trace. The trace is diagnostic and
includes instrumentation overhead.

Eleven interleaved release-profile pairs compared the decoded-runtime-CMI
version with and without cross-domain image sharing. Clean median wall time
fell from 1,145 to 1,051 ms, and all eleven pairs favored sharing. Median
sampled peak tree RSS fell from 543,976 to 472,236 KiB. Unchanged and
single-edit medians were 44 versus 45 ms and 44 versus 45 ms, respectively.

A separate eleven-pair gate compared the complete change directly with the
original per-worker WebAPI cache at revision `d1c43a0c0`:

| scenario | original median wall | new median wall | original peak tree RSS | new peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean, eight workers | 1,199 ms | 1,014 ms | 545,312 KiB | 457,660 KiB |
| Unchanged | 45 ms | 44 ms | 26,744 KiB | 26,888 KiB |
| One source edit | 44 ms | 45 ms | 26,796 KiB | 27,076 KiB |

The clean median improved by 15% and all eleven paired runs favored the
change. An earlier eleven-pair comparison of the same source change measured
1,190 versus 1,032 ms; ten pairs favored the change and one new-build run was
an outlier at 1,485 ms. Both executables in the final gate used
the same release-profile standalone `bsc`, runtime, fixture, and eight-worker
setting. They made identical 1,031 clean, four unchanged, and six edit compiler
requests. Complete post-build file sets and generated artifact bytes matched.
The 20 ms process-tree memory sampler is directional. The load-path guard was
included in this final gate.
An additional clean build with full graph-integrity auditing checked all 128
reused snapshots, restored the shared image on seven domains, and reported no
dirty snapshots.
`make test`, `make test-rewatch`, the OCaml Rewatch integration script, the
focused Rewatch OUnit suite, and `make checkformat` passed.

## Bulk label table checkpoint

Revision `56164c19e3b0cc751301e4344cc0e4ecff46df20` builds the opened
signature's record-label table once per distinct label name. The previous
revision was `8d1fa55ed0f88bfdebef17bbde797109dfa1e52f`. A temporary
single-worker trace of the testrepo's `WebAPI.DOMAPI` signature found 6,133
label entries under 991 names in each of 137 expansions. Insertion into the
persistent table took about 461 ms summed across those expansions. The new
builder retains the first-seen insertion order, latest key, and per-name
declaration order; the temporary trace code was removed.

On the same 12-CPU Linux ARM64 host, five interleaved eight-worker testrepo
runs used release-profile executables placed in the same directory, the same
standalone `bsc` and runtime, and one warm-up per executable. The benchmark
gate, updated at `719e4bd231bf59b21029dfff7584267550fb1799`, times process
completion separately from its 20 ms process-tree resource sampler:

| scenario | before median wall | after median wall | before peak tree RSS | after peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean | 1,424 ms | 1,414 ms | 373,612 KiB | 365,252 KiB |
| Unchanged | 45 ms | 44 ms | 26,892 KiB | 26,540 KiB |
| One source edit | 45 ms | 46 ms | 26,980 KiB | 26,628 KiB |

The five-run clean difference is small beside run-to-run variation. A separate
ten-pair, high-resolution interleaved clean comparison without the resource
sampler measured 1,457 ms before and 1,430 ms after (1.9% faster). Twenty
warmed unchanged builds on isolated fixtures measured 45.10 and 45.01 ms;
there was no measurable incremental gain. These direct timings used
`process.hrtime.bigint()` around each child build, after cleaning before each
clean sample. The resource figures above are sampled peaks, not exact maximum
RSS, and do not establish a memory reduction.

One exploratory five-pair single-worker comparison had isolated long clean
builds in both versions (15 s before and 52 s after). Five later traced clean
builds per version did not reproduce those outliers and made the expected 512
compile requests each. The cause is unknown, so these single-worker samples
are not evidence for or against a stable tail-latency change.

Both versions made the same 1,031 clean, four unchanged, and six edit compiler
requests. The complete post-build file sets and stable artifact bytes matched.
The clean time and memory gate passed. A seven-edit retained-watch comparison
measured 84 ms before and 82 ms after, with seven parse and seven compile
requests each, identical edited JavaScript, and stable watcher resources.
The small watch fixture cannot establish a latency gain. `make test`,
`make test-rewatch`, the OCaml Rewatch integration script, and the Rewatch
unit tests passed with the new compiler. No company-project performance is
inferred from these repository measurements.

To reproduce the before/after gates after installing the dependencies shown
below, build both revisions with the Dune `release` profile and put their
executables in the same directory. For two embedded compiler executables,
`REWATCH_FIRST_EMBEDDED=1` makes the first argument use the logical request
trace; the harness still labels that first executable `Rust` in its output:

```sh
git worktree add --detach /tmp/rewatch-before-bulk 8d1fa55ed
(cd /tmp/rewatch-before-bulk && opam exec -- dune build --profile release \
  compiler/bsc/rescript_compiler_main.exe rewatch-ocaml/rescript_ocaml.exe)
opam exec -- dune build --profile release \
  compiler/bsc/rescript_compiler_main.exe rewatch-ocaml/rescript_ocaml.exe
mkdir -p /tmp/rewatch-bulk-binaries
cp /tmp/rewatch-before-bulk/_build/default/rewatch-ocaml/rescript_ocaml.exe \
  /tmp/rewatch-bulk-binaries/before
cp _build/default/rewatch-ocaml/rescript_ocaml.exe \
  /tmp/rewatch-bulk-binaries/after
export RESCRIPT_BSC_EXE=/tmp/rewatch-before-bulk/_build/default/compiler/bsc/rescript_compiler_main.exe
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
REWATCH_FIRST_EMBEDDED=1 REWATCH_COMPILER_DOMAINS=8 \
  rewatch-ocaml/bench/performance_gate.sh \
  /tmp/rewatch-bulk-binaries/before /tmp/rewatch-bulk-binaries/after 5
REWATCH_FIRST_EMBEDDED=1 REWATCH_WATCH_COMPILER_DOMAINS=8 \
  rewatch-ocaml/bench/watch_performance_gate.sh \
  /tmp/rewatch-bulk-binaries/before /tmp/rewatch-bulk-binaries/after 7
```

## Current Rust comparison and compiler placement

At revision `f8c60996fbb50a1f3678723f4910549e9fe3994d`, five interleaved
clean, unchanged, and edit runs used the Cargo release Rust executable, the
Dune release standalone `bsc` from `_build/default`, the Dune release OCaml
executable, and the same local runtime. This used the gate's independent wall
timer and 20 ms process-tree RSS sampler:

| scenario | Rust median wall | OCaml median wall | Rust sampled peak tree RSS | OCaml sampled peak tree RSS |
| --- | ---: | ---: | ---: | ---: |
| Clean, 8 OCaml workers | 3,377 ms | 1,368 ms | 272,316 KiB | 361,520 KiB |
| Unchanged, 8 workers | 125 ms | 57 ms | 28,976 KiB | 26,320 KiB |
| One source edit, 8 workers | 126 ms | 56 ms | 38,784 KiB | 26,400 KiB |
| Clean, 6 OCaml workers | 3,626 ms | 1,572 ms | 263,240 KiB | 303,368 KiB |
| Unchanged, 6 workers | 131 ms | 57 ms | 29,880 KiB | 26,276 KiB |
| One source edit, 6 workers | 132 ms | 58 ms | 32,536 KiB | 26,268 KiB |

Eight workers gave a 2.47x clean wall-time gain but exceeded the gate's 125%
sampled-memory limit. Six workers gave a 2.31x gain and passed that limit.
Both counts matched the 1,031 clean, four unchanged, and six edit logical
compiler requests, complete post-build file sets, and stable artifact bytes.
These are separate runs; compare ratios within the same worker-count group.
The memory sampler can miss brief child-process peaks, so treat its ratios as
directional. This refreshes the earlier native Linux checkpoint above; no
company-project performance is implied.

The standalone compiler's filesystem location materially affects the Rust
baseline on this host. The workspace's Dune build directory is on `virtiofs`;
`/tmp` is on `overlay`. A copied release `bsc` had the same SHA-256 digest
(`cfefc4fe91cd78b7906fde1f546026d971f29775557654269d7a34415d15dfa9`)
as the Dune-path executable. Thirty interleaved `bsc -version` launches
measured about 5 ms median from `/tmp` versus 15 ms from the Dune path. This
is consistent with executable loading from the different mounts; it does not
show a compiler-code difference.

With that byte-identical compiler on `/tmp`, a separate five-run eight-worker
comparison measured 1,483 ms Rust versus 1,358 ms OCaml clean wall time, with
sampled peak tree RSS of 336,308 versus 353,276 KiB. Unchanged medians were
84 versus 59 ms and single-edit medians 83 versus 59 ms. Work counts, complete
file sets, and stable artifact bytes matched, and the clean time and memory
gate passed. The clean OCaml gain was only 1.09x under this placement, versus
2.47x with the compiler on the workspace mount. Compare each pair only within
its run. The gate now prints executable and compiler hashes, runtime path, and
worker settings, so a benchmark can be reproduced with its actual compiler
storage layout. Neither layout predicts the closed-source company project.

After the gate began restoring the source between timed edit samples, a
five-run repeat at `9fa158aee436b0804ae7f6d0bb5d72144e038053` with the
same `/tmp` compiler measured 1,482 ms Rust versus 1,398 ms OCaml clean wall
time, and 339,716 versus 359,656 KiB sampled peak tree RSS. Unchanged
medians were 80 versus 59 ms; edit medians were 77 versus 57 ms. Equal
compiler work, complete file sets, and stable artifact bytes passed the gate.
The clean gain in this repeat was 1.06x. The older fast-placement results
above used cumulative comment edits; compare medians only within each run.

With the fast-placement Rust median as the reference, a 5x clean-build gain
would require about 297 ms total. The separate instrumented OCaml compile
span was 1,172 ms on this fixture, before accounting for the rest of the
build. That trace has overhead and is not a same-run lower bound, but it shows
why scheduling and startup changes alone are unlikely to reach the target;
compiler work would need a several-fold reduction as well.

The earlier 142 versus 83 ms Rust/OCaml retained-watch comparison timed Rust
through the counting compiler proxy. With the revised gate and the same real
`bsc` on `/tmp` for both implementations, seven retained edits measured 92
ms Rust versus 81 ms OCaml median. Each made seven parse and seven compile
requests; edited JavaScript changed and matched, and file descriptor, task,
and RSS growth stayed within the gate's limits. This small watch fixture measures
single-module edits only.

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
artifact bytes at the same absolute path. Each timed source edit adds a
comment to `packages/watch-warnings/src/B.res` in an isolated fixture. The
harness then restores the original source and completes an untimed build, so
every edit sample starts from the same compiled baseline.

The incremental figures recorded above used the earlier cumulative-comment
procedure; the clean-build figures are unaffected by this harness change.
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

It warms both implementations, interleaves an odd number of timed edits with
the real `bsc` path, requires byte-identical generated JavaScript and equal
logical parser/compiler work counts, and samples file descriptors, tasks, and
RSS after every build. External compiler work is counted in a separate,
untimed replay through the `bsc` proxy; embedded OCaml work is observed at the
shared logical compiler-request boundary during the timed run. Every edit
changes `B.res`'s generated JavaScript; the gate verifies each timed result
changed and compares both implementations and the replay. The proxy therefore
adds no launch overhead to the measured edits.
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
