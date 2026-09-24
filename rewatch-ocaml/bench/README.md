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

A third temporary trace timed CMI loading in `Bs_cmi_load` on one eight-worker
clean build. The 512 compile requests made 2,952 successful persistent-module
lookups, taking 2,711 ms summed across workers, including file selection and
failed candidate opens. They decoded 2,992 CMIs totaling 194 MB of repeated
input, which took 1,511 ms summed across workers; the extra 40 reads came
through other CMI call sites. These measurements include tracing overhead and
are not wall-time savings. Even eliminating all 2,711 ms of lookup work would
have an ideal eight-worker bound of about 0.34 s, short of closing the 5x gap.
A raw-byte cache would still pay most decoding cost, while reusing decoded
type graphs across fresh compiler requests would need safe copying and CMI
invalidation to preserve dependency correctness. The temporary trace code
was removed and both release compiler executables rebuilt afterward.

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
