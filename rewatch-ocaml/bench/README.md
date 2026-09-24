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

Build both release executables, then run the Linux clean-build, work, resource,
and artifact comparison:

```sh
cargo build --manifest-path rewatch/Cargo.toml --release
opam exec -- dune build --profile release rewatch-ocaml/rescript_ocaml.exe
rewatch-ocaml/bench/performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe 5
```

The harness isolates dependency trees, interleaves builds, traces Rust `bsc`
requests and OCaml's logical compiler-request log, compares clean, unchanged,
and one-edit work, and compares complete file sets and stable artifact bytes at
the same absolute path. `KEEP_REWATCH_BENCHMARK_WORKDIR=1` retains raw outputs.
The fixture currently does work on an unchanged build. In a one-run smoke check,
both implementations performed four compiler requests there, and their clean
and single-edit request counts matched too. The source of those unexpected
unchanged requests is unresolved. The same check found byte differences in
some `rescript-bun` CMI files and an AST despite equal generated file sets;
investigate these before using the artifact gate as an acceptance result.

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
