# Performance and work-equivalence gate

`performance_gate.sh` compares release builds on two fully isolated copies of
the tracked `rewatch/testrepo` fixture. It deliberately archives both the
fixture and its external Belt/runtime targets and copies all installed,
Git-ignored dependency trees (including nohoisted dependencies) separately
into each root. Cleaning one
implementation therefore cannot warm or remove artifacts used by the other.

The gate performs one warm-up per implementation, at least five interleaved
clean builds, and reports median wall time plus peak summed process-tree RSS. It
then traces clean, unchanged, and single-edit builds with `strace` and requires
identical normalized package/phase/input multisets as well as identical counts
for parser, namespace, compiler, interface, and PPX process launches. The edit
targets the same leaf source in each isolated fixture. This sequence detects
superfluous incremental parsing or compilation that a clean-only comparison
cannot expose. Finally, both implementations clean and build a third fixture at
the same absolute path. The gate first requires the complete post-build
file-name sets to match, including auxiliary cache and editor-control files,
and then requires byte-identical generated JavaScript, compiler interfaces
(`.cmi`), JavaScript IR (`.cmj`), parser AST caches, namespace maps, copied
sources, source-directory metadata, and `build.ninja` markers, including those
below installed dependency trees. It does not treat typed debug metadata
(`.cmt`/`.cmti`), compiler logs, or `compiler-info.json` as byte-stable: those
contain compiler debug data, timestamps, or intentionally
implementation-specific state and are covered by file-set and integration
checks instead. The default
acceptance threshold requires both OCaml medians to be no more than 125% of
Rust.

This is one part of equivalence checking, not a substitute for the test suites.
Before accepting a performance increment, also run the OCaml unit/focused tests
and the canonical Rust rewatch integration suite against the OCaml executable:

```sh
opam exec -- dune runtest tests/rewatch_ounit_tests
bash rewatch-ocaml/tests/run.sh \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
(cd rewatch/tests && \
  bash ./suite.sh ../../_build/default/rewatch-ocaml/rescript_ocaml.exe)
```

Together these cover three different failure classes:

- the canonical and focused suites check observable command/build/watch
  behavior;
- the three `strace` classifications check that a speed result did not hide
  skipped or superfluous clean/incremental module or PPX work (argument
  semantics remain covered by the compiler-argument and integration tests);
- the fresh-tree comparisons check the complete post-build file set plus the
  byte contents of every stable generated artifact class.

The manifest comparison intentionally recreates its fixture between runners.
Using only each implementation's `clean` command would allow a Rust-only file
to survive into the OCaml run and could conceal a missing-output bug.

`filesystem_audit.sh` writes normalized `*.categories.tsv`, `*.paths.tsv`, and
`*.processes.tsv` files when `KEEP_REWATCH_FILESYSTEM_AUDIT=1` is set. The last
form attributes each path operation category to the executable recorded for
that traced process, separating driver work from compiler, PPX, and helper
work. Processes which inherit a trace file without a subsequent `execve` are
reported as `inherited-process`; do not assume those calls belong to the
driver without inspecting the raw trace.

Build both release executables and run:

```sh
cargo build --manifest-path rewatch/Cargo.toml --release
opam exec -- dune build --profile release rewatch-ocaml/rescript_ocaml.exe

rewatch-ocaml/bench/performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe \
  5
```

By default the harness uses the compiler and runtime selected by
`rewatch/tests/get_bin_paths.js`. Set both `RESCRIPT_BSC_EXE` and
`RESCRIPT_RUNTIME` to compare with another local compiler build; the harness
preserves them only when both are set, so the two implementations still use
the same inputs.

The authoritative gate requires Linux (`/proc`), `strace`, GNU-compatible
nanosecond `date`, and a stable plugged-in host with no competing heavy work.
Keep the isolated fixtures on a case-sensitive Linux filesystem. A Linux
container backed by a case-insensitive macOS bind mount can transiently report
that a differently-cased recreated CMI exists to `stat` and then return
`ENOENT` from the immediately following `open`; that host-filesystem artifact
is not valid scheduler or benchmark evidence. The harness's default `mktemp`
workspace normally stays on the container filesystem.
Set `REWATCH_PERFORMANCE_THRESHOLD_PERCENT` to exercise a proposed threshold
change; changing the committed 125% completion criterion requires an explicit
project decision. Set `KEEP_REWATCH_BENCHMARK_WORKDIR=1` to retain traces and raw
stdout/stderr for investigation. For a quick correctness-only check, an odd run
count below five is accepted only with `REWATCH_ALLOW_SMOKE_RUN=1`; its timing
must never be treated as a quality-gate result.

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

It warms both implementations, interleaves an odd number of timed edits,
requires byte-identical generated JavaScript and normalized parser/compiler
argument logs, and samples file descriptors, tasks, and RSS after every build.
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
