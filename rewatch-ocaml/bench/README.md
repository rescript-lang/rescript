# Performance and work-equivalence gate

`performance_gate.sh` compares release builds on two fully isolated copies of
the tracked `rewatch/testrepo` fixture. It deliberately archives both the
fixture and its external Belt/runtime targets and copies all installed,
Git-ignored dependency trees (including nohoisted dependencies) separately
into each root. Cleaning one
implementation therefore cannot warm or remove artifacts used by the other.

The gate performs one warm-up per implementation, at least five interleaved
clean builds, and reports median wall time, peak summed process-tree RSS, and
peak process-tree task count. Wall time and RSS are acceptance criteria; task
count is diagnostic evidence for subprocess-management overhead. It then traces
clean, unchanged, and single-edit builds with `strace` and requires identical
normalized package/phase/input multisets as well as identical counts for parser,
namespace, compiler, interface, and PPX process launches. The edit targets the
same leaf source in each isolated fixture. This sequence detects superfluous
incremental parsing or compilation that a clean-only comparison cannot expose.
Finally, both implementations clean and build a third fixture at the same
absolute path. The gate first requires the complete post-build
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

The OCaml implementation compiles in-process, so it has no compiler `execve`
to trace. The gate counts its compiler requests from the log it writes when
`REWATCH_COMPILER_CALL_LOG` is set, and still traces PPXs as processes.
`RESCRIPT_BSC_EXE` only selects Rust's compiler; the OCaml executable uses the
compiler linked into it, so build both from the same checkout. Set
`REWATCH_COMPILER_DOMAINS` to measure a specific number of compiler workers.

Set `REWATCH_PERFORMANCE_THRESHOLD_PERCENT` to change the 125% threshold. Set `KEEP_REWATCH_BENCHMARK_WORKDIR=1` to retain traces and raw
stdout/stderr for investigation. For a quick correctness-only check, an odd run
count below five is accepted only with `REWATCH_ALLOW_SMOKE_RUN=1`; its timing
is not a meaningful measurement.

## Watch performance gate

The retained-watch performance and resource gate exercises several ordinary
edits through one long-lived watcher:

```sh
rewatch-ocaml/bench/watch_performance_gate.sh \
  rewatch/target/release/rescript \
  _build/default/rewatch-ocaml/rescript_ocaml.exe \
  7
```

It warms both implementations, interleaves an odd number of timed edits,
requires byte-identical generated JavaScript and equal parser/compiler work
counts (Rust's through the counting `bsc` proxy, OCaml's through
`REWATCH_COMPILER_CALL_LOG`), and samples file descriptors, tasks, and RSS after
every build. Set `REWATCH_WATCH_COMPILER_DOMAINS` to run the OCaml watcher with
a specific number of compiler workers.
This catches retained-state implementations that appear fast by skipping work,
as well as resource growth that a single build cannot show. The
default median-latency limit is 150% of Rust because individual watch events
include operating-system notification and 50 ms polling intervals; override it
with `REWATCH_WATCH_PERFORMANCE_THRESHOLD_PERCENT` only for investigation.
The build gate's lower-noise 125% clean/incremental threshold remains the
authoritative general performance criterion. Set
`KEEP_REWATCH_WATCH_PERFORMANCE=1` to retain output, compiler-call logs,
latencies, and fixtures. This gate requires Linux `/proc`, GNU-compatible
millisecond `date`, and `setsid`.
