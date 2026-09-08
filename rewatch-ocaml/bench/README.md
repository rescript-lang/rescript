# Performance and work-equivalence gate

`performance_gate.sh` compares release builds on two fully isolated copies of
the tracked `rewatch/testrepo` fixture. It deliberately archives both the
fixture and its external Belt/runtime targets and copies all installed,
Git-ignored dependency trees (including nohoisted dependencies) separately
into each root. Cleaning one
implementation therefore cannot warm or remove artifacts used by the other.

The gate performs one warm-up per implementation, at least five interleaved
clean builds, and reports median wall time plus peak summed process-tree RSS. It
then traces a clean build with `strace` and requires identical normalized
package/phase/input multisets as well as identical counts for parser, namespace,
compiler, interface, and PPX process launches. Finally, both
implementations clean and build a third fixture at the same absolute path; the
gate requires identical generated JavaScript, compiler interfaces (`.cmi`),
JavaScript IR (`.cmj`), and namespace maps. It deliberately does not treat
`.cmt/.cmti`, parser AST caches, compiler logs, `build.ninja`,
`compiler-info.json`, or `.sourcedirs.json` as byte-stable outputs: those files
contain diagnostics/debug metadata or implementation-specific incremental
state and are covered by integration tests instead. The default
acceptance threshold requires both OCaml medians to be no more than 125% of
Rust.

This is one part of equivalence checking, not a substitute for the test suites.
Before accepting a performance increment, also run the OCaml unit/focused tests
and the canonical Rust rewatch integration suite against the OCaml executable:

```sh
opam exec -- dune runtest rewatch-ocaml
bash rewatch-ocaml/tests/run.sh \
  _build/default/rewatch-ocaml/rescript_ocaml.exe
(cd rewatch/tests && \
  bash ./suite.sh ../../_build/default/rewatch-ocaml/rescript_ocaml.exe)
```

Together these cover three different failure classes:

- the canonical and focused suites check observable command/build/watch
  behavior;
- the `strace` classification checks that a speed result did not hide skipped
  or superfluous module/PPX work (argument semantics remain covered by the
  compiler-argument and integration tests);
- the fresh-tree manifest comparison checks the selected generated file set and
  byte contents.

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

The authoritative gate requires Linux (`/proc`), `strace`, GNU-compatible
nanosecond `date`, and a stable plugged-in host with no competing heavy work.
Set `REWATCH_PERFORMANCE_THRESHOLD_PERCENT` to exercise a proposed threshold
change; changing the committed 125% completion criterion requires an explicit
project decision. Set `KEEP_REWATCH_BENCHMARK_WORKDIR=1` to retain traces and raw
stdout/stderr for investigation. For a quick correctness-only check, an odd run
count below five is accepted only with `REWATCH_ALLOW_SMOKE_RUN=1`; its timing
must never be treated as a quality-gate result.

## Planned filesystem-work audit

After pipe-based subprocess capture removes the intentional temporary capture
files, compare filesystem work as a second orchestration audit. On Linux this
can use `strace -f -e trace=%file` around isolated clean, unchanged, and
single-edit builds. Normalize each fixture root, retain operations whose target
is inside that root, and compare both per-path operation multisets and readable
categories such as metadata probes, opens, directory scans, creates, renames,
and removals.

Do not gate on the raw process-wide syscall total: Rust, OCaml, libc, the
dynamic loader, and subprocess startup legitimately perform different
toolchain-level accesses. Report those separately, and treat repeated accesses
to the same project artifact or discovery path as the primary evidence of
superfluous orchestration work. The existing compiler-work and artifact checks
must remain enabled so fewer filesystem calls cannot conceal skipped work.
