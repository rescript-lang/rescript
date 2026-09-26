# Experimental OCaml rewatch

This directory contains the OCaml port of the ReScript build system. Linux,
macOS, and Windows packages use it experimentally as `rescript`; the Rust
implementation remains available as `rescript-rust` on every platform.

## Status

The OCaml port's cross-platform baseline gates passed before parallel
compiler domains became the default. Domain builds now pass repeated Linux
artifact and focused concurrency checks; broader project and platform
validation continues. Current domain measurements and limits are in
[`bench/README.md`](bench/README.md).

Before the domain cleanup, an uninterrupted `make test-all` run passed all
compiler, runtime, build, GenType, analysis, tools, and canonical rewatch tests.
The shared integration suite runs against the packaged OCaml executable.
All 139 Rust unit tests were reviewed: 131 mapped to focused or shared tests,
five documented intentional differences, and three covered omitted telemetry.

OpenTelemetry is deliberately omitted, and source filters support the documented
common Rust/Re regular-expression subset rather than every Rust-regex construct.
Earlier native Windows validation covered
the focused suite, all 42 OUnit2 cases, all 48 applicable canonical integration
cases, Rust's 136 unit tests, package promotion/inventory, and both packaged
executables. On a two-vCPU Windows 11 ARM64 guest running the x64 package under
emulation, five interleaved canonical clean builds measured 42.267 s for Rust
and 50.502 s for OCaml (1.195x). Seven-run no-op medians were 465 ms and 533 ms
(1.146x), and seven interleaved single-edit medians were 192 ms and 225 ms
(1.172x). These are low-core VM checkpoints, not portable absolute timings.

## Documentation

- This README is the maintained implementation, architecture, build, packaging,
  and platform-status entry point.
- [`PARITY_CHECKLIST.md`](PARITY_CHECKLIST.md) is the behavior-by-behavior
  contract and validation inventory.
- [`bench/README.md`](bench/README.md) documents reproducible performance,
  filesystem-work, resource, artifact, and source-size gates.

## Build

From the repository root, with the dependencies declared in `rescript.opam`
installed:

```sh
opam exec -- dune build rewatch-ocaml/rescript_ocaml.exe
```

The executable is written to:

```text
_build/default/rewatch-ocaml/rescript_ocaml.exe
```

Linux release jobs use Dune's `static` profile. The rewatch executable declares
the same explicit `-ccopt -static` profile flag as the compiler executables, so
the npm artifact does not depend on the runner's glibc. A local Linux packaging
check can reproduce that link with:

```sh
opam exec -- dune build --profile static rewatch-ocaml/rescript_ocaml.exe
file _build/default/rewatch-ocaml/rescript_ocaml.exe
```

Build and watch requests compile on a bounded pool of OCaml domains by
default. The main domain schedules dependencies and records build state;
compiler workers parse and compile independent modules. One export domain can
copy independent implementation artifacts while compiler jobs continue. The
compiler worker count is the lesser of eight and one fewer than the available
CPU count, with a minimum of one. Set `REWATCH_COMPILER_DOMAINS` to override it
(one through the scheduler's platform bound, at least twelve). The heuristic
is provisional.
PPXs and hooks remain external processes.

When running outside this repository's normal Makefile environment, supply
the runtime explicitly:

```sh
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
_build/default/rewatch-ocaml/rescript_ocaml.exe build path/to/project
```

The repeated testrepo clean builds matched selected AST, IAST, CMI, CMJ,
JavaScript, source-map, and namespace-map artifacts across one, two, four,
eight, and twelve domains. A rare earlier output mismatch remains unexplained;
see [`bench/README.md`](bench/README.md) for measurements and limits.

On this experimental branch, published ReScript packages use the OCaml
implementation for the normal `rescript` command. The Rust reference
implementation remains available for side-by-side testing on every platform:

```sh
npx rescript build
npx rescript-rust build
```

Windows ships the Dune-promoted OCaml executable as `rescript.exe` and the Rust
reference as `rescript-rust.exe`, matching the other platform packages. The
x64 package declares compatibility with ARM64 Windows because Windows runs
these x64 executables through its emulation layer; the launcher maps native
ARM64 Node to that package.

The packaged executable discovers `bsc.exe` beside itself, like Rust rewatch,
and the npm launcher supplies the installed runtime path. Direct invocation can
instead resolve `@rescript/runtime` from the project hierarchy. The environment
variables above remain useful overrides for the dune development executable;
they are not required by the normal packaged launcher.

Supported commands are `build` (the default), `watch`, `clean`, `format`, and
`compiler-args`. The CLI is declared with Cmdliner; run the executable with
`--help` for the current option summary.

The binary version has a single OCaml source in `rewatch_version.ml`.
Repository releases synchronize it with the compiler and Rust rewatch versions
through `yarn constraints`; `yarn constraints --fix` updates all three from the
release version declared in `yarn.config.cjs`.

The implementation is split by ownership rather than mirroring the Rust source
layout mechanically. Configuration types and the shared error identity live in
`config_types.ml`, duplicate-aware JSON primitives and structured field decoders
in `config_decode.ml`, and `config.ml` retains the top-level loader and
runtime/path queries. `file_util.ml` owns general portable path, directory,
copy, comparison, inventory, and removal operations; `build_artifacts.ml` owns
ReScript output paths, publication, ownership, and stale-artifact
cleanup. `package_graph.ml` owns package discovery; `package_parse.ml` and
`package_compilation.ml` own per-package parsing and compiler-job construction;
and `package_build.ml` sequences those phases. `build.ml` retains transaction
orchestration and aggregate dispatch, while `build_report.ml` owns presentation.
`build_preparation.ml` consumes the prepared packages to initialize compiler
context, clean stale assets, and run the preliminary parse; `module_graph.ml`
owns dependency resolution, graph-node identities, and cycle analysis.

The experimental `REWATCH_FROZEN_VALUES=1` compiler session passes newly
parsed ASTs, dependency lists, frozen interfaces, and cross-module optimization
metadata directly between jobs. Dependent compiler jobs can start before CMI
and CMJ artifacts are exported; export finishes before build success. Its
module-result API also retains bounded typed semantic data, structured
diagnostics, and output paths. Separate CMI and CMJ fingerprints control
dependent recompilation. The current implementation and benchmark results are
documented in
[`compiler/ml/IMMUTABLE_INTERFACES.md`](../compiler/ml/IMMUTABLE_INTERFACES.md).
`compiler_process.ml` is the boundary between logical compiler jobs and
in-process execution. Independent parse and compile requests run on a bounded
domain pool. The scheduler accepts module results and records build state;
eligible artifact exports run on one separate domain. The
driver resets command-line flags, warnings, JSX and experimental settings,
package and output state, runtime and project paths, load paths, environment
and CRC caches, predefined type graphs, delayed checks, CMT accumulation,
backend caches, and diagnostic
state before and after every request, including exceptional returns. CMT files
record each request's logical compiler argv rather than rewatch's process argv.
Compiler output routed through request-owned channels and formatters is
captured per request and returned to rewatch;
help, version, formatting, and reprinting requests return status instead of
terminating the host process. Built-in-PPX names, type-node identifiers, and
Lambda static-exit identifiers also restart at the request boundary; without
those resets, repeated requests can change binary AST or CMI serialization even
when their source is unchanged. The compiler build identity is generated from
the recursive compiler source, platform-stub, C-stub, and Dune-rule inputs and
shared by the embedded driver and standalone wrapper, so nested compiler
changes invalidate artifacts while rewatch-only edits do not.
`package_plan.ml` owns immutable per-package build inputs, `build_session.ml`
owns the project graph, package configuration, compiled-artifact freshness,
and compiler dependency cache retained across watch rebuilds. The driver gives
each module job fresh inference, diagnostics, and
environment state while the project session lends each job a private table of
decoded small interfaces and an expanded signature graph. A cache hit checks
the current load path and file identity; typed graph checks detect mutations,
and changed graphs are restored from a saved image before reuse. Once a job
ends, its table returns to the session
and can be used by a later worker domain, including after a watch edit. A full
watch rebuild reconstructs the graph but retains the compiler dependency
session for the same project. Set
`REWATCH_PROJECT_CMI_CACHE=0` to limit the small-interface cache to `Stdlib`
and `Pervasives` when comparing build performance.
`build_attempt.ml` owns
attempt kinds, parse outcomes, diagnostics, counters, scheduled work, and final
cleanup for one build attempt. `source_dirs.ml` owns
source-directory metadata projection and serialization. `process_child.ml`
owns the lifecycle of one subprocess while `process.ml` owns scheduling.
Command-level post-build execution and its error handling live in
`after_build.ml`.
External PPXs are deliberately still subprocesses. Their launch is routed back
through rewatch's interruptible process owner so Unix process groups and Windows
Job Objects continue to terminate PPX descendants. JavaScript post-build hooks,
`--after-build`, formatting, and lock-owner probes likewise retain the generic
process infrastructure.

Both rewatch implementations skip a small set of PPXs when the source does not
contain the syntax that can trigger them. This is deliberately conservative:
a false positive only performs extra work, while a false negative changes the
compiled program. New rules therefore require a PPX-specific guarantee that all
of its transformations are gated by the marker. The current rules are kept in
matching tables in `rewatch/src/build/parse.rs` and
`rewatch-ocaml/compiler_args.ml`.

If this optimization grows, the preferred generalization is explicit
source-trigger metadata on each `ppx-flags` entry in `rescript.json`, decoded by
both build systems while preserving the existing string and string-array forms.
The build system can then strip the metadata and invoke the unchanged PPX
command only when one of its declared markers is present. Inferring behavior
from arbitrary PPX package names or automatically skipping every PPX is not
safe because a PPX may transform an otherwise unmarked file.

Genuinely platform-specific behavior is consolidated behind a `Platform`
boundary rather than mixed into those modules. Unix and Windows modules now own
executable lookup, subprocess creation, signal deferral, and process-tree
termination as well as lock-owner PID probing and capture-pipe creation. The
native watcher has a separate, narrow cross-platform boundary over libuv.
`watch_scope.ml` owns package/source selection and watch paths,
`watch_snapshot.ml` owns filesystem baselines and diffs, and `watcher.ml` owns
event reconciliation and rebuild lifecycle. Portable `Filename`-based path and
artifact logic remains shared.

## Test

```sh
opam exec -- dune runtest tests/rewatch_ounit_tests
rewatch-ocaml/tests/check_config_acceptance.sh
rewatch-ocaml/tests/check_command_validation.sh
rewatch-ocaml/tests/check_interactive_output.sh
rewatch-ocaml/tests/check_windows_job_stub.sh "$WINDOWS_CC" \
  "$WINDOWS_OCAML_INCLUDE"
sh rewatch-ocaml/tests/run.sh \
  "$PWD/_build/default/rewatch-ocaml/rescript_ocaml.exe"
```

The interactive-output parity gate requires the `script` PTY utility. Windows
CI skips that gate when `script` is unavailable; Windows output formatting is
still covered by unit tests, while redirected and verbose behavior is covered
by the platform-independent parity gates.

The canonical integration suite can use the port through its existing override:

```sh
export REWATCH_EXECUTABLE="$PWD/_build/default/rewatch-ocaml/rescript_ocaml.exe"
eval "$(cd rewatch/tests && node ./get_bin_paths.js)"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
bash rewatch/tests/compile/01-basic-compile.sh
```

## Packaging checks

The release inventory uses the repository's existing Dune promotion and npm
artifact tooling. On Linux, build and verify the current platform package with:

```sh
opam exec -- dune build --profile static compiler/sync/rescript.exe
file packages/@rescript/linux-arm64/bin/rescript.exe
node scripts/checkCompilerExes.js
node scripts/updateArtifactList.js
git diff --exit-code packages/artifacts.json
yarn workspace @rescript/linux-arm64 pack --json --dry-run
```

Use `linux-x64` instead on an x64 host. On Windows, build the Dune target and
run the same checks against `@rescript/win32-x64`; Corepack can invoke the final
command as `corepack yarn workspace @rescript/win32-x64 pack --json --dry-run`.
The package listing must contain the
OCaml `bin/rescript.exe`, the Rust reference `bin/rescript-rust.exe`, and both
rewatch notice files. CI runs the artifact-list check only after downloading
all platform builds and treats any missing declared executable as an error;
local manifest generation uses temporary placeholders solely for platforms
that were not built on the current machine.

OpenTelemetry/OTLP tracing is intentionally not part of this port. This is an
explicit project scope decision, not a silently ignored configuration feature;
ordinary command output, verbosity, diagnostics, and exit statuses remain in
scope.

## Platform status

The Windows implementation is complete and natively validated. Windows ships
the OCaml implementation as the default `rescript.exe` and retains Rust as
`rescript-rust.exe`. Unix subprocesses use the cross-platform `spawn` library.
Windows uses a narrow native
`CreateProcessW` owner so it can establish Job Object ownership before a child
starts running.
Compiler output is captured through close-on-exec pipes drained by blocking
reader threads, avoiding reliance on Windows `select` support for anonymous
pipes. Each child is attached to a retained Windows Job Object so cancellation
can terminate descendants after the direct process has exited; the native stub
check above compiles that API boundary with warnings as errors when given a
Windows-targeting C compiler and its matching OCaml header directory. Watch mode
uses long-lived filesystem-event handles through Luv/libuv and retains the
snapshot-based polling loop only as a runtime fallback. Native tests exercise
Job assignment, descendant-held-pipe cancellation, lock-driven cancellation,
native process IDs and lock contention, watcher rebuild/recovery, formatting,
and post-build command execution. The MSYS harness cannot deliver a normal
Windows console-control event to a native child, so it exercises the same
cleanup path by removing `watch.lock`; console signal delivery remains covered
on Unix. Windows streaming `--after-build` commands inherit terminal stdin while
remaining in their Job Object. Unix terminal stdin is intentionally withheld
from these commands because their separately owned process group would be
stopped by `SIGTTIN`; redirected stdin is inherited, and full terminal input
requires a future PTY relay that preserves process-tree cancellation. Shared
path construction uses OCaml's `Filename` APIs so Windows
separators and drive roots are not hard-coded assumptions.
