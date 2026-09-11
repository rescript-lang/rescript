# Experimental OCaml rewatch

This directory contains the separately named OCaml port of the ReScript build
system. It does not replace the Rust `rescript` executable.

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

It invokes `bsc` as an external process. When running outside this repository's
normal Makefile environment, point it at the compiler and runtime explicitly:

```sh
export RESCRIPT_BSC_EXE="$PWD/_build/default/compiler/bsc/rescript_compiler_main.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
_build/default/rewatch-ocaml/rescript_ocaml.exe build path/to/project
```

On this experimental branch, published ReScript packages on Linux and macOS use
the OCaml implementation for the normal `rescript` command. The Rust reference
implementation remains available for side-by-side testing:

```sh
npx rescript build
npx rescript-rust build
```

The `rescript-ocaml` launcher remains as an alias for existing testers. Windows
continues to use Rust for `rescript` until the native Windows implementation and
runtime test pass are complete, and does not yet expose a separate Rust alias.

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
cleanup. `package_graph.ml` owns package discovery, `package_build.ml` owns
per-package parsing, dirty-state preparation, and compiler-job construction,
and `build.ml` retains command/reporting orchestration and aggregate dispatch.
`build_preparation.ml` consumes the prepared packages to initialize compiler
context, clean stale assets, run the preliminary parse, and construct global
dependency/build state. Command-level post-build execution and its error
handling live in `after_build.ml`.
Genuinely platform-specific behavior is consolidated behind a `Platform`
boundary rather than mixed into those modules. Unix and Windows modules now own
executable lookup, subprocess creation, signal deferral, and process-tree
termination as well as lock-owner PID probing and capture-pipe creation. The
native watcher has a separate, narrow cross-platform boundary over libuv, and
portable `Filename`-based path and artifact logic remains shared.

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

The canonical integration suite can use the port through its existing override:

```sh
export REWATCH_EXECUTABLE="$PWD/_build/default/rewatch-ocaml/rescript_ocaml.exe"
eval "$(cd rewatch/tests && node ./get_bin_paths.js)"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
bash rewatch/tests/compile/01-basic-compile.sh
```

See `PROGRESS.md` for verified coverage, measurements, review results, and
remaining compatibility or platform gaps. `PARITY_CHECKLIST.md` defines the
separate validation-inventory and interactive-output gates that must be closed
before replacement.

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

Use `linux-x64` instead on an x64 host. The package listing must contain the
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

Windows support is required for completion, even though runtime verification is
not available in the current Linux development environment. Unix subprocesses
use the cross-platform `spawn` library. Windows uses a narrow native
`CreateProcessW` owner so it can establish Job Object ownership before a child
starts running.
Compiler output is captured through close-on-exec pipes drained by blocking
reader threads, avoiding reliance on Windows `select` support for anonymous
pipes. Each child is attached to a retained Windows Job Object so cancellation
can terminate descendants after the direct process has exited; the native stub
check above compiles that API boundary with warnings as errors when given a
Windows-targeting C compiler and its matching OCaml header directory. Watch mode
uses long-lived filesystem-event handles through Luv/libuv and retains the
snapshot-based polling loop only as a runtime fallback. Job assignment and
pipe-tree cancellation, the native watcher, and the lock lifecycle still
require native Windows runtime verification.
`PROGRESS.md` tracks the remaining portability blockers. Shared path
construction uses OCaml's `Filename` APIs so Windows separators and drive roots
are not hard-coded assumptions.
