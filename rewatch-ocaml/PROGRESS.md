# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

No milestone is complete against the canonical `rewatch/tests` suite yet. The
experimental `rescript_ocaml.exe` builds the full `rewatch/testrepo` and now
implements the first slice of persistent incremental state using existing AST,
CMI, CMT, and generated-output artifacts. Work remains focused on milestone 4:
expanding invalidation and diagnostic parity through the canonical edit tests.

The implementation currently has configuration loading, source and package
discovery, external `bsc` parsing, AST dependency extraction, cycle detection,
dependency-ordered compilation, interface-before-implementation compilation,
bounded concurrent external `bsc` execution, feature selection, artifact
cleanup, and compiler artifact publication to `lib/ocaml`.

## Source review

The first comparison pass covered the OCaml configuration, package traversal,
source discovery, process runner, compile scheduling, cleanup, CLI, formatting,
and polling watcher against their Rust owners. It found and fixed these blocking
differences:

- Resolved package paths were not canonical, so workspace symlink cycles could
  recurse indefinitely.
- Traversal tracked only the active recursion stack instead of a command-wide
  package set, rebuilding the same package several times.
- External packages incorrectly included `dev-dependencies`.
- `namespace: true` used a scoped package name as a literal filename instead of
  applying Rust's namespace normalization.
- PPX resolution did not search hoisted `node_modules`.
- Stale cleanup treated every JavaScript-looking file as owned output and
  deleted checked-in legacy files that had no corresponding source or AST.
- Standalone package builds refused to build dependencies resolved outside the
  invoked package directory.

The main remaining architectural differences are substantial: Rust constructs
one unified package/module build state and schedules a single cross-package
graph. The OCaml port still recurses by package and reconstructs its in-memory
state for every invocation, although it now derives dirty parse and compile
nodes from persistent compiler artifacts and propagates CMI/removal changes
across package boundaries. Rust also has robust build/watch locks, native
filesystem events, diagnostic persistence, telemetry, and much broader
configuration and platform handling that are not yet ported.

## Verified

- `dune runtest rewatch-ocaml` passes graph unit coverage.
- A clean one-shot build of the installed `rewatch/testrepo` succeeds with the
  OCaml executable, including workspace packages, external dependencies,
  namespace entries, and the hoisted PPX executable.
- The canonical compile tests 01 through 08 pass unchanged with the OCaml
  executable. This covers clean builds, standalone packages, implementation and
  interface renames, namespaced dependents, orphan-interface warnings, and
  cross-package source removal.
- Incremental builds reuse clean ASTs and compiler outputs, preserve unchanged
  CMI timestamps, recompile dependents after interface changes, avoid dependent
  recompilation after implementation-only changes, and replay local compiler
  warnings using the same artifact behavior as Rust.
- `rewatch-ocaml/tests/run.sh` passes with both the OCaml executable and the
  Rust reference executable for a three-module fixture, a `.res`/`.resi` pair,
  cycle diagnostics, compilation failure, and a successful recovery build.
- Generated JavaScript for the selected successful fixture is produced by the
  same `bsc` invocations and is byte-identical between runners.
- `build`, `clean`, `watch`, `format`, `compiler-args`, `--prod`, `--features`,
  `--filter`, `--after-build`, `--warn-error`, `--help`, and `--version`
  dispatch successfully. `clean` removes root and local dependency build
  artifacts, including in-source JavaScript and maps.
- Independent parser/compiler jobs are launched in bounded batches (four
  children by default), with private output files and deterministic diagnostic
  collection. Their transient logs are created in the owning project/build
  directory, and interruption terminates and reaps launched children before
  cleaning those logs.
- `warnings`, `ppx-flags`, JSX v4, source-map, `LetUnwrap` experimental
  features, and `js-post-build` are projected into external compiler/process
  invocations. The post-build fixture verifies its generated-file argument.
- `format`, `compiler-args`, `--filter`, and `--after-build` are implemented.
  The test runner covers stdin formatting, compiler-argument JSON, filtering,
  and an after-build assertion.
- Namespace packages generate and compile their `.mlmap` before member modules.
  Out-of-source package output directories are created before compilation and
  stale output is removed; `clean` also removes in-source JavaScript and maps.
- The integration runner builds a three-package monorepo through relative
  `node_modules` workspace links, including a transitive dependency resolved
  from an ancestor hoist. It also exercises a package-level dependency back
  edge and verifies that `clean` removes every package's compiler artifacts.
- A project-local copy of `rewatch/testrepo` completes a one-shot build with
  the OCaml executable. This exercises the existing workspace package graph,
  including its package-level dependency back-edge and `namespace-entry`.
- A minimal nested-workspace regression verifies that recursive build and clean
  own only dependencies canonically contained by the workspace root, leaving
  external linked packages untouched. The full copied fixture still needs a
  dependency-ownership adapter before it can be a repeatable runner.
- `bsc-flags` is accepted as the Rust-compatible alias for `compiler-flags`;
  nested compiler flag groups are flattened into direct `bsc` arguments, and
  `--warn-error` replaces config warning errors.
- Legacy `bs-dependencies`, `bs-dev-dependencies`, `es6`, and `cjs`
  configuration aliases are accepted with the same effective dependency and
  package-output behavior as their modern spellings.
- `gentypeconfig` is validated and projected to compile-only `bsc` flags. The
  focused fixture verifies argument projection and a successful GenType-enabled
  build, including resolved local dependency metadata.
- Workspace packages inherit project-root JSX, source-map, experimental, and
  package-output settings. The dependency fixture verifies root-suffix output
  and cleanup despite a conflicting package-local suffix.
- The integration runner starts watch mode, confirms the lock, performs a
  source edit, changes the configured output suffix, observes the resulting
  rebuild and stale-output removal, adds then deletes a source module while
  observing its generated output appear and disappear, and confirms lock
  cleanup after `SIGTERM`.
- `watch.lock` contains the running watch process PID, matching the lock-file
  protocol used by the existing integration helpers.
- Unknown top-level configuration fields emit an explicit warning and are
  ignored, matching Rust rewatch's forward-compatible configuration behavior.

## Known gaps

- Incremental state currently relies on artifact timestamps and byte-identical
  CMI publication. Rust's richer persisted compile-state model and diagnostic
  storage are not yet ported.
- Packages are deduplicated during recursive traversal, but compilation still
  happens as separate per-package graphs rather than Rust's unified graph.
- Full monorepo/package graph parity, configuration validation parity, compiler
  argument parity, locks, telemetry, and production-grade filesystem watching
  remain incomplete.
- `watch` currently uses conservative polling and has no signal/lock/event
  batching parity with Rust rewatch.
- Polling watches root and recursively resolved local dependency roots, but it
  is not yet a native event backend and has not been exercised against the full
  Rust watch suite.
- `watchexec` is available on the current macOS development host and provides
  a native-event candidate, but it is not bundled with this experimental dune
  executable; polling remains the portable fallback until packaging is decided.
- Local source dependencies under `node_modules` or a sibling package are
  recursively built with dependency feature selections and cycle protection;
  prebuilt packages are accepted through their `lib/ocaml` include path.
- Package resolution searches a package's `node_modules` and ancestor hoists,
  then workspace-sibling locations. A copied `rewatch/testrepo` cannot yet be
  used for end-to-end verification because its workspace symlinks are relative
  to the original repository and become broken when copied; the dedicated
  monorepo fixture preserves those links instead.
- The initial implementation targets Unix process semantics; supported platform
  parity has not been evaluated.

## Next actions

1. Continue the canonical compile suite at dependency-cycle reporting, duplicate
   modules, and dev-dependency visibility.
2. Replace recursive package scheduling with a unified cross-package module
   graph as later correctness cases require it.
3. Continue the remaining canonical groups, then complete configuration and
   watch parity exposed by them.
