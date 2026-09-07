# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

Milestones 1 and 3 are implemented, and milestones 2, 4, and 5 have working
but incomplete coverage. The experimental
`rescript_ocaml.exe` currently implements single-package configuration loading,
recursive source discovery, external `bsc` parsing, AST dependency extraction,
cycle detection, dependency-ordered compilation, interface-before-implementation
compilation, bounded concurrent external `bsc` execution, feature-gated source
selection, stale artifact cleanup, and compiler artifact publication to
`lib/ocaml`.

## Verified

- `dune runtest rewatch-ocaml` passes graph unit coverage.
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
  collection.
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
  from an ancestor hoist.
- A project-local copy of `rewatch/testrepo` completes a one-shot build with
  the OCaml executable. This exercises the existing workspace package graph,
  including its package-level dependency back-edge and `namespace-entry`.
- `bsc-flags` is accepted as the Rust-compatible alias for `compiler-flags`;
  nested compiler flag groups are flattened into direct `bsc` arguments, and
  `--warn-error` replaces config warning errors.
- `gentypeconfig` is validated and projected to compile-only `bsc` flags. The
  focused fixture verifies argument projection and a successful GenType-enabled
  build.
- The integration runner starts watch mode, confirms the lock, performs a
  source edit, changes the configured output suffix, observes the resulting
  rebuild and stale-output removal, adds then deletes a source module while
  observing its generated output appear and disappear, and confirms lock
  cleanup after `SIGTERM`.

## Known gaps

- Full monorepo/package graph parity, configuration validation parity, compiler
  argument parity, telemetry, and production-grade filesystem watching remain
  incomplete.
- GenType dependency-path metadata and root-project inheritance remain
  incomplete; the current implementation forwards declared dependency names and
  source directories only.
- `watch` currently uses conservative polling and has no signal/lock/event
  batching parity with Rust rewatch.
- Polling watches root and recursively resolved local dependency roots, but it
  is not yet a native event backend and has not been exercised against the full
  Rust watch suite.
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

1. Parameterize applicable Rust integration fixtures for the OCaml executable.
2. Complete monorepo/package discovery and remaining configuration fields.
3. Replace polling with a supported event backend and verify applicable watch tests.
