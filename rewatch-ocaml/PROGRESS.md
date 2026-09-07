# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

Milestones 1 and the core of milestone 3 are implemented for the experimental
single-package path. The experimental
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
- `build`, `clean`, `watch`, `--prod`, `--features`, `--help`, and `--version`
  dispatch successfully; `clean` removes only the selected package's build
  artifact directories.
- Independent parser/compiler jobs are launched in bounded batches (four
  children by default), with private output files and deterministic diagnostic
  collection.
- `warnings`, `ppx-flags`, JSX v4, source-map, `LetUnwrap` experimental
  features, and `js-post-build` are projected into external compiler/process
  invocations. The post-build fixture verifies its generated-file argument.

## Known gaps

- Package graph construction and recursive dependency builds, namespace maps,
  full compiler argument parity, configuration validation parity, format and
  compiler-args commands, incremental state, telemetry, and production-grade
  filesystem watching remain incomplete.
- `watch` currently uses conservative polling and has no signal/lock/event
  batching parity with Rust rewatch.
- Local source dependencies under `node_modules` or a sibling package are
  recursively built with dependency feature selections and cycle protection;
  prebuilt packages are accepted through their `lib/ocaml` include path.
- The initial implementation targets Unix process semantics; supported platform
  parity has not been evaluated.

## Next actions

1. Address milestone-1 independent review findings and rerun its gate.
2. Add package discovery and full configuration projection for milestone 2.
3. Parameterize the existing Rust integration suite for the OCaml executable.
