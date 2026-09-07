# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

The complete applicable canonical `rewatch/tests` suite now passes with the
experimental `rescript_ocaml.exe`. Milestone 6 remains open for the broader
configuration/platform inventory, performance and resource measurements, and
final whole-port review. Incremental state uses existing AST, CMI, CMT, and
generated-output artifacts rather than in-process compiler state.

The implementation currently has configuration loading, source and package
discovery, external `bsc` parsing, AST dependency extraction, cycle detection,
dependency-ordered compilation, interface-before-implementation compilation,
bounded concurrent external `bsc` execution, feature selection, artifact
cleanup, and compiler artifact publication to `lib/bs` and `lib/ocaml`.

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

A fresh review of the compile 09–13 increment found failure-log omissions,
unsafe deferred watch outputs, first-edge-wins feature selection, dependency
filter leakage, missing cycle-log diagnostics, ANSI-bearing logs, cwd-dependent
duplicate paths, and ordinary-namespace display errors. All were addressed;
the deferred-output mechanism was removed, and the affected compile, feature,
warning, and atomic-save tests were rerun successfully.
A focused follow-up found that local cycles bypassed the global diagnostic,
pre-parsing missed `--warn-error`, completion preceded log finalization, and a
package back-edge could widen root CLI features. These were also fixed; the
reviewer-confirmed watch snapshot logic was retained. A final cycle review also
identified unblocked transitive dependents; the global graph now blocks their
reverse closure while continuing to compile unrelated modules, with focused
unit coverage for that invariant.

Two independent watch reviews covered behavioral parity and resource/locking
safety. Their confirmed findings drove absent-output staging (including source
maps), recoverable initial/rebuild errors, atomic populated lock creation with
stale-owner takeover, workspace build locks, owned lock removal, race-tolerant
symlink-aware snapshots, and cached content hashes. Takeover markers also carry
an owner PID and can themselves be recovered after an interrupted takeover.

## Verified

- `dune runtest rewatch-ocaml` passes graph unit coverage.
- A clean one-shot build of the installed `rewatch/testrepo` succeeds with the
  OCaml executable, including workspace packages, external dependencies,
  namespace entries, and the hoisted PPX executable.
- The canonical compile tests 01 through 08 pass unchanged with the OCaml
  executable. This covers clean builds, standalone packages, implementation and
  interface renames, namespaced dependents, orphan-interface warnings, and
  cross-package source removal.
- Canonical compile tests 09 through 13 pass unchanged. Cross-package cycles
  use a global module view and match the Rust diagnostic snapshot, duplicate
  modules are rejected with project-relative paths, production sources cannot
  see dev-only dependencies, dev sources can, and package back-edges terminate.
- Canonical compile tests 14 through 19 also pass unchanged. Builds leave the
  tracked fixture outputs and snapshots byte-identical, create no unowned files,
  `--prod` excludes dev dependencies and dev sources, external legacy uncurried
  syntax remains visible without leaking unrelated external warnings, and UTF-8
  warning source lines remain intact. This completes the canonical compile group.
- Incremental builds reuse clean ASTs and compiler outputs, preserve unchanged
  CMI timestamps, recompile dependents after interface changes, avoid dependent
  recompilation after implementation-only changes, and replay local compiler
  warnings using the same artifact behavior as Rust.
- `rewatch-ocaml/tests/run.sh` passes with the OCaml executable for a
  three-module fixture, a `.res`/`.resi` pair, cycle diagnostics, compilation
  failure, and a successful recovery build. Its dependency inputs now come
  from a tracked fixture rather than an absent ignored `node_modules` tree.
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
- Every canonical watch test passes with the polling backend: ordinary and
  atomic edits, warning replay, new and deleted sources, configuration suffix
  changes, ignored non-source paths, and missing source folders. Input snapshots
  are deduplicated to local package roots, tolerate rename races, and include a
  content digest so same-size edits are not lost to timestamp granularity.
- Watch publication delays brand-new JavaScript and source maps until the whole
  build succeeds, while existing outputs remain available during recompilation.
  Failed staged modules have their AST invalidated so the next edit recompiles
  them. Global after-build hooks run after publication and outside `build.lock`.
- The canonical lock test passes. Watch locks validate live PIDs, recover stale
  owners, and remove only locks still owned by the exiting process. Build and
  clean commands use a separate PID lock, while an active watch retains its own
  independent lock.
- Compiler logs are initialized and finalized for success and failure, contain
  color-free diagnostics, and receive cross-package cycle errors. The canonical
  atomic-save warning test passes, including an edit that lands during the
  initial build and warning persistence in `.compiler.log`.
- Canonical feature tests 01 through 06 pass. Active features are unioned across
  all consumers, root `--filter` does not hide dependency modules from the
  global graph, feature-map cycles use the Rust diagnostic wording, and empty
  CLI feature selections are rejected compatibly.
- The canonical UTF-8 warning test passes, and a focused failure check verifies
  that `.compiler.log` contains the compiler error and `#Done` without ANSI
  escape sequences.
- Unknown top-level configuration fields emit an explicit warning and are
  ignored, matching Rust rewatch's forward-compatible configuration behavior.
- The canonical suffix test passes. In-source JavaScript, maps, and source
  files are published to `lib/bs` as compiler assets as well as to their public
  output locations, and `clean` removes both forms.
- All four canonical format tests pass. An argument-free format run follows the
  current project context: direct local packages at a workspace root, or only
  the selected package when invoked inside one.
- All four canonical clean tests pass, including scoped package cleaning,
  dev-dependency and external dependency cleanup, and byte-identical rebuild
  output after an explicit clean.
- All experimental and invalid-experimental tests pass. Root experimental
  options reach both parser and compiler arguments for workspace packages,
  invalid shapes include configuration context, and unknown keys list the
  supported feature.
- Both canonical compiler-argument tests pass, including cwd-invariant output
  and parser/compiler warning flag parity.
- `allowed-dependents` is parsed and enforced for regular and development
  dependency edges. Package outputs reject duplicate effective suffix/location
  pairs and require an explicit module when configured, matching current Rust
  validation; legacy `cjs`/`es6` values retain their deprecation diagnostics.
- `watch --clear-screen` is accepted and clears an interactive terminal before
  rebuilds. Comma-separated feature names are trimmed like the Rust CLI.
- GenType compiler arguments distinguish single-file inspection from a full
  build: `compiler-args` omits unavailable expanded source/dependency paths,
  while builds retain them; both include the workspace project root.
- Legacy `bsconfig.json` files are discovered for root and dependency packages,
  formatting, compiler-argument lookup, and watch snapshots. `rescript.json`
  takes precedence when both exist, and using the legacy filename emits the
  same migration diagnostic as Rust rewatch.
- `sourceMap` follows the current object schema (`enabled` is `"always"` or
  `"dev"`, with an explicit mode). Development-only maps are passed as disabled
  for one-shot builds and enabled for watch builds; the obsolete boolean `true`
  form is rejected.
- GenType now receives `-bs-gentype-suffix` only when the top-level suffix was
  explicitly configured, and inherits the module format from object-form
  `package-specs` when `gentypeconfig.module` is absent.

## Performance snapshot

One Linux development-build sample was taken on the current 10-CPU container
using the full `rewatch/testrepo`, the same external `bsc` and runtime, and a
10–20 ms `/proc` sampler that sums the live process tree. Times and peak RSS are
therefore comparative observations, not a benchmark distribution:

| Scenario | Rust | OCaml |
| --- | ---: | ---: |
| Clean build | 7,433 ms / 266,964 KiB | 10,869 ms / 287,396 KiB |
| Unchanged build | 616 ms / 40,056 KiB | 833 ms / 31,600 KiB |
| Single-module edit | 589 ms / 44,824 KiB | 843 ms / 26,632 KiB |
| Watch edit visible | 111 ms | 738 ms |
| Idle watcher | 22,444 KiB / 10 ms CPU per 2 s | 7,568 KiB / 30 ms CPU per 2 s |

The OCaml subprocess bound now follows the detected CPU count, capped at 32;
raising it from the provisional fixed value of four reduced this sample's clean
build from 14,065 ms to 10,869 ms. The remaining clean/edit gap is consistent
with reconstructing package/global state on every command, while watch latency
also includes the 200 ms polling interval.

## Known gaps

- Incremental state currently relies on artifact timestamps and byte-identical
  CMI publication. Rust's richer persisted compile-state model and diagnostic
  storage are not yet ported.
- Packages are deduplicated during recursive traversal, but compilation still
  happens as separate per-package graphs rather than Rust's unified graph.
- Full configuration validation parity, telemetry, performance parity, and
  production-grade filesystem watching remain incomplete.
- `watch` currently uses conservative polling and has no signal/lock/event
  batching parity with Rust rewatch.
- Polling watches root and recursively resolved local dependency roots, but it
  is not yet a native event backend and has only been verified on Unix.
- Existing generated outputs are updated as their compiler subprocesses
  succeed; only previously absent outputs are held until whole-build success.
  This preserves artifact/output consistency and avoids removing last-known
  output during compilation, but it is not an all-or-nothing filesystem
  transaction across an entire incremental build.
- An interrupted build can leave a staging sidecar for a source that is later
  deleted. `clean` removes sidecars for discovered generated outputs, but does
  not sweep suffix-matching files indiscriminately because those may be user
  assets.
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

1. Inventory and close remaining configuration, CLI, telemetry, and supported
   platform gaps, then produce the clean/unchanged/edit/watch performance and
   resource comparison required by milestone 6.
2. Replace recursive per-package compilation with scheduling over the global
   cross-package module graph; cycle discovery is global now, but compilation
   batches are still package-local.
3. Perform the final two-scope whole-port review and address confirmed findings.
4. Replace or supplement polling with a production-grade native event backend
   and evaluate supported-platform packaging and behavior.
