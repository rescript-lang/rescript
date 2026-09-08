# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

The complete applicable canonical `rewatch/tests` suite now passes with the
experimental `rescript_ocaml.exe`. Milestone 6 remains open for the broader
configuration/platform inventory, performance and resource measurements, and
final whole-port review. OpenTelemetry parity is explicitly excluded by project
decision; ordinary verbosity and diagnostics remain in scope. Incremental state
uses existing AST, CMI, CMT, and generated-output artifacts rather than
in-process compiler state.

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

The clean-build path now prepares all packages before launching compiler work,
parses dirty sources as one global batch, emits namespaces as one global batch,
and schedules compilation over one cross-package dependency graph using
critical-path priorities. It still reconstructs its in-memory state for every
invocation, while Rust persists richer compile state. Rust also has native
filesystem events, diagnostic persistence, telemetry, and broader
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

The pinned Rust algorithms remain the default reference. Confirmed Rust bugs or
obvious low-risk inefficiencies may be corrected rather than copied, but every
intentional divergence must be recorded here and backed by a focused regression
or measurement. The first recorded divergence is Windows lock probing: failure
to launch `tasklist` is treated as inconclusive/live, preserving the lock,
instead of allowing an internal subprocess-launch exception to escape. This is
the same conservative result Rust intends for an unsuccessful probe.
Two configuration validations also intentionally improve Rust failure modes.
`namespace-entry` without an enabled namespace is rejected instead of being
silently ignored. Unsupported JSX versions are rejected as configuration
errors; Rust accepts them until compiler-argument construction and then panics.
Focused configuration tests protect both validations.
Compiler flag strings are also split without retaining empty arguments from
leading, trailing, or repeated spaces. Rust currently preserves those empty
argv elements, which can make an otherwise valid `bsc` invocation fail; the
OCaml behavior is the low-risk normalization intended by a flag-list decoder,
and a focused configuration test records the difference.
Likewise, an empty array entry in `ppx-flags` is ignored rather than indexing
its nonexistent first element and panicking as Rust's source filter does.

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
- Independent parser/compiler jobs use a CPU-bounded dynamic scheduler that
  refills each freed slot immediately, with private output files and
  deterministic input-order diagnostic collection. Their transient logs are
  created in the operating system's temporary directory; interruption signals
  all children, performs a bounded graceful reap, then escalates and cleans
  logs.
- Subprocess creation uses `spawn >= v0.17.0`: Unix children receive their own
  process groups, while Windows uses `CreateProcess` with explicit working
  directories. Bare executables resolve through PATH/PATHEXT, including
  `cmd.exe` dispatch for batch shims; lookup skips directories and non-executable
  Unix files. Private output-capture files use the operating system's temporary
  directory rather than the project tree. Portable self-executable tests cover
  scheduling without `/bin/sh`.
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
  external linked packages untouched. The benchmark harness creates fully
  isolated copies of the full fixture, its external Belt/runtime targets, and
  every installed `node_modules` tree, so the two implementations cannot share
  or inherit generated artifacts.
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
- The focused feature-dependency monorepo additionally covers per-consumer
  feature unions, dependency-local transitive expansion, explicit empty
  selections, and exclusion of dev-only requests under `--prod`.
- The canonical UTF-8 warning test passes, and a focused failure check verifies
  that `.compiler.log` contains the compiler error and `#Done` without ANSI
  escape sequences.
- Subprocess captures are normalized with lossy UTF-8 decoding before results
  reach either the sequential or parallel build paths. This matches Rust when
  a compiler code frame truncates a multi-byte character; direct tests also
  mirror the critical external-warning filter for LF and Windows CRLF streams.
- Unknown top-level configuration fields emit an explicit warning and are
  ignored, matching Rust rewatch's forward-compatible configuration behavior.
- Unknown nested fields now follow Rust's decoder boundaries as well. Fields
  inside `warnings`, `jsx`, `gentypeconfig`, and `js-post-build` use the same
  `parent.?.field` path form; fields hidden by Rust's untagged/custom decoders
  (`sources`, `package-specs`, and `sourceMap`) remain silent. These boundaries
  were confirmed against the pinned Rust executable and have a dedicated
  `config_tests.ml` regression test.
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
- Non-local dependencies now compile without their own warning configuration
  or the root CLI warning override, matching Rust. This matters for
  `warnings.error`: suppressing external warning text alone was insufficient,
  because passing `-warn-error` could still fail the dependency build. The
  focused external-boundary fixture now exercises this end to end.
- Deprecations in non-local packages are still reported, as in Rust, while
  unsupported and unknown fields remain local-only. When available, the
  diagnostic includes the package's `bugs` URL or an issues URL derived from
  `repository`; the precedence and URL forms have focused tests in
  `package_metadata_tests.ml`.
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
- Legacy array-form GenType shims now match Rust's map semantics: whitespace
  around the first `=` is trimmed, later duplicate source names win, and the
  emitted compiler arguments are sorted by source name.
- Source objects accept arbitrary string values for `type`, as Rust's Serde
  schema does; only the exact value `"dev"` marks the source as development
  code. Non-string values remain configuration errors. When explicit `subdirs`
  are flattened, the parent source type is propagated through the subtree just
  as in Rust, rather than allowing nested source types to override it.
- `source_tests.ml` exercises development-source filtering across exact,
  shorthand, mixed, and recursive directories, plus tagged and untagged
  feature selection and leaf features without a declaration map.
- All legacy top-level fields that Rust classifies as known but unsupported
  (`ignored-dirs`, generators, preprocessor/entry fields, and external include
  paths) receive the dedicated unsupported-field diagnostic rather than a
  generic unknown-field warning or silent acceptance.
- The CLI now uses Cmdliner declarations instead of a bespoke option parser.
  Focused tests, kept in a separate `cli_tests.ml`, cover implicit builds,
  command and global help/version placement, verbosity placement, build-only
  `-n`/`--no-timing` boolean forms, `--`-delimited option-looking folders,
  per-command flags, early regular-expression validation, feature parsing, and
  order-independent format input conflicts. Format stdin accepts only `.res`
  and `.resi`, matching Rust's enumerated argument; raw non-UTF-8 arguments are
  rejected before Cmdliner parsing, matching clap's string-argument
  validation. A small routing adapter is
  retained because Cmdliner treats a leading positional argument as a
  subcommand and parses subcommands before options; it inserts the implicit
  `build` command and preserves clap's global flag behavior without parsing
  command options itself.
- A missing project folder is rejected before path canonicalization with Rust's
  user-facing preflight diagnostic instead of leaking an OCaml `Unix_error`;
  the focused runner checks the complete path-bearing message.
- Linux and macOS npm platform packages include the experimental executable as
  `rescript-ocaml.exe`, and the root package exposes it through a separate
  `rescript-ocaml` launcher while retaining Rust rewatch as `rescript`. The
  artifact manifest includes the launcher and its shared signal-forwarding
  helper. Non-Windows CI runs the OCaml unit, focused, and complete canonical
  rewatch suites against the packaged executable and repeats the canonical
  suite through the installed package. Windows keeps running that suite against
  Rust until the native OCaml binary is ready rather than publishing an
  unverified executable.

## Performance and equivalence gate

[`bench/performance_gate.sh`](bench/performance_gate.sh) is the maintained
clean-build quality gate; [`bench/README.md`](bench/README.md) documents its
prerequisites, command line, scope, and exclusions. It archives a fully isolated
fixture for each implementation, warms both implementations, interleaves at
least five measured builds, samples summed process-tree RSS from `/proc`, and
records the commit and host. It then uses `strace` to compare exact
package/phase/input work multisets for clean, unchanged, and single-edit builds,
and recreates a third fixture at the same
absolute path for each runner before comparing generated JavaScript, `.cmi`,
`.cmj`, and `.mlmap`
manifests. Recreating that tree is essential: `clean` alone could leave a
Rust-only artifact for the OCaml build to inherit and mask a parity failure.

Clean-build performance is a completion gate, not just a reported metric. The
current acceptance threshold is a median wall time and peak process-tree RSS no
worse than 1.25× Rust on the full fixture, using at least five interleaved
post-warm-up runs with the same compiler and runtime. Passing the ratio is not
sufficient on its own: the compiler-work tuple and selected artifact manifests
must also be identical, and the canonical/focused integration tests remain the
behavioral-equivalence gate.

The latest five-run release-build measurement was made in the Linux Docker
environment on the plugged-in Mac host:

| Implementation | Median wall time | Median peak tree RSS |
| --- | ---: | ---: |
| Rust | 4,454 ms | 600,280 KiB |
| OCaml | 5,596 ms | 606,244 KiB |

The 1.256× wall-time ratio narrowly fails the 1.25× gate; RSS passes at 1.010×.
An earlier isolated run was 1.273×, so global scheduling and subprocess-capture
changes improved the result, but no completion claim is warranted yet. Docker
on a Mac is still a noisier platform than native Linux or dedicated CI even
when plugged in, so final acceptance should repeat the distribution on a stable
host rather than treating this single five-run set as universal.

Both implementations performed exactly 1,031 `bsc` launches: 512 parses, 7
namespace compilations, and 512 module compilations, of which 40 were interface
compilations; each also launched the PPX once. This rules out extra compiler
invocations on clean builds as the current wall-time source. The extended work
gate also measures incremental orchestration. Its latest correctness smoke run
reported identical work in every scenario:

| Scenario | Rust `bsc` launches | OCaml `bsc` launches |
| --- | ---: | ---: |
| Clean | 1,031 | 1,031 |
| Unchanged | 4 | 4 |
| Single leaf edit | 6 | 6 |

The normalized package/phase/input manifests also match in every row. The first
extended run exposed seven unconditional OCaml namespace compilations and two
case-sensitive artifact-name false misses on both incremental paths. Namespace
maps are now rewritten/compiled only when their contents, package modules, or
outputs require it, and global graph keys are no longer used as case-sensitive
on-disk artifact names. The rerun closed both differences. The hardened
fixture-recreation
check also passed: both implementations performed the same normalized
package/phase/input work and produced identical selected artifact sets and
contents without inheriting files from one another. Its latest one-run timing
sample was 13,932 ms / 621,948 KiB for Rust and 15,851 ms / 645,312 KiB for
OCaml. That 1.138× sample is useful only as a correctness smoke test and does
not replace the five-run performance result; its much higher absolute times
also illustrate why a single run is not an acceptance measurement.

The harness now respects an explicitly paired `RESCRIPT_BSC_EXE` and
`RESCRIPT_RUNTIME` and classifies compiler work by that exact executable path,
rather than assuming the binary is named `bsc.exe`. This closed a false-positive
case where a locally built `rescript_compiler_main.exe` produced matching zero
counts. A corrected smoke run again measured exactly 1,031 matching compiler
launches and identical artifacts. Concurrent work on the Docker host made the
September 8 timing samples too variable for acceptance, so their wall-time
ratios are intentionally not recorded as a replacement gate result.

The remaining measured gap is therefore orchestration overhead around the same
external compiler work: process launch/wait/capture, artifact publication, and
repeated filesystem/configuration work are the main candidates. Capture files
are opened once in the OS temporary directory and empty captures avoid a second
open. Pipe-based capture remains the intended final backend so successful builds
do not create transient files, but it is deferred until the scheduler lifecycle
is settled because it requires concurrent draining, bounded memory, and reliable
descriptor/descendant cleanup on Windows as well as Unix.
Once pipes are in place, the benchmark plan adds a normalized Linux `%file`
syscall trace for clean, unchanged, and single-edit builds. It will compare
fixture-local path/operation multisets and repeated accesses, while reporting
runtime/loader/toolchain calls separately rather than treating incomparable raw
process-wide syscall totals as a quality metric.

The current `cloc` 2.06 source-size snapshot reports 7,818 Rust production
lines after excluding the intentionally omitted telemetry module and inline
test-only sections, versus 3,789 OCaml production lines, or 48.5%. Counting
language-specific tests separately gives 2,773 embedded Rust unit-test lines
and 975 OCaml test lines (557 unit-test + 418 tracked focused-test harness,
fixture, and configuration lines); the OCaml benchmark tooling adds another
314 lines, including the source-size script itself. The shared canonical
integration suite is deliberately not charged to either side. These figures
describe maintainability surface, not parity or quality: this port is still
incomplete, and later comments and tests should increase useful lines.
[`bench/source_size.sh`](bench/source_size.sh) preserves the scope and command;
rerun it for the final maintainability review alongside maximum module size.

## Known gaps

- Incremental state currently relies on artifact timestamps, byte-identical CMI
  publication, and in-memory warning state during watch. Rust's richer
  compile-state model is not otherwise ported.
- Full configuration validation parity, performance parity, and
  production-grade filesystem watching remain incomplete.
- Full validation coverage is now an explicit source-inventory gate in
  `PARITY_CHECKLIST.md`: every user-reachable Rust guard must map to an OCaml
  location and test or to a documented intentional divergence. Existing suite
  coverage alone does not close that gate.
- Rust unit-test scenario coverage is tracked separately from source guards.
  `tests/check_rust_test_coverage.sh` currently inventories all 136 Rust unit
  tests and validates their exact entries in `tests/rust_test_coverage.tsv`;
  all 136 scenarios have now been reviewed, with no confirmed gaps or
  unreviewed entries. The `--require-complete` mode is a
  final quality gate and fails for either
  unreviewed scenarios or confirmed coverage gaps.
- Canonical integration-test inclusion is mechanically checked as well.
  `tests/check_canonical_test_coverage.sh` inventories all 48 shell tests below
  `rewatch/tests` and requires `suite.sh` to reference every one exactly once,
  without stale entries. CI runs this check and the strict Rust unit-test check
  before exercising that same shared suite against the packaged OCaml binary.
  This prevents suite-routing drift; the broader source-validation and output
  inventories remain separate gates for behavior Rust does not currently test.
- Focused locking tests now hold a compiler behind an explicit release marker,
  verify that `build.lock` remains present, start a second build, observe it
  waiting, and then verify both builds complete and release the lock. Failure
  and interrupt paths also assert cleanup, and the runner terminates registered
  background processes during test cleanup.
- Per-package `compiler-info.json` fingerprints now invalidate `lib/bs` and
  `lib/ocaml` when the compiler path or contents, runtime path, package config,
  or effective root source-map arguments change. Paths are constructed with
  `Filename`, and cleanup does not follow directory symlinks. The OCaml file
  uses the standard-library content digest rather than Rust's BLAKE3 because
  this is an internal change detector, not a shared cache key. Unlike Rust, an
  unchanged fingerprint is not rewritten on every successful build; focused
  tests cover both this intentional efficiency improvement and invalidation.
- Watch builds retain compiler warnings in memory by package-relative source
  path, replay implementation and interface warnings in deterministic module
  order, and discard entries when a path changes or recompiles cleanly. A
  compiler-call-count regression proves that editing an unrelated module does
  not recompile the warning module. That test also exposed that `bsc` gives AST
  outputs epoch mtimes; freshness now uses the published `lib/ocaml` AST copy,
  avoiding a full-project reparse on each watch cycle. Both canonical warning
  persistence tests, including atomic saves, pass with this state.
- Configuration decoding now mirrors Serde's `Option` treatment of JSON `null`
  at every optional top-level and nested field audited. A 41-case differential
  acceptance run found no remaining mismatch, and focused tests retain the
  covered field inventory. Unsupported `ignored-dirs` is diagnosed but no
  longer honored, matching Rust rather than silently omitting source files;
  `jsx.v3-dependencies` is decoded as a string array even though its value is
  not otherwise used by this build system.
- Duplicate keys now follow the reference decoder's two distinct rules:
  typed configuration structs reject repeated known fields, while JSON-map
  backed values retain the last occurrence. Differential acceptance covered
  15 representative struct/map cases; focused tests also verify last-value
  semantics for source maps, features, experimental flags, and GenType debug
  maps. Repeated unknown fields remain accepted, as in Rust.
- Redirected warnings are persisted to compiler logs during scheduling but
  presented during final reporting, after the build summary and before config
  diagnostics. This matches Rust's deterministic snapshot order without
  delaying failure detection; the complete canonical suite protects it.
- Parser and compiler arguments now follow Rust's phase-specific ordering, and
  `compiler-args` reports the parser's actual path relative to `lib/bs`.
  PPXs are owned by parsing only: known GraphQL, Spice, Relay, Formality, and
  Bisect PPXs are filtered using the same source markers/environment rule as
  Rust. Unit tests cover every filter branch, while a focused build proves a
  filtered missing PPX is not resolved or launched.
- Interactive completion now uses the Rust status text, warning suffix,
  two-decimal timing, and clean/warning emoji after verifying that both output
  streams are terminals. `--no-timing` is threaded into the build instead of
  being parsed and discarded, and the clear-screen predicate is separately
  tested for interactive and redirected output. This closes the Rust unit-test
  inventory; it does not close the broader spinner/phase presentation gate.
- Interactive output parity remains open. The OCaml executable now selects a
  TTY-specific final status with timing and emoji and supports watch
  clear-screen behavior, but does not yet reproduce Rust's phase-by-phase
  parsing/compilation spinner, progress counts, or complete verbosity behavior.
  Plain redirected output and pseudo-terminal output are tracked as distinct
  gates in `PARITY_CHECKLIST.md`.
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
  then workspace-sibling locations. The benchmark fixture copier preserves all
  of those ignored dependency trees in isolated roots; the smaller tracked
  monorepo fixture remains preferable for ordinary integration tests.
- Windows support is required before this port can be considered complete. It
  cannot be executed in the current Linux environment, but it must still be
  designed and cross-built where possible. Subprocess creation now uses the
  cross-platform `spawn` library (`CreateProcess` on Windows), including child
  working directories and PATH/PATHEXT resolution. Windows cleanup uses
  `taskkill /T` for compiler/helper trees (with a direct-PID fallback), while
  Unix retains process-group cleanup. Watch lock/process
  probing and polling behavior still need a Windows cross-build and runtime
  verification. Shared filesystem logic uses `Filename` operations rather than
  embedded `/` or `\\` separators; Unix-only test cases are being isolated or
  replaced with portable helpers.
- The preferred non-CI Windows validation environment is a Windows 11 ARM VM on
  the Apple Silicon development host, with the repository on the guest's local
  NTFS volume. Run the existing Bash suites in the Cygwin environment supplied
  by the native Windows OCaml/opam toolchain; the canonical helpers already
  detect Cygwin/MSYS and normalize Windows paths. The smaller OCaml-focused
  runner may only need explicit `cygpath` conversion for absolute paths passed
  through custom environment variables. An occasional native x64 Windows run
  should remain the release-confidence check. WSL exercises the Unix backend,
  and Wine does not faithfully validate NTFS events or Windows process-tree
  behavior.
- A static `platform.mli` now defines the common platform contract, and Dune
  selects either `platform_unix.ml` or `platform_windows.ml` as `platform.ml`
  using `%{os_type}`. Process-tree termination, PID probing, executable lookup,
  subprocess creation, signal deferral, post-build shell invocation, and path
  comparison are behind that boundary. The unselected Windows implementation
  is also type-checked against the contract in Linux unit builds. Pipe
  descriptor ownership and a future native watcher backend belong behind the
  same boundary; actual Windows cross-build/runtime verification remains open.
- Native Windows implementation and runtime validation are deliberately an
  end-stage milestone that can be completed by a separate Codex session inside
  the Windows VM. Until that handoff, every increment must keep Windows in its
  design constraints: shared code must use `Filename` rather than literal
  separators, avoid Unix shell/process/signal assumptions, route genuinely
  platform-specific capabilities through `platform.mli`, retain a type-checked
  Windows implementation, and accept only dependencies with credible native
  Windows support. The handoff must identify the exact commit, setup and test
  commands, expected results, unverified behaviors, and platform-sensitive
  scenarios so the Windows session can continue without reconstructing history.

## Dependency decisions

- `spawn` is accepted: it is a narrow, MIT-licensed Jane Street package with
  explicit Linux, macOS, and Windows support. It replaces bespoke fork/exec/cwd
  code and materially reduces process-launch risk.
- OpenTelemetry is intentionally omitted from the OCaml port by project
  decision. Adding an OTLP exporter, span stack, and shutdown lifecycle would
  introduce substantial optional machinery and dependencies; this does not
  relax ordinary verbosity, diagnostic, or exit-status compatibility.
- `Cmdliner` is accepted for the CLI. It is actively maintained (2.1.1 was
  released in April 2026), ISC-licensed, has no runtime package dependencies,
  supports OCaml 4.08 and newer, and replaces the hand-written option parser.
  Help rendering intentionally uses Cmdliner's man-page structure rather than
  reproducing clap's whitespace and headings. This presentation difference is
  accepted; command and option discoverability, command selection, validation,
  and exit classes remain compatibility requirements and are tested
  independently.
- JSON deriving is not currently justified. The config loader must retain raw
  keys to distinguish deprecated, known-unsupported, and forward-compatible
  unknown fields; generated codecs would still require substantial custom
  validation around the derived layer.
- No watcher binding is accepted yet. A libuv binding could provide native
  Windows/macOS/Linux events, but it adds a vendored C library plus ctypes
  dependencies and its current maintenance cadence must be established before
  adoption. Polling remains the fallback while this is evaluated.

## Next actions

1. Finish the source-level validation inventory and the per-Rust-unit-test
   coverage review, closing confirmed configuration/CLI gaps; OpenTelemetry is
   an explicitly documented non-goal.
2. Profile and close the remaining clean-build wall-time gap while preserving
   exact compiler-work and artifact equivalence; retain pipe capture as an
   end-stage option.
3. Continue splitting `build.ml` along stable responsibility boundaries. The
   filesystem and artifact-ownership layer now lives in `build_artifacts.ml`;
   package preparation/scheduling and watch lifecycle remain candidates.
4. Perform the final two-scope whole-port review and address confirmed findings.
5. Replace or supplement polling with a production-grade native event backend
   and evaluate supported-platform behavior. Experimental Linux/macOS package
   distribution and CI exercise are already in place.
6. At the final maintainability pass, add comments around ownership,
   concurrency, platform, and algorithmic invariants that are not apparent from
   the code itself; avoid comments that only paraphrase individual statements.
7. Prepare the pinned Windows handoff, then finish the Windows watcher/lock
   backend and path audit and run the native build, unit, focused, and canonical
   Bash suites in the VM. Address findings there and finish with an x64 Windows
   confidence run where available.
