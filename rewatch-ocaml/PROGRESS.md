# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

The complete applicable canonical `rewatch/tests` suite now passes with the
experimental `rescript_ocaml.exe`. Milestone 6 remains open for native platform
validation, final performance/resource confirmation, maintainability cleanup,
and the final whole-port review. The configuration, Rust-source validation,
Rust-test, control-file, and ordinary redirected-output inventories are
complete. OpenTelemetry parity is explicitly excluded by project decision;
ordinary verbosity and diagnostics remain in scope. Incremental state uses
existing AST, CMI, CMT, and generated-output artifacts rather than in-process
compiler state.

At code checkpoint `324112908`, `opam exec -- make test-all` passed
uninterrupted with the packaged OCaml rewatch binary as the default. This
covered formatting, roughly 300 OCaml unit assertions (now grouped into 17
OUnit2 suites), compiler/runtime and build integration tests, runtime
docstrings, both GenType suites, analysis and reanalyze, tools, and the complete
canonical rewatch suite. The run left no watcher process or worktree change
behind.

After the project-context, compiler-log, and compiler-argument module splits at
checkpoint `d5ed6acc8`, the complete 48-test canonical `rewatch/tests` suite
passed again with `_build/default/rewatch-ocaml/rescript_ocaml.exe`. The run
covered clean/build, diagnostics, incremental edits, watcher lifecycle, locks,
formatting, features, and compiler arguments; it restored `rewatch/testrepo`
and left no watcher process behind.

The implementation currently has configuration loading, source and package
discovery, external `bsc` parsing, AST dependency extraction, cycle detection,
dependency-ordered compilation, interface-before-implementation compilation,
bounded concurrent external `bsc` execution, feature selection, artifact
cleanup, and compiler artifact publication to `lib/bs` and `lib/ocaml`.

Project/workspace path policy now has an explicit `project_context.ml` owner
instead of remaining embedded in `build.ml`. It owns workspace-root selection,
ancestor/hoisted/sibling dependency lookup, dependency-config preflight,
canonical locality classification, and project-relative presentation. `Format`
and the OUnit2 project-context tests use that owner directly; `Build.Error` and
`Build.Package_error` remain exception aliases so command exit classes and
existing callers retain their contract. This removes roughly one hundred lines
from `build.ml` without adding a facade layer or changing filesystem work.

Compiler-log lifecycle now lives in `compiler_log.ml`, matching Rust's
`build/logs.rs` responsibility. The module owns log paths, initialization,
append/finalization, ANSI stripping, and publication from `lib/bs` to
`lib/ocaml`; external-warning selection remains in compilation, where Rust also
keeps it. This removes another fifty lines from `build.ml` and gives the
filesystem-writing surface a four-function interface.

Compiler argument policy now lives in `compiler_args.ml` behind a focused
interface. It owns source-dependent PPX filtering, parser-versus-compiler flag
ordering, development source-map selection, external warning policy, namespace
arguments, package-output encoding, and GenType dependency arguments. Build job
construction consumes those values but still owns subprocess execution and
artifact publication. The existing argument-order and PPX tests now target the
policy module directly.

Build and watch lock lifecycle now lives in `build_lock.ml`, matching Rust's
`lock.rs` responsibility. PID validation, stale-lock takeover, active-process
checks, ownership polling, and release no longer have separate copies inside
the build and watch paths. Process probing remains behind `platform.mli`; the
atomic hard-link primitive is centralized here so native Windows validation can
either approve it or replace it through the same platform boundary.

Lock acquisition and use are callback-scoped so cleanup ownership is installed
before deferred termination signals can be delivered. Candidate files are
atomically created and opened, flushed before publication, removed before build
or watch work begins, and cleaned on every synchronous or asynchronous failure
path. Final-lock removal reports sharing and permission errors instead of
mistaking them for a missing lock; the optional early release used before
post-build hooks is immediate and idempotent. The warning-free build, all 19
OUnit2 groups, focused integration runner, and 74-case differential command
validation pass with these invariants, including candidate-cleanup checks after
failed acquisition.

Dependency-aware compiler dispatch now lives in `compiler_scheduler.ml`. Its
abstract scheduled-module type owns the interface-before-implementation phase
machine, parallel dependency scheduling, deterministic failure aggregation,
warning persistence, CMI digest comparison, reverse-dependent invalidation,
and guaranteed phase cleanup. `build.ml` still decides which modules are dirty
and constructs compilation/publication callbacks, but cannot mutate the
scheduler's transient phase or message state directly.

The extraction passed the 17 OUnit2 suites, the focused integration runner,
the 69-case Rust/OCaml command-validation gate, `dune build @all`, and the
complete canonical suite through its final compiler-argument check. The suite
left no watcher, lock, or testrepo change behind. An independent review found
no concrete correctness, exception-identity, cleanup, warning-order,
invalidation, phase-ordering, API, or portability issue.

Watch lifecycle now lives in `watcher.ml`, matching the responsibility of
Rust's `watcher.rs` while retaining `native_watcher.ml` as the narrow libuv
boundary. It owns local-package watch-root discovery, cached content snapshots,
native-event reconciliation and handle refresh, polling fallback, edits that
arrive during a build, terminal clearing, signal deferral, and watch-lock
cleanup. `build.ml` now supplies only a rebuild callback with the persistent
warning state and build-specific diagnostic handling.

The watch resource review found two lifecycle bugs that predated the split:
the lock release protection began only after setup, and process-global signal
handlers were never restored. Lock ownership is now protected immediately
after acquisition, while SIGINT/SIGTERM handlers are scoped inside that owner
and restore their previous dispositions on callback failure, signal exit, and
normal lock-loss shutdown. A focused OUnit2 lifecycle test covers exceptional
and normal return, lock cleanup, and both restored signal dispositions.
Independent behavioral review found no extraction regression; focused resource
follow-up confirmed the lock, native-handle, signal-restoration, and cleanup
ordering after the fixes.

The `compiler-args` command implementation now lives in
`compiler_args_command.ml`. It owns source/config discovery, workspace option
inheritance, runtime and dependency include resolution, and the final JSON
projection, while `compiler_args.ml` remains the shared argument-policy owner.
`Build.compiler_args` is retained as a compatibility alias for existing callers.

External compiler work now lives in `compiler_process.ml`. It owns parser and
compiler job construction, AST dependency decoding, namespace map compilation,
external-warning selection, compiler-artifact publication, watch-output
staging, and per-output JavaScript post-build hooks. The scheduler remains
process-agnostic, and `build.ml` supplies package state and callbacks without
creating unused parse/compile tuple payloads. Generic substring assertions also
moved out of production `Build` into the OUnit2-only `test_support.ml`.
Independent review found no argument-order, AST-decoding, namespace,
publication, warning, hook, staging, exception-identity, or API regression.

Recursive explicit cleaning now lives in `clean.ml` over `build_artifacts.ml`
and `file_util.ml`. It owns package traversal, consumer-versus-
independent dependency ownership, generated-output and watch-sidecar removal,
and compiler-tree deletion; `Build.clean` retains only project/lock/progress
command policy. Shared package warnings, missing-source reporting, and
package.json-name validation now live in `package_diagnostics.ml` and are used
consistently by build, clean, and format. Test teardown uses the shared
`File_util.remove_tree`; clean's stricter command-local deletion helper
remains private rather than creating a second ambiguous filesystem API.
Independent review found no behavior regression; its sole API-ownership finding
was resolved by making that command-local helper private.

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
- Namespace maps included punctuated/exotic source module names that Rust
  deliberately filters because they are not valid map identifiers. The shared
  ASCII predicate now excludes them before `.mlmap` generation; a differential
  build compares the complete map while focused unit cases retain accepted,
  punctuated, and empty names.
- Source filters were applied to package-relative paths rather than basenames,
  so a directory-only regex selected files that Rust excludes. Discovery now
  applies the regex to `Filename.basename`; differential builds retain both a
  directory-only non-match and a basename match, with focused source tests for
  the same boundary. The engines are not otherwise syntax-compatible: Rust
  uses the `regex` crate, while the port currently uses OCaml's `Str`. Their
  common basic forms (for example `^Foo.*\.res$`) work, but Rust expressions
  such as `Foo|Bar`, `\d+`, or `(?:Foo|Bar)` do not have the same meaning in
  `Str`, whose alternation and grouping operators use its older escaped syntax.
  This is a documented CLI compatibility gap rather than a new watcher
  requirement.
- PPX resolution did not search hoisted `node_modules`.
- Stale cleanup treated every JavaScript-looking file as owned output and
  deleted checked-in legacy files that had no corresponding source or AST.
- Standalone package builds refused to build dependencies resolved outside the
  invoked package directory.
- Packages without a `sources` field lost Rust's non-root-package warning, and
  implicit `format` selected the right local files but did not validate the
  complete dependency graph first. Field presence is now retained separately
  from an explicit empty source list; build, clean, and format emit the exact
  warning, while format also rejects missing or malformed dependencies and
  retains Rust's first-path duplicate-package selection and warning.
- Feature-map cycle validation ran even when all features were implicitly
  active, while implicit `format` did not validate specifically requested
  dependency feature closures at all. Feature implications are now traversed
  only for restricted selections, as in Rust; format aggregates all applicable
  consumer requests before deciding between all-features and a restricted
  union. It then scans that same resolved package graph once, retaining missing
  source-folder diagnostics from installed dependencies and using the effective
  dependency features when selecting files from local packages. Differential
  cases retain acceptance of an irrelevant cycle, exact rejection of a
  requested cycle, the installed-package diagnostic, and the local-package file
  set.

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
For `compiler-args`, a missing regular dependency is reported as a contextual
command error instead of triggering Rust's `Expected to find dependent package`
panic. Missing development dependencies remain optional, matching Rust.
Dependency permission validation also considers only graph edges that are
actually traversed. Rust correctly omits an installed package's
`dev-dependencies` from package discovery, but its later permission pass checks
such a dormant edge when the target package happens to be reachable elsewhere.
That makes the same installed package conditionally break an unrelated root
build. A differential case retains Rust's rejection and the port's successful
active-edge-only behavior.
The same permission pass reports every denied active edge. Rust's helper stops
after the first denied dependency in each regular/dev class, emits that detail
at info level on stdout (so `-q` hides it), and recommends the obsolete
`unallowed_dependents` field in `config.json`. The port keeps the complete
diagnostic on stderr and names `allowed-dependents` in `rescript.json`; a
two-dependency differential fixture retains both the Rust behavior and this
deliberate correction.
Watch filtering keeps the same positive regex meaning during initial discovery
and later changes. Rust's package discovery includes matching files, but
`watcher.rs::matches_filter` negates the regex for events, so an included-file
edit is ignored while an excluded-file edit wakes the watcher. A synchronized
differential watch case uses successful hook markers rather than sleeps: it
proves Rust completes the excluded-file wakeup with stale included output and
that OCaml rebuilds the included output directly. The port's consistent
positive semantics are retained as a Rust bug fix.

### Rust panic follow-ups

These malformed-input paths should be considered for fixes in the Rust
implementation as well. The OCaml behavior and focused tests provide the
expected non-panicking result:

- `config.rs`: `Config::get_jsx_args` explicitly panics for every integer
  `jsx.version` other than `4`. For example, `{"jsx":{"version":3}}` is
  accepted by Serde and panics later during argument construction. Rust should
  reject it as a contextual configuration error; `config_tests.ml` exercises
  that result in the port.
- `build/parse.rs`: `filter_ppx_flags` calls `first().unwrap()` for an
  array-form PPX entry. A configuration such as `{"ppx-flags":[[]]}` therefore
  panics when a source is filtered. Rust should either reject the empty command
  during configuration decoding or safely omit it; the port omits it and tests
  the filter directly in `config_tests.ml`.
- `build/compile.rs`: `get_dependency_args` explicitly panics when a regular
  dependency cannot be resolved, including through `compiler-args`. Rust should
  return the same package/dependency context as a normal command error. The
  port retains the existing message context and covers it in
  `compiler_args_tests.ml`; unresolved development dependencies remain optional.
- `build.rs`: `get_compiler_args` calls `expect("Couldn't find package root")`
  when a readable source has no ancestor configuration. Rust should return a
  normal project-discovery error; the differential command-validation gate
  retains the current panic and the port's non-panicking rejection.
- `helpers.rs`: `read_file` calls `File::open(...).expect("file not found")`.
  This is reachable both when `compiler-args` names a missing source below a
  valid project and when a discovered source disappears before its parser
  worker reads it. Rust should propagate the path-bearing I/O error. The
  differential command-validation gate retains exit 101 for both Rust paths
  and normal contextual OCaml errors.
- `helpers.rs`: `get_bsc` canonicalizes the selected compiler path with
  `expect`. A stale or misspelled `RESCRIPT_BSC_EXE` therefore panics before a
  build starts. Rust should return a normal toolchain-discovery error containing
  the selected path; the command-validation gate covers the current Rust panic
  and the port's contextual rejection.
- `build/read_compile_state.rs`: dependency packages are keyed by the requested
  dependency name, but `make_package` tags their modules with the preferred
  `package.json.name`. When that metadata name differs from the matching
  `rescript.json.name`, the later package lookup returns `None` and is
  unwrapped. Rust should retain one consistent dependency identity after its
  existing mismatch warning, or reject the mismatch normally. The command gate
  reproduces the panic; the port consistently uses the ReScript dependency name
  and successfully compiles the same fixture.
- `build/parse.rs::generate_ast` adds a second `expect("Error reading file")`
  around `helpers::read_file`; a missing file actually panics first at the
  helper's inner `expect`, while another read error could reach this outer
  panic. `build/compile.rs` separately uses
  `expect("copying source file failed")` when a source disappears after `bsc`
  succeeds but before publication. The latter panic occurs on a worker thread
  before it sends its completion message, so the Rust scheduler then waits
  indefinitely. Both races should be ordinary path-bearing build errors. The
  port's top-level `Sys_error`/`Unix_error` and compiler-error handlers provide
  that failure class; differential compiler wrappers deterministically delete
  sources before later parse work and during publication, bound the Rust hang,
  and check the OCaml error paths.
- `build/deps.rs::get_dep_modules` panics when a successfully generated AST
  disappears before dependency extraction. Rust should propagate an ordinary
  path-bearing build error. A second compiler wrapper deterministically removes
  the AST after `bsc` returns; the command-validation gate retains Rust's exit
  101 and OCaml's normal rejection.
- `build/compile.rs::compile_file` ignores failed CMT/CMTI publication because
  `-bs-no-bin-annot` legitimately suppresses those debug artifacts. OCaml had
  treated every successful implementation compile as requiring a CMT, which
  broke the repository's `mario_game.res` during `make test-all`. CMT and CMTI
  publication are now optional while CMI and CMJ remain required. A focused
  fresh build retains the no-bin-annotation case and the full `tests/tests`
  package now builds successfully through the packaged OCaml executable.
- `watcher.rs` unwraps `initialize_build` during a full rebuild. An editor save
  that temporarily makes `rescript.json` invalid therefore panics and exits the
  Rust watcher. The port reports the parse error and keeps its event loop alive;
  the differential lifecycle gate restores a valid config with a new output
  suffix and requires the OCaml watcher to produce it without restarting.
- `cmd.rs::run` indexes the first whitespace-split command and unwraps process
  creation, so an empty `--after-build` value or a missing executable panics.
  It also waits for a launched hook but discards its exit status, allowing a
  failing hook to leave `rescript build` successful. The port rejects all three
  normally, includes the command or exit status and captured output in the
  diagnostic, and releases the build lock before starting the hook as before.
  Differential command cases retain Rust's two panics and ignored exit status
  alongside the intended OCaml outcomes.

Fixing these in Rust is outside the OCaml-port changes themselves. If they are
fixed upstream, the differential configuration gate should be tightened from
semantic rejection to the corresponding normal error exit class where
applicable.

### Rust cleanup follow-up

- `helpers.rs`: `get_bs_compiler_asset` constructs a working-tree artifact as
  `format!("{basename}{extension}")`, omitting the dot before `cmi`, `cmj`,
  `cmt`, or `cmti`. `clean.rs::remove_compile_assets` consequently removes the
  published `lib/ocaml` artifact but permanently leaves the corresponding
  `lib/bs` artifact and copied source behind after a rename or deletion. The
  stale working CMI currently helps `bsc` retain Rust's source-located
  missing-module diagnostic while the dependent compiles. The OCaml port
  preserves that artifact shape only for the duration of the command and
  removes the working CMI in its outer finalizer on both success and failure.
  `tests/run.sh` asserts that neither working nor published stale CMI survives;
  the canonical internal and namespaced rename snapshots prove the diagnostic
  remains unchanged. Rust should add the missing dot and then make the
  diagnostic dependency explicit rather than relying on the accidental leak.
- Rust issue [#7728](https://github.com/rescript-lang/rescript/issues/7728)
  reports that restarting watch does not recreate a manually deleted generated
  JavaScript file. The port intentionally treats absence from its already
  collected public-output inventory as compile-dirty state. A focused restart
  test deletes an otherwise-current output and observes its recreation without
  adding a per-module filesystem probe.
- The port includes the package-output invalidation proposed in Rust PR
  [#8540](https://github.com/rescript-lang/rescript/pull/8540): every package's
  `compiler-info.json` fingerprints the root project's effective module format,
  output location, and resolved suffix. A mismatch removes outputs described by
  the previous fingerprint before rebuilding dependency compiler state. The
  focused test covers both the PR's changed-path migration and a stricter
  same-path ES-module-to-CommonJS change, which output existence alone cannot
  detect.

### Compatibility control artifacts

The external control-file inventory found one omitted compatibility artifact:
Rust writes an empty `lib/bs/build.ninja` to invalidate editor-tooling caches
after normal and structural builds. The port now writes the same marker after
successful and compiler-failing normal builds and after post-initial watch
rebuilds. The current snapshot watcher does not expose native event kinds, so
it conservatively rewrites the marker after content-only rebuilds too. That is
a documented over-invalidation and one extra file write, not a missing cache
invalidation. Focused success/failure builds and the recoverable config-change
watch case retain the behavior; isolated manifests confirm there are no other
missing control-file names.

## Verified

- Recursive directory creation now has `create_dir_all`-style race semantics:
  concurrent creators may observe `EEXIST` only when the winning path is a
  directory, while an existing non-directory remains an error. A 16-thread
  OUnit regression repeatedly creates the same nested tree and separately
  retains the non-directory failure boundary. Recursion also stops at a
  `Filename.dirname` fixed point so an unavailable Windows volume root returns
  its native filesystem error rather than overflowing the OCaml stack.
- `dune runtest tests/rewatch_ounit_tests` passes all 18 OUnit2 suites; the
  former `dune runtest rewatch-ocaml` scope forwards to the same suite so it
  cannot silently run zero tests.
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
- Compilation now snapshots each module's dirty flag before scheduling, as
  Rust does, instead of reevaluating filesystem-backed closures while other
  compiler jobs publish artifacts. A successful compile compares CMI contents,
  refreshes the shared compile-asset/module state, and dirties reverse
  dependents only when the CMI changed. Cycle-blocked modules remain blocked.
  Unit coverage, the focused stale-CMI lifecycle test, and the complete
  canonical suite cover these transitions.
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
- Format failures now retain Rust's user-facing context: invalid stdin is
  labeled `stdin` rather than exposing the OCaml temporary filename, file
  formatting invokes `bsc` before reading the original like Rust, and
  `--check` prints the same singular/plural summary before failing. Focused
  unit and integration tests cover the labels, summaries, and exit status.
  Implicit format scope also matches Rust's project boundary: the current
  directory itself must contain `rescript.json` or `bsconfig.json`; formatting
  from an arbitrary descendant does not silently select a parent project.
  Implicit format also shares the build graph's locality predicate, so ordinary
  installed packages below `node_modules` are not mistaken for symlink-local
  workspace packages and rewritten; focused filesystem coverage retains this
  boundary.
- Build and watch lock readers validate the complete serialized owner as a Rust
  `u32`. Malformed or partially written lock content is no longer classified as
  a dead owner and deleted: both commands reject it and preserve the file so an
  operator can resolve unknown ownership safely. The differential command gate
  covers both lock kinds, while focused unit tests retain the exact numeric
  boundary.
- Source discovery now rejects an implementation/interface pair whose relative
  path or basename casing differs before the extension, matching Rust instead
  of silently attaching the interface by capitalized module name. Focused tests
  cover both casing and cross-directory mismatches alongside duplicate-module
  handling.
- Package source discovery now applies Rust's locality rule independently of
  the root CLI mode: installed dependencies exclude `type: "dev"` source
  folders even during a normal development build, while root and symlink-local
  packages retain them unless `--prod` is selected. Clean traversal, global
  graph preparation, and fallback compilation share the same predicate.
- Missing active source folders retain Rust's non-fatal diagnostic for both
  local and installed packages, including the relative folder, package name,
  and package root. The differential gate compares the installed-package
  diagnostic byte for byte; excluded dev and feature folders remain unscanned.
- Explicit `clean` no longer recursively deletes the complete local `lib/es6`
  and `lib/js` trees. It removes configured source-derived JavaScript and maps,
  plus the wholly owned `lib/bs` and `lib/ocaml` compiler trees, matching Rust
  while preserving unrelated files (including manual JavaScript) beside
  out-of-source outputs. Exact configured outputs are removed from resolved
  installed dependencies as Rust does, but their neighboring unowned files are
  likewise preserved. A dedicated filesystem test retains both boundaries.
- A retained differential command-validation gate covers valid, missing, and
  non-ReScript `compiler-args` inputs; sources without a project; missing,
  config-less, and malformed build folders; and implicit format from below a
  project root. It distinguishes ordinary rejection from Rust panic exit 101,
  preserving two additional `compiler-args` panic candidates for an upstream
  Rust fix. It also records the deliberate OCaml extension check: Rust accepts
  an existing `.txt` even though the command documents `.res`/`.resi` only.
- Dependency package validation is shared by OCaml build-graph preparation and
  clean traversal. Missing packages, existing package directories without a
  ReScript config, and malformed dependency configs now terminate build and
  clean with Rust's package-tree exit class 2 instead of being skipped or
  reported as a generic exit 1; watch startup uses the same path. The command
  gate now has 26 cases and verifies failed OCaml commands leave neither build
  nor watch locks. Rust currently calls `process::exit(2)` from package-tree
  library code; OCaml raises a typed package error to the CLI so cleanup still
  runs before the matching exit status is returned.
- Independent parser/compiler jobs use a CPU-bounded dynamic scheduler that
  refills each freed slot immediately, with private output files and
  deterministic input-order diagnostic collection. Their transient logs are
  created in the operating system's temporary directory; interruption signals
  all children, performs a bounded graceful reap, then escalates and cleans
  logs. File formatting now reuses the same bounded scheduler for independent
  `bsc -format` subprocesses instead of serializing every file; a process-level
  OUnit regression requires two formatter children to overlap.
- Subprocess creation uses `spawn >= v0.17.0`: Unix children receive their own
  process groups, while Windows uses `CreateProcess` with explicit working
  directories. Bare executables resolve through PATH/PATHEXT, including
  `cmd.exe` dispatch for batch shims; lookup skips directories and non-executable
  Unix files. Private output-capture files use the operating system's temporary
  directory rather than the project tree. Portable self-executable tests cover
  scheduling without `/bin/sh`.
- `warnings`, `ppx-flags`, JSX v4, source-map, `LetUnwrap` experimental
  features, and `js-post-build` are projected into external compiler/process
  invocations. The post-build fixture verifies its generated-file argument, and
  a differential failure case requires both implementations to identify the
  generated JavaScript path when the configured command exits nonzero.
- `format`, `compiler-args`, `--filter`, and `--after-build` are implemented.
  The test runner covers stdin formatting, compiler-argument JSON, filtering,
  and a successful after-build assertion. The command-validation gate also
  covers empty, missing, and nonzero after-build commands; the port reports
  each as a contextual command error instead of reproducing Rust's panics or
  ignored failure status.
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
- The package-resolution source audit found that the port emitted Rust's
  duplicate-package warning but still traversed the later nested path, turning
  a valid first-path-wins build into a duplicate-module error. Graph preparation
  now resolves and reserves every package's direct dependency names before
  recursively visiting any of them. It retains one canonical `(root, config)`
  for each name and reuses it after warning, matching `build/packages.rs` even
  when an earlier dependency has a nested copy of a later sibling. The
  differential command gate constructs exactly that ordering and requires
  successful builds plus the complete warning from both implementations. The
  complete canonical rewatch suite also passes with this resolver change.
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
- Compiler metadata records which project built each package's artifacts.
  Dependencies built as part of the current project continue to inherit that
  project's package-output settings and are invalidated when those settings
  change, including stale-output cleanup and deleted-source handling from Rust
  PR #8540. An independently built dependency instead retains its published
  output layout when a consumer requests different output settings. This is a
  deliberate ownership refinement beyond that Rust patch: applying consumer
  invalidation unconditionally deleted Belt's `lib/js` and `lib/es6` trees
  during the repository test suite. The recorded owner selects the same
  package-output settings for cleanup, freshness repair, compilation, and
  subsequent metadata writes, including when an independently built package
  itself needs rebuilding. Consumer `clean` follows the same boundary: it
  removes consumer-built source dependencies but leaves an independently built
  package's compiler metadata and published tree intact. The metadata stores
  canonical native paths and compares them through the platform
  path-normalization boundary for Windows compatibility. Focused integration
  coverage exercises both build and clean ownership paths.
- Generated CommonJS, GenType, and canonical rewatch fixtures now import
  Belt's actual published `.cjs` and `.mjs` files. Their former hybrid or
  in-source paths combined Belt's output directory with a consumer's suffix,
  or assumed that a consumer had rewritten the package, and depended on stale
  dependency CMIs. Treating those paths as compatibility requirements would
  preserve a build-cache accident rather than Rust's intended package-output
  algorithm.
- The integration runner starts watch mode, confirms the lock, performs a
  source edit, changes the configured output suffix, observes the resulting
  rebuild and stale-output removal, adds then deletes a source module while
  observing its generated output appear and disappear, and confirms lock
  cleanup after `SIGTERM`.
- `watch.lock` contains the running watch process PID, matching the lock-file
  protocol used by the existing integration helpers.
- Lock removal is also observed while parser, namespace, or compiler children
  are active. The subprocess schedulers poll the watcher ownership predicate
  and terminate their active process trees through the platform boundary before
  unwinding the build. Unit coverage uses a nonterminating child, and the
  focused integration runner removes `watch.lock` while a deliberately slow
  compiler child is active and checks both processes disappear.
- Every canonical watch test passes with the native libuv backend: ordinary and
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
  rebuilds. Its final presentation still lacks the change/full-rebuild header
  and post-failure watching footer and remains in the output-presentation gate.
  Comma-separated feature names are trimmed like the Rust CLI.
- GenType compiler arguments distinguish single-file inspection from a full
  build: `compiler-args` omits unavailable expanded source/dependency paths,
  while builds retain them; both include the workspace project root.
- `compiler-args` now classifies the inspected source using the configured
  development-source tree. Development sources receive dev dependency includes
  before regular dependency includes; ordinary sources receive only regular
  dependencies. Resolved include directories are emitted even before the
  dependency has produced `lib/ocaml`, matching Rust's argument construction.
  Dedicated tests cover ordering, ordinary-source exclusion, and both missing
  dependency policies.
- Project context now follows Rust's ReScript-level workspace rule: a child
  inherits the nearest parent configuration only when that parent lists the
  child's package name in `dependencies` or `dev-dependencies`. Merely matching
  an ancestor `package.json` workspace glob no longer changes JSX, package
  outputs, locks, or cleanup scope. Tests cover regular/dev membership and an
  unrelated standalone project nested below this repository.
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
- Redirected `clean` now emits Rust's `Cleaning <package>` info-level progress
  for every package whose outputs the command actually owns. The parsed global
  verbosity is retained by the clean command, so `-q` suppresses this output as
  it does in Rust. A differential command case compares stdout and stderr
  exactly at the default level and requires both implementations to stay quiet
  under `-q`; interactive clean phase rendering remains in the final terminal
  presentation pass.
- Quiet build and watch now share Rust's `show_progress` boundary. `-q`
  suppresses redirected and interactive cleanup, parse, compile, completion,
  and terminal-clear output while retaining compiler warnings and failures.
  Differential success, compile-error, parse-error, warning, clean, and
  recoverable watch-failure cases bring the command-validation gate to 74
  cases; the watch case compares the raw parse diagnostic, proves a subsequent
  successful rebuild, and rejects a duplicate high-level failure summary. The
  PTY gate separately retains quiet interactive builds.
- Redirected build failures now retain Rust's phase and stream boundaries.
  Compile failures send the `Compiled <n> modules` summary to stderr, while
  parser failures stop before the parse/compile summaries, prefix the compiler
  diagnostic with its package, and finish with `Could not parse Source Files`.
  Standalone warning output also retains Rust's final separator without adding
  an extra blank line before configuration diagnostics. Three isolated
  differential builds compare normalized stdout and stderr byte-for-byte for a
  compiler error, parser error, and successful warning build.
- Explicit `format` operands now have retained filesystem-validation coverage.
  Missing files, directories, and unsupported extensions produce byte-identical
  Rust/OCaml stdout, stderr, and exit classes because both implementations
  delegate those operands to the same formatter. A missing
  `RESCRIPT_BSC_EXE` is also exercised for format: Rust's existing helper panic
  is retained as a documented safety fix, while OCaml reports the stale path
  normally. The command gate shares one missing-compiler probe across build and
  format rather than duplicating its setup.
- Runtime discovery now has differential failure coverage in addition to its
  successful package-resolution test. With no `RESCRIPT_RUNTIME` and no
  installed `@rescript/runtime`, both implementations retain the stable setup
  guidance. A stale explicit `RESCRIPT_RUNTIME` is deliberately rejected by
  OCaml before parser or compiler subprocesses are started. Rust currently
  accepts that missing path, performs avoidable compiler work, and reports
  repeated misleading `Pervasives` lookup failures. The OCaml preflight is a
  documented simple Rust bug/inefficiency fix rather than a parity gap.
- Configuration and dependency-discovery failures now retain their stable
  diagnostic ownership explicitly. Differential command checks require the
  selected root or parent configuration path, the shared package-tree
  operation/dependency/workspace prefix, and the shared malformed
  `package.json` prefix. Serde versus Yojson locations, canonicalization and OS
  error wording remain intentionally native after that context; comparing
  those tails byte-for-byte would make the gate platform- and library-specific.
- Every rejected JSX and source-map schema case now also has a differential
  diagnostic ownership check. This covers 25 invalid shapes, including Rust's
  JSX-version panic, and requires both outputs to identify `jsx` or
  `sourceMap` even though their Serde/Yojson explanations differ.
- Redirected clean-build output now has the same exact normalized differential
  coverage as compiler failures, parser failures, and successful warning
  builds. Together with exact clean output, focused format/compiler-argument
  checks, and canonical redirected watch scenarios, this closes ordinary plain
  output; the intentionally deferred `-v`/`-vv` event stream remains part of
  the final terminal-presentation pass.
- The pre-parser source-disappearance branch now has deterministic differential
  coverage. With Rust's Rayon parser constrained to one worker, the first
  parser subprocess wrapper deletes both discovered fixture sources; Rust then
  panics on the second unchecked source read. OCaml has already constructed its
  parse jobs and returns a normal path-bearing parser failure. This closes the
  last source-race item that was audited but previously lacked a retained gate.
- Implicit `format` project discovery now has semantic differential coverage for
  a missing config, malformed JSON, and a directory at `rescript.json`. Both
  implementations must retain the command and path context plus the relevant
  failure class; OS error numbers and Serde/Yojson parse locations remain
  library- and platform-native. The shared OCaml config reader no longer repeats
  the same filename inside its own `Could not read '<path>'` diagnostic, and
  unit tests retain that single-path invariant for missing files and directories.
- Invalid format stdin now retains the stable `Error formatting stdin` and
  compiler-diagnostic content without comparing random temporary filenames.
  Formatter write-back failures no longer escape as an OCaml `Sys_error`
  constructor: they report the write operation, requested source path, and
  platform error. Unit coverage forces the contextual branch with a
  non-directory parent, and the differential runner exercises a read-only file
  when the host filesystem enforces its permission bits.
- Cycle detection now selects a shortest cycle as Rust's compile scheduler does,
  rather than reporting the first depth-first cycle encountered. Equal shortest
  cycles and rotations use a deterministic lexical presentation instead of
  inheriting hash traversal order. Focused graph coverage contains disjoint
  three-node and two-node cycles and requires the two-node result; the canonical
  multi-package cycle snapshot remains unchanged.
- A missing project folder is rejected before path canonicalization with Rust's
  user-facing preflight diagnostic instead of leaking an OCaml `Unix_error`;
  the focused runner checks the complete path-bearing message.
- On Linux and macOS, Dune promotes the OCaml implementation as the normal
  `rescript.exe`, while Cargo retains Rust rewatch as `rescript-rust.exe`. The
  root package exposes `rescript` and `rescript-rust`; `rescript-ocaml` remains
  an alias for early testers. This makes ordinary workspace builds and the full
  repository test pipeline exercise OCaml without per-test overrides. The
  artifact manifest includes both launchers and their shared signal-forwarding
  helper. Non-Windows CI runs the OCaml unit, focused, and complete canonical
  rewatch suites against the default packaged executable and repeats the
  canonical suite through the installed package. Windows keeps Rust as the
  default until the native OCaml binary is ready rather than publishing an
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
| Rust | 5,489 ms | 759,232 KiB |
| OCaml | 6,468 ms | 752,496 KiB |

The latest completed gate's 1.178× wall-time ratio and 0.991× RSS ratio pass
the 1.25× gate.
The host was plugged in and otherwise idle for this run. Docker on a Mac is
still noisier than native Linux or dedicated CI, so final acceptance should
repeat the distribution on a stable host rather than treating this one passing
set as universal. Repeated runs observed impossible non-median LinuxKit clock
jumps despite the affected builds completing in seconds; the preceding run
reported one OCaml sample as 254 seconds while its surrounding samples were
5.6–5.8 seconds. The latest run had five coherent samples for each
implementation, but a final native/stable-host run remains necessary. Passing
this aggregate gate also does not excuse the clean-build publication probes
identified by the filesystem audit below.

Both implementations performed exactly 1,031 `bsc` launches: 512 parses, 7
namespace compilations, and 512 module compilations, of which 40 were interface
compilations; each also launched the PPX once. This rules out extra compiler
invocations on clean builds as the current wall-time source. The extended work
gate also measures incremental orchestration. Its latest five-run acceptance
run reported identical work in every scenario:

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
repeated filesystem/configuration work are the main candidates. Compiler output
capture now uses close-on-exec pipes rather than two temporary files per
subprocess. A blocking reader thread drains each stream, which avoids
stdout/stderr pipe-capacity deadlocks and works on Windows without assuming
that `select` supports anonymous pipes. Capture is intentionally unbounded like
Rust's `Command::output`; changing diagnostic limits would be a separate
behavior decision. Descriptor creation stays behind `platform.mli`, and
termination is deferred before descriptor acquisition so an asynchronous watch
stop cannot interrupt the handoff to the cleanup owner. Failure cleanup closes
unowned descriptors, terminates and reaps any launched child, then joins reader
threads after closing their parent-side write ends. A stress test verifies exact
capture of 1 MiB on both streams without truncation or deadlock.

Child completion now uses one blocking waiter per launched process and a
condition-variable notification to the single scheduling thread. This removes
the former 10-microsecond `waitpid(WNOHANG)` scan: a four-source trace fell from
419 driver `wait4` calls to 10, with no empty `pselect6` polling. The waiter
publishes a job only after the direct child is reaped and both output readers
finish, so a descendant-held pipe cannot block the central scheduler or stop
watch cancellation polling. A 100-node serial `/bin/true` dependency probe took
0.091 seconds in independent review, down from 0.649 seconds with a fixed
five-millisecond retry and close to the 0.033-second direct spawn/wait reference.
The retained regression cancels an exited parent whose descendant holds both
pipes in under one second on Unix. A single five-millisecond ticker exists only
for scheduler calls with a watch poll callback; child completion wakes the
scheduler immediately.

A one-run working-tree performance smoke check measured 5,534 ms / 739,804 KiB
for OCaml and 4,766 ms / 723,876 KiB for Rust (1.161x wall time and 1.022x RSS).
It retained identical clean, unchanged, and edit compiler-work manifests,
including 1,031 clean compiler launches, plus identical complete file sets and
stable artifact contents. This is useful resource/correctness evidence for the
bounded waiter threads, but is not the required five-run acceptance result.
The warning-free build, all 19 OUnit2 groups, focused integration runner,
74-case differential command validation, and complete 48-test canonical
rewatch suite pass with the notification scheduler. The canonical run covered
ordinary and interrupted watch lifecycle, locks, diagnostics, cleanup,
formatting, features, and compiler arguments and left no watcher or fixture
change behind.
[`bench/filesystem_audit.sh`](bench/filesystem_audit.sh) now preserves a
normalized Linux `%file` syscall audit for clean, unchanged, and single-edit
builds. It reports fixture-local path/operation multisets and repeated accesses
by readable category, while keeping runtime/loader/toolchain calls out of the
comparison rather than treating incomparable raw process-wide totals as a
quality metric. Its first post-pipe audit found no `.rewatch-ocaml-stdout` or
`.rewatch-ocaml-stderr` accesses, confirming that capture sidecars are gone. It
also exposed a separate issue worth profiling: on the benchmark fixture an
unchanged build made 50,368 OCaml versus 3,368 Rust project-local metadata
calls, and 798 versus 160 directory scans. Single-edit counts were nearly
identical to unchanged. The first safe Rust-parity cleanup now reuses resolved
dependency roots and inventories each cleanup tree once. Attempts to cache
artifact paths or mtimes more aggressively were rejected: the canonical rename
and deletion sequences then intermittently emitted a low-level missing-CMI I/O
error instead of Rust's missing-module diagnostic. The retained changes reduced
the unchanged result to 37,602 metadata calls and 477 directory scans (Rust:
3,367 and 160); the edit result was 37,625 and 477 (Rust: 3,385 and 160).
The first explicit compile-asset-state slice now scans each flat `lib/ocaml`
directory once and passes that inventory to stale cleanup. This matches
`read_compile_state.rs` ownership and avoids a second metadata probe for every
entry. The latest unchanged result is 35,371 metadata calls and 477 directory
scans (Rust: 3,369 and 160); the edit result is 35,394 and 477 (Rust: 3,384 and
160). The directory count is unchanged because the state scan replaces the
cleanup scan; moving freshness consumers onto explicit module state is what
should remove the repeated popular-CMI probes.
The fixed dirty-state scheduler then reduced repeated readiness-time freshness
checks without changing compiler work: the current unchanged result is 29,499
metadata calls and 475 directory scans (Rust: 3,367 and 160), while the edit
result is 29,524 and 475 (Rust: 3,384 and 160). Its clean trace records 26,123
metadata calls and 318 scans (Rust: 12,423 and 158). The remaining repeated
popular-CMI probes and path canonicalization still dominate the incremental
gap.
Moving dependency freshness onto resolved `Build_state` edges and the shared
CMI/CMT inventory removed the popular-CMI probes and recursive dependency
artifact searches. The latest unchanged result is 19,093 metadata calls and
443 directory scans (Rust: 3,367 and 160); the edit result is 19,123 and 443
(Rust: 3,384 and 160). Clean remains 26,123 and 318 because compiler work, not
incremental dependency freshness, dominates that trace.
Reusing canonical package identities through collection, graph visitation,
build traversal, and internal locality checks reduces unchanged metadata calls
again to 18,315 and clean calls to 25,345; edit records 18,345. Directory scans
remain 443 incrementally and 318 clean because this slice removes redundant
`realpath`/`readlinkat` work rather than directory walks. The public locality
entry point still canonicalizes arbitrary caller paths, while graph internals
use the explicitly named canonical-path variant.
Removing redundant existence probes before `stat`/`lstat` reduced the latest
unchanged trace to 11,240 metadata calls (Rust: 2,967), the edit trace to 11,271
(Rust: 2,984), and the clean trace to 23,118 (Rust: 12,022). Directory scans
remain 443 incrementally and 318 for clean builds: each inventory walk now uses
one metadata operation per entry, but overlapping consumers still walk the same
trees. Live symlinks remain leaf entries and dangling symlinks remain omitted,
with focused coverage that is skipped only at runtime on Windows. An experiment
that also replaced guarded removal with unconditional best-effort deletion was
rejected after the canonical watcher observed an output between publication
states; restoring the guard passed that case and the complete suite. The guard
therefore remains until output cleanup and publication have a stronger shared
ownership boundary.
Package source discovery now retains both the compilation view and a full leaf
inventory for stale-output and clean-command consumers. It also derives the
GenType directory list during that same discovery phase, matching Rust's
package-owned `source_files`/`gentype_dirs` state instead of performing I/O
during configuration decoding. The latest unchanged trace falls to 321
directory-scan calls and 10,662 metadata calls (Rust: 160 and 3,367); edit is
321 and 10,693 (Rust: 160 and 3,386), and clean is 196 and 22,969 (Rust: 158
and 12,424). Recursive and non-recursive compilation, inactive cleanup trees,
directory symlinks, and GenType's feature/dev-source rules have focused
coverage.
The compile-asset inventory now reads the absolute source location embedded in
each published AST, as Rust's `read_compile_state.rs` does. Stale compiler
artifacts are addressed directly in the corresponding `lib/bs` source
directory, and a public generated output determines the exact path of its
working mirror. A recursive `lib/bs` inventory remains as a lazy recovery path
only when a stale artifact has no usable AST mapping. Focused tests cover the
direct namespaced/deferred-CMI path and the malformed/legacy fallback, and the
complete canonical suite covers rename, deletion, suffix changes, feature
changes, and watch rebuilds. The latest unchanged trace consequently falls to
162 directory-scan calls and 7,940 metadata calls (Rust: 160 and 3,367); edit
is 162 and 7,971 (Rust: 160 and 3,386), and clean is 160 and 22,951 (Rust: 158
and 12,422). This run intentionally did not update wall-clock measurements
because unrelated host work made timings unsuitable for comparison.
Restricting published-artifact metadata reads to the AST/IAST/CMI/CMT entries
whose timestamps or contents are actually consumed reduces the next unchanged
trace to 7,046 metadata calls (Rust: 3,367) and edit to 7,077 (Rust: 3,384).
Cleanup still inventories the names of CMJ/CMTI/copied-source/MLMAP entries, so
stale removal behavior is unchanged; clean-build counts remain effectively
unchanged because those directories start empty.
Source discovery now retains the source mtimes already read while walking, and
the compile-asset state indexes published AST mtimes by their encoded source
locations. Both global parsing and package compilation consume those snapshots
instead of probing every source and AST again. This also matches Rust's strict
freshness rule: an AST must be newer than its source, rather than merely not
older. The latest unchanged trace is 5,326 metadata calls (Rust: 3,367), edit
is 5,359 (Rust: 3,384), and clean is 22,089 (Rust: 12,424). Per-process audit
output confirms identical `bsc` filesystem-call counts in all three scenarios;
the residual belongs to the build-system drivers. Timings remain deferred while
the host is busy.
Source-directory deduplication and symlink-cycle detection now reuse the
metadata already obtained by traversal on Unix: `(device,inode)` identifies a
directory without a `realpath` call for every recursive step. The platform
boundary keeps canonical, case-normalized path identity on Windows, where Unix
inode emulation is not a dependable cross-volume contract. Existing overlapping
source-root and directory-symlink coverage remains green. The latest unchanged
trace is 4,997 metadata calls (Rust: 3,367), edit is 5,030 (Rust: 3,384), and
clean is 21,760 (Rust: 12,425), with directory scans still within two calls.
These are observational counts rather than a raw-total gate, and they include
compiler process behavior. The directory-traversal gap is now explained and
effectively closed, but the incremental metadata difference remains material
and keeps the superfluous-work audit open.
The remaining AST/CMT freshness and generated-output presence checks now consume
the compile-asset and cleanup inventories instead of probing each module. This
preserves the deliberate repair of manually deleted JavaScript while reducing
the unchanged trace to 3,711 metadata calls and 162 directory scans (Rust:
3,367 and 160); edit records 3,745 and 162 (Rust: 3,384 and 160), and clean
records 20,902 and 160 (Rust: 12,423 and 158). The first form still eagerly
hashed every scheduled module's CMI even when clean. Moving that hash to the
actual dirty-module dispatch point, matching Rust's `compile.rs`, leaves OCaml
with fewer incremental opens than Rust: 1,187 versus 1,220 unchanged and 1,215
versus 1,246 after an edit. The remaining 344 unchanged metadata calls are
primarily repeated package-path canonicalization (`readlinkat`); compiler
process calls match and directory traversal is within two calls. Clean-build
metadata still includes the much larger compiler workload, so executable
attribution below separates compiler and driver behavior.
Generating `.sourcedirs.json` now reuses the canonical dependency roots already
owned by graph preparation instead of resolving every local package edge again.
In the latest paired audit, unchanged metadata is 2,916 calls (Rust: 2,962)
and edit metadata is 2,936 (Rust: 2,979), while directory scans remain 162
versus 160. OCaml also retains lower incremental open counts: 1,187 versus
1,220 unchanged and 1,215 versus 1,246 after an edit. Incremental metadata and
open work are therefore now slightly below Rust despite the two additional
inventory scans.

The same retained trace attributes every clean-build `bsc` filesystem call
identically between implementations: 7,123 metadata and 5,371 open calls.
The first attributed trace showed that the aggregate clean metadata difference
(20,167 OCaml versus 12,016 Rust) was driver-side, not extra compiler work. Its
largest OCaml-only groups were repeated `newfstatat` calls on already-created
`lib/ocaml`, `lib/bs`, and source-directory parents while publishing artifacts;
the WebAPI `lib/ocaml` directory alone was probed 1,880 times. Publication now
uses a narrow helper when both the compiler-produced source and package-owned
destination directory are already known to exist, while the generic defensive
copy path retains its old missing-source behavior. This removes redundant
per-artifact source and parent probes without changing the cross-platform file
APIs. Clean metadata falls to 13,191 calls versus Rust's 12,017; the remaining
1,174-call delta is mostly per-CMI comparison and case-candidate checks rather
than directory discovery or extra compilation.

### Active filesystem-performance work

The aggregate timing, memory, compiler-work, artifact, and behavioral gates
pass, and incremental filesystem work is now slightly below Rust apart from two
inventory scans. Clean-build driver metadata remains about 1,174 calls above
Rust, with a concrete per-CMI comparison/candidate shape rather than repeated
directory discovery. Closing or specifically documenting that residual is a
completion gate for the current architecture refactor. Rerun
the evidence with `bench/filesystem_audit.sh`; its prerequisites, isolation,
normalization, and caveats are in `bench/README.md`.

Rust-parity improvements should be attempted before novel optimizations. The
remaining candidate is to compare clean-build per-CMI equality and
case-candidate checks with Rust's compile-state transitions. Incremental
canonicalization is no longer a material deficit, and publication-parent probes
have been removed; neither should be redesigned merely to lower a raw total.

The asset/module state must retain explicit transitions for discovery, stale
cleanup, parse publication, interface publication, implementation publication,
source rename/deletion, failed compilation, and watch rebuilds. Dirty-state
snapshotting, CMI-content propagation, successful-publication refresh, and
failure-preserved state are now explicit. Do not cache a missing or present
artifact independently of those transitions. Earlier path/mtime cache
prototypes reduced the trace further but failed
`rewatch/tests/compile/04-rename-file-internal-dep.sh` and
`rewatch/tests/compile/08-remove-file.sh`, replacing the intended missing-module
diagnostic with a missing-CMI I/O error. Those two tests, the namespaced rename
case, the complete canonical suite, compiler-work manifests, and artifact
manifests are mandatory regression gates for another attempt.
Do not confuse that deterministic regression with a separately observed Docker
Desktop/macOS bind-mount anomaly. On the case-insensitive host-backed workspace,
`bsc` has intermittently seen a differently cased stale-CMI candidate in
`stat` and then received `ENOENT` from the immediately following `open`, even
though no build action occurs between those calls. The same canonical command
can pass on its next invocation without a binary change, and the rename/delete
scenario consistently emits the source-located diagnostic on the container's
case-sensitive `/tmp` filesystem. Five consecutive runs against the Rust
reference binary reproduced the identical lowercase-CMI I/O diagnostic on the
bind mount, confirming that this observation is not specific to the OCaml
driver. The benchmark documentation therefore requires case-sensitive isolated
fixtures; a bind-mount occurrence is recorded but is not evidence of a
scheduler regression unless it reproduces there.
The host-backed worktree also intermittently left an ownerless Git index lock
during canonical fixture restoration. Shared test helpers now restore requested
tracked files atomically from their indexed blobs without modifying the index;
this also respects the suite's temporary Intel-macOS index. Restoration errors
are fatal instead of allowing later scenarios to run against a partially
mutated fixture. The configuration-watch case now polls the implementations'
completion markers before changing or restoring configuration, and verifies
the watcher process exits after lock removal. The clean/rebuild lifecycle check
captures output in a temporary file instead of rewriting an otherwise
unasserted tracked snapshot.
An intermediate attempt that changed freshness consumption and publication in
one step reproduced the same regression, while retaining only state
construction and inventory sharing passed the complete canonical suite. The
fixed dirty-state and scheduler-propagation slice now also passes the complete
suite, so the next slice can replace live filesystem freshness checks while
preserving these transitions.

Ideas not present in Rust remain separate hypotheses for after parity:

- retain a validated build inventory across short-lived CLI invocations, with a
  content/version fingerprint and conservative fallback to discovery;
- use a persistent pool of compiler workers or eventual in-process compiler
  integration to reduce process startup, only after pipe parity and with strict
  isolation of compiler-global state;
- parallelize independent configuration parsing or directory inventory with
  OCaml domains if profiling shows CPU saturation rather than I/O latency;
- use watcher event state to avoid a full rediscovery after quiet periods,
  retaining overflow/config-change fallbacks to a clean rescan.

For each hypothesis, measure it independently, preserve the 1.25× timing/RSS
gate and exact work/artifact checks, compare filesystem calls, and include a
Windows design review. None should be mixed into compatibility work merely to
improve a headline benchmark.

A post-pipe correctness smoke run of the performance harness retained exact
compiler work: clean `1031/512/7/512/40/1`, unchanged `4/2/0/2/1/0`, and
single-edit `6/3/0/3/1/0` for total/parser/namespace/compiler/interface/PPX
launches in both implementations. The selected artifact sets and contents were
byte-identical. Its one-run timing is deliberately not an acceptance result.
The retained gate now also compares the complete fresh-build file-name set,
including cache and editor-control artifacts. Byte comparison remains limited
to stable generated artifact classes because compiler logs and
`compiler-info.json` contain timestamps or intentionally different internal
encodings. The stable-content set includes AST, copied-source,
source-directory, and build-marker files in local and installed dependency
trees rather than checking only top-level JavaScript/CMI/CMJ outputs. An audit
that temporarily included `.cmt`/`.cmti` found one differing typed-debug file
while its CMI, CMJ, AST, JavaScript, and source copy all matched; typed debug
metadata therefore remains existence-checked but is not a byte-stability
contract between sequential builds.
Its first run found that OCaml removed warning-bearing parser outputs from both
`lib/bs` and `lib/ocaml`, while Rust invalidates only the published
`lib/ocaml` freshness copy. OCaml now retains the parser's working AST in
`lib/bs` as Rust does; removing the published copy still forces the intended
warning replay on the next build without performing superfluous deletion.
A follow-up correctness smoke run passed the complete file-set comparison, all
three exact compiler-work manifests, and the stable artifact-content manifest.
Its single timing sample was 5.511 seconds for OCaml versus 4.652 seconds for
Rust (1.185x) with 749,052 versus 739,592 KiB peak tree RSS; those timings are
observational and do not replace the five-run acceptance result.

The current `cloc` 2.04 source-size snapshot reports 7,818 Rust production
lines after excluding the intentionally omitted telemetry module and inline
test-only sections, versus 6,910 OCaml production lines, or 88.4%. Counting
language-specific tests separately gives 2,773 embedded Rust unit-test lines
and 5,287 OCaml unit/focused test and fixture lines. The OCaml benchmark tooling
adds another 372 lines, including the source-size script itself. The shared
canonical integration suite is deliberately not charged to either side. These
figures describe maintainability surface, not parity or quality: explicit
interfaces and separate test infrastructure add useful lines rather than
indicating behavioral duplication.
[`bench/source_size.sh`](bench/source_size.sh) preserves the scope and command;
rerun it for the final maintainability review alongside maximum module size.

General portable filesystem operations now live behind the narrow
`file_util.mli` interface. Recursive directory creation, file reading/copying,
content comparison, timestamps, recursive inventory, and removal no longer
share a module with ReScript-specific generated-output naming and stale
cleanup. `build_artifacts.ml` is now 361 lines, while the general owner is 135
lines. Its focused coverage moved into `file_util_tests.ml`, leaving
`build_artifacts_tests.ml` concerned only with artifact cleanup. The extraction
also closes partial-channel leaks when opening a copy destination or the second
file in a comparison fails: every successfully opened channel enters its own
`Fun.protect` before the next acquisition.

The qualification audit removed broad `File_util` and `Build_artifacts` opens.
Calls such as `File_util.remove_file`, `File_util.ensure_dir`, and
`Build_artifacts.generated_js_path` now show both ownership and potentially
destructive effects at the call site. `Build_types` remains open in the four
record-heavy build modules because qualifying its pervasive record fields and
types would add noise without clarifying effects; Cmdliner's module and term
syntax opens remain for the same DSL-readability reason. `Config_decode` and
`Config_types` retain their local include/open relationship because `Config`
deliberately presents them as one public configuration API. Narrow
`build_artifacts.mli` and `process.mli` interfaces now hide implementation-only
classification, pipe, thread, notification, and scheduler details. This also
made an unused `generated_output_owner` helper visible as dead API, so it and
its tests were removed; stale-output behavior remains covered through the
public cleanup operation.
The warning-free whole build, all 19 OUnit2 groups, the focused integration
runner, all 74 command-validation cases, and the complete applicable canonical
Rewatch suite pass after the audit. An independent maintainability review found
no semantic, API, coverage, or Windows-portability issue in the slice.

The command-cycle accumulator and the prepared-package/global-module records
now live in `build_types.ml`, including one constructor for their initial
state. This removes 92 lines of state declaration and initialization from
`build.ml`, makes the ownership parallel to Rust's `build_types.rs` explicit,
and leaves the remaining package-graph extraction free to share state without
duplicating records or introducing callback-heavy interfaces. The extraction
does not alter filesystem access or build scheduling; all 18 OUnit2 tests and
the focused build/incremental runner passed afterward.

The remaining `build.ml` lifecycle was reviewed again after those extractions.
Its setup, progress/error reporting, staged-output publication, log and lock
finalization, and build/watch entry points share command-scoped state and form
one coherent owner; splitting them further would primarily replace local
closures with callback plumbing. It therefore remains one 387-line
implementation, while a narrow `build.mli` exposes only the command entry
points and translated public exceptions.

Package-tree discovery now lives in `package_graph.ml`. It owns the two-pass
feature-union traversal, command-wide dependency resolution and duplicate
selection, `allowed-dependents` validation, local/dev classification, source
inventory, root-only warning/filter overrides, output ownership, and prepared
package ordering. The production/test helper calls moved to the new owner,
reducing `build.ml` from 1,407 to 1,174 lines without changing path or process
APIs. The warning-free build, all 18 OUnit2 tests, focused integration runner,
69-case command-validation gate, and complete canonical rewatch suite passed
after the split. `build_preparation.ml` now consumes the prepared list for
cleanup, preliminary parsing, and dependency-state construction; `build.ml`
receives that state for recursive compilation.

Artifact/source timestamp comparisons and the published-AST freshness marker
now live in `build_freshness.ml`; cycle-dependent closure now lives beside
cycle detection in `graph.ml`. Their existing focused tests address the new
owners directly. This removes two small algorithms from orchestration and
keeps the forthcoming dependency-preparation interface from exposing helpers
that belong to reusable state/graph boundaries.

Command-level `--after-build` parsing, execution, output forwarding, and error
classification now live in `after_build.ml`. Build orchestration retains only
the post-success dispatch point and still releases the build lock before the
hook runs. Existing focused success coverage and the three differential empty,
missing-program, and nonzero-exit cases protect the extracted behavior.

Toolchain and compiler-context initialization, compile-asset cleanup,
preliminary global parsing, dependency extraction, build-state construction,
and cycle detection now live in `build_preparation.ml`. It consumes the package
ordering from `package_graph.ml` and hands a ready global state to `build.ml`
for recursive compilation. This exact-move extraction reduces `build.ml` from
1,094 to 781 lines without changing filesystem calls, process work, phase
ordering, timing accumulation, or cleanup accounting. The warning-free build,
all 18 OUnit2 tests, focused integration runner, 69-case command-validation
gate, and complete canonical rewatch suite passed after the extraction.

Recursive dependency-package traversal, per-package parsing and publication,
dirty-state construction, namespace-job preparation, and scheduled compiler-job
construction now live in `package_build.ml`. `build.ml` retains aggregate
namespace/compiler dispatch, command output, finalization, and lock handling.
The extraction is an exact move apart from renaming `run_internal` to
`prepare_tree` and replacing its command-facing exception with an identity alias;
it reduces `build.ml` from 781 to 386 lines while leaving the new cohesive owner
at 405 lines. The warning-free build, all 18 OUnit2 tests, focused integration
runner, 69-case command-validation gate, and complete canonical rewatch suite
passed after the extraction.

Configuration ownership is now split without changing the `Config` facade:
`config_types.ml` owns public records and the shared error constructor,
`config_decode.ml` owns duplicate-aware JSON primitives and structured field
decoding, and `config.ml` retains file loading, top-level assembly, and
runtime/path queries. The implementation is an exact move with explicit narrow
interfaces; in particular, the intentional raw-map last-value behavior and
typed-field duplicate rejection are unchanged. `config.ml` falls from 782 to
383 lines and the extracted decoder is 362 lines. The
warning-free build, all 18 OUnit2 tests, the dedicated 297-case differential
configuration gate, focused integration runner, and 69-case command-validation
gate passed after the split.

## Known gaps

- Incremental state currently relies on artifact timestamps, byte-identical CMI
  publication, and in-memory warning state during watch. Rust's richer
  compile-state model is not otherwise ported.
- Native Windows verification remains incomplete. Configuration schema
  acceptance, argument projection, source-level validation, and ordinary
  redirected-output inventory are complete; semantic `-v`/`-vv` events stay
  deferred with terminal presentation. Incremental filesystem work is at or
  below Rust, while the explained clean-build driver delta remains documented
  for future optimization.
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
- Configuration schema now has a retained 297-case differential acceptance
  gate. Its 36 source cases compare shorthand and qualified sources, nested
  `subdirs`, nullable optional fields, arbitrary non-`dev` type strings,
  forward-compatible unknown fields, every invalid JSON kind, and duplicate
  typed fields. Another 42 cases cover dependency forms, modern/legacy alias
  conflicts, dependency feature requests, feature maps, and
  `allowed-dependents`. Another 91 cases exhaust package-spec shapes and output
  conflicts, JSX and source-map fields/modes, and post-build commands. They
  record the reference implementation's current distinction between duplicate
  typed fields (rejected) and source-map object keys decoded through an
  intermediate JSON map (last value wins). This appears to be an incidental
  decoder consequence, not an intended configuration contract. Another 128
  cases cover JSON roots and names, Rust's user-deserializable internal `path`,
  warnings, compiler and PPX flags, namespaces, experimental features, and the
  complete GenType schema. For every shared accepted case the gate also
  deep-compares Rust and OCaml
  parser/compiler argument arrays. CI runs the table against both promoted
  executables; existing unit and canonical tests cover source inheritance,
  feature closure and cycles, dependency permissions, traversal behavior, and
  post-build execution.
- Four known configuration divergences are first-class gate expectations:
  unsupported JSX and empty PPX commands expose Rust panics, namespace entries
  without a namespace are rejected only by OCaml, and compiler-flag whitespace
  is normalized only by OCaml. Explicit divergence rows skip argument equality
  but still assert each implementation's expected outcome; all ordinary
  accepted rows retain exact comparison. Rust panic expectations require exit
  status 101, so upstream fixes cannot silently weaken the audit.
- Rust's internal `Config.path` field is currently user-deserializable: a JSON
  string is accepted and then replaced by the actual configuration filename,
  while other JSON kinds are rejected. The OCaml decoder now reproduces that
  schema without trusting or storing the supplied value. A focused unit test
  and differential cases retain this otherwise easy-to-miss behavior.
- Configuration path canonicalization and file opening now translate both
  `Sys_error` and `Unix_error` into path-bearing `Config.Error` diagnostics.
  Missing paths and directory-valued config paths are tested, preventing raw
  OCaml exception rendering on these Rust validation paths.
- Duplicate keys now reproduce the reference decoder's two observed rules:
  typed configuration structs reject repeated known fields, while JSON-map
  backed values retain the last occurrence. This is recorded as a compatibility
  quirk rather than intentional configuration behavior. Differential acceptance
  covered 15 representative struct/map cases; focused tests also verify
  last-value semantics for source maps, features, experimental flags, and
  GenType debug maps. Repeated unknown fields remain accepted, as in Rust.
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
- Toolchain discovery no longer depends on the invoking working directory.
  Without `RESCRIPT_BSC_EXE`, the promoted OCaml executable canonicalizes its
  own location and uses the sibling packaged `bsc.exe`, matching Rust. Without
  `RESCRIPT_RUNTIME`, it resolves `@rescript/runtime` through the project package
  search. Focused tests remove each override independently, including a build
  against the actual promoted npm-package layout. Windows canonicalization
  strips `\\?\` drive and UNC prefixes before paths reach `bsc`; pure tests cover
  both forms, while native Windows execution remains part of the final VM gate.
- Interactive completion now uses the Rust status text, warning suffix,
  two-decimal timing, and clean/warning emoji after verifying that both output
  streams are terminals. `--no-timing` is threaded into the build instead of
  being parsed and discarded, and the clear-screen predicate is separately
  tested for interactive and redirected output. This closes the Rust unit-test
  inventory; it does not close the broader spinner/phase presentation gate.
- Interactive builds now also emit Rust-shaped cleanup, parse, and compile
  completion lines with three-step initial-build numbering, two-step watch
  rebuild numbering, phase-specific emojis, counts, and two-decimal timing.
  A retained PTY gate now runs both watchers, changes a source, and exactly
  compares normalized initial and incremental phase/final-status frames. It
  exposed and fixed the initial OCaml watch label from generic `Finished
  compilation` to Rust's `Finished initial compilation`. Redirected output
  remains unchanged. Live spinner frames and positive verbosity events remain
  separate output-gate work.
- Interactive output parity remains open. The OCaml executable now selects a
  TTY-specific final status with timing and emoji, emits phase completion
  counts, and clears the terminal when requested, but does not yet reproduce
  the live parsing/compilation spinner, positive `-v`/`-vv` events, or the
  clear-screen rebuild/failure headers. Plain redirected output and
  pseudo-terminal output are tracked as distinct gates in
  `PARITY_CHECKLIST.md`.
- A focused verbosity audit isolates the remaining non-spinner output gap. Rust
  `-v` reports project context, package discovery, AST generation, and
  interface/implementation compilation events; OCaml currently reports only
  its project root. Rust `-vv` also reports the compiled/dirty scheduler
  universe. The eventual gate should compare normalized event multisets rather
  than Rayon-dependent ordering. Redirected default output is unaffected.
- `watch` now uses long-lived libuv filesystem-event handles for the root and
  recursively resolved local dependency directories. Native events are treated
  as wakeups for the established snapshot/diff algorithm, so correctness does
  not depend on platform-specific rename payloads or event ordering. Handles
  are retained across builds and only added or closed when directory topology
  changes; a focused resource test covers stable, added, and removed counts.
  The former polling loop remains a runtime fallback if native setup fails.
- Existing generated outputs are updated as their compiler subprocesses
  succeed; only previously absent outputs are held until whole-build success.
  This preserves artifact/output consistency and avoids removing last-known
  output during compilation, but it is not an all-or-nothing filesystem
  transaction across an entire incremental build.
- Ordinary build cleanup and explicit `clean` remove abandoned watch staging
  sidecars even when their source was subsequently deleted. Sweeping is limited
  to tool-specific sidecar suffixes whose underlying path is a recognized
  generated JavaScript or source-map name; unrelated user files with a
  staging-like suffix are preserved and covered by a focused filesystem test.
- The focused integration runner also creates an empty nested source directory,
  waits for native registration, and then adds a source, covering directory
  discovery independently of a single coalesced create batch.
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
  Unix retains process-group cleanup. `spawn` exposes only the child PID, not a
  retained Windows process or job handle. If the direct child exits while a
  descendant still holds an output pipe, cancellation must not pass that
  potentially reused PID to `taskkill`; the current safe fallback therefore
  cannot terminate that descendant and may wait indefinitely for pipe closure.
  An atomic reaped-state flag narrows but cannot close the check/use race
  between a waiter reaping the child and `taskkill` opening the PID.
  The Windows milestone must add retained process/job ownership before native
  watch cancellation is complete; this is implementation work, not only
  verification. Watch lock/process
  probing and native watcher behavior still need a Windows cross-build and runtime
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
  creation uses `Spawn.safe_pipe` behind that boundary, while portable reader
  ownership stays in `Process`. The cross-platform native watcher has its own
  narrow interface over libuv rather than duplicating
  identical Unix and Windows implementations; actual Windows cross-build/runtime
  verification remains open.
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
- No additional regular-expression library is being added at this stage solely
  for `--filter`. The maintained, pure-OCaml `re` package and its `Re.Perl`
  frontend are substantially closer to Rust's syntax, but still do not provide
  exact `regex`-crate compatibility (notably for Unicode property classes).
  Switching engines would therefore reduce rather than close the documented
  gap while adding another production dependency. Revisit this during the final
  CLI audit if real-world filters or a deliberately supported syntax subset
  justify it; keep filter compilation behind one owner if that change is made.
- `luv` 0.5.14 is accepted for native filesystem events. It is a thin
  MIT-licensed binding that vendors and statically links libuv, supports the
  required Linux/macOS/Windows targets, and keeps the executable free of a
  runtime libuv dependency. Its latest release was September 2024, so the
  binding's cadence is quieter than ideal; the narrow `Native_watcher` boundary
  keeps replacement or localized vendoring practical if maintenance becomes a
  problem. The pinned version and upstream status must be reviewed during
  dependency updates. On Linux ARM64, static inclusion increased the promoted
  executable from approximately 3.6 MiB to 5.2 MiB; `ldd` still reports only
  libc and libm. The packaged third-party notices must include Luv and libuv's
  permissive license notices before general distribution.

## Next actions

1. Perform the final two-scope whole-port review and address confirmed findings.
2. At the final maintainability pass, add comments around ownership,
   concurrency, platform, and algorithmic invariants that are not apparent from
   the code itself. Comments should start with why the code or invariant is
   needed, provide enough context for readers who are not specialists in every
   relevant OCaml, build-system, compiler, or operating-system detail, and
   stand on their own rather than explaining code mainly by comparison with
   Rust (unless that compatibility relationship is itself the reason). Review
   naming and module qualification, including whether generic utility calls are
   clearer as `Module.function` than through `open`; do not apply either style
   mechanically. Remove dead code, and document the complete
   compatibility-oddity, corrected-Rust-behavior, and future-performance lists.
3. Validate macOS packaging and native event behavior, then prepare the pinned
   Windows handoff. Finish the Windows watcher/lock
   backend and path audit and run the native build, unit, focused, and canonical
   Bash suites in the VM. Address findings there and finish with an x64 Windows
   confidence run where available.
4. Complete the final output-presentation pass after platform validation: port
   Rust's semantic `-v`/`-vv` events with an order-insensitive differential
   gate, then implement and test the live interactive spinner frames and the
   clear-screen rebuild/failure headers.

Live spinner animation and positive verbose-event parity are explicitly
deferred until the final output-presentation pass after macOS and Windows
validation. The future filesystem-performance ideas documented above do not
block completion of the compatibility port.

The OCaml unit-test sources now live in `tests/rewatch_ounit_tests` and use the
repository's existing OUnit2 dependency, leaving production modules in
`rewatch-ocaml`. Consolidation exposed and fixed a leaked `RESCRIPT_BSC_EXE`
mutation that the former per-executable process isolation had hidden. Moving
the shared shell suite from `rewatch/tests` to a future `tests/rewatch_tests`
location is intentionally outside this PR because it would create unrelated
Rust, CI, and tooling path churn.
