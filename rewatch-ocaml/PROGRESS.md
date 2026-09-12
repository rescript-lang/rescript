# OCaml rewatch port progress

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.

## Current milestone

The complete applicable canonical `rewatch/tests` suite now passes with the
experimental `rescript_ocaml.exe`. Milestone 6 remains open for the final
implementation and code-quality work, performance/equivalence verification,
documentation, review, release-quality, and comment passes. The Windows
implementation remains in the tree, but native Windows validation and changing
the Windows default are deferred to a separate follow-up PR.
Stable Linux performance/resource measurement, macOS testing, and the
non-comment maintainability cleanup are complete. The configuration,
Rust-source validation,
Rust-test, control-file, and ordinary redirected-output inventories are
complete. OpenTelemetry parity is explicitly excluded by project decision;
ordinary verbosity and diagnostics remain in scope. Incremental state uses
existing AST, CMI, CMT, and generated-output artifacts rather than in-process
compiler state.

The September whole-port source review's six findings are now covered. Full
watch attempts initialize persistent compile freshness even when parsing fails;
Windows launches restrict inheritance to the three intended standard handles;
configured output suffixes drive freshness and stale-family cleanup; stdin
formatting remains interruptible while its input stays open; formatting uses a
discovered-file inventory rather than compilation modules; and diagnostics use
permissive display paths for external dependency cycles and namespace
collisions. Focused regressions cover initial-failure recovery, compound and
non-JavaScript suffixes, open-stdin termination, orphan interfaces, duplicate
module basenames, and external diagnostic paths. Native execution of the new
Windows handle-list path remains part of the dedicated Windows phase.

The same pass avoids rewriting `.sourcedirs.json` on ordinary retained edits,
caches namespace-member freshness summaries per attempt, and reuses the CMI
byte comparison performed by publication instead of rereading the CMI before
and after every compile. CMI publication state is a normal three-case variant
(`changed`, `unchanged`, or `unknown`) rather than an optional boolean; an audit
found no remaining `bool option` in the port or its unit tests. A session-owned
worker pool and a different capture backend remain measurement-gated: both add
lifecycle and Windows-pipe risk, and prior capture-buffer experiments did not
improve wall time. The overlapping historical progress documents should be
consolidated only after implementation stabilizes, so active review evidence is
not discarded during the review cycle.

A renewed lifecycle audit found a gap in the earlier source-oriented
comparison: Rust retains its initialized build
state across ordinary watch edits, while the OCaml callback reconstructed that
state on every event. The common existing-file edit path now carries the
snapshot's exact changed paths into the build, retains package, dependency,
artifact, cleanup, and module state, and reparses only those paths. Additions,
removals, unknown paths, and configuration changes still conservatively
reinitialize the build. When an existing source changes its dependency header,
the retained graph now replaces that module's forward edges, removes obsolete
reverse edges, adds new reverse edges without duplication, and checks the
updated graph for cycles without rescanning the project. A focused long-lived
watch case changes an edge, observes the rebuilt dependent, introduces a cycle,
confirms that the watcher remains alive, and then confirms recovery after the
cycle is removed. The retained-watch work/resource measurements and lifecycle
audit are now complete on Linux. For ordinary existing-file
content events, the libuv backend now returns the event path and kind directly;
the watcher skips configuration reload, source snapshots, and handle refresh.
Ambiguous filenames, source or directory topology changes, control files,
unresolved dependencies, and symlink targets retain snapshot reconciliation.
The retained-watch filesystem audit reports zero source-directory scans for
both implementations on a single edit. Its latest diagnostic run reported 46
OCaml versus 38 Rust project-local opens and 44 versus 25 metadata operations;
the remaining OCaml delta is concentrated in output/source-map and
artifact-safety checks rather than project rediscovery.

Retained watch builds now keep their attempted initialized state even when the
initial compile fails, so repairing a source performs the same two-phase
incremental parse/compile recovery as Rust instead of an unnecessary full
three-phase rebuild. Parsed ASTs belonging to that retained state survive, so
a partially successful initial compile can recover without retaining in-memory
state that refers to a deleted AST. Polling fallback compares the pre-event snapshot with the
snapshot immediately before compilation; an edit arriving after the trigger
snapshot can no longer be absorbed into the next baseline without compiling.
Focused tests retain both cases, including exact Rust/OCaml PTY recovery output.

Signal handlers only record deferred termination, including while libuv is
executing a callback. Compiler, parser, namespace, JavaScript post-build,
after-build, and build-lock waits all poll that request, while interrupted
debounce waits convert `EINTR` into the same controlled shutdown path. This
keeps lock, watcher-handle, child-process, and signal restoration finalizers in
control regardless of where SIGINT or SIGTERM arrives.
One-shot build, clean, format, and compiler-argument commands install a separate
command-scoped handler that raises through those same finalizers. A focused
slow-compiler case sends SIGTERM during a build and requires exit 143, complete
subprocess-group cleanup, build-lock removal, and no abandoned capture log.
Atomic replacements are reconciled even when the filesystem reports only the
now-absent temporary filename: every structural event immediately below an
explicit watch root requests a snapshot, while unchanged unrelated files still
do not request a build.
Native setup failure now has an injected end-to-end OUnit path through the real
polling loop. It requires the initial build, changes a source, observes one
incremental rebuild with the changed path, checks the fallback diagnostic, and
exits through the callback while verifying lock and signal cleanup. This closes
the previous test-evidence gap for the retained polling backend without adding
a production environment switch.

The retained-watch gate now verifies seven interleaved edits against release
executables, requiring identical parser/compiler calls and generated output
while sampling process resources after every edit. Its first recorded run
measured a 127 ms Rust median and 113 ms OCaml median. Rust stayed at 8 file
descriptors and 14 tasks, while OCaml stayed at 12 file descriptors and 3
tasks; RSS changed from 6,764 to 6,820 KiB for Rust and 9,844 to 9,872 KiB for
OCaml. These idle-host figures are evidence for this checkpoint rather than a
portable absolute baseline.

Native macOS validation at checkpoint `30725fb01` passed `make test-all` and,
after making temporary fixture paths canonical and accounting for the host's
case-insensitive filesystem, all 19 dedicated rewatch OUnit2 groups. The
focused and canonical native watcher suites and packaged-binary checks remain
part of the macOS gate.

The oldest-supported OCaml 5.0 static matrix is also exercised locally. An
isolated 5.0 switch exposed use of the newer `Mutex.protect` convenience API;
the notifier now uses a small `Fun.protect`-based lock wrapper with identical
exception-safe release behavior, and the static rewatch executable builds in
both the 5.0 and 5.5 switches.
The `static` Dune profile now passes `-ccopt -static` for the OCaml rewatch
executable itself, matching the compiler executable stanzas rather than relying
on switch configuration to imply the final native link mode. A local 5.5 build
was confirmed by both `file` (`statically linked`) and `ldd` (`not a dynamic
executable`); the artifact assembly gate independently rejects dynamic Linux
binaries.

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
contextual package-local/invocation/workspace `node_modules` lookup,
standalone-only ancestor hoisting, dependency-config preflight,
invocation-scoped canonical locality classification, and project-relative
presentation. A listed workspace package can resolve siblings through the
workspace without treating them as locally owned: their development graph,
source-directory metadata, cleanup policy, formatting scope, and watcher roots
remain excluded. Package discovery, build preparation, clean, format, and watch
all consume the same context decision. Focused tests cover both a workspace-root
invocation and a direct child-package invocation, while integration coverage
keeps a sibling's missing development dependency dormant and inspects the
resulting `.sourcedirs.json`. Build-internal exception aliases preserve concise
control flow, while the command entry point catches their owning module's
exception directly instead of exposing redundant aliases through `Build`'s
interface. This removes roughly one hundred lines from `build.ml` without
adding a facade layer or changing filesystem work.

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

Cross-platform ownership probing rejects PID zero because it is not a usable
child-process identity (`kill(0, 0)` addresses the caller's process group on
Unix). Linux verifies a live owner's executable through procfs; macOS, where
procfs is absent, queries `/bin/ps` and accepts only a `rescript` executable
name. This prevents an abandoned lock from becoming permanent after its
numeric PID is reused by an unrelated process. Inconclusive probes remain
conservative and preserve the lock.

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
inventory updates, and per-output JavaScript post-build hooks. The scheduler remains
process-agnostic, and `build.ml` supplies package state and callbacks without
creating unused parse/compile tuple payloads. Generic substring assertions also
moved out of production `Build` into the OUnit2-only `test_support.ml`.
Independent review found no argument-order, AST-decoding, namespace,
publication, warning, hook, exception-identity, or API regression.

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
  the same boundary. The original `Str` matcher has since been replaced by
  maintained `Re.Perl`, closing common alternation, grouping, shorthand-class,
  and repetition gaps while retaining linear-time matching. Rust-only Unicode
  and class-set behavior is an accepted engine divergence recorded in the
  compatibility matrix and final review.
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
short-lived invocation, while ordinary watch edits now retain it. Rust also has
diagnostic persistence, telemetry, and broader configuration and platform
handling that are not all ported.

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
safety. Their findings drove recoverable initial/rebuild errors, atomic populated lock creation with
stale-owner takeover, workspace build locks, owned lock removal, race-tolerant
symlink-aware snapshots, and cached content hashes. Takeover markers also carry
an owner PID and can themselves be recovered after an interrupted takeover.
They also introduced absent-output staging, which a later phase-ordering audit
removed after demonstrating that `bsc` made the public output observable before
the staging rename and could therefore expose an incoherent project. Those
reviews established event, locking, and recovery correctness but did not
compare end-to-end state lifetime or work per repeated event. The resulting
claim was therefore too broad. The replacement audit records state
ownership and measured work for initial build, content edit, dependency edit,
add/remove/rename, configuration change, failure, and recovery separately.

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
Incremental dependency replacement also removes the module from dependencies
it no longer references before adding its current reverse edges. Rust's
`build::deps::get_deps` replaces forward edges but only appends reverse edges,
so a long-lived watch can retain stale dependents after a source stops
referencing a module. That
causes unnecessary recompilation rather than an incorrect build result. The
port performs the complete replacement, and focused build-state coverage
protects both obsolete-edge removal and duplicate prevention.

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
after one-shot and full structural builds. The port represents one-shot,
initial-watch, incremental-watch, and full-watch compilation explicitly and
writes the same marker for one-shot/full builds, but not initial or retained
incremental watches. Focused success/failure builds and the recoverable
config-change watch case retain the behavior; isolated manifests confirm there
are no other missing control-file names.

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
- Namespace packages generate and compile their `.mlmap` during the parse phase,
  before member modules. The interactive compile counter still includes the
  already-prepared namespace marker because it participates in the scheduler
  universe, while the completed phase summary counts only source modules.
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
- Rust explicit clean still rebuilds a portal dependency with the consumer's
  output settings, while the OCaml implementation preserves an independently
  built dependency's compiler metadata and published output tree. The shared
  canonical fixture normalizes the resulting equivalent Belt import before its
  clean-tree assertion. This is a Rust ownership follow-up, not a Windows path
  difference.
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
- `watch --clear-screen` clears an interactive terminal before rebuilds and
  emits the same incremental/full change header plus the post-failure watching
  footer. The PTY gate covers a successful incremental edit, parse failure and
  recovery, and a configuration-triggered full rebuild.
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
  presentation pass, which is now complete.
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
  the terminal-presentation pass.
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
  helper, plus every platform package and its exact binary/notice paths.
  `updateArtifactList.js` temporarily creates only absent executable names for
  local cross-platform inventory and removes its owned placeholders in a
  `finally` block; CI requires every downloaded executable to exist and cannot
  use that convenience to hide a missing artifact. Non-Windows CI runs the
  OCaml unit, focused, and complete canonical
  rewatch suites against the default packaged executable and repeats the
  canonical suite through the installed package. Windows keeps Rust as the
  default until the native OCaml binary is ready rather than publishing an
  unverified executable.
  Regenerating the manifest at checkpoint `2f5067d50a` produced no diff, and
  the command-validation gate passed all 111 cases with physical binaries named
  exactly like CI's `rescript-rust.exe` and `rescript.exe` artifacts.

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

The latest five-run release-build measurement was made from the
measurement-tooling working tree above commit `353479276` in the Linux Docker
environment on the plugged-in, otherwise idle Mac host:

| Implementation | Median wall time | Median peak tree RSS | Median peak tasks |
| --- | ---: | ---: | ---: |
| Rust | 4,548 ms | 1,421,436 KiB | 74 |
| OCaml | 5,406 ms | 1,468,184 KiB | 94 |

The latest completed gate's 1.1887× wall-time ratio and 1.0329× RSS ratio
pass the 1.25× gate. Peak process-tree task count is diagnostic rather than an
acceptance threshold; the observed 1.2703× ratio establishes that the OCaml
subprocess-capture design has materially greater thread fan-out, but does not
by itself attribute the wall-time gap to those threads.
The host was plugged in and otherwise idle for this run. Docker on a Mac still
makes the absolute values less portable than native Linux or dedicated
CI, but all ten interleaved samples were coherent and this is the required
current stable-host acceptance run. Passing this aggregate gate also does not
excuse the clean-build publication probes identified by the filesystem audit
below.

An earlier five-run directional check at `4566147ab`, after the scheduler and
review-driven simplifications, measured 5,300 ms / 1,431,388 KiB for Rust and
6,278 ms / 1,468,592 KiB for OCaml (1.1845× wall time and 1.026× RSS). It
passed the 1.25× gate and again matched all clean, unchanged, and single-edit
compiler-work manifests (1,031/4/6 launches), the complete output file set,
and every stable artifact byte. Individual Rust samples ranged from 4,820 to
6,791 ms and OCaml samples from 5,668 to 7,654 ms because other host work could
interfere. The small ratio change from 1.199× therefore does not demonstrate a
performance improvement; this run establishes directional regression and work
equivalence evidence only. The quiet-host release measurement remains due.

A subsequent three-run smoke check at `6c8446b21`, after making parser-job construction
demand-driven, measured 5,124 ms / 1,437,824 KiB for Rust and 5,661 ms /
1,461,376 KiB for OCaml (1.105× wall time and 1.016× RSS). It again matched
the 1,031/4/6 clean, unchanged, and edit compiler-work manifests plus complete
file and stable-artifact contents. This suggests that eliminating the serial
pre-launch preparation prefix may be material, but three samples on a host with
potential competing work are not an acceptance measurement and do not establish
the improvement's size. The authoritative five-run result above did not
reproduce that ratio and shows no demonstrated material timing improvement from
parser overlap alone.

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

The filesystem audit at commit `d5cead598` reports nearly identical
incremental driver work. Unchanged builds use 2,911 OCaml versus 2,962 Rust
metadata calls, 1,221 versus 1,220 opens, and 162 versus 160 directory scans.
After one source edit the counts are 2,931 versus 2,979 metadata calls, 1,249
versus 1,246 opens, and again 162 versus 160 scans. Clean builds use 13,150
versus 12,018 metadata calls and 14,363 versus 13,492 opens, with 160 versus 158
scans. The remaining clean-build delta is therefore 1,132 metadata and 871 open
calls around the same 1,031 compiler jobs; prior per-process attribution
identifies repeated CMI comparison and case-candidate checks, not extra
compilation or directory-tree discovery. Raw create/remove totals intentionally
remain diagnostic because the drivers use different publication mechanics.

The maintained source-size tool reports 9,225 lines of OCaml-port production
code, including its native C boundary,
and 7,818 lines of Rust production code when Rust telemetry is excluded. Tests
remain separate: OCaml has 7,743 test/fixture lines and 1,032 benchmark-tooling
lines; Rust has 2,773 inline unit-test lines. Blank and comment lines are
reported separately by `bench/source_size.sh` and are not included in these
code counts. The tooling scope includes all six executable shell/JavaScript
benchmark and filesystem-audit scripts rather than only the original clean
build gate and counting script.

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

Compiler completion finalizers now run on the child waiter threads before the
dispatcher is notified. Successful compiler jobs publish artifacts and hash the
published CMI there, allowing independent modules' filesystem work to overlap;
the dispatcher alone applies the resulting digest to shared build state before
releasing dependents. Per-module post-build commands are tracked as subsequent
tasks for the same graph node, so independent hooks also overlap while every
child remains visible to normal cancellation and process-tree cleanup. The node
keeps its scheduler slot until compilation, publication, and all hooks finish,
matching the reference worker lifecycle. A synchronization-based unit test
requires two independent finalizers to enter concurrently, and retained watch
tests cover CMI propagation when a hook fails.

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
inventory scans. Clean-build driver metadata remains 1,132 calls above Rust,
with a concrete per-CMI comparison/candidate shape rather than repeated
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
test-only sections, versus 9,225 OCaml-port production lines including the
native C boundary, or 118.0%. Counting
language-specific tests separately gives 2,773 embedded Rust unit-test lines
and 7,743 OCaml unit/focused test and fixture lines. The OCaml benchmark tooling
adds another 1,032 lines across every executable audit/measurement script. The
shared canonical integration suite is deliberately not charged to either side. These
figures describe maintainability surface, not parity or quality: explicit
interfaces and separate test infrastructure add useful lines rather than
indicating behavioral duplication.
[`bench/source_size.sh`](bench/source_size.sh) preserves the scope and command;
it now reports largest files directly. The current largest production modules
are `watcher.ml` (728 code lines), `build.ml` (651), `process.ml` (584),
`build_preparation.ml` (402), and `cli.ml` (377). The largest test/tooling files
are `check_command_validation.sh` (1,791), `run.sh` (1,067), `unit_tests.ml`
(921), `check_interactive_output.sh` (575), and `config_tests.ml` (494).

The final API-surface audit added explicit interfaces for build state, compile
asset inventory, graph algorithms, package metadata, source-directory output,
warning persistence, CLI commands, compiler fingerprints, formatting, terminal
output/progress, shared platform helpers, and source discovery. Every
production library module now has an interface. Mutable representations are
exposed only where the scheduler and build phases currently need the fields;
directory indexes, warning tables, progress state, and discovery internals
remain abstract. The selected platform implementation files deliberately do
not have separate interfaces because Dune copies one to `platform.ml`, which is
checked against the shared `platform.mli`; the executable entry point also does
not need a library interface. Constraining `Output.Progress` exposed and removed
an accidental optional `force` argument inherited by `tick` from a direct
function alias. The same pass removed the last two production `assert false`
branches: deterministic duplicate-path ordering now compares its two paths
directly, and CLI routing returns an explicit `(globals, command, rest)` only
after finding a command. All 19 OUnit2 groups, the 106-case command gate, and
`dune build @all` pass afterward.

The module-qualification review removed broad `open Build_types` directives
from the build coordinator, package traversal, and package build modules.
Boundary parameter annotations and a few `Build_types.field` selections now
make state ownership explicit there. `build_preparation.ml` retains the open
because nearly every expression constructs or transforms those records and
qualification would obscure the graph algorithm. The dense Cmdliner DSL and
configuration decoder similarly retain their focused opens; no generic utility
module is opened merely to shorten calls.

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
The same pass removed `clean.ml`'s duplicate recursive tree remover in favor of
`File_util.remove_tree`. Command cleanup intentionally ignores removal failures,
and the shared helper also handles dangling links without a misleading
preflight existence check.
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
It is now 640 code lines because retained-watch initialization, incremental
preparation, and command presentation were subsequently added. Its setup,
progress/error reporting, output publication, log and lock finalization,
incremental transition, and build/watch entry points still share one
command-scoped state owner. The largest function is the command transaction
whose local finalizers close logs and the build lock; extracting
pieces would expose that mutable lifetime through callbacks rather than create a
cohesive new owner. A narrow `build.mli` exposes only the command entry points
and translated public exceptions. Revisit this decision if another independent
command lifecycle is added, rather than splitting solely to reduce the count.

The final illegal-state review replaced format's independent `check`, `stdin`,
and file-list fields with a `Format_stdin` versus `Format_files` command value,
so successfully parsed CLI state cannot contain conflicting input modes. The
other recorded candidates represent meaningful absence rather than accidental
partial state: build initialization options support guarded full-rebuild
fallbacks, `None` versus `Some []` distinguishes all features from an explicit
empty selection and unrestricted from no allowed dependents, and configuration
decoding rejects a namespace entry without a namespace before constructing a
usable build. Introducing variants for those cases would rename valid states
without removing an observed failure mode.

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

The watcher now derives both native registrations and content snapshots from
the effective source selection of each local package. Dependency feature
requests are unioned before selecting directories, development sources use the
same local/production rule as build discovery, and root `--filter` matching is
applied before a source enters a snapshot. Edits confined to a disabled feature
or an excluded basename therefore do not run an otherwise empty rebuild or its
post-build hook. Snapshot reconciliation reads only the three package control
files and active configured source trees; it no longer recursively scans every
package root and filters the resulting files afterward. This closes a concrete
source of superfluous filesystem calls rather than merely improving benchmark
timings. Snapshot reconciliation captures the prospective scope before each
build, so a scope activation itself does not cause a duplicate build while a
file created after discovery in that newly active directory still queues a
follow-up build. Unresolved dependency paths within the workspace remain in the
snapshot and temporarily register their nearest existing ancestor, so creating
an installed or workspace package can recover the command without an unrelated
source edit. Candidate watches advance one path component at a time instead of
recursively expanding `node_modules`, and canonical containment prevents them
from escaping the workspace through `..` or symlink components. Active regular
source symlinks also retain a shallow watch on their target parent, including
while the target is temporarily absent, so external atomic rewrites and
delete/recreate cycles remain observable. Their containing directory and its
parent are watched shallowly so moving the watched directory itself remains
observable across filesystem backends. Initial native handles are installed
before compilation, and registration is followed by a fresh snapshot, closing
both the initial-build and refresh handoff windows. The 106-case differential
gate covers dependency installation and candidate fallback, external symlink
target replacement, the delayed-compiler race, included, filter-excluded, and
feature-disabled live edits, signal-safe lock waiting, and recovery from
malformed root or dependency configuration.

All file writers now report flush and close failures. Generated package
metadata, source-directory metadata, and namespace maps use same-directory
temporary files so their public paths always contain a complete old or new
value, with termination deferred only across temporary ownership and final
publication. Formatting deliberately writes through the existing user-owned
file instead: replacing that directory entry would break symlinks and hard
links and could discard ownership, ACLs, or extended attributes. OUnit coverage
retains complete replacement and permissions for generated files, propagates a
buffered `/dev/full` failure where that Unix device exists, and verifies source
symlink and hard-link behavior. The stdin formatter installs cleanup ownership
before deferred termination can be delivered.

### Review of later Rust fixes

Three later Rust fixes were audited explicitly against the port:

- [#8639](https://github.com/rescript-lang/rescript/pull/8639) keeps GenType's
  parser locations and `-bs-project-root` in the same canonical Windows path
  form. The OCaml data flow already has that invariant: loading a config
  canonicalizes its path and package root, and both the parse working directory
  and compile project-root argument derive from that same value. The existing
  canonical-root unit check covers the platform-independent part; a native
  Windows build through an 8.3 alias remains in the VM handoff because Linux
  cannot provide that path form.
- [#8640](https://github.com/rescript-lang/rescript/pull/8640) lets `clean`
  operate when the module graph is invalid. Cleanup discovery now returns raw
  implementation paths before duplicate/interface graph validation, while
  ordinary build discovery retains those validations. OUnit creates two
  colliding modules and requires both in-source/out-of-source owned outputs to
  be removed successfully.
- [#8641](https://github.com/rescript-lang/rescript/pull/8641) keeps watch alive
  after full-rebuild initialization errors. The OCaml watch error boundary
  already retains the prior watch state and reports configuration, source,
  package, process, and filesystem failures. Differential lifecycle cases
  corrupt and repair root/dependency configuration and install a previously
  missing dependency without restarting the watcher.

## Release inventories

These are the three separately maintained inventories required by the final
code-quality gate. They describe the current implementation, but remain open
until the native-platform and final whole-port reviews confirm that no entry is
missing.

### Compatibility behavior retained despite appearing odd or inconsistent

- A JSON `path` field is accepted only when it is a string and is then ignored
  in favor of the file actually read. This preserves the public shape leaked by
  Rust's internal configuration type. The 297-case configuration table and a
  focused decoder test retain it; a future schema version can remove it only as
  an announced configuration change.
- Duplicate known fields in typed configuration objects are rejected, while
  duplicate keys decoded through JSON-map-backed fields keep the last value.
  This is a decoder implementation consequence rather than a useful language
  rule. Fifteen focused duplicate-key cases plus the full configuration table
  retain it; a future cleanup should choose one documented rule and migrate both
  implementations together.
- `--no-timing` accepts an optional boolean, so `build --no-timing folder`
  consumes `folder` as that value and rejects it instead of treating it as the
  project path. `cli_tests.ml` and the executable differential gate retain this
  parse result; changing it requires a coordinated CLI compatibility decision.
- `--version` is global only before an explicit subcommand, and clustered short
  flags use the first `h` or `V` to decide help versus version. Cmdliner routing
  tests and executable differential cases retain both orders. The normalization
  layer should remain isolated so this behavior can be removed if the CLI is
  intentionally simplified later.
- `lib/bs/build.ninja` is an empty compatibility marker rather than the actual
  build plan. It is written by one-shot and structural rebuilds but not by
  retained initial/incremental watch builds. Focused build/watch assertions and
  artifact manifests cover the distinction; editor tooling should eventually
  use an explicitly named invalidation marker.
- Compiler jobs publish generated outputs as they succeed. A prior attempt to
  stage only previously absent outputs was removed: because `bsc` writes to the
  public package-output path, the file was observable while compilation was in
  progress and then hidden after the job completed. That could expose a root
  output while a successfully compiled dependency was hidden in a sidecar. The
  canonical clean-to-watch test now requires the root and imported dependency
  outputs to become observable coherently.

### Rust bugs and simple inefficiencies corrected by the port

- Formatting schedules individual source files up to the process bound. Rust
  splits files into chunks of four times the CPU count, runs chunks in parallel,
  and formats every file inside a chunk serially; projects below that threshold
  therefore use only one formatter process at a time. The OCaml scheduler keeps
  the same bounded external-process model without that accidental serialization,
  and `format_tests.ml` requires two independent formatter jobs to overlap.
- Malformed input and missing-resource paths return contextual errors instead
  of panicking or hanging. This covers unsupported JSX versions, empty PPX
  commands, unresolved regular dependencies, sources outside a project,
  disappearing source/AST files, stale compiler paths, mismatched dependency
  identities, invalid watch reinitialization, and empty, missing, or failing
  after-build commands. The individual source locations, expected behavior, and
  focused coverage are catalogued under **Rust panic follow-ups** above; future
  Rust fixes should change the differential expectations from exit 101 or a
  bounded hang to the same normal error class.
- Namespace entries without a namespace are rejected rather than silently
  ignored, and compiler-flag strings do not manufacture empty argv elements
  from repeated whitespace. Configuration unit and differential cases retain
  both low-risk validations; upstream can adopt them without changing valid
  configurations.
- Dependency permissions are checked only on traversed graph edges and every
  denied active edge is reported on stderr with current field/file names. The
  command-validation fixtures retain Rust's dormant-edge false rejection and
  incomplete/obsolete diagnostic beside the corrected result; the Rust pass can
  reuse the already resolved graph and aggregate all violations.
- Watch filtering uses the same positive regex meaning during discovery and
  event handling. The synchronized differential watch case proves Rust's
  inverted event predicate and the corrected rebuild; Rust should remove the
  negation in `watcher.rs::matches_filter`.
- Replacing a module's dependencies removes obsolete reverse edges instead of
  only appending new ones. Focused retained-state tests cover removal and
  duplicate prevention; Rust can rebuild those reverse entries when replacing
  its forward set to avoid unnecessary later recompilation.
- An inconclusive Windows lock-owner probe preserves the lock rather than
  leaking an internal process-launch exception. Platform tests cover the
  conservative result; Rust can translate probe failures at the same boundary.
- Compiler fingerprints are not rewritten when their contents are unchanged,
  avoiding an unnecessary write without weakening invalidation. Focused
  compiler-info tests cover stability and every fingerprint input; Rust can
  conditionally replace the file after comparing its contents.
- Stale runtime overrides are rejected before parser/compiler work, instead of
  producing repeated misleading compiler failures. The differential command
  gate also checks that no compiler work starts; Rust can apply the same
  toolchain preflight used for its compiler path.
- Working compiler artifacts from renamed or deleted sources are removed after
  preserving the source-located diagnostic for the current command. Focused
  rename/delete cases and artifact manifests cover both diagnostics and final
  absence; the detailed missing-dot defect and upstream repair path are under
  **Rust cleanup follow-up** above.
- Missing generated JavaScript is repaired on the next build, and package-output
  format/path/suffix changes invalidate old outputs. Focused restart and output
  migration tests cover issue #7728 and PR #8540 respectively; both behaviors
  can be ported to Rust through its compile-state fingerprint/inventory.
- Visible packages that publish the same namespace artifact fail during graph
  initialization instead of allowing include-path order to select the wrong
  map. A focused fixture requires both packages in the diagnostic; Rust should
  add the same visibility-scoped uniqueness check before compilation.

### Possible post-parity performance improvements absent from Rust

- Replace the two pipe-reader threads plus waiter thread created for every
  child with centrally serviced libuv process pipes. The latest stable clean
  gate measured 129 peak process-tree tasks for OCaml versus 78 for Rust, so
  the additional fan-out is real. The earlier fixed-reader experiment removed
  the per-child threads without improving wall time, while persistent compiler
  domains brought the total ratio to 1.096x despite retaining them. A shared
  event loop could therefore reduce thread creation, stack memory,
  synchronization, and per-child capture buffers, but it is a high-risk
  process-lifecycle change. It must preserve separate ordered stdout/stderr
  capture, completion only after exit and both EOFs, descendant-held-pipe
  cancellation, deterministic scheduling, and concurrent publication. On
  Windows it requires libuv overlapped pipes while retaining suspended Job
  Object assignment and whole-tree termination; replacing `Spawn` entirely is
  not justified unless that ownership can be reproduced cleanly. Profile driver
  CPU, context switches, and ready-work idle intervals first, then require the
  full process failure/cancellation suite, native Windows validation, exact
  work/artifact equivalence, clean/watch resource gates, and stable timing.
- Persist a validated discovery/build inventory across separate CLI processes.
  This is a hypothesis aimed at configuration and filesystem setup in repeated
  short-lived builds. It needs a versioned content fingerprint, conservative
  fallback, Windows path review, exact work/artifact equivalence, filesystem
  tracing, and clean/unchanged/edit timing before adoption.
- Keep compiler workers alive across jobs, or eventually integrate the compiler
  in-process. This targets process startup and repeated compiler initialization,
  but carries high compiler-global-state and failure-isolation risk and remains
  outside this port. Prototype it only after profiling process startup, then run
  the full failure/recovery, output-equivalence, memory, and Windows process-tree
  gates.
- Parallelize independent configuration parsing or directory inventory with
  OCaml domains. This is useful only if profiling shows CPU saturation rather
  than filesystem latency; it adds synchronization and deterministic-diagnostic
  risk. Require a measured hot phase, stable single-thread fallback, exact
  diagnostics/artifacts, resource measurements, and native Windows validation.

The current clean-build per-CMI equality and case-candidate investigation is
not in the last list because it is work-equivalence analysis against Rust, not
a novel optimization. The source-filter compatibility gap is likewise a
required behavior decision, not a performance proposal.

## Known gaps

- By explicit project decision, exact Rust-regex semantics are not required for
  `--filter`. It uses `Re.Perl`, matching common Rust-regex
  syntax and retaining linear-time matching. It remains a documented subset:
  Unicode properties and inline modes, Python-style named groups, possessive
  quantifiers, and class-set algebra accepted by Rust fail visibly in OCaml.
  Known Re-only syntax, ranges, nested/collating classes, and divergent in-class
  escapes are rejected before compilation so they cannot silently select the
  wrong source set. Because the matcher is byte-oriented, shared patterns can
  still select non-ASCII basenames differently, including dot, hexadecimal
  escapes, shorthand classes, and literal classes; the differential suite
  records the supported boundary. This accepted divergence is not a completion
  blocker; silently different meanings for syntax known to diverge remain a
  bug.
- Native Windows verification remains incomplete. Configuration schema
  acceptance, argument projection, source-level validation, and ordinary
  redirected-output inventory are complete, including semantic `-v`/`-vv`
  events. Incremental filesystem work is at or
  below Rust, while the explained clean-build driver delta remains documented
  for future optimization.

## Completed compatibility evidence

The following results are retained because they explain how the port closes
specific compatibility risks; they are not remaining gaps.

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
- Namespace dependency resolution now retains each complete AST-header entry
  until the graph resolver and ignores an entry equal to the source package's
  own namespace. The compiler records `OL.Coordinate` as the imprecise header
  dependency `OL`; expanding that marker into every `OL` member created false
  edges and could turn a valid `Geometry -> Extent -> Coordinate` graph into an
  `Extent <-> Geometry` cycle. A focused clean-build regression combines the
  ordinary `Coordinate` dependency, the qualified `OL.Coordinate` reference,
  and the reverse `Geometry -> Extent` edge. Resolver candidates using the
  current namespace suffix now also apply the same declared-package visibility
  check as other candidates.
- A source that refers to a same-package module *only* through its namespace
  still exposes a limitation in the compiler dependency header: the exact
  member name is unavailable, so neither implementation can schedule that CMI
  deterministically on a clean tree. A synthetic reproduction fails in Rust as
  well when `Extent` contains only `OL.Coordinate` and `Coordinate-OL.cmi` does
  not yet exist. Fixing this reliably belongs in the compiler dependency
  format/extractor rather than guessing source dependencies in rewatch; keep it
  in the final corrected-Rust/future-work inventory.
- Visible packages that publish the same compiler namespace artifact are now
  rejected during graph initialization. The check is scoped to a package and
  its direct dependency include paths, so unrelated dependency branches may
  reuse a namespace. This deliberately improves on the current Rust behavior,
  which reaches compilation and can load the wrong namespace map or report an
  unrelated missing module. Focused coverage requires the early diagnostic to
  identify the shared namespace and both packages.
- Parser and compiler arguments now follow Rust's phase-specific ordering, and
  `compiler-args` reports the parser's actual path relative to `lib/bs`.
  The command reads the source and constructs parser arguments before resolving
  dependency includes, then resolves regular dependencies before the runtime;
  focused failure tests retain that observable ordering. Its source lookup
  preserves a final symlink as a path in the project rather than moving project
  ownership to the symlink target. The JSON contract is checked structurally,
  including the order of each argument array; insignificant serializer
  whitespace is not treated as command behavior.
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
  inventory and is now covered together with live phase presentation.
- CLI normalization now retains two non-obvious Clap behaviors: a version flag
  is global only before an explicit subcommand, and the optional boolean value
  for `--no-timing` consumes a following folder token and rejects it as a
  non-boolean. Unit and executable-level tests cover both forms. Keep the
  latter in the final inventory of compatibility behavior that appears odd.
- Clustered global short flags are recognized by their characters rather than
  a fixed spelling list. This preserves arbitrarily repeated verbosity and
  mixed display clusters before an explicit command; the first `h` or `V`
  determines help versus version, while command-local help only wins if its
  `h` precedes an otherwise invalid `V`. OUnit and differential executable
  cases retain both orders and the `-vvvvv` routing form.
- Verbosity selection now rejects combining any `-v`/`--verbose` occurrence
  with any `-q`/`--quiet` occurrence, matching Clap's mutually exclusive
  verbosity modes instead of subtracting the two counts. OUnit and differential
  executable cases cover both implicit and explicit build routing.
- Redirected config diagnostics now use the same single leading blank line as
  Rust. The differential output gate combines deprecated aliases, a known
  unsupported field, and an unknown field so spacing and ordering are compared
  exactly rather than inferred from separate schema tests. Color enablement now
  also follows the reference CLI-color policy: a capable terminal respects
  `CLICOLOR`, `NO_COLOR`, and `TERM`, while nonzero `CLICOLOR_FORCE` enables
  ANSI output even when redirected. Each diagnostic is wrapped independently,
  matching the three Rust call sites. Pure policy tests and a byte-exact forced
  color differential case retain the CI-only behavior that the earlier source
  audit missed.
- Parser subprocess results are consumed completely before the parse phase is
  reported. Successful warnings are buffered until after the `Parsed` line,
  while a failed batch retains every independent parser diagnostic before
  returning. A PTY wrapper compares the warning/phase order directly with Rust,
  and the 107-case command gate requires both errors from a two-file failing
  batch without imposing Rust's hash-map-dependent order. Parsing already used
  one bounded global subprocess batch, so closing this gap did not require a
  different scheduler.
- Interactive builds now also emit Rust-shaped cleanup, parse, and compile
  completion lines with three-step initial-build numbering, two-step watch
  rebuild numbering, phase-specific emojis, counts, and two-decimal timing.
  A retained PTY gate now runs both watchers, changes a source, and exactly
  compares normalized initial and incremental phase/final-status frames. It
  exposed and fixed the initial OCaml watch label from generic `Finished
  compilation` to Rust's `Finished initial compilation`. Redirected output
  remains unchanged.
- Interactive output parity now includes throttled parsing and compilation
  spinner frames, module-based position/length counters, clear-screen rebuild
  headers, and the post-failure watching footer. Animation runs from the
  existing scheduler poll loop rather than another thread, is capped at roughly
  12 frames per second, and is disabled entirely for redirected and quiet
  output. The PTY gate uses a module with both an interface and implementation
  to ensure the parse total counts modules rather than subprocesses.
- Configuration diagnostics are reported during the initial watch build but
  not repeated by either incremental edits or full structural rebuilds. The PTY
  lifecycle check now performs both kinds of rebuild and requires the
  deprecation diagnostic to occur exactly once.
- Initial watch configuration validation runs inside the acquired watch-lock
  scope. A second watcher therefore reports the active owner before attempting
  to parse a concurrently malformed configuration, while the lock finalizer
  still releases ownership if initial validation fails. A differential command
  case retains the lock-before-config ordering. Its live-owner probe creates a
  separate process with each tested executable's basename: release CI names the
  reference binary `rescript-rust.exe`, so one shared `rescript` dummy would be
  rejected by Rust and could erase the lock before the OCaml assertion ran.
- Explicit `clean` now validates and collects the complete dependency-first
  package plan before mutating the filesystem, then removes compiler trees and
  generated outputs in distinct phases. The previous per-package interleaving
  produced the right final files and redirected package lines but left an
  interactive terminal blank; that exposed a phase-ordering hole in the earlier
  outcome-oriented source audit. The PTY gate now compares both implementations'
  five `[1/2]`/`[2/2]` progress frames exactly after normalizing timing and also
  requires quiet interactive clean to stay silent. The 106-case command gate
  retains redirected and validation behavior, while the focused runner retains
  cleanup ownership and final artifacts.
- `clean` does not resolve or hash `bsc` and does not resolve the runtime,
  because neither installation is read while deleting known build artifacts.
  Rust currently performs both preflights and therefore panics on a missing
  compiler or rejects a missing runtime without cleaning. This is retained as a
  small reference-side inefficiency rather than imposing an unrelated
  toolchain requirement on the OCaml command. Differential cases require the
  OCaml command to remove its owned compiler tree in both conditions and retain
  the current Rust outcomes for visibility.
- `clean` inventories source-owned outputs without requiring the sources to
  form a valid compilation module graph. Rust deletes `lib/bs` and `lib/ocaml`,
  then reconstructs that graph; duplicate module names make it return early and
  leave the corresponding generated JavaScript behind. The OCaml command
  completes both cleanup phases for the same project. A differential case
  retains the reference's partial-clean result and the port's complete-clean
  result so this corrected failure mode remains explicit.
- Positive verbosity now reports Rust's semantic project-context, package
  discovery, AST generation, and interface/implementation compilation events.
  `-vv` additionally reports the initially dirty modules and the completed
  scheduler universe. The differential gate compares normalized event
  multisets rather than imposing Rayon completion order on OCaml. Building the
  universe also now schedules only initially dirty modules and their transitive
  dependents; clean unrelated modules are not inserted as no-op scheduler
  nodes.
- `watch` now uses long-lived libuv filesystem-event handles for the root and
  recursively resolved local dependency directories. Existing source content
  events use libuv's path directly; structural or ambiguous events use the
  snapshot/diff reconciliation path, so correctness does not depend on every
  platform supplying precise rename payloads or event ordering. Generated
  output events are ignored before reconciliation. Handles are retained across
  builds and only added or closed when directory topology changes; a focused
  resource test covers stable, added, and removed counts.
  The former polling loop remains a runtime fallback if native setup fails. An
  injected native-constructor failure drives the actual fallback loop in OUnit,
  changes a source after the initial build, and requires exactly one
  path-specific incremental rebuild plus lock and signal cleanup.
- Watch snapshots treat only `rescript.json` and legacy `bsconfig.json` as
  package-root control files. `package.json` is not a reference watch trigger;
  hashing it added work while allowing unrelated metadata edits to request an
  extra full rebuild. A focused scope test retains the two accepted names and
  rejects `package.json`.
- Generated outputs are published as their compiler subprocesses succeed, so a
  module does not become visible before a dependency whose CMI allowed it to
  compile. Ordinary build cleanup and explicit `clean` still remove abandoned
  sidecars created by earlier experimental OCaml binaries. Sweeping is limited
  to tool-specific suffixes whose underlying path is a recognized generated
  JavaScript or source-map name; unrelated user files are preserved and covered
  by a focused filesystem test.
- The focused integration runner creates a nested source directory and source
  as one topology-change batch and requires the new output. Separately,
  `native_watcher_tests.ml` refreshes after creating an empty nested directory
  and asserts that the handle count increases, so dynamic registration is
  proved without a timing-based sleep.
- Local source dependencies linked from `node_modules` into a sibling package
  are recursively built with dependency feature selections and cycle
  protection; prebuilt packages are accepted through their `lib/ocaml` include
  path.
- Package resolution checks the active package, invocation root, and workspace
  root `node_modules` directories in priority order. Only standalone projects
  continue searching ancestor `node_modules` directories. Bare sibling and
  `packages/` directories are deliberately not candidates: accepting them
  silently expanded the graph and allowed `clean` to delete outputs outside
  the dependency graph selected by the reference implementation. Focused
  coverage requires an unlinked `packages/dep` build and clean to fail while
  preserving that package's JavaScript and compiler artifacts.
## Native Windows implementation and handoff

- Windows support is required before this port can be considered complete. It
  cannot be executed in the current Linux environment, but it must still be
  designed and cross-built where possible. Unix subprocess creation uses the
  `spawn` library, while Windows uses a narrow native owner around the same
  `CreateProcess` behavior, working directory, environment, pipe inheritance,
  argument quoting, and PATH/PATHEXT resolution. It creates each child suspended,
  assigns it to a new Job Object, and only then resumes it, so no descendant can
  escape during launch. The job remains a valid tree identity after `waitpid`
  closes the direct process handle, allowing cancellation to terminate
  descendants that still own capture pipes without relying on a reusable numeric
  PID. A failed job assignment attempts to terminate the still-suspended child
  and makes the launch fail instead of falling back to a racy `taskkill`; an
  operating-system failure to terminate it is reported as the cleanup operation
  without waiting indefinitely on inherited capture pipes. Normal completion
  closes the job without terminating detached descendants; cancellation calls
  `TerminateJobObject` explicitly. Unix retains process-group cleanup. Normal
  completion, partial launch, scheduler failure, and cancellation all release
  the platform process owner after reader and waiter threads finish. Native
  Windows must still verify this lifecycle in the VM, including the
  descendant-held-pipe cancellation case.
  Watch lock/process probing and native watcher behavior also need a Windows
  cross-build and runtime verification. Shared filesystem logic uses `Filename`
  operations rather than embedded `/` or `\\` separators; Unix-only test cases
  are being isolated or replaced with portable helpers. The maintained
  `tests/check_windows_job_stub.sh` check compiles the native Job Object branch
  with a Windows-targeting C compiler, its matching OCaml headers, and warnings
  as errors; this catches Windows API and OCaml C-interface mistakes without
  claiming native runtime coverage.
  The repository Makefile's compiler source stamp includes C and header files,
  so changing this native owner cannot leave an older promoted/package binary
  in place while `make compiler` incorrectly reports it current.
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
  comparison are behind that boundary. The abstract process owner also keeps
  Unix process groups and Windows Job Objects out of shared scheduling code.
  The unselected Windows implementation is type-checked against the contract in
  Linux unit builds. Pipe creation uses `Spawn.safe_pipe` behind that boundary,
  while portable reader ownership stays in `Process`. The cross-platform native
  watcher has its own narrow interface over libuv rather than duplicating
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

### Native Windows handoff

The implementation checkpoint for the Windows session is `60231ebaa`. Use a
native checkout on the VM's NTFS volume, OCaml 5.5 through the repository's
opam setup, and the Cygwin Bash installed with that toolchain. The checkpoint
already selects `platform_windows.ml` through Dune, compiles the Job Object C
owner, provides volume-and-file-index identities for replaced watch directories,
and type-checks the Windows module in the cross-platform unit suite. It also
runs dependency-ready compiler tasks and their publication callbacks on bounded
persistent domains while the main domain retains graph admission and diagnostic
ordering.

Build and run the portable gates from the repository root:

```sh
opam exec -- make
rust=packages/@rescript/win32-x64/bin/rescript.exe
ocaml=_build/default/rewatch-ocaml/rescript_ocaml.exe
opam exec -- dune runtest tests/rewatch_ounit_tests
bash rewatch-ocaml/tests/check_rust_test_coverage.sh --require-complete
bash rewatch-ocaml/tests/check_canonical_test_coverage.sh
bash rewatch-ocaml/tests/check_config_acceptance.sh "$rust" "$ocaml"
bash rewatch-ocaml/tests/check_command_validation.sh "$rust" "$ocaml"
bash rewatch-ocaml/tests/check_interactive_output.sh "$rust" "$ocaml"
bash rewatch-ocaml/tests/check_verbose_output.sh "$rust" "$ocaml"
sh rewatch-ocaml/tests/run.sh "$ocaml"
bash rewatch/tests/suite.sh "$ocaml"
```

All commands must finish without failures, leave `rewatch/testrepo` unchanged,
and leave no rewatch process, lock, capture file, or temporary output behind.
Convert custom absolute `RESCRIPT_BSC_EXE` and `RESCRIPT_RUNTIME` values with
`cygpath -w` if Cygwin does not translate them when launching the native binary.

The native-only acceptance pass must exercise: concurrent suspended launches,
Job Object assignment, and artifact publication from worker domains;
cancellation during launch and before and after the direct child exits,
including a descendant that retains a capture pipe; stdout/stderr EOF and
handle cleanup without double release;
active, stale, malformed, and concurrently replaced build/watch locks; `cmd.exe`
post-build quoting; PATH/PATHEXT lookup; spaces, Unicode, drive, UNC, mixed-case,
and 8.3 project paths; native recursive events, atomic-save replacement, new and
removed directories, replacement of a watched directory at the same pathname,
edits during a build, and polling fallback. Run through
the promoted/package layout as well as the Dune executable so sibling `bsc.exe`
and runtime discovery are covered. Only after this pass should Windows CI and
the Windows npm package switch their default `rescript.exe` to the OCaml port.

## Dependency decisions

- `spawn` is accepted: it is a narrow, MIT-licensed Jane Street package with
  explicit Linux, macOS, and Windows support. It replaces bespoke fork/exec/cwd
  code and materially reduces process-launch risk. Production constraints pin
  the reviewed v0.17.0 release exactly.
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
  independently. Production constraints pin the reviewed 2.1.1 release.
- JSON deriving is not currently justified. The config loader must retain raw
  keys to distinguish deprecated, known-unsupported, and forward-compatible
  unknown fields; generated codecs would still require substantial custom
  validation around the derived layer.
- `re` 1.14.0 is accepted for `--filter`. It is actively maintained by the
  OCaml organization, pure OCaml, supports native Windows, and is distributed
  under LGPL-2.1-or-later with the OCaml linking exception. Its DFA matcher has
  the same linear-time safety property as Rust's `regex` crate. `Re.Perl`
  closes the user-relevant `Str` gaps for alternation, capturing and
  non-capturing groups, shorthand classes, and conventional repetition.
  `source_filter.ml` owns validation, compilation, basename selection, and the
  abstract compiled type, so invalid raw patterns cannot enter build or watch
  APIs and structural rebuilds do not recompile the expression. Unicode
  properties and inline modes, Python-style named groups, possessive
  quantifiers, and character-class algebra remain documented, visibly rejected
  engine differences. A compatibility check also rejects known Re-only quoting,
  comment, anchor, control/octal escape, range, nested/collating class, and
  divergent in-class escape forms rather than accepting them with a different
  meaning. Representative shared and rejected syntax has direct unit and
  differential build coverage. Exact Rust-regex semantics are explicitly out of
  scope by project decision, so this visible subset does not require a Rust or
  PCRE2 bridge. Production constraints pin the reviewed 1.14.0 release.
- `luv` 0.5.14 is accepted for native filesystem events. It is a thin
  MIT-licensed binding that vendors and statically links libuv, supports the
  required Linux/macOS/Windows targets, and keeps the executable free of a
  runtime libuv dependency. Its latest release was September 2024, so the
  binding's cadence is quieter than ideal; the narrow `Native_watcher` boundary
  keeps replacement or localized vendoring practical if maintenance becomes a
  problem. The pinned version and upstream status must be reviewed during
  dependency updates. On Linux ARM64, static inclusion increased the promoted
  executable from approximately 3.6 MiB to 5.2 MiB; `ldd` still reports only
  libc and libm. Production constraints pin the reviewed 0.5.14 release. Every
  non-Windows binary package that carries the OCaml executable includes
  `THIRD_PARTY_NOTICES_REWATCH.md`, covering Cmdliner, Yojson, Re, Spawn, Luv,
  libuv, ctypes, and integers, plus Re's full license and linking exception in
  `RE_LICENSE.md`; a dry-run package build confirms both files are included.
  Luv's vendored-libuv rule currently drops the ARM64 musl compiler's
  `-mno-outline-atomics` flag, so CI applies a repository-local opam package
  patch that restores the flag only for that target. We should contribute that
  build fix upstream. Luv 0.5.14 vendors libuv 1.48.0 while newer 1.x releases
  exist; evaluating and contributing a vendored-libuv update upstream is a
  separate follow-up, not part of this compatibility port.

## Next actions

The command-by-command phase and ordering audit is complete for build, clean,
format, compiler-args, and watch. It compared intermediate state transitions and
observable phases rather than inferring parity from final files or exit status.
The closing evidence is 136/136 reviewed Rust unit tests with no gaps, 297
configuration cases, 111 command-validation cases, the interactive phase gate,
all 20 OUnit groups, and the canonical OCaml suite. The audit's material fixes
and intentional corrections are recorded above.

A subsequent external source review found that the OCaml scheduler published a
successfully changed interface CMI but lost its dependent invalidation when the
implementation then failed. The next retained watch build could consequently
skip a stale dependent. Rust does not share this defect: it compares the CMI
around the complete module attempt, propagates that change independently of the
compile result, and persists dirtiness for modules left unscheduled after a
failure. The OCaml scheduler now refreshes CMI state immediately after a
successful interface and incremental preparation preserves already-propagated
dirtiness. A canonical watch regression changes an interface, observes the
implementation failure, repairs the implementation, and verifies that the
dependent is recompiled and reports its newly invalid type use.

The same review found three watch-scope defects. Recursive registration and
polling snapshots skipped every directory named `lib`, so configured sources
under `src/lib` compiled initially but were never observed again. This was an
OCaml-only defect: Rust filters only `lib/bs` and `lib/ocaml` artifact paths.
Both OCaml traversal paths now use that precise artifact rule, with native and
canonical watch regressions for a valid nested `lib` source.

The root `--filter` was also applied to every watched package even though
dependency discovery intentionally receives no filter. Source watch roots now
retain package ownership and apply the filter only to the root package. The
exact dependency-suppression defect was OCaml-only; Rust does observe a
non-matching dependency edit, but direct source inspection found a separate
inverted event-filter predicate that can suppress the matching root edit. The
OCaml behavior follows discovery semantics rather than retaining either bug,
and a workspace watch regression covers a dependency edit under a root filter.

Finally, OCaml registered the parent of a source symlink's target but discarded
the target identity, so an in-place target edit outside the source tree could
be ignored when the target did not have a ReScript extension. Snapshots now
retain exact target paths and use their native content events to request
reconciliation. Rust registers no external target watch, so the underlying
behavioral defect is shared; the OCaml port intentionally corrects it. A watch
regression covers a `.res` symlink whose target has a `.txt` name.

Overlapping source declarations previously remembered only that a directory
had been visited, not whether its module and GenType traversal had been shallow
or recursive. A later recursive declaration could therefore fail to discover
any descendants. The discovery tables now retain traversal coverage and allow
a shallow visit to be upgraded without rediscovering files at the shared root.
This defect was OCaml-only: Rust scans each source declaration independently
and merges the resulting path maps. Focused tests retain both descendant module
discovery and GenType directories for the shallow-then-recursive ordering.

Build preparation previously blocked only the one cycle selected for the user
diagnostic. A second independent cycle then reached the subprocess scheduler's
acyclic-graph precondition, preventing unrelated work from running and replacing
the source-level diagnostic with an internal scheduler error. Preparation now
uses a linear scheduling pass to identify every cyclic component and its
transitive dependents, then searches that residual graph once for the shortest
deterministic cycle to present. Rust does not share this defect: its compiler
scheduler dispatches available work first and diagnoses the remaining cycle
only when scheduling stalls. A canonical fixture with two independent cycles
verifies that an unrelated module compiles, all cycle members remain blocked,
and only the normal circular-dependency diagnostic is emitted.

The review's two graph-construction performance findings were OCaml-only.
Unresolved dependency names previously scanned every global module looking for
a matching namespace, including the common case of runtime modules absent from
the project graph. Preparation now builds a namespace-to-module-key index once
and retains it across incremental watch builds, making both missing-namespace
checks and namespace expansion indexed lookups. Rust already uses hash-backed
module membership and explicit namespace entries, so it does not share this
quadratic path.

Reverse dependency edges were also stored as lists, making insertion into a
high-fan-out dependency quadratic through repeated membership checks. They now
use the standard OCaml string set for logarithmic insertion and removal with no
new dependency, while forward dependencies remain sorted lists where their
ordering is useful. Rust already stores reverse dependencies in `AHashSet`, so
it does not share this defect. These are algorithmic complexity corrections;
their effect will be included in the deferred stable performance gate rather
than claimed from noisy timing here.

All eight initial external-review findings are resolved. The affected OUnit,
canonical watch/build, 111-case command-validation, Rust-test-inventory, and
formatting gates pass.

A follow-up source review found three interactions in those fixes. CMI refresh
was still tied to successful completion of the complete publication callback,
so an implementation that published a changed CMI and then failed its
`js-post-build` command could lose dependent invalidation. CMI refresh and
propagation now run before post-build commands, while the module itself remains
dirty until all of its work succeeds. Rust already propagates the CMI result of
failed module attempts, so it does not share this defect. A retained-watch
regression exercises a changed inferred interface, failing post-build command,
retry, and stale dependent type error.

Retained dirtiness could also override the cycle-blocking decision made during
the next preparation, allowing a previously failing module to compile after an
edit introduced a cycle. Cycle admission is now an independent requirement:
blocked modules are never dirty or scheduled, regardless of retained recovery
state. This interaction was specific to the OCaml retained-state model. A live
watch regression first retains a compiler failure, introduces a cycle, and
verifies that the prior JavaScript output is not replaced with an import from
the blocked module.

Finally, the first multiple-cycle fix searched for a shortest cycle from every
node even on ordinary acyclic builds, then repeated that search while blocking
cycles. The linear residual pass described above removes that regression and
reserves the more expensive shortest-cycle search for actual error reporting.
Rust's scheduling-based cycle detection does not have the OCaml regression.
Clean-build dirty-source membership also used repeated linear list searches;
preparation now builds hash-backed membership tables once for dirty paths and
removed module names. This was another OCaml-only quadratic path.

The next source-only review found two remaining interactions at the parallel
publication boundary. A successful CMI copy followed by a failing CMJ, source,
or JavaScript mirror copy escaped the worker callback before the scheduler
could propagate the CMI change. Worker completion now records a normal
publication outcome, including exceptions, and the scheduler consumes that
outcome and refreshes the CMI before reporting the publication error. A focused
retained-watch regression forces a source-mirror failure after changing an
inferred interface, repairs it, and requires the stale dependent type error on
retry. Rust does not have this recoverable stale-state path, although several
of its later artifact copies instead panic or are ignored as separately
inventoried.

Parallel publication callbacks also updated the retained package output
inventory through one unsynchronized hash table. Publication file I/O remains
parallel, but the scheduler thread now records whichever public outputs exist
after each attempt. A deterministic scheduler test verifies that this callback
runs on the dispatch thread, grows the shared inventory through multiple hash
table resizes, and recompiles a retained dependent after a late publication
failure. Rust likewise confines shared build-state mutation to its dispatcher,
so it did not share the unsafe update.

Artifact copies no longer materialize each complete CMI, CMT, CMJ, source, or
JavaScript file as an OCaml string. They stream through one bounded 64 KiB
buffer, and equality checks compare reusable buffers without allocating a pair
of slices for every chunk. This preserves the existing portable channel-based
implementation and publication semantics while bounding per-copy live memory.
The full clean-build effect remains to be measured under the stable performance
gate rather than inferred from allocation behavior.

Parser subprocess preparation no longer reads every dirty source and constructs
the complete argument list before launching the first `bsc`. The generic
process runner now builds each job when a scheduler slot is filled, launches it
immediately, and prepares later jobs while earlier children are already
running. This retains deterministic input-order results and portable external
process parallelism without adding domains. A synchronization-based unit test
requires the first child to start before the second job can be constructed.
The change removes the concrete serial-prefix difference from Rust's parallel
parse jobs, but its wall-time contribution remains unclaimed until a stable
measurement can isolate it.

The follow-up review also identified larger simplification candidates for the
final non-comment cleanup. Package build now requires the graph, source set,
compile configuration, output ownership, source mtimes, and cleanup inventory
prepared by `Build_preparation`; it no longer carries a second fallback policy
that reloads configuration, resolves dependencies, rediscovers sources, or
recomputes cleanup. A missing prepared root is an internal-state error, while a
resolved dependency without a build configuration remains a prebuilt include
directory rather than being recursively prepared. Raw dependency names now
have one owner in each prepared global-module record, while `Build_state` owns
only resolved forward and reverse graph edges. The redundant global raw table,
unused build-state raw/deps-dirty fields, and unused mutable dependency list on
source-discovery records have been removed; incremental AST updates change the
global-module record and rebuild its resolved edges directly. Deferred compile
cleanup now has one execution owner in the build transaction's protected
finalizer. The compiler scheduler no longer executes those actions early, and
the individual cleanup closures no longer carry defensive one-shot flags; the
transaction consumes and clears its pending action list on success, reported
failure, or an unexpected exception. Parser and compiler argument construction
is now shared as pure functions by actual build jobs and `compiler-args`;
callers explicitly supply the intentional differences in watch mode, interface
presence, dependency include directories, and GenType dependency arguments.
The closed internal visitation and CLI-routing states now use normal variants.
The remaining polymorphic variants are required at Yojson, Cmdliner, Re, and
Luv API boundaries; no internally owned open case set remains. Remaining
measured-performance candidates are per-child reader/waiter threads and
buffers, retained-build reconstruction, and residual publication allocation/GC.
They require profiling before architectural work; the cycle, dirty-membership,
worker-lifecycle, and parser-launch differences were concrete enough to correct
immediately.

The non-comment cleanup removed the obsolete `.rewatch-pending` and
`.rewatch-backup` recognition, cleanup scans, explicit deletion paths, and
tests. No supported implementation creates those staging files after output
publication was aligned with Rust, so retaining migration code would add
filesystem work and make unrelated files appear owned by Rewatch.
The cleanup plan no longer retains the now-unused full file inventory, and its
source traversal stops at non-recursive directories when no inventory consumer
exists. The watch helper was also renamed from the ambiguous "build directory"
predicate to `is_compiler_artifact_directory`, reflecting that valid configured
source directories remain watchable.

The remaining non-comment quality audit revisited naming, module qualification,
interfaces, dead code, and the recorded illegal-state candidates after the
review fixes. The new graph fields and watch-scope values have specific owners
and names, all production modules remain interface-constrained, and the full
build is warning-free. No further module split is warranted: the larger build
and watcher modules still each own one lifecycle, while another extraction
would mainly expose that mutable lifetime through callbacks. The broad comment
and documentation pass remains deliberately deferred until immediately before
the final release-quality gate.

The next source-only review found six more correctness gaps. All were specific
to the OCaml port rather than confirmed Rust defects:

- A successfully published IAST now makes its module compilation-pending even
  when another source's parse error prevents the compilation phase from
  starting. Freshness considers both the implementation AST and interface IAST,
  and the pending bit survives the failed attempt. Rust already derives this
  state from both source artifacts.
- Direct native-watch events snapshot their changed contents before starting a
  build. An atomic replacement that arrives while compilation is running can
  therefore no longer be mistaken for the completed build's baseline. Rust's
  queued event handling did not expose the stale-output sequence.
- The command-wide warning-error override now reaches every local dependency;
  the existing locality policy still removes it at installed-package
  boundaries. Rust already applies that command-wide/local rule.
- Native watch handles retain filesystem identity as well as pathname and are
  reinstalled when a directory is replaced at the same path. Unix uses device
  and inode identity; the Windows backend uses volume serial and file index.
  Rust's parent-directory event followed by full watch re-registration already
  covers this replacement case.
- Stale generated outputs are classified before deletion, then JavaScript,
  source map, and working mirrors are removed as one output family. Rust's
  source-based cleanup already removes JavaScript and map together.
- Source discovery now exposes a non-owning presence inventory in addition to
  the cleanup inventory. It follows configured directory symlinks for freshness
  without authorizing recursive deletion through them, avoiding needless
  unchanged rebuilds. Rust's compile-asset state does not couple these two
  responsibilities.

The same round consolidated dependency selection in `package_resolution.ml`.
Build, clean, implicit format, and watch topology now share candidate order,
canonical locality, first-package selection, duplicate reporting, and config
loading, while watch retains its deliberately recoverable handling of missing
or malformed packages. Prepared compiler context, compile assets, and module
state form one required value, and persistent graph/artifact state is separated
structurally from per-attempt diagnostics and work queues. Package compilation
accepts a prepared package instead of looking it up through an optional phase
state. GenType arguments reuse the dependency paths chosen for that graph.

The concrete performance recommendations were also implemented. Retained
builds reuse a normalized source-path index, accumulate changed-module parse
failures in one pass, restrict freshness work to explicitly changed paths,
skip unchanged namespace reconstruction, and avoid reading source text in the
driver when no PPX filtering needs it. Package-level compiler arguments and
GenType dependency arguments are constructed once instead of once per module.
The remaining subprocess design was profiled rather than replaced: clean builds
still peak near 95 tasks because each child has two stream readers and one
waiter, but bounded pipe capture has no growth across retained builds.

A further source-only review found five actionable gaps, all now fixed at
implementation checkpoint `1ab6b3493`. Failed parses and warning-bearing parses
remain pending across incremental attempts instead of allowing an old AST to
make a later build appear successful. Namespace maps are represented in the
dependency graph, so adding or removing a namespaced module invalidates
unchanged namespace consumers. The Windows Job Object stub reacquires its
custom-block pointer after a GC-capable allocation. Explicit relative programs
such as `./tool` are never redirected through `PATH`, and after-build hooks
stream both output channels while running rather than buffering indefinitely.
Focused regressions cover each behavior; the native Windows fix remains subject
to the planned VM run.

The same batch completed the low-risk cleanup and source-derived performance
recommendations. Build, watch, and format share feature-request aggregation
through a normal closed variant. Attempt construction has one initializer, and
compiler publication uses explicit outcomes for complete and partially
published attempts. Incremental builds skip whole-graph cycle analysis when
dependency edges are unchanged and the retained graph was acyclic. Stale
artifact cleanup indexes source mappings once. The compiler scheduler computes
the affected reverse closure from lightweight candidates and constructs
compiler/publication callbacks only for modules in that closure; warning
retention still covers every source. The 20 OUnit2 groups, focused integration
runner, 111-case command-validation gate, and formatting/build checks pass at
this checkpoint.

Two review proposals remained deliberately measurement-dependent at this
checkpoint. The process runner still used two concurrent pipe readers and one
waiter per child; replacing this with a shared libuv event loop had to preserve
deadlock-free dual-stream draining, cancellation, process group or Job Object
ownership, and native Windows behavior. Publication used bounded 64 KiB
streaming buffers but did not pool buffers across files in one module. Later
measurements below distinguish thread fan-out from serialized worker ownership.
A broad new module-identity wrapper was also not introduced: the concrete
ambiguity sites now use explicit dependency, namespace-map, and build-state node
types, while wrapping every remaining path/name string would currently add
conversion ceremony without removing a known invalid state. These decisions
must be reported and reconsidered in the final quality review.

The follow-up behavioral review found one additional watch-only traversal gap.
An installed dependency can itself resolve a regular dependency back into the
local workspace. Build discovery already traversed that graph and compiled the
local package, but watch discovery stopped at the installed package and never
registered the local package's sources. Watch topology now traverses regular
dependencies through non-local packages while adding source/config watches
only for packages classified as local. A focused fixture builds
`root -> installed -> local workspace`, edits the local source, and requires
its generated JavaScript to change without another trigger. Rust already
traverses the full dependency graph before selecting local watch roots, so this
was an OCaml-port defect rather than a shared Rust issue. The resource/quality
review found no other concrete P0-P3 issue at this checkpoint, and the targeted
source-only follow-up approved both the traversal fix and its regression.

The latest idle-host five-run measurement at `7e6d17bc0` was coherent but did
not pass the timing threshold: OCaml measured 5.680 s versus Rust at 4.145 s
(1.370x). Median peak process-tree RSS remained close at 1,592,496 versus
1,573,116 KiB, while OCaml used 94 versus 74 peak tasks. Clean, unchanged, and
single-edit compiler-work counts and the complete/stable artifact sets still
matched exactly. A same-host five-run control using the earlier `a4b0728b2f`
OCaml binary measured 5.710 s versus Rust at 4.124 s (1.385x). No Rust,
compiler, runtime, or Belt source changed between those checkpoints. This rules
out a performance regression introduced by the intervening OCaml changes—the
current binary was slightly faster—but the current environment now gives Rust
a materially larger advantage than the earlier passing run. The 1.25x
completion gate therefore remains open and requires investigation or a later
coherent reproduction within the threshold; matching work alone is not enough.

A same-host historical-binary investigation did not find an OCaml regression
behind that changed ratio. The pre-pipe `0585d07dc4` binary measured 5.886 s
against a 4.250 s Rust median, and the earlier `353479276f` binary measured
5.864 s against 4.173 s. A subsequent idle-host rerun compared the exact
`54a7273d4f` source checkpoint for which the original post-pipe gate recorded
1.190x with the current binary. In separate five-run Rust comparisons, the old
checkpoint measured 5.704 s versus 4.146 s (1.376x), while current measured
5.674 s versus 4.115 s (1.379x). A direct five-run interleaved old/current
OCaml comparison measured 5.744 s for the old checkpoint and 5.673 s for
current, making current about 1.2% faster under the same conditions. The
current gate also retained exact compiler-work counts and identical complete
and stable artifact sets.

The 1.19x-to-1.38x change is therefore reproducible for the old checkpoint as
well as current and cannot have originated in the intervening port changes.
The same Rust source and binary are used; only its measured runtime changed
relative to the historical gate. The reason for that environment-sensitive
difference remains unexplained, so the 1.25x completion gate stays open even
though the source-regression hypothesis is closed.

A later five-run gate at `00db0a010` measured 5.733 s OCaml versus 4.106 s
Rust (1.396x), with identical clean, unchanged, and edited compiler work and
identical complete and stable artifact sets. Peak RSS remained close at
1,564,736 versus 1,531,416 KiB. Process tracing confirmed that the portable
capture implementation creates three threads for each child: the clean build
made 3,095 thread-clone calls in addition to 1,032 process clones, while Rust
made 1,043 total clone calls. A measured Unix experiment drained both pipes
with `select` on a fixed worker pool. It removed the peak-task difference and
reduced per-build thread creation to the pool size, but its smoke median was
still 5.583 s versus 4.046 s (1.380x). The experiment was therefore discarded:
thread churn is real resource overhead, but it does not explain the wall-time
gap and the extra lifecycle machinery did not earn its maintenance cost.
An untraced process-tree sample attributed nearly identical compiler user time
(8.22 s Rust versus 8.49 s OCaml) but higher OCaml system time (14.61 s versus
11.89 s). `spawn` was confirmed to use `CLONE_VM|CLONE_VFORK`; replacing it for
a supposedly faster fork API is therefore not an opportunity. Reducing the
10-child limit to eight was neutral, while raising it to twelve made both parse
and compile phases slower. A second experiment replaced channel copying with a
runtime-lock-releasing C implementation backed by Linux `sendfile`; its 5.47 s
median was likewise indistinguishable and the added platform code was removed.
The strongest remaining architectural hypothesis is that Rust launches each
child and publishes its artifacts inside persistent Rayon workers, whereas the
OCaml dispatcher performs every spawn serially and its system threads share one
OCaml domain for publication. Testing that requires a worker design that owns
the complete spawn/capture/wait/publication lifetime, not another isolated pipe
or copy optimization, and must preserve the documented Windows Job Object
lifecycle.

Checkpoint `60231ebaa` confirms that hypothesis. The dependency graph remains
owned by the main domain, while a bounded set of persistent domains now owns
each ready task's spawn, concurrent pipe capture, wait, publication callback,
and process-handle release. A task returning another phase is admitted through
the same dependency scheduler, and fatal failure or command cancellation marks
active children once before the owning workers join and release them. OCaml
delivers signal handlers on the main domain, so workers do not temporarily
replace the process-wide handlers used by Windows; a child completing launch
after cancellation observes the stopped pool as it registers and terminates
its own process tree. Partial pool creation also stops and joins workers that
were already created.

The powered, idle-host five-run interleaved gate measured 4.460 s OCaml versus
4.069 s Rust, or 1.096x. Median summed process-tree RSS was 1,660,044 KiB versus
1,568,236 KiB, or 1.058x. Both pass the 1.25x gate. Clean, unchanged, and edit
work counts remained exactly 1031/512/7/512/40/1, 4/2/0/2/1/0, and
6/3/0/3/1/0 respectively, and complete file sets plus stable artifacts were
identical. Peak task count increased to 129 versus 78 because the persistent
domains coexist with the portable per-child reader/waiter threads; the earlier
fixed-reader experiment showed that reducing those threads alone did not
improve wall time. OUnit covers fatal worker-finalizer cancellation while
another process is active. The focused runner, 111 command cases, exact
interactive and verbose gates, formatting, and the complete 48-test canonical
suite pass; the faster scheduler exposed and fixed a stdout/stderr phase-order
race by flushing completed parse output before diagnostics and compile progress.
At documentation checkpoint `d1c576507`, `opam exec -- make test-all` also
passed the repository-wide gate, including promotion of the OCaml rewatch
binary, compiler and runtime tests, both GenType projects, analysis, tools, and
the packaged canonical rewatch suite. It left the worktree clean and no watcher
or subprocess helper running.

The latest retained-watch gate at `d1c3ca9732` was coherent and passed: 118 ms
OCaml versus 123 ms Rust, exactly seven parser and seven compiler calls per
implementation, identical generated output, stable file descriptors and task
counts, and no RSS growth. The complete canonical 48-test rewatch suite also
passed against the same OCaml executable and left no watcher or testrepo change
behind. `opam exec -- make test-all` subsequently passed the repository-wide
gate at `9cfde3697`, including formatting, compiler and runtime tests, both
GenType projects, analysis and reanalyze, tools, and the packaged OCaml rewatch
canonical suite. It likewise left the worktree clean and no watcher running.

At the earlier `a4b0728b2f` checkpoint on the quiet, powered host, the five-run
interleaved release gate measured a 5.455 s OCaml median against 4.644 s Rust
(1.175x), with 1,516,904 KiB versus 1,504,696 KiB summed process-tree RSS.
Compiler work matched exactly for clean, unchanged, and edited builds, and
stable artifacts were byte-identical. The seven-edit retained-watch gate
measured 119 ms OCaml versus 120 ms Rust, exactly seven parser and compiler
calls per implementation, stable descriptors/tasks, and no RSS growth. Both
were within their documented thresholds.

The deterministic non-Windows packaging gate was repeated at checkpoint
`88b171cc46`. A targeted `static`-profile promotion produced a statically
linked `rescript.exe`; `scripts/checkCompilerExes.js` confirmed that the
promoted platform binary was the current Dune output. Regenerating
`packages/artifacts.json` produced no diff, and Yarn's package dry run contained
both `rescript.exe` and `rescript-rust.exe` together with
`THIRD_PARTY_NOTICES_REWATCH.md` and `RE_LICENSE.md`. CI repeats the manifest
generation only after downloading every platform artifact and rejects a
missing declared executable instead of creating a local placeholder. The
coverage build likewise installs the instrumented OCaml executable as the
non-Windows default without expecting a nonexistent Dune-installed `rescript`
binary.

The latest source-only review found seven further dependency, recovery, and
watch reconciliation gaps. They are fixed at checkpoints `fe440144c` and
`5225a4f97`. Cycle membership now controls dispatch eligibility without
clearing persistent compilation dirtiness, including invalidations received
while blocked. Restart freshness follows namespace-map members, and namespace
entry modules have the implicit edge required by their `-open` argument.
Duplicate-package diagnostics use relative paths when contained and absolute
paths otherwise. Resolution caches package identity separately from each
declaration's feature request. Structural events prioritize explicitly tracked
symlink targets even when the target has a generated-output extension. Stale
output planning derives prior public JavaScript and map candidates from AST
provenance before compiler artifacts are removed. The last cleanup limitation
also exists in the reference Rust ordering and is recorded as an inherited bug
that the OCaml implementation fixes.

The associated maintainability changes are complete at `f5553335c`. Build,
watch, format, and clean share dependency selection and feature aggregation
through `package_traversal.ml`; cached dependency identity remains separate
from request-specific kind and features. Diagnostic path presentation has a
permissive API distinct from containment checks. Captured and streaming
subprocess modes share one lifetime owner. Required artifact deletion reports
errors, while only explicitly best-effort lock cleanup suppresses them.
Configuration decoding now fingerprints the exact bytes it parsed rather than
rereading a potentially changed file later. Focused regressions additionally
restart a namespace consumer after a failed member-interface change and edit a
feature directory selected through combined regular and development requests.

The remaining suggestions from that review do not identify missing current
work. Scheduler setup is global rather than repeated per package, and the
process path already returns immediately for an empty task list. Namespace
references are persistent graph nodes with resolved edges, so scheduler setup
does not repeat namespace expansion. The measured persistent-domain change
closes serialized spawn and publication while retaining the fixed child limit.
Reusing CMI bytes or digests across publication would require changing the
publication result and streaming-copy contract; reducing the two reader threads
plus waiter or changing the child ceiling likewise affects cancellation and
native Windows ownership. Those remaining changes stay explicitly
profile-dependent in the future-performance inventory for consideration after
Windows validation.

A final scheduler handoff audit corrected an OCaml 5 signal assumption. Pending
signals are process-wide and may be handled by a worker domain, rather than
being confined to the initial domain. One-shot and watch handlers now record
requests in atomic state, and the owning scheduler domain raises an explicit
fatal interruption from its poll point. This eliminates asynchronous unwinding
between child creation and registration; the scheduler then cancels every
owned process tree. Windows also serializes temporary signal-handler
replacement because it has no Unix-style per-thread signal mask; this prevents
concurrent artifact publication from restoring process-wide handlers out of
order. The selected
Unix build, the unselected Windows module typecheck, all 20 OUnit groups, and
the focused integration suite pass; native signal delivery remains part of the
planned Windows VM validation.

At cooperative-interruption checkpoint `0175a05ce`, `opam exec -- make
test-all` passed the repository-wide gate. This included OCaml/ReScript/JS/Rust
formatting, compiler and runtime tests, both GenType projects, analysis and
reanalyze, tools, and the packaged OCaml executable running all 48 canonical
rewatch scenarios. The run left the worktree clean and no watcher or subprocess
helper alive.

A subsequent source-only review found four more correctness gaps. Removing an
interface now removes its published and working CMTI instead of leaving editor
analysis pointed at a deleted interface. Native watcher content events first
check whether a source path is also the target of another source symlink, so an
edit rebuilds every alias through snapshot reconciliation. Configuration
loading canonicalizes the requested project directory without following a
symlinked `rescript.json` into a different project root. Failed process-tree
termination no longer enters an unconditional join: ownership moves to a
detached reaper, the worker reports the cancellation error, and Windows process
handles remain live until the existing waiter eventually completes.

The inherited compiler-fingerprint omission identified by the same review is
also fixed in the OCaml implementation. Effective root JSX and experimental
arguments now participate in every package's compiler metadata, so a root
configuration change cannot reuse dependency output compiled with old inherited
settings. The Rust implementation has the same omission and should receive an
equivalent fix independently.

The associated cleanup uses one source-activation operation for build and watch
scope, reserves formatter traversal work before recursion without changing
first-path package selection, and replaces three overlapping preliminary-parse
tables with one normal variant describing success, failure, or reuse of an
existing AST. Test-only graph/compiler-info APIs and an unused freshness helper
were removed. Stale-output probing consults its existing inventory first, while
cycle canonicalization rotates once at the smallest node and reuses normalized
adjacency lists. Stable retained candidate/package caching remains a possible
whole-project edit optimization, but requires measured evidence before adding a
second cache-invalidation lifecycle. Process-pool reuse, capture-thread changes,
and the 32-child policy likewise remain explicitly profiling- and Windows-gated.

On the quiet host, the five-run interleaved release gate for this batch measured
4.874 s OCaml versus 4.520 s Rust (1.078x) and 1,602,712 versus 1,495,644 KiB
median summed process-tree RSS (1.071x). Clean, unchanged, and edit compiler
work matched exactly at 1031/512/7/512/40/1, 4/2/0/2/1/0, and
6/3/0/3/1/0. Complete file sets and byte-stable generated artifacts were
identical. The focused integration suite, all 20 OUnit groups, the interactive
output gate, and all 111 command-validation cases also pass.

At review-fix checkpoint `895807b478`, `opam exec -- make test-all` passed the
complete repository gate. This included formatting, compiler and runtime tests,
both GenType projects, analysis and reanalyze, tools, and the installed OCaml
rewatch executable running all canonical build, failure, clean, format, and
watch scenarios. The run left the worktree clean and no watcher or compiler
subprocess alive.

The subsequent non-comment maintainability pass is complete through checkpoint
`c7780d735`. It removed unused retained state and test-only production APIs,
made namespace and lifecycle stages explicit normal variants, and centralized
artifact naming, namespace naming, AST-header decoding, path/config lookup,
effective compiler options, source activation, and common string/file
operations. Watch scope, snapshot entries, source references, lock ownership,
and subprocess launch ownership now use named records instead of positional
tuples or loosely related options. Package attempt preparation was split into
focused parsing and compilation modules, leaving `package_build.ml` as the
small orchestration owner. The public configuration interface now re-exports
`Config_types` directly instead of repeating every type definition.

The same pass consolidated shared test fixtures and split the two largest
catch-all OUnit cases by subsystem and configuration concern. The suite now
reports 32 independently named groups, so one failure no longer suppresses
unrelated assertions. `Mutex.protect` was deliberately not adopted because it
is unavailable in the supported OCaml 5.0 baseline; the exception-safe local
mutex helper remains. A fresh `opam exec -- make test-all` at `c7780d735`
passed formatting, compiler/runtime, GenType, analysis, tools, and the complete
installed-package rewatch suite on Linux. This is the checkpoint for the next
fresh source review before comments are added.

A dead-code audit used Reanalyze master at
`ad9894832fcd33bb0e1f799e1573d1b9b4f2c9af`. Build the current Rewatch CMTs and
the analyzer, then run it with every Rewatch interface treated as an external
entry boundary:

```sh
opam exec -- dune build rewatch-ocaml tests/rewatch_ounit_tests
live_interfaces=$(find rewatch-ocaml -maxdepth 1 -name '*.mli' -print | paste -sd, -)
/path/to/reanalyze/_build/default/src/Reanalyze.exe \
  -dce-cmt _build/default/rewatch-ocaml \
  -live-paths "$live_interfaces" \
  -live-names watch,run_files,format_stdin
```

The audit removed unused one-shot process environment parameters, made
always-supplied notifier/wait arguments mandatory, deleted an unused standalone
dependency-candidate wrapper, narrowed internal compiler-argument and format
helpers, and removed exception re-exports that callers did not need. Snapshot
equality now names every field that determines a watcher change instead of
depending on polymorphic comparison.

The live names are command entry points called by the separately compiled
executable, which is outside the scanned directory. Unmodified Reanalyze
currently reports ten false positives on OCaml 5.5 CMTs: the test-supplied
`max_jobs`, `on_complete`, and `bisect_enabled` optional arguments, and the
`Publication_failure`, `Config_types.Error`, and `Process.Interrupted`
cross-module exceptions. Reanalyze also reports the private `Package_error` and
`Build_failure` exception aliases in `Build` and `Package_build`; their local
handlers use those aliases, but the analyzer does not connect alias patterns to
the original exception declarations. Each reported argument and exception was
checked against its source call, construction, and handler. Any new report
outside that reviewed set fails the gate. A temporary analyzer experiment
confirmed that accepting declaration locations within the corresponding
signature item removes the optional-argument reports; it was not added to this
repository because changing Reanalyze is outside the port.

The source audit after `960ae779f3` found six further cleanup opportunities.
Buffered stdin-format writes now use the close-error-propagating file writer,
and missing copy sources fail independently of the destination-directory
policy. Package traversal now has one graph walk that owns visited packages,
resolved edges, locality, and aggregated feature requests; build and format use
its strict resolver, while watch supplies an explicit recovery resolver for
broken and not-yet-installed dependencies. Build materialization reuses those
resolved edges instead of resolving them a second time.

Published CMI/CMT timestamps and dependent invalidation now pass through
`Build_state` transitions for both source modules and namespace maps. Warning
cleanup records published AST paths when the warning occurs instead of
repeatedly searching the package module list. Parse-error classification and
dependency-kind variants have one owner. Finally, the idempotent
`Signal_restore` primitive owns restoration and exception precedence for lock,
atomic-file, formatting, and subprocess setup while resource cleanup remains
local. This remains compatible with OCaml 5.0 and does not use
`Mutex.protect`.

This audit batch passes all 32 OUnit groups, the focused integration suite, all
297 configuration cases, all 111 command-validation cases, interactive-output
parity, the reviewed Reanalyze gate, and `opam exec -- make test-all`, including
the installed OCaml binary running the canonical build/clean/format/watch suite.

The following ownership audit removed another set of overlapping contracts.
Subprocess capture descriptors now have exactly one cleanup owner, including
reader-thread creation failures. Build cleanup is drained before execution,
attempts every registered action, and cannot prevent compiler-log finalization;
the first failure is reported after the remaining cleanup has run. One-shot and
watch orchestration now derive process polling and watch behavior from the
compilation kind instead of accepting redundant parameters.

Retained graph state and cleanup queues are abstract outside `Build_types`.
Callers use owning lookup, update, iteration, and drain operations, while package
parsing and compilation receive their already-prepared context explicitly.
Filesystem inspection treats only `ENOENT` and `ENOTDIR` as absence in artifact
and watcher state; other inspection failures remain visible. Managed artifact
suffixes, source kinds, and GenType field names each have one definition.
Watcher snapshots distinguish ordinary files, present dependency candidates,
and missing candidates with a normal variant rather than fabricated timestamps
and digest strings. Source scanning likewise uses named records and an
`Implementation | Interface` variant instead of positional tuples and booleans.
Feature requests are registered once per traversal edge.

`String_util.contains` no longer allocates a substring at every candidate
position. OCaml 5.5 supplies `String.includes`, but the supported OCaml 5.0
baseline does not. The compatibility implementation therefore uses a small
allocation-free character scan. Its searched affixes are short PPX markers, so
the simpler scan avoids both regular-expression setup and the data structures
of a general-purpose substring algorithm. Focused tests cover empty, exact,
overlapping, prefix, suffix, absent, and longer-than-input cases.

This checkpoint passes all 33 OUnit groups, the focused integration suite, all
297 configuration cases, all 111 command-validation cases, interactive and
verbose output parity, and the reviewed Reanalyze gate. The complete
`opam exec -- make test-all` repository gate also passed across the ownership
and representation changes; after selecting the final simple substring scan,
the affected build, unit, focused integration, configuration, command, and
output-parity gates were rerun successfully.

The quiet-host release benchmark at checkpoint `f8c0a7bc0` measured a 4.871 s
OCaml median versus 4.594 s for Rust (1.060x), with 1,652,256 versus 1,562,792
KiB median summed process-tree RSS (1.057x). Clean, unchanged, and edit compiler
work again matched exactly at 1031/512/7/512/40/1, 4/2/0/2/1/0, and
6/3/0/3/1/0. Complete file sets and byte-stable generated artifacts were
identical. This supersedes the earlier 1.078x checkpoint as the most recent
stable baseline; it does not by itself prove that later cleanup remains neutral,
so the reordered performance gate will repeat the measurement after the state
split and final implementation cleanup.

The next source review identified duplicated namespace publication and several
filesystem-policy copies. Namespace and ordinary module publication now share
the CMI comparison/copy operation and typed partial-failure bookkeeping, so a
later namespace artifact-copy failure cannot discard a CMI invalidation.
Namespace-map generation and dependency-graph membership use one member
selection policy. Source and artifact traversal catch only missing-path errors
at the individual filesystem operation; callback, cancellation, permission, and
I/O failures remain visible. Source discovery admits only regular files as
compiler inputs, preventing a source-named FIFO or device from blocking the
compiler. Recursive artifact inventory likewise treats only a missing path or
dangling link as empty.

Freshness now acquires cached or filesystem metadata separately and applies one
`source >= artifact` policy in both paths. Capture-pipe creation and diagnostic
prefix stripping have one implementation, and watcher event classification
uses its existing directory predicate. Build artifact/log finalization and
build-result presentation have dedicated owners, reducing the central build
transaction without exposing individual mutable-state getters. Focused tests
cover namespace CMI bookkeeping after a later copy failure, shared namespace
membership, and exclusion of source-named FIFOs.

This review-fix batch passes 34 OUnit groups, the focused integration suite,
all 297 configuration cases, all 111 command-validation cases, interactive and
verbose output parity, and the full `opam exec -- make test-all` repository
gate. Reanalyze reports only the same ten reviewed false positives; removing the
now-unused `copy_file_if_changed` wrapper prevented the shared publication
cleanup from adding an eleventh report.

The former `Build_types.t` state container is now split by lifetime.
`Build_types` contains passive graph, parse-result, and prepared-context values;
the abstract `Build_session` owns graph indexes, readiness, pending parses,
warning state, and output inventories retained by watch mode; and
`Build_attempt` owns one build's diagnostics, counters, scheduled work, and
cleanup lifecycle. Retained builds now store the session directly instead of a
previous attempt record. Output-presence inventories remain session state, but
the cleanup decisions derived from them belong only to the current attempt.
This removes the previous shallow record copy and makes session readiness and
the attempt's freshness mode explicit at construction. Two focused
state-lifetime tests bring
the OUnit suite to 36 groups: they verify that diagnostics, failures, and
cleanup actions do not cross attempts while the output-presence inventory does.
The split passes the focused integration and parity gates and the complete
`opam exec -- make test-all` repository gate. Reanalyze still reports exactly
the same ten previously reviewed false positives and no new dead code.

The first internal code-quality round replaced the last internal optional-bool
state encodings. Source discovery and watcher snapshot traversal now share a
normal-variant `Traversal_coverage` policy that distinguishes a first shallow
visit, a first recursive visit, a shallow-to-recursive upgrade, and work that
is already covered. The Windows process-list parser likewise reports
`Process_found`, `Process_absent`, or `Malformed_output`, preserving its
conservative lock policy without encoding three meanings as `bool option`.
Two traversal-policy tests bring the OUnit suite to 38 groups; the focused
integration suite also passes.

The following ownership round split subprocess lifecycle from scheduling.
`Process_child` now owns capture descriptors, reader and waiter threads,
partially launched processes, completion notification, cancellation, and
platform-process release behind an abstract running-child type. `Process`
remains the public facade and retains the two deliberately separate scheduling
policies: ordered parallel lists and dependency graphs. Callers therefore keep
the same API, while neither scheduler can inspect or reconstruct child-resource
ownership. The complete OUnit suite and focused OCaml integration suite pass,
and the dead-code audit adds no report beyond the previously reviewed analyzer
limitations.

The filesystem-contract round replaced production `Sys.file_exists` decisions
with one `File_util.exists` operation that treats only a genuinely missing path
or missing parent as absence. Build preparation, freshness, artifact cleanup,
dependency lookup, toolchain discovery, configuration discovery, and watcher
reconciliation now propagate permission and I/O failures instead of silently
changing policy. Executable probing inside the platform PATH search remains
deliberately best-effort. Configuration source classification likewise ignores
only paths that disappeared during inspection. The 38 OUnit groups and focused
integration suite pass after the change.

The naming and interface round removed an unused broad `Build_types` open and
made artifact provenance, namespace compilation work, post-build work, and
platform hook commands named records instead of positional tuples. The record
types preserve the distinction between a subprocess result that still needs
artifact publication and a namespace job whose publication has already been
finalized into build-state updates. The Unix build, dormant Windows platform
module type-check, 38 OUnit groups, focused integration suite, and current
ten-false-positive dead-code baseline all pass.

The attempt-lifecycle round moved pending namespace jobs, compiler candidates,
artifact cleanup, and initialized compiler logs behind owning `Build_attempt`
operations. Scheduling and finalization now consume these collections exactly
once instead of directly reading and mutating shared lists and tables. This
enforces the lifecycle contract without adding getters for ordinary phase data.
A focused drain-once test brings the suite to 39 OUnit groups; the integration
suite and dead-code baseline remain clean.

The final phase-boundary audit made prepared build context a required value
after initialization. Full preparation returns the context together with its
cycle result; retained preparation validates and supplies the existing context;
package traversal and compiler scheduling then receive it explicitly. The two
exception-raising session accessors for recovering prepared state were removed,
so later phases cannot conceal an initialization-order mistake behind a lookup.
All 39 OUnit groups and the focused integration suite pass.

1. Run multiple rounds of the final implementation/code-quality gate, including
   ownership, naming, duplication, dead-code, illegal-state, filesystem,
   resource-lifecycle, and platform-boundary audits.
2. Run the stable performance and work-equivalence gate while the host is idle.
3. Consolidate maintained documentation, without starting the broad source
   comment pass.
4. Wait for reviewer availability, then address final whole-port external review
   rounds until no material finding remains.
5. Run the final release-quality gate.
6. Perform the broad comment pass last. Comments should start with why, provide
   enough context for non-specialists, and stand on their own rather than using
   Rust as the explanation unless compatibility itself is the reason. Follow it
   with a narrow formatting/build check.

Native Windows runtime verification, Windows watcher/process/lock validation,
and switching Windows CI or packages to the OCaml executable are explicitly
outside this PR. The implementation and cross-platform type-checking remain so
the later Windows PR starts from the same architecture.

The future filesystem-performance ideas documented above do not block
completion of the compatibility port.

The OCaml unit-test sources now live in `tests/rewatch_ounit_tests` and use the
repository's existing OUnit2 dependency, leaving production modules in
`rewatch-ocaml`. Consolidation exposed and fixed a leaked `RESCRIPT_BSC_EXE`
mutation that the former per-executable process isolation had hidden. Moving
the shared shell suite from `rewatch/tests` to a future `tests/rewatch_tests`
location is intentionally outside this PR because it would create unrelated
Rust, CI, and tooling path churn.
