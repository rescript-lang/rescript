# OCaml rewatch port status

Reference Rust implementation: `2e532c7f6587d4201befd00ced516e267c90fe73`.
The chronological implementation and review record is archived in
[`IMPLEMENTATION_HISTORY.md`](IMPLEMENTATION_HISTORY.md); the detailed behavior
inventory remains in [`PARITY_CHECKLIST.md`](PARITY_CHECKLIST.md).

## Current state

The OCaml implementation is the experimental default `rescript` executable in
Linux and macOS npm packages. The unchanged Rust implementation remains
available there as `rescript-rust`. Windows continues to ship and test the Rust
executable; native Windows validation and switching its package default are a
separate future PR.

Feature, diagnostic, work, and artifact parity are complete for the current
non-Windows scope. OpenTelemetry is deliberately omitted. Exact Rust regular
expression semantics for `--filter` are also not a goal: the documented common
subset uses the maintained pure-OCaml `re` library and rejects known divergent
syntax rather than silently selecting different files.

The port is organized by ownership, not by mechanically reproducing Rust file
boundaries. In particular:

- `Build_session` owns graph, freshness, warning, and inventory state retained
  across watch attempts.
- `Build_attempt` owns diagnostics, scheduled work, logs, and cleanup for one
  attempt.
- `Package_traversal`, `Build_preparation`, `Package_build`, and
  `Package_compilation` separate package selection, stable graph preparation,
  per-attempt parsing, and compilation/publication.
- `Process_child` owns one child lifecycle; `Process` owns the ordered-list and
  dependency-graph schedulers.
- `Platform` and `Native_watcher` contain operating-system and libuv boundaries.

## Latest quality evidence

At checkpoint `e258487586`, the implementation passed:

- all 39 OUnit2 rewatch groups;
- the focused OCaml integration, configuration, command-validation,
  interactive-output, and verbose-output gates;
- the complete installed-package canonical rewatch suite;
- OCaml 5.0-compatible compilation, selected Unix compilation, and dormant
  Windows implementation type-checking;
- formatting and the reviewed Reanalyze dead-code gate.

The temporary pull-request OPAM cache override has also been removed from the
shared setup action and both workflows; only push runs now save caches.

The last repository-wide `make test-all` run passed formatting, compiler and
runtime tests, GenType, analysis, tools, and the canonical rewatch suite until
the missing-source watcher case exposed an over-strict `realpath`. That defect
was fixed at `e258487586`; its focused case and the complete canonical rewatch
suite then passed. The final release gate will repeat `make test-all` as one
uninterrupted run.

Reanalyze master (`ad9894832fcd33bb0e1f799e1573d1b9b4f2c9af`) currently reports
only ten reviewed analyzer limitations: optional arguments exercised by tests
or external entry points, cross-module exception construction/handling, and
private exception aliases. Any additional report fails the no-dead-code gate.
Build bytecode CMTs before the audit with:

```sh
opam exec -- dune build rewatch-ocaml/rewatch_ocaml_lib.cma
live_interfaces=$(find rewatch-ocaml -maxdepth 1 -name '*.mli' -print | paste -sd, -)
/path/to/reanalyze/_build/default/src/Reanalyze.exe \
  -dce-cmt _build/default/rewatch-ocaml \
  -live-paths "$live_interfaces" \
  -live-names watch,run_files,format_stdin
```

## Performance and equivalence checkpoint

The powered, idle-host seven-run release gate at `e258487586` measured:

| Measure | Rust | OCaml | OCaml/Rust |
|---|---:|---:|---:|
| Clean-build median wall time | 4.445 s | 4.673 s | 1.051x |
| Median summed process-tree RSS | 1,653,820 KiB | 1,730,560 KiB | 1.046x |
| Peak process-tree tasks | 74 | 127 | 1.716x |
| Retained-watch edit median | 141 ms | 150 ms | 1.064x |

The authoritative wall-time and RSS limit is 1.25x. Retained-watch latency has
a separate 1.50x limit because notification and polling granularity dominate
short measurements. File descriptors, task counts, and RSS stayed stable over
seven retained edits.

Compiler work matched exactly:

| Scenario | total `bsc` | parse | namespace | compile | interface | PPX |
|---|---:|---:|---:|---:|---:|---:|
| Clean, both implementations | 1031 | 512 | 7 | 512 | 40 | 1 |
| Unchanged, both implementations | 4 | 2 | 0 | 2 | 1 | 0 |
| One edit, both implementations | 6 | 3 | 0 | 3 | 1 | 0 |

Complete post-build file sets and all byte-stable generated artifacts were
identical. The retained gate also observed exactly seven parser and seven
compiler calls for each implementation and byte-identical generated output.

Filesystem tracing found no unexplained algorithmic extra-work pattern. On a
clean build OCaml made more metadata calls (14,836 versus 12,018), but fewer
opens (13,055 versus 13,492), creates (112 versus 492), and the same removals.
On unchanged and one-edit builds OCaml made fewer metadata and open calls than
Rust overall. Inside a long-lived watcher, one edit used 32 versus 25 metadata
calls and 39 versus 38 opens. The small retained delta is concentrated in
artifact/output safety checks, not package or source rediscovery.

The reproducible methodology and tooling are documented in
[`bench/README.md`](bench/README.md). Absolute times are host-specific; work
counts and artifact comparisons prevent a faster result from hiding skipped or
superfluous work.

The current source-size snapshot using cloc 2.04 is:

| Scope | Code lines |
|---|---:|
| Rust production, excluding telemetry | 7,818 |
| Rust inline unit tests, excluding telemetry | 2,773 |
| OCaml production, including both platform backends | 10,500 |
| OCaml tests and fixtures | 8,764 |
| OCaml benchmark tooling | 1,032 |

Line count is diagnostic, not an acceptance target. The larger OCaml total
includes explicit compatibility decoding, two maintained platform backends,
and lifecycle boundaries whose behavior is exercised independently.

## Intentional deviations and unusual compatibility behavior

- OpenTelemetry/OTLP support is omitted by project decision.
- `--filter` supports a documented common Rust/Re subset rather than exact
  Rust-regex semantics.
- A JSON `path` field is type-checked and then ignored in favor of the file
  actually read, preserving the leaked Rust configuration shape.
- Duplicate typed configuration fields reject, while duplicate keys in raw
  JSON-map-backed fields use the last value. This is retained compatibility,
  not a recommended schema rule.
- `--no-timing` accepts an optional boolean, which gives a following positional
  argument surprising parsing behavior.
- `--version` routing and clustered `-h`/`-V` behavior retain the Rust CLI's
  established order dependence.
- `lib/bs/build.ninja` is an empty compatibility marker rather than a build
  plan. It is not rewritten for ordinary retained watch edits.
- Compiler output is published as successful jobs complete; there are no
  OCaml-only sidecar artifacts.

## Rust defects or inefficiencies corrected in OCaml

These should remain visible for potential Rust follow-up work:

- Formatting schedules independent files concurrently instead of serializing
  all files below Rust's chunk threshold.
- Malformed configuration/resource paths produce contextual errors rather than
  panics or bounded hangs. The detailed cases remain in the history and parity
  inventories.
- Namespace entries without a namespace reject, and whitespace-only compiler
  flag fragments do not create empty argv elements.
- Dependency permissions are checked only for traversed edges and report every
  active violation.
- Watch filters use the same positive-match semantics in discovery and event
  handling.
- Incremental edge replacement removes obsolete reverse dependencies.
- Inconclusive Windows lock-owner probing preserves the lock.
- Unchanged compiler fingerprints are not rewritten.
- Stale runtime overrides fail before starting compiler work.
- Renamed/deleted compiler artifacts are removed without losing the useful
  current-command diagnostic.
- Missing generated JavaScript is repaired (issue #7728), and output
  format/path/suffix changes invalidate old output (the behavior from #8540).
- Visible packages exporting the same namespace artifact fail early.
- Removing a configured source directory cleans its old public output.
- Effective inherited JSX/experimental compiler options participate in
  dependency fingerprints.

The last two limitations are also present in the reference Rust implementation;
the OCaml fixes should be ported independently rather than removed for parity.

## Future performance inventory

These ideas are not required for the current parity gate and should be pursued
only with profiling and the same work/artifact checks:

- Replace each child's two pipe-reader threads and waiter thread with a shared
  event-driven pipe backend. This may reduce task count and stack/buffer cost,
  but a prior fixed-reader experiment did not improve wall time. It must retain
  concurrent stdout/stderr draining, deterministic output, cancellation, and
  Windows Job Object ownership.
- Reuse process-worker infrastructure across retained watch builds. This may
  improve very small edits but adds session shutdown and failure ownership.
- Persist a validated discovery/build inventory across separate CLI processes.
- Cache stable candidate/package metadata for retained edits if profiling shows
  whole-project reconstruction remains significant.
- Reuse per-worker publication buffers or CMI read results if allocation/GC
  profiling identifies them as material.
- Revisit the fixed child ceiling only on machines where measurements show it
  constrains utilization.
- Eventually integrate compiler work in-process or keep compiler workers alive.
  This is higher risk because compiler global state and crash isolation change.
- Parallelize configuration or inventory work with domains only after a profile
  identifies a CPU-bound phase; multicore itself is not expected to improve
  external compiler time.

## Deferred Windows handoff

The Windows code remains maintained and cross-platform type-checked, but native
runtime validation is outside this PR. The follow-up must verify:

- `CreateProcessW` quoting, explicit handle-list inheritance, suspended launch,
  Job Object assignment, descendant cancellation, and failed-termination
  behavior;
- pipe EOF and capture behavior when descendants inherit standard streams;
- drive, UNC, verbatim, 8.3, case-insensitive, and symlinked configuration paths;
- watcher registration/replacement, lock probing, atomic save, and polling
  fallback on NTFS;
- build, clean, format, compiler-args, interactive output, and the canonical
  rewatch suite under the repository's Cygwin/bash tooling;
- static/package artifact construction before Windows CI and npm packages switch
  their default `rescript.exe` to OCaml.

## Remaining order

1. Wait for the external reviewer, then address final whole-port review rounds
   until no material finding remains.
2. Run the final release-quality gate, including one uninterrupted
   `make test-all`, packaging/artifact checks, formatting, Reanalyze, and a final
   clean-worktree/process check.
3. Perform the broad source-comment pass last. Comments must start with why,
   provide enough context for readers who are not OCaml/build-system/platform
   specialists, and stand on their own unless compatibility itself is the
   reason. Follow it with a narrow formatting/build check.

Native Windows execution remains a later PR and is not a blocker for these
remaining non-Windows release steps.
