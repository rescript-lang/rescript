# Embedded compiler performance plan

**Goal workflow:** Process unchecked checkpoints in order. Each leaves a working
build. Record changed files, gate results, measurements, and the next checkpoint
before checking one off. Retain fallbacks until parity passes.

**Target contract:** Fresh request-local inference, configuration, and diagnostics;
deterministic artifacts. `REWATCH_BIN_ANNOT=1` preserves `.cmt`/`.cmti`, source
paths, discovery metadata, and standalone editor analysis. Disabled builds skip
editor-only work, including when GenType is enabled.

1. [x] **Baseline.** Record fixed-setting, interleaved [benchmarks](bench/README.md)
   for clean, no-op, edit, shared-interface, GenType, PPX, restarted builds, and
   annotation modes: elapsed time, CPU, allocation, and RSS.

   Record: Added `bench/embedded_baseline.py`, its usage in `bench/README.md`,
   and [raw samples, settings, and medians](bench/baseline-2026-09-30.md).
   The compiler and OCaml Rewatch built; smoke runs and the full 165-sample
   run passed. With four domains, the 201-module clean-build median was 79.3
   ms and 9.4 MB allocated without annotations, versus 116.6 ms and 23.3 MB
   with annotations. Next: checkpoint 2, typed requests and per-request
   configuration.

2. [x] **Typed requests.** Share option decoding with standalone `bsc`; copy
   reusable configuration per request. Preserve option precedence, source
   overrides, logical argv, and fresh state.

   Record: Updated the shared compiler driver, option-state modules, driver
   tests, and OCaml Rewatch documentation. The session caches bounded option
   templates by working directory and argv; requests install private copies.
   Standalone and effectful requests retain direct dispatch. The five-run
   one-domain testrepo gate passed with identical compiler work, complete file
   sets, and artifact bytes. Clean medians were 1,658 ms before and 1,667 ms
   after; unchanged 42/43 ms; edit 43/42 ms. The [four-domain wide-fixture
   samples and gate report](bench/checkpoint-2-2026-09-30.md) record allocation,
   RSS, and annotation modes. The four-domain full-fixture CMI comparison is
   affected by existing parallel nondeterminism: two builds of checkpoint 1
   differed in 203 WebAPI CMIs, while one-domain repeats matched. `make test`,
   `make test-rewatch`, focused OUnit (104 tests), `make test-gentype`,
   `make test-analysis`, and `make checkformat` passed. Next:
   checkpoint 3, separate compiler and editor policies.

3. [x] **Independent policies.** Separate capture/handoff from frozen lookup,
   and editor artifacts from GenType inputs. Preserve initial defaults;
   invalidate caches on policy changes.

   Record: Split session capture/handoff from frozen dependency lookup in the
   compiler driver. Policy changes discard retained session results; classic
   lookup keeps disk publication ahead of dependents and the disk AST roundtrip.
   Build metadata now fingerprints handoff, lookup, editor-artifact, and
   GenType-input policies independently. The initial annotation behavior is
   preserved. The one-domain five-run gate passed with identical compiler work,
   file sets, and artifact bytes; clean medians were 1,686/1,703 ms. The
   [GenType measurements](bench/checkpoint-3-2026-09-30.md) include all three
   annotation settings, each with byte-identical generated artifacts across
   revisions. `make test-all` (compiler, GenType, analysis, tools, and Rewatch),
   focused OUnit (105 tests), and `make checkformat` passed. Next: checkpoint 4,
   owned semantic inputs for GenType.

4. [x] **Owned semantic inputs.** Provide file-independent implementation/interface
   inputs to GenType with safe ownership/lifetimes. Preserve interface precedence,
   TypeScript, and errors. Dependency reads move in checkpoint 5.

   Record: Updated GenType input selection, the embedded compiler session's
   published-semantic lookup, focused ownership/precedence tests, and OCaml
   Rewatch documentation. GenType consumes request-local implementation
   semantics and an isolated session copy of a validated interface result;
   missing or stale results use the original CMTI reader. A clean GenType trace
   reduced current-module CMT reads from one to zero. The [five-run gate and
   GenType measurements](bench/checkpoint-4-2026-09-30.md) passed with identical
   compiler work, complete file sets, artifact bytes, TypeScript, and all three
   annotation modes. `make test-all`, the OCaml Rewatch port integration suite,
   a final `make test`, focused OUnit (107 tests), and `make checkformat` passed.
   Next: checkpoint 5, versioned GenType dependency summaries.

5. [x] **GenType dependency inputs.** Replace recursive annotation reads with
   versioned summaries; validate sources, compiler, configuration, and dependencies.
   Keep legacy readers for prebuilt packages. Require clean/edit/restart TypeScript
   parity using summaries.

   Record: Added declaration-only CMT/CMTI sidecars, compiler/source/configuration/
   CMI validation, the recursive summary reader with legacy fallback, artifact
   publication and cleanup, a recursive-import fixture, validation and integration
   tests, and repeatable GenType parity and timing runners. The [five-run gate
   and GenType measurements](bench/checkpoint-5-2026-09-30.md) passed with
   identical compiler work, complete file sets, artifact bytes, and TypeScript
   in clean/edit/restart builds for all three annotation modes. The small
   GenType fixture showed added clean and restart latency from summary writing;
   this is a preparatory step for removing forced annotations. `make test-all`,
   the OCaml Rewatch port suite, focused OUnit (108 tests), and
   `make checkformat` passed. Next: checkpoint 6, editor-only annotations.

6. [x] **Editor-only annotations.** Remove GenType's forced annotations and
   compatibility copies. Gate editor collection, metadata, capture, snapshots,
   serialization, and export before allocation when disabled. Enabled builds
   retain editor features and complete/partial annotations.

   Record: Decoupled GenType inputs from the editor annotation and compatibility
   copy policies. The compiler retains a normalized request-local GenType
   semantic result without writing CMT/CMTI, and summary-only interfaces keep
   the semantic input needed after restart. Disabled builds skip editor type
   collection, metadata, session capture, CMT serialization, and export;
   enabled builds retain complete and partial annotations. The
   [five-run gate and GenType measurements](bench/checkpoint-6-2026-09-30.md)
   passed with identical compiler work, complete file sets and artifact bytes
   outside GenType, and matching TypeScript, JavaScript, errors, and enabled
   editor artifacts in GenType. Disabled GenType allocation fell 8% on the
   interface fixture and 19% on recursive imports, with roughly level clean
   wall time. `make test-all`, the OCaml Rewatch port suite, focused OUnit
   (109 tests), and `make checkformat` passed. Next: checkpoint 7, owned AST
   results.

7. [x] **Owned AST results.** Return ASTs/dependencies before disk persistence;
   validate session generations and disk identities. Preserve persistent caches,
   restart behavior, and cancellation recovery.

   Record: Updated `compiler/depends/binary_ast.ml`, the embedded compiler
   driver, and the OCaml Rewatch session, parse, and export modules. Frozen
   requests capture ASTs and dependencies before staging writes; the export
   worker persists byte-identical ASTs while compilation consumes the owned
   result. Source identity, request generation, and then staging file identity
   validate handoff. Classic lookup and an explicit synchronous fallback
   remain. Added driver and export unit tests, updated Rewatch and benchmark
   documentation, and recorded the [five-run gate](bench/checkpoint-7-2026-09-30.md).
   It passed with identical compiler work, complete file sets, and artifact
   bytes. Clean medians were 3,136/3,140 ms before/after with 196,304/198,708
   KiB sampled peak RSS; unchanged 229/227 ms and edit 245/262 ms. The clean
   gate showed no demonstrated speed gain. `make test-all` passed before the
   final buffer-retention cleanup; final `make test`, the OCaml Rewatch port
   suite, focused OUnit (111 tests), and `make checkformat` passed afterward.
   Next: checkpoint 8, owned CMI/CMJ and persistence.

8. [ ] **Owned CMI/CMJ and persistence.** Migrate CMI, CMJ, then optional editor
   outputs. Snapshot mutable graphs before async serialization; serialize once;
   bound retained heap memory. Preserve fingerprints, freshness, partial publication,
   hook ordering, failure recovery, atomic editor files, and eviction/exit disk fallback.

9. [ ] **Immutable imported types.** Measure copying/materialization; migrate
   values, constructors, records, modules, then substitution/inclusion as separate
   validated changes. Preserve sharing/identities; mutable nodes/views remain
   request-local. Replace measured fallbacks.

10. [ ] **Frozen GenType parity.** Fix TypeScript differences before enabling
    frozen lookup. Test mixed imports both ways; restrict classic lookup to
    affected requests and invalidate policy-dependent caches.

11. [ ] **Legacy PPX boundary.** Measure AST 0 conversion, serialization, I/O,
    and execution. Isolate the adapter; preserve frozen AST 0, bridge coverage,
    and no conversion without external PPXs.

12. [ ] **Core promotion.** Pass final gates; enable validated defaults, remove
    superseded paths, and update documentation, parity coverage, and changelog.

**Separate PPX goals:** After checkpoint 11, persistent AST 0 workers require a
cooperating executable and request reset/recovery tests. A modern AST protocol
follows worker validation and requires a cooperating consumer. Retain the legacy
adapter; these integrations do not hold up core promotion.

**Gates:** Each checkpoint builds and passes affected suites; compiler changes
require `make test`. Use `make test-rewatch`, focused OCaml tests,
`make test-gentype`, `make test-analysis`, and syntax suites where affected.
Check file sets per mode, retained artifact bytes, diagnostics, source maps;
GenType with annotations unset/`0`/`1`; editor completion, hover, references, and
incomplete-source recovery; cancellation and concurrent requests.
Final promotion requires Linux/macOS/Windows and multiple domain counts.
Optimizations need repeatable gains without material latency/memory regressions;
preparatory changes need parity.
