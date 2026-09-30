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

3. [ ] **Independent policies.** Separate capture/handoff from frozen lookup,
   and editor artifacts from GenType inputs. Preserve initial defaults;
   invalidate caches on policy changes.

4. [ ] **Owned semantic inputs.** Provide file-independent implementation/interface
   inputs to GenType with safe ownership/lifetimes. Preserve interface precedence,
   TypeScript, and errors. Dependency reads move in checkpoint 5.

5. [ ] **GenType dependency inputs.** Replace recursive annotation reads with
   versioned summaries; validate sources, compiler, configuration, and dependencies.
   Keep legacy readers for prebuilt packages. Require clean/edit/restart TypeScript
   parity using summaries.

6. [ ] **Editor-only annotations.** Remove GenType's forced annotations and
   compatibility copies. Gate editor collection, metadata, capture, snapshots,
   serialization, and export before allocation when disabled. Enabled builds
   retain editor features and complete/partial annotations.

7. [ ] **Owned AST results.** Return ASTs/dependencies before disk persistence;
   validate session generations and disk identities. Preserve persistent caches,
   restart behavior, and cancellation recovery.

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
