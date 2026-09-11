# Goal: Port rewatch from Rust to OCaml

Implement an OCaml version of the ReScript build system currently in `rewatch/`, reproducing its existing behavior. Continue invoking `bsc` as an external process, including parallel subprocess execution.

The OCaml implementation must run multiple `bsc` subprocesses concurrently, preserving Rust rewatch’s dependency-aware parallel scheduling.

Direct in-process compiler integration and compiler-state caching are outside this goal. OCaml 5 domains are permitted if useful for implementing the build tool, but are not required: compilation parallelism comes from running independent `bsc` processes.

Build the OCaml implementation alongside Rust rewatch, preferably in `rewatch-ocaml/`. Preserve the Rust implementation and unrelated worktree changes. Do not switch the production default as part of this task.

## Working approach

Follow `AGENTS.md` and read the relevant area guides. Start with:

- `rewatch/README.md`
- `rewatch/CompilerConfigurationSpec.md`
- `rewatch/MonorepoSupport.md`
- `rewatch/Features.md`
- `rewatch/src/`, particularly configuration, package discovery, build scheduling, and watching
- `rewatch/tests/` and `rewatch/testrepo/`

Inventory the commands, options, configuration fields, platform behavior, and tests. Treat the current Rust implementation as the behavioral reference. Record the reference commit so ongoing upstream changes do not silently change the target.

Implement idiomatic OCaml rather than translating Rust structure mechanically. Keep configuration, module graphs, build state, subprocess management, artifact handling, and watching in cohesive modules with explicit ownership. Local mutation is fine; avoid unnecessary process-wide globals.

Choose dependencies pragmatically. A small native C/Rust watcher component or helper process is acceptable if it provides dependable platform support. Compare existing OCaml watcher libraries, libuv bindings, and established implementations before building a backend. Respect licenses and document packaging requirements.

This is an implementation task. Progress autonomously through the milestones, including tests and review. Milestone gates are verification checkpoints, not requests for routine user approval.

## Required compatibility

Cover all current rewatch responsibilities, including:

- Commands, CLI options, configuration validation and precedence.
- Package dependencies, monorepos, namespaces, source discovery, generated and feature-gated directories.
- Compiler discovery, environment overrides, PPX and compiler argument construction.
- Parsing through `bsc`, dependency extraction, cycle detection, and dependency-ordered compilation.
- Bounded parallel subprocess scheduling.
- Incremental invalidation, interface changes, and stale artifact cleanup.
- Diagnostics, exit status, verbosity, and supported tracing behavior.
- Watch-event handling, configuration changes, error recovery, and shutdown.
- Supported-platform path, process, and filesystem behavior.

Do not silently ignore unsupported options or configuration. Document temporary gaps precisely.

Exact parser and Unicode semantics of Rust's `regex` crate are not required for
`--filter`. The OCaml port may expose a documented, visibly rejected subset
through a maintained native OCaml matcher. Known parser and non-ASCII matching
differences must remain documented and covered at the compatibility boundary.

## Milestones and gates

### 1. Working one-shot build

Add build integration and a separately named experimental executable. Implement a complete single-package build: configuration → discovery → parsing → dependency graph → compilation → artifacts and diagnostics.

Support multiple modules, `.res`/`.resi` pairs, dependency cycles, compilation failures, and a subsequent successful build.

**Gate:** Selected existing fixtures produce equivalent results under Rust and OCaml rewatch.

### 2. Configuration and workspace parity

Complete configuration handling, commands and options, namespaces, dependency packages, monorepos, generated directories, and feature-gated sources.

Maintain a concise compatibility matrix covering the inventoried behavior.

**Gate:** Relevant existing fixtures pass, and every configuration field and command is accounted for.

### 3. Parallel subprocess scheduling

Run independent `bsc` processes concurrently with bounded parallelism. Schedule work only when prerequisites are satisfied. Handle failed prerequisites, output collection, child-process cleanup, interruption, and shutdown without deadlocks or conflicting writes.

The compiler remains an external executable; this milestone does not require parallel OCaml compiler execution.

**Gate:** Sequential and parallel builds agree, failure paths are tested, and clean-build performance is compared with Rust rewatch.

### 4. Incremental builds

Port dirty-state propagation and artifact ownership. Handle source and interface edits, additions, deletions, renames, dependency changes, configuration changes, and recovery after failed builds.

Temporary over-invalidation is acceptable if documented; under-invalidation is not.

**Gate:** After every step in representative edit sequences, incremental results match a clean reference build.

### 5. Watch mode

Integrate a watcher backend. Handle recursive watching, newly created directories, editor atomic saves, duplicate or reordered events, batching, configuration changes, and changes arriving during a build.

Ensure coherent diagnostics, reliable error recovery, clean shutdown, and bounded resource usage.

**Gate:** Applicable existing watch tests pass reliably. Platform support and unverified platforms are stated explicitly.

### 6. Full compatibility and evaluation

Run the complete applicable rewatch suite. Account for every failure and close implementation gaps without weakening tests. Compare clean builds, unchanged builds, edit latency, watch responsiveness, peak memory, and packaging requirements.

**Gate:** Deliver a working port with a factual compatibility and performance report. Do not replace Rust rewatch or begin in-process compiler integration.

## Testing

Reuse existing fixtures and integration infrastructure. Parameterize the runner or add a thin alternative runner rather than duplicating the fixture tree.

Compare:

- Exit status and diagnostics.
- Compiler invocation arguments where relevant.
- Generated JavaScript and compiler artifacts.
- Created and removed files.
- Behavior after edit sequences and failed builds.

Compare deterministic output exactly. Where normalization or semantic comparison is necessary, explain why and ensure it does not hide differences.

Add focused unit tests for configuration, graph algorithms, invalidation, argument construction, and event normalization. Use end-to-end tests to establish observable behavior.

Do not use fixed sleeps for asynchronous tests. Wait for explicit observable conditions. Run focused tests during development and broader relevant checks at milestone gates. Serialize tests that mutate shared fixtures.

## Code quality

Produce code that a maintainer can understand and extend:

- Prefer idiomatic OCaml and established repository conventions.
- Keep interfaces narrow and state ownership clear.
- Avoid speculative abstractions, trivial wrapper layers, duplicated logic, and oversized catch-all modules.
- Do not add future compiler-integration machinery.
- Do not hard-code fixture-specific behavior.
- Do not swallow errors or substitute success-shaped defaults.
- Pass subprocess arguments directly rather than constructing interpolated shell commands.
- Clean up processes, file descriptors, temporary files, and watcher resources on success and failure.
- Remove dead code, abandoned experiments, stale comments, and placeholders before completing a milestone.
- Comments should explain invariants and non-obvious decisions rather than restate the code.
  Start with why the code or invariant is needed, give enough context for a
  reader who is not a specialist in every relevant OCaml, build-system,
  compiler, or operating-system detail, and make the explanation stand on its
  own. Refer to the Rust implementation only when that compatibility
  relationship is itself the reason for the decision.
- Do not suppress warnings or weaken tests to make the port pass.
- Measure before introducing performance-driven complexity.
- Treat performance parity as a work-equivalence gate, not only a wall-clock
  ratio. Inventory the Rust implementation's avoidance strategies (including
  filesystem traversal, metadata calls, parsing, graph construction, artifact
  checks, subprocess creation, and output capture), and implement applicable
  missing strategies before accepting the benchmark. Use syscall or equivalent
  tracing where available to detect superfluous work.
- Keep proposed optimizations that are not present in Rust in a separate,
  prioritized backlog. For each proposal, record whether it addresses a measured
  bottleneck or is still a hypothesis, its expected benefit, complexity and
  correctness risk, Windows implications, and the benchmark plus equivalence
  checks required before adoption. Do not mix speculative improvements into the
  compatibility port merely to improve headline timings.

## Review gates

After every milestone:

1. The implementation agent reviews the complete milestone diff, simplifies unnecessary code, checks parity against Rust, and runs formatting, compilation, and relevant tests.
2. A separate reviewer with fresh context reviews the code, corresponding Rust behavior, tests, and acceptance criteria.
3. The implementer addresses findings, explains any disagreement with evidence, and reruns relevant checks. Material fixes receive a focused follow-up review.

Review both correctness and maintainability. Require concrete findings with affected code and consequences; avoid speculative redesigns and style churn.

For subprocess scheduling, incremental invalidation, watch mode, and final evaluation, use two independent reviewers with complementary scopes: behavioral correctness, and design/resource/concurrency concerns.

Do not call a milestone complete while confirmed material findings remain unresolved.

### Final code-quality gate

After behavior, work equivalence, and performance gates pass, perform a distinct
whole-port maintainability pass before release:

- Split modules whose size or mixed responsibilities obstruct review; keep test
  code and benchmark tooling separate from production implementation.
- Review module, file, type, function, field, and test names for clear ownership
  and consistent terminology. Remove misleading Rust-derived names and unclear
  abbreviations, while keeping established ReScript concepts recognizable.
  Review opened modules at the same time: explicitly qualify calls when doing
  so makes ownership or side effects clearer, especially for generic utility
  names, but retain an open when qualification would only add repetitive noise.
- Simplify duplicated control flow and remove dead code, stale compatibility
  scaffolding, abandoned experiments, and avoidable allocations without
  regressing measured performance.
- Audit optional booleans and other encodings with unnamed states. Prefer a
  normal variant when each state has distinct meaning so invalid combinations
  are unrepresentable and compiler errors name the missing case.
- Add comments for ownership, concurrency, platform, cleanup, and algorithmic
  invariants that are not evident from the code. Do not add comments that merely
  paraphrase statements.
- Document the unit, focused, canonical, full-repository, work-equivalence,
  performance, filesystem-call, and source-size tooling so future changes can
  reproduce the gates.
- Require warning-free builds, formatting, and available linters without warning
  suppressions.
- Audit every process, pipe descriptor, watcher handle, lock, and staged or
  temporary output across success, failure, interruption, and partial-launch
  paths.
- Audit the platform boundary for hidden Unix assumptions and type-check both
  selected and unselected implementations; complete the native Windows run.
- Review dependency maintenance, licenses/notices, static packaging, and the
  final npm artifact manifest.
- Remove the temporary `save-pr-cache` setup-OCaml input and its
  `rewatch-ocaml` branch settings from CI and coverage once the new OPAM cache
  key is available on the default branch, and in all cases before merge.
- Review test isolation and reliability, replacing fragile sleeps with observable
  polling where possible and retaining tests for every intentional Rust
  divergence or corrected Rust bug.
- Recheck public diagnostics, exit classes, redirected/interactive output, and
  CLI discoverability.
- Record final production/test/tooling line counts and largest modules as review
  signals, not optimization targets.
- Publish separate final inventories of (a) compatibility behavior retained even
  though it appears odd or inconsistent, (b) documented Rust bugs or simple
  inefficiencies intentionally corrected by the OCaml port, and (c) possible
  post-parity performance improvements absent from Rust. Include rationale,
  coverage, and a future cleanup or validation path for every entry.

**Gate:** The whole-port review has no unresolved material correctness,
resource, portability, maintainability, documentation, or packaging finding,
and all behavior/performance gates still pass after cleanup.

## Models

Use **GPT-5.6 Sol at medium reasoning** for implementation and ordinary independent reviews. Use **GPT-5.6 Terra at medium reasoning** for bounded tasks with clear acceptance criteria.

After each milestone, perform an implementation self-review and one independent review. Fix confirmed findings and rerun relevant tests. Request a second review only when substantial fixes, unresolved concerns, or particularly complex scheduling or invalidation logic justify it. Perform a final whole-port review.

Escalate to Sol high reasoning for a specific difficult issue when medium repeatedly fails to resolve it. Use Astra only with explicit user approval. Do not automatically increase reasoning effort based on milestone number or task size.

Evaluate model suitability after the first working build milestone using behavioral correctness, code clarity, review findings, and rework required. Keep medium as the default if those results are satisfactory.

## Completion and reporting

Maintain one concise progress document containing the reference commit, completed milestones, compatibility gaps, tests, measurements, review outcomes, and next actions. Avoid generating a collection of redundant planning documents.

The goal is complete when the OCaml port reproduces current rewatch behavior, passes the applicable suite, still invokes `bsc` externally, and includes clear build/run/test instructions. Required behavior that remains unsupported means the goal is incomplete; unavailable platform verification must be disclosed.

At milestones, report what works, what was verified, remaining gaps, and material decisions. If execution is interrupted, leave a buildable, tested checkpoint and precise continuation instructions. Resume from that checkpoint rather than treating partial progress as completion.
