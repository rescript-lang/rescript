# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Maintain coherence as you change the system

A task is not complete merely because the edited code works. Preserve the
relationships between implementation, interfaces, tests, and the explanations
contributors rely on, including relationships the build cannot check. Apply
this alongside the development workflow below, with effort proportional to the
change and its risks; a small local fix does not require a repository-wide audit.

1. **Identify what must stay true.** Before editing, read the relevant area
   guide, interface contracts, and nearby rationale. Work out the few
   relationships this task could affect, what intentionally changes, and what
   must remain true. Make them concrete: "build and watch apply this option with
   the same precedence," not "check the build system." These are task-specific
   working questions, not a new document or a fixed checklist for every task.
2. **Follow the relationships beyond the diff.** Trace relevant producers,
   consumers, alternative paths, tests, and explanations, including unchanged
   files. Search for the concepts and assumptions involved, not just matching
   names or existing links. Consider newly added, removed, or renamed cases:
   an overview can remain literally true while omitting an important new path.
   Check that different explanations agree in meaning and retain material
   qualifications, not that they use identical wording. Follow a relationship
   when the requested change gives a concrete reason it could be affected.
   Stop expanding the investigation when further checks lack such a reason;
   report remaining material uncertainty.
3. **Challenge, rather than confirm.** For each important relationship at risk,
   seek a concrete case that would expose a mismatch. Prefer an existing test,
   executable example, or checkable invariant when available. When the check is
   semantic, inspect the relevant definitions and passages on both sides and
   state the reason they agree or conflict. A plausible explanation, matching
   names, or passing unrelated tests is not evidence. Distinguish observed
   behavior from intended contracts, and implemented behavior from proposals.
4. **Repair the right thing and check the consequences.** Make the smallest
   justified set of changes that preserves the requested behavior and intended
   contracts. Do not obtain agreement by weakening a test, silently dropping a
   guarantee, or rewriting a design rationale to bless an accidental behavior.
   Regenerate expected outputs only after checking why the differences are
   correct. After editing, revisit the relationships from the callers' and
   readers' perspectives: what could now be false elsewhere, even with tests
   passing? Flag unresolved design conflicts or unrelated pre-existing drift
   for maintainers rather than silently expanding the task.
5. **Leave durable understanding in its owning place.** Record a discovery when
   it explains a consequential constraint that is not already clear in its
   owning location. Preserve the reason and relevant counterpart in an existing
   test, interface contract, local comment, or area guide, whichever owns it.
   Follow [CONTRIBUTING.md](CONTRIBUTING.md#repository-guides): caller contracts
   belong in module interfaces or source API documentation; algorithm and
   representation invariants belong beside the implementation; guides provide
   navigation and useful high-level explanations. Prefer links to duplicating
   detailed contracts. Do not add parallel relationship registries,
   synchronization versions, review stamps, or boilerplate comments. Leave an
   accurate explanation unchanged when the change does not affect it.
6. **Give maintainers evidence, not a self-assessment.** In the final response
   or PR description, include a brief coherence note: the important
   relationships examined, any mismatches repaired, the supporting paths and
   checks, and anything unresolved or not checked. It may be part of the normal
   change and testing description rather than a separate section. Distinguish
   executed checks and their results from inspection-based reasoning; reuse the
   normal testing summary rather than repeating it. A sentence can suffice for
   a small task. Do not invent a finding or claim repository-wide consistency.
   Keep this in the handoff, not in a tracked audit log.

## Quick Start: Essential Commands

```bash
# Build the platform toolchain (default target)
make

# Build the platform toolchain + stdlib
make lib

# Build the platform toolchain + stdlib and run tests
make test

# Format code
make format

# Check formatting
make checkformat
```

The Makefile’s targets build on each other in this order:

1. `yarn-install` runs automatically for targets that need JavaScript tooling (lib, playground, tests, formatting, etc.).
2. `build` (default target) builds the toolchain binaries (all copied into `packages/@rescript/<platform>/bin`):
   - `compiler` builds the dune executables (`bsc`, `rescript-*`, `ounit_tests`, etc.).
   - `rewatch` builds the Rust-based ReScript build system and CLI.
3. `lib` uses those toolchain outputs to build the runtime sources.
4. Test targets (`make test`, `make test-syntax`, etc.) reuse everything above.

## Area guidance

Before changing a subsystem, read its local agent instructions and area guide.
For compiler changes, read [compiler/AGENTS.md](compiler/AGENTS.md). For build
system changes, read [rewatch/AGENTS.md](rewatch/AGENTS.md). These apply even
when the task starts at the repository root. Changes spanning areas need the
relevant guidance from each.

- [`compiler/syntax/README.md`](compiler/syntax/README.md) for parsing,
  printing, and JSX transformation
- [`compiler/ml/README.md`](compiler/ml/README.md) for the type checker and
  typed tree
- [`compiler/core/README.md`](compiler/core/README.md) for Lambda
  optimization and JavaScript generation
- [`analysis/README.md`](analysis/README.md) for editor analysis
- [`rewatch/README.md`](rewatch/README.md) for the build system
- [`tools/README.md`](tools/README.md) for `rescript-tools`

## Coding guidelines

- **Use underscore patterns carefully** - Don't use `_` patterns as lazy placeholders for new language features that then get forgotten. Only use them when you're certain the value should be ignored for that specific case. Ensure all new language features are handled correctly and completely across all compiler layers
- **Avoid `let _ = …` for side effects** - If you need to call a function only for its side effects, use `ignore expr` (or bind the result and thread state explicitly). Do not write `let _ = expr in ()`, and do not discard stateful results—plumb them through instead.

- **Don't use unit `()` with mandatory labeled arguments** - When a function has a mandatory labeled argument (like `~config`), don't add a trailing `()` parameter. The labeled argument already prevents accidental partial application. Only use `()` when all parameters are optional and you need to force evaluation. Example: `let forceDelayedItems ~config = ...` not `let forceDelayedItems ~config () = ...`

- **Avoid warning suppressions** - Never use `[@@warning "..."]` to silence warnings. Instead, fix the underlying issue properly
- **Skip trailing `; _` in record patterns** - The warning it targets is disabled in this codebase, so prefer `{field = x}` over `{field = x; _}`.

## Coding Conventions

### Naming

- **OCaml code**: snake_case (e.g., `to_string`)
- **ReScript code**: camelCase (e.g., `toString`)

### Commit Standards

- Use DCO sign-off: `Signed-Off-By: Your Name <email>`
- Include appropriate tests with all changes
- Build must pass before committing

### Stacked pull requests

When a PR depends on another unmerged PR, make a native GitHub stack. Keep the
branches linear and in the same repository, then run `gh stack link BOTTOM
[NEXT...]`, listing the stack bottom to top. Each argument is a branch name or
a PR number: the command pushes each branch, reuses the PR that already exists
for it, and opens one where there is none, so branches alone are enough. Open
the PRs yourself first if you want to write their titles and descriptions.

Linking sets each PR's base to the branch below it, leaving only the bottom PR
on `master`. CI runs on every PR in the stack.

### Code Quality

- Follow existing patterns in the codebase
- Prefer existing utility functions over reinventing
- Comment complex algorithms and non-obvious logic
- Maintain backward compatibility where possible

## Testing and changelog

**Add a `CHANGELOG.md` entry** for any user-facing change (bug fix, feature, or breaking change). Put it under the matching section of the current `(Unreleased)` version and end the line with the PR link. See [CONTRIBUTING.md](CONTRIBUTING.md). PRs are expected to include one.

For compiler or standard-library changes, run `make test`. For syntax changes,
also run `make test-syntax`; use `make test-syntax-roundtrip` when parsing or
printing changes. Other focused suites are `make test-gentype`,
`make test-analysis`, `make test-tools`, and `make test-rewatch`.

### Testing Requirements

#### When to Add Tests

- **Always** for new language features
- **Always** for bug fixes
- **When modifying** analysis passes
- **When changing** JavaScript generation

#### Test Types to Include

1. **Syntax tests** (`tests/syntax_tests/`) - Parser validation
2. **Integration tests** (`tests/tests/`) - End-to-end behavior
3. **Unit tests** (`tests/ounit_tests/`) - Compiler functions
4. **Build tests** (`tests/build_tests/`) - Error cases and edge cases
5. **Type tests** (`tests/build_tests/super_errors/`) - Single-file type checking errors
6. **Multi-file error tests** (`tests/build_tests/super_errors_multi/`) - Cross-module errors that need separate `.res` / `.resi` files

#### Error variant catalog

[`tests/ERROR_VARIANTS.md`](tests/ERROR_VARIANTS.md) is a per-module
catalog of every error and warning variant the compiler can emit, with
each entry mapped to a fixture (or a documented reason it's unreachable).

**When adding or removing an error variant**, also update the catalog:

1. Add (or remove) the row in the relevant module section.
2. Set the status (`✓` covered / `⚠` unreachable / `☐` TODO).
3. If covered, link the fixture path; if unreachable, note the reason.

**When adding or removing a fixture**, update the corresponding row's
`Fixture` and status columns so the catalog stays in sync with the test
suite. The catalog is the primary tool for finding coverage gaps and
dead-code removal candidates; stale entries make both jobs harder.

## Additional development commands

Run commands from the repository root:

```bash
make watch        # Watch compiler sources
make artifacts    # Build artifacts and update the artifact list
make clean        # Clean build outputs
npm run check     # Lint with Biome
npm run check:all
npm run typecheck # TypeScript type checking
```

See [CONTRIBUTING.md](CONTRIBUTING.md#setup) for toolchain requirements and
setup, and the area guides above for subsystem-specific debugging commands.
