# ReScript AI

This document captures a first working direction for AI-oriented tooling in ReScript.

The lint command is the first concrete piece, but the intended surface is broader: commands and output modes in `rescript-tools` that are shaped for LLMs and agent workflows.

## Goal

Build AI-oriented tooling that:

- works well when an AI edits one file and wants immediate feedback
- also works on a whole project or subtree
- can inspect both raw source AST and typed information from `.cmt/.cmti`
- emits compact, reliable output that is easy for an LLM to consume

The first concrete feature under this umbrella is an AI-oriented lint command.
Aggressive source normalization for agents should be a separate command rather than an extension of lint.

## First Feature: Lint

The rest of this document focuses on lint as the first implementation target.

## Command Surface

Recommended first shape:

```sh
rescript-tools lint <file-or-root> [--config <file>] [--json]
rescript-tools rewrite <file-or-root> [--config <file>] [--json]
```

Notes:

- `lint <file>` is the primary AI workflow
- `lint <root>` should walk project sources
- source AST analysis and typed analysis should run together as one lint pass
- text should be the default output
- `--json` should opt into compact JSON
- git-aware filtering is a later extension
- `rewrite` is separate from `lint`
- `rewrite` is allowed to be more aggressive because it is explicitly agent-oriented
- lint reports style/semantic problems; rewrite canonicalizes source into a narrower normal form

## Recommended Placement

- CLI entrypoint: `tools/bin/main.ml`
- command implementation: new module in `tools/src/`
- semantic loading and package resolution: reuse `analysis/src/Cmt.ml`
- typedtree-derived structure/reference data: reuse `analysis/src/ProcessCmt.ml`
- compact JSON helpers: likely reuse or extend `analysis/src/Protocol.ml`

## Lint Analysis Model

Use two analysis lanes with one shared finding type, but expose them as one command rather than separate modes.

### 1. AST lane

Input: raw source file.

Use this for:

- fast per-file checks
- pre-build use
- syntax-shaped rules
- parse and structural checks that do not depend on semantic artifacts

Implementation should reuse the existing parser path already used in `tools/src/tools.ml`.

### 2. Typed lane

Input: `.cmt/.cmti`.

Use this for:

- resolved references
- handling `open`s and aliases correctly
- distinguishing module/type/value usage precisely
- future semantic queries like search/hover-like lookups

This lane should be the source of truth for semantic rules, while still running alongside the AST lane in the same invocation.

### Shared finding type

Proposed fields:

- `rule`
- `path`
- `range`
- `severity`
- `message`
- `symbol`
- `snippet` in text output when useful

Example JSON finding:

```json
{"rule":"forbidden-reference","path":"src/A.res","range":[12,2,12,20],"severity":"error","symbol":"Belt.Array.forEach","message":"Forbidden reference"}
```

## Lint Output Contract

Primary goal: compact and deterministic output for AI.

Recommended behavior:

- text by default
- `--json` for machine-readable compact JSON
- stable field names
- no extra prose in either mode
- exit `0` when clean, `1` when findings exist, `2` on usage/internal failure

Recommended default shape:

````text
severity: error
rule: forbidden-reference
path: src/A.res
range: 13:3-13:21
message: Forbidden reference
symbol: Belt.Array.forEach
snippet:
```text
  12 | let values = [1, 2, 3]
> 13 | Belt.Array.forEach(values, value => Console.log(value))
    |   ^^^^^^^^^^^^^^^^^^
  14 | let done = true
```
````

## Lint Config

Recommendation: keep lint config outside `rescript.json`.

Reason:

- lint rules will likely evolve faster than core project config
- keeps this feature decoupled from build config
- easier to make AI-specific without polluting `rescript.json`

Possible file names:

- `.rescript-lint.json`
- `rescript-lint.json`

Example shape:

```json
{
  "rules": {
    "forbidden-reference": {
      "severity": "error",
      "items": [
        "RescriptCore",
        "Belt",
        "Belt.Array.forEach"
      ]
    },
    "single-use-function": {
      "severity": "warning"
    }
  }
}
```

## Lint V1 Rules

### Forbidden references

Support banning references to:

- modules
- namespaces
- types
- values/functions
- deep member paths

Examples:

- `RescriptCore`
- `Belt`
- `Belt.Array.forEach`

Implementation notes:

- typed mode should resolve real referenced symbols and paths
- config items should be resolved once up front to canonical typed paths before matching
- matching should support both exact symbol bans and parent bans
  - if `Belt` is banned, `Belt.Array.forEach` should also fail

### Single-use functions

Goal: discourage helper functions that are defined once and used once.

V1 scope should stay intentionally small:

- same file only
- non-exported values only
- only regular function bindings
- skip cross-file reasoning

Why:

- cross-file usage becomes a project analysis problem
- local-only checks get most of the value with much lower complexity

Likely exclusions for V1:

- exported bindings
- recursive functions
- callbacks passed inline through transformations that make counting noisy
- generated/PPX-shaped artifacts if they create unstable counts

Longer term, this can grow into a reanalyze-style map/merge analysis.

## Separate Command: Agent Rewrite

Aggressive source normalization for agents should be a separate command rather than part of lint.

Recommended first shape:

```sh
rescript-tools rewrite <file-or-root> [--config <file>] [--json]
```

Goal:

- rewrite source into a narrower canonical form for agents
- reduce syntax variety even when multiple source spellings are semantically equivalent
- allow aggressive transformations as long as the rewritten program passes verification

This is intentionally not just lint autofix.
It is an agent-oriented normalization pass.

### Rewrite model

- rewrite to a fixed point until the file stops changing
- use a deterministic pass order so output is stable
- prefer AST-based rewriting and printing over text hacks
- rewrite whole enclosing items when that yields cleaner canonical output
- report which rules fired, which rewrites were skipped, and which were rejected by verification

### Verification

Because this mode is intentionally aggressive, it should verify more than a normal lint fix.

- reparse rewritten source
- re-typecheck rewritten source when semantic artifacts are available
- reject rewrites that fail verification
- optionally compare normalized typed output or generated JS for especially aggressive rules

### Candidate rewrite rules

#### Prefer `switch` over `if` / ternary

Goal: enforce a narrower set of control-flow patterns when agents write or rewrite code.

Initial rule idea:

- disallow `if`
- disallow ternary expressions
- or allow separate switches so a project can ban just one of them first
- rewrite these forms to `switch`

Why:

- this is exactly the kind of mechanical rewrite an AI tool can do reliably
- a smaller allowed shape makes generated code more uniform
- `switch` is often the more idiomatic branch form in ReScript anyway

Implementation notes:

- this should likely be an AST-based rewrite rule
- it should support distinct config toggles for `if` and ternary if we want a softer rollout
- this can be intentionally aggressive, including rewriting `else if` chains into a canonical `switch` shape

#### No `?Some(...)`

Goal: disallow pointless option wrapping in optional argument positions.

Examples to rewrite:

- `~value=?Some(x)`
- `~value=?Some(expensiveComputation())`

Why:

- `?Some(...)` is redundant in these positions
- it adds noise without changing the meaning we want

Implementation notes:

- this should be an AST-based rewrite rule
- the rewrite is straightforward: `?Some(expr)` -> `?expr`

#### Other good candidates

- normalize boolean branching shape
- swap branches instead of preserving unnecessary `!cond`
- rewrite nested ternaries into structured `switch`
- normalize optional-argument spellings into one canonical form
- recursively normalize branches until they also match canonical syntax
- skip or reject rewrites in generated/PPX-shaped regions when preservation is unreliable

## Lint Project Mode

For `lint <root>`:

- discover project files through existing package/project loading machinery
- run per-file analysis
- aggregate findings

For typed mode:

- only analyze files with available `.cmt/.cmti`
- report missing semantic artifacts cleanly when needed

## Lint Git Mode

`--git` should narrow reporting to relevant changed code.

### Phase 1

Filter findings to changed line ranges from the current diff.

This is the cheapest and safest first version.

### Phase 2

Expand changed lines to the enclosing code block, then filter findings to that block.

Candidate block definitions:

- top-level structure/signature item
- local `let` binding
- type declaration
- module declaration

This is closer to how an AI edits code: a small change often affects the whole enclosing item.

## Future

This section is intentionally a scratchpad. Add ideas here freely before they are fully designed.

### Compiler AI mode

- AI mode for the compiler with output tailored to LLMs
- compact, structured diagnostics with stable field names
- error output shaped for repair loops instead of humans scanning terminal prose
- likely a separate mode/flag rather than changing normal compiler output

### Deterministic codebase iteration

- "Find all of type"
  - example: find all local `external` definitions
- this should build on an iterator-style loop where an agent can traverse a codebase deterministically
- example flow:
  - ask for all `external`
  - get one item or one small page at a time
  - keep explicit cursor/bookkeeping until the full result set is exhausted
- likely needs:
  - stable ordering
  - cursors or checkpoints
  - optional notes/comments/bookkeeping attached by the agent
  - resumability across multiple requests

### CLI/agent-friendly semantic commands

- find references for CLI/agents
  - compact output
  - easy pagination
  - machine-readable ranges and symbol identity
- find definition / type definition
- hover-like symbol info as a CLI command
  - type
  - docs
  - module path
  - source location
- search symbols/modules by semantic identity rather than raw grep
- find implementations / exported values / local bindings by kind
- rename preview for agents
  - show affected files and ranges before applying changes
- code-action preview for agents
  - return suggested edits without editor integration
- range or block diagnostics
  - ask for diagnostics only for one binding, type, module, or changed block

### Other candidate directions

- ad-hoc docs generation using the existing JSON doc extraction work
- more semantic rules driven by real AI editing failures
- diff-aware semantic review commands
- project summaries for agents
  - exported modules
  - notable types
  - important entry points
- symbol-centric search
  - "show me all modules exporting a type named `t`"
  - "show all externals in this package"
  - "show all uses of Belt in changed files"

## Suggested Implementation Order

1. Add `lint` command and help text in `tools/bin/main.ml`
2. Create a new `Tools.Lint` module in `tools/src/`
3. Define config parsing and finding/output types
4. Implement AST lane plumbing
5. Implement typed lane plumbing on top of `analysis/src/Cmt.ml`
6. Implement `forbidden-reference`
7. Implement local `single-use-function`
8. Add `--git` line-range filtering
9. Add tests for file mode and project mode
10. Add `rewrite` command and a separate `Tools.Rewrite` module
11. Implement rewrite verification and fixed-point pass ordering
12. Add aggressive canonicalization rules like `if`/ternary -> `switch`
13. Add optional-arg normalization like `?Some(expr)` -> `?expr`

## Testing Plan

We should cover at least:

- source-only AST checks
- typed semantic checks using built project fixtures
- whole-project runs
- config-driven allow/deny cases
- `--git` filtering
- stable machine output snapshots

Likely homes:

- `tests/tools_tests/` for command behavior and output snapshots
- `tests/analysis_tests/` only if we need extra semantic fixtures

## Open Questions

1. Should default output be JSON, or should JSON be opt-in?
2. What should the config file be named?
3. Should typed mode fail hard when CMT is missing, or silently fall back in `auto` mode?
4. What exact forms count as a "single-use function" in V1?
5. In git mode, should we filter by raw changed lines only, or by enclosing AST block from day one?
