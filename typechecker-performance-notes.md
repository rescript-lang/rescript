# Typechecker Performance Notes

These are rough notes on possible ways to make ReScript typechecking faster.
The short version: expect the biggest wins from profiling, caching, and reducing
frontend churn rather than from a new typechecking theory drop-in.

## Most Promising Directions

| Direction | Likely payoff | Why it matters |
|---|---:|---|
| Profile real projects | High | We need to know whether time is in parsing, JSX rewrite, `Ctype`, env lookup, CMI/CMJ loading, error formatting, or build orchestration. |
| Merlin-style incrementality | High for editor/watch | Merlin keeps pipeline state, caches parsed/typechecked items, and backtracks to valid environments after edits. |
| Better CMI/CMJ/env caching | Medium-high | Watch mode should avoid repeated loads, path normalization, and environment reconstruction across modules. |
| Reduce JSX PPX churn | Medium | JSX currently creates extra AST/lambda plumbing. A language-level JSX path could reduce work and complexity. |
| Query-based architecture | High long-term | Salsa/rust-analyzer-style queries are a strong model for IDE/watch mode, but adopting this is invasive. |
| Audit upstream OCaml typing changes | Unknown | Worth checking, but recent OCaml releases look more runtime/codegen/error-message heavy than typechecker-speed heavy. |

## Practical Plan

1. Add timing spans around the pipeline:
   - parse
   - JSX rewrite
   - typecheck
   - lambda
   - JS emit
   - CMI/CMJ loading
   - dependency analysis

2. Benchmark on representative inputs:
   - real production apps
   - large generated codebases
   - JSX-heavy projects
   - namespace-heavy projects
   - pathological nested-module cases

3. Compare against upstream OCaml and Merlin:
   - scan `typing/` for performance-related changes we missed
   - inspect Merlin’s pipeline caches and environment backtracking
   - identify anything portable to the batch compiler or rewatch

4. Optimize only the top hotspots.

## Likely Wins

### Watch Mode

The most likely major win is avoiding repeated work between builds. ReScript’s
fast-refresh story depends on keeping small edits cheap, so incremental reuse is
more important than optimizing cold full builds.

Good candidates:

- Cache parsed ASTs where file contents are unchanged.
- Cache PPX/JSX rewrite results.
- Cache loaded CMI/CMJ metadata across rebuilds.
- Reuse environments for unchanged prefixes of a module.
- Avoid rebuilding dependency information for unaffected files.

### JSX

JSX is currently still PPX-shaped. That tends to create extra intermediate
structure and forces later compiler phases to recover intent from generated AST
and attributes.

A language-level JSX implementation could:

- avoid some PPX rewriting work,
- preserve component-path intent structurally,
- reduce lambda/JS rewrite heuristics,
- make diagnostics cleaner,
- reduce snapshot churn.

This is probably more of a medium-term cleanup than a tactical speed patch.

### Environment And Metadata Loading

For large projects, repeated metadata work can dominate. Things to inspect:

- `Env` construction and lookup patterns
- path normalization
- CMI loading
- CMJ loading
- namespace resolution
- repeated package/runtime metadata reads

## Research And Existing Work

### Merlin

Merlin is probably the most relevant practical reference. It is designed around
interactive latency, incomplete programs, pipeline caching, and environment
backtracking.

Things worth studying:

- typed pipeline cache
- reader/parser cache
- PPX cache
- environment cache
- recovery after partial errors

### Salsa / Query Systems

Salsa and rust-analyzer model compiler work as memoized queries with tracked
dependencies. This is attractive for watch mode and language tooling.

Potential benefits:

- recompute only affected queries,
- make dependencies explicit,
- support IDE and build-system reuse,
- make performance easier to reason about.

Cost:

- very invasive architecture change,
- not a small compiler patch.

### Incremental Typechecking Papers

There is research on mechanically incrementalizing typing algorithms. It is
conceptually relevant, but likely less immediately useful than Merlin/Salsa-style
engineering.

Useful takeaway:

- Retype only changed subtrees where possible.
- Make dependencies explicit.
- Cache derivations/results with stable names.
- Be careful about invalidation boundaries.

## Current Bet

The biggest realistic ReScript win is probably:

> Less repeated work in watch mode, plus less generated PPX/lambda junk for JSX.

Not:

> A radically cleverer unification algorithm.

The language already has a good performance profile because the type system is
strong but not too ambitious. We should protect that and improve the incremental
pipeline around it.
