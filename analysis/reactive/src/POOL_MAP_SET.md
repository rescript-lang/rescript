# ReactivePoolMapSet: Production Notes and API Guidance

## Purpose

`ReactivePoolMapSet` implements a pooled `Map<K, Set<V>>` for churn-heavy paths
(`flatMap` provenance and `join` provenance/reverse index).

Goal: reduce allocation from nested-structure churn by recycling inner sets.

## Current Public API (single API)

- `create ~capacity`
- `add`
- `drain_key`
- `remove_from_set_and_recycle_if_empty`
- `find_maybe`
- `iter_with`
- `clear`
- `tighten`
- `cardinal`
- `debug_miss_count` (diagnostics/tests)

`ensure` is intentionally internal.

## Semantics that matter

- `add` on an absent key:
  - reuses a set from pool if available,
  - otherwise allocates a fresh set (`pool_set_miss_create`).
- `drain_key`:
  - iterates values for one key,
  - removes key from outer map,
  - clears and recycles the set.
- `remove_from_set_and_recycle_if_empty`:
  - removes one value,
  - recycles the set only if it becomes empty.
- Pool grows on demand (`pool_set_resize`) when recycled-set stack is full.

## What we measured on real workload

Experiment: full hyperindex replay (`benchmark/rescript-baseline..benchmark/rescript-followup`, 56 commits), reactive-only, request-attributed allocation log.

Observed from `alloc-events.log`:

- Startup phase (before first request):
  - 331 alloc events (`map_create`, `set_create` only).
- Request phase totals:
  - `pool_set_miss_create`: 31,963
  - `pool_set_resize`: 63
  - `pool_set_drain_key`: 542,071
  - `pool_set_remove_recycle_if_empty`: 544,768
- Misses are heavily front-loaded:
  - request 1: 31,825 misses
  - requests 2..56 combined: 138 misses (~0.43% of total misses)
- Resizes are non-zero after warmup:
  - 63 total (`join.right_key_to_left_keys`: 29, `join.provenance`: 18, `flatmap.provenance`: 16)

Takeaway: recycling dominates steady state; late allocations exist but are small for misses and non-zero for pool-stack growth.

## Best practices (from production replay)

1. Use only churn-safe teardown operations.

- For whole-key teardown: `drain_key`.
- For inverse-index unlink: `remove_from_set_and_recycle_if_empty`.
- Avoid API shapes that remove entries without recycling.

2. Warm up before judging allocation behavior.

- First request/phase discovers sizes and pays most miss costs.
- Evaluate steady-state from later requests, not request 1.

3. Track pool misses and pool resizes separately.

- `pool_set_miss_create` answers "fresh inner-set allocation".
- `pool_set_resize` answers "pool metadata growth pressure".
- Both are needed; misses alone are not the whole picture.

4. Keep attribution in the same log stream.

- Use request markers (`ALLOC_REQ_BEGIN/SUMMARY/END`) in alloc log.
- Include startup phase markers.
- This is required to connect events to concrete commits/requests.

5. Set initial capacity from expected concurrent recycled keys.

- Too small: more `pool_set_resize` and potential pressure.
- Too large: higher resident memory.
- Current implementation grows on demand; initial capacity still affects early behavior.

6. Use `tighten` deliberately, not continuously.

- `tighten` is allocating by design.
- Reserve it for explicit maintenance points after major churn phases.

## Test guidance (important)

For this structure, "no allocation" should be specified precisely:

- If requirement is "no fresh set allocation after warmup under churn",
  assert `debug_miss_count` delta is `0` in measured phase.
- Do not equate this with `words/iter = 0` in generic churn loops:
  other structures (outer map/set internals, diagnostics, etc.) may allocate.

Current churn tests in `AllocTest.ml` use this pattern:

- warmup first,
- measure churn loop,
- assert `pool_miss_delta = 0`.

## Reusable lessons for Map-of-Map work

Use this exact process for `Map<K, Map<...>>`:

1. Define churn-safe teardown APIs first (remove+recycle semantics explicit).
2. Add event-level instrumentation for misses/resizes with request attribution.
3. Run realistic replay, not only synthetic microbenchmarks.
4. Separate startup from steady-state in analysis.
5. Convert findings into budgeted assertions in tests (post-warmup deltas).

This avoids overfitting to synthetic "zero words/iter" and keeps API design aligned with production behavior.
