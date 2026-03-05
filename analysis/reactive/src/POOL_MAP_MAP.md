# ReactivePoolMapMap: Design Draft (from production lessons)

## Why this exists

`ReactivePoolMapSet` removed important churn footguns for `Map<K, Set<V>>`.
We still have `Map<K, Map<I, V>>` shapes in reactive internals with similar risks:

- per-key inner container allocation/discovery,
- empty inner maps lingering unless callers remember to remove them,
- fragmentation pressure split across many independently sized inners.

Goal: centralize lifecycle/recycling for inner maps, so callers cannot accidentally leak empty inners or bypass reuse.

## Inventory of current map-of-map usage

### 1) `ReactiveFlatMap.contributions`

File: `ReactiveFlatMap.ml`

Shape:

- outer key: `k2` (derived key)
- inner key: `k1` (source key)
- value: `v2` (contribution payload)

Ops pattern:

- add/update one inner entry (`k2`,`k1`) frequently,
- remove one inner entry on source churn,
- recompute aggregate by iterating inner map for one `k2`,
- if inner becomes empty, target is removed.

Current footgun: empty inner contribution maps are not always removed/recycled by construction.

### 2) `ReactiveJoin.contributions`

File: `ReactiveJoin.ml`

Same shape and lifecycle as flatMap:

- outer key: `k3`
- inner key: `k1`
- value: `v3`

### 3) `ReactiveFixpoint.pred_map`

File: `ReactiveFixpoint.ml`

Type is map-of-map (`k -> (pred -> unit)`), but semantically this is map-of-set.
This likely belongs on `ReactivePoolMapSet` (or equivalent set API helpers), not a generic map-of-map API.

## Design constraints carried from PoolMapSet

1. Public API must encode correct teardown semantics.

- Avoid exposing mutable inner map handles as a normal path.
- Prefer operation names that force recycling decisions.

2. Keep diagnostics first-class.

- Track miss and pool-resize independently.
- Request-attributed trace events for realistic replay analysis.

3. Validate with realistic replay, then encode budgets in tests.

- As with PoolMapSet, startup and steady-state must be analyzed separately.
- Tests should assert post-warmup miss deltas for typical churn patterns.

## Proposed minimal API (v0 draft)

```ocaml
type ('ko, 'ki, 'v) t

val create : capacity:int -> ('ko, 'ki, 'v) t

val replace : ('ko, 'ki, 'v) t -> 'ko -> 'ki -> 'v -> unit
(** Ensure inner map for outer key and set one entry. *)

val remove_from_inner_and_recycle_if_empty :
  ('ko, 'ki, 'v) t -> 'ko -> 'ki -> unit
(** Remove one inner entry; recycle and remove outer key if inner becomes empty. *)

val drain_outer :
  ('ko, 'ki, 'v) t -> 'ko -> 'a -> ('a -> 'ki -> 'v -> unit) -> unit
(** Iterate all entries for one outer key, then recycle that inner map. *)

val iter_inner_with :
  ('ko, 'ki, 'v) t -> 'ko -> 'a -> ('a -> 'ki -> 'v -> unit) -> unit
(** Read-only iteration for one outer key without exposing mutable inner map. *)

val inner_cardinal : ('ko, 'ki, 'v) t -> 'ko -> int
val outer_cardinal : ('ko, 'ki, 'v) t -> int

val find_inner_maybe :
  ('ko, 'ki, 'v) t -> 'ko -> ('ki, 'v) ReactiveHash.Map.t ReactiveMaybe.t
(** Optional: keep internal/private if we want stricter discipline. *)

val tighten : ('ko, 'ki, 'v) t -> unit

val debug_miss_count : ('ko, 'ki, 'v) t -> int
```

## Why this API shape

- `replace` + `remove_from_inner_and_recycle_if_empty` is the map-of-map analog of the safe PoolMapSet pair.
- `drain_outer` provides the whole-key teardown fast path.
- `iter_inner_with` avoids the main footgun: callers mutating inner maps directly and bypassing recycle.

## Instrumentation (draft)

Emitted event names in `ReactiveAllocTrace`:

- `pool_map_resize`
- `pool_map_miss_create`
- `pool_map_drain_outer`
- `pool_map_remove_recycle_if_empty`

The replay script should summarize miss/create vs pool-resize separately (same as PoolMapSet).

## Migration status

- Implemented: `ReactivePoolMapMap` module (API-aligned with this draft).
- Implemented: `ReactiveFlatMap.contributions` migrated to `ReactivePoolMapMap`.
- Implemented: `ReactiveJoin.contributions` migrated to `ReactivePoolMapMap`.
- Implemented decision: `ReactiveFixpoint.pred_map` migrated to
  `ReactivePoolMapSet` (semantic map-of-set), not `ReactivePoolMapMap`.

## Initial migration targets

1. `ReactiveFlatMap.contributions`

- Replace ad-hoc `get_contributions + Map.remove` with PoolMapMap operations.
- Ensure per-source removal path always uses `remove_from_inner_and_recycle_if_empty`.

2. `ReactiveJoin.contributions`

- Same migration pattern as flatMap.

3. `ReactiveFixpoint.pred_map`

- Done: migrated to `ReactivePoolMapSet` (map-of-set semantics).

## Test plan (mirrors PoolMapSet)

- Add allocation tests that mimic actual flatMap/join churn (not synthetic random patterns).
- Assert in measured phase:
  - pool-map miss delta is zero for stable-key churn,
  - functional result matches baseline,
  - optional: bounded pool-map resize events after warmup.
