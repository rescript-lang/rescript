# Bug Report: Reactive Pipeline Order-Dependence

## Summary

The reactive dead code analysis pipeline produces incorrect results when `ReactiveLiveness` is created before files are processed. This manifests as 2 extra false-positive dead value warnings (382 vs 380 issues).

## Current Workaround

Using `Lazy.t` in `Reanalyze.ml` to defer `ReactiveLiveness.create` until after all files are processed:

```ocaml
let reactive_liveness =
  match reactive_merge with
  | Some merged -> Some (lazy (ReactiveLiveness.create ~merged))
  | None -> None
in
```

This avoids the issue but prevents true incremental updates when files change.

## Symptoms

When `ReactiveLiveness.create ~merged` is called **before** files are processed:

| Metric | Non-Reactive | Reactive |
|--------|-------------|----------|
| Dead Values | 233 | 235 (+2) |
| Total Issues | 380 | 382 (+2) |
| Roots | ~129 | 327 (+198 spurious) |
| Live Positions | 629 | 635 (+6 spurious) |

### Specific Incorrectly Dead Declarations

1. `DeadTest.res:63:6` - `let y = 55` inside module MM
2. `Unison.res:19:16` - `group` function

Both are correctly marked live in non-reactive mode.

## Root Cause Analysis

### The Reference Chain

For `DeadTest.res:63:6` (`y`):
- Line 64: `let x = y` (x references y)
- Line 68: `Js.log(MM.x)` (x is externally referenced)
- Expected: y is live because x→y edge exists and x is live

### What Goes Wrong

The edge `64:6 → [63:6]` exists in `state.edges`, and `64:6` IS in `current`, but `63:6` is NOT marked live.

**Delta ordering issue:**

1. When `value_refs_from` emits before `decls`, the join marks targets as "externally referenced"
2. When `decls` emits later, the join should emit `Remove` deltas to correct the spurious external refs
3. But something in the Remove propagation through `union → fixpoint` breaks

### Evidence from Debug Tracing

```
FIXPOINT stats: init_deltas=1850 edges_deltas=9147 total_added=1393 current=635 base=327 edges=711
```

- `total_added=1393` but `current=635` → 758 elements were added then removed
- This suggests Remove deltas ARE being processed, but not correctly

## Affected Code Paths

1. **`ReactiveLiveness.ml`** - `external_value_refs` and `external_type_refs` joins
2. **`Reactive.ml`** - `join` combinator's Remove delta handling
3. **`Reactive.ml`** - `union` combinator's Remove delta propagation
4. **`Reactive.ml`** - `Fixpoint.apply_init_delta` when Remove arrives

## Reproduction Steps

1. In `Reanalyze.ml`, change:
   ```ocaml
   | Some merged -> Some (lazy (ReactiveLiveness.create ~merged))
   ```
   to:
   ```ocaml
   | Some merged -> Some (ReactiveLiveness.create ~merged)
   ```

2. Update usage to not force lazy:
   ```ocaml
   | Some merged, Some liveness_result ->
     let live = liveness_result.ReactiveLiveness.live in
   ```

3. Run:
   ```bash
   cd tests/analysis_tests/tests-reanalyze/deadcode
   dune exec rescript-editor-analysis -- reanalyze -config -ci -reactive
   ```

4. Compare with non-reactive:
   ```bash
   dune exec rescript-editor-analysis -- reanalyze -config -ci
   ```

## Debug Infrastructure

A `trace_edges` flag exists in `Reactive.ml` inside the `Fixpoint` module:

```ocaml
module Fixpoint = struct
  let trace_edges = false  (* Set to true to debug *)
  ...
end
```

When enabled, it prints:
- `EDGE: Set source with N successors` when edge deltas arrive

## Potential Fixes

### Option 1: Fix Remove propagation in join/union

Debug why Remove deltas from the join don't correctly propagate through the union to the fixpoint. The join's `handle_right_delta` should trigger reprocessing that emits Removes.

### Option 2: Ensure edges before init

Modify the fixpoint to process all edges before processing any init deltas. This would require buffering or ordering guarantees.

### Option 3: Two-phase subscription

Subscribe to edges first, wait for them to stabilize, then subscribe to init. This is complex and may not be feasible in a streaming model.

### Option 4: Barrier/synchronization point

Introduce a barrier that ensures all file data has flowed before the fixpoint starts computing. This is essentially what `Lazy.t` does.

## Files Involved

- `analysis/reanalyze/src/ReactiveLiveness.ml` - Creates the reactive liveness pipeline
- `analysis/reanalyze/src/Reanalyze.ml` - Creates and uses ReactiveLiveness
- `analysis/reactive/src/Reactive.ml` - Core reactive combinators (join, union, fixpoint)

## Priority

**Medium** - The Lazy.t workaround is effective and has minimal performance impact (creation happens once per session). However, this blocks true incremental updates for file changes.

