# Stable-Safety Guide

## What is stable-safety?

A module is **stable-safe** when it contains zero calls to `Stable.unsafe_of_value`.
This means all values stored in stable containers (`StableMap`, `StableSet`,
`StableWave`) are known to be stable by construction — their `Stable.t` types
flow from the module's inputs, not from unchecked casts.

`Stable.unsafe_of_value` is the only truly unsafe operation in the system: if a
minor-heap value is stored in a stable container, the GC may relocate it and the
container will hold a dangling pointer. Eliminating it from a module proves that
module cannot cause such corruption.

## Reading values back: linear vs non-linear

`Stable.to_linear_value` reads a value from stable storage. The caller must
consume it immediately — don't stash it in a long-lived OCaml structure. This
is safe because the value is used and discarded before the stable container
could overwrite or destroy the slot.

`Stable.unsafe_to_nonlinear_value` is for cases where the value *will* be
stored in a long-lived structure (hashtable, accumulator list, returned to
caller). This is safe only when the stable container will not destroy or
overwrite the slot while the OCaml reference is alive. Each call site must
be audited individually. Use `grep unsafe_to_nonlinear_value` to find them.

**When to use which:**
- Comparison, field access, `PosSet.iter`, passing to a pure function → `to_linear_value`
- Storing in a `Hashtbl`, consing onto a ref list, returning `Some v` → `unsafe_to_nonlinear_value`

## How to audit a module

1. **Count `unsafe_of_value` calls.** Zero means the module is stable-safe.

2. **For each call, ask: why is this needed?** Common reasons:
   - A callback/function field operates on raw `'v` instead of `'v Stable.t`
   - A value was prematurely unwrapped with `to_linear_value` and needs
     rewrapping
   - An intermediate computation produces a raw value that must be stored

3. **Classify each call as eliminable or boundary.**

4. **Audit `unsafe_to_nonlinear_value` calls.** For each, verify the stable
   container won't destroy/overwrite the slot during the value's lifetime.

## How to fix violations

### Pattern 1: Thread `Stable.t` through instead of unwrap/rewrap

**Before (violation):**
```ocaml
let v = Stable.to_linear_value (StableMap.find map k) in
(* ... use v ... *)
StableMap.replace other_map k (Stable.unsafe_of_value v)
```

**After (safe):**
```ocaml
let v = StableMap.find map k in   (* v : 'v Stable.t *)
(* ... pass v as Stable.t ... *)
StableMap.replace other_map k v   (* no conversion needed *)
```

The key insight: if a value came from a stable container, it already has type
`'v Stable.t`. Keep it in that type as long as you're just moving it between
stable containers. Only unwrap with `to_linear_value` when you genuinely need
to inspect or compute with the raw value.

### Pattern 2: Use `Maybe.of_stable` / `Maybe.to_stable` to reorder wrappers

Stable container iterators provide `'v Stable.t`, but sometimes you need
`'v Stable.t Maybe.t` (e.g., after `StableMap.find_maybe`). The `Maybe` module
provides zero-allocation conversions:

```ocaml
(* StableWave stores ('k, 'v Maybe.t) — so push needs 'v Maybe.t Stable.t *)
(* StableMap.find_maybe returns 'v Stable.t Maybe.t *)

(* Reorder: 'v Stable.t Maybe.t → 'v Maybe.t Stable.t *)
let mv_stable = Maybe.to_stable (Maybe.some v)  (* v : 'v Stable.t *)

(* Reorder: 'v Maybe.t Stable.t → 'v Stable.t Maybe.t *)
let mv = Maybe.of_stable mv_stable
```

### Pattern 3: Change callback signatures to accept `Stable.t`

**Before (violation in the module):**
```ocaml
type ('k, 'v) t = {
  merge: 'v -> 'v -> 'v;  (* raw values *)
  ...
}

(* Every merge call requires unwrap + rewrap *)
let merged = t.merge (Stable.to_linear_value a) (Stable.to_linear_value b) in
StableMap.replace t.target k (Stable.unsafe_of_value merged)
```

**After (safe):**
```ocaml
type ('k, 'v) t = {
  merge: 'v Stable.t -> 'v Stable.t -> 'v Stable.t;  (* stable values *)
  ...
}

(* No conversion needed *)
let merged = t.merge a b in
StableMap.replace t.target k merged
```

This pushes the `unsafe_of_value` to the boundary where the callback is created.

### Pattern 4: Eliminate pointless round-trips

When `Reactive.iter` or `StableMap.iter_with` provides `'k Stable.t` and
`'v Stable.t`, and the callback just passes them to another stable API, don't
unwrap and rewrap:

**Before (pointless round-trip):**
```ocaml
StableMap.iter_with
  (fun wave k v ->
    StableWave.push wave k
      (Stable.unsafe_of_value (Stable.to_linear_value v)))
  output_wave pending
```

**After (direct pass-through):**
```ocaml
StableMap.iter_with
  (fun wave k v -> StableWave.push wave k v)
  output_wave pending
```

## Pushing the boundary outward

When an inner module becomes stable-safe, the `unsafe_of_value` calls don't
disappear — they move to the next layer out. This is the right trade-off:

1. **Inner modules** (e.g., `ReactiveUnion`, `Reactive.ml`) become provably
   safe. They cannot corrupt stable storage regardless of what the caller does,
   as long as the `Stable.t` types in their API are respected.

2. **Boundary callers** (e.g., `ReactiveTypeDeps.ml`, `ReactiveLiveness.ml`)
   wrap user-provided functions to bridge raw ↔ stable:

   ```ocaml
   Reactive.Union.create ~name:"u1" left right
     ~merge:(fun a b ->
       Stable.unsafe_of_value
         (PosSet.union (Stable.to_linear_value a) (Stable.to_linear_value b)))
     ()
   ```

3. **The ideal boundary** is where values are first introduced into the stable
   world — typically at `Source.emit` or initial population. At that point
   `unsafe_of_value` (or the checked `of_value`) is unavoidable and correct.

## Current status

**Stable-safe (zero `unsafe_of_value`):**
- `ReactiveUnion.ml` — merge signature takes `'v Stable.t`
- `Reactive.ml` — Source, Union, FlatMap, Join, Fixpoint wrappers

**Boundary (callers responsible for `unsafe_of_value`):**
- `ReactiveTypeDeps.ml`, `ReactiveLiveness.ml`, `ReactiveDeclRefs.ml`,
  `ReactiveExceptionRefs.ml`, `ReactiveMerge.ml` — Union merge functions
- `ReactiveSolver.ml` — `Reactive.get` calls with `unsafe_of_value` on keys

**Non-linear reads (`unsafe_to_nonlinear_value`) — audit surface:**
- `ReactiveMerge.ml` freeze functions — copy stable → OCaml-heap hashtables
- `ReactiveSolver.ml` collect_issues — accumulate issues into ref lists
- `DeclarationStore.ml` find_opt/fold/iter — return or pass values to callers
- `Reanalyze.ml` find_decl — return value to caller

## Reference: ReactiveUnion → Reactive.ml as a worked example

`ReactiveUnion` was made fully stable-safe by:

1. Changing `merge: 'v -> 'v -> 'v` → `merge: 'v Stable.t -> 'v Stable.t -> 'v Stable.t`
   (eliminated 3 `unsafe_of_value` calls at merge sites)

2. Keeping `'k Stable.t` from `iter_with` callbacks instead of unwrapping
   (eliminated ~20 `unsafe_of_value` calls for keys passed between containers)

3. Using `Maybe.of_stable` / `Maybe.to_stable` to convert between
   `'v Stable.t Maybe.t` and `'v Maybe.t Stable.t` without allocation
   (eliminated unwrap/rewrap pairs around maybe-values)

Then `Reactive.ml` was made stable-safe by:

4. Pushing `Union.create`'s `?merge` signature to accept `'v Stable.t`
   (moved wrapping to callers in `reanalyze/`)

5. Removing 3 pointless round-trips in `Source` (iter, get, pending→wave)
   where values were unwrapped and immediately rewrapped

6. Rewriting `Source.apply_emit` with `Maybe.of_stable`/`Maybe.to_stable`
   instead of unwrap/rewrap through `to_linear_value`/`unsafe_of_value`

## Checklist for making a module stable-safe

- [ ] `grep unsafe_of_value` — list all occurrences
- [ ] For each: is the value already `Stable.t` upstream? If so, thread it through
- [ ] For callback fields: change signature to accept/return `Stable.t`
- [ ] For `Maybe` wrapper reordering: use `Maybe.of_stable` / `Maybe.to_stable`
- [ ] For pointless round-trips: remove the unwrap/rewrap pair entirely
- [ ] After changes: verify zero `unsafe_of_value` remains
- [ ] Build and run tests (especially allocation tests — zero words/iter)
- [ ] Verify the `unsafe_of_value` moved to the appropriate boundary layer
- [ ] Audit all `to_linear_value` in changed code — use `unsafe_to_nonlinear_value` for non-linear uses
