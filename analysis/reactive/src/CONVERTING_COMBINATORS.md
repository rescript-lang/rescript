# Converting Reactive Combinators to Zero-Allocation Modules

This document describes how to extract a reactive combinator from
`Reactive.ml` into its own private module backed by `ReactiveHash`
(Hachis open-addressing tables), following the pattern established
by `ReactiveUnion` and `ReactiveFlatMap`.

## Best Practices for Zero-Allocation Code

Lessons learned from converting combinators to zero-allocation:

### What allocates in OCaml

Verified with assembly inspection (`ocamlfind ocamlopt -S`) and
`Gc.stat()` measurement (see `ClosureAllocTest.ml`).

**Closures that capture variables always allocate.** The compiler
lifts the function *code* to a static top-level function, but still
heap-allocates a closure *record* (environment block) every time the
closure is created, to pair the code pointer with captured variables.
A closure capturing N variables costs N+3 words (header + code ptr +
arity info + N env slots).

```
one closure (captures 1 param):       4 words
two closures (capture 1 param each):  8 words
closure + ref (captures ref + param): 7 words  (ref=2 + closure=5)
no closure (passes value directly):   0 words
```

**Non-capturing closures are free.** The compiler lifts them to
static constants — `Map.iter (fun _k _v -> ()) m` allocates 0 words.

**Capturing a function parameter still costs 4 words.**
`let f k = Array.length t + k in use f` allocates even though `t` is
just a function parameter — the runtime needs a closure record to
bundle the code pointer with `t`.

**Refs that escape into closures allocate** (2 words: header + value).
`let count = ref 0 in let f k = incr count; ... in` — the `ref`
is heap-allocated because it escapes into the closure.

**Refs that do NOT escape are free.** When a `ref` is only used in
straight-line code (not captured by any closure), the compiler
unboxes it into a local mutable — zero allocation.

**Higher-order function arguments create closures at call sites.**
Each function argument that captures variables costs a closure record
at the call site.

### Pass data directly instead of wrapping it in closures

When a function takes a higher-order argument just to abstract over
how data is accessed, but there is only one implementation, pass the
data directly and call the concrete function at the point of use.

```ocaml
(* Before: 13 words — 3 closure records allocated at call site *)
let apply_list t ~init_iter ~edge_iter ~emit_entry = ...
apply_list t
  ~init_iter:(fun f -> ReactiveWave.iter roots f)
  ~edge_iter:(fun f -> ReactiveWave.iter edges f)
  ~emit_entry:(fun k v -> ReactiveWave.push t.output_wave k v)

(* After: 0 words — data passed directly, concrete calls at use site *)
let apply_list t ~roots ~edges = ...
apply_list t ~roots ~edges
```

### Lift local helpers to module-level functions

A local `let f k = ... t ... in` captures `t` and allocates a
closure. Moving it to `let f t k = ...` at module level eliminates
the closure. When passing it to debug-only callbacks, guard with
`if Invariants.enabled` so the partial application `(f t)` is
never allocated on the hot path.

### Use `iter_with` and `list_iter_with` on hot paths

When a module-level function `f t k` is passed to an iterator,
both `iter (f t)` and `iter (fun k -> f t k)` allocate a closure
capturing `t`. Use the `_with` variants to pass `t` as data:

```ocaml
(* Allocates a closure capturing t: *)
ReactiveHash.Map.iter (fun k () -> enqueue t k) m
List.iter (fun k -> mark_deleted t k) succs

(* Zero allocation — t passed as data: *)
ReactiveHash.Map.iter_with enqueue_kv t m
list_iter_with mark_deleted t succs
```

`iter_with f arg t` calls `f arg k v` directly, where `f` is a
static top-level function (no closure record needed). Available on
`ReactiveHash.Map`, `ReactiveHash.Set`, `ReactiveWave`, and as
`list_iter_with` for `'a list`.

### Use `Maybe` instead of `option` for lookups

`ReactiveHash.Map.find_maybe` returns a `Maybe.t` — an
unboxed optional that avoids allocating `Some`. Use this instead of
`find_opt` in hot paths:

```ocaml
(* Zero allocation: *)
let r = ReactiveHash.Map.find_maybe t.pred_map k in
if Maybe.is_some r then
  use (Maybe.unsafe_get r)

(* Allocates Some on hit: *)
match ReactiveHash.Map.find_opt t.pred_map k with
| Some v -> use v
| None -> ...
```

### Use `Map.has_common_key` for set intersection tests

When checking whether any key in map A exists in map B (e.g.
"does this node have a live predecessor?"), use the dedicated
`has_common_key` instead of `iter` + exception:

```ocaml
(* Zero allocation, early-exit: *)
ReactiveHash.Map.has_common_key pred_set current

(* Allocates 5 words/call due to capturing closure: *)
try
  ReactiveHash.Map.iter (fun k () ->
    if ReactiveHash.Map.mem current k then raise Found) pred_set;
  false
with Found -> true
```

### `Obj.magic` for type-erased iteration

`ReactiveHash` stores `Obj.t` internally. The `iter` implementation
uses `Obj.magic f` to cast the user's typed callback directly,
avoiding a wrapper closure that would allocate 10 words per call:

```ocaml
(* Zero allocation — Obj.magic casts without wrapping: *)
let iter f t = table_iter_kv (Obj.magic f) t

(* 10 words/call — wrapper closure allocates: *)
let iter f t = table_iter_kv (fun k v -> f (Obj.obj k) (Obj.obj v)) t
```

This is safe because we never mix types within a single table
instance.

### `unit option` is already unboxed

OCaml represents `Some ()` identically to `()` at runtime — no
allocation. Switching `unit option` to `Maybe.t` does not
save allocations (confirmed by measurement). Focus optimization
effort on closures and non-unit option types instead.

### Use `StableQueue` for BFS/worklist patterns

Stable FIFOs (`StableQueue`) eliminate cons-cell
allocation from worklist patterns. Clear + push cycles reuse the
backing array at steady state.

### Measure, don't guess

Use `Gc.stat().minor_words` before/after to measure actual
allocation. Test at multiple sizes (n=10, 100, 1000) to distinguish
constant overhead from per-element allocation. See `AllocTest.ml`
for the pattern.

**Measurement rules:**
- `Gc.stat()` itself allocates a record — never call it inside
  measured code. Measure from outside only:
  ```ocaml
  let before = Gc.stat () in
  for _ = 1 to n do f () done;
  let after = Gc.stat () in
  ```
- Measure the actual code, not "similar" code — similar code can
  lead to red herrings due to compiler optimization differences.
- Use `stop_after_phase` to truncate execution at phase boundaries
  and measure cumulative allocation up to each point.
- Verify with assembly (`ocamlfind ocamlopt -S ...`) — look for
  `sub x27, x27, #N` (bump-pointer allocation on ARM64) inside
  the function. Count bytes / 8 = words.

## Why

`Stdlib.Hashtbl` uses chaining (linked-list buckets). Every
`clear` + `replace` cycle allocates fresh `Cons` cells on the heap.
For combinators whose `process()` runs on every scheduler wave, this
means O(n) allocations per wave just for internal bookkeeping.

`ReactiveHash` wraps Hachis, which uses open addressing with flat
arrays. After the table reaches steady-state capacity, `clear` +
`replace` reuses existing array slots — zero heap allocation.

## Step-by-step

### 1. Create the private module files

Create `Reactive<Name>.ml` and `Reactive<Name>.mli` in
`analysis/reactive/src/`.

Add the module to `private_modules` in
`analysis/reactive/src/dune` (alongside `ReactiveFixpoint`,
`ReactiveFlatMap`, `ReactiveUnion`).

### 2. Define a state type and `process_result`

The state record holds all persistent tables and scratch buffers.
Use `ReactiveHash.Map` for key-value maps and `ReactiveHash.Set`
for dedup sets.

```ocaml
type ('k, 'v) t = {
  (* persistent state *)
  target: ('k, 'v) ReactiveHash.Map.t;
  ...
  (* scratch — allocated once, cleared per process() *)
  scratch: ('k, 'v option) ReactiveHash.Map.t;
  affected: 'k ReactiveHash.Set.t;
  (* pre-allocated output buffer *)
  output_wave: ('k, 'v option) ReactiveWave.t;
}
```

`process_result` carries stats deltas back to the caller (avoids
a dependency on `Reactive.stats`):

```ocaml
type process_result = {
  entries_received: int;
  adds_received: int;
  removes_received: int;
  entries_emitted: int;
  adds_emitted: int;
  removes_emitted: int;
}
```

### 3. Implement `push`, `process`, `init_*`, and target accessors

- **`push`**: called from the subscribe callback. Writes directly
  into the scratch map (last-write-wins dedup is automatic).
- **`process`**: clears scratch and affected set, applies updates,
  recomputes affected keys, writes directly to `output_wave`.
  Returns `process_result`. No intermediate lists.
- **`init_*`**: called during setup to populate persistent state
  from existing upstream data (before subscriptions fire).
- **`iter_target`**, **`find_target`**, **`target_length`**: expose
  read access to the output state for the `('k, 'v) t` record.

### 4. Rewrite the combinator in `Reactive.ml`

The combinator function in `Reactive.ml` becomes a thin wiring
layer:

```ocaml
let my_combinator ~name ... =
  let state = ReactiveMyCombinator.create ~... ~output_wave in
  let pending_count = ref 0 in    (* one per input edge *)

  let process () =
    (* snapshot + reset pending counts *)
    (* dec_inflight for each input edge *)
    let r = ReactiveMyCombinator.process state in
    (* apply r.* to my_stats *)
    (* if r.entries_emitted > 0: notify subscribers *)
  in

  (* Registry.register, add_edge, add_combinator *)

  (* Subscribe: push into scratch, incr pending_count *)
  src.subscribe (fun wave ->
      Registry.inc_inflight ...;
      incr pending_count;
      ReactiveWave.iter wave (fun k v_opt ->
          ReactiveMyCombinator.push state k v_opt);
      Registry.mark_dirty name);

  (* Initialize from existing data *)
  src.iter (fun k v -> ReactiveMyCombinator.init_entry state k v);

  { name; subscribe; iter; get; length; stats; level }
```

### 5. Key patterns to follow

**Replace per-process-call allocations:**
| Old (Hashtbl)                              | New (ReactiveHash)                       |
|--------------------------------------------|------------------------------------------|
| `pending := wave :: !pending`              | `ReactiveWave.iter wave push_to_scratch` |
| `merge_wave_entries !pending`              | scratch map already merged               |
| `merge_entries entries`                    | scratch map already deduped              |
| `Hashtbl.create n` for `seen`             | persistent `ReactiveHash.Set`, `clear`   |
| `List.filter_map ... recompute_target`    | `ReactiveHash.Set.iter` + write to wave  |
| `count_adds_removes entries` (list walk)  | count inline with `ref` during iteration |

**Eliminate intermediate lists:**
- `recompute_target` should write directly to `output_wave`
  instead of returning `Some (k, v_opt)`.
- Fold contributions directly instead of building a values list.
- `remove_source`/`add_source` should add to the `affected` set
  instead of returning key lists.

**Things that still allocate (inherent to the algorithm):**
- User-supplied functions (`f`, `key_of`) return lists — can't
  be avoided without changing the public API.
- `provenance` stores `'k list` per input key.
- Inner contribution maps are created when new output keys appear.

### 6. Build and test

```bash
dune build @analysis/reactive/src/all
make -C analysis/reactive test
```

## Appendix: Conversion Status

### Converted

| Combinator | Module                | Notes                                    |
|------------|-----------------------|------------------------------------------|
| `union`    | `ReactiveUnion.ml`    | Fully zero-alloc steady-state process()  |
| `flatMap`  | `ReactiveFlatMap.ml`  | Zero-alloc except user `f` and provenance lists |
| `join`     | `ReactiveJoin.ml`     | Zero-alloc except user `f`, `key_of`, provenance lists, and reverse-index list updates |
| `fixpoint` | `ReactiveFixpoint.ml` | Zero-alloc steady-state (constant 4 words/iter regardless of n). See remaining items below |
| `source`   | N/A                   | No internal tables — just emits deltas |

### Remaining allocations in `ReactiveFixpoint`

Converted to `ReactiveHash`:
- Persistent state: `current`, `edge_map`, `pred_map` (including inner pred sets), `roots`
- Per-call scratch: `deleted_nodes`, `rederive_pending`, `expansion_seen`, `old_successors_for_changed`, `new_successors_for_changed`, `edge_has_new`
- Temp sets in `process_edge_change` and `apply_edge_update`

Converted to `StableQueue` (stable FIFO):
- 3 BFS queues: `delete_queue`, `rederive_queue`, `expansion_queue`
- 2 phase queues: `added_roots_queue`, `edge_change_queue`
- All persistent fields cleared per call, zero allocation at steady state

Eliminated intermediate lists, records, and closures:
- `removed_roots` cons list — replaced by inline `mark_deleted` during init iteration
- `added_roots` cons list — replaced by `added_roots_queue`
- `edge_changes` cons list + `edge_change` records — replaced by `edge_change_queue` + `new_successors_for_changed` map + `edge_has_new` set
- `process_edge_change` uses callback-based `on_removed` instead of returning a `removed_targets` list
- `has_live_predecessor` capturing closure — replaced by `Map.has_common_key` (eliminated 5 words/node/iter)

Still allocating (inherent to the algorithm or debug-only):
- **`edge_map` values** are `'k list` — list allocation is inherent when edges change.
- **`pred_map` inner maps**: new `ReactiveHash.Map` created when a node first gains predecessors (same pattern as `contributions` in flatMap/join — allocates once per new target, then reuses).
- **`compute_reachable_from_roots_with_work`**: creates a fresh `Hashtbl` for full BFS. Only called during `initialize` and in `Metrics` mode.
- **`Invariants` module**: uses `Hashtbl` for debug-only set operations (copies, diffs). Opt-in via env var — not on the hot path.
