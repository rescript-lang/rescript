(** A map from outer keys to inner maps, with pooled recycling of inner maps.

    This mirrors the churn-safe API style of [ReactivePoolMapSet] for
    map-of-map structures. *)

type ('ko, 'ki, 'v) t = {
  outer: ('ko, ('ki, 'v) ReactiveHash.Map.t) ReactiveHash.Map.t;
  mutable pool: ('ki, 'v) ReactiveHash.Map.t array;
  mutable pool_len: int;
  mutable recycle_count: int;
  mutable miss_count: int;
}

let create ~capacity:pool_capacity =
  {
    outer = ReactiveHash.Map.create ();
    pool = Array.make pool_capacity (Obj.magic 0);
    pool_len = 0;
    recycle_count = 0;
    miss_count = 0;
  }

let grow_pool t =
  let old_pool = t.pool in
  let old_cap = Array.length old_pool in
  let new_cap = max 1 (2 * old_cap) in
  let new_pool = Array.make new_cap (Obj.magic 0) in
  Array.blit old_pool 0 new_pool 0 old_cap;
  t.pool <- new_pool;
  ReactiveAllocTrace.emit_alloc_kind ReactiveAllocTrace.Pool_map_resize

let pool_push t inner =
  if t.pool_len >= Array.length t.pool then grow_pool t;
  Array.unsafe_set t.pool t.pool_len inner;
  t.pool_len <- t.pool_len + 1

let pool_pop t =
  if t.pool_len > 0 then (
    t.pool_len <- t.pool_len - 1;
    let inner = Array.unsafe_get t.pool t.pool_len in
    Array.unsafe_set t.pool t.pool_len (Obj.magic 0);
    inner)
  else (
    t.miss_count <- t.miss_count + 1;
    ReactiveAllocTrace.emit_alloc_kind ReactiveAllocTrace.Pool_map_miss_create;
    ReactiveHash.Map.create ())

let ensure_inner t ko =
  let m = ReactiveHash.Map.find_maybe t.outer ko in
  if ReactiveMaybe.is_some m then ReactiveMaybe.unsafe_get m
  else
    let inner = pool_pop t in
    ReactiveHash.Map.replace t.outer ko inner;
    inner

let replace t ko ki v =
  let inner = ensure_inner t ko in
  ReactiveHash.Map.replace inner ki v

let remove_from_inner_and_recycle_if_empty t ko ki =
  let mb = ReactiveHash.Map.find_maybe t.outer ko in
  if ReactiveMaybe.is_some mb then (
    let inner = ReactiveMaybe.unsafe_get mb in
    ReactiveHash.Map.remove inner ki;
    let after = ReactiveHash.Map.cardinal inner in
    if after = 0 then (
      ReactiveHash.Map.remove t.outer ko;
      ReactiveHash.Map.clear inner;
      pool_push t inner;
      t.recycle_count <- t.recycle_count + 1);
    ReactiveAllocTrace.emit_op_kind
      ReactiveAllocTrace.Pool_map_remove_recycle_if_empty)

let drain_outer t ko ctx f =
  let mb = ReactiveHash.Map.find_maybe t.outer ko in
  if ReactiveMaybe.is_some mb then (
    let inner = ReactiveMaybe.unsafe_get mb in
    ReactiveHash.Map.iter_with f ctx inner;
    ReactiveHash.Map.remove t.outer ko;
    ReactiveHash.Map.clear inner;
    pool_push t inner;
    t.recycle_count <- t.recycle_count + 1;
    ReactiveAllocTrace.emit_op_kind ReactiveAllocTrace.Pool_map_drain_outer)

let find_inner_maybe t ko = ReactiveHash.Map.find_maybe t.outer ko

let iter_inner_with t ko ctx f =
  let mb = ReactiveHash.Map.find_maybe t.outer ko in
  if ReactiveMaybe.is_some mb then
    ReactiveHash.Map.iter_with f ctx (ReactiveMaybe.unsafe_get mb)

let inner_cardinal t ko =
  let mb = ReactiveHash.Map.find_maybe t.outer ko in
  if ReactiveMaybe.is_some mb then
    ReactiveHash.Map.cardinal (ReactiveMaybe.unsafe_get mb)
  else 0

let outer_cardinal t = ReactiveHash.Map.cardinal t.outer

let tighten t = ReactiveHash.Map.tighten t.outer

let debug_miss_count t = t.miss_count
