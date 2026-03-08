(** A map from keys to sets, with an internal pool for recycling inner sets.

    When a key is removed via [drain_key] or
    [remove_from_set_and_recycle_if_empty], its inner set is cleared and returned
    to a pool. When a new key is added via [add], a set is taken from the
    pool (if available) instead of allocating a fresh one.

    This eliminates allocation under key churn (e.g., position keys that shift
    on every source edit). *)

type ('k, 'v) t = {
  outer: ('k, 'v ReactiveHash.Set.t) ReactiveHash.Map.t;
  mutable pool: 'v ReactiveHash.Set.t array;
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
  ReactiveAllocTrace.emit_alloc_kind ReactiveAllocTrace.Pool_set_resize

let pool_push t set =
  if t.pool_len >= Array.length t.pool then grow_pool t;
  Array.unsafe_set t.pool t.pool_len set;
  t.pool_len <- t.pool_len + 1

let pool_pop t =
  if t.pool_len > 0 then (
    t.pool_len <- t.pool_len - 1;
    let set = Array.unsafe_get t.pool t.pool_len in
    Array.unsafe_set t.pool t.pool_len (Obj.magic 0);
    set)
  else (
    t.miss_count <- t.miss_count + 1;
    ReactiveAllocTrace.emit_alloc_kind ReactiveAllocTrace.Pool_set_miss_create;
    ReactiveHash.Set.create ())

let ensure t k =
  let m = ReactiveHash.Map.find_maybe t.outer k in
  if Maybe.is_some m then Maybe.unsafe_get m
  else
    let set = pool_pop t in
    ReactiveHash.Map.replace t.outer k set;
    set

let add t k v =
  let set = ensure t k in
  ReactiveHash.Set.add set v

let drain_key t k ctx f =
  let mb = ReactiveHash.Map.find_maybe t.outer k in
  if Maybe.is_some mb then (
    let set = Maybe.unsafe_get mb in
    ReactiveHash.Set.iter_with f ctx set;
    ReactiveHash.Map.remove t.outer k;
    ReactiveHash.Set.clear set;
    pool_push t set;
    t.recycle_count <- t.recycle_count + 1;
    ReactiveAllocTrace.emit_op_kind ReactiveAllocTrace.Pool_set_drain_key)

let remove_from_set_and_recycle_if_empty t k v =
  let mb = ReactiveHash.Map.find_maybe t.outer k in
  if Maybe.is_some mb then (
    let set = Maybe.unsafe_get mb in
    ReactiveHash.Set.remove set v;
    let after = ReactiveHash.Set.cardinal set in
    if after = 0 then (
      ReactiveHash.Map.remove t.outer k;
      ReactiveHash.Set.clear set;
      pool_push t set;
      t.recycle_count <- t.recycle_count + 1);
    ReactiveAllocTrace.emit_op_kind
      ReactiveAllocTrace.Pool_set_remove_recycle_if_empty)

let find_maybe t k = ReactiveHash.Map.find_maybe t.outer k

let iter_with t ctx f = ReactiveHash.Map.iter_with f ctx t.outer

let recycle_inner_set t _k set =
  ReactiveHash.Set.clear set;
  pool_push t set;
  t.recycle_count <- t.recycle_count + 1

let clear t =
  ReactiveHash.Map.iter_with recycle_inner_set t t.outer;
  ReactiveHash.Map.clear t.outer

let tighten t = ReactiveHash.Map.tighten t.outer

let cardinal t = ReactiveHash.Map.cardinal t.outer

let debug_miss_count t = t.miss_count
