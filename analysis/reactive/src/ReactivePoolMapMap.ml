(** A map from outer keys to inner maps, backed by stable storage.

    Each outer key owns its inner map. When an outer binding is removed, the
    inner map is destroyed immediately. *)

type ('ko, 'ki, 'v) t = ('ko, ('ki, 'v) StableMap.t) StableMap.t

let create () = StableMap.create ()

let destroy t =
  StableMap.iter_with
    (fun () _ko inner -> StableMap.destroy (Stable.unsafe_to_value inner))
    () t;
  StableMap.destroy t

let ensure_inner t ko =
  let m = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some m then Stable.unsafe_to_value (Maybe.unsafe_get m)
  else
    let inner = StableMap.create () in
    StableMap.replace t
      (Stable.unsafe_of_value ko)
      (Stable.unsafe_of_value inner);
    inner

let replace t ko ki v =
  let inner = ensure_inner t ko in
  StableMap.replace inner
    (Stable.unsafe_of_value ki)
    (Stable.unsafe_of_value v)

let remove_from_inner_and_recycle_if_empty t ko ki =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some mb then (
    let inner = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableMap.remove inner (Stable.unsafe_of_value ki);
    if StableMap.cardinal inner = 0 then (
      StableMap.remove t (Stable.unsafe_of_value ko);
      StableMap.destroy inner))

let drain_outer t ko ctx f =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some mb then (
    let inner = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableMap.iter_with (Obj.magic f) ctx inner;
    StableMap.remove t (Stable.unsafe_of_value ko);
    StableMap.destroy inner)

let find_inner_maybe t ko =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some mb then
    Maybe.some (Stable.unsafe_to_value (Maybe.unsafe_get mb))
  else Maybe.none

let iter_inner_with t ko ctx f =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some mb then
    let inner = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableMap.iter_with (Obj.magic f) ctx inner

let inner_cardinal t ko =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value ko) in
  if Maybe.is_some mb then
    StableMap.cardinal (Stable.unsafe_to_value (Maybe.unsafe_get mb))
  else 0

let outer_cardinal t = StableMap.cardinal t
let debug_miss_count _t = 0
