(** A map from keys to sets, backed by stable storage.

    Each outer key owns its inner set. When an outer binding is removed, the
    inner set is destroyed immediately. *)

type ('k, 'v) t = ('k, 'v StableSet.t) StableMap.t

let create () = StableMap.create ()

let destroy t =
  StableMap.iter_with
    (fun () _k set -> StableSet.destroy (Stable.unsafe_to_value set))
    () t;
  StableMap.destroy t

let destroy_inner_set () _k set =
  StableSet.destroy (Stable.unsafe_to_value set)

let ensure t k =
  let m = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some m then Stable.unsafe_to_value (Maybe.unsafe_get m)
  else
    let set = StableSet.create () in
    StableMap.replace t
      (Stable.unsafe_of_value k)
      (Stable.unsafe_of_value set);
    set

let add t k v =
  let set = ensure t k in
  StableSet.add set (Stable.unsafe_of_value v)

let drain_key t k ctx f =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some mb then (
    let set = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableSet.iter_with (Obj.magic f) ctx set;
    StableMap.remove t (Stable.unsafe_of_value k);
    StableSet.destroy set)

let remove_from_set_and_recycle_if_empty t k v =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some mb then (
    let set = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableSet.remove set (Stable.unsafe_of_value v);
    if StableSet.cardinal set = 0 then (
      StableMap.remove t (Stable.unsafe_of_value k);
      StableSet.destroy set))

let find_inner_maybe t k =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some mb then
    Maybe.some (Stable.unsafe_to_value (Maybe.unsafe_get mb))
  else Maybe.none

let iter_inner_with t k ctx f =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some mb then
    let set = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableSet.iter_with (Obj.magic f) ctx set

let exists_inner_with t k ctx f =
  let mb = StableMap.find_maybe t (Stable.unsafe_of_value k) in
  if Maybe.is_some mb then
    let set = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableSet.exists_with (Obj.magic f) ctx set
  else false

let iter_with t ctx f =
  StableMap.iter_with (Obj.magic f) ctx t

let clear t =
  StableMap.iter_with destroy_inner_set () t;
  StableMap.clear t
let cardinal t = StableMap.cardinal t
let debug_miss_count _t = 0
