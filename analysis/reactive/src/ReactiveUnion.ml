(** Zero-allocation union state and processing logic.

    Uses ReactiveHash (Hachis-backed) tables for all internal state.
    After steady-state capacity is reached, [process] performs zero
    heap allocation. *)

type ('k, 'v) t = {
  merge: 'v -> 'v -> 'v;
  left_values: ('k, 'v) ReactiveHash.Map.t;
  right_values: ('k, 'v) ReactiveHash.Map.t;
  target: ('k, 'v) ReactiveHash.Map.t;
  left_scratch: ('k, 'v ReactiveMaybe.t) ReactiveHash.Map.t;
  right_scratch: ('k, 'v ReactiveMaybe.t) ReactiveHash.Map.t;
  affected: 'k ReactiveHash.Set.t;
  output_wave: ('k, 'v ReactiveMaybe.t) ReactiveWave.t;
  result: process_result;
}

and process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

let create ~merge =
  {
    merge;
    left_values = ReactiveHash.Map.create ();
    right_values = ReactiveHash.Map.create ();
    target = ReactiveHash.Map.create ();
    left_scratch = ReactiveHash.Map.create ();
    right_scratch = ReactiveHash.Map.create ();
    affected = ReactiveHash.Set.create ();
    output_wave = ReactiveWave.create ();
    result =
      {
        entries_received = 0;
        adds_received = 0;
        removes_received = 0;
        entries_emitted = 0;
        adds_emitted = 0;
        removes_emitted = 0;
      };
  }

let destroy t = ReactiveWave.destroy t.output_wave

let output_wave t = t.output_wave

let push_left t k mv =
  ReactiveHash.Map.replace t.left_scratch
    (ReactiveAllocator.unsafe_from_offheap k)
    (ReactiveAllocator.unsafe_from_offheap mv)

let push_right t k mv =
  ReactiveHash.Map.replace t.right_scratch
    (ReactiveAllocator.unsafe_from_offheap k)
    (ReactiveAllocator.unsafe_from_offheap mv)

(* Module-level helpers for iter_with — avoid closure allocation *)

let apply_left_entry t k (mv : 'v ReactiveMaybe.t) =
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if ReactiveMaybe.is_some mv then (
    ReactiveHash.Map.replace t.left_values k (ReactiveMaybe.unsafe_get mv);
    r.adds_received <- r.adds_received + 1)
  else (
    ReactiveHash.Map.remove t.left_values k;
    r.removes_received <- r.removes_received + 1);
  ReactiveHash.Set.add t.affected k

let apply_right_entry t k (mv : 'v ReactiveMaybe.t) =
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if ReactiveMaybe.is_some mv then (
    ReactiveHash.Map.replace t.right_values k (ReactiveMaybe.unsafe_get mv);
    r.adds_received <- r.adds_received + 1)
  else (
    ReactiveHash.Map.remove t.right_values k;
    r.removes_received <- r.removes_received + 1);
  ReactiveHash.Set.add t.affected k

let recompute_affected_entry t k =
  let r = t.result in
  let lv = ReactiveHash.Map.find_maybe t.left_values k in
  let rv = ReactiveHash.Map.find_maybe t.right_values k in
  let has_left = ReactiveMaybe.is_some lv in
  let has_right = ReactiveMaybe.is_some rv in
  if has_left then (
    if has_right then (
      let merged =
        t.merge (ReactiveMaybe.unsafe_get lv) (ReactiveMaybe.unsafe_get rv)
      in
      ReactiveHash.Map.replace t.target k merged;
      ReactiveWave.push t.output_wave
        (ReactiveAllocator.unsafe_to_offheap k)
        (ReactiveAllocator.unsafe_to_offheap (ReactiveMaybe.some merged)))
    else
      let v = ReactiveMaybe.unsafe_get lv in
      ReactiveHash.Map.replace t.target k v;
      ReactiveWave.push t.output_wave
        (ReactiveAllocator.unsafe_to_offheap k)
        (ReactiveAllocator.unsafe_to_offheap (ReactiveMaybe.some v)))
  else if has_right then (
    let v = ReactiveMaybe.unsafe_get rv in
    ReactiveHash.Map.replace t.target k v;
    ReactiveWave.push t.output_wave
      (ReactiveAllocator.unsafe_to_offheap k)
      (ReactiveAllocator.unsafe_to_offheap (ReactiveMaybe.some v)))
  else (
    ReactiveHash.Map.remove t.target k;
    ReactiveWave.push t.output_wave
      (ReactiveAllocator.unsafe_to_offheap k)
      ReactiveMaybe.none_offheap);
  r.entries_emitted <- r.entries_emitted + 1;
  if has_left || has_right then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process t =
  ReactiveHash.Set.clear t.affected;
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.entries_emitted <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  ReactiveHash.Map.iter_with apply_left_entry t t.left_scratch;
  ReactiveHash.Map.iter_with apply_right_entry t t.right_scratch;

  ReactiveHash.Map.clear t.left_scratch;
  ReactiveHash.Map.clear t.right_scratch;

  if ReactiveHash.Set.cardinal t.affected > 0 then (
    ReactiveWave.clear t.output_wave;
    ReactiveHash.Set.iter_with recompute_affected_entry t t.affected);

  r

let init_left t k v =
  ReactiveHash.Map.replace t.left_values k v;
  ReactiveHash.Map.replace t.target k v

let init_right t k v =
  ReactiveHash.Map.replace t.right_values k v;
  let lv = ReactiveHash.Map.find_maybe t.left_values k in
  let merged =
    if ReactiveMaybe.is_some lv then t.merge (ReactiveMaybe.unsafe_get lv) v
    else v
  in
  ReactiveHash.Map.replace t.target k merged

let iter_target f t = ReactiveHash.Map.iter f t.target
let find_target t k = ReactiveHash.Map.find_maybe t.target k
let target_length t = ReactiveHash.Map.cardinal t.target
