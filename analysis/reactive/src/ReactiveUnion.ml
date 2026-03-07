(** Zero-allocation union state and processing logic. *)

type ('k, 'v) t = {
  merge: 'v -> 'v -> 'v;
  left_values: ('k, 'v) ReactiveMap.t;
  right_values: ('k, 'v) ReactiveMap.t;
  target: ('k, 'v) ReactiveMap.t;
  left_scratch: ('k, 'v Maybe.t) ReactiveMap.t;
  right_scratch: ('k, 'v Maybe.t) ReactiveMap.t;
  affected: 'k ReactiveSet.t;
  output_wave: ('k, 'v Maybe.t) ReactiveWave.t;
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
    left_values = ReactiveMap.create ();
    right_values = ReactiveMap.create ();
    target = ReactiveMap.create ();
    left_scratch = ReactiveMap.create ();
    right_scratch = ReactiveMap.create ();
    affected = ReactiveSet.create ();
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

let destroy t =
  ReactiveMap.destroy t.left_values;
  ReactiveMap.destroy t.right_values;
  ReactiveMap.destroy t.target;
  ReactiveMap.destroy t.left_scratch;
  ReactiveMap.destroy t.right_scratch;
  ReactiveSet.destroy t.affected;
  ReactiveWave.destroy t.output_wave

let output_wave t = t.output_wave

let push_left t k mv = ReactiveMap.replace t.left_scratch k mv

let push_right t k mv = ReactiveMap.replace t.right_scratch k mv

(* Module-level helpers for iter_with — avoid closure allocation *)

let apply_left_entry t k mv =
  let k = Allocator.unsafe_from_offheap k in
  let mv = Allocator.unsafe_from_offheap mv in
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if Maybe.is_some mv then (
    ReactiveMap.replace t.left_values
      (Allocator.unsafe_to_offheap k)
      (Allocator.unsafe_to_offheap (Maybe.unsafe_get mv));
    r.adds_received <- r.adds_received + 1)
  else (
    ReactiveMap.remove t.left_values (Allocator.unsafe_to_offheap k);
    r.removes_received <- r.removes_received + 1);
  ReactiveSet.add t.affected (Allocator.unsafe_to_offheap k)

let apply_right_entry t k mv =
  let k = Allocator.unsafe_from_offheap k in
  let mv = Allocator.unsafe_from_offheap mv in
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if Maybe.is_some mv then (
    ReactiveMap.replace t.right_values
      (Allocator.unsafe_to_offheap k)
      (Allocator.unsafe_to_offheap (Maybe.unsafe_get mv));
    r.adds_received <- r.adds_received + 1)
  else (
    ReactiveMap.remove t.right_values (Allocator.unsafe_to_offheap k);
    r.removes_received <- r.removes_received + 1);
  ReactiveSet.add t.affected (Allocator.unsafe_to_offheap k)

let recompute_affected_entry t k =
  let k = Allocator.unsafe_from_offheap k in
  let r = t.result in
  let lv =
    ReactiveMap.find_maybe t.left_values (Allocator.unsafe_to_offheap k)
  in
  let rv =
    ReactiveMap.find_maybe t.right_values (Allocator.unsafe_to_offheap k)
  in
  let has_left = Maybe.is_some lv in
  let has_right = Maybe.is_some rv in
  if has_left then (
    if has_right then (
      let merged =
        t.merge
          (Allocator.unsafe_from_offheap (Maybe.unsafe_get lv))
          (Allocator.unsafe_from_offheap (Maybe.unsafe_get rv))
      in
      ReactiveMap.replace t.target
        (Allocator.unsafe_to_offheap k)
        (Allocator.unsafe_to_offheap merged);
      ReactiveWave.push t.output_wave
        (Allocator.unsafe_to_offheap k)
        (Allocator.unsafe_to_offheap (Maybe.some merged)))
    else
      let v = Allocator.unsafe_from_offheap (Maybe.unsafe_get lv) in
      ReactiveMap.replace t.target
        (Allocator.unsafe_to_offheap k)
        (Allocator.unsafe_to_offheap v);
      ReactiveWave.push t.output_wave
        (Allocator.unsafe_to_offheap k)
        (Allocator.unsafe_to_offheap (Maybe.some v)))
  else if has_right then (
    let v = Allocator.unsafe_from_offheap (Maybe.unsafe_get rv) in
    ReactiveMap.replace t.target
      (Allocator.unsafe_to_offheap k)
      (Allocator.unsafe_to_offheap v);
    ReactiveWave.push t.output_wave
      (Allocator.unsafe_to_offheap k)
      (Allocator.unsafe_to_offheap (Maybe.some v)))
  else (
    ReactiveMap.remove t.target (Allocator.unsafe_to_offheap k);
    ReactiveWave.push t.output_wave
      (Allocator.unsafe_to_offheap k)
      Maybe.none_offheap);
  r.entries_emitted <- r.entries_emitted + 1;
  if has_left || has_right then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process t =
  ReactiveSet.clear t.affected;
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.entries_emitted <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  ReactiveMap.iter_with apply_left_entry t t.left_scratch;
  ReactiveMap.iter_with apply_right_entry t t.right_scratch;

  ReactiveMap.clear t.left_scratch;
  ReactiveMap.clear t.right_scratch;

  if ReactiveSet.cardinal t.affected > 0 then (
    ReactiveWave.clear t.output_wave;
    ReactiveSet.iter_with recompute_affected_entry t t.affected);

  r

let init_left t k v =
  ReactiveMap.replace t.left_values
    (Allocator.unsafe_to_offheap k)
    (Allocator.unsafe_to_offheap v);
  ReactiveMap.replace t.target
    (Allocator.unsafe_to_offheap k)
    (Allocator.unsafe_to_offheap v)

let init_right t k v =
  ReactiveMap.replace t.right_values
    (Allocator.unsafe_to_offheap k)
    (Allocator.unsafe_to_offheap v);
  let lv =
    ReactiveMap.find_maybe t.left_values (Allocator.unsafe_to_offheap k)
  in
  let merged =
    if Maybe.is_some lv then
      t.merge (Allocator.unsafe_from_offheap (Maybe.unsafe_get lv)) v
    else v
  in
  ReactiveMap.replace t.target
    (Allocator.unsafe_to_offheap k)
    (Allocator.unsafe_to_offheap merged)

let iter_target f t =
  ReactiveMap.iter
    (fun k v ->
      f (Allocator.unsafe_from_offheap k) (Allocator.unsafe_from_offheap v))
    t.target

let find_target t k =
  ReactiveMap.find_maybe t.target (Allocator.unsafe_to_offheap k)
  |> Maybe.to_option
  |> function
  | Some v -> Maybe.some (Allocator.unsafe_from_offheap v)
  | None -> Maybe.none

let target_length t = ReactiveMap.cardinal t.target
