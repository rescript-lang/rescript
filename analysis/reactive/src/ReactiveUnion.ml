(** Zero-allocation union state and processing logic. *)

type ('k, 'v) t = {
  merge: 'v Stable.t -> 'v Stable.t -> 'v Stable.t;
  left_values: ('k, 'v) StableMap.t;
  right_values: ('k, 'v) StableMap.t;
  target: ('k, 'v) StableMap.t;
  left_scratch: ('k, 'v Maybe.t) StableMap.t;
  right_scratch: ('k, 'v Maybe.t) StableMap.t;
  affected: 'k StableSet.t;
  output_wave: ('k, 'v Maybe.t) StableWave.t;
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
    left_values = StableMap.create ();
    right_values = StableMap.create ();
    target = StableMap.create ();
    left_scratch = StableMap.create ();
    right_scratch = StableMap.create ();
    affected = StableSet.create ();
    output_wave = StableWave.create ();
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
  StableMap.destroy t.left_values;
  StableMap.destroy t.right_values;
  StableMap.destroy t.target;
  StableMap.destroy t.left_scratch;
  StableMap.destroy t.right_scratch;
  StableSet.destroy t.affected;
  StableWave.destroy t.output_wave

let output_wave t = t.output_wave

let push_left t k mv = StableMap.replace t.left_scratch k mv

let push_right t k mv = StableMap.replace t.right_scratch k mv

(* Module-level helpers for iter_with — avoid closure allocation *)

let apply_left_entry t k mv =
  let mv = Maybe.of_stable mv in
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if Maybe.is_some mv then (
    StableMap.replace t.left_values k (Maybe.unsafe_get mv);
    r.adds_received <- r.adds_received + 1)
  else (
    StableMap.remove t.left_values k;
    r.removes_received <- r.removes_received + 1);
  StableSet.add t.affected k

let apply_right_entry t k mv =
  let mv = Maybe.of_stable mv in
  let r = t.result in
  r.entries_received <- r.entries_received + 1;
  if Maybe.is_some mv then (
    StableMap.replace t.right_values k (Maybe.unsafe_get mv);
    r.adds_received <- r.adds_received + 1)
  else (
    StableMap.remove t.right_values k;
    r.removes_received <- r.removes_received + 1);
  StableSet.add t.affected k

let recompute_affected_entry t k =
  let r = t.result in
  let lv = StableMap.find_maybe t.left_values k in
  let rv = StableMap.find_maybe t.right_values k in
  let has_left = Maybe.is_some lv in
  let has_right = Maybe.is_some rv in
  if has_left then (
    if has_right then (
      let merged = t.merge (Maybe.unsafe_get lv) (Maybe.unsafe_get rv) in
      StableMap.replace t.target k merged;
      StableWave.push t.output_wave k (Maybe.to_stable (Maybe.some merged)))
    else
      let v = Maybe.unsafe_get lv in
      StableMap.replace t.target k v;
      StableWave.push t.output_wave k (Maybe.to_stable (Maybe.some v)))
  else if has_right then (
    let v = Maybe.unsafe_get rv in
    StableMap.replace t.target k v;
    StableWave.push t.output_wave k (Maybe.to_stable (Maybe.some v)))
  else (
    StableMap.remove t.target k;
    StableWave.push t.output_wave k Maybe.none_stable);
  r.entries_emitted <- r.entries_emitted + 1;
  if has_left || has_right then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process t =
  StableSet.clear t.affected;
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.entries_emitted <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  StableMap.iter_with apply_left_entry t t.left_scratch;
  StableMap.iter_with apply_right_entry t t.right_scratch;

  StableMap.clear t.left_scratch;
  StableMap.clear t.right_scratch;

  if StableSet.cardinal t.affected > 0 then (
    StableWave.clear t.output_wave;
    StableSet.iter_with recompute_affected_entry t t.affected);

  r

let init_left t k v =
  StableMap.replace t.left_values k v;
  StableMap.replace t.target k v

let init_right t k v =
  StableMap.replace t.right_values k v;
  let lv = StableMap.find_maybe t.left_values k in
  let merged =
    if Maybe.is_some lv then t.merge (Maybe.unsafe_get lv) v else v
  in
  StableMap.replace t.target k merged

let iter_target f t = StableMap.iter f t.target

let find_target t k = StableMap.find_maybe t.target k

let target_length t = StableMap.cardinal t.target
