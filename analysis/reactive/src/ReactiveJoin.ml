(** Zero-allocation (steady-state) join state and processing logic. *)

type ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t = {
  key_of: 'k1 Stable.t -> 'v1 Stable.t -> 'k2 Stable.t;
  f:
    'k1 Stable.t ->
    'v1 Stable.t ->
    'v2 Stable.t Maybe.t ->
    ('k3, 'v3) StableWave.t ->
    unit;
  merge: 'v3 Stable.t -> 'v3 Stable.t -> 'v3 Stable.t;
  right_get: 'k2 Stable.t -> 'v2 Stable.t Maybe.t;
  (* Persistent state *)
  left_entries: ('k1, 'v1) StableMap.t;
  provenance: ('k1, 'k3) StableMapSet.t;
  contributions: ('k3, 'k1, 'v3) StableMapMap.t;
  target: ('k3, 'v3) StableMap.t;
  left_to_right_key: ('k1, 'k2) StableMap.t;
  right_key_to_left_keys: ('k2, 'k1) StableMapSet.t;
  (* Scratch — allocated once, cleared per process() *)
  left_scratch: ('k1, 'v1 Maybe.t) StableMap.t;
  right_scratch: ('k2, 'v2 Maybe.t) StableMap.t;
  affected: 'k3 StableSet.t;
  (* Pre-allocated output buffer *)
  output_wave: ('k3, 'v3 Maybe.t) StableWave.t;
  (* Pre-allocated buffer for f's emissions *)
  emit_wave: ('k3, 'v3) StableWave.t;
  (* Set before drain_key / emit_wave iteration *)
  mutable current_k1: 'k1 Stable.t Maybe.t;
  (* Mutable stats — allocated once, returned by process() *)
  result: process_result;
  (* Merge accumulator for recompute_target — Maybe.none = first element *)
  mutable merge_acc: 'v3 Stable.t Maybe.t;
}

and process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

(* Record one contribution from the emit wave — marks affected *)
let record_contribution (t : (_, _, _, _, _, _) t) k3 v3 =
  let k1 = Maybe.unsafe_get t.current_k1 in
  StableMapSet.add t.provenance k1 k3;
  StableMapMap.replace t.contributions k3 k1 v3;
  StableSet.add t.affected k3

(* Record one contribution and write directly to target (init path) *)
let record_contribution_init (t : (_, _, _, _, _, _) t) k3 v3 =
  let k1 = Maybe.unsafe_get t.current_k1 in
  StableMapSet.add t.provenance k1 k3;
  StableMapMap.replace t.contributions k3 k1 v3;
  StableMap.replace t.target k3 v3

let create ~key_of ~f ~merge ~right_get =
  {
    key_of;
    f;
    merge;
    right_get;
    left_entries = StableMap.create ();
    provenance = StableMapSet.create ();
    contributions = StableMapMap.create ();
    target = StableMap.create ();
    left_to_right_key = StableMap.create ();
    right_key_to_left_keys = StableMapSet.create ();
    left_scratch = StableMap.create ();
    right_scratch = StableMap.create ();
    affected = StableSet.create ();
    output_wave = StableWave.create ();
    emit_wave = StableWave.create ();
    current_k1 = Maybe.none;
    result =
      {
        entries_received = 0;
        adds_received = 0;
        removes_received = 0;
        entries_emitted = 0;
        adds_emitted = 0;
        removes_emitted = 0;
      };
    merge_acc = Maybe.none;
  }

let destroy t =
  StableMap.destroy t.left_entries;
  StableMapSet.destroy t.provenance;
  StableMapMap.destroy t.contributions;
  StableMap.destroy t.target;
  StableMap.destroy t.left_to_right_key;
  StableMapSet.destroy t.right_key_to_left_keys;
  StableMap.destroy t.left_scratch;
  StableMap.destroy t.right_scratch;
  StableSet.destroy t.affected;
  StableWave.destroy t.output_wave;
  StableWave.destroy t.emit_wave

let output_wave t = t.output_wave

let push_left t k v_opt = StableMap.replace t.left_scratch k v_opt

let push_right t k v_opt = StableMap.replace t.right_scratch k v_opt

(* Remove one contribution key during remove_left_contributions iteration *)
let remove_one_contribution_key (t : (_, _, _, _, _, _) t) k3 =
  StableMapMap.remove_from_inner_and_recycle_if_empty t.contributions k3
    (Maybe.unsafe_get t.current_k1);
  StableSet.add t.affected k3

let remove_left_contributions (t : (_, _, _, _, _, _) t) k1 =
  t.current_k1 <- Maybe.some k1;
  StableMapSet.drain_key t.provenance k1 t remove_one_contribution_key

let unlink_right_key (t : (_, _, _, _, _, _) t) k1 =
  let mb = StableMap.find_maybe t.left_to_right_key k1 in
  if Maybe.is_some mb then (
    let old_k2 = Maybe.unsafe_get mb in
    StableMap.remove t.left_to_right_key k1;
    StableMapSet.remove_from_set_and_recycle_if_empty t.right_key_to_left_keys
      old_k2 k1)

let process_left_entry (t : (_, _, _, _, _, _) t) k1 v1 =
  remove_left_contributions t k1;
  unlink_right_key t k1;
  let k2 = t.key_of k1 v1 in
  StableMap.replace t.left_to_right_key k1 k2;
  StableMapSet.add t.right_key_to_left_keys k2 k1;
  let right_val = t.right_get k2 in
  t.current_k1 <- Maybe.some k1;
  StableWave.clear t.emit_wave;
  t.f k1 v1 right_val t.emit_wave;
  StableWave.iter_with t.emit_wave record_contribution t

let remove_left_entry (t : (_, _, _, _, _, _) t) k1 =
  StableMap.remove t.left_entries k1;
  remove_left_contributions t k1;
  unlink_right_key t k1

(* Merge callback for recompute_target iter_with *)
let merge_one_contribution (t : (_, _, _, _, _, _) t) _k1 v =
  if Maybe.is_none t.merge_acc then t.merge_acc <- Maybe.some v
  else t.merge_acc <- Maybe.some (t.merge (Maybe.unsafe_get t.merge_acc) v)

let recompute_target (t : (_, _, _, _, _, _) t) k3 =
  if StableMapMap.inner_cardinal t.contributions k3 > 0 then (
    t.merge_acc <- Maybe.none;
    StableMapMap.iter_inner_with t.contributions k3 t merge_one_contribution;
    StableMap.replace t.target k3 (Maybe.unsafe_get t.merge_acc);
    StableWave.push t.output_wave k3 (Maybe.to_stable t.merge_acc))
  else (
    StableMap.remove t.target k3;
    StableWave.push t.output_wave k3 Maybe.none_stable)

(* Single-pass process + count for left scratch *)
let process_left_scratch_entry (t : (_, _, _, _, _, _) t) k1 mv =
  let mv = Maybe.of_stable mv in
  t.result.entries_received <- t.result.entries_received + 1;
  if Maybe.is_some mv then (
    t.result.adds_received <- t.result.adds_received + 1;
    let v1 = Maybe.unsafe_get mv in
    StableMap.replace t.left_entries k1 v1;
    process_left_entry t k1 v1)
  else (
    t.result.removes_received <- t.result.removes_received + 1;
    remove_left_entry t k1)

(* Reprocess a left entry when its right key changed *)
let reprocess_left_entry (t : (_, _, _, _, _, _) t) k1 =
  let mb = StableMap.find_maybe t.left_entries k1 in
  if Maybe.is_some mb then process_left_entry t k1 (Maybe.unsafe_get mb)

(* Single-pass process + count for right scratch *)
let process_right_scratch_entry (t : (_, _, _, _, _, _) t) k2 mv =
  let mv = Maybe.of_stable mv in
  t.result.entries_received <- t.result.entries_received + 1;
  if Maybe.is_some mv then t.result.adds_received <- t.result.adds_received + 1
  else t.result.removes_received <- t.result.removes_received + 1;
  StableMapSet.iter_inner_with t.right_key_to_left_keys k2 t
    reprocess_left_entry

let count_output_entry (r : process_result) _k mv =
  let mv = Maybe.of_stable mv in
  if Maybe.is_some mv then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process (t : (_, _, _, _, _, _) t) =
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  StableSet.clear t.affected;
  StableWave.clear t.output_wave;

  StableMap.iter_with process_left_scratch_entry t t.left_scratch;
  StableMap.iter_with process_right_scratch_entry t t.right_scratch;

  StableMap.clear t.left_scratch;
  StableMap.clear t.right_scratch;

  StableSet.iter_with recompute_target t t.affected;

  let num_entries = StableWave.count t.output_wave in
  r.entries_emitted <- num_entries;
  if num_entries > 0 then
    StableWave.iter_with t.output_wave count_output_entry r;
  r

let init_entry (t : (_, _, _, _, _, _) t) k1 v1 =
  StableMap.replace t.left_entries k1 v1;
  let k2 = t.key_of k1 v1 in
  StableMap.replace t.left_to_right_key k1 k2;
  StableMapSet.add t.right_key_to_left_keys k2 k1;
  let right_val = t.right_get k2 in
  t.current_k1 <- Maybe.some k1;
  StableWave.clear t.emit_wave;
  t.f k1 v1 right_val t.emit_wave;
  StableWave.iter_with t.emit_wave record_contribution_init t

let iter_target f t = StableMap.iter f t.target

let find_target t k = StableMap.find_maybe t.target k

let target_length t = StableMap.cardinal t.target
