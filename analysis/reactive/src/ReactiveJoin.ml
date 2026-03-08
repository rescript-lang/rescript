(** Zero-allocation (steady-state) join state and processing logic. *)

type ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t = {
  key_of: 'k1 -> 'v1 -> 'k2;
  f: 'k1 -> 'v1 -> 'v2 Maybe.t -> ('k3 -> 'v3 -> unit) -> unit;
  merge: 'v3 -> 'v3 -> 'v3;
  right_get: 'k2 -> 'v2 Maybe.t;
  (* Persistent state *)
  left_entries: ('k1, 'v1) StableMap.t;
  provenance: ('k1, 'k3) ReactivePoolMapSet.t;
  contributions: ('k3, 'k1, 'v3) ReactivePoolMapMap.t;
  target: ('k3, 'v3) StableMap.t;
  left_to_right_key: ('k1, 'k2) StableMap.t;
  right_key_to_left_keys: ('k2, 'k1) ReactivePoolMapSet.t;
  (* Scratch — allocated once, cleared per process() *)
  left_scratch: ('k1, 'v1 Maybe.t) StableMap.t;
  right_scratch: ('k2, 'v2 Maybe.t) StableMap.t;
  affected: 'k3 StableSet.t;
  (* Pre-allocated output buffer *)
  output_wave: ('k3, 'v3 Maybe.t) ReactiveWave.t;
  (* Emit callback state — allocated once, reused per entry *)
  mutable current_k1: 'k1;
  emit_fn: 'k3 -> 'v3 -> unit;
  (* Mutable stats — allocated once, returned by process() *)
  result: process_result;
  (* Mutable merge state for recompute_target *)
  mutable merge_first: bool;
  mutable merge_acc: 'v3;
}

and process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

(* Emit callback for steady-state — marks affected *)
let add_single_contribution (t : (_, _, _, _, _, _) t) k3 v3 =
  ReactivePoolMapSet.add t.provenance t.current_k1 k3;
  ReactivePoolMapMap.replace t.contributions k3 t.current_k1 v3;
  StableSet.add t.affected (Stable.unsafe_of_value k3)

(* Emit callback for init — writes directly to target *)
let add_single_contribution_init (t : (_, _, _, _, _, _) t) k3 v3 =
  ReactivePoolMapSet.add t.provenance t.current_k1 k3;
  ReactivePoolMapMap.replace t.contributions k3 t.current_k1 v3;
  StableMap.replace t.target
    (Stable.unsafe_of_value k3)
    (Stable.unsafe_of_value v3)

let create ~key_of ~f ~merge ~right_get =
  let rec t =
    {
      key_of;
      f;
      merge;
      right_get;
      left_entries = StableMap.create ();
      provenance = ReactivePoolMapSet.create ~capacity:128;
      contributions = ReactivePoolMapMap.create ~capacity:128;
      target = StableMap.create ();
      left_to_right_key = StableMap.create ();
      right_key_to_left_keys = ReactivePoolMapSet.create ~capacity:128;
      left_scratch = StableMap.create ();
      right_scratch = StableMap.create ();
      affected = StableSet.create ();
      output_wave = ReactiveWave.create ();
      current_k1 = Obj.magic ();
      emit_fn = (fun k3 v3 -> add_single_contribution t k3 v3);
      result =
        {
          entries_received = 0;
          adds_received = 0;
          removes_received = 0;
          entries_emitted = 0;
          adds_emitted = 0;
          removes_emitted = 0;
        };
      merge_first = true;
      merge_acc = Obj.magic ();
    }
  in
  t

let destroy t =
  StableMap.destroy t.left_entries;
  StableMap.destroy t.target;
  StableMap.destroy t.left_to_right_key;
  StableMap.destroy t.left_scratch;
  StableMap.destroy t.right_scratch;
  StableSet.destroy t.affected;
  ReactiveWave.destroy t.output_wave

let output_wave t = t.output_wave

let push_left t k v_opt = StableMap.replace t.left_scratch k v_opt

let push_right t k v_opt = StableMap.replace t.right_scratch k v_opt

(* Remove one contribution key during remove_left_contributions iteration *)
let remove_one_contribution_key (t : (_, _, _, _, _, _) t) k3 =
  ReactivePoolMapMap.remove_from_inner_and_recycle_if_empty t.contributions k3
    t.current_k1;
  StableSet.add t.affected (Stable.unsafe_of_value k3)

let remove_left_contributions (t : (_, _, _, _, _, _) t) k1 =
  t.current_k1 <- k1;
  ReactivePoolMapSet.drain_key t.provenance k1 t remove_one_contribution_key

let unlink_right_key (t : (_, _, _, _, _, _) t) k1 =
  let mb =
    StableMap.find_maybe t.left_to_right_key (Stable.unsafe_of_value k1)
  in
  if Maybe.is_some mb then (
    let old_k2 = Stable.unsafe_to_value (Maybe.unsafe_get mb) in
    StableMap.remove t.left_to_right_key (Stable.unsafe_of_value k1);
    ReactivePoolMapSet.remove_from_set_and_recycle_if_empty
      t.right_key_to_left_keys old_k2 k1)

let process_left_entry (t : (_, _, _, _, _, _) t) k1 v1 =
  remove_left_contributions t k1;
  unlink_right_key t k1;
  let k2 = t.key_of k1 v1 in
  StableMap.replace t.left_to_right_key
    (Stable.unsafe_of_value k1)
    (Stable.unsafe_of_value k2);
  ReactivePoolMapSet.add t.right_key_to_left_keys k2 k1;
  let right_val = t.right_get k2 in
  t.current_k1 <- k1;
  t.f k1 v1 right_val t.emit_fn

let remove_left_entry (t : (_, _, _, _, _, _) t) k1 =
  StableMap.remove t.left_entries (Stable.unsafe_of_value k1);
  remove_left_contributions t k1;
  unlink_right_key t k1

(* Merge callback for recompute_target iter_with *)
let merge_one_contribution (t : (_, _, _, _, _, _) t) _k1 v =
  if t.merge_first then (
    t.merge_acc <- v;
    t.merge_first <- false)
  else t.merge_acc <- t.merge t.merge_acc v

let recompute_target (t : (_, _, _, _, _, _) t) k3 =
  let k3 = Stable.unsafe_to_value k3 in
  if ReactivePoolMapMap.inner_cardinal t.contributions k3 > 0 then (
    t.merge_first <- true;
    ReactivePoolMapMap.iter_inner_with t.contributions k3 t
      merge_one_contribution;
    StableMap.replace t.target
      (Stable.unsafe_of_value k3)
      (Stable.unsafe_of_value t.merge_acc);
    ReactiveWave.push t.output_wave
      (Stable.unsafe_of_value k3)
      (Stable.unsafe_of_value (Maybe.some t.merge_acc)))
  else (
    StableMap.remove t.target (Stable.unsafe_of_value k3);
    ReactiveWave.push t.output_wave
      (Stable.unsafe_of_value k3)
      Maybe.none_stable)

(* Single-pass process + count for left scratch *)
let process_left_scratch_entry (t : (_, _, _, _, _, _) t) k1 mv =
  let k1 = Stable.unsafe_to_value k1 in
  let mv = Stable.unsafe_to_value mv in
  t.result.entries_received <- t.result.entries_received + 1;
  if Maybe.is_some mv then (
    t.result.adds_received <- t.result.adds_received + 1;
    let v1 = Maybe.unsafe_get mv in
    StableMap.replace t.left_entries
      (Stable.unsafe_of_value k1)
      (Stable.unsafe_of_value v1);
    process_left_entry t k1 v1)
  else (
    t.result.removes_received <- t.result.removes_received + 1;
    remove_left_entry t k1)

(* Reprocess a left entry when its right key changed *)
let reprocess_left_entry (t : (_, _, _, _, _, _) t) k1 =
  let mb = StableMap.find_maybe t.left_entries (Stable.unsafe_of_value k1) in
  if Maybe.is_some mb then
    process_left_entry t k1 (Stable.unsafe_to_value (Maybe.unsafe_get mb))

(* Single-pass process + count for right scratch *)
let process_right_scratch_entry (t : (_, _, _, _, _, _) t) k2 _mv =
  let k2 = Stable.unsafe_to_value k2 in
  let _mv = Stable.unsafe_to_value _mv in
  t.result.entries_received <- t.result.entries_received + 1;
  if Maybe.is_some _mv then t.result.adds_received <- t.result.adds_received + 1
  else t.result.removes_received <- t.result.removes_received + 1;
  let mb = ReactivePoolMapSet.find_maybe t.right_key_to_left_keys k2 in
  if Maybe.is_some mb then
    StableHash.Set.iter_with reprocess_left_entry t (Maybe.unsafe_get mb)

let count_output_entry (r : process_result) _k mv =
  let mv = Stable.unsafe_to_value mv in
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
  ReactiveWave.clear t.output_wave;

  StableMap.iter_with process_left_scratch_entry t t.left_scratch;
  StableMap.iter_with process_right_scratch_entry t t.right_scratch;

  StableMap.clear t.left_scratch;
  StableMap.clear t.right_scratch;

  StableSet.iter_with recompute_target t t.affected;

  let num_entries = ReactiveWave.count t.output_wave in
  r.entries_emitted <- num_entries;
  if num_entries > 0 then
    ReactiveWave.iter_with t.output_wave count_output_entry r;
  r

let init_entry (t : (_, _, _, _, _, _) t) k1 v1 =
  StableMap.replace t.left_entries
    (Stable.unsafe_of_value k1)
    (Stable.unsafe_of_value v1);
  let k2 = t.key_of k1 v1 in
  StableMap.replace t.left_to_right_key
    (Stable.unsafe_of_value k1)
    (Stable.unsafe_of_value k2);
  ReactivePoolMapSet.add t.right_key_to_left_keys k2 k1;
  let right_val = t.right_get k2 in
  t.current_k1 <- k1;
  t.f k1 v1 right_val (fun k3 v3 -> add_single_contribution_init t k3 v3)

let iter_target f t =
  StableMap.iter
    (fun k v -> f (Stable.unsafe_to_value k) (Stable.unsafe_to_value v))
    t.target

let find_target t k =
  StableMap.find_maybe t.target (Stable.unsafe_of_value k) |> Maybe.to_option
  |> function
  | Some v -> Maybe.some (Stable.unsafe_to_value v)
  | None -> Maybe.none

let target_length t = StableMap.cardinal t.target
