(** Zero-allocation (steady-state) flatMap state and processing logic. *)

type ('k1, 'v1, 'k2, 'v2) t = {
  f:
    'k1 Stable.t ->
    'v1 Stable.t ->
    ('k2 Stable.t -> 'v2 Stable.t -> unit) ->
    unit;
  merge: 'v2 Stable.t -> 'v2 Stable.t -> 'v2 Stable.t;
  (* Persistent state *)
  provenance: ('k1, 'k2) StableMapSet.t;
  contributions: ('k2, 'k1, 'v2) StableMapMap.t;
  target: ('k2, 'v2) StableMap.t;
  (* Scratch — allocated once, cleared per process() *)
  scratch: ('k1, 'v1 Maybe.t) StableMap.t;
  affected: 'k2 StableSet.t;
  (* Pre-allocated output buffer *)
  output_wave: ('k2, 'v2 Maybe.t) StableWave.t;
  (* Emit callback state — allocated once, reused per entry *)
  mutable current_k1: 'k1 Stable.t;
  emit_fn: 'k2 Stable.t -> 'v2 Stable.t -> unit;
  (* Mutable stats — allocated once, returned by process() *)
  result: process_result;
  (* Mutable merge state for recompute_target *)
  mutable merge_first: bool;
  mutable merge_acc: 'v2 Stable.t;
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
let add_single_contribution (t : (_, _, _, _) t) k2 v2 =
  StableMapSet.add t.provenance t.current_k1 k2;
  StableMapMap.replace t.contributions k2 t.current_k1 v2;
  StableSet.add t.affected k2

(* Emit callback for init — writes directly to target *)
let add_single_contribution_init (t : (_, _, _, _) t) k2 v2 =
  StableMapSet.add t.provenance t.current_k1 k2;
  StableMapMap.replace t.contributions k2 t.current_k1 v2;
  StableMap.replace t.target k2 v2

let create ~f ~merge =
  let rec t =
    {
      f;
      merge;
      provenance = StableMapSet.create ();
      contributions = StableMapMap.create ();
      target = StableMap.create ();
      scratch = StableMap.create ();
      affected = StableSet.create ();
      output_wave = StableWave.create ();
      current_k1 = Obj.magic ();
      emit_fn = (fun k2 v2 -> add_single_contribution t k2 v2);
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
  StableMapSet.destroy t.provenance;
  StableMapMap.destroy t.contributions;
  StableMap.destroy t.target;
  StableMap.destroy t.scratch;
  StableSet.destroy t.affected;
  StableWave.destroy t.output_wave

let output_wave t = t.output_wave

let push t k v_opt = StableMap.replace t.scratch k v_opt

(* Remove one contribution key during remove_source iteration *)
let remove_one_contribution (t : (_, _, _, _) t) k2 =
  StableMapMap.remove_from_inner_and_recycle_if_empty t.contributions k2
    t.current_k1;
  StableSet.add t.affected k2

let remove_source (t : (_, _, _, _) t) k1 =
  t.current_k1 <- k1;
  StableMapSet.drain_key t.provenance k1 t remove_one_contribution

(* Merge callback for recompute_target iter_with *)
let merge_one_contribution (t : (_, _, _, _) t) _k1 v =
  if t.merge_first then (
    t.merge_acc <- v;
    t.merge_first <- false)
  else t.merge_acc <- t.merge t.merge_acc v

let recompute_target (t : (_, _, _, _) t) k2 =
  if StableMapMap.inner_cardinal t.contributions k2 > 0 then (
    t.merge_first <- true;
    StableMapMap.iter_inner_with t.contributions k2 t merge_one_contribution;
    StableMap.replace t.target k2 t.merge_acc;
    StableWave.push t.output_wave k2 (Maybe.to_stable (Maybe.some t.merge_acc)))
  else (
    StableMap.remove t.target k2;
    StableWave.push t.output_wave k2 Maybe.none_stable)

(* Single-pass process + count for scratch *)
let process_scratch_entry (t : (_, _, _, _) t) k1 mv =
  let mv = Maybe.of_stable mv in
  t.result.entries_received <- t.result.entries_received + 1;
  remove_source t k1;
  if Maybe.is_some mv then (
    t.result.adds_received <- t.result.adds_received + 1;
    let v1 = Maybe.unsafe_get mv in
    t.current_k1 <- k1;
    t.f k1 v1 t.emit_fn)
  else t.result.removes_received <- t.result.removes_received + 1

let count_output_entry (r : process_result) _k mv =
  let mv = Maybe.of_stable mv in
  if Maybe.is_some mv then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process (t : (_, _, _, _) t) =
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  StableSet.clear t.affected;
  StableWave.clear t.output_wave;

  StableMap.iter_with process_scratch_entry t t.scratch;
  StableMap.clear t.scratch;

  StableSet.iter_with recompute_target t t.affected;

  let num_entries = StableWave.count t.output_wave in
  r.entries_emitted <- num_entries;
  if num_entries > 0 then
    StableWave.iter_with t.output_wave count_output_entry r;
  r

let init_entry (t : (_, _, _, _) t) k1 v1 =
  t.current_k1 <- k1;
  t.f k1 v1 (fun k2 v2 -> add_single_contribution_init t k2 v2)

let iter_target f t = StableMap.iter f t.target

let find_target t k = StableMap.find_maybe t.target k

let target_length t = StableMap.cardinal t.target
