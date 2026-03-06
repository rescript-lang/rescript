(** Zero-allocation (steady-state) flatMap state and processing logic. *)

type ('k1, 'v1, 'k2, 'v2) t = {
  f: 'k1 -> 'v1 -> ('k2 -> 'v2 -> unit) -> unit;
  merge: 'v2 -> 'v2 -> 'v2;
  (* Persistent state *)
  provenance: ('k1, 'k2) ReactivePoolMapSet.t;
  contributions: ('k2, 'k1, 'v2) ReactivePoolMapMap.t;
  target: ('k2, 'v2) ReactiveMap.t;
  (* Scratch — allocated once, cleared per process() *)
  scratch: ('k1, 'v1 ReactiveMaybe.t) ReactiveMap.t;
  affected: 'k2 ReactiveSet.t;
  (* Pre-allocated output buffer *)
  output_wave: ('k2, 'v2 ReactiveMaybe.t) ReactiveWave.t;
  (* Emit callback state — allocated once, reused per entry *)
  mutable current_k1: 'k1;
  emit_fn: 'k2 -> 'v2 -> unit;
  (* Mutable stats — allocated once, returned by process() *)
  result: process_result;
  (* Mutable merge state for recompute_target *)
  mutable merge_first: bool;
  mutable merge_acc: 'v2;
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
  ReactivePoolMapSet.add t.provenance t.current_k1 k2;
  ReactivePoolMapMap.replace t.contributions k2 t.current_k1 v2;
  ReactiveSet.add t.affected (ReactiveAllocator.unsafe_to_offheap k2)

(* Emit callback for init — writes directly to target *)
let add_single_contribution_init (t : (_, _, _, _) t) k2 v2 =
  ReactivePoolMapSet.add t.provenance t.current_k1 k2;
  ReactivePoolMapMap.replace t.contributions k2 t.current_k1 v2;
  ReactiveMap.replace t.target
    (ReactiveAllocator.unsafe_to_offheap k2)
    (ReactiveAllocator.unsafe_to_offheap v2)

let create ~f ~merge =
  let rec t =
    {
      f;
      merge;
      provenance = ReactivePoolMapSet.create ~capacity:128;
      contributions = ReactivePoolMapMap.create ~capacity:128;
      target = ReactiveMap.create ();
      scratch = ReactiveMap.create ();
      affected = ReactiveSet.create ();
      output_wave = ReactiveWave.create ();
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
  ReactiveMap.destroy t.target;
  ReactiveMap.destroy t.scratch;
  ReactiveSet.destroy t.affected;
  ReactiveWave.destroy t.output_wave

let output_wave t = t.output_wave

let push t k v_opt = ReactiveMap.replace t.scratch k v_opt

(* Remove one contribution key during remove_source iteration *)
let remove_one_contribution (t : (_, _, _, _) t) k2 =
  ReactivePoolMapMap.remove_from_inner_and_recycle_if_empty t.contributions k2
    t.current_k1;
  ReactiveSet.add t.affected (ReactiveAllocator.unsafe_to_offheap k2)

let remove_source (t : (_, _, _, _) t) k1 =
  t.current_k1 <- k1;
  ReactivePoolMapSet.drain_key t.provenance k1 t remove_one_contribution

(* Merge callback for recompute_target iter_with *)
let merge_one_contribution (t : (_, _, _, _) t) _k1 v =
  if t.merge_first then (
    t.merge_acc <- v;
    t.merge_first <- false)
  else t.merge_acc <- t.merge t.merge_acc v

let recompute_target (t : (_, _, _, _) t) k2 =
  let k2 = ReactiveAllocator.unsafe_from_offheap k2 in
  if ReactivePoolMapMap.inner_cardinal t.contributions k2 > 0 then (
    t.merge_first <- true;
    ReactivePoolMapMap.iter_inner_with t.contributions k2 t
      merge_one_contribution;
    ReactiveMap.replace t.target
      (ReactiveAllocator.unsafe_to_offheap k2)
      (ReactiveAllocator.unsafe_to_offheap t.merge_acc);
    ReactiveWave.push t.output_wave
      (ReactiveAllocator.unsafe_to_offheap k2)
      (ReactiveAllocator.unsafe_to_offheap (ReactiveMaybe.some t.merge_acc)))
  else (
    ReactiveMap.remove t.target (ReactiveAllocator.unsafe_to_offheap k2);
    ReactiveWave.push t.output_wave
      (ReactiveAllocator.unsafe_to_offheap k2)
      ReactiveMaybe.none_offheap)

(* Single-pass process + count for scratch *)
let process_scratch_entry (t : (_, _, _, _) t) k1 mv =
  let k1 = ReactiveAllocator.unsafe_from_offheap k1 in
  let mv = ReactiveAllocator.unsafe_from_offheap mv in
  t.result.entries_received <- t.result.entries_received + 1;
  remove_source t k1;
  if ReactiveMaybe.is_some mv then (
    t.result.adds_received <- t.result.adds_received + 1;
    let v1 = ReactiveMaybe.unsafe_get mv in
    t.current_k1 <- k1;
    t.f k1 v1 t.emit_fn)
  else t.result.removes_received <- t.result.removes_received + 1

let count_output_entry (r : process_result) _k mv =
  let mv = ReactiveAllocator.unsafe_from_offheap mv in
  if ReactiveMaybe.is_some mv then r.adds_emitted <- r.adds_emitted + 1
  else r.removes_emitted <- r.removes_emitted + 1

let process (t : (_, _, _, _) t) =
  let r = t.result in
  r.entries_received <- 0;
  r.adds_received <- 0;
  r.removes_received <- 0;
  r.adds_emitted <- 0;
  r.removes_emitted <- 0;

  ReactiveSet.clear t.affected;
  ReactiveWave.clear t.output_wave;

  ReactiveMap.iter_with process_scratch_entry t t.scratch;
  ReactiveMap.clear t.scratch;

  ReactiveSet.iter_with recompute_target t t.affected;

  let num_entries = ReactiveWave.count t.output_wave in
  r.entries_emitted <- num_entries;
  if num_entries > 0 then
    ReactiveWave.iter_with t.output_wave count_output_entry r;
  r

let init_entry (t : (_, _, _, _) t) k1 v1 =
  t.current_k1 <- k1;
  t.f k1 v1 (fun k2 v2 -> add_single_contribution_init t k2 v2)

let iter_target f t =
  ReactiveMap.iter
    (fun k v ->
      f
        (ReactiveAllocator.unsafe_from_offheap k)
        (ReactiveAllocator.unsafe_from_offheap v))
    t.target

let find_target t k =
  ReactiveMap.find_maybe t.target (ReactiveAllocator.unsafe_to_offheap k)
  |> ReactiveMaybe.to_option
  |> function
  | Some v -> ReactiveMaybe.some (ReactiveAllocator.unsafe_from_offheap v)
  | None -> ReactiveMaybe.none

let target_length t = ReactiveMap.cardinal t.target
