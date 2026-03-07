(** Shared test helpers for Reactive tests *)

open Reactive

(** {1 Wave-based emit helpers} *)

(** Shared scratch wave for test emit helpers.
    Tests are single-threaded so one global wave is safe.
    The wave stores [Obj.t] internally, so a single concrete instance
    can be safely reused at any type via [Obj.magic]. *)
let scratch_wave : (int, int) ReactiveWave.t = ReactiveWave.create ()

let wave () : ('k, 'v) ReactiveWave.t = Obj.magic scratch_wave

(** Emit a single set entry *)
let emit_set emit k v =
  let w = wave () in
  ReactiveWave.clear w;
  ReactiveWave.push w
    (Allocator.unsafe_to_offheap k)
    (Allocator.unsafe_to_offheap (Maybe.some v));
  emit w

(** Emit a single edge-set entry, converting the successor list to the
    explicit offheap-list type. *)
let emit_edge_set emit k vs =
  let w = wave () in
  ReactiveWave.clear w;
  ReactiveWave.push w
    (Allocator.unsafe_to_offheap k)
    (Maybe.maybe_offheap_list_to_offheap
       (Maybe.some (ReactiveOffheapList.unsafe_of_list vs)));
  emit w

(** Emit a single remove entry *)
let emit_remove emit k =
  let w = wave () in
  ReactiveWave.clear w;
  ReactiveWave.push w (Allocator.unsafe_to_offheap k) Maybe.none_offheap;
  emit w

(** Emit a batch of (key, value) set entries *)
let emit_sets emit entries =
  let w = wave () in
  ReactiveWave.clear w;
  List.iter
    (fun (k, v) ->
      ReactiveWave.push w
        (Allocator.unsafe_to_offheap k)
        (Allocator.unsafe_to_offheap (Maybe.some v)))
    entries;
  emit w

(** Emit a batch of (key, value option) entries — for mixed set/remove batches *)
let emit_batch emit entries =
  let w = wave () in
  ReactiveWave.clear w;
  List.iter
    (fun (k, v_opt) ->
      match v_opt with
      | Some v ->
        ReactiveWave.push w
          (Allocator.unsafe_to_offheap k)
          (Allocator.unsafe_to_offheap (Maybe.some v))
      | None ->
        ReactiveWave.push w (Allocator.unsafe_to_offheap k) Maybe.none_offheap)
    entries;
  emit w

(** Emit a batch of edge entries using the explicit offheap-list type. *)
let emit_edge_batch emit entries =
  let w = wave () in
  ReactiveWave.clear w;
  List.iter
    (fun (k, vs_opt) ->
      match vs_opt with
      | Some vs ->
        ReactiveWave.push w
          (Allocator.unsafe_to_offheap k)
          (Maybe.maybe_offheap_list_to_offheap
             (Maybe.some (ReactiveOffheapList.unsafe_of_list vs)))
      | None ->
        ReactiveWave.push w (Allocator.unsafe_to_offheap k) Maybe.none_offheap)
    entries;
  emit w

(** {1 Compatibility helpers} *)

(* subscribe takes collection first in V2, but we want handler first for compatibility *)
let subscribe handler t =
  t.subscribe (fun wave ->
      let rev_entries = ref [] in
      ReactiveWave.iter wave (fun k mv ->
          let k = Allocator.unsafe_from_offheap k in
          let mv = Allocator.unsafe_from_offheap mv in
          rev_entries := (k, mv) :: !rev_entries);
      handler (List.rev !rev_entries))

(* Helper to track added/removed across all delta types *)
let[@warning "-32"] track_changes () =
  let added = ref [] in
  let removed = ref [] in
  let handler = function
    | entries ->
      List.iter
        (fun (k, mv) ->
          if Maybe.is_some mv then added := k :: !added
          else removed := k :: !removed)
        entries
  in
  (added, removed, handler)

(** {1 File helpers} *)

let[@warning "-32"] read_lines path =
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !lines

let[@warning "-32"] write_lines path lines =
  let oc = open_out path in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines;
  close_out oc

(** {1 Maybe/option helpers} *)

(** Convert [get] result to option for test assertions *)
let get_opt t k = Maybe.to_option (get t k)

(** {1 Common set modules} *)

module IntSet = Set.Make (Int)
module StringMap = Map.Make (String)
