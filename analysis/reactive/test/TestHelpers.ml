(** Shared test helpers for Reactive tests *)

open Reactive

(** {1 Compatibility helpers} *)

(* V2's emit takes deltas, not tuples. These helpers adapt tuple-style calls. *)
let[@warning "-32"] emit_kv emit (k, v_opt) =
  match v_opt with
  | Some v -> emit (Set (k, v))
  | None -> emit (Remove k)

(* subscribe takes collection first in V2, but we want handler first for compatibility *)
let subscribe handler t = t.subscribe handler

(* emit_batch: emit a batch delta to a source *)
let emit_batch entries emit_fn = emit_fn (Batch entries)

(* Helper to track added/removed across all delta types *)
let[@warning "-32"] track_changes () =
  let added = ref [] in
  let removed = ref [] in
  let handler = function
    | Set (k, _) -> added := k :: !added
    | Remove k -> removed := k :: !removed
    | Batch entries ->
      List.iter
        (fun (k, v_opt) ->
          match v_opt with
          | Some _ -> added := k :: !added
          | None -> removed := k :: !removed)
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

(** {1 Common set modules} *)

module IntSet = Set.Make (Int)
module StringMap = Map.Make (String)
