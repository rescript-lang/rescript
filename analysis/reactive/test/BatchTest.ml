(** Batch processing tests *)

open Reactive
open TestHelpers

let test_batch_flatmap () =
  reset ();
  Printf.printf "=== Test: batch flatmap ===\n";

  let source, emit = source ~name:"source" () in
  let derived =
    flatMap ~name:"derived" source ~f:(fun k v -> [(k ^ "_derived", v * 2)]) ()
  in

  (* Subscribe to track what comes out *)
  let received_batches = ref 0 in
  let received_entries = ref [] in
  subscribe
    (function
      | Batch entries ->
        incr received_batches;
        received_entries := entries @ !received_entries
      | Set (k, v) -> received_entries := [(k, Some v)] @ !received_entries
      | Remove k -> received_entries := [(k, None)] @ !received_entries)
    derived;

  (* Send a batch *)
  emit_batch [set "a" 1; set "b" 2; set "c" 3] emit;

  Printf.printf "Received batches: %d, entries: %d\n" !received_batches
    (List.length !received_entries);
  assert (!received_batches = 1);
  assert (List.length !received_entries = 3);
  assert (get derived "a_derived" = Some 2);
  assert (get derived "b_derived" = Some 4);
  assert (get derived "c_derived" = Some 6);

  Printf.printf "PASSED\n\n"

let test_batch_fixpoint () =
  reset ();
  Printf.printf "=== Test: batch fixpoint ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track batches received *)
  let batch_count = ref 0 in
  let total_added = ref 0 in
  subscribe
    (function
      | Batch entries ->
        incr batch_count;
        entries
        |> List.iter (fun (_, v_opt) ->
               match v_opt with
               | Some () -> incr total_added
               | None -> ())
      | Set (_, ()) -> incr total_added
      | Remove _ -> ())
    fp;

  (* Set up edges first *)
  emit_edges (Set ("a", ["b"; "c"]));
  emit_edges (Set ("b", ["d"]));

  (* Send batch of roots *)
  emit_batch [set "a" (); set "x" ()] emit_init;

  Printf.printf "Batch count: %d, total added: %d\n" !batch_count !total_added;
  Printf.printf "fp length: %d\n" (length fp);
  (* Should have a, b, c, d (reachable from a) and x (standalone root) *)
  assert (length fp = 5);
  assert (get fp "a" = Some ());
  assert (get fp "b" = Some ());
  assert (get fp "c" = Some ());
  assert (get fp "d" = Some ());
  assert (get fp "x" = Some ());

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Batch Tests ======\n\n";
  test_batch_flatmap ();
  test_batch_fixpoint ()
