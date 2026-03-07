(** Batch processing tests *)

open Reactive
open TestHelpers

let test_batch_flatmap () =
  reset ();
  Printf.printf "=== Test: batch flatmap ===\n";

  let source, emit = Source.create ~name:"source" () in
  let derived =
    FlatMap.create ~name:"derived" source
      ~f:(fun k v emit -> emit (k ^ "_derived") (v * 2))
      ()
  in

  (* Subscribe to track what comes out *)
  let received_batches = ref 0 in
  let received_entries = ref [] in
  subscribe
    (function
      | entries ->
        incr received_batches;
        received_entries := entries @ !received_entries)
    derived;

  (* Send a batch *)
  emit_sets emit [("a", 1); ("b", 2); ("c", 3)];

  Printf.printf "Received batches: %d, entries: %d\n" !received_batches
    (List.length !received_entries);
  assert (!received_batches = 1);
  assert (List.length !received_entries = 3);
  assert (get_opt derived "a_derived" = Some 2);
  assert (get_opt derived "b_derived" = Some 4);
  assert (get_opt derived "c_derived" = Some 6);

  Printf.printf "PASSED\n\n"

let test_batch_fixpoint () =
  reset ();
  Printf.printf "=== Test: batch fixpoint ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track batches received *)
  let batch_count = ref 0 in
  let total_added = ref 0 in
  subscribe
    (function
      | entries ->
        incr batch_count;
        entries
        |> List.iter (fun (_, mv) ->
               if ReactiveMaybe.is_some mv then incr total_added))
    fp;

  (* Set up edges first *)
  emit_edge_set emit_edges "a" ["b"; "c"];
  emit_edge_set emit_edges "b" ["d"];

  (* Send batch of roots *)
  emit_sets emit_init [("a", ()); ("x", ())];

  Printf.printf "Batch count: %d, total added: %d\n" !batch_count !total_added;
  Printf.printf "fp length: %d\n" (length fp);
  (* Should have a, b, c, d (reachable from a) and x (standalone root) *)
  assert (length fp = 5);
  assert (get_opt fp "a" = Some ());
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());
  assert (get_opt fp "d" = Some ());
  assert (get_opt fp "x" = Some ());

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Batch Tests ======\n\n";
  test_batch_flatmap ();
  test_batch_fixpoint ()
