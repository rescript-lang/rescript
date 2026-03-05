(** Basic fixpoint graph traversal tests *)

open Reactive
open TestHelpers

let test_fixpoint () =
  reset ();
  Printf.printf "Test: fixpoint\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Set up graph: 1 -> [2, 3], 2 -> [4], 3 -> [4] *)
  emit_set emit_edges 1 [2; 3];
  emit_set emit_edges 2 [4];
  emit_set emit_edges 3 [4];

  (* Compute fixpoint *)
  let reachable = fixpoint ~name:"reachable" ~init ~edges () in

  (* Initially empty *)
  Printf.printf "Initially: length=%d\n" (length reachable);
  assert (length reachable = 0);

  (* Add root 1 *)
  emit_set emit_init 1 ();
  Printf.printf "After adding root 1: length=%d\n" (length reachable);
  assert (length reachable = 4);
  (* 1, 2, 3, 4 *)
  assert (get_opt reachable 1 = Some ());
  assert (get_opt reachable 2 = Some ());
  assert (get_opt reachable 3 = Some ());
  assert (get_opt reachable 4 = Some ());
  assert (get_opt reachable 5 = None);

  (* Add another root 5 with edge 5 -> [6] *)
  emit_set emit_edges 5 [6];
  emit_set emit_init 5 ();
  Printf.printf "After adding root 5: length=%d\n" (length reachable);
  assert (length reachable = 6);

  (* 1, 2, 3, 4, 5, 6 *)

  (* remove root 1 *)
  emit_remove emit_init 1;
  Printf.printf "After removing root 1: length=%d\n" (length reachable);
  assert (length reachable = 2);
  (* 5, 6 *)
  assert (get_opt reachable 1 = None);
  assert (get_opt reachable 5 = Some ());
  assert (get_opt reachable 6 = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_basic_expansion () =
  reset ();
  Printf.printf "=== Test: fixpoint basic expansion ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b -> c *)
  emit_set emit_edges "a" ["b"];
  emit_set emit_edges "b" ["c"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();

  assert (length fp = 3);
  assert (get_opt fp "a" = Some ());
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());
  assert (get_opt fp "d" = None);

  Printf.printf "PASSED\n\n"

let test_fixpoint_multiple_roots () =
  reset ();
  Printf.printf "=== Test: fixpoint multiple roots ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b, c -> d (disconnected components) *)
  emit_set emit_edges "a" ["b"];
  emit_set emit_edges "c" ["d"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  emit_set emit_init "c" ();

  assert (length fp = 4);
  assert (get_opt fp "a" = Some ());
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());
  assert (get_opt fp "d" = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_diamond () =
  reset ();
  Printf.printf "=== Test: fixpoint diamond ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b, a -> c, b -> d, c -> d *)
  emit_set emit_edges "a" ["b"; "c"];
  emit_set emit_edges "b" ["d"];
  emit_set emit_edges "c" ["d"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();

  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_cycle () =
  reset ();
  Printf.printf "=== Test: fixpoint cycle ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b -> c -> b (cycle from root) *)
  emit_set emit_edges "a" ["b"];
  emit_set emit_edges "b" ["c"];
  emit_set emit_edges "c" ["b"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();

  assert (length fp = 3);
  assert (get_opt fp "a" = Some ());
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_empty_base () =
  reset ();
  Printf.printf "=== Test: fixpoint empty base ===\n";

  let init, _emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  emit_set emit_edges "a" ["b"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  assert (length fp = 0);

  Printf.printf "PASSED\n\n"

let test_fixpoint_self_loop () =
  reset ();
  Printf.printf "=== Test: fixpoint self loop ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> a (self loop) *)
  emit_set emit_edges "a" ["a"];

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();

  assert (length fp = 1);
  assert (get_opt fp "a" = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_existing_data () =
  reset ();
  Printf.printf "=== Test: fixpoint with existing data ===\n";

  (* Create source and pre-populate *)
  let init, emit_init = source ~name:"init" () in
  emit_set emit_init "root" ();

  let edges, emit_edges = source ~name:"edges" () in
  emit_set emit_edges "root" ["a"; "b"];
  emit_set emit_edges "a" ["c"];

  (* Create fixpoint - should immediately have all reachable *)
  let fp = fixpoint ~name:"fp" ~init ~edges () in

  Printf.printf "Fixpoint length: %d (expected 4)\n" (length fp);
  assert (length fp = 4);
  (* root, a, b, c *)
  assert (get_opt fp "root" = Some ());
  assert (get_opt fp "a" = Some ());
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Fixpoint Basic Tests ======\n\n";
  test_fixpoint ();
  test_fixpoint_basic_expansion ();
  test_fixpoint_multiple_roots ();
  test_fixpoint_diamond ();
  test_fixpoint_cycle ();
  test_fixpoint_empty_base ();
  test_fixpoint_self_loop ();
  test_fixpoint_existing_data ()
