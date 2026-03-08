(** Incremental fixpoint update tests (add/remove base and edges) *)

open Reactive
open TestHelpers

let test_fixpoint_add_base () =
  reset ();
  Printf.printf "=== Test: fixpoint add base ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Graph: a -> b, c -> d *)
  emit_edge_set emit_edges "a" ["b"];
  emit_edge_set emit_edges "c" ["d"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 2);

  (* a, b *)

  (* Track changes via subscription *)
  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        entries
        |> List.iter (fun (k, mv) ->
               if Maybe.is_some mv then added := k :: !added
               else removed := k :: !removed))
    fp;

  emit_set emit_init "c" ();

  Printf.printf "Added: [%s]\n" (String.concat ", " !added);
  assert (List.length !added = 2);
  (* c, d *)
  assert (List.mem "c" !added);
  assert (List.mem "d" !added);
  assert (!removed = []);
  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_base () =
  reset ();
  Printf.printf "=== Test: fixpoint remove base ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Graph: a -> b -> c *)
  emit_edge_set emit_edges "a" ["b"];
  emit_edge_set emit_edges "b" ["c"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  emit_remove emit_init "a";

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 3);
  assert (length fp = 0);

  Printf.printf "PASSED\n\n"

let test_fixpoint_add_edge () =
  reset ();
  Printf.printf "=== Test: fixpoint add edge ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 1);

  (* just a *)
  let added = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) -> if Maybe.is_some mv then added := k :: !added)
          entries)
    fp;

  (* Add edge a -> b *)
  emit_edge_set emit_edges "a" ["b"];

  Printf.printf "Added: [%s]\n" (String.concat ", " !added);
  assert (List.mem "b" !added);
  assert (length fp = 2);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge () =
  reset ();
  Printf.printf "=== Test: fixpoint remove edge ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Graph: a -> b -> c *)
  emit_edge_set emit_edges "a" ["b"];
  emit_edge_set emit_edges "b" ["c"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove edge a -> b *)
  emit_edge_set emit_edges "a" [];

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 2);
  (* b, c *)
  assert (length fp = 1);

  (* just a *)
  Printf.printf "PASSED\n\n"

let test_fixpoint_cycle_removal () =
  reset ();
  Printf.printf "=== Test: fixpoint cycle removal (well-founded) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Graph: a -> b -> c -> b (b-c cycle reachable from a) *)
  emit_edge_set emit_edges "a" ["b"];
  emit_edge_set emit_edges "b" ["c"];
  emit_edge_set emit_edges "c" ["b"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove edge a -> b *)
  emit_edge_set emit_edges "a" [];

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  (* Both b and c should be removed - cycle has no well-founded support *)
  assert (List.length !removed = 2);
  assert (List.mem "b" !removed);
  assert (List.mem "c" !removed);
  assert (length fp = 1);

  (* just a *)
  Printf.printf "PASSED\n\n"

let test_fixpoint_alternative_support () =
  reset ();
  Printf.printf "=== Test: fixpoint alternative support ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Graph: a -> b, a -> c -> b
     If we remove a -> b, b should survive via a -> c -> b *)
  emit_edge_set emit_edges "a" ["b"; "c"];
  emit_edge_set emit_edges "c" ["b"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "a" ();
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove direct edge a -> b (but keep a -> c) *)
  emit_edge_set emit_edges "a" ["c"];

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  (* b should NOT be removed - still reachable via c *)
  assert (!removed = []);
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_deltas () =
  reset ();
  Printf.printf "=== Test: fixpoint delta emissions ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  emit_edge_set emit_edges 1 [2; 3];
  emit_edge_set emit_edges 2 [4];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Count entries, not deltas - V2 emits batches *)
  let all_entries = ref [] in
  subscribe
    (function
      | entries -> all_entries := entries @ !all_entries)
    fp;

  (* Add root *)
  emit_set emit_init 1 ();
  Printf.printf "After add root: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 4);

  (* 1, 2, 3, 4 *)
  all_entries := [];

  (* Add edge 3 -> 5 *)
  emit_edge_set emit_edges 3 [5];
  Printf.printf "After add edge 3->5: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 1);

  (* 5 added *)
  all_entries := [];

  (* remove root (should remove all) *)
  emit_remove emit_init 1;
  Printf.printf "After remove root: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 5);

  (* 1, 2, 3, 4, 5 removed *)
  Printf.printf "PASSED\n\n"

(* Test: remove from init but still reachable via edges *)
let test_fixpoint_remove_spurious_root () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove spurious root (still reachable) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track all deltas *)
  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        entries
        |> List.iter (fun (k, mv) ->
               if Maybe.is_some mv then added := k :: !added
               else removed := k :: !removed))
    fp;

  (* Step 1: "b" is spuriously marked as a root *)
  emit_set emit_init "b" ();
  Printf.printf "After spurious root b: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  assert (get_opt fp "b" = Some ());

  (* Step 2: The real root "root" is added *)
  emit_set emit_init "root" ();
  Printf.printf "After true root: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  (* Step 3: Edge root -> a is added *)
  emit_edge_set emit_edges "root" ["a"];
  Printf.printf "After edge root->a: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  assert (get_opt fp "a" = Some ());

  (* Step 4: Edge a -> b is added *)
  emit_edge_set emit_edges "a" ["b"];
  Printf.printf "After edge a->b: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  added := [];
  removed := [];

  (* Step 5: The spurious root "b" is REMOVED from init *)
  emit_remove emit_init "b";

  Printf.printf "After removing b from init: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via a *)
  assert (not (List.mem "b" !removed));
  assert (get_opt fp "b" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_alternative_source () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove edge entry (alternative source) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Set up initial edges: a -> b, c -> b *)
  emit_edge_set emit_edges "a" ["b"];
  emit_edge_set emit_edges "c" ["b"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* Add roots a and c *)
  emit_set emit_init "a" ();
  emit_set emit_init "c" ();

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  removed := [];

  (* remove entire edge entry for "a" *)
  emit_remove emit_edges "a";

  Printf.printf "After remove edge entry 'a': fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via c -> b *)
  assert (not (List.mem "b" !removed));
  assert (get_opt fp "b" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_rederivation () =
  reset ();
  Printf.printf "=== Test: fixpoint remove edge (re-derivation needed) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  subscribe
    (function
      | entries ->
        entries
        |> List.iter (fun (k, mv) ->
               if Maybe.is_some mv then added := k :: !added
               else removed := k :: !removed))
    fp;

  (* Add root *)
  emit_set emit_init "root" ();

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edge_set emit_edges "root" ["a"];
  emit_edge_set emit_edges "a" ["b"; "c"];
  emit_edge_set emit_edges "b" ["c"];

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 4);

  removed := [];
  added := [];

  (* remove the direct edge a -> c *)
  emit_edge_set emit_edges "a" ["b"];

  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint - reachable via root -> a -> b -> c *)
  assert (get_opt fp "c" = Some ());
  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_rederivation () =
  reset ();
  Printf.printf "=== Test: fixpoint remove edge entry (re-derivation) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Set up edges before creating fixpoint *)
  emit_edge_set emit_edges "a" ["c"];
  emit_edge_set emit_edges "b" ["c"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* Add roots a and b *)
  emit_set emit_init "a" ();
  emit_set emit_init "b" ();

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  removed := [];

  (* remove entire edge entry for "a" using remove delta *)
  emit_remove emit_edges "a";

  Printf.printf "After remove 'a' entry: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* c should survive - b -> c still exists *)
  assert (not (List.mem "c" !removed));
  assert (get_opt fp "c" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_higher_rank_support () =
  reset ();
  Printf.printf "=== Test: fixpoint edge removal (higher rank support) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  subscribe
    (function
      | entries ->
        entries
        |> List.iter (fun (k, mv) ->
               if Maybe.is_some mv then added := k :: !added
               else removed := k :: !removed))
    fp;

  (* Add root *)
  emit_set emit_init "root" ();

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edge_set emit_edges "root" ["a"];
  emit_edge_set emit_edges "a" ["b"; "c"];
  emit_edge_set emit_edges "b" ["c"];

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 4);
  assert (get_opt fp "c" = Some ());

  removed := [];
  added := [];

  (* remove direct edge a -> c, keeping a -> b *)
  emit_edge_set emit_edges "a" ["b"];

  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := Stable.unsafe_to_value k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint via root -> a -> b -> c *)
  assert (get_opt fp "c" = Some ());
  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_needs_rederivation () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove edge entry (needs re-derivation) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Pre-populate edges so fixpoint initializes with them *)
  emit_edge_set emit_edges "r" ["a"; "b"];
  emit_edge_set emit_edges "a" ["y"];
  emit_edge_set emit_edges "b" ["c"];
  emit_edge_set emit_edges "c" ["x"];
  emit_edge_set emit_edges "x" ["y"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  (* Make r live *)
  emit_set emit_init "r" ();

  (* Sanity: y initially reachable via short path *)
  assert (get_opt fp "y" = Some ());
  assert (get_opt fp "x" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove the entire edge entry for a (removes a->y) *)
  emit_remove emit_edges "a";

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* Correct: y is still reachable via r->b->c->x->y *)
  assert (get_opt fp "y" = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_base_needs_rederivation () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove base element (needs re-derivation) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* Pre-populate edges so fixpoint initializes with them *)
  emit_edge_set emit_edges "r1" ["a"];
  emit_edge_set emit_edges "a" ["y"];
  emit_edge_set emit_edges "r2" ["b"];
  emit_edge_set emit_edges "b" ["c"];
  emit_edge_set emit_edges "c" ["x"];
  emit_edge_set emit_edges "x" ["y"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in

  emit_set emit_init "r1" ();
  emit_set emit_init "r2" ();

  (* Sanity: y initially reachable *)
  assert (get_opt fp "y" = Some ());
  assert (get_opt fp "x" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove r1 from base: y should remain via r2 path *)
  emit_remove emit_init "r1";

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  assert (get_opt fp "y" = Some ());
  Printf.printf "PASSED\n\n"

let test_fixpoint_batch_overlapping_deletions () =
  reset ();
  Printf.printf "=== Test: fixpoint batch overlapping deletions ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* r -> a,b ; a -> x ; b -> x ; x -> y *)
  emit_edge_set emit_edges "r" ["a"; "b"];
  emit_edge_set emit_edges "a" ["x"];
  emit_edge_set emit_edges "b" ["x"];
  emit_edge_set emit_edges "x" ["y"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in
  emit_set emit_init "r" ();

  assert (get_opt fp "x" = Some ());
  assert (get_opt fp "y" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove both supports for x in one batch. *)
  emit_edge_batch emit_edges [("a", Some []); ("b", Some [])];

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.mem "x" !removed);
  assert (List.mem "y" !removed);
  assert (List.length !removed = 2);
  assert (get_opt fp "x" = None);
  assert (get_opt fp "y" = None);
  assert (length fp = 3);

  (* r, a, b *)
  Printf.printf "PASSED\n\n"

let test_fixpoint_batch_delete_add_same_wave () =
  reset ();
  Printf.printf "=== Test: fixpoint batch delete+add same wave ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* r -> a,c ; a -> x ; c -> [] *)
  emit_edge_set emit_edges "r" ["a"; "c"];
  emit_edge_set emit_edges "a" ["x"];
  emit_edge_set emit_edges "c" [];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in
  emit_set emit_init "r" ();

  assert (get_opt fp "x" = Some ());
  assert (length fp = 4);

  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if Maybe.is_some mv then added := k :: !added
            else removed := k :: !removed)
          entries)
    fp;

  (* In one batch: remove a->x and add c->x. x should stay live. *)
  emit_edge_batch emit_edges [("a", Some []); ("c", Some ["x"])];

  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  assert (get_opt fp "x" = Some ());
  assert (length fp = 4);
  assert (!removed = []);
  assert (!added = []);

  Printf.printf "PASSED\n\n"

let test_fixpoint_fanin_single_predecessor_removed () =
  reset ();
  Printf.printf "=== Test: fixpoint fan-in single predecessor removed ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* r -> a,b,c ; a,b,c -> z *)
  emit_edge_set emit_edges "r" ["a"; "b"; "c"];
  emit_edge_set emit_edges "a" ["z"];
  emit_edge_set emit_edges "b" ["z"];
  emit_edge_set emit_edges "c" ["z"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in
  emit_set emit_init "r" ();

  assert (get_opt fp "z" = Some ());
  assert (length fp = 5);

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove only one predecessor contribution; z should remain live. *)
  emit_edge_set emit_edges "a" [];

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (get_opt fp "z" = Some ());
  assert (length fp = 5);
  assert (!removed = []);

  Printf.printf "PASSED\n\n"

let test_fixpoint_cycle_alternative_external_support () =
  reset ();
  Printf.printf
    "=== Test: fixpoint cycle with alternative external support ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* r1 -> b ; r2 -> c ; b <-> c *)
  emit_edge_set emit_edges "r1" ["b"];
  emit_edge_set emit_edges "r2" ["c"];
  emit_edge_set emit_edges "b" ["c"];
  emit_edge_set emit_edges "c" ["b"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in
  emit_set emit_init "r1" ();
  emit_set emit_init "r2" ();

  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if not (Maybe.is_some mv) then removed := k :: !removed)
          entries)
    fp;

  (* remove one external support edge; cycle should remain via r2 -> c. *)
  emit_edge_set emit_edges "r1" [];

  Printf.printf "After removing r1->b, removed: [%s]\n"
    (String.concat ", " !removed);
  assert (get_opt fp "b" = Some ());
  assert (get_opt fp "c" = Some ());
  assert (!removed = []);

  removed := [];

  (* remove the other external support edge; cycle should now disappear. *)
  emit_edge_set emit_edges "r2" [];

  Printf.printf "After removing r2->c, removed: [%s]\n"
    (String.concat ", " !removed);
  assert (List.mem "b" !removed);
  assert (List.mem "c" !removed);
  assert (get_opt fp "b" = None);
  assert (get_opt fp "c" = None);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_then_readd_via_expansion_same_wave () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove then re-add via expansion (same wave) ===\n";

  let init, emit_init = Source.create ~name:"init" () in
  let edges, emit_edges = Source.create ~name:"edges" () in

  (* r -> s ; s -> x ; y -> x ; then update s -> y.
     x is first tentatively deleted (s no longer points to x),
     then becomes reachable again via new path r -> s -> y -> x. *)
  emit_edge_set emit_edges "r" ["s"];
  emit_edge_set emit_edges "s" ["x"];
  emit_edge_set emit_edges "y" ["x"];

  let fp = Fixpoint.create ~name:"fp" ~init ~edges () in
  emit_set emit_init "r" ();

  assert (get_opt fp "x" = Some ());
  assert (get_opt fp "y" = None);
  assert (length fp = 3);

  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | entries ->
        List.iter
          (fun (k, mv) ->
            if Maybe.is_some mv then added := k :: !added
            else removed := k :: !removed)
          entries)
    fp;

  emit_edge_set emit_edges "s" ["y"];

  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* x should remain reachable; it must not be emitted as removed. *)
  assert (get_opt fp "x" = Some ());
  assert (get_opt fp "y" = Some ());
  assert (length fp = 4);
  assert (not (List.mem "x" !removed));
  assert (List.mem "y" !added);

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Fixpoint Incremental Tests ======\n\n";
  test_fixpoint_add_base ();
  test_fixpoint_remove_base ();
  test_fixpoint_add_edge ();
  test_fixpoint_remove_edge ();
  test_fixpoint_cycle_removal ();
  test_fixpoint_alternative_support ();
  test_fixpoint_deltas ();
  test_fixpoint_remove_spurious_root ();
  test_fixpoint_remove_edge_entry_alternative_source ();
  test_fixpoint_remove_edge_rederivation ();
  test_fixpoint_remove_edge_entry_rederivation ();
  test_fixpoint_remove_edge_entry_higher_rank_support ();
  test_fixpoint_remove_edge_entry_needs_rederivation ();
  test_fixpoint_remove_base_needs_rederivation ();
  test_fixpoint_batch_overlapping_deletions ();
  test_fixpoint_batch_delete_add_same_wave ();
  test_fixpoint_fanin_single_predecessor_removed ();
  test_fixpoint_cycle_alternative_external_support ();
  test_fixpoint_remove_then_readd_via_expansion_same_wave ()
