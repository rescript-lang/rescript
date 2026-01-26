(** Incremental fixpoint update tests (add/remove base and edges) *)

open Reactive
open TestHelpers

let test_fixpoint_add_base () =
  reset ();
  Printf.printf "=== Test: fixpoint add base ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b, c -> d *)
  emit_edges (Set ("a", ["b"]));
  emit_edges (Set ("c", ["d"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 2);

  (* a, b *)

  (* Track changes via subscription *)
  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | Set (k, ()) -> added := k :: !added
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        entries
        |> List.iter (fun (k, v_opt) ->
               match v_opt with
               | Some () -> added := k :: !added
               | None -> removed := k :: !removed))
    fp;

  emit_init (Set ("c", ()));

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

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b -> c *)
  emit_edges (Set ("a", ["b"]));
  emit_edges (Set ("b", ["c"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  emit_init (Remove "a");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 3);
  assert (length fp = 0);

  Printf.printf "PASSED\n\n"

let test_fixpoint_add_edge () =
  reset ();
  Printf.printf "=== Test: fixpoint add edge ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 1);

  (* just a *)
  let added = ref [] in
  subscribe
    (function
      | Set (k, ()) -> added := k :: !added
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = Some () then added := k :: !added)
          entries
      | _ -> ())
    fp;

  (* Add edge a -> b *)
  emit_edges (Set ("a", ["b"]));

  Printf.printf "Added: [%s]\n" (String.concat ", " !added);
  assert (List.mem "b" !added);
  assert (length fp = 2);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge () =
  reset ();
  Printf.printf "=== Test: fixpoint remove edge ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b -> c *)
  emit_edges (Set ("a", ["b"]));
  emit_edges (Set ("b", ["c"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Remove edge a -> b *)
  emit_edges (Set ("a", []));

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 2);
  (* b, c *)
  assert (length fp = 1);

  (* just a *)
  Printf.printf "PASSED\n\n"

let test_fixpoint_cycle_removal () =
  reset ();
  Printf.printf "=== Test: fixpoint cycle removal (well-founded) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b -> c -> b (b-c cycle reachable from a) *)
  emit_edges (Set ("a", ["b"]));
  emit_edges (Set ("b", ["c"]));
  emit_edges (Set ("c", ["b"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Remove edge a -> b *)
  emit_edges (Set ("a", []));

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

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Graph: a -> b, a -> c -> b
     If we remove a -> b, b should survive via a -> c -> b *)
  emit_edges (Set ("a", ["b"; "c"]));
  emit_edges (Set ("c", ["b"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (length fp = 3);

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Remove direct edge a -> b (but keep a -> c) *)
  emit_edges (Set ("a", ["c"]));

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  (* b should NOT be removed - still reachable via c *)
  assert (!removed = []);
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_deltas () =
  reset ();
  Printf.printf "=== Test: fixpoint delta emissions ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  emit_edges (Set (1, [2; 3]));
  emit_edges (Set (2, [4]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Count entries, not deltas - V2 emits batches *)
  let all_entries = ref [] in
  subscribe
    (function
      | Set (k, v) -> all_entries := (k, Some v) :: !all_entries
      | Remove k -> all_entries := (k, None) :: !all_entries
      | Batch entries -> all_entries := entries @ !all_entries)
    fp;

  (* Add root *)
  emit_init (Set (1, ()));
  Printf.printf "After add root: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 4);

  (* 1, 2, 3, 4 *)
  all_entries := [];

  (* Add edge 3 -> 5 *)
  emit_edges (Set (3, [5]));
  Printf.printf "After add edge 3->5: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 1);

  (* 5 added *)
  all_entries := [];

  (* Remove root (should remove all) *)
  emit_init (Remove 1);
  Printf.printf "After remove root: %d entries\n" (List.length !all_entries);
  assert (List.length !all_entries = 5);

  (* 1, 2, 3, 4, 5 removed *)
  Printf.printf "PASSED\n\n"

(* Test: Remove from init but still reachable via edges *)
let test_fixpoint_remove_spurious_root () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove spurious root (still reachable) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track all deltas *)
  let added = ref [] in
  let removed = ref [] in
  subscribe
    (function
      | Set (k, ()) -> added := k :: !added
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        entries
        |> List.iter (fun (k, v_opt) ->
               match v_opt with
               | Some () -> added := k :: !added
               | None -> removed := k :: !removed))
    fp;

  (* Step 1: "b" is spuriously marked as a root *)
  emit_init (Set ("b", ()));
  Printf.printf "After spurious root b: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  assert (get fp "b" = Some ());

  (* Step 2: The real root "root" is added *)
  emit_init (Set ("root", ()));
  Printf.printf "After true root: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  (* Step 3: Edge root -> a is added *)
  emit_edges (Set ("root", ["a"]));
  Printf.printf "After edge root->a: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  assert (get fp "a" = Some ());

  (* Step 4: Edge a -> b is added *)
  emit_edges (Set ("a", ["b"]));
  Printf.printf "After edge a->b: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  added := [];
  removed := [];

  (* Step 5: The spurious root "b" is REMOVED from init *)
  emit_init (Remove "b");

  Printf.printf "After removing b from init: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via a *)
  assert (not (List.mem "b" !removed));
  assert (get fp "b" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_alternative_source () =
  reset ();
  Printf.printf
    "=== Test: fixpoint remove edge entry (alternative source) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Set up initial edges: a -> b, c -> b *)
  emit_edges (Set ("a", ["b"]));
  emit_edges (Set ("c", ["b"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Add roots a and c *)
  emit_init (Set ("a", ()));
  emit_init (Set ("c", ()));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  removed := [];

  (* Remove entire edge entry for "a" *)
  emit_edges (Remove "a");

  Printf.printf "After Remove edge entry 'a': fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via c -> b *)
  assert (not (List.mem "b" !removed));
  assert (get fp "b" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_rederivation () =
  reset ();
  Printf.printf "=== Test: fixpoint remove edge (re-derivation needed) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Set (k, ()) -> added := k :: !added
      | Batch entries ->
        entries
        |> List.iter (fun (k, v_opt) ->
               match v_opt with
               | Some () -> added := k :: !added
               | None -> removed := k :: !removed))
    fp;

  (* Add root *)
  emit_init (Set ("root", ()));

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edges (Set ("root", ["a"]));
  emit_edges (Set ("a", ["b"; "c"]));
  emit_edges (Set ("b", ["c"]));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 4);

  removed := [];
  added := [];

  (* Remove the direct edge a -> c *)
  emit_edges (Set ("a", ["b"]));

  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint - reachable via root -> a -> b -> c *)
  assert (get fp "c" = Some ());
  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_rederivation () =
  reset ();
  Printf.printf "=== Test: fixpoint Remove edge entry (re-derivation) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Set up edges before creating fixpoint *)
  emit_edges (Set ("a", ["c"]));
  emit_edges (Set ("b", ["c"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Add roots a and b *)
  emit_init (Set ("a", ()));
  emit_init (Set ("b", ()));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 3);

  removed := [];

  (* Remove entire edge entry for "a" using Remove delta *)
  emit_edges (Remove "a");

  Printf.printf "After Remove 'a' entry: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* c should survive - b -> c still exists *)
  assert (not (List.mem "c" !removed));
  assert (get fp "c" = Some ());
  assert (length fp = 3);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_higher_rank_support () =
  reset ();
  Printf.printf "=== Test: fixpoint edge removal (higher rank support) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Set (k, ()) -> added := k :: !added
      | Batch entries ->
        entries
        |> List.iter (fun (k, v_opt) ->
               match v_opt with
               | Some () -> added := k :: !added
               | None -> removed := k :: !removed))
    fp;

  (* Add root *)
  emit_init (Set ("root", ()));

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edges (Set ("root", ["a"]));
  emit_edges (Set ("a", ["b"; "c"]));
  emit_edges (Set ("b", ["c"]));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));

  assert (length fp = 4);
  assert (get fp "c" = Some ());

  removed := [];
  added := [];

  (* Remove direct edge a -> c, keeping a -> b *)
  emit_edges (Set ("a", ["b"]));

  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     iter (fun k _ -> items := k :: !items) fp;
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint via root -> a -> b -> c *)
  assert (get fp "c" = Some ());
  assert (length fp = 4);

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_edge_entry_needs_rederivation () =
  reset ();
  Printf.printf
    "=== Test: fixpoint Remove edge entry (needs re-derivation) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Pre-populate edges so fixpoint initializes with them *)
  emit_edges (Set ("r", ["a"; "b"]));
  emit_edges (Set ("a", ["y"]));
  emit_edges (Set ("b", ["c"]));
  emit_edges (Set ("c", ["x"]));
  emit_edges (Set ("x", ["y"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  (* Make r live *)
  emit_init (Set ("r", ()));

  (* Sanity: y initially reachable via short path *)
  assert (get fp "y" = Some ());
  assert (get fp "x" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Remove the entire edge entry for a (removes a->y) *)
  emit_edges (Remove "a");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* Correct: y is still reachable via r->b->c->x->y *)
  assert (get fp "y" = Some ());

  Printf.printf "PASSED\n\n"

let test_fixpoint_remove_base_needs_rederivation () =
  reset ();
  Printf.printf
    "=== Test: fixpoint Remove base element (needs re-derivation) ===\n";

  let init, emit_init = source ~name:"init" () in
  let edges, emit_edges = source ~name:"edges" () in

  (* Pre-populate edges so fixpoint initializes with them *)
  emit_edges (Set ("r1", ["a"]));
  emit_edges (Set ("a", ["y"]));
  emit_edges (Set ("r2", ["b"]));
  emit_edges (Set ("b", ["c"]));
  emit_edges (Set ("c", ["x"]));
  emit_edges (Set ("x", ["y"]));

  let fp = fixpoint ~name:"fp" ~init ~edges () in

  emit_init (Set ("r1", ()));
  emit_init (Set ("r2", ()));

  (* Sanity: y initially reachable *)
  assert (get fp "y" = Some ());
  assert (get fp "x" = Some ());

  let removed = ref [] in
  subscribe
    (function
      | Remove k -> removed := k :: !removed
      | Batch entries ->
        List.iter
          (fun (k, v_opt) -> if v_opt = None then removed := k :: !removed)
          entries
      | _ -> ())
    fp;

  (* Remove r1 from base: y should remain via r2 path *)
  emit_init (Remove "r1");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  assert (get fp "y" = Some ());
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
  test_fixpoint_remove_base_needs_rederivation ()
