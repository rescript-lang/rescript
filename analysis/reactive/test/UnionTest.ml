(** Union combinator tests *)

open Reactive
open TestHelpers

let test_union_basic () =
  reset ();
  Printf.printf "=== Test: union basic ===\n";

  (* Left collection *)
  let left, emit_left = source ~name:"left" () in

  (* Right collection *)
  let right, emit_right = source ~name:"right" () in

  (* Create union without merge (right takes precedence) *)
  let combined = union ~name:"combined" left right () in

  (* Initially empty *)
  assert (length combined = 0);

  (* Add to left *)
  emit_left (Set ("a", 1));
  Printf.printf "After left Set(a, 1): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get combined "a" = Some 1);

  (* Add different key to right *)
  emit_right (Set ("b", 2));
  Printf.printf "After right Set(b, 2): combined=%d\n" (length combined);
  assert (length combined = 2);
  assert (get combined "a" = Some 1);
  assert (get combined "b" = Some 2);

  (* Add same key to right (should override left) *)
  emit_right (Set ("a", 10));
  Printf.printf "After right Set(a, 10): combined a=%d\n"
    (get combined "a" |> Option.value ~default:(-1));
  assert (length combined = 2);
  assert (get combined "a" = Some 10);

  (* Right takes precedence *)

  (* Remove from right (left value should show through) *)
  emit_right (Remove "a");
  Printf.printf "After right Remove(a): combined a=%d\n"
    (get combined "a" |> Option.value ~default:(-1));
  assert (get combined "a" = Some 1);

  (* Left shows through *)

  (* Remove from left *)
  emit_left (Remove "a");
  Printf.printf "After left Remove(a): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get combined "a" = None);
  assert (get combined "b" = Some 2);

  Printf.printf "PASSED\n\n"

let test_union_with_merge () =
  reset ();
  Printf.printf "=== Test: union with merge ===\n";

  (* Left collection *)
  let left, emit_left = source ~name:"left" () in

  (* Right collection *)
  let right, emit_right = source ~name:"right" () in

  (* Create union with set union as merge *)
  let combined = union ~name:"combined" left right ~merge:IntSet.union () in

  (* Add to left: key "x" -> {1, 2} *)
  emit_left (Set ("x", IntSet.of_list [1; 2]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After left Set(x, {1,2}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2]));

  (* Add to right: key "x" -> {3, 4} (should merge) *)
  emit_right (Set ("x", IntSet.of_list [3; 4]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After right Set(x, {3,4}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2; 3; 4]));

  (* Update left: key "x" -> {1, 5} *)
  emit_left (Set ("x", IntSet.of_list [1; 5]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After left update to {1,5}: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 3; 4; 5]));

  (* Remove right *)
  emit_right (Remove "x");
  let v = get combined "x" |> Option.get in
  Printf.printf "After right Remove(x): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 5]));

  Printf.printf "PASSED\n\n"

let test_union_existing_data () =
  reset ();
  Printf.printf "=== Test: union on collections with existing data ===\n";

  (* Create collections with existing data *)
  let left, emit_left = source ~name:"left" () in
  emit_left (Set (1, "a"));
  emit_left (Set (2, "b"));

  let right, emit_right = source ~name:"right" () in
  emit_right (Set (2, "B"));
  (* Overlaps with left *)
  emit_right (Set (3, "c"));

  (* Create union after both have data *)
  let combined = union ~name:"combined" left right () in

  Printf.printf "Union has %d entries (expected 3)\n" (length combined);
  assert (length combined = 3);
  assert (get combined 1 = Some "a");
  (* Only in left *)
  assert (get combined 2 = Some "B");
  (* Right takes precedence *)
  assert (get combined 3 = Some "c");

  (* Only in right *)
  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Union Tests ======\n\n";
  test_union_basic ();
  test_union_with_merge ();
  test_union_existing_data ()
