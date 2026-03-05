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
  emit_set emit_left "a" 1;
  Printf.printf "After left set (a, 1): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get_opt combined "a" = Some 1);

  (* Add different key to right *)
  emit_set emit_right "b" 2;
  Printf.printf "After right set (b, 2): combined=%d\n" (length combined);
  assert (length combined = 2);
  assert (get_opt combined "a" = Some 1);
  assert (get_opt combined "b" = Some 2);

  (* Add same key to right (should override left) *)
  emit_set emit_right "a" 10;
  Printf.printf "After right set (a, 10): combined a=%d\n"
    (get_opt combined "a" |> Option.value ~default:(-1));
  assert (length combined = 2);
  assert (get_opt combined "a" = Some 10);

  (* Right takes precedence *)

  (* remove from right (left value should show through) *)
  emit_remove emit_right "a";
  Printf.printf "After right Remove(a): combined a=%d\n"
    (get_opt combined "a" |> Option.value ~default:(-1));
  assert (get_opt combined "a" = Some 1);

  (* Left shows through *)

  (* remove from left *)
  emit_remove emit_left "a";
  Printf.printf "After left Remove(a): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get_opt combined "a" = None);
  assert (get_opt combined "b" = Some 2);

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
  emit_set emit_left "x" (IntSet.of_list [1; 2]);
  let v = get_opt combined "x" |> Option.get in
  Printf.printf "After left set (x, {1,2}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2]));

  (* Add to right: key "x" -> {3, 4} (should merge) *)
  emit_set emit_right "x" (IntSet.of_list [3; 4]);
  let v = get_opt combined "x" |> Option.get in
  Printf.printf "After right set (x, {3,4}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2; 3; 4]));

  (* Update left: key "x" -> {1, 5} *)
  emit_set emit_left "x" (IntSet.of_list [1; 5]);
  let v = get_opt combined "x" |> Option.get in
  Printf.printf "After left update to {1,5}: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 3; 4; 5]));

  (* remove right *)
  emit_remove emit_right "x";
  let v = get_opt combined "x" |> Option.get in
  Printf.printf "After right Remove(x): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 5]));

  Printf.printf "PASSED\n\n"

let test_union_existing_data () =
  reset ();
  Printf.printf "=== Test: union on collections with existing data ===\n";

  (* Create collections with existing data *)
  let left, emit_left = source ~name:"left" () in
  emit_set emit_left 1 "a";
  emit_set emit_left 2 "b";

  let right, emit_right = source ~name:"right" () in
  emit_set emit_right 2 "B";
  (* Overlaps with left *)
  emit_set emit_right 3 "c";

  (* Create union after both have data *)
  let combined = union ~name:"combined" left right () in

  Printf.printf "Union has %d entries (expected 3)\n" (length combined);
  assert (length combined = 3);
  assert (get_opt combined 1 = Some "a");
  (* Only in left *)
  assert (get_opt combined 2 = Some "B");
  (* Right takes precedence *)
  assert (get_opt combined 3 = Some "c");

  (* Only in right *)
  Printf.printf "PASSED\n\n"

let test_union_existing_data_with_non_idempotent_merge () =
  reset ();
  Printf.printf "=== Test: union existing data with non-idempotent merge ===\n";

  (* Create collections with existing data *)
  let left, emit_left = source ~name:"left" () in
  emit_set emit_left "only_left" 3;

  let right, _emit_right = source ~name:"right" () in

  (* Create union after left already has data.
     With merge = (+), a left-only key must stay 3, not 6. *)
  let combined = union ~name:"combined" left right ~merge:( + ) () in

  assert (length combined = 1);
  assert (get_opt combined "only_left" = Some 3);

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Union Tests ======\n\n";
  test_union_basic ();
  test_union_with_merge ();
  test_union_existing_data ();
  test_union_existing_data_with_non_idempotent_merge ()
