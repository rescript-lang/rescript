(** FlatMap combinator tests *)

open Reactive
open TestHelpers

let test_flatmap_basic () =
  reset ();
  Printf.printf "=== Test: flatMap basic ===\n";

  (* Create a simple source collection *)
  let source, emit = source ~name:"source" () in

  (* Create derived collection via flatMap *)
  let derived =
    flatMap ~name:"derived" source
      ~f:(fun key value ->
        [(key * 10, value); ((key * 10) + 1, value); ((key * 10) + 2, value)])
      ()
  in

  (* Add entry -> derived should have 3 entries *)
  emit (Set (1, "a"));
  Printf.printf "After Set(1, 'a'): derived has %d entries\n" (length derived);
  assert (length derived = 3);
  assert (get source 1 = Some "a");
  (* Check source was updated *)
  assert (get derived 10 = Some "a");
  assert (get derived 11 = Some "a");
  assert (get derived 12 = Some "a");

  (* Add another entry *)
  emit (Set (2, "b"));
  Printf.printf "After Set(2, 'b'): derived has %d entries\n" (length derived);
  assert (length derived = 6);

  (* Update entry *)
  emit (Set (1, "A"));
  Printf.printf "After Set(1, 'A'): derived has %d entries\n" (length derived);
  assert (get derived 10 = Some "A");
  assert (length derived = 6);

  (* Remove entry *)
  emit (Remove 1);
  Printf.printf "After Remove(1): derived has %d entries\n" (length derived);
  assert (length derived = 3);
  assert (get derived 10 = None);
  assert (get derived 20 = Some "b");

  Printf.printf "PASSED\n\n"

let test_flatmap_with_merge () =
  reset ();
  Printf.printf "=== Test: flatMap with merge ===\n";

  let source, emit = source ~name:"source" () in

  (* Create derived with merge *)
  let derived =
    flatMap ~name:"derived" source
      ~f:(fun _key values -> [(0, values)]) (* all contribute to key 0 *)
      ~merge:IntSet.union ()
  in

  (* Source 1 contributes {1, 2} *)
  emit (Set (1, IntSet.of_list [1; 2]));
  let v = get derived 0 |> Option.get in
  Printf.printf "After source 1: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2]));

  (* Source 2 contributes {3, 4} -> should merge *)
  emit (Set (2, IntSet.of_list [3; 4]));
  let v = get derived 0 |> Option.get in
  Printf.printf "After source 2: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2; 3; 4]));

  (* Remove source 1 *)
  emit (Remove 1);
  let v = get derived 0 |> Option.get in
  Printf.printf "After remove 1: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [3; 4]));

  Printf.printf "PASSED\n\n"

let test_composition () =
  reset ();
  Printf.printf "=== Test: composition (flatMap chain) ===\n";

  (* Source: file -> list of items *)
  let source, emit = source ~name:"source" () in

  (* First flatMap: file -> items *)
  let items =
    flatMap ~name:"items" source
      ~f:(fun path items ->
        List.mapi (fun i item -> (Printf.sprintf "%s:%d" path i, item)) items)
      ()
  in

  (* Second flatMap: item -> chars *)
  let chars =
    flatMap ~name:"chars" items
      ~f:(fun key value ->
        String.to_seq value
        |> Seq.mapi (fun i c -> (Printf.sprintf "%s:%d" key i, c))
        |> List.of_seq)
      ()
  in

  (* Add file with 2 items *)
  emit (Set ("file1", ["ab"; "cd"]));
  Printf.printf "After file1: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 2);
  assert (length chars = 4);

  (* Add another file *)
  emit (Set ("file2", ["xyz"]));
  Printf.printf "After file2: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 3);
  assert (length chars = 7);

  (* Update file1 *)
  emit (Set ("file1", ["a"]));
  Printf.printf "After update file1: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 2);
  (* 1 from file1 + 1 from file2 *)
  assert (length chars = 4);

  (* 1 from file1 + 3 from file2 *)
  Printf.printf "PASSED\n\n"

let test_flatmap_on_existing_data () =
  reset ();
  Printf.printf "=== Test: flatMap on collection with existing data ===\n";

  (* Create source and add data before creating flatMap *)
  let source, emit = source ~name:"source" () in
  emit (Set (1, "a"));
  emit (Set (2, "b"));

  Printf.printf "Source has %d entries before flatMap\n" (length source);

  (* Create flatMap AFTER source has data *)
  let derived =
    flatMap ~name:"derived" source ~f:(fun k v -> [(k * 10, v)]) ()
  in

  (* Check derived has existing data *)
  Printf.printf "Derived has %d entries (expected 2)\n" (length derived);
  assert (length derived = 2);
  assert (get derived 10 = Some "a");
  assert (get derived 20 = Some "b");

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== FlatMap Tests ======\n\n";
  test_flatmap_basic ();
  test_flatmap_with_merge ();
  test_composition ();
  test_flatmap_on_existing_data ()
