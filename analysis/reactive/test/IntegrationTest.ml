(** End-to-end integration tests *)

open Reactive
open TestHelpers

let test_file_collection () =
  reset ();
  Printf.printf "=== Test: File collection simulation ===\n";

  (* Simulate file processing with regular sources *)
  let files, emit_file = source ~name:"files" () in

  (* file_a: hello(2), world(1) *)
  (* file_b: hello(1), foo(1) *)

  (* First flatMap: aggregate word counts across files with merge *)
  let word_counts =
    flatMap ~name:"word_counts" files
      ~f:(fun _path counts -> StringMap.bindings counts)
        (* Each file contributes its word counts *)
      ~merge:( + ) (* Sum counts from multiple files *)
      ()
  in

  (* Second flatMap: filter to words with count >= 2 *)
  let frequent_words =
    flatMap ~name:"frequent_words" word_counts
      ~f:(fun word count -> if count >= 2 then [(word, count)] else [])
      ()
  in

  (* Simulate processing files by emitting their word counts *)
  let counts_a =
    StringMap.empty |> StringMap.add "hello" 2 |> StringMap.add "world" 1
  in
  let counts_b =
    StringMap.empty |> StringMap.add "hello" 1 |> StringMap.add "foo" 1
  in
  emit_file (Set ("file_a", counts_a));
  emit_file (Set ("file_b", counts_b));

  Printf.printf "Word counts:\n";
  iter (fun word count -> Printf.printf "  %s: %d\n" word count) word_counts;

  Printf.printf "Frequent words (count >= 2):\n";
  iter (fun word count -> Printf.printf "  %s: %d\n" word count) frequent_words;

  (* Verify: hello=3 (2 from a + 1 from b), world=1, foo=1 *)
  assert (get word_counts "hello" = Some 3);
  assert (get word_counts "world" = Some 1);
  assert (get word_counts "foo" = Some 1);
  assert (length word_counts = 3);

  (* Verify frequent: only "hello" with count 3 *)
  assert (length frequent_words = 1);
  assert (get frequent_words "hello" = Some 3);

  (* Modify file_a: now hello(1), world(2) *)
  Printf.printf "\nModifying file_a...\n";
  let counts_a' =
    StringMap.empty |> StringMap.add "hello" 1 |> StringMap.add "world" 2
  in
  emit_file (Set ("file_a", counts_a'));

  Printf.printf "Word counts after modification:\n";
  iter (fun word count -> Printf.printf "  %s: %d\n" word count) word_counts;

  Printf.printf "Frequent words after modification:\n";
  iter (fun word count -> Printf.printf "  %s: %d\n" word count) frequent_words;

  (* Verify: hello=2 (1 from a + 1 from b), world=2, foo=1 *)
  assert (get word_counts "hello" = Some 2);
  assert (get word_counts "world" = Some 2);
  assert (get word_counts "foo" = Some 1);

  (* Verify frequent: hello=2, world=2 *)
  assert (length frequent_words = 2);
  assert (get frequent_words "hello" = Some 2);
  assert (get frequent_words "world" = Some 2);

  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Integration Tests ======\n\n";
  test_file_collection ()
